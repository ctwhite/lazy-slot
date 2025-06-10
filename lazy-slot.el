;;; lazy-slot.el --- Async slot resolution with advanced caching -*-
;;; lexical-binding: t; -*-

;; Author: Christian White <christiantwhite@protonmail.com>
;; Maintainer: Christian White <christiantwhite@protonmail.com>
;; Version: 0.9.1
;; Keywords: lisp, programming, async, lazy, cache, struct, concurrency
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5") (concur "1.0") (dash "2.18.0") (ht "2.3") (cacheus "1.0"))
;; Homepage: https://github.com/ctwhite/lazy-slot
;;
;;; Commentary:
;;
;; This file provides macros and functions for defining and using "lazy" slots
;; in Emacs Lisp objects (specifically, those defined with `cl-defstruct`).
;; Lazy slots delay the computation of their values until they are first
;; accessed. The result is then cached for subsequent accesses, improving
;; performance by avoiding redundant computations.
;;
;; This module supports both synchronous and asynchronous lazy evaluation and
;; integrates with `cacheus` to provide advanced caching capabilities,
;; including Time-To-Live (TTL), persistence, and versioning for the
;; computed slot values. Asynchronous operations are managed using `concur`
;; futures and promises, allowing long-running computations or I/O to occur
;; without blocking the Emacs UI.
;;
;; **Key Concepts:**
;;
;; - **Lazy Evaluation:** The fundamental principle. The value of a lazy slot is
;;   computed only when it is first accessed (or explicitly forced). This
;;   contrasts with "eager" slots whose values are computed at object
;;   initialization.
;;
;; - **Synchronous Slots:** For computations that are fast and CPU-bound,
;;   the result is computed immediately upon first access and stored directly
;;   in the slot. Subsequent accesses retrieve the cached value directly.
;;
;; - **Asynchronous Slots:** Designed for I/O-bound or potentially long-running
;;   computations (e.g., network requests, heavy data processing). These slots
;;   store a `concur` future. Accessing the slot will block Emacs until the
;;   future resolves (i.e., the computation completes). The actual computation
;;   occurs in the background, initiated typically when the object is created
;;   (if `auto-fetch` is enabled) or on first access.
;;
;; - **Implicit Context (`obj`, `cb`):** Within the body of a lazy slot
;;   definition (e.g., in `lazy-slots!`), the special symbols `obj` and `cb`
;;   (for async slots) are available as anaphors.
;;   - `obj`: Refers to the current `cl-defstruct` object instance whose slot
;;     is being computed.
;;   - `cb`: For `:async` slots, `cb` is the callback function that you must
;;     call with the computed result once the asynchronous operation is complete.
;;
;; - **Caching Integration (`cacheus`):** The library tightly integrates with
;;   `cacheus` to provide robust caching. By specifying `:cache-options` in
;;   slot definitions, users can configure features like:
;;   - **TTL (Time-To-Live):** Automatically expire cached values after a set duration.
;;   - **Versioning:** Invalidate caches when underlying data models change.
;;   - **Persistence:** Save/load cached values across Emacs sessions.
;;
;; - **`concur` Integration:** Asynchronous operations rely on `concur` for
;;   futures and promises. `concur:timeout` and `concur:retry` are used
;;   to enhance the robustness of async slot computations.
;;
;; **Core Functionality:**
;;
;; - `lazy-slots!`: A high-level, declarative macro for defining multiple lazy
;;   slots (eager, synchronous, and asynchronous) for a `cl-defstruct` type.
;;   This is the primary entry point for users.
;;
;; - `lazy-slot-sync!`: A lower-level macro for defining individual lazy
;;   synchronous slots. Useful when defining slots outside of `lazy-slots!`.
;;
;; - `lazy-slot-async!`: A lower-level macro for defining individual lazy
;;   asynchronous slots. Also typically used via `lazy-slots!`.
;;
;; - `lazy-slot-status`: Inspects the current state of a lazy slot
;;   (`'uninitialized`, `'pending`, `'resolved`, `'rejected`, `'cancelled`).
;;
;; - `lazy-slot-force-fetch`: Explicitly triggers (or re-triggers) the
;;   computation of a lazy slot's value, useful for eager loading or refreshing.
;;
;; - `lazy-slot-clear-cache`: Invalidates the cached value for a specific lazy
;;   slot, forcing a re-computation on the next access.
;;
;; - `lazy-slot-value`: Retrieves the resolved value of a lazy slot without
;;   triggering computation or blocking if it's still pending.
;;
;; - `lazy-slot-error`: Retrieves the error value of a lazy slot if its
;;   computation failed.
;;
;;; Code:

(require 'cl-lib)
(require 'concur)
(require 'dash)
(require 'ht)
(require 'cacheus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization and Internal State

(defconst lazy-slot-version "0.9.1"
  "Version of the `lazy-slot.el` library.")

(defgroup lazy-slot nil
  "Lazy struct slot library for Emacs."
  :group 'lisp) 

(defcustom lazy-slot-log-hook nil
  "Hook run for logging messages within the Lazy Slot library.

This hook allows users to integrate `lazy-slot`'s internal logging with their
preferred logging system (e.g., `s.el`'s logging, `message` to the echo area).
Functions added to this hook should accept a `LEVEL` symbol, a `FORMAT-STRING`,
and any additional `ARGS` for the format string.

Arguments:
- `LEVEL` (symbol): The log level, e.g., `:debug`, `:info`, `:warn`, `:error`.
- `FORMAT-STRING` (string): The format-control string for the message.
- `ARGS` (rest): Arguments for the format string.

Example:
  (add-hook 'lazy-slot-log-hook (lambda (level fmt &rest args)
                                   (apply #'message (format \"[Lazy-Slot %s] %s\" level fmt) args)))"
  :type 'hook
  :group 'lazy-slot)

(defvar lazy-slot--definitions (ht-create)
  "Internal hash table for storing lazy slot definitions.

Keys are `cons` cells `(TYPE . SLOT)` where `TYPE` is the `cl-defstruct`
type symbol and `SLOT` is the lazy slot symbol.
Values are property lists (`plist`) containing the details of the lazy slot's
definition, such as its body expression, options, and cache configuration.
This allows the library to retrieve the original computation logic when a
lazy slot is accessed for the first time.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definitions

(cl-defstruct (lazy-slot-opts
               (:constructor lazy-slot--make-opts))
  "Configuration options for asynchronous lazy slot resolution.

This struct holds parameters that control the behavior of async slots,
such as when they should fetch, how long to wait, and retry policies.

Fields:
- `auto-fetch` (boolean): If `t`, the slot's value will be fetched
  asynchronously in the background immediately after the object is
  initialized. If `nil`, the fetch is delayed until the slot is first
  accessed via its getter. Defaults to `t`.
- `timeout` (number | nil): Maximum time in seconds to wait for the
  asynchronous result to resolve before signaling a timeout error.
  If `nil`, there is no explicit timeout. This integrates with `concur:timeout`.
- `retry` (plist | nil): A plist of options for `concur:retry`. If
  provided, the asynchronous computation will be retried automatically
  if it fails. Examples: `(:count 3 :delay 0.5)`. If `nil`, no retries.
  This specifies the retry policy for the underlying promise."
  (auto-fetch t :type boolean)
  (timeout nil :type (or null number))
  (retry nil :type (or null list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Helpers

(defun lazy-slot--log (level format-string &rest args)
  "Internal logging helper for the `lazy-slot` library.

This function dispatches log messages to `lazy-slot-log-hook` if it's
configured, allowing external modules to consume and display `lazy-slot`'s
internal messages.

Arguments:
- `LEVEL` (symbol): The severity of the message (e.g., `:debug`, `:info`,
  `:warn`, `:error`).
- `FORMAT-STRING` (string): The format string for the message.
- `ARGS` (list): Arguments for the format string.

Results:
- None."
  (apply #'run-hook-with-args 'lazy-slot-log-hook level format-string args))

(defun lazy-slot--register-definition (type slot def-plist)
  "Register the definition plist `DEF-PLIST` for SLOT on TYPE.

This function stores the metadata for a lazy slot, including its computation
body, configuration options, and caching details. This metadata is later
retrieved when the slot needs to be computed or its status queried.

Arguments:
- `TYPE` (symbol): The `cl-defstruct` type symbol (e.g., `my-struct`).
- `SLOT` (symbol): The lazy slot name (e.g., `data-cache`).
- `DEF-PLIST` (plist): The property list containing all definition details
  (e.g., `:is-async`, `:body`, `:cache-options`).

Results:
- The provided `DEF-PLIST`."
  (lazy-slot--log :debug "Registering lazy slot definition: Type=%S, Slot=%S, Def=%S"
                  type slot def-plist)
  (ht-set! lazy-slot--definitions (cons type slot) def-plist))

(defun lazy-slot--get-definition (type slot)
  "Retrieve the definition plist for SLOT on TYPE.

This function looks up the stored metadata for a given lazy slot, which was
registered during the `lazy-slots!` macro expansion.

Arguments:
- `TYPE` (symbol): The `cl-defstruct` type symbol.
- `SLOT` (symbol): The lazy slot name.

Results:
- The definition plist (a `plist`), or `nil` if no definition is found for
  the specified slot on that type."
  (ht-get lazy-slot--definitions (cons type slot)))

(defun lazy-slot--parse-opts (options)
  "Create a `lazy-slot-opts` struct from an OPTIONS plist or lambda.

This helper normalizes the `:options` argument provided in `lazy-slot-async!`
into a structured `lazy-slot-opts` object for easier access to configuration.

Arguments:
- `OPTIONS` (plist | function): A `plist` containing option keywords (e.g.,
  `:timeout`, `:auto-fetch`) or a zero-argument function that returns such a
  `plist`.

Results:
- A `lazy-slot-opts` struct populated with the parsed options."
  (let ((opts (if (functionp options) (funcall options) options)))
    (unless (or (null opts) (plistp opts))
      (lazy-slot--log :warn "lazy-slot: OPTIONS expected to be a plist or lambda. Got: %S" opts)
      (setq opts nil)) ; Fallback to nil if invalid
    (apply #'lazy-slot--make-opts (append opts nil)))) ; `append opts nil` ensures it's a valid plist even if opts is nil

(defun lazy-slot--substitute-anaphor (form anaphor replacement)
  "Recursively replace an ANAPHOR symbol in FORM with a REPLACEMENT symbol.

This function is used during macro expansion to replace the special `obj`
and `cb` symbols (anaphors) in the user-provided slot computation body
with the actual gensymmed variables used in the generated code.

Arguments:
- `FORM` (any): The Lisp form to traverse.
- `ANAPHOR` (symbol): The symbol to find (e.g., `obj`, `cb`).
- `REPLACEMENT` (symbol): The symbol to substitute with (e.g., `obj-sym-123`).

Results:
- A new form with substitutions applied."
  (cond
   ((eq form anaphor) replacement) ; Direct match, replace.
   ((atom form) form) ; Base case: atom, return as is.
   (t (cons (lazy-slot--substitute-anaphor (car form) anaphor replacement) ; Recurse into car
            (lazy-slot--substitute-anaphor (cdr form) anaphor replacement))))) ; Recurse into cdr

(defun lazy-slot--init-async-slot (obj slot opts promise-thunk-form)
  "Initialize an asynchronous SLOT on OBJ using PROMISE-THUNK-FORM and OPTS.

This function orchestrates the background computation for an async slot.
It takes the user-provided computation (wrapped in `promise-thunk-form`),
applies any `timeout` or `retry` policies, converts the resulting promise
into a `concur` future, and stores this future in the object's slot.
If `auto-fetch` is enabled, it immediately starts the computation.

Arguments:
- `OBJ` (cl-struct): The object instance whose slot is being initialized.
- `SLOT` (symbol): The slot name to initialize.
- `OPTS` (lazy-slot-opts): The parsed options for this async slot.
- `PROMISE-THUNK-FORM` (function): A zero-argument function (`lambda () ...`)
  that, when called, returns a `concur` promise representing the lazy
  slot's computation.

Results:
- The processed `concur` promise (which the future wraps)."
  (lazy-slot--log :debug "Initializing async slot %S on %S. Options: %S" slot obj opts)
  (let* ((base-promise (funcall promise-thunk-form)) ; Execute the thunk to get the raw promise.
         (timeout (lazy-slot-opts-timeout opts))
         (retry-opts (lazy-slot-opts-retry opts))
         (processed-promise base-promise))
    ;; Apply timeout to the promise if specified.
    (when timeout
      (lazy-slot--log :debug "  Applying timeout %S to promise for %S." timeout slot)
      (setq processed-promise (concur:timeout processed-promise timeout)))
    ;; Apply retry logic to the promise if specified.
    (when retry-opts
      (lazy-slot--log :debug "  Applying retry options %S to promise for %S." retry-opts slot)
      ;; `concur:retry` takes the thunk directly to re-execute on failure.
      (setq processed-promise (apply #'concur:retry promise-thunk-form retry-opts)))

    ;; Convert the processed promise into a future and store it in the slot.
    (let ((future (concur:from-promise processed-promise)))
      (lazy-slot--log :debug "  Storing future %S in slot %S for %S." future slot obj)
      (setf (cl-struct-slot-value (type-of obj) slot obj) future)
      ;; If auto-fetch is enabled, immediately trigger the computation.
      (when (lazy-slot-opts-auto-fetch opts)
        (lazy-slot--log :debug "  Auto-fetching future for slot %S on %S." slot obj)
        (concur:force future))) ; `concur:force` starts the promise resolution.
    processed-promise)) ; Return the promise, not the future, as `await` needs the promise.

(defun lazy-slot--await-slot (obj slot opts promise-thunk-form)
  "Synchronously fetch or return cached value of OBJ's SLOT.

This is the core blocking getter for asynchronous lazy slots. When an async
slot is accessed, this function checks its current state:
1. If the slot already holds a resolved value, it returns it directly.
2. If the slot holds a pending `concur` future, it blocks Emacs until that
   future resolves (or times out), then returns the resolved value.
3. If the slot is uninitialized, it calls `lazy-slot--init-async-slot` to
   start the computation and then blocks, waiting for the result.

Arguments:
- `OBJ` (cl-struct): The object instance.
- `SLOT` (symbol): The slot to await.
- `OPTS` (lazy-slot-opts): The parsed options for this operation.
- `PROMISE-THUNK-FORM` (function): A zero-argument function that returns a promise.

Results:
- The resolved value of the slot (blocking if necessary)."
  (lazy-slot--log :debug "Attempting to await slot %S on %S." slot obj)
  (let ((val (cl-struct-slot-value (type-of obj) slot obj)))
    (cond
     ;; If the slot already holds a future (meaning it's pending or resolved/rejected),
     ;; await its resolution.
     ((concur-future-p val)
      (lazy-slot--log :debug "  Slot %S on %S already holds a future. Awaiting it." slot obj)
      ;; `concur:await` blocks until the future resolves.
      ;; `car` is used because `concur:await` returns a list `(value . status)`.
      (car (concur:await (concur:force val) (lazy-slot-opts-timeout opts))))
     ;; If the slot has a non-nil value (meaning it's already resolved directly), return it.
     ((not (null val))
      (lazy-slot--log :debug "  Slot %S on %S already resolved directly. Returning value: %S." slot obj val)
      val)
     ;; If the slot is `nil` (uninitialized for async), start the computation and await.
     (t
      (lazy-slot--log :debug "  Slot %S on %S uninitialized. Initializing and awaiting." slot obj)
      (let ((promise (lazy-slot--init-async-slot obj slot opts promise-thunk-form)))
        (car (concur:await promise (lazy-slot-opts-timeout opts))))))))

(defun lazy-slot--get-cache-key (prefix type slot obj)
  "Generate a standardized cache key for a given slot.

This function constructs a unique key for `cacheus` based on the slot's
context. This key is used for memoization and cache invalidation.

Arguments:
- `PREFIX` (symbol): A symbol to distinguish different types of lazy slots
  in the cache (e.g., `'lazy-slot-sync`, `'lazy-slot-async`).
- `TYPE` (symbol): The `cl-defstruct` type symbol (e.g., `my-data-struct`).
- `SLOT` (symbol): The slot name (e.g., `computed-data`).
- `OBJ` (cl-struct): The object instance. `object-hash` is used to get a
  unique identifier for the object itself, ensuring different instances
  have different cache entries.

Results:
- A `list` suitable for use as a `cacheus` key. Example: `(lazy-slot-async my-data-struct computed-data 123456)`.
  This structure allows `cacheus` to effectively manage cached values keyed by
  the object instance and slot name."
  (list prefix type slot (object-hash obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Macros

;;;###autoload
(cl-defmacro lazy-slot-sync! (slot type body &key cache-options)
  "Define a lazy synchronous accessor for SLOT in struct TYPE.

This is a low-level macro, usually called by `lazy-slots!`. It defines a
getter function that computes the slot's value only on first access and caches
it directly in the slot. If `cache-options` are provided, the computation is
additionally memoized by `cacheus`.

Arguments:
- `SLOT` (symbol): The name of the slot to define (e.g., `computed-field`).
- `TYPE` (symbol): The `cl-defstruct` type symbol (e.g., `my-data-struct`).
- `BODY` (form): The Emacs Lisp expression that computes the slot's value.
  Within this `BODY`, the symbol `obj` is bound to the current object instance.
- `:cache-options` (plist, optional): A `plist` of options to pass directly
  to `cacheus-memoize!`. This enables advanced caching features like TTL,
  persistence, and versioning for the slot's computed value.

Results:
- A `progn` form defining the getter and setter functions for the slot.
  The getter will implement the lazy evaluation logic."
  (declare (indent 2)) 
  (let* ((getter (intern (format "%s-%s" type slot))) ; Generate standard getter name (e.g., `my-struct-computed-field`).
         (setter (intern (format "setf-%s-%s" type slot))) ; Generate standard setter name (e.g., `setf-my-struct-computed-field`).
         (obj-sym (gensym "obj-"))) 
    `(progn
       ;; Register the slot's definition for later introspection or re-fetching.
       (lazy-slot--register-definition ',type ',slot
                                       '(:is-async nil ; Mark as synchronous.
                                         :body ',body ; Store the original body for `force-fetch`.
                                         :cache-options ,cache-options)) ; Store cache options.

       (defun ,getter (,obj-sym)
         ,(format "Synchronous lazy getter for slot %S of type %S." slot type)
         ,(if cache-options
              ;; If cache-options are provided, use `cacheus-memoize!` for advanced caching.
              (let ((cacheus-fn-sym (gensym (format "lazy-%s-%s-" type slot))))
                `(progn
                   ;; Define a memoized function that computes the slot value.
                   (cacheus-memoize! ,cacheus-fn-sym (,obj-sym)
                     :key-fn (lambda (obj-arg) (lazy-slot--get-cache-key 'lazy-slot-sync ',type ',slot obj-arg))
                     :async nil ; Mark as synchronous for cacheus.
                     ,@cache-options ; Pass through user-defined cache options.
                     (let ((obj ,obj-sym)) ,body)) ; Bind `obj` anaphor within the memoized function.
                   ;; Call the memoized function to get the value.
                   (funcall ,cacheus-fn-sym ,obj-sym)))
            ;; If no cache-options, use simple in-slot caching (set-once behavior).
            `(or (cl-struct-slot-value ,type ',slot ,obj-sym) ; If already computed, return it.
                 (setf (cl-struct-slot-value ,type ',slot ,obj-sym) ; Otherwise, compute and store.
                       (let ((obj ,obj-sym)) ,body))))) ; Bind `obj` anaphor for computation.

       ;; Define a standard setter function.
       (unless (fboundp ',setter) ; Only define if not already present (e.g., from `cl-defstruct`).
         (cl-defun ,setter (val ,obj-sym)
           ,(format "Set the value of slot `%S' for an object of type `%S'." slot type)
           (setf (cl-struct-slot-value ,type ',slot ,obj-sym) val))))))

;;;###autoload
(cl-defmacro lazy-slot-async! (slot type fn-body &key options constructor cache-options)
  "Define a lazy asynchronous accessor for SLOT in struct TYPE.

This is a low-level macro, usually called by `lazy-slots!`. It defines a
getter function that manages asynchronous computation via `concur` futures.
The computation is triggered either on object initialization (`:auto-fetch`)
or on first access.

Arguments:
- `SLOT` (symbol): The name of the slot to define.
- `TYPE` (symbol): The `cl-defstruct` type symbol.
- `FN-BODY` (form): The Emacs Lisp expression that performs the asynchronous
  computation. This must be a form that typically calls a function taking a
  callback. Within `FN-BODY`, the special symbols `obj` (the object instance)
  and `cb` (the callback function to call with the result) are available as anaphors.
  Example: `(my-async-api-call (my-struct-id obj) cb)`.
- `:options` (plist, optional): A `plist` of options for `lazy-slot-opts`,
  controlling `auto-fetch`, `timeout`, and `retry` behavior for async resolution.
- `:constructor` (symbol, optional): The `cl-defstruct`'s constructor function
  (e.g., `make-my-struct`). If provided, `lazy-slot-async!` will advise this
  constructor to automatically initiate the async fetch if `:auto-fetch` is `t`.
  Defaults to `(intern (format \"make-%s\" type))`.
- `:cache-options` (plist, optional): Options for `cacheus-memoize!`, applied
  to the asynchronous computation. This allows caching of async results.

Results:
- A `progn` form defining the getter, setter, and an internal initializer function.
  The getter will block until the async value is resolved.
  An advice is added to the constructor to trigger `auto-fetch`.
"
  (declare (indent 2))
  (let* ((getter (intern (format "%s-%s" type slot)))
         (setter (intern (format "setf-%s-%s" type slot)))
         (init-fn (intern (format "%s--init-%s" type slot))) ; Internal function to initialize/re-fetch async slot
         (ctor (or constructor (intern (format "make-%s" type)))) ; Determine the constructor function.
         (obj-sym (gensym "obj-")) 
         (cb-sym (gensym "cb-")) 
         (cacheus-fn-sym (when cache-options (gensym (format "lazy-async-%s-%s-" type slot)))) ; Gensym for memoized function if caching.
         ;; This `promise-thunk-form` is a zero-argument function that, when executed,
         ;; either calls the memoized cacheus function (if caching is enabled) or
         ;; directly wraps the user's `FN-BODY` into a `concur` promise.
         (promise-thunk-form
          `(lambda ()
             ,(if cacheus-fn-sym
                  ;; If caching is enabled, the computation goes through cacheus-memoize!.
                  ;; The memoized function itself should return a promise.
                  `(funcall ',cacheus-fn-sym ,obj-sym)
                ;; If no caching, directly create a promise from the user's callback-style body.
                `(concur:from-callback
                  (lambda (,cb-sym)
                    ;; Substitute `obj` and `cb` anaphors in the user's `FN-BODY` with the actual gensyms.
                    ,(lazy-slot--substitute-anaphor
                      (lazy-slot--substitute-anaphor fn-body 'obj obj-sym)
                      'cb cb-sym)))))))
    `(progn
       ;; Register the slot's definition for later introspection or re-fetching.
       (lazy-slot--register-definition ',type ',slot
                                       '(:is-async t ; Mark as asynchronous.
                                         :fn-body ',fn-body ; Store original async body.
                                         :options ,options ; Store lazy-slot-opts.
                                         :cache-options ,cache-options ; Store cacheus options.
                                         :cacheus-fn-sym ,cacheus-fn-sym ; Store generated cacheus function symbol.
                                         :constructor ,ctor)) ; Store constructor for advice.

       ;; If `cache-options` are provided, define a `cacheus-memoize!` function.
       ;; This function will handle the caching of asynchronous results.
       ,@(when cacheus-fn-sym
           `((cacheus-memoize! ,cacheus-fn-sym (,obj-sym)
               :async t ; Crucially, tell cacheus this is an async memoization.
               :key-fn (lambda (obj-arg) (lazy-slot--get-cache-key 'lazy-slot-async ',type ',slot obj-arg))
               ,@cache-options ; Pass user-defined cache options.
               ;; The body of the memoized function must return a promise.
               (concur:from-callback (lambda (,cb-sym)
                                       ,(lazy-slot--substitute-anaphor
                                         (lazy-slot--substitute-anaphor fn-body 'obj obj-sym)
                                         'cb cb-sym))))))

       (defun ,getter (,obj-sym)
         ,(format "Asynchronous lazy getter for slot %S of type %S. Blocks until resolved." slot type)
         ;; When the getter is called, `lazy-slot--await-slot` handles the logic:
         ;; - If already resolved, return value.
         ;; - If pending, await its resolution.
         ;; - If uninitialized, trigger the async computation and await.
         (lazy-slot--await-slot ,obj-sym ',slot (lazy-slot--parse-opts ,options) ,promise-thunk-form))

       ;; Define a standard setter function for the slot.
       (unless (fboundp ',setter)
         (cl-defun ,setter (val ,obj-sym)
           ,(format "Set the value of slot `%S' for object `%S'." slot obj-sym)
           (setf (cl-struct-slot-value ,type ',slot ,obj-sym) val)))

       ;; Define an internal initializer function. This is called by the constructor advice.
       (defun ,init-fn (,obj-sym)
         ,(format "Initialize or re-fetch async slot %S for instance %S." slot obj-sym)
         ;; This directly calls `lazy-slot--init-async-slot` to trigger the background fetch.
         (lazy-slot--init-async-slot ,obj-sym ',slot (lazy-slot--parse-opts ,options) ,promise-thunk-form))

       ;; Add an advice to the struct's constructor. This ensures that if `:auto-fetch`
       ;; is enabled, the async computation starts immediately when a new object is created.
       (advice-add #',ctor :after
                   (lambda (&rest args)
                     ;; The newly created object is always the last argument passed to the constructor.
                     (let ((new-obj (car (last args))))
                       (when (typep new-obj ',type) ; Ensure it's an instance of the correct struct type.
                         (funcall #',init-fn new-obj)))))))) ; Call the internal initializer.

;;;###autoload
(cl-defmacro lazy-slots! (&key type eager async sync)
  "Define a set of accessor functions for a struct of `TYPE`.

This macro serves as the primary declarative interface for defining lazy and
eager slots. It consolidates the definition of various types of accessors
(standard eager, lazy synchronous, lazy asynchronous) for a single `cl-defstruct`
type into one convenient block.

Within the `BODY-FORM` of synchronous and asynchronous slot definitions,
you can use the special symbol `obj` to refer to the current object instance.
For asynchronous slots, `cb` is also available as the callback function.

Arguments:
- `:type` (symbol): The `cl-defstruct` type symbol for which slots are being defined.
- `:eager` (list, optional): A list of slot symbols for which standard (eager)
  accessors should be defined. These slots are like regular `cl-defstruct` slots.
  Each element is a slot symbol (e.g., `(field1 field2)`).
- `:async` (list, optional): A list of asynchronous lazy slot definitions.
  Each element is a list `(SLOT-NAME FN-BODY &rest PLIST-OPTS)`, where:
    - `SLOT-NAME`: The symbol for the async slot.
    - `FN-BODY`: The expression defining the asynchronous computation. `obj` and `cb`
      anaphors are available here. This body should eventually call `cb` with the result.
    - `PLIST-OPTS`: Optional `plist` for `lazy-slot-async!` (e.g., `:options`, `:cache-options`).
- `:sync` (list, optional): A list of synchronous lazy slot definitions.
  Each element is a list `(SLOT-NAME BODY-FORM &rest PLIST-OPTS)`, where:
    - `SLOT-NAME`: The symbol for the sync slot.
    - `BODY-FORM`: The expression that computes the slot's value. `obj` anaphor
      is available here.
    - `PLIST-OPTS`: Optional `plist` for `lazy-slot-sync!` (e.g., `:cache-options`).

Results:
- A `progn` form that defines all the specified accessor functions and registers
  their lazy evaluation logic.
"
  (declare (indent 1))
  `(progn
     ;; Define eager slots (standard getters and setters).
     ,@(--map `(progn
                (defun ,(intern (format "%s-%s" type it)) (obj)
                  ,(format "Eager getter for slot %S of type %S." it type)
                  (cl-struct-slot-value ,type ',it obj))
                (cl-defun ,(intern (format "setf-%s-%s" type it)) (val obj)
                  ,(format "Eager setter for slot %S of type %S." it type)
                  (setf (cl-struct-slot-value ,type ',it obj) val)))
              eager)
     ;; Define synchronous lazy slots using `lazy-slot-sync!`.
     ,@(--map (let* ((slot (car it)) ; Extract slot name.
                     (body (cadr it)) ; Extract computation body.
                     (opts (cddr it))) ; Extract additional options.
               `(lazy-slot-sync! ,slot ,type ,body ,@opts))
              sync)
     ;; Define asynchronous lazy slots using `lazy-slot-async!`.
     ,@(--map (let* ((slot (car it)) ; Extract slot name.
                     (fn-body (cadr it)) ; Extract async function body.
                     (opts (cddr it))) ; Extract additional options.
               `(lazy-slot-async! ,slot ,type ,fn-body ,@opts))
              async)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API for Slot Inspection and Manipulation

;;;###autoload
(defun lazy-slot-status (obj slot)
  "Return the status of a lazy SLOT on OBJ without triggering computation.

This function allows you to inspect the current state of a lazy slot's
value. It will not initiate any computation if the slot is `uninitialized`
or block if it's `pending`.

Arguments:
- `OBJ` (cl-struct): The object instance.
- `SLOT` (symbol): The lazy slot name.

Results:
- (symbol): One of:
  - `'uninitialized`: The slot's value has not yet been computed or set.
  - `'pending`: For asynchronous slots, the computation has started but not yet
    completed (the slot holds a `concur` future that is not yet resolved).
  - `'resolved`: The slot's value has been successfully computed and is available.
  - `'rejected`: For asynchronous slots, the computation failed (the `concur`
    future resolved to an error).
  - `'cancelled`: For asynchronous slots, the pending computation was explicitly
    cancelled."
  (lazy-slot--log :debug "Getting status for slot %S on %S." slot obj)
  (let* ((type (type-of obj))
         (current-val (cl-struct-slot-value type slot obj)))
    (cond
     ((null current-val) 'uninitialized) ; If the slot is literally nil, it's uninitialized.
     ((concur-future-p current-val) ; If it holds a future, check the future's status.
      (concur:status (concur-future-promise current-val)))
     (t 'resolved)))) ; If it holds a non-nil, non-future value, it's resolved.

;;;###autoload
(defun lazy-slot-force-fetch (obj slot &optional options)
  "Explicitly force or re-force computation of a lazy SLOT on OBJ.

This function is useful for eagerly loading a lazy slot's value in the
background, or for refreshing a potentially stale cached value.

Arguments:
- `OBJ` (cl-struct): The object instance.
- `SLOT` (symbol): The lazy slot name.
- `OPTIONS` (plist, optional): An optional `plist` of options that can
  override the original `lazy-slot-opts` for this specific fetch operation
  (e.g., to force `auto-fetch` or change `timeout` temporarily).

Results:
- (concur-promise): Returns the `concur` promise associated with the
  (re)computation of the slot's value. You can use `concur:await` on this
  promise if you need to block until the fetch is complete.
"
  (lazy-slot--log :info "Forcing fetch for slot %S on %S. Options: %S" slot obj options)
  (let* ((type (type-of obj))
         (definition (lazy-slot--get-definition type slot)))
    (unless definition
      (error "lazy-slot-force-fetch: Slot %S on type %S not defined via lazy-slot macros.
              Only slots defined with `lazy-slot-sync!` or `lazy-slot-async!` can be force-fetched."
             slot type))

    (let* ((is-async (plist-get definition :is-async))
           (body-form (plist-get definition :body)) ; For sync slots
           (fn-body-form (plist-get definition :fn-body)) ; For async slots
           (cacheus-fn (plist-get definition :cacheus-fn-sym)) ; Memoized function if caching
           (opts (lazy-slot--parse-opts (or options (plist-get definition :options)))) ; Parse and possibly override options
           (promise-thunk-form
            (lambda () 
              (if cacheus-fn
                  ;; If caching is enabled, use the memoized function.
                  (lazy-slot--log :debug "  Force-fetch using cacheus memoized function for %S." slot)
                  (funcall cacheus-fn obj)
                ;; If no caching, create a promise directly based on slot type.
                (if is-async
                    (progn
                      (lazy-slot--log :debug "  Force-fetch creating promise from async body for %S." slot)
                      (concur:from-callback
                       (lambda (cb)
                         (let ((obj obj) (cb cb)) ; bind anaphors
                           (eval fn-body-form)))))
                  (progn
                    (lazy-slot--log :debug "  Force-fetch creating resolved promise from sync body for %S." slot)
                    (concur:resolved! (let ((obj obj)) (eval body-form)))))))))
      ;; Initialize the async slot (which triggers the fetch via `lazy-slot--init-async-slot`).
      ;; The result is the promise managing the computation.
      (lazy-slot--init-async-slot obj slot opts promise-thunk-form))))

;;;###autoload
(defun lazy-slot-clear-cache (obj slot)
  "Clear the cached value of a lazy SLOT on OBJ.

This function invalidates any stored value for the specified lazy slot,
forcing its re-computation on the next access. For asynchronous slots,
it also attempts to cancel any ongoing background computation associated
with the slot. If `cacheus` is used, its corresponding cache entry is
invalidated.

Arguments:
- `OBJ` (cl-struct): The object instance.
- `SLOT` (symbol): The lazy slot name.

Results:
- None.
"
  (cl-block lazy-slot-clear-cache-block 
    (lazy-slot--log :info "Clearing cache for slot %S on %S." slot obj)
    (let* ((type (type-of obj))
           (current-val (cl-struct-slot-value type slot obj))
           (definition (lazy-slot--get-definition type slot)))

      (unless definition
        (lazy-slot--log :warn "No definition found for slot %S on type %S. Cannot clear cache."
                        slot type)
        (cl-return-from lazy-slot-clear-cache-block nil)) ; Exit if no definition.

      ;; If the slot holds a pending future, try to cancel it.
      (when (concur-future-p current-val)
        (lazy-slot--log :info "Cancelling ongoing computation for slot %S on %S: %S." slot obj current-val)
        (let ((promise (concur-future-promise current-val)))
          (when promise (concur:cancel promise "Cache cleared for lazy slot"))))

      ;; If the slot uses `cacheus` memoization, invalidate the cache entry.
      (when (plist-get definition :cache-options)
        (let* ((is-async (plist-get definition :is-async))
               (prefix (if is-async 'lazy-slot-async 'lazy-slot-sync))
               (key (lazy-slot--get-cache-key prefix type slot obj)))
          (lazy-slot--log :info "Invalidating cacheus key for slot %S on %S: %S" slot obj key)
          (cacheus-invalidate! key)))

      ;; Finally, set the slot value in the object itself to nil to force re-computation.
      (setf (cl-struct-slot-value type slot obj) nil)
      (lazy-slot--log :info "Cleared local slot %S on %S to nil." slot obj))))

;;;###autoload
(defun lazy-slot-value (obj slot)
  "Return the resolved value of a lazy SLOT on OBJ if resolved.

This function provides a non-blocking way to retrieve a lazy slot's value.
It will *not* trigger the computation if the slot is uninitialized, nor will it
block if the computation is still pending. It returns the value only if the
underlying promise for an async slot is already in the 'resolved state, or if
a synchronous slot has already been computed.

Arguments:
- `OBJ` (cl-struct): The object instance.
- `SLOT` (symbol): The lazy slot name.

Results:
- (any): The resolved value of the slot, or `nil` if the slot is `uninitialized`,
  `pending`, `rejected`, or `cancelled`."
  (lazy-slot--log :debug "Getting resolved value for slot %S on %S (non-blocking)." slot obj)
  (let* ((type (type-of obj))
         (current-val (cl-struct-slot-value type slot obj)))
    (cond
     ((concur-future-p current-val) ; If it's a future, check its resolved value.
      (concur:value (concur-future-promise current-val)))
     ((not (null current-val)) current-val) ; If it's a non-nil direct value, it's resolved.
     (t nil)))) ; Otherwise, it's not resolved (uninitialized, pending, rejected, cancelled).

;;;###autoload
(defun lazy-slot-error (obj slot)
  "Return the error value of a lazy SLOT on OBJ if rejected.

This function allows inspecting the error state of a lazy slot. It will
not trigger computation or block. It returns the error object only if the
underlying promise for an async slot is in the 'rejected state.

Arguments:
- `OBJ` (cl-struct): The object instance.
- `SLOT` (symbol): The lazy slot name.

Results:
- (any): The error value (e.g., an Emacs Lisp error object, string), or `nil`
  if the slot is not in a 'rejected state."
  (lazy-slot--log :debug "Getting error value for slot %S on %S (non-blocking)." slot obj)
  (let* ((type (type-of obj))
         (current-val (cl-struct-slot-value type slot obj)))
    (cond
     ((concur-future-p current-val) ; If it's a future, get its error value.
      (concur:error-value (concur-future-promise current-val)))
     (t nil)))) ; Otherwise, no error associated (or not an async slot).

(provide 'lazy-slot)
;;; lazy-slot.el ends here
