;;; lazy-slot.el --- Async slot resolution with advanced caching -*- lexical-binding: t; -*-

;; Author: Christian White <christiantwhite@protonmail.com>
;; Maintainer: Christian White <christiantwhite@protonmail.com>
;; Version: 0.9.1
;; Keywords: lisp, programming, async, lazy, cache, struct
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5") (concur "1.0") (dash "2.18.0") (ht "2.3") (cacheus "1.0"))
;; Homepage: https://github.com/ctwhite/lazy-slot

;;; Commentary:
;;
;; This file provides macros and functions for defining and using "lazy" slots
;; in Emacs Lisp objects (specifically, those defined with `cl-defstruct`).
;; Lazy slots delay the computation of their values until they are first
;; accessed. The result is then cached for subsequent accesses.
;;
;; This module supports both synchronous and asynchronous lazy evaluation and
;; integrates with `cacheus` to provide advanced caching capabilities,
;; including Time-To-Live (TTL), persistence, and versioning for the
;; computed slot values.
;;
;; It uses the `concur:` prefixed API from `concur.el` for promise-based
;; asynchrony.
;;
;; **Key Concepts:**
;;
;; - **Lazy Evaluation:** The value of a lazy slot is computed only when it is
;;   first accessed. For synchronous slots, the result is stored directly
;;   in the slot.
;;
;; - **Asynchronous Slots:** For I/O or long-running tasks, this module uses
;;   `concur` futures to manage the result without blocking Emacs. The actual
;;   value is computed in the background. Accessing the slot will block until
;;   the value is ready.
;;
;; - **Implicit Context:** In the `lazy-slots!` macro, you can use the symbol
;;   `obj` within the body of a `:sync` slot definition to refer to the
;;   current object instance. For `:async` slots, both `obj` and `cb` (the
;;   callback function) are available.
;;
;; - **Caching Integration:** By specifying `:cache-options`, users can leverage
;;   `cacheus-memoize!` to cache the results of slot computations, complete
;;   with TTL, versioning, and persistence.
;;
;; **Core Functionality:**
;;
;; - `lazy-slots!`: A convenience macro to define eager, async, and sync
;;   accessors for a struct. This is the primary entry point for users.
;;
;; - `lazy-slot-sync!` & `lazy-slot-async!`: The underlying macros for defining
;;   individual lazy slots.
;;
;; - `lazy-slot-status`, `lazy-slot-force-fetch`, `lazy-slot-clear-cache`:
;;   A public API for inspecting and manipulating the state of lazy slots.

;;; Code:

(require 'cl-lib)
(require 'concur)
(require 'dash)
(require 'ht)
(require 'cacheus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization and Internal State

(defconst lazy-slot-version "0.9.1"
  "Version of lazy-slot.el.")

(defgroup lazy-slot nil
  "Lazy struct slot library for Emacs."
  :group 'lisp)

(defcustom lazy-slot-log-hook nil
  "Hook run for logging messages within the Lazy Slot library.

Functions added to this hook should accept a LEVEL symbol, a
format string FMT, and any additional ARGS.

Arguments:
- `LEVEL` (symbol): The log level, e.g., `:debug`, `:info`, `:warn`, `:error`.
- `FMT` (string): The format-control string for the message.
- `ARGS` (rest): Arguments for the format string."
  :type 'hook
  :group 'lazy-slot)

;; Internal registry mapping `(TYPE . SLOT)` to its definition details.
(defvar lazy-slot--definitions (ht-create)
  "Internal hash table for storing lazy slot definitions.
Each key is a cons cell `(TYPE . SLOT)` and each value is a
property list containing the details of the lazy slot's definition,
such as its body, options, and cache configuration.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definitions

(cl-defstruct (lazy-slot-opts
               (:constructor lazy-slot--make-opts))
  "Configuration options for asynchronous lazy slot resolution.

Fields:
- `auto-fetch` (boolean): If t, fetch value when the object is initialized.
- `timeout` (number | nil): Max time in seconds to wait for the async result.
- `retry` (plist | nil): A plist of options for `concur:retry`."
  (auto-fetch t :type boolean)
  (timeout nil :type (or null number))
  (retry nil :type (or null list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Helpers

(defun lazy-slot--log (level format-string &rest args)
  "Internal logging helper.
Arguments:
- LEVEL (symbol): The severity of the message (e.g., `:info`, `:warn`).
- FORMAT-STRING (string): The format string for the message.
- ARGS (list): Arguments for the format string.
Results:
- None."
  (apply #'run-hook-with-args 'lazy-slot-log-hook level format-string args))

(defun lazy-slot--register-definition (type slot def-plist)
  "Register the definition plist `DEF-PLIST` for SLOT on TYPE.
Arguments:
- TYPE (symbol): The struct type.
- SLOT (symbol): The slot name.
- DEF-PLIST (plist): The property list containing definition details.
Results:
- The provided `DEF-PLIST`."
  (ht-set! lazy-slot--definitions (cons type slot) def-plist))

(defun lazy-slot--get-definition (type slot)
  "Retrieve the definition plist for SLOT on TYPE.
Arguments:
- TYPE (symbol): The struct type.
- SLOT (symbol): The slot name.
Results:
- The definition plist, or nil if not found."
  (ht-get lazy-slot--definitions (cons type slot)))

(defun lazy-slot--parse-opts (options)
  "Create a `lazy-slot-opts` struct from an OPTIONS plist or lambda.
Arguments:
- OPTIONS (plist | function): A plist or a function that returns a plist.
Results:
- A `lazy-slot-opts` struct."
  (let ((opts (if (functionp options) (funcall options) options)))
    (unless (or (null opts) (plistp opts))
      (warn "lazy-slot: OPTIONS expected to be a plist or lambda. Got: %S" opts)
      (setq opts nil))
    (apply #'lazy-slot--make-opts (append opts nil))))

(defun lazy-slot--init-async-slot (obj slot opts promise-thunk-form)
  "Initialize async SLOT on OBJ using PROMISE-THUNK-FORM and OPTS.
This function creates a promise, wraps it with timeout and retry
logic, converts it to a future, and stores it in the object's slot.
`PROMISE-THUNK-FORM` is the raw form (e.g., a lambda) that should
return a promise.

Arguments:
- OBJ (cl-struct): The object instance.
- SLOT (symbol): The slot to initialize.
- OPTS (lazy-slot-opts): The parsed options for this operation.
- PROMISE-THUNK-FORM (function): A zero-argument function that returns a promise.
Results:
- The processed `concur` promise."
  (let* ((base-promise (funcall promise-thunk-form obj)) ; Pass obj to the promise-thunk-form
         (timeout (lazy-slot-opts-timeout opts))
         (retry-opts (lazy-slot-opts-retry opts))
         (processed-promise base-promise))
    (when timeout (setq processed-promise (concur:timeout processed-promise timeout)))
    ;; Use the base-promise for retries, but pass a THUNK that re-evaluates it
    (when retry-opts
      (setq processed-promise (apply #'concur:retry
                                     (lambda () (funcall promise-thunk-form obj)) ; Re-evaluate promise-thunk-form on retry
                                     retry-opts)))
    (let ((future (concur:from-promise processed-promise)))
      (setf (cl-struct-slot-value (type-of obj) slot obj) future)
      (when (lazy-slot-opts-auto-fetch opts) (concur:force future))
      processed-promise)))

(defun lazy-slot--await-slot (obj slot opts promise-thunk-form)
  "Synchronously fetch or return cached value of OBJ's SLOT.
This is the core blocking logic for async slot getters. It handles
the three main states: value is a future, value is already resolved,
or the slot is uninitialized.
Arguments:
- OBJ (cl-struct): The object instance.
- SLOT (symbol): The slot to await.
- OPTS (lazy-slot-opts): The parsed options for this operation.
- PROMISE-THUNK-FORM (form): A form that, when evaluated with `obj`, returns a promise.
Results:
- The resolved value of the slot."
  (let ((val (cl-struct-slot-value (type-of obj) slot obj)))
    (cond
     ((concur-future-p val)
      (car (concur:await (concur:force val) (lazy-slot-opts-timeout opts) t)))
     ;; Slot already has a non-future, non-nil value.
     ((not (null val)) val)
     ;; Slot is uninitialized (nil).
     (t
      (let ((promise (lazy-slot--init-async-slot obj slot opts promise-thunk-form)))
        (car (concur:await promise (lazy-slot-opts-timeout opts) t)))))))

(defun lazy-slot--get-cache-key (prefix type slot obj)
  "Generate a standardized cache key for a given slot.
Arguments:
- PREFIX (symbol): A symbol to distinguish sync/async, e.g., 'lazy-slot-sync.
- TYPE (symbol): The struct type.
- SLOT (symbol): The slot name.
- OBJ (cl-struct): The object instance.
Results:
- A list suitable for use as a `cacheus` key."
  (list prefix type slot (object-hash obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Macros

;;;###autoload
(cl-defmacro lazy-slot-sync! (slot type body &key cache-options)
  "Define a lazy synchronous accessor for SLOT in struct TYPE.
This is a low-level macro, usually called by `lazy-slots!`.
The `BODY` form is evaluated in a context where `obj` is bound to
the object instance.

Arguments:
- `SLOT` (symbol): The slot name.
- `TYPE` (symbol): The `cl-defstruct` type.
- `BODY` (form): The computation expression, which can refer to `obj`.
- `:cache-options` (plist): Optional. Options for `cacheus-memoize!`.
Results:
- A `progn` form defining the getter and setter functions."
  (declare (indent 2) (debug t))
  (let* ((getter (intern (format "%s-%s" type slot)))
         (setter (intern (format "setf-%s-%s" type slot)))
         (obj-sym (gensym "obj"))) ; Generate a unique symbol for the obj parameter
    `(progn
       ;; Register the definition for introspection by other functions.
       (lazy-slot--register-definition ',type ',slot
                                       '(:is-async nil
                                         :body ',body ; Quote body for storage
                                         :cache-options ,cache-options))

       ;; Define the main getter function.
       (defun ,getter (,obj-sym)
         ;; Corrected: Use `format` to construct the docstring at runtime.
         (format "Synchronous lazy getter for slot %S of type %S." ',slot ',type)
         ,(if cache-options
              ;; If caching, the memoized function is defined outside and called.
              ;; The memoized function itself handles the (obj) argument.
              (let ((cacheus-fn-sym (gensym (format "lazy-%s-%s-" type slot))))
                `(progn
                   (cacheus-memoize! ,cacheus-fn-sym (,obj-sym)
                     :key-fn (lambda (obj-arg) (lazy-slot--get-cache-key 'lazy-slot-sync ',type ',slot obj-arg))
                     ,@cache-options
                     (let ((obj ,obj-sym)) ,body)) ; Bind obj-sym to obj in body
                   (funcall ,cacheus-fn-sym ,obj-sym)))
            ;; If no caching, handle the non-memoized logic directly.
            `(or (cl-struct-slot-value ,type ',slot ,obj-sym)
                 (setf (cl-struct-slot-value ,type ',slot ,obj-sym)
                       (let ((obj ,obj-sym)) ,body))))) ; Bind obj-sym to obj in body

       ;; Define a standard setter, unless one already exists.
       (unless (fboundp ',setter)
         (cl-defun ,setter (val ,obj-sym)
           ;; Corrected: Use `format` to construct the docstring at runtime.
           (format "Set the value of slot `%S' for an object of type `%S'." ',slot ',type)
           (setf (cl-struct-slot-value ,type ',slot ,obj-sym) val))))))

;;;###autoload
(cl-defmacro lazy-slot-async! (slot type fn-body &key options constructor cache-options)
  "Define a lazy asynchronous accessor for SLOT in struct TYPE.
This is a low-level macro, usually called by `lazy-slots!`.
`FN-BODY` must be a lambda of the form `(lambda (obj callback) ...)`
or a form that evaluates to one.

Arguments:
- `SLOT` (symbol): The slot name.
- `TYPE` (symbol): The `cl-defstruct` type.
- `FN-BODY` (form): A lambda form `(lambda (obj cb) ...)` for computation, or a form
  that when evaluated produces such a lambda.
- `:options` (plist): Options for `lazy-slot-opts` (e.g., :timeout, :auto-fetch).
- `:constructor` (symbol): The struct's constructor, used to hook initialization.
- `:cache-options` (plist): Options for `cacheus-memoize!`.
Results:
- A `progn` form defining the getter, setter, and initializer functions."
  (declare (indent 2) (debug t))
  (let* ((getter (intern (format "%s-%s" type slot)))
         (setter (intern (format "setf-%s-%s" type slot)))
         (init-fn (intern (format "%s--init-%s" type slot)))
         (ctor (or constructor (intern (format "make-%s" type))))
         (obj-sym (gensym "obj")) ; Unique symbol for object parameter
         (cacheus-fn-sym (when cache-options (gensym (format "lazy-async-%s-%s-" type slot))))
         ;; The promise-thunk-form is now the raw lambda or a form that *produces* a lambda.
         ;; lazy-slot--init-async-slot and lazy-slot--await-slot will funcall this form.
         (promise-thunk-form
          (if cacheus-fn-sym
              `(funcall ',cacheus-fn-sym ,obj-sym) ; If caching, call the memoized function.
            ;; If not caching, create a lambda for the promise generation that takes obj as arg
            ;; and in turn calls the user's fn-body with obj and a callback.
            `(lambda (,obj-sym)
               (concur:from-callback (lambda (cb) (funcall ,fn-body ,obj-sym cb)))))))
    `(progn
       ;; Register the definition for introspection.
       (lazy-slot--register-definition ',type ',slot
                                       '(:is-async t
                                         :fn-body ',fn-body ; Quote fn-body for storage
                                         :options ,options
                                         :cache-options ,cache-options
                                         :cacheus-fn-sym ,cacheus-fn-sym
                                         :constructor ,ctor))

       ;; Define the memoized function if caching is enabled.
       ,@(when cacheus-fn-sym
           `((cacheus-memoize! ,cacheus-fn-sym (,obj-sym)
               :async t
               :key-fn (lambda (obj-arg) (lazy-slot--get-cache-key 'lazy-slot-async ',type ',slot obj-arg))
               ,@cache-options
               ;; The body for cacheus must return the promise from the user's FN-BODY.
               (concur:from-callback (lambda (cb) (funcall ,fn-body ,obj-sym cb))))))

       ;; Define the main blocking getter function.
       (defun ,getter (,obj-sym)
         ;; Corrected: Use `format` to construct the docstring at runtime.
         (format "Asynchronous lazy getter for slot %S. Blocks until resolved and returns the value." ',slot)
         (lazy-slot--await-slot ,obj-sym ',slot (lazy-slot--parse-opts ,options) ,promise-thunk-form))

       ;; Define a standard setter.
       (unless (fboundp ',setter)
         (cl-defun ,setter (val ,obj-sym)
           ;; Corrected: Use `format` to construct the docstring at runtime.
           (format "Set the value of slot `%S' for an object of type `%S'." ',slot ',type)
           (setf (cl-struct-slot-value ,type ',slot ,obj-sym) val)))

       ;; Define the initializer function, which starts the async computation.
       (defun ,init-fn (,obj-sym)
         ;; Corrected: Use `format` to construct the docstring at runtime.
         (format "Initialize or re-fetch the asynchronous slot %S for instance %S." ',slot ',obj-sym)
         (lazy-slot--init-async-slot ,obj-sym ',slot (lazy-slot--parse-opts ,options) ,promise-thunk-form))

       ;; Advise the constructor to call the initializer automatically.
       (advice-add #',ctor :after
                   (lambda (&rest args)
                     ;; The return value of the constructor is the new object, which is the last arg.
                     (let ((new-obj (car (last args))))
                       (when (typep new-obj ',type)
                         (funcall #',init-fn new-obj))))))))

;;;###autoload
(cl-defmacro lazy-slots! (&key type eager async sync)
  "Define a set of accessor functions for a struct of `TYPE`.
This macro allows defining standard (eager) slots, lazy
synchronous slots, and lazy asynchronous slots in a declarative
way. Within the bodies of slots, you can use the symbol `obj` to
refer to the object instance and `cb` (for async slots) for the
callback.

Example Use for Synchronous (implicit `obj`):
  (lazy-slots!
    :type my-struct
    :sync ((full-name (concat (my-struct-name obj) \" (\" (my-struct-id obj) \")\"))))
  ;; In this case, `obj` inside the `concat` refers to the `my-struct` instance.

Example Use for Asynchronous (explicit `lambda (obj cb)`):
  (lazy-slots!
    :type my-struct
    :async ((data
              (lambda (obj cb)
                (message \"Fetching data for ID: %S\" (my-struct-id obj))
                (run-at-time
                 \"1s\" nil
                 (lambda ()
                   (funcall cb (format \"Data for ID: %S (Fetched at %s)\"
                                       (my-struct-id obj) (current-time-string))))))
              :options (:auto-fetch t :timeout 5.0)
              :cache-options (:ttl 3600 :version \"1.0\"))))
  ;; Here, `obj` and `cb` are explicit parameters to your lambda.

Arguments:
- `:type` (symbol): The `cl-defstruct` type.
- `:eager` (list): A list of slot symbols for standard (eager) accessors.
- `:async` (list): A list of async slot definitions.
  Each element is `(SLOT-NAME BODY-FORM &rest PLIST-OPTS)`. `BODY-FORM`
  should be a lambda `(lambda (obj cb) ...)`, or a form that evaluates to one.
- `:sync` (list): A list of sync slot definitions.
  Each element is `(SLOT-NAME BODY-FORM &rest PLIST-OPTS)`. `BODY-FORM`
  can directly use the `obj` anaphor.
Results:
- A `progn` form defining all specified accessors."
  (declare (indent 1))
  `(progn
     ;; Generate standard accessors for eager slots.
     ,@(--map `(progn
                (defun ,(intern (format "%s-%s" type it)) (obj)
                  (format "Eager getter for slot %S of type %S." ',it ',type)
                  (cl-struct-slot-value ,type ',it obj))
                (cl-defun ,(intern (format "setf-%s-%s" type it)) (val obj)
                  (format "Eager setter for slot %S of type %S." ',it ',type)
                  (setf (cl-struct-slot-value ,type ',it obj) val)))
              eager)

     ;; Generate lazy synchronous accessors.
     ,@(--map (let* ((slot (car it))
                     (body (cadr it))
                     (opts (cddr it)))
               ;; The body is now a raw form, not a lambda itself for sync slots.
               `(lazy-slot-sync! ,slot ,type ,body ,@opts))
              sync)

     ;; Generate lazy asynchronous accessors.
     ,@(--map (let* ((slot (car it))
                     (fn-body (cadr it))
                     (opts (cddr it)))
               ;; fn-body is the lambda (lambda (obj cb) ...), pass it directly.
               `(lazy-slot-async! ,slot ,type ,fn-body ,@opts))
              async)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API for Slot Inspection and Manipulation

;;;###autoload
(defun lazy-slot-status (obj slot)
  "Return the status of a lazy SLOT on OBJ without triggering computation.
Arguments:
- `OBJ` (cl-struct): The object instance.
- `SLOT` (symbol): The lazy slot name.
Results:
- (symbol): One of 'uninitialized, 'pending, 'resolved, 'rejected, or 'cancelled."
  (let* ((type (type-of obj))
         (current-val (cl-struct-slot-value type slot obj)))
    (cond
     ((null current-val) 'uninitialized)
     ((concur-future-p current-val)
      (concur:status (concur-future-promise current-val)))
     (t 'resolved))))

;;;###autoload
(defun lazy-slot-force-fetch (obj slot &optional options)
  "Explicitly force or re-force computation of a lazy SLOT on OBJ.
If the slot was already resolved, this will re-trigger the computation.
If the slot holds a pending future, it returns that future.
If the slot is uninitialized, it initializes and fetches it.
`OPTIONS` (plist) can override original `lazy-slot-opts` for this fetch.

Arguments:
- `OBJ` (cl-struct): The object instance.
- `SLOT` (symbol): The lazy slot name.
- `OPTIONS` (plist | nil): Optional override for `lazy-slot-opts`.
Results:
- (concur-promise): A promise for the slot's value."
  (let* ((type (type-of obj))
         (definition (lazy-slot--get-definition type slot)))
    (unless definition
      (error "lazy-slot-force-fetch: Slot %S on type %S not defined via lazy-slot macros"
             slot type))

    (let* ((is-async (plist-get definition :is-async))
           (body-form (plist-get definition :body)) ; For sync
           (fn-body-form (plist-get definition :fn-body)) ; For async
           (cacheus-fn (plist-get definition :cacheus-fn-sym))
           (opts (lazy-slot--parse-opts (or options (plist-get definition :options))))
           (promise-thunk-form
            (lambda (obj-arg) ; This thunk itself takes obj as an arg
              (if cacheus-fn
                  (funcall cacheus-fn obj-arg)
                (if is-async
                    (concur:from-callback (lambda (cb) (funcall fn-body-form obj-arg cb)))
                  ;; For sync slots, we create a resolved promise from evaluating the body.
                  ;; The `obj` in `(let ((obj obj-arg)) (eval body-form))` now correctly refers to obj-arg.
                  (concur:resolved! (let ((obj obj-arg)) (eval body-form))))))))
      (lazy-slot--init-async-slot obj slot opts promise-thunk-form))))

;;;###autoload
(defun lazy-slot-clear-cache (obj slot)
  "Clear the cached value of a lazy SLOT on OBJ.
This forces re-computation on the next access. It attempts to
cancel any ongoing async computation and clears the slot's value
on the object. If the slot uses `cacheus`, its entry is
invalidated.

Arguments:
- `OBJ` (cl-struct): The object instance.
- `SLOT` (symbol): The lazy slot name.
Results:
- None."
  (let* ((type (type-of obj))
         (current-val (cl-struct-slot-value type slot obj))
         (definition (lazy-slot--get-definition type slot)))

    (unless definition
      (lazy-slot--log :warn "No definition found for slot %S on type %S. Cannot clear cacheus."
                      slot type))

    ;; 1. Cancel any ongoing future associated with the slot.
    (when (concur-future-p current-val)
      (lazy-slot--log :info "Cancelling ongoing computation for slot %S on %S." slot obj)
      (let ((promise (concur-future-promise current-val)))
        (when promise (concur:cancel promise "Cache cleared for lazy slot"))))

    ;; 2. Clear the `cacheus-memoize` cache if applicable by invalidating its key.
    (when (and definition (plist-get definition :cache-options))
      (let* ((is-async (plist-get definition :is-async))
             (prefix (if is-async 'lazy-slot-async 'lazy-slot-sync))
             (key (lazy-slot--get-cache-key prefix type slot obj)))
        (lazy-slot--log :info "Invalidating cacheus key for slot %S on %S: %S" slot obj key)
        (cacheus-invalidate! key)))

    ;; 3. Clear the local slot value to ensure re-initialization on next access.
    (setf (cl-struct-slot-value type slot obj) nil)
    (lazy-slot--log :info "Cleared local slot %S on %S." slot obj)))

;;;###autoload
(defun lazy-slot-value (obj slot)
  "Return the resolved value of a lazy SLOT on OBJ if resolved.
This function does not trigger computation or wait for pending
promises. It returns the value only if the underlying promise
is in the 'resolved state.

Arguments:
- `OBJ` (cl-struct): The object instance.
- `SLOT` (symbol): The lazy slot name.
Results:
- (any): The resolved value, or nil if not resolved."
  (let* ((type (type-of obj))
         (current-val (cl-struct-slot-value type slot obj)))
    (cond
     ((concur-future-p current-val)
      (concur:value (concur-future-promise current-val)))
     ;; It's not a future, so it must be a resolved sync value.
     ((not (null current-val)) current-val)
     (t nil))))

;;;###autoload
(defun lazy-slot-error (obj slot)
  "Return the error value of a lazy SLOT on OBJ if rejected.
This function does not trigger computation or wait. It returns
the error only if the underlying promise is in the 'rejected state.

Arguments:
- `OBJ` (cl-struct): The object instance.
- `SLOT` (symbol): The lazy slot name.
Results:
- (any): The error value, or nil if not in a rejected state."
  (let* ((type (type-of obj))
         (current-val (cl-struct-slot-value type slot obj)))
    (cond
     ((concur-future-p current-val)
      (concur:error-value (concur-future-promise current-val)))
     (t nil))))

(provide 'lazy-slot)
;;; lazy-slot.el ends here
