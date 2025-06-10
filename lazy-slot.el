;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; ... (omitted for brevity)

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

(defgroup lazy-slot nil "Lazy struct slot library for Emacs." :group 'lisp)

(defcustom lazy-slot-log-hook nil
  "Hook run for logging messages within the Lazy Slot library."
  :type 'hook
  :group 'lazy-slot)

(defvar lazy-slot--definitions (ht-create)
  "Internal hash table for storing lazy slot definitions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definitions

(cl-defstruct (lazy-slot-opts (:constructor lazy-slot--make-opts))
  "Configuration options for asynchronous lazy slot resolution."
  (auto-fetch t :type boolean)
  (timeout nil :type (or null number))
  (retry nil :type (or null list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Helpers

(defun lazy-slot--log (level format-string &rest args)
  "Internal logging helper for the `lazy-slot` library."
  (apply #'run-hook-with-args 'lazy-slot-log-hook level format-string args))

(defun lazy-slot--register-definition (type slot def-plist)
  "Register the definition plist `DEF-PLIST` for SLOT on TYPE."
  (lazy-slot--log :debug "Registering lazy slot: Type=%S, Slot=%S" type slot)
  (ht-set! lazy-slot--definitions (cons type slot) def-plist))

(defun lazy-slot--get-definition (type slot)
  "Retrieve the definition plist for SLOT on TYPE."
  (ht-get lazy-slot--definitions (cons type slot)))

(defun lazy-slot--parse-opts (options)
  "Create a `lazy-slot-opts` struct from an OPTIONS plist or lambda."
  (let ((opts (if (functionp options) (funcall options) options)))
    (unless (or (null opts) (plistp opts))
      (lazy-slot--log :warn "lazy-slot: OPTIONS expected to be a plist. Got: %S" opts)
      (setq opts nil))
    (apply #'lazy-slot--make-opts (append opts nil))))

(defun lazy-slot--substitute-anaphor (form anaphor replacement)
  "Recursively replace ANAPHOR symbol in FORM with a REPLACEMENT symbol."
  (cond
   ((equal form anaphor) replacement)
   ((atom form) form)
   (t (cons (lazy-slot--substitute-anaphor (car form) anaphor replacement)
            (lazy-slot--substitute-anaphor (cdr form) anaphor replacement)))))

(defun lazy-slot--init-async-slot (obj slot opts promise-thunk-form)
  "Initialize an asynchronous SLOT on OBJ using PROMISE-THUNK-FORM and OPTS."
  (lazy-slot--log :debug "Initializing async slot %S on %S. Options: %S" slot obj opts)
  (let* ((base-promise (funcall promise-thunk-form obj))
         (timeout (lazy-slot-opts-timeout opts))
         (retry-opts (lazy-slot-opts-retry opts))
         (processed-promise base-promise))
    (when timeout
      (setq processed-promise (concur:timeout processed-promise timeout)))
    (when retry-opts
      (setq processed-promise (apply #'concur:retry
                                     (lambda () (funcall promise-thunk-form obj))
                                     retry-opts)))
    (let ((future (concur:from-promise processed-promise)))
      (setf (cl-struct-slot-value (type-of obj) slot obj) future)
      (when (lazy-slot-opts-auto-fetch opts)
        (concur:force future)))
    processed-promise))

(defun lazy-slot--await-slot (obj slot opts promise-thunk-form)
  "Synchronously fetch or return cached value of OBJ's SLOT."
  (lazy-slot--log :debug "Awaiting slot %S on %S" slot obj)
  (let ((val (cl-struct-slot-value (type-of obj) slot obj)))
    (cond
     ((concur-future-p val)
      (lazy-slot--log :debug "  Slot holds a future. Awaiting it.")
      (car (concur:await (concur:force val) (lazy-slot-opts-timeout opts))))
     ((not (null val))
      (lazy-slot--log :debug "  Slot already resolved directly.")
      val)
     (t
      (lazy-slot--log :debug "  Slot uninitialized. Initializing and awaiting.")
      (let ((promise (lazy-slot--init-async-slot obj slot opts promise-thunk-form)))
        (car (concur:await promise (lazy-slot-opts-timeout opts))))))))

(defun lazy-slot--get-cache-key (prefix type slot obj)
  "Generate a standardized cache key for a given slot."
  (list prefix type slot (object-hash obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Macros

;;;###autoload
(cl-defmacro lazy-slot-sync! (slot type body &key cache-options)
  "Define a lazy synchronous accessor for SLOT in struct TYPE."
  (declare (indent 2))
  (let* ((getter (intern (format "%s-%s" type slot)))
         (setter (intern (format "setf-%s-%s" type slot)))
         (obj-sym (gensym "obj-")))
    `(progn
       (lazy-slot--register-definition ',type ',slot
                                       '(:is-async nil :body ',body
                                         :cache-options ,cache-options))
       (defun ,getter (,obj-sym)
         ,(format "Synchronous lazy getter for slot %S of type %S." slot type)
         ,(if cache-options
              (let ((cacheus-fn-sym (gensym (format "lazy-%s-%s-" type slot))))
                `(progn
                   (cacheus-memoize! ,cacheus-fn-sym (,obj-sym)
                     :key-fn (lambda (obj-arg) (lazy-slot--get-cache-key 'lazy-slot-sync ',type ',slot obj-arg))
                     ,@cache-options
                     (let ((obj ,obj-sym)) ,body))
                   (funcall ,cacheus-fn-sym ,obj-sym)))
            `(or (cl-struct-slot-value ,type ',slot ,obj-sym)
                 (setf (cl-struct-slot-value ,type ',slot ,obj-sym)
                       (let ((obj ,obj-sym)) ,body)))))
       (unless (fboundp ',setter)
         (cl-defun ,setter (val ,obj-sym)
           ,(format "Set the value of slot `%S' for an object of type `%S'." slot type)
           (setf (cl-struct-slot-value ,type ',slot ,obj-sym) val))))))

;;;###autoload
(cl-defmacro lazy-slot-async! (slot type fn-body &key options constructor cache-options)
  "Define a lazy asynchronous accessor for SLOT in struct TYPE."
  (declare (indent 2))
  (let* ((getter (intern (format "%s-%s" type slot)))
         (setter (intern (format "setf-%s-%s" type slot)))
         (init-fn (intern (format "%s--init-%s" type slot)))
         (ctor (or constructor (intern (format "make-%s" type))))
         (cacheus-fn-sym (when cache-options (gensym (format "lazy-async-%s-%s-" type slot)))))
    `(progn
       (lazy-slot--register-definition ',type ',slot
                                       (list :is-async t
                                             :fn-body ',fn-body
                                             :options ',options
                                             :cache-options ',cache-options
                                             :cacheus-fn-sym ',cacheus-fn-sym
                                             :constructor ',ctor))

       ,@(when cacheus-fn-sym
           `((cacheus-memoize! ,cacheus-fn-sym (obj)
               :async t
               :key-fn (lambda (obj-arg) (lazy-slot--get-cache-key 'lazy-slot-async ',type ',slot obj-arg))
               ,@cache-options
               (concur:from-callback
                #'(lambda (cb)
                    ;; **CLEANUP**: Rely on lexical scope for `cb`.
                    (let ((obj obj))
                      ,(lazy-slot--substitute-anaphor fn-body 'obj 'obj)))))))

       (defun ,getter (obj)
         ,(format "Asynchronous lazy getter for slot %S. Blocks until resolved." slot)
         (cl-labels ((promise-thunk (obj-arg)
                       ,(if cacheus-fn-sym
                            `(funcall ',cacheus-fn-sym obj-arg)
                          `(concur:from-callback
                            #'(lambda (cb)
                                ;; **CLEANUP**: Rely on lexical scope for `cb`.
                                (let ((obj obj-arg))
                                  ,(lazy-slot--substitute-anaphor fn-body 'obj 'obj)))))))
           (lazy-slot--await-slot obj ',slot (lazy-slot--parse-opts ,options) #'promise-thunk)))

       (unless (fboundp ',setter)
         (cl-defun ,setter (val obj)
           ,(format "Set the value of slot `%S' for object `%S'." slot 'obj)
           (setf (cl-struct-slot-value ,type ',slot obj) val)))

       (defun ,init-fn (obj)
         ,(format "Initialize or re-fetch async slot %S for instance %S." slot 'obj)
         (cl-labels ((promise-thunk (obj-arg)
                       ,(if cacheus-fn-sym
                            `(funcall ',cacheus-fn-sym obj-arg)
                          `(concur:from-callback
                            #'(lambda (cb)
                                ;; **CLEANUP**: Rely on lexical scope for `cb`.
                                (let ((obj obj-arg))
                                  ,(lazy-slot--substitute-anaphor fn-body 'obj 'obj)))))))
           (lazy-slot--init-async-slot obj ',slot (lazy-slot--parse-opts ,options) #'promise-thunk)))

       (advice-add #',ctor :after
                   (lambda (&rest ctor-args)
                     (let ((new-obj (car (last ctor-args))))
                       (when (typep new-obj ',type)
                         (funcall #',init-fn new-obj))))))))

;;;###autoload
(cl-defmacro lazy-slots! (&key type eager async sync)
  "Define a set of accessor functions for a struct of `TYPE`."
  (declare (indent 1))
  `(progn
     ,@(--map `(progn
                (defun ,(intern (format "%s-%s" type it)) (obj)
                  ,(format "Eager getter for slot %S of type %S." it type)
                  (cl-struct-slot-value ,type ',it obj))
                (cl-defun ,(intern (format "setf-%s-%s" type it)) (val obj)
                  ,(format "Eager setter for slot %S of type %S." it type)
                  (setf (cl-struct-slot-value ,type ',it obj) val)))
              eager)
     ,@(--map (let* ((slot (car it)) (body (cadr it)) (opts (cddr it)))
               `(lazy-slot-sync! ,slot ,type ,body ,@opts))
              sync)
     ,@(--map (let* ((slot (car it)) (fn-body (cadr it)) (opts (cddr it)))
               `(lazy-slot-async! ,slot ,type ,fn-body ,@opts))
              async)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API for Slot Inspection and Manipulation

;;;###autoload
(defun lazy-slot-status (obj slot)
  "Return the status of a lazy SLOT on OBJ without triggering computation."
  (let* ((type (type-of obj))
         (current-val (cl-struct-slot-value type slot obj)))
    (cond
     ((null current-val) 'uninitialized)
     ((concur-future-p current-val)
      (concur:status (concur-future-promise current-val)))
     (t 'resolved))))

;;;###autoload
(defun lazy-slot-force-fetch (obj slot &optional options)
  "Explicitly force or re-force computation of a lazy SLOT on OBJ."
  (let* ((type (type-of obj))
         (definition (lazy-slot--get-definition type slot)))
    (unless definition
      (error "Slot %S on type %S not defined via lazy-slot macros." slot type))

    (let* ((is-async (plist-get definition :is-async))
           (body-form (plist-get definition :body))
           (fn-body-form (plist-get definition :fn-body))
           (cacheus-fn (plist-get definition :cacheus-fn-sym))
           (opts (lazy-slot--parse-opts (or options (plist-get definition :options))))
           (promise-thunk
            (lambda (obj-arg)
              (if cacheus-fn
                  (funcall cacheus-fn obj-arg)
                (if is-async
                    ;; **FIX**: Correctly bind `obj` and `cb` before evaluating `fn-body-form`.
                    (concur:from-callback
                     (lambda (cb) ; `cb` here is the real callback from `concur`.
                       ;; Establish the lexical environment for the anaphors.
                       (let ((obj obj-arg)
                             (cb cb))
                         (eval fn-body-form))))
                  (concur:resolved! (let ((obj obj-arg)) (eval body-form))))))))
      (lazy-slot--init-async-slot obj slot opts promise-thunk))))

;;;###autoload
(defun lazy-slot-clear-cache (obj slot)
  "Clear the cached value of a lazy SLOT on OBJ."
  (cl-block lazy-slot-clear-cache-block
    (let* ((type (type-of obj))
           (current-val (cl-struct-slot-value type slot obj))
           (definition (lazy-slot--get-definition type slot)))

      (unless definition
        (lazy-slot--log :warn "No def for slot %S on type %S" slot type)
        (cl-return-from lazy-slot-clear-cache-block nil))

      (when (concur-future-p current-val)
        (let ((promise (concur-future-promise current-val)))
          (when promise (concur:cancel promise "Cache cleared for lazy slot"))))

      (when (plist-get definition :cache-options)
        (let* ((is-async (plist-get definition :is-async))
               (prefix (if is-async 'lazy-slot-async 'lazy-slot-sync))
               (key (lazy-slot--get-cache-key prefix type slot obj)))
          (lazy-slot--log :info "Invalidating cacheus key for %S: %S" slot key)
          (cacheus-invalidate! key)))

      (setf (cl-struct-slot-value type slot obj) nil))))

;;;###autoload
(defun lazy-slot-value (obj slot)
  "Return the resolved value of a lazy SLOT on OBJ if resolved."
  (let* ((type (type-of obj))
         (current-val (cl-struct-slot-value type slot obj)))
    (cond
     ((concur-future-p current-val)
      (concur:value (concur-future-promise current-val)))
     ((not (null current-val)) current-val)
     (t nil))))

;;;###autoload
(defun lazy-slot-error (obj slot)
  "Return the error value of a lazy SLOT on OBJ if rejected."
  (let* ((type (type-of obj))
         (current-val (cl-struct-slot-value type slot obj)))
    (cond
     ((concur-future-p current-val)
      (concur:error-value (concur-future-promise current-val)))
     (t nil))))

(provide 'lazy-slot)
;;; lazy-slot.el ends here