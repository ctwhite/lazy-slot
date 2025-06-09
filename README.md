# Lazy Slot for Emacs Lisp

`lazy-slot.el` provides macros and functions for defining and using "lazy" slots in Emacs Lisp objects, specifically those defined with `cl-defstruct`. Lazy slots delay the computation of their values until they are first accessed, with the result then being cached for subsequent accesses.

This module supports both synchronous and asynchronous lazy evaluation and integrates with `cacheus` to provide advanced caching capabilities, including Time-To-Live (TTL), persistence, and versioning for the computed slot values. It leverages the `concur:` prefixed API from `concur.el` for promise-based asynchrony.

## Key Concepts

* **Lazy Evaluation**: The value of a lazy slot is computed only when it is first accessed. For synchronous slots, the result is stored directly in the slot.

* **Asynchronous Slots**: For I/O or long-running tasks, this module uses `concur` futures to manage the result without blocking Emacs. The actual value is computed in the background. Accessing the slot will block until the value is ready.

* **Implicit Context**: In the `lazy-slots!` macro, you can use the symbol `obj` within the body of a `:sync` slot definition to refer to the current object instance. For `:async` slots, both `obj` and `cb` (the callback function) are available.

* **Caching Integration**: By specifying `:cache-options`, users can leverage `cacheus-memoize!` to cache the results of slot computations, complete with TTL, versioning, and persistence.

## Installation

1. Place `lazy-slot.el` in your Emacs `load-path`.

2. Add `(require 'lazy-slot)` to your Emacs configuration file (e.g., `init.el`).

3. Ensure all required packages (listed below) are installed, ideally via MELPA or another package archive.

## Usage

The primary entry entry point for defining lazy slots is the `lazy-slots!` macro.

```emacs-lisp
(cl-defstruct my-data
  id
  name
  data)

(lazy-slots!
  :type my-data
  :eager (id name) ; Define standard (eager) slots
  :sync ((full-name (concat (my-data-name obj) " (" (my-data-id obj) ")")))
  :async ((data
           (lambda (obj cb)
             (message "Fetching data for ID: %S" (my-data-id obj))
             ;; Simulate an asynchronous operation (e.g., network request)
             (run-at-time
              "1s" nil
              (lambda ()
                (funcall cb (format "Data for ID: %S (Fetched at %s)"
                                    (my-data-id obj) (current-time-string))))))
           :options (:auto-fetch t :timeout 5.0) ; Options for async behavior
           :cache-options (:ttl 3600 :version "1.0")))) ; Caching options
```

**Example Usage:**

```emacs-lisp
;; Create an instance of your struct
(setq my-obj (make-my-data :id "user123" :name "John Doe"))

;; Access eager slots directly
(message "ID: %S" (my-data-id my-obj))
(message "Name: %S" (my-data-name my-obj))

;; Access a synchronous lazy slot
;; The 'full-name' will be computed and cached on first access.
(message "Full Name: %S" (my-data-full-name my-obj))

;; Access an asynchronous lazy slot
;; The 'data' will trigger an async fetch. The first access might block
;; until the data is resolved, subsequent accesses will return the cached value.
(message "Data Status (before first access): %S" (lazy-slot-status my-obj 'data))
(message "Data: %S" (my-data-data my-obj)) ; This will block for ~1 second
(message "Data Status (after first access): %S" (lazy-slot-status my-obj 'data))

;; Clear cache and force re-fetch
(lazy-slot-clear-cache my-obj 'data)
(lazy-slot-force-fetch my-obj 'data) ; This re-initiates the async fetch
(message "Data Status (after force-fetch): %S" (lazy-slot-status my-obj 'data))
```

## Public API

Beyond the `lazy-slots!` macro, the library provides functions for inspecting and manipulating lazy slot states:

* `lazy-slot-status OBJ SLOT`: Returns the status of a lazy `SLOT` on `OBJ` without triggering computation. Possible statuses include `'uninitialized`, `'pending`, `'resolved`, `'rejected`, or `'cancelled`.

* `lazy-slot-force-fetch OBJ SLOT &optional OPTIONS`: Explicitly forces or re-forces the computation of a lazy `SLOT` on `OBJ`. If the slot was already resolved, this will re-trigger the computation. `OPTIONS` can be used to override the original `lazy-slot-opts` for this specific fetch.

* `lazy-slot-clear-cache OBJ SLOT`: Clears the cached value of a lazy `SLOT` on `OBJ`. This forces re-computation on the next access and invalidates any associated `cacheus` entry.

* `lazy-slot-value OBJ SLOT`: Returns the resolved value of a lazy `SLOT` on `OBJ` *only* if it is *already resolved*. This function does not trigger computation or wait for pending promises.

* `lazy-slot-error OBJ SLOT`: Returns the error value of a lazy `SLOT` on `OBJ` *only if its underlying promise is in the 'rejected state*. This function does not trigger computation or wait.

## Dependencies

* **Emacs**: `27.1` or higher

* **cl-lib**: `0.5` or higher

* **concur**: `1.0` or higher

* **dash**: `2.18.0` or higher

* **ht**: `2.3` or higher

* **cacheus**: `1.0` or higher

## Author & Maintainer

Christian White <christiantwhite@protonmail.com>

## Version

0.1

## License

This software is licensed under the GNU General Public License, Version 3.
