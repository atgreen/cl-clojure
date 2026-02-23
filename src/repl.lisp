;;; repl.lisp -- Executable image builder for the Clojure REPL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:clojure)

;;; ---------------------------------------------------------------------------
;;; Reopen stale file streams after image restore
;;; ---------------------------------------------------------------------------

(defun %reopen-stale-fd (fis)
  "Reopen the file backing a FileInputStream whose CL stream was closed
   by save-lisp-and-die.  Stores the fresh stream back into the fd slot."
  (let ((fd-sym (intern "fd" :openldk)))
    (when (and (slot-exists-p fis fd-sym)
               (slot-boundp fis fd-sym))
      (let* ((file-descriptor (slot-value fis fd-sym))
             (fd (if (and file-descriptor
                          (typep file-descriptor 'standard-object)
                          (slot-exists-p file-descriptor fd-sym)
                          (slot-boundp file-descriptor fd-sym))
                     (slot-value file-descriptor fd-sym)
                     file-descriptor)))
        (when (and (typep fd 'sb-sys:fd-stream)
                   (not (open-stream-p fd)))
          (let ((path (pathname fd)))
            (when path
              (let ((new (open path :element-type '(unsigned-byte 8) :direction :input)))
                (if (and file-descriptor
                         (typep file-descriptor 'standard-object)
                         (slot-exists-p file-descriptor fd-sym)
                         (slot-boundp file-descriptor fd-sym))
                    (setf (slot-value file-descriptor fd-sym) new)
                    (setf (slot-value fis fd-sym) new))))))))))

(defun %fix-stale-file-input-streams ()
  "Walk the heap and reopen any FileInputStream objects whose backing
   CL streams were closed by save-lisp-and-die.  Called once at image
   startup before any Java code runs."
  (let ((stale nil))
    (sb-vm:map-allocated-objects
     (lambda (obj type size)
       (declare (ignore type size))
       (when (typep obj 'openldk::|java/io/FileInputStream|)
         (push obj stale)))
     :dynamic)
    (dolist (fis stale)
      (%reopen-stale-fd fis))))

;;; ---------------------------------------------------------------------------
;;; Image building
;;; ---------------------------------------------------------------------------

(defun make-image (&key (output-path "cl-clojure") classpath)
  "Dump an executable SBCL image that starts a Clojure REPL.
   OUTPUT-PATH: path for the saved executable (default \"clojure\").
   CLASSPATH:   classpath string; defaults to LDK_CLASSPATH env var
                or \"libs/clojure-1.12.0.jar\"."
  (let ((cp (or classpath
                (uiop:getenv "LDK_CLASSPATH")
                "libs/clojure-1.12.0.jar")))
    ;; Initialize the Clojure runtime
    (startup :classpath cp)

    ;; Resolve REPL vars so they're warm in the image
    (lookup "pr-str" "clojure.core")
    (lookup "*ns*" "clojure.core")

    ;; Warmup eval to ensure the compiler path is hot
    (eval "(+ 1 1)")

    ;; Load clojure.main and REPL helper namespaces so everything is warm
    (eval "(require 'clojure.main)")
    (eval "(apply require clojure.main/repl-requires)")

    ;; Clear monitor state to prevent deadlocks in the saved image
    (clrhash openldk::*monitors*)

    ;; Kill all Java threads before saving (SBCL can't save with threads running)
    (loop for thread in (bt:all-threads)
          when (and (not (eq thread (bt:current-thread)))
                    (search "Java-Thread" (bt:thread-name thread)))
          do (bt:destroy-thread thread))

    ;; Dump the image
    (sb-ext:save-lisp-and-die output-path
                              :executable t
                              :save-runtime-options t
                              :toplevel #'repl-toplevel)))

(defun repl-toplevel ()
  "Entry point for the dumped Clojure REPL image."
  ;; Disable floating-point traps to match Java semantics
  (sb-int:set-floating-point-modes :traps nil)
  ;; Reopen any file streams closed by save-lisp-and-die (e.g. /dev/urandom)
  (%fix-stale-file-input-streams)
  (handler-case
      (let* ((repl-code "(do (println \"Clojure\" (clojure-version))
                            (clojure.main/repl
                              :init (fn []
                                      (in-ns 'user)
                                      (apply require clojure.main/repl-requires))))")
             (form (cl:funcall *sym-rt-readstring* (openldk::jstring repl-code)))
             (result (cl:funcall *sym-compiler-eval* form)))
        (declare (ignore result)))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e)
      (finish-output *error-output*)
      (uiop:quit 1)))
  (uiop:quit 0))
