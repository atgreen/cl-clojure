;;; repl.lisp -- Interactive Clojure REPL and executable image builder
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:clojure)

;;; ---------------------------------------------------------------------------
;;; Balanced-input check for multi-line support
;;; ---------------------------------------------------------------------------

(defun %balanced-input-p (text)
  "Return T if all delimiters in TEXT are balanced.
   Handles strings, comments, (), [], and {}."
  (let ((depth-paren 0)
        (depth-bracket 0)
        (depth-brace 0)
        (in-string nil)
        (in-comment nil)
        (escape nil)
        (len (length text)))
    (dotimes (i len)
      (let ((ch (char text i)))
        (cond
          ;; Handle escape sequences inside strings
          (escape
           (setf escape nil))
          ;; Inside a string
          (in-string
           (case ch
             (#\\ (setf escape t))
             (#\" (setf in-string nil))))
          ;; Inside a line comment (;)
          (in-comment
           (when (char= ch #\Newline)
             (setf in-comment nil)))
          ;; Start of string
          ((char= ch #\")
           (setf in-string t))
          ;; Start of comment
          ((char= ch #\;)
           (setf in-comment t))
          ;; Delimiters
          ((char= ch #\() (incf depth-paren))
          ((char= ch #\)) (decf depth-paren))
          ((char= ch #\[) (incf depth-bracket))
          ((char= ch #\]) (decf depth-bracket))
          ((char= ch #\{) (incf depth-brace))
          ((char= ch #\}) (decf depth-brace)))))
    (and (not in-string)
         (zerop depth-paren)
         (zerop depth-bracket)
         (zerop depth-brace))))

;;; ---------------------------------------------------------------------------
;;; REPL
;;; ---------------------------------------------------------------------------

(defun clojure-repl ()
  "Run an interactive Clojure REPL.
   Reads raw Clojure source text, supports multi-line input,
   and prints results using Clojure's pr-str for authentic output.
   Exit with EOF (Ctrl-D)."
  (ensure-started)
  (let ((pr-str-var (lookup "pr-str" "clojure.core"))
        (ns-var (lookup "*ns*" "clojure.core")))
    (loop
      ;; Build the prompt from current namespace
      (let* ((current-ns (openldk::|deref()| ns-var))
             (ns-name (openldk::lstring (openldk::|toString()| current-ns)))
             (prompt (format nil "~A=> " ns-name))
             (continuation (make-string (length prompt)
                                        :initial-element #\Space)))
        ;; Replace last 3 chars of continuation with ".. "
        (setf (char continuation (- (length continuation) 3)) #\.)
        (setf (char continuation (- (length continuation) 2)) #\.)
        (setf (char continuation (- (length continuation) 1)) #\Space)
        ;; Read input (possibly multi-line)
        (format t "~A" prompt)
        (finish-output)
        (let ((first-line (read-line *standard-input* nil nil)))
          (unless first-line
            (terpri)
            (return))
          (let ((input first-line))
            ;; Accumulate lines until balanced
            (loop while (not (%balanced-input-p input))
                  do (format t "~A" continuation)
                     (finish-output)
                     (let ((next-line (read-line *standard-input* nil nil)))
                       (unless next-line
                         (terpri)
                         (return-from clojure-repl))
                       (setf input (concatenate 'string input (string #\Newline) next-line))))
            ;; Eval and print (skip blank input)
            (when (plusp (length (string-trim '(#\Space #\Tab #\Newline) input)))
              (handler-case
                  (let* ((source input)
                         (form (cl:funcall *sym-rt-readstring* (openldk::jstring source)))
                         (result (cl:funcall *sym-compiler-eval* form)))
                    (if (null result)
                        (format t "nil~%")
                        (let ((printed (openldk::lstring
                                        (openldk::|invoke(Ljava/lang/Object;)|
                                         pr-str-var result))))
                          (format t "~A~%" printed))))
                (error (e)
                  (format *error-output* "~A~%" e)
                  (finish-output *error-output*))))))))))

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
  (handler-case
      (clojure-repl)
    (error (e)
      (format *error-output* "~&Error: ~A~%" e)
      (finish-output *error-output*)
      (uiop:quit 1)))
  (uiop:quit 0))
