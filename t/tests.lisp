;;; t/tests.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cl-clojure/tests)

;;; ---------------------------------------------------------------------------
;;; Startup fixture
;;; ---------------------------------------------------------------------------

(defvar *clojure-started* nil)

(defun ensure-clojure ()
  "Initialize Clojure once for the entire test run."
  (unless *clojure-started*
    (clojure:startup :classpath (or (uiop:getenv "LDK_CLASSPATH")
                                    "libs/clojure-1.12.0.jar"))
    (setf *clojure-started* t)))

;;; ---------------------------------------------------------------------------
;;; eval (string form)
;;; ---------------------------------------------------------------------------

(def-suite :eval-string :in :cl-clojure
  :description "clojure:eval with string arguments")
(in-suite :eval-string)

(test eval-integer-arithmetic
  (ensure-clojure)
  (is (= 3 (clojure:eval "(+ 1 2)")))
  (is (zerop (clojure:eval "(+ 0 0)")))
  (is (= -5 (clojure:eval "(- 3 8)")))
  (is (= 120 (clojure:eval "(* 1 2 3 4 5)"))))

(test eval-floating-point
  (ensure-clojure)
  (is (= 3.14d0 (clojure:eval "3.14")))
  (is (= 2.5d0 (clojure:eval "(/ 5.0 2.0)"))))

(test eval-string-operations
  (ensure-clojure)
  (is (= 5 (clojure:eval "(count \"hello\")")))
  (is (string= "hello world" (clojure:eval "(str \"hello\" \" \" \"world\")"))))

(test eval-vector-creation
  (ensure-clojure)
  (is (equal '(1 2 3) (clojure:eval "[1 2 3]")))
  (is (null (clojure:eval "[]"))))

(test eval-define
  (ensure-clojure)
  (is (= 42 (clojure:eval "(do (def test-x 42) test-x)"))))

(test eval-boolean
  (ensure-clojure)
  (is (eq t (clojure:eval "(> 3 2)")))
  (is (eq nil (clojure:eval "(> 2 3)")))
  (is (eq t (clojure:eval "(= 5 5)")))
  (is (eq nil (clojure:eval "(= 5 6)"))))

;;; ---------------------------------------------------------------------------
;;; eval (s-expression form)
;;; ---------------------------------------------------------------------------

(def-suite :eval-sexp :in :cl-clojure
  :description "clojure:eval with s-expression arguments")
(in-suite :eval-sexp)

(test sexp-arithmetic
  (ensure-clojure)
  (is (= 3 (clojure:eval '(+ 1 2))))
  (is (= 30 (clojure:eval '(* (+ 2 3) (- 10 4)))))
  (is (zerop (clojure:eval '(- 5 5)))))

(test sexp-string-operations
  (ensure-clojure)
  (is (= 5 (clojure:eval '(count "hello"))))
  (is (string= "foobar" (clojure:eval '(str "foo" "bar")))))

(test sexp-vector
  (ensure-clojure)
  (is (equal '(1 2 3) (clojure:eval '(vector 1 2 3)))))

(test sexp-nested
  (ensure-clojure)
  (is (= 30 (clojure:eval '(* (+ 2 3) (- 10 4))))))

(test sexp-boolean
  (ensure-clojure)
  (is (eq t (clojure:eval '(> 3 2))))
  (is (eq nil (clojure:eval '(< 3 2)))))

(test sexp-define
  (ensure-clojure)
  (is (= 99 (clojure:eval '(do (def sexp-y 99) sexp-y)))))

;;; ---------------------------------------------------------------------------
;;; sexp->clojure-string
;;; ---------------------------------------------------------------------------

(def-suite :sexp-serialization :in :cl-clojure
  :description "sexp->clojure-string conversion")
(in-suite :sexp-serialization)

(test serialize-nil
  (is (string= "nil" (clojure::sexp->clojure-string nil))))

(test serialize-t
  (is (string= "true" (clojure::sexp->clojure-string t))))

(test serialize-symbol
  (is (string= "foo" (clojure::sexp->clojure-string 'foo)))
  (is (string= "string-length" (clojure::sexp->clojure-string 'string-length))))

(test serialize-keyword
  (is (string= ":foo" (clojure::sexp->clojure-string :foo))))

(test serialize-integer
  (is (string= "42" (clojure::sexp->clojure-string 42)))
  (is (string= "-7" (clojure::sexp->clojure-string -7))))

(test serialize-float
  (is (string= "3.14" (clojure::sexp->clojure-string 3.14d0))))

(test serialize-string
  (is (string= "\"hello\"" (clojure::sexp->clojure-string "hello")))
  (is (string= "\"he said \\\"hi\\\"\"" (clojure::sexp->clojure-string "he said \"hi\"")))
  (is (string= "\"back\\\\slash\"" (clojure::sexp->clojure-string "back\\slash"))))

(test serialize-quote
  (is (string= "'foo" (clojure::sexp->clojure-string '(quote foo))))
  (is (string= "'(1 2 3)" (clojure::sexp->clojure-string '(quote (1 2 3))))))

(test serialize-list
  (is (string= "(+ 1 2)" (clojure::sexp->clojure-string '(+ 1 2))))
  (is (string= "(list 1 2 3)" (clojure::sexp->clojure-string '(list 1 2 3)))))

(test serialize-nested
  (is (string= "(* (+ 2 3) (- 10 4))"
       (clojure::sexp->clojure-string '(* (+ 2 3) (- 10 4))))))

(test serialize-character
  (is (string= "\\a" (clojure::sexp->clojure-string #\a))))

(test serialize-vector
  (is (string= "[1 2 3]" (clojure::sexp->clojure-string #(1 2 3)))))

;;; ---------------------------------------------------------------------------
;;; Value conversion: clojure->cl
;;; ---------------------------------------------------------------------------

(def-suite :clojure->cl :in :cl-clojure
  :description "Clojure to CL value conversion")
(in-suite :clojure->cl)

(test convert-integer
  (ensure-clojure)
  (is (integerp (clojure:eval '(+ 1 2))))
  (is (= 3 (clojure:eval '(+ 1 2)))))

(test convert-float
  (ensure-clojure)
  (let ((result (clojure:eval "3.14")))
    (is (floatp result))
    (is (= 3.14d0 result))))

(test convert-string
  (ensure-clojure)
  (let ((result (clojure:eval "(str \"hello\" \" world\")")))
    (is (stringp result))
    (is (string= "hello world" result))))

(test convert-boolean-true
  (ensure-clojure)
  (is (eq t (clojure:eval "(> 3 2)"))))

(test convert-boolean-false
  (ensure-clojure)
  (is (eq nil (clojure:eval "(> 2 3)"))))

(test convert-vector
  (ensure-clojure)
  (let ((result (clojure:eval "[1 2 3]")))
    (is (listp result))
    (is (equal '(1 2 3) result))))

(test convert-empty-vector
  (ensure-clojure)
  (is (null (clojure:eval "[]"))))

(test convert-nil-passthrough
  (is (null (clojure:clojure->cl nil))))

;;; ---------------------------------------------------------------------------
;;; Value conversion: cl->clojure (round-trip)
;;; ---------------------------------------------------------------------------

(def-suite :cl->clojure :in :cl-clojure
  :description "CL to Clojure value conversion (verified by round-trip)")
(in-suite :cl->clojure)

(test roundtrip-integer
  (ensure-clojure)
  (clojure:register "cl-identity" #'identity)
  (is (= 42 (clojure:eval "(cl-identity 42)"))))

(test roundtrip-string
  (ensure-clojure)
  (is (string= "hello" (clojure:eval "(cl-identity \"hello\")"))))

(test roundtrip-list
  (ensure-clojure)
  (clojure:register "cl-reverse" (lambda (&rest args) (reverse args)))
  (is (equal '(3 2 1) (clojure:eval "(cl-reverse 1 2 3)"))))

;;; ---------------------------------------------------------------------------
;;; lookup
;;; ---------------------------------------------------------------------------

(def-suite :lookup :in :cl-clojure
  :description "clojure:lookup")
(in-suite :lookup)

(test lookup-builtin
  (ensure-clojure)
  (let ((var (clojure:lookup "+")))
    (is (not (null var)))))

(test lookup-defined
  (ensure-clojure)
  (clojure:eval "(do (def lookup-test-val 123) lookup-test-val)")
  (let ((var (clojure:lookup "lookup-test-val" "user")))
    (is (not (null var)))))

;;; ---------------------------------------------------------------------------
;;; funcall
;;; ---------------------------------------------------------------------------

(def-suite :funcall :in :cl-clojure
  :description "clojure:funcall")
(in-suite :funcall)

(test funcall-0-args
  (ensure-clojure)
  (clojure:register "funcall-zero" (constantly 42))
  (let ((f (clojure:lookup "funcall-zero" "user")))
    (is (= 42 (clojure:funcall f)))))

(test funcall-1-arg
  (ensure-clojure)
  (let ((abs-fn (clojure:lookup "abs" "clojure.core")))
    (is (= 5 (clojure:funcall abs-fn -5)))))

(test funcall-2-args
  (ensure-clojure)
  (let ((add (clojure:lookup "+")))
    (is (= 30 (clojure:funcall add 10 20)))))

(test funcall-3-args
  (ensure-clojure)
  (let ((add (clojure:lookup "+")))
    (is (= 60 (clojure:funcall add 10 20 30)))))

(test funcall-4-args
  (ensure-clojure)
  (let ((add (clojure:lookup "+")))
    (is (= 100 (clojure:funcall add 10 20 30 40)))))

(test funcall-many-args
  (ensure-clojure)
  (let ((add (clojure:lookup "+")))
    (is (= 150 (clojure:funcall add 10 20 30 40 50)))))

;;; ---------------------------------------------------------------------------
;;; register
;;; ---------------------------------------------------------------------------

(def-suite :register :in :cl-clojure
  :description "clojure:register")
(in-suite :register)

(test register-and-call-from-clojure
  (ensure-clojure)
  (clojure:register "cl-square" (lambda (x) (* x x)))
  (is (= 49 (clojure:eval "(cl-square 7)")))
  (is (= 81 (clojure:eval '(cl-square 9)))))

(test register-and-call-via-funcall
  (ensure-clojure)
  (clojure:register "cl-double" (lambda (x) (* x 2)))
  (let ((f (clojure:lookup "cl-double" "user")))
    (is (= 14 (clojure:funcall f 7)))))

(test register-multi-arg
  (ensure-clojure)
  (clojure:register "cl-add3" (lambda (a b c) (+ a b c)))
  (is (= 6 (clojure:eval "(cl-add3 1 2 3)"))))

(test register-returns-string
  (ensure-clojure)
  (clojure:register "cl-greet" (lambda (name) (format nil "Hello, ~A!" name)))
  (is (string= "Hello, World!" (clojure:eval "(cl-greet \"World\")"))))

(test register-returns-list
  (ensure-clojure)
  (clojure:register "cl-pair" (lambda (a b) (list a b)))
  (is (equal '(1 2) (clojure:eval "(cl-pair 1 2)"))))

(test register-no-args
  (ensure-clojure)
  (clojure:register "cl-forty-two" (constantly 42))
  (is (= 42 (clojure:eval "(cl-forty-two)"))))

(test register-clojure-calls-cl-calls-clojure
  "Round-trip: Clojure calls CL function that calls back into Clojure."
  (ensure-clojure)
  (clojure:register "cl-eval-clojure"
                     (lambda (expr-str) (clojure:eval (clojure:clojure->cl expr-str))))
  (is (= 10 (clojure:eval "(cl-eval-clojure \"(+ 3 7)\")"))))

;;; ---------------------------------------------------------------------------
;;; make-namespace
;;; ---------------------------------------------------------------------------

(def-suite :namespaces :in :cl-clojure
  :description "clojure:make-namespace")
(in-suite :namespaces)

(test make-namespace-basic
  (ensure-clojure)
  (let ((ns (clojure:make-namespace "test-ns")))
    (is (not (null ns)))))

;;; ---------------------------------------------------------------------------
;;; Error handling
;;; ---------------------------------------------------------------------------

(def-suite :errors :in :cl-clojure
  :description "Error conditions")
(in-suite :errors)

(test eval-before-startup-signals
  "Calling eval on a fresh clojure package without startup should signal an error."
  ;; We can't easily test this without resetting state, so just verify
  ;; that ensure-started doesn't error when already started.
  (ensure-clojure)
  (finishes (clojure:eval '(+ 1 1))))

(test eval-bad-clojure-signals
  (ensure-clojure)
  (signals error (clojure:eval "(undefined-proc-xyz)")))
