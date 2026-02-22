;;; test.lisp -- cl-clojure smoke tests
;;;
;;; Run: LDK_CLASSPATH=libs/clojure-1.12.0.jar JAVA_HOME=... sbcl --load test.lisp

(require :asdf)

;; Register both cl-clojure and openldk so ASDF can find them
(push (make-pathname :directory (pathname-directory *load-truename*))
      asdf:*central-registry*)
(push #p"~/git/openldk/"
      asdf:*central-registry*)

(asdf:load-system :cl-clojure)

(format t "~%=== cl-clojure tests ===~%")

(clojure:startup :classpath (or (uiop:getenv "LDK_CLASSPATH")
                                "libs/clojure-1.12.0.jar"))

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro test (name expr expected &key (test '#'equal))
  `(handler-case
       (let ((result ,expr))
         (if (funcall ,test result ,expected)
             (progn (format t "  PASS: ~A~%" ,name)
                    (incf *pass*))
             (progn (format t "  FAIL: ~A~%    expected: ~S~%    got:      ~S~%"
                            ,name ,expected result)
                    (incf *fail*))))
     (error (e)
       (format t "  ERROR: ~A~%    ~A~%" ,name e)
       (incf *fail*))))

;; Basic eval
(test "integer arithmetic" (clojure:eval "(+ 1 2)") 3)
(test "count" (clojure:eval "(count \"hello\")") 5)
(test "vector creation" (clojure:eval "[1 2 3]") '(1 2 3))
(test "define value" (clojure:eval "(do (def x 42) x)") 42)
(test "boolean true" (clojure:eval "(> 3 2)") t)
(test "boolean false" (clojure:eval "(> 2 3)") nil)

;; Lookup + funcall
(test "lookup + funcall"
      (let ((add (clojure:lookup "+")))
        (clojure:funcall add 10 20))
      30)

;; Register CL function
(test "register + call"
      (progn
        (clojure:register "cl-square" (lambda (x) (* x x)))
        (clojure:eval "(cl-square 7)"))
      49)

;; S-expression eval (no strings needed)
(test "sexp arithmetic" (clojure:eval '(+ 1 2)) 3)
(test "sexp count" (clojure:eval '(count "hello")) 5)
(test "sexp vector" (clojure:eval '(vector 1 2 3)) '(1 2 3))
(test "sexp nested" (clojure:eval '(* (+ 2 3) (- 10 4))) 30)
(test "sexp boolean" (clojure:eval '(> 3 2)) t)
(test "sexp define + call"
      (clojure:eval '(do (def y 99) y))
      99)
(test "sexp register call"
      (clojure:eval '(cl-square 9))
      81)

(format t "~%~D passed, ~D failed.~%" *pass* *fail*)
(uiop:quit (if (zerop *fail*) 0 1))
