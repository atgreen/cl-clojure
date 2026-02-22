;;; hello.lisp -- Hello World: Common Lisp -> Clojure -> Java
;;;
;;; Demonstrates the full interop chain:
;;;   Common Lisp calls Clojure, which calls a Java method,
;;;   and the result flows back to Common Lisp.
;;;
;;; Run:
;;;   LDK_CLASSPATH=libs/clojure-1.12.0.jar \
;;;     JAVA_HOME=/path/to/java8/jre \
;;;     sbcl --load hello.lisp

(asdf:load-system :cl-clojure)

(clojure:startup :classpath (or (uiop:getenv "LDK_CLASSPATH")
                                "libs/clojure-1.12.0.jar"))

;;; Common Lisp calls Clojure, Clojure calls Java's .toUpperCase(),
;;; and the result flows back to Common Lisp.

(format t "~A~%"
  (clojure:eval "(.toUpperCase (str \"Hello\" \", \" \"World!\"))"))

(uiop:quit 0)
