# cl-clojure

Common Lisp / Clojure interop via [OpenLDK](https://github.com/atgreen/openldk).

Evaluate Clojure code from Common Lisp, call Clojure functions,
register CL functions callable from Clojure, and exchange values --
all within a single SBCL process.

## Quick Start

```sh
# Download the Clojure JAR
make libs/clojure-1.12.0.jar

# Run the demo
LDK_CLASSPATH=libs/clojure-1.12.0.jar \
  JAVA_HOME=/path/to/java8/jre \
  sbcl --load hello.lisp
```

## API

```lisp
;; Initialize the Clojure runtime
(clojure:startup :classpath "libs/clojure-1.12.0.jar")

;; Evaluate Clojure expressions (string or s-expression)
(clojure:eval "(+ 1 2)")        ;=> 3
(clojure:eval '(str "hi" "!"))  ;=> "hi!"

;; Look up a Clojure Var
(clojure:lookup "+" "clojure.core")

;; Call a Clojure function with CL arguments
(clojure:funcall (clojure:lookup "+") 10 20)  ;=> 30

;; Register a CL function callable from Clojure
(clojure:register "cl-square" (lambda (x) (* x x)))
(clojure:eval "(cl-square 7)")  ;=> 49

;; Convert values between CL and Clojure
(clojure:clojure->cl obj)
(clojure:cl->clojure val)

;; Create or find a Clojure namespace
(clojure:make-namespace "my-ns")
```

## Running Tests

```sh
make check
```

Or manually:

```sh
LDK_CLASSPATH=libs/clojure-1.12.0.jar \
  JAVA_HOME=/path/to/java8/jre \
  sbcl --load test.lisp
```

## Author and License

`cl-clojure` was written by Anthony Green and is distributed
under the terms of the MIT license.
