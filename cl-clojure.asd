;;; cl-clojure.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(asdf:defsystem "cl-clojure"
  :description "Common Lisp / Clojure interop via OpenLDK."
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     (:read-file-form "version.sexp")
  :depends-on  ("openldk")
  :serial t
  :components ((:file "src/package")
               (:file "src/clojure")
               (:file "src/repl"))
  :in-order-to ((test-op (test-op "cl-clojure/tests"))))

(asdf:defsystem "cl-clojure/tests"
  :description "Tests for cl-clojure."
  :depends-on  ("cl-clojure" "fiveam")
  :serial t
  :components ((:file "t/package")
               (:file "t/tests"))
  :perform (test-op (op c)
             (symbol-call :fiveam :run! :cl-clojure)))
