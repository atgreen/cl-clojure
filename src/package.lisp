;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(defpackage #:clojure
  (:use #:cl)
  (:nicknames #:cl-clojure)
  (:shadow #:eval #:funcall)
  (:documentation "Common Lisp / Clojure interop via OpenLDK.")
  (:export #:startup
           #:eval
           #:funcall
           #:lookup
           #:register
           #:clojure->cl
           #:cl->clojure
           #:make-namespace
           #:clojure-repl
           #:make-image))

(in-package #:clojure)
