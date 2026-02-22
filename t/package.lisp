;;; t/package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(defpackage #:cl-clojure/tests
  (:use #:cl #:fiveam)
  (:documentation "Tests for cl-clojure."))

(in-package #:cl-clojure/tests)

(def-suite :cl-clojure
  :description "cl-clojure test suite")

(in-suite :cl-clojure)
