;;; clojure.lisp -- Common Lisp / Clojure interop via OpenLDK
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:clojure)

;;; ---------------------------------------------------------------------------
;;; Runtime state
;;; ---------------------------------------------------------------------------

(defvar *clojure-initialized* nil
  "T after STARTUP has completed successfully.")

;;; Cached static method symbols (resolved at startup)
(defvar *sym-rt-readstring* nil)
(defvar *sym-compiler-eval* nil)
(defvar *sym-rt-var* nil)
(defvar *sym-var-intern* nil)
(defvar *sym-symbol-intern* nil)
(defvar *sym-keyword-intern* nil)
(defvar *sym-ns-findorcreate* nil)
(defvar *sym-long-valueof* nil)
(defvar *sym-double-valueof* nil)
(defvar *sym-arrayseq-create* nil)

;;; Cached class symbols (resolved at startup)
(defvar *class-long* nil)
(defvar *class-integer* nil)
(defvar *class-double* nil)
(defvar *class-string* nil)
(defvar *class-boolean* nil)
(defvar *class-keyword* nil)
(defvar *class-symbol* nil)
(defvar *class-persistent-vector* nil)
(defvar *class-persistent-list* nil)
(defvar *class-persistent-array-map* nil)
(defvar *class-persistent-hash-map* nil)
(defvar *class-ratio* nil)

;;; Cached MethodHandle for IFn dispatch (resolved at startup)
(defvar *afn-apply-to-mh* nil)

(defun ensure-started ()
  "Signal an error if STARTUP has not been called."
  (unless *clojure-initialized*
    (error "cl-clojure: call (clojure:startup) first")))

;;; ---------------------------------------------------------------------------
;;; Symbol lookup helpers
;;; ---------------------------------------------------------------------------

(defun %find-static (name)
  "Find a static method symbol by searching OPENLDK.APP, OPENLDK.SYSTEM, :openldk."
  (loop for pkg-name in '("OPENLDK.APP" "OPENLDK.SYSTEM" "OPENLDK")
        for pkg = (find-package pkg-name)
        when pkg
        do (let ((sym (find-symbol name pkg)))
             (when (and sym (fboundp sym))
               (return sym)))))

(defun %find-class (name)
  "Find a CLOS class symbol for a Java class by searching packages."
  (loop for pkg-name in '("OPENLDK.APP" "OPENLDK.SYSTEM" "OPENLDK")
        for pkg = (find-package pkg-name)
        when pkg
        do (let ((sym (find-symbol name pkg)))
             (when (and sym (find-class sym nil))
               (return sym)))))

(defun %find-static-field (class-bin-name field-name)
  "Get the value of a static field on a Java class."
  (let ((static-name (format nil "+static-~A+" class-bin-name)))
    (loop for pkg-name in '("OPENLDK.APP" "OPENLDK.SYSTEM" "OPENLDK")
          for pkg = (find-package pkg-name)
          when pkg
          do (let ((sym (find-symbol static-name pkg)))
               (when (and sym (boundp sym))
                 (let ((static-obj (symbol-value sym)))
                   (return (slot-value static-obj (intern field-name :openldk)))))))))

;;; ---------------------------------------------------------------------------
;;; Value conversion: Clojure -> CL
;;; ---------------------------------------------------------------------------

(defun clojure->cl (obj)
  "Convert a Java/Clojure value to a Common Lisp value.
   Returns the object unchanged if no conversion applies."
  (cond
    ((null obj) nil)
    ((and *class-long* (typep obj *class-long*))
     (slot-value obj (intern "value" :openldk)))
    ((and *class-integer* (typep obj *class-integer*))
     (slot-value obj (intern "value" :openldk)))
    ((and *class-double* (typep obj *class-double*))
     (slot-value obj (intern "value" :openldk)))
    ((and *class-string* (typep obj *class-string*))
     (openldk::lstring obj))
    ((and *class-boolean* (typep obj *class-boolean*))
     (not (eql 0 (slot-value obj (intern "value" :openldk)))))
    ((and *class-keyword* (typep obj *class-keyword*))
     (let ((name-str (openldk::lstring (openldk::|getName()| obj))))
       (intern (string-upcase name-str) :keyword)))
    ((and *class-symbol* (typep obj *class-symbol*))
     (let ((name-str (openldk::lstring (openldk::|getName()| obj))))
       (intern (string-upcase name-str))))
    ((and *class-persistent-vector* (typep obj *class-persistent-vector*))
     (let ((cnt (openldk::|count()| obj)))
       (loop for i below cnt
             collect (clojure->cl (openldk::|nth(I)| obj i)))))
    ((and *class-persistent-list* (typep obj *class-persistent-list*))
     (loop for seq = obj then (openldk::|next()| seq)
           while seq
           collect (clojure->cl (openldk::|first()| seq))))
    ((or (and *class-persistent-array-map* (typep obj *class-persistent-array-map*))
         (and *class-persistent-hash-map* (typep obj *class-persistent-hash-map*)))
     (loop for seq = (openldk::|seq()| obj) then (openldk::|next()| seq)
           while seq
           for entry = (openldk::|first()| seq)
           collect (cons (clojure->cl (openldk::|key()| entry))
                         (clojure->cl (openldk::|val()| entry)))))
    ((and *class-ratio* (typep obj *class-ratio*))
     (let ((s (openldk::lstring (openldk::|toString()| obj))))
       (read-from-string s)))
    (t obj)))

;;; ---------------------------------------------------------------------------
;;; Value conversion: CL -> Clojure
;;; ---------------------------------------------------------------------------

(defun cl->clojure (val)
  "Convert a Common Lisp value to a Java/Clojure object.
   Returns the value unchanged if no conversion applies."
  (cond
    ((null val) nil)
    ((eq val t)
     (%find-static-field "java/lang/Boolean" "TRUE"))
    ((integerp val)
     (cl:funcall *sym-long-valueof* val))
    ((floatp val)
     (cl:funcall *sym-double-valueof* (coerce val 'double-float)))
    ((stringp val)
     (openldk::jstring val))
    ((keywordp val)
     (cl:funcall *sym-keyword-intern*
      nil (openldk::jstring (string-downcase (symbol-name val)))))
    ((consp val)
     ;; Convert CL list to Clojure PersistentVector via array
     (let* ((items (mapcar #'cl->clojure val))
            (arr (openldk::make-java-array
                  :component-class "java/lang/Object"
                  :initial-contents (coerce items 'vector))))
       (cl:funcall (%find-static "clojure/lang/PersistentVector.create([Ljava/lang/Object;)") arr)))
    (t val)))

;;; ---------------------------------------------------------------------------
;;; Startup
;;; ---------------------------------------------------------------------------

(defun startup (&key classpath)
  "Initialize the Clojure runtime.
   CLASSPATH is a string pointing to the Clojure JAR (colon-separated if multiple).
   If nil, reads from the LDK_CLASSPATH environment variable."
  (when *clojure-initialized*
    (return-from startup t))

  ;; Initialize the OpenLDK JVM runtime (loads JDK bootstrap classes)
  (openldk:initialize)

  ;; Set up boot and app class loaders (mirrors what openldk::main does).
  ;; initialize() loads JDK classes but doesn't create the LDK loader wrappers.
  (unless openldk::*boot-ldk-class-loader*
    (let ((system-pkg (or (find-package "OPENLDK.SYSTEM")
                          (make-package "OPENLDK.SYSTEM" :use '(:openldk)))))
      (setf openldk::*boot-ldk-class-loader*
            (make-instance 'openldk::<ldk-class-loader>
                           :id 0
                           :pkg system-pkg
                           :parent-loader nil
                           :java-loader nil
                           :ldk-classes-by-bin-name openldk::*ldk-classes-by-bin-name*
                           :ldk-classes-by-fq-name openldk::*ldk-classes-by-fq-name*
                           :java-classes-by-bin-name openldk::*java-classes-by-bin-name*
                           :java-classes-by-fq-name openldk::*java-classes-by-fq-name*))))
  (unless openldk::*app-ldk-class-loader*
    (setf openldk::*app-ldk-class-loader*
          (openldk::make-ldk-class-loader
           :parent-loader openldk::*boot-ldk-class-loader*
           :java-loader nil
           :package-name "OPENLDK.APP")))

  ;; Build the classpath: user JARs + JAVA_HOME jars
  (let* ((java-home (uiop:getenv "JAVA_HOME"))
         (java-jars (when java-home
                      (format nil "~{~A~^:~}"
                              (mapcar #'namestring
                                      (directory (concatenate 'string java-home "/lib/*.jar"))))))
         (clj-cp (or classpath ""))
         (full-cp (if java-jars
                      (concatenate 'string clj-cp ":" java-jars)
                      clj-cp)))
    (setf openldk::*classpath*
          (loop for cpe in (split-sequence:split-sequence
                            (uiop:inter-directory-separator) full-cp)
                when (plusp (length cpe))
                collect (if (str:ends-with? ".jar" cpe)
                            (make-instance 'openldk::jar-classpath-entry :jarfile cpe)
                            (make-instance 'openldk::dir-classpath-entry :dir cpe)))))

  ;; Reset system properties and init Launcher (needed for MethodHandle infrastructure)
  (handler-case
      (let ((sym (%find-static "java/lang/System.initProperties(Ljava/util/Properties;)")))
        (when sym
          (cl:funcall sym
                      (slot-value openldk::|+static-java/lang/System+| (intern "props" :openldk)))))
    (error () nil))
  (handler-case
      (openldk::%clinit (openldk::%get-ldk-class-by-bin-name "sun/misc/Launcher" t))
    (error () nil))

  ;; Quiet down class loading output
  (setf openldk::*debug-load* nil)
  (setf openldk::*debug-compile* nil)

  ;; Load key Clojure classes
  (dolist (c '("clojure/lang/Symbol"
               "clojure/lang/Keyword"
               "clojure/lang/Namespace"
               "clojure/lang/Var"
               "clojure/lang/IFn"
               "clojure/lang/AFn"
               "clojure/lang/RT"
               "clojure/lang/Compiler"
               "clojure/lang/PersistentVector"
               "clojure/lang/PersistentList"
               "clojure/lang/PersistentArrayMap"
               "clojure/lang/PersistentHashMap"
               "clojure/lang/Ratio"
               "clojure/lang/ArraySeq"))
    (handler-case (openldk::classload c)
      (error (e) (warn "cl-clojure: could not load ~A: ~A" c e))))

  ;; Resolve and cache static method symbols
  (setf *sym-rt-readstring*
        (%find-static "clojure/lang/RT.readString(Ljava/lang/String;)"))
  (setf *sym-compiler-eval*
        (%find-static "clojure/lang/Compiler.eval(Ljava/lang/Object;)"))
  (setf *sym-rt-var*
        (%find-static "clojure/lang/RT.var(Ljava/lang/String;Ljava/lang/String;)"))
  (setf *sym-var-intern*
        (%find-static "clojure/lang/Var.intern(Lclojure/lang/Namespace;Lclojure/lang/Symbol;Ljava/lang/Object;)"))
  (setf *sym-symbol-intern*
        (%find-static "clojure/lang/Symbol.intern(Ljava/lang/String;)"))
  (setf *sym-keyword-intern*
        (%find-static "clojure/lang/Keyword.intern(Ljava/lang/String;Ljava/lang/String;)"))
  (setf *sym-ns-findorcreate*
        (%find-static "clojure/lang/Namespace.findOrCreate(Lclojure/lang/Symbol;)"))
  (setf *sym-long-valueof*
        (%find-static "java/lang/Long.valueOf(J)"))
  (setf *sym-double-valueof*
        (%find-static "java/lang/Double.valueOf(D)"))
  (setf *sym-arrayseq-create*
        (%find-static "clojure/lang/ArraySeq.create([Ljava/lang/Object;)"))

  ;; Resolve and cache class symbols for type dispatch
  (setf *class-long*                (%find-class "java/lang/Long"))
  (setf *class-integer*             (%find-class "java/lang/Integer"))
  (setf *class-double*              (%find-class "java/lang/Double"))
  (setf *class-string*              (%find-class "java/lang/String"))
  (setf *class-boolean*             (%find-class "java/lang/Boolean"))
  (setf *class-keyword*             (%find-class "clojure/lang/Keyword"))
  (setf *class-symbol*              (%find-class "clojure/lang/Symbol"))
  (setf *class-persistent-vector*   (%find-class "clojure/lang/PersistentVector"))
  (setf *class-persistent-list*     (%find-class "clojure/lang/PersistentList"))
  (setf *class-persistent-array-map* (%find-class "clojure/lang/PersistentArrayMap"))
  (setf *class-persistent-hash-map* (%find-class "clojure/lang/PersistentHashMap"))
  (setf *class-ratio*               (%find-class "clojure/lang/Ratio"))

  ;; Define the CL->Clojure function bridge class
  (%define-cl-function-class)

  ;; Switch *ns* to 'user namespace (matching standard Clojure REPL behavior).
  ;; Without this, *ns* stays as clojure.core (its root value from RT init),
  ;; and vars registered in the "user" namespace are invisible to eval.
  (handler-case
      (let* ((core-sym (cl:funcall *sym-symbol-intern* (openldk::jstring "clojure.core")))
             (core-ns (cl:funcall *sym-ns-findorcreate* core-sym))
             (ns-name-sym (cl:funcall *sym-symbol-intern* (openldk::jstring "*ns*")))
             (user-sym (cl:funcall *sym-symbol-intern* (openldk::jstring "user")))
             (user-ns (cl:funcall *sym-ns-findorcreate* user-sym)))
        ;; Re-bind the *ns* var root to the user namespace
        (cl:funcall *sym-var-intern* core-ns ns-name-sym user-ns)
        ;; Refer clojure.core into user so builtins remain accessible
        (let* ((source "(clojure.core/refer 'clojure.core)")
               (form (cl:funcall *sym-rt-readstring* (openldk::jstring source))))
          (cl:funcall *sym-compiler-eval* form)))
    (error (e) (warn "cl-clojure: could not switch to user namespace: ~A" e)))

  (setf *clojure-initialized* t)
  t)

;;; ---------------------------------------------------------------------------
;;; S-expression serialization
;;; ---------------------------------------------------------------------------

(defun sexp->clojure-string (expr)
  "Convert a CL s-expression to a Clojure source string."
  (cond
    ((null expr) "nil")
    ((eq expr t) "true")
    ((keywordp expr)
     (concatenate 'string ":" (string-downcase (symbol-name expr))))
    ((symbolp expr)
     (let ((name (symbol-name expr)))
       (if (string= name (string-upcase name))
           (string-downcase name)          ; normal CL symbol: FOO -> foo
           name)))                         ; pipe-quoted: |toUpperCase| preserved
    ((stringp expr)
     (with-output-to-string (s)
       (write-char #\" s)
       (loop for ch across expr do
         (case ch
           (#\\ (write-string "\\\\" s))
           (#\" (write-string "\\\"" s))
           (otherwise (write-char ch s))))
       (write-char #\" s)))
    ((numberp expr)
     (let ((*read-default-float-format* 'double-float))
       (write-to-string expr)))
    ((and (consp expr) (eq (first expr) 'quote) (consp (rest expr)) (null (cddr expr)))
     (concatenate 'string "'" (sexp->clojure-string (cadr expr))))
    ((consp expr)
     (with-output-to-string (s)
       (write-char #\( s)
       (loop for cell on expr
             for first = t then nil do
         (unless first (write-char #\Space s))
         (write-string (sexp->clojure-string (first cell)) s))
       (write-char #\) s)))
    ((characterp expr)
     (format nil "\\~A" expr))
    ((vectorp expr)
     (with-output-to-string (s)
       (write-char #\[ s)
       (loop for i below (length expr)
             for first = t then nil do
         (unless first (write-char #\Space s))
         (write-string (sexp->clojure-string (aref expr i)) s))
       (write-char #\] s)))
    (t (write-to-string expr))))

;;; ---------------------------------------------------------------------------
;;; eval
;;; ---------------------------------------------------------------------------

(defun eval (expr &optional ns)
  "Evaluate a Clojure expression.  EXPR can be a string of Clojure source
   or a CL s-expression (which is converted automatically).
   Returns the result as a CL value."
  (declare (ignore ns))
  (ensure-started)
  (let* ((source (if (stringp expr) expr (sexp->clojure-string expr)))
         (form (cl:funcall *sym-rt-readstring* (openldk::jstring source))))
    (clojure->cl
     (cl:funcall *sym-compiler-eval* form))))

;;; ---------------------------------------------------------------------------
;;; lookup
;;; ---------------------------------------------------------------------------

(defun lookup (name &optional (ns-name "clojure.core"))
  "Look up a Clojure Var by namespace and name.  Returns the Var (which
   implements IFn and can be used directly with FUNCALL)."
  (ensure-started)
  (cl:funcall *sym-rt-var*
   (openldk::jstring ns-name)
   (openldk::jstring name)))

;;; ---------------------------------------------------------------------------
;;; funcall
;;; ---------------------------------------------------------------------------

(defun funcall (fn &rest args)
  "Call a Clojure IFn with CL arguments.  Returns the result as a CL value."
  (ensure-started)
  (let ((jargs (mapcar #'cl->clojure args)))
    (clojure->cl
     (case (length jargs)
       (0 (openldk::|invoke()| fn))
       (1 (openldk::|invoke(Ljava/lang/Object;)| fn (first jargs)))
       (2 (openldk::|invoke(Ljava/lang/Object;Ljava/lang/Object;)|
           fn (first jargs) (second jargs)))
       (3 (openldk::|invoke(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
           fn (first jargs) (second jargs) (third jargs)))
       (4 (openldk::|invoke(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
           fn (first jargs) (second jargs) (third jargs) (fourth jargs)))
       (5 (openldk::|invoke(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
           fn (first jargs) (second jargs) (third jargs) (fourth jargs) (fifth jargs)))
       (6 (openldk::|invoke(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
           fn (first jargs) (second jargs) (third jargs) (fourth jargs) (fifth jargs) (sixth jargs)))
       (7 (openldk::|invoke(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
           fn (first jargs) (second jargs) (third jargs) (fourth jargs) (fifth jargs) (sixth jargs) (seventh jargs)))
       (otherwise
        (let* ((arr (openldk::make-java-array
                     :component-class "java/lang/Object"
                     :initial-contents (coerce jargs 'vector)))
               (seq (cl:funcall *sym-arrayseq-create* arr)))
          (openldk::|applyTo(Lclojure/lang/ISeq;)| fn seq)))))))

;;; ---------------------------------------------------------------------------
;;; register
;;; ---------------------------------------------------------------------------

(defvar *cl-function-class-defined* nil)

(defun %define-cl-function-class ()
  "Define the cl-function bridge class after Clojure classes are loaded."
  (when *cl-function-class-defined*
    (return-from %define-cl-function-class))

  (let ((afn-sym (%find-class "clojure/lang/AFn")))
    (unless afn-sym
      (warn "cl-clojure: clojure/lang/AFn class not found; register will not work")
      (return-from %define-cl-function-class))

    ;; Define the bridge class
    (cl:eval `(defclass clojure::cl-function (,afn-sym)
                ((clojure::%cl-fn :initarg :cl-function :accessor clojure::%cl-fn))
                (:documentation "Bridge class wrapping a CL function as a Clojure IFn.")))

    ;; 0-arg invoke
    (cl:eval `(defmethod ,(intern "invoke()" :openldk) ((fn clojure::cl-function))
                (clojure:cl->clojure (cl:funcall (clojure::%cl-fn fn)))))

    ;; 1-arg invoke
    (cl:eval `(defmethod ,(intern "invoke(Ljava/lang/Object;)" :openldk)
                  ((fn clojure::cl-function) arg)
                (clojure:cl->clojure (cl:funcall (clojure::%cl-fn fn) (clojure:clojure->cl arg)))))

    ;; 2-arg invoke
    (cl:eval `(defmethod ,(intern "invoke(Ljava/lang/Object;Ljava/lang/Object;)" :openldk)
                  ((fn clojure::cl-function) a b)
                (clojure:cl->clojure (cl:funcall (clojure::%cl-fn fn)
                                                 (clojure:clojure->cl a) (clojure:clojure->cl b)))))

    ;; 3-arg invoke
    (cl:eval `(defmethod ,(intern "invoke(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)" :openldk)
                  ((fn clojure::cl-function) a b c)
                (clojure:cl->clojure (cl:funcall (clojure::%cl-fn fn)
                                                 (clojure:clojure->cl a)
                                                 (clojure:clojure->cl b)
                                                 (clojure:clojure->cl c)))))

    ;; applyTo for arbitrary arity
    (cl:eval `(defmethod ,(intern "applyTo(Lclojure/lang/ISeq;)" :openldk)
                  ((fn clojure::cl-function) arglist)
                (let ((cl-args (loop for seq = arglist then (openldk::|next()| seq)
                                     while seq
                                     collect (clojure:clojure->cl (openldk::|first()| seq)))))
                  (clojure:cl->clojure (apply (clojure::%cl-fn fn) cl-args))))))

  (setf *cl-function-class-defined* t))

(defun register (name function &optional (ns-name "user"))
  "Register a CL function as a Clojure function with the given NAME in NS-NAME."
  (ensure-started)
  (let* ((ns-sym (cl:funcall *sym-symbol-intern*
                  (openldk::jstring ns-name)))
         (ns (cl:funcall *sym-ns-findorcreate* ns-sym))
         (sym (cl:funcall *sym-symbol-intern*
               (openldk::jstring name)))
         (bridge (make-instance 'cl-function :cl-function function)))
    (cl:funcall *sym-var-intern* ns sym bridge)
    name))

;;; ---------------------------------------------------------------------------
;;; make-namespace
;;; ---------------------------------------------------------------------------

(defun make-namespace (name)
  "Create or find a Clojure namespace with the given NAME."
  (ensure-started)
  (let ((sym (cl:funcall *sym-symbol-intern*
              (openldk::jstring name))))
    (cl:funcall *sym-ns-findorcreate* sym)))
