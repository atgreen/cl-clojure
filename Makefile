JAVA_HOME ?= /home/linuxbrew/.linuxbrew/opt/openjdk@8/libexec/jre
CLOJURE_JAR = libs/clojure-1.12.0.jar

check: $(CLOJURE_JAR)
	LDK_CLASSPATH=$(CLOJURE_JAR) JAVA_HOME=$(JAVA_HOME) \
	  sbcl --noinform --dynamic-space-size 32768 --disable-debugger \
	  --eval "(handler-bind ((warning #'muffle-warning)) (asdf:load-system :cl-clojure/tests))" \
	  --eval "(let ((results (5am:run :cl-clojure))) (5am:explain! results) (uiop:quit (if (5am:results-status results) 0 1)))"

cl-clojure: src/*.lisp *.asd Makefile $(CLOJURE_JAR)
	LDK_CLASSPATH=$(CLOJURE_JAR) JAVA_HOME=$(JAVA_HOME) \
	  sbcl --dynamic-space-size 32768 --disable-debugger \
	  --eval "(asdf:load-system :cl-clojure)" \
	  --eval "(clojure:make-image)"

$(CLOJURE_JAR):
	mvn dependency:copy-dependencies -DoutputDirectory=./libs
