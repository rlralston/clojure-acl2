; main$repl$read_eval_print__6588$fn__6589-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:53 CDT 2014.
;

(defconst *clojure.main$repl$read_eval_print__6588$fn__6589*
 (make-class-def
      '(class "clojure.main$repl$read_eval_print__6588$fn__6589"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "pop-thread-bindings"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "request_prompt" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "request_exit" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "read" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "pop-thread-bindings"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "request_prompt" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "request_exit" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "read" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 74)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (getfield (fieldCP "read" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "java.lang.Object")))) 
                                      (4 (aload_0)) 
                                      (5 (aconst_null)) 
                                      (6 (putfield (fieldCP "read" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "java.lang.Object")))) 
                                      (9 (checkcast (class "clojure.lang.IFn"))) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "request_prompt" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "java.lang.Object")))) 
                                      (16 (aload_0)) 
                                      (17 (aconst_null)) 
                                      (18 (putfield (fieldCP "request_prompt" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "java.lang.Object")))) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "request_exit" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "java.lang.Object")))) 
                                      (25 (aload_0)) 
                                      (26 (aconst_null)) 
                                      (27 (putfield (fieldCP "request_exit" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "java.lang.Object")))) 
                                      (30 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (35 (astore_1)) 
                                      (36 (getstatic (fieldCP "const__0" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (39 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (42 (checkcast (class "clojure.lang.IFn"))) 
                                      (45 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (50 (pop)) 
                                      (51 (goto 72)) ;;to TAG_0
                                      (54 (astore_2)) ;;at TAG_3
                                      (55 (getstatic (fieldCP "const__0" "clojure.main$repl$read_eval_print__6588$fn__6589" (class "clojure.lang.Var")))) 
                                      (58 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (61 (checkcast (class "clojure.lang.IFn"))) 
                                      (64 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (69 (pop)) 
                                      (70 (aload_2)) 
                                      (71 (athrow)) 
                                      (72 (aload_1)) ;;at TAG_0
                                      (73 (areturn)) 
                                      (endofcode 74))
                                   (Exceptions 
                                     (handler 0 36  54 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *main$repl$read_eval_print__6588$fn__6589-class-table*
  (make-static-class-decls 
   *clojure.main$repl$read_eval_print__6588$fn__6589*))

(defconst *package-name-map* 
  ("clojure.main$repl$read_eval_print__6588$fn__6589" . "clojure"))

