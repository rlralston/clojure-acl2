; repl$set_break_handler_BANG_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.repl$set_break_handler_BANG_*
 (make-class-def
      '(class "clojure.repl$set_break_handler_BANG_"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.repl")
                        (STRING  "set-break-handler!")
                        (STRING  "thread-stopper")
                        (STRING  "clojure.core")
                        (STRING  "init-proxy")
                        (STRING  "INT")
                        (STRING  "handle"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 40)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.repl"
                                      (2 (ldc 1))         ;;STRING:: "set-break-handler!"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.repl$set_break_handler_BANG_" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.repl"
                                      (15 (ldc 2))        ;;STRING:: "thread-stopper"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.repl$set_break_handler_BANG_" (class "clojure.lang.Var"))))
                                      (26 (ldc 3))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "init-proxy"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.repl$set_break_handler_BANG_" (class "clojure.lang.Var"))))
                                      (39 (return))
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 10) (max_locals . 3) (code_length . 74)
                                   (parsedcode
                                      (0 (new (class "sun.misc.Signal")))
                                      (3 (dup))
                                      (4 (ldc 5))         ;;STRING:: "INT"
                                      (6 (checkcast (class "java.lang.String")))
                                      (9 (invokespecial
					(methodCP "<init>" "sun.misc.Signal" ((class "java.lang.String")) void)))
                                      (12 (checkcast (class "sun.misc.Signal")))
                                      (15 (new (class "clojure.repl.proxy$java.lang.Object$SignalHandler$7826cc2")))
                                      (18 (dup))
                                      (19 (invokespecial
					(methodCP "<init>" "clojure.repl.proxy$java.lang.Object$SignalHandler$7826cc2" () void)))
                                      (22 (astore_2))
                                      (23 (getstatic (fieldCP "const__2" "clojure.repl$set_break_handler_BANG_" (class "clojure.lang.Var"))))
                                      (26 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (29 (checkcast (class "clojure.lang.IFn")))
                                      (32 (aload_2))
                                      (33 (iconst_2))
                                      (34 (anewarray (class "java.lang.Object")))
                                      (37 (dup))
                                      (38 (iconst_0))
                                      (39 (ldc 6))        ;;STRING:: "handle"
                                      (41 (aastore))
                                      (42 (dup))
                                      (43 (iconst_1))
                                      (44 (new (class "clojure.repl$set_break_handler_BANG_$fn__8806")))
                                      (47 (dup))
                                      (48 (aload_1))
                                      (49 (aconst_null))
                                      (50 (astore_1))
                                      (51 (invokespecial
					(methodCP "<init>" "clojure.repl$set_break_handler_BANG_$fn__8806" ((class "java.lang.Object")) void)))
                                      (54 (aastore))
                                      (55 (invokestatic
					(methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (58 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (63 (pop))
                                      (64 (aload_2))
                                      (65 (aconst_null))
                                      (66 (astore_2))
                                      (67 (checkcast (class "sun.misc.SignalHandler")))
                                      (70 (invokestatic
					(methodCP "handle" "sun.misc.Signal" ((class "sun.misc.Signal") (class "sun.misc.SignalHandler")) (class "sun.misc.SignalHandler"))))
                                      (73 (areturn))
                                      (endofcode 74))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 29)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.repl$set_break_handler_BANG_" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.repl$set_break_handler_BANG_" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1))
                                      (23 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (28 (areturn))
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *repl$set_break_handler_BANG_-class-table*
  (make-static-class-decls 
   *clojure.repl$set_break_handler_BANG_*))

(defconst *package-name-map* 
  ("clojure.repl$set_break_handler_BANG_" . "clojure"))

