; core$load$fn__5016-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$load$fn__5016*
 (make-class-def
      '(class "clojure.core$load$fn__5016"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "pop-thread-bindings"))
            (fields
                        (field "const__0" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "path" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 21)
                                   (parsedcode
                                      (0 (lconst_1))
                                      (1 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (4 (putstatic (fieldCP "const__0" "clojure.core$load$fn__5016" (class "java.lang.Object"))))
                                      (7 (ldc 0))         ;;STRING:: "clojure.core"
                                      (9 (ldc 1))         ;;STRING:: "pop-thread-bindings"
                                      (11 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (14 (checkcast (class "clojure.lang.Var")))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$load$fn__5016" (class "clojure.lang.Var"))))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "path" "clojure.core$load$fn__5016" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 60)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (getfield (fieldCP "path" "clojure.core$load$fn__5016" (class "java.lang.Object")))) 
                                      (4 (checkcast (class "java.lang.String"))) 
                                      (7 (lconst_1)) 
                                      (8 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (11 (invokevirtual (methodCP "substring" "java.lang.String" (int) (class "java.lang.String")))) 
                                      (14 (checkcast (class "java.lang.String"))) 
                                      (17 (invokestatic (methodCP "load" "clojure.lang.RT" ((class "java.lang.String")) void))) 
                                      (20 (aconst_null)) 
                                      (21 (astore_1)) 
                                      (22 (getstatic (fieldCP "const__1" "clojure.core$load$fn__5016" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (25 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (28 (checkcast (class "clojure.lang.IFn"))) 
                                      (31 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (36 (pop)) 
                                      (37 (goto 58)) ;;to TAG_0
                                      (40 (astore_2)) ;;at TAG_3
                                      (41 (getstatic (fieldCP "const__1" "clojure.core$load$fn__5016" (class "clojure.lang.Var")))) 
                                      (44 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (47 (checkcast (class "clojure.lang.IFn"))) 
                                      (50 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (55 (pop)) 
                                      (56 (aload_2)) 
                                      (57 (athrow)) 
                                      (58 (aload_1)) ;;at TAG_0
                                      (59 (areturn)) 
                                      (endofcode 60))
                                   (Exceptions 
                                     (handler 0 22  40 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$load$fn__5016-class-table*
  (make-static-class-decls 
   *clojure.core$load$fn__5016*))

(defconst *package-name-map* 
  ("clojure.core$load$fn__5016" . "clojure"))
