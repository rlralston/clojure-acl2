; pprint$write$fn__7583-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$write$fn__7583*
 (make-class-def
      '(class "clojure.pprint$write$fn__7583"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.pprint")
                        (STRING  "write-out")
                        (STRING  "clojure.core")
                        (STRING  "*out*")
                        (STRING  "pop-thread-bindings")
                        (STRING  "ppflush"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "object" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 40)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.pprint"
                                      (2 (ldc 1))         ;;STRING:: "write-out"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$write$fn__7583" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "*out*"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$write$fn__7583" (class "clojure.lang.Var"))))
                                      (26 (ldc 2))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "pop-thread-bindings"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$write$fn__7583" (class "clojure.lang.Var"))))
                                      (39 (return))
                                      (endofcode 40))
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
                                      (6 (putfield (fieldCP "object" "clojure.pprint$write$fn__7583" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 69)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$write$fn__7583" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "object" "clojure.pprint$write$fn__7583" (class "java.lang.Object")))) 
                                      (13 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (18 (pop)) 
                                      (19 (getstatic (fieldCP "const__1" "clojure.pprint$write$fn__7583" (class "clojure.lang.Var")))) 
                                      (22 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (25 (ldc 5)) ;;STRING:: "ppflush"
                                      (27 (invokestatic (methodCP "invokeNoArgInstanceMember" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object")))) 
                                      (30 (astore_1)) 
                                      (31 (getstatic (fieldCP "const__2" "clojure.pprint$write$fn__7583" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (34 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (37 (checkcast (class "clojure.lang.IFn"))) 
                                      (40 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (45 (pop)) 
                                      (46 (goto 67)) ;;to TAG_0
                                      (49 (astore_2)) ;;at TAG_3
                                      (50 (getstatic (fieldCP "const__2" "clojure.pprint$write$fn__7583" (class "clojure.lang.Var")))) 
                                      (53 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (56 (checkcast (class "clojure.lang.IFn"))) 
                                      (59 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (64 (pop)) 
                                      (65 (aload_2)) 
                                      (66 (athrow)) 
                                      (67 (aload_1)) ;;at TAG_0
                                      (68 (areturn)) 
                                      (endofcode 69))
                                   (Exceptions 
                                     (handler 0 31  49 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$write$fn__7583-class-table*
  (make-static-class-decls 
   *clojure.pprint$write$fn__7583*))

(defconst *package-name-map* 
  ("clojure.pprint$write$fn__7583" . "clojure"))
