; set$join$fn__6698-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.set$join$fn__6698*
 (make-class-def
      '(class "clojure.set$join$fn__6698"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.set")
                        (STRING  "rename-keys")
                        (STRING  "clojure.core")
                        (STRING  "select-keys")
                        (STRING  "keys")
                        (STRING  "reduce"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "k" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "idx" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 53)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.set"
                                      (2 (ldc 1))         ;;STRING:: "rename-keys"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.set$join$fn__6698" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "select-keys"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.set$join$fn__6698" (class "clojure.lang.Var"))))
                                      (26 (ldc 2))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "keys"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.set$join$fn__6698" (class "clojure.lang.Var"))))
                                      (39 (ldc 2))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "reduce"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.set$join$fn__6698" (class "clojure.lang.Var"))))
                                      (52 (return))
                                      (endofcode 53))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "k" "clojure.set$join$fn__6698" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "idx" "clojure.set$join$fn__6698" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 113)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "idx" "clojure.set$join$fn__6698" (class "java.lang.Object")))) 
                                      (4 (checkcast (class "clojure.lang.IFn"))) 
                                      (7 (getstatic (fieldCP "const__0" "clojure.set$join$fn__6698" (class "clojure.lang.Var")))) 
                                      (10 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (13 (checkcast (class "clojure.lang.IFn"))) 
                                      (16 (getstatic (fieldCP "const__1" "clojure.set$join$fn__6698" (class "clojure.lang.Var")))) 
                                      (19 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (22 (checkcast (class "clojure.lang.IFn"))) 
                                      (25 (aload_2)) 
                                      (26 (getstatic (fieldCP "const__2" "clojure.set$join$fn__6698" (class "clojure.lang.Var")))) 
                                      (29 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (32 (checkcast (class "clojure.lang.IFn"))) 
                                      (35 (aload_0)) 
                                      (36 (getfield (fieldCP "k" "clojure.set$join$fn__6698" (class "java.lang.Object")))) 
                                      (39 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (44 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "k" "clojure.set$join$fn__6698" (class "java.lang.Object")))) 
                                      (53 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (58 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (63 (astore_3)) 
                                      (64 (aload_3)) 
                                      (65 (dup)) 
                                      (66 (ifnull 108)) ;;to TAG_0
                                      (69 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (72 (if_acmpeq 109)) ;;to TAG_1
                                      (75 (getstatic (fieldCP "const__3" "clojure.set$join$fn__6698" (class "clojure.lang.Var")))) 
                                      (78 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (81 (checkcast (class "clojure.lang.IFn"))) 
                                      (84 (new (class "clojure.set$join$fn__6698$fn__6699"))) 
                                      (87 (dup)) 
                                      (88 (aload_2)) 
                                      (89 (aconst_null)) 
                                      (90 (astore_2)) 
                                      (91 (invokespecial (methodCP "<init>" "clojure.set$join$fn__6698$fn__6699" ((class "java.lang.Object")) void))) 
                                      (94 (aload_1)) 
                                      (95 (aconst_null)) 
                                      (96 (astore_1)) 
                                      (97 (aload_3)) 
                                      (98 (aconst_null)) 
                                      (99 (astore_3)) 
                                      (100 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (105 (goto 112))  ;;to TAG_2
                                      (108 (pop)) ;;at TAG_0
                                      (109 (aload_1)) ;;at TAG_1
                                      (110 (aconst_null)) 
                                      (111 (astore_1)) 
                                      (112 (areturn)) ;;at TAG_2
                                      (endofcode 113))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *set$join$fn__6698-class-table*
  (make-static-class-decls 
   *clojure.set$join$fn__6698*))

(defconst *package-name-map* 
  ("clojure.set$join$fn__6698" . "clojure"))

