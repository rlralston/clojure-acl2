; set$intersection-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.set$intersection*
 (make-class-def
      '(class "clojure.set$intersection"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "<")
                        (STRING  "count")
                        (STRING  "reduce")
                        (STRING  "clojure.set")
                        (STRING  "bubble-max-key")
                        (STRING  "conj")
                        (STRING  "intersection")
                        (STRING  "first")
                        (STRING  "rest"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 105)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "<"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "count"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "reduce"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (39 (ldc 4))        ;;STRING:: "clojure.set"
                                      (41 (ldc 5))        ;;STRING:: "bubble-max-key"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 6))        ;;STRING:: "conj"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (65 (ldc 4))        ;;STRING:: "clojure.set"
                                      (67 (ldc 7))        ;;STRING:: "intersection"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 8))        ;;STRING:: "first"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 9))        ;;STRING:: "rest"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (104 (return))
                                      (endofcode 105))
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 102)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__3" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (new (class "clojure.set$intersection$fn__6658")))
                                      (12 (dup))
                                      (13 (invokespecial
					(methodCP "<init>" "clojure.set$intersection$fn__6658" () void)))
                                      (16 (getstatic (fieldCP "const__4" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (19 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (22 (checkcast (class "clojure.lang.IFn")))
                                      (25 (aload_3))
                                      (26 (aconst_null))
                                      (27 (astore_3))
                                      (28 (aload_2))
                                      (29 (aconst_null))
                                      (30 (astore_2))
                                      (31 (aload_1))
                                      (32 (aconst_null))
                                      (33 (astore_1))
                                      (34 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (39 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (44 (astore 4))
                                      (46 (getstatic (fieldCP "const__2" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (49 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (52 (checkcast (class "clojure.lang.IFn")))
                                      (55 (getstatic (fieldCP "const__5" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (58 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (61 (getstatic (fieldCP "const__6" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (64 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (67 (checkcast (class "clojure.lang.IFn")))
                                      (70 (aload 4))
                                      (72 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (77 (getstatic (fieldCP "const__7" "clojure.set$intersection" (class "clojure.lang.Var"))))
                                      (80 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (83 (checkcast (class "clojure.lang.IFn")))
                                      (86 (aload 4))
                                      (88 (aconst_null))
                                      (89 (astore 4))
                                      (91 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (96 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (101 (areturn))
                                      (endofcode 102))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 58)
                                   (parsedcode
                                      (0 (aload_2)) ;;at TAG_1
                                      (1 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (4 (i2l)) 
                                      (5 (aload_1)) 
                                      (6 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (9 (i2l)) 
                                      (10 (lcmp)) 
                                      (11 (ifge 29)) ;;to TAG_0
                                      (14 (aload_2)) 
                                      (15 (aconst_null)) 
                                      (16 (astore_2)) 
                                      (17 (aload_1)) 
                                      (18 (aconst_null)) 
                                      (19 (astore_1)) 
                                      (20 (astore_2)) 
                                      (21 (astore_1)) 
                                      (22 (goto 0)) ;;to TAG_1
                                      (25 (goto 57))  ;;to TAG_2
                                      (28 (pop)) 
                                      (29 (getstatic (fieldCP "const__2" "clojure.set$intersection" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (32 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (35 (checkcast (class "clojure.lang.IFn"))) 
                                      (38 (new (class "clojure.set$intersection$fn__6656"))) 
                                      (41 (dup)) 
                                      (42 (aload_2)) 
                                      (43 (aconst_null)) 
                                      (44 (astore_2)) 
                                      (45 (invokespecial (methodCP "<init>" "clojure.set$intersection$fn__6656" ((class "java.lang.Object")) void))) 
                                      (48 (aload_1)) 
                                      (49 (aload_1)) 
                                      (50 (aconst_null)) 
                                      (51 (astore_1)) 
                                      (52 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (57 (areturn)) ;;at TAG_2
                                      (endofcode 58))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 4)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *set$intersection-class-table*
  (make-static-class-decls 
   *clojure.set$intersection*))

(defconst *package-name-map* 
  ("clojure.set$intersection" . "clojure"))

