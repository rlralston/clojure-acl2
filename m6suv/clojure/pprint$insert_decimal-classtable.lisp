; pprint$insert_decimal-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$insert_decimal*
 (make-class-def
      '(class "clojure.pprint$insert_decimal"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "neg?")
                        (STRING  "str")
                        (STRING  "inc")
                        (STRING  "subs")
                        (STRING  "."))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 60)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "neg?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$insert_decimal" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "str"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$insert_decimal" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "inc"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$insert_decimal" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "subs"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.pprint$insert_decimal" (class "clojure.lang.Var"))))
                                      (52 (lconst_0))
                                      (53 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (56 (putstatic (fieldCP "const__4" "clojure.pprint$insert_decimal" (class "java.lang.Object"))))
                                      (59 (return))
                                      (endofcode 60))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 4) (code_length . 93)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (invokestatic (methodCP "isNeg" "clojure.lang.Numbers" ((class "java.lang.Object")) boolean))) 
                                      (4 (ifeq 30))  ;;to TAG_0
                                      (7 (getstatic (fieldCP "const__1" "clojure.pprint$insert_decimal" (class "clojure.lang.Var")))) 
                                      (10 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (13 (checkcast (class "clojure.lang.IFn"))) 
                                      (16 (ldc 5)) ;;STRING:: "."
                                      (18 (aload_1)) 
                                      (19 (aconst_null)) 
                                      (20 (astore_1)) 
                                      (21 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (26 (goto 92)) ;;to TAG_1
                                      (29 (pop)) 
                                      (30 (aload_2)) ;;at TAG_0
                                      (31 (aconst_null)) 
                                      (32 (astore_2)) 
                                      (33 (invokestatic (methodCP "inc" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (36 (astore_3)) 
                                      (37 (getstatic (fieldCP "const__1" "clojure.pprint$insert_decimal" (class "clojure.lang.Var")))) 
                                      (40 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (43 (checkcast (class "clojure.lang.IFn"))) 
                                      (46 (getstatic (fieldCP "const__3" "clojure.pprint$insert_decimal" (class "clojure.lang.Var")))) 
                                      (49 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (52 (checkcast (class "clojure.lang.IFn"))) 
                                      (55 (aload_1)) 
                                      (56 (getstatic (fieldCP "const__4" "clojure.pprint$insert_decimal" (class "java.lang.Object")))) 
                                      (59 (aload_3)) 
                                      (60 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (65 (ldc 5)) ;;STRING:: "."
                                      (67 (getstatic (fieldCP "const__3" "clojure.pprint$insert_decimal" (class "clojure.lang.Var")))) 
                                      (70 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (73 (checkcast (class "clojure.lang.IFn"))) 
                                      (76 (aload_1)) 
                                      (77 (aconst_null)) 
                                      (78 (astore_1)) 
                                      (79 (aload_3)) 
                                      (80 (aconst_null)) 
                                      (81 (astore_3)) 
                                      (82 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (87 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (92 (areturn)) ;;at TAG_1
                                      (endofcode 93))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$insert_decimal-class-table*
  (make-static-class-decls 
   *clojure.pprint$insert_decimal*))

(defconst *package-name-map* 
  ("clojure.pprint$insert_decimal" . "clojure"))
