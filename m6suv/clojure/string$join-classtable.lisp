; string$join-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.string$join*
 (make-class-def
      '(class "clojure.string$join"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "apply")
                        (STRING  "str")
                        (STRING  "first")
                        (STRING  "next"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 53)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "apply"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.string$join" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "str"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.string$join" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "first"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.string$join" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "next"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.string$join" (class "clojure.lang.Var"))))
                                      (52 (return))
                                      (endofcode 53))
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
                                   (max_stack . 5) (max_locals . 6) (code_length . 187)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder"))) 
                                      (3 (dup)) 
                                      (4 (getstatic (fieldCP "const__1" "clojure.string$join" (class "clojure.lang.Var")))) 
                                      (7 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (10 (checkcast (class "clojure.lang.IFn"))) 
                                      (13 (getstatic (fieldCP "const__2" "clojure.string$join" (class "clojure.lang.Var")))) 
                                      (16 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (19 (checkcast (class "clojure.lang.IFn"))) 
                                      (22 (aload_2)) 
                                      (23 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (28 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (33 (checkcast (class "java.lang.String"))) 
                                      (36 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" ((class "java.lang.String")) void))) 
                                      (39 (astore_3)) 
                                      (40 (getstatic (fieldCP "const__3" "clojure.string$join" (class "clojure.lang.Var")))) 
                                      (43 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (46 (checkcast (class "clojure.lang.IFn"))) 
                                      (49 (aload_2)) 
                                      (50 (aconst_null)) 
                                      (51 (astore_2)) 
                                      (52 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (57 (astore 4)) 
                                      (59 (getstatic (fieldCP "const__1" "clojure.string$join" (class "clojure.lang.Var")))) 
                                      (62 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (65 (checkcast (class "clojure.lang.IFn"))) 
                                      (68 (aload_1)) 
                                      (69 (aconst_null)) 
                                      (70 (astore_1)) 
                                      (71 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (76 (astore 5)) 
                                      (78 (aload 4)) ;;at TAG_2
                                      (80 (dup)) 
                                      (81 (ifnull 170)) ;;to TAG_0
                                      (84 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (87 (if_acmpeq 171)) ;;to TAG_1
                                      (90 (aload_3)) 
                                      (91 (checkcast (class "java.lang.StringBuilder"))) 
                                      (94 (aload 5)) 
                                      (96 (checkcast (class "java.lang.String"))) 
                                      (99 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (102 (checkcast (class "java.lang.StringBuilder"))) 
                                      (105 (getstatic (fieldCP "const__1" "clojure.string$join" (class "clojure.lang.Var")))) 
                                      (108 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (111 (checkcast (class "clojure.lang.IFn"))) 
                                      (114 (getstatic (fieldCP "const__2" "clojure.string$join" (class "clojure.lang.Var")))) 
                                      (117 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (120 (checkcast (class "clojure.lang.IFn"))) 
                                      (123 (aload 4)) 
                                      (125 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (130 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (135 (checkcast (class "java.lang.String"))) 
                                      (138 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (141 (getstatic (fieldCP "const__3" "clojure.string$join" (class "clojure.lang.Var")))) 
                                      (144 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (147 (checkcast (class "clojure.lang.IFn"))) 
                                      (150 (aload 4)) 
                                      (152 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (157 (aload 5)) 
                                      (159 (astore 5)) 
                                      (161 (astore 4)) 
                                      (163 (astore_3)) 
                                      (164 (goto 78))  ;;to TAG_2
                                      (167 (goto 186)) ;;to TAG_3
                                      (170 (pop)) ;;at TAG_0
                                      (171 (getstatic (fieldCP "const__1" "clojure.string$join" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (174 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (177 (checkcast (class "clojure.lang.IFn"))) 
                                      (180 (aload_3)) 
                                      (181 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (186 (areturn)) ;;at TAG_3
                                      (endofcode 187))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.string$join" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.string$join" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (aload_1))
                                      (16 (aconst_null))
                                      (17 (astore_1))
                                      (18 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (23 (areturn))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *string$join-class-table*
  (make-static-class-decls 
   *clojure.string$join*))

(defconst *package-name-map* 
  ("clojure.string$join" . "clojure"))

