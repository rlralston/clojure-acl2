; core$overload_name-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$overload_name*
 (make-class-def
      '(class "clojure.core$overload_name"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "apply")
                        (STRING  "str")
                        (STRING  "interleave")
                        (STRING  "repeat")
                        (STRING  "map")
                        (STRING  "escape-class-name")
                        (STRING  "-void"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 100)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$overload_name" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "apply"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$overload_name" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "str"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$overload_name" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "interleave"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$overload_name" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "repeat"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$overload_name" (class "clojure.lang.Var"))))
                                      (65 (bipush 45))
                                      (67 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (70 (putstatic (fieldCP "const__5" "clojure.core$overload_name" (class "java.lang.Object"))))
                                      (73 (ldc 0))        ;;STRING:: "clojure.core"
                                      (75 (ldc 6))        ;;STRING:: "map"
                                      (77 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (80 (checkcast (class "clojure.lang.Var")))
                                      (83 (putstatic (fieldCP "const__6" "clojure.core$overload_name" (class "clojure.lang.Var"))))
                                      (86 (ldc 0))        ;;STRING:: "clojure.core"
                                      (88 (ldc 7))        ;;STRING:: "escape-class-name"
                                      (90 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (93 (checkcast (class "clojure.lang.Var")))
                                      (96 (putstatic (fieldCP "const__7" "clojure.core$overload_name" (class "clojure.lang.Var"))))
                                      (99 (return))
                                      (endofcode 100))
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
                                   (max_stack . 9) (max_locals . 3) (code_length . 126)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$overload_name" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_2)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 105)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 106)) ;;to TAG_1
                                      (25 (getstatic (fieldCP "const__1" "clojure.core$overload_name" (class "clojure.lang.Var")))) 
                                      (28 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (31 (checkcast (class "clojure.lang.IFn"))) 
                                      (34 (getstatic (fieldCP "const__2" "clojure.core$overload_name" (class "clojure.lang.Var")))) 
                                      (37 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (40 (aload_1)) 
                                      (41 (aconst_null)) 
                                      (42 (astore_1)) 
                                      (43 (getstatic (fieldCP "const__3" "clojure.core$overload_name" (class "clojure.lang.Var")))) 
                                      (46 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (49 (checkcast (class "clojure.lang.IFn"))) 
                                      (52 (getstatic (fieldCP "const__4" "clojure.core$overload_name" (class "clojure.lang.Var")))) 
                                      (55 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (58 (checkcast (class "clojure.lang.IFn"))) 
                                      (61 (getstatic (fieldCP "const__5" "clojure.core$overload_name" (class "java.lang.Object")))) 
                                      (64 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (69 (getstatic (fieldCP "const__6" "clojure.core$overload_name" (class "clojure.lang.Var")))) 
                                      (72 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (75 (checkcast (class "clojure.lang.IFn"))) 
                                      (78 (getstatic (fieldCP "const__7" "clojure.core$overload_name" (class "clojure.lang.Var")))) 
                                      (81 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (84 (aload_2)) 
                                      (85 (aconst_null)) 
                                      (86 (astore_2)) 
                                      (87 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (92 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (97 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (102 (goto 125))  ;;to TAG_2
                                      (105 (pop)) ;;at TAG_0
                                      (106 (getstatic (fieldCP "const__2" "clojure.core$overload_name" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (109 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (112 (checkcast (class "clojure.lang.IFn"))) 
                                      (115 (aload_1)) 
                                      (116 (aconst_null)) 
                                      (117 (astore_1)) 
                                      (118 (ldc 8)) ;;STRING:: "-void"
                                      (120 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (125 (areturn)) ;;at TAG_2
                                      (endofcode 126))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$overload_name-class-table*
  (make-static-class-decls 
   *clojure.core$overload_name*))

(defconst *package-name-map* 
  ("clojure.core$overload_name" . "clojure"))

