; core$filter_key-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$filter_key*
 (make-class-def
      '(class "clojure.core$filter_key"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "first")
                        (STRING  "assoc")
                        (STRING  "key")
                        (STRING  "val")
                        (STRING  "next"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 79)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$filter_key" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "first"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$filter_key" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "assoc"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$filter_key" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "key"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$filter_key" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "val"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$filter_key" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "next"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$filter_key" (class "clojure.lang.Var"))))
                                      (78 (return))
                                      (endofcode 79))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 215)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentArrayMap" (class "clojure.lang.PersistentArrayMap")))) 
                                      (3 (astore 4)) 
                                      (5 (getstatic (fieldCP "const__0" "clojure.core$filter_key" (class "clojure.lang.Var")))) 
                                      (8 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (11 (checkcast (class "clojure.lang.IFn"))) 
                                      (14 (aload_3)) 
                                      (15 (aconst_null)) 
                                      (16 (astore_3)) 
                                      (17 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (22 (astore 5)) 
                                      (24 (aload 5)) ;;at TAG_4
                                      (26 (dup)) 
                                      (27 (ifnull 211)) ;;to TAG_0
                                      (30 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (33 (if_acmpeq 212)) ;;to TAG_1
                                      (36 (aload_2)) 
                                      (37 (checkcast (class "clojure.lang.IFn"))) 
                                      (40 (aload_1)) 
                                      (41 (checkcast (class "clojure.lang.IFn"))) 
                                      (44 (getstatic (fieldCP "const__1" "clojure.core$filter_key" (class "clojure.lang.Var")))) 
                                      (47 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (50 (checkcast (class "clojure.lang.IFn"))) 
                                      (53 (aload 5)) 
                                      (55 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (60 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (65 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (70 (dup)) 
                                      (71 (ifnull 182))  ;;to TAG_2
                                      (74 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (77 (if_acmpeq 183)) ;;to TAG_3
                                      (80 (getstatic (fieldCP "const__2" "clojure.core$filter_key" (class "clojure.lang.Var")))) 
                                      (83 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (86 (checkcast (class "clojure.lang.IFn"))) 
                                      (89 (aload 4)) 
                                      (91 (getstatic (fieldCP "const__3" "clojure.core$filter_key" (class "clojure.lang.Var")))) 
                                      (94 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (97 (checkcast (class "clojure.lang.IFn"))) 
                                      (100 (getstatic (fieldCP "const__1" "clojure.core$filter_key" (class "clojure.lang.Var")))) 
                                      (103 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (106 (checkcast (class "clojure.lang.IFn"))) 
                                      (109 (aload 5)) 
                                      (111 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (116 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (121 (getstatic (fieldCP "const__4" "clojure.core$filter_key" (class "clojure.lang.Var")))) 
                                      (124 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (127 (checkcast (class "clojure.lang.IFn"))) 
                                      (130 (getstatic (fieldCP "const__1" "clojure.core$filter_key" (class "clojure.lang.Var")))) 
                                      (133 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (136 (checkcast (class "clojure.lang.IFn"))) 
                                      (139 (aload 5)) 
                                      (141 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (146 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (151 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (156 (getstatic (fieldCP "const__5" "clojure.core$filter_key" (class "clojure.lang.Var")))) 
                                      (159 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (162 (checkcast (class "clojure.lang.IFn"))) 
                                      (165 (aload 5)) 
                                      (167 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (172 (astore 5)) 
                                      (174 (astore 4)) 
                                      (176 (goto 24)) ;;to TAG_4
                                      (179 (goto 208)) ;;to TAG_5
                                      (182 (pop)) ;;at TAG_2
                                      (183 (aload 4)) ;;at TAG_3
                                      (185 (getstatic (fieldCP "const__5" "clojure.core$filter_key" (class "clojure.lang.Var")))) 
                                      (188 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (191 (checkcast (class "clojure.lang.IFn"))) 
                                      (194 (aload 5)) 
                                      (196 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (201 (astore 5)) 
                                      (203 (astore 4)) 
                                      (205 (goto 24)) ;;to TAG_4
                                      (208 (goto 214)) ;;to TAG_6;;at TAG_5
                                      (211 (pop)) ;;at TAG_0
                                      (212 (aload 4)) ;;at TAG_1
                                      (214 (areturn)) ;;at TAG_6
                                      (endofcode 215))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$filter_key-class-table*
  (make-static-class-decls 
   *clojure.core$filter_key*))

(defconst *package-name-map* 
  ("clojure.core$filter_key" . "clojure"))

