; core$gen_interface-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$gen_interface*
 (make-class-def
      '(class "clojure.core$gen_interface"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "apply")
                        (STRING  "hash-map")
                        (STRING  "generate-interface")
                        (STRING  "nth")
                        (STRING  "*compile-files*")
                        (STRING  "deref")
                        (STRING  "str")
                        (STRING  "name"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 138)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "apply"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$gen_interface" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "hash-map"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$gen_interface" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "generate-interface"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$gen_interface" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "nth"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$gen_interface" (class "clojure.lang.Var"))))
                                      (52 (lconst_0))
                                      (53 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (56 (putstatic (fieldCP "const__4" "clojure.core$gen_interface" (class "java.lang.Object"))))
                                      (59 (lconst_1))
                                      (60 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (63 (putstatic (fieldCP "const__5" "clojure.core$gen_interface" (class "java.lang.Object"))))
                                      (66 (ldc 0))        ;;STRING:: "clojure.core"
                                      (68 (ldc 5))        ;;STRING:: "*compile-files*"
                                      (70 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (73 (checkcast (class "clojure.lang.Var")))
                                      (76 (putstatic (fieldCP "const__6" "clojure.core$gen_interface" (class "clojure.lang.Var"))))
                                      (79 (ldc 0))        ;;STRING:: "clojure.core"
                                      (81 (ldc 6))        ;;STRING:: "deref"
                                      (83 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (86 (checkcast (class "clojure.lang.Var")))
                                      (89 (putstatic (fieldCP "const__7" "clojure.core$gen_interface" (class "clojure.lang.Var"))))
                                      (92 (ldc 0))        ;;STRING:: "clojure.core"
                                      (94 (ldc 7))        ;;STRING:: "str"
                                      (96 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (99 (checkcast (class "clojure.lang.Var")))
                                      (102 (putstatic (fieldCP "const__8" "clojure.core$gen_interface" (class "clojure.lang.Var"))))
                                      (105 (aconst_null))
                                      (106 (ldc 8))       ;;STRING:: "name"
                                      (108 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (111 (checkcast (class "clojure.lang.Keyword")))
                                      (114 (putstatic (fieldCP "const__9" "clojure.core$gen_interface" (class "clojure.lang.Keyword"))))
                                      (117 (new (class "clojure.lang.KeywordLookupSite")))
                                      (120 (dup))
                                      (121 (aconst_null))
                                      (122 (ldc 8))       ;;STRING:: "name"
                                      (124 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (127 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (130 (dup))
                                      (131 (putstatic (fieldCP "__site__0__" "clojure.core$gen_interface" (class "clojure.lang.KeywordLookupSite"))))
                                      (134 (putstatic (fieldCP "__thunk__0__" "clojure.core$gen_interface" (class "clojure.lang.ILookupThunk"))))
                                      (137 (return))
                                      (endofcode 138))
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
                                   (max_stack . 6) (max_locals . 8) (code_length . 205)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$gen_interface" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$gen_interface" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (aload_3)) 
                                      (16 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (21 (astore 4)) 
                                      (23 (getstatic (fieldCP "const__2" "clojure.core$gen_interface" (class "clojure.lang.Var")))) 
                                      (26 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (29 (checkcast (class "clojure.lang.IFn"))) 
                                      (32 (aload 4)) 
                                      (34 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (39 (astore 5)) 
                                      (41 (aload 5)) 
                                      (43 (lconst_0)) 
                                      (44 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (47 (aconst_null)) 
                                      (48 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (51 (astore 6)) 
                                      (53 (aload 5)) 
                                      (55 (aconst_null)) 
                                      (56 (astore 5)) 
                                      (58 (lconst_1)) 
                                      (59 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (62 (aconst_null)) 
                                      (63 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (66 (astore 7)) 
                                      (68 (getstatic (fieldCP "const__6" "clojure.core$gen_interface" (class "clojure.lang.Var")))) 
                                      (71 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (74 (dup)) 
                                      (75 (ifnull 107)) ;;to TAG_0
                                      (78 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (81 (if_acmpeq 108)) ;;to TAG_1
                                      (84 (aload 6)) 
                                      (86 (aconst_null)) 
                                      (87 (astore 6)) 
                                      (89 (checkcast (class "java.lang.String"))) 
                                      (92 (aload 7)) 
                                      (94 (aconst_null)) 
                                      (95 (astore 7)) 
                                      (97 (checkcast (array byte))) 
                                      (100 (invokestatic (methodCP "writeClassFile" "clojure.lang.Compiler" ((class "java.lang.String") (array byte)) void))) 
                                      (103 (aconst_null)) 
                                      (104 (goto 204))  ;;to TAG_2
                                      (107 (pop)) ;;at TAG_0
                                      (108 (getstatic (fieldCP "const__7" "clojure.core$gen_interface" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (111 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (114 (checkcast (class "clojure.lang.IFn"))) 
                                      (117 (getstatic (fieldCP "LOADER" "clojure.lang.Compiler" (class "clojure.lang.Var")))) 
                                      (120 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (125 (checkcast (class "clojure.lang.DynamicClassLoader"))) 
                                      (128 (getstatic (fieldCP "const__8" "clojure.core$gen_interface" (class "clojure.lang.Var")))) 
                                      (131 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (134 (checkcast (class "clojure.lang.IFn"))) 
                                      (137 (getstatic (fieldCP "__thunk__0__" "clojure.core$gen_interface" (class "clojure.lang.ILookupThunk")))) 
                                      (140 (dup)) 
                                      (141 (aload 4)) 
                                      (143 (aconst_null)) 
                                      (144 (astore 4)) 
                                      (146 (dup_x2)) 
                                      (147 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (152 (dup_x2)) 
                                      (153 (if_acmpeq 160)) ;;to TAG_3
                                      (156 (pop)) 
                                      (157 (goto 182)) ;;to TAG_4
                                      (160 (swap)) ;;at TAG_3
                                      (161 (pop)) 
                                      (162 (dup)) 
                                      (163 (getstatic (fieldCP "__site__0__" "clojure.core$gen_interface" (class "clojure.lang.KeywordLookupSite")))) 
                                      (166 (swap)) 
                                      (167 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (172 (dup)) 
                                      (173 (putstatic (fieldCP "__thunk__0__" "clojure.core$gen_interface" (class "clojure.lang.ILookupThunk")))) 
                                      (176 (swap)) 
                                      (177 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (182 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_4
                                      (187 (checkcast (class "java.lang.String"))) 
                                      (190 (aload 7)) 
                                      (192 (aconst_null)) 
                                      (193 (astore 7)) 
                                      (195 (checkcast (array byte))) 
                                      (198 (aload_3)) 
                                      (199 (aconst_null)) 
                                      (200 (astore_3)) 
                                      (201 (invokevirtual (methodCP "defineClass" "clojure.lang.DynamicClassLoader" ((class "java.lang.String") (array byte) (class "java.lang.Object")) (class "java.lang.Class")))) 
                                      (204 (areturn)) ;;at TAG_2
                                      (endofcode 205))
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
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 28)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 27 (0 . 0) (20))))  ;;to TAG_0;;to TAG_1
                                      (20 (aload_2)) ;;at TAG_1
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.core$gen_interface" (class "clojure.lang.ILookupThunk")))) 
                                      (24 (goto 27))  ;;to TAG_0
                                      (27 (return)) ;;at TAG_0
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$gen_interface-class-table*
  (make-static-class-decls 
   *clojure.core$gen_interface*))

(defconst *package-name-map* 
  ("clojure.core$gen_interface" . "clojure"))
