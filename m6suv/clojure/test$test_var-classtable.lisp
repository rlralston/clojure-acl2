; test$test_var-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.test$test_var*
 (make-class-def
      '(class "clojure.test$test_var"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "test")
                        (STRING  "clojure.core")
                        (STRING  "meta")
                        (STRING  "push-thread-bindings")
                        (STRING  "hash-map")
                        (STRING  "clojure.test")
                        (STRING  "*testing-vars*")
                        (STRING  "conj")
                        (STRING  "do-report")
                        (STRING  "type")
                        (STRING  "begin-test-var")
                        (STRING  "var")
                        (STRING  "inc-report-counter")
                        (STRING  "end-test-var")
                        (STRING  "pop-thread-bindings"))
            (fields
                        (field "const__0" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 185)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (ldc 0))         ;;STRING:: "test"
                                      (3 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (6 (checkcast (class "clojure.lang.Keyword")))
                                      (9 (putstatic (fieldCP "const__0" "clojure.test$test_var" (class "clojure.lang.Keyword"))))
                                      (12 (ldc 1))        ;;STRING:: "clojure.core"
                                      (14 (ldc 2))        ;;STRING:: "meta"
                                      (16 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (19 (checkcast (class "clojure.lang.Var")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.test$test_var" (class "clojure.lang.Var"))))
                                      (25 (ldc 1))        ;;STRING:: "clojure.core"
                                      (27 (ldc 3))        ;;STRING:: "push-thread-bindings"
                                      (29 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (32 (checkcast (class "clojure.lang.Var")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.test$test_var" (class "clojure.lang.Var"))))
                                      (38 (ldc 1))        ;;STRING:: "clojure.core"
                                      (40 (ldc 4))        ;;STRING:: "hash-map"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.test$test_var" (class "clojure.lang.Var"))))
                                      (51 (ldc 5))        ;;STRING:: "clojure.test"
                                      (53 (ldc 6))        ;;STRING:: "*testing-vars*"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.test$test_var" (class "clojure.lang.Var"))))
                                      (64 (ldc 1))        ;;STRING:: "clojure.core"
                                      (66 (ldc 7))        ;;STRING:: "conj"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.test$test_var" (class "clojure.lang.Var"))))
                                      (77 (ldc 5))        ;;STRING:: "clojure.test"
                                      (79 (ldc 8))        ;;STRING:: "do-report"
                                      (81 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (84 (checkcast (class "clojure.lang.Var")))
                                      (87 (putstatic (fieldCP "const__6" "clojure.test$test_var" (class "clojure.lang.Var"))))
                                      (90 (aconst_null))
                                      (91 (ldc 9))        ;;STRING:: "type"
                                      (93 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (96 (checkcast (class "clojure.lang.Keyword")))
                                      (99 (putstatic (fieldCP "const__7" "clojure.test$test_var" (class "clojure.lang.Keyword"))))
                                      (102 (aconst_null))
                                      (103 (ldc 10))      ;;STRING:: "begin-test-var"
                                      (105 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (108 (checkcast (class "clojure.lang.Keyword")))
                                      (111 (putstatic (fieldCP "const__8" "clojure.test$test_var" (class "clojure.lang.Keyword"))))
                                      (114 (aconst_null))
                                      (115 (ldc 11))      ;;STRING:: "var"
                                      (117 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (120 (checkcast (class "clojure.lang.Keyword")))
                                      (123 (putstatic (fieldCP "const__9" "clojure.test$test_var" (class "clojure.lang.Keyword"))))
                                      (126 (ldc 5))       ;;STRING:: "clojure.test"
                                      (128 (ldc 12))      ;;STRING:: "inc-report-counter"
                                      (130 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (133 (checkcast (class "clojure.lang.Var")))
                                      (136 (putstatic (fieldCP "const__10" "clojure.test$test_var" (class "clojure.lang.Var"))))
                                      (139 (aconst_null))
                                      (140 (ldc 13))      ;;STRING:: "end-test-var"
                                      (142 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (145 (checkcast (class "clojure.lang.Keyword")))
                                      (148 (putstatic (fieldCP "const__11" "clojure.test$test_var" (class "clojure.lang.Keyword"))))
                                      (151 (ldc 1))       ;;STRING:: "clojure.core"
                                      (153 (ldc 14))      ;;STRING:: "pop-thread-bindings"
                                      (155 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (158 (checkcast (class "clojure.lang.Var")))
                                      (161 (putstatic (fieldCP "const__12" "clojure.test$test_var" (class "clojure.lang.Var"))))
                                      (164 (new (class "clojure.lang.KeywordLookupSite")))
                                      (167 (dup))
                                      (168 (aconst_null))
                                      (169 (ldc 0))       ;;STRING:: "test"
                                      (171 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (174 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (177 (dup))
                                      (178 (putstatic (fieldCP "__site__0__" "clojure.test$test_var" (class "clojure.lang.KeywordLookupSite"))))
                                      (181 (putstatic (fieldCP "__thunk__0__" "clojure.test$test_var" (class "clojure.lang.ILookupThunk"))))
                                      (184 (return))
                                      (endofcode 185))
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
                                   (max_stack . 6) (max_locals . 6) (code_length . 298)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.test$test_var" (class "clojure.lang.ILookupThunk")))) 
                                      (3 (dup)) 
                                      (4 (getstatic (fieldCP "const__1" "clojure.test$test_var" (class "clojure.lang.Var")))) 
                                      (7 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (10 (checkcast (class "clojure.lang.IFn"))) 
                                      (13 (aload_1)) 
                                      (14 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (19 (dup_x2)) 
                                      (20 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (25 (dup_x2)) 
                                      (26 (if_acmpeq 33)) ;;to TAG_0
                                      (29 (pop)) 
                                      (30 (goto 55))  ;;to TAG_1
                                      (33 (swap)) ;;at TAG_0
                                      (34 (pop)) 
                                      (35 (dup)) 
                                      (36 (getstatic (fieldCP "__site__0__" "clojure.test$test_var" (class "clojure.lang.KeywordLookupSite")))) 
                                      (39 (swap)) 
                                      (40 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (45 (dup)) 
                                      (46 (putstatic (fieldCP "__thunk__0__" "clojure.test$test_var" (class "clojure.lang.ILookupThunk")))) 
                                      (49 (swap)) 
                                      (50 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (55 (astore_2)) ;;at TAG_1
                                      (56 (aload_2)) 
                                      (57 (dup)) 
                                      (58 (ifnull 295)) ;;to TAG_2
                                      (61 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (64 (if_acmpeq 296)) ;;to TAG_3
                                      (67 (aload_2)) 
                                      (68 (aconst_null)) 
                                      (69 (astore_2)) 
                                      (70 (astore_3)) 
                                      (71 (getstatic (fieldCP "const__2" "clojure.test$test_var" (class "clojure.lang.Var")))) 
                                      (74 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (77 (checkcast (class "clojure.lang.IFn"))) 
                                      (80 (getstatic (fieldCP "const__3" "clojure.test$test_var" (class "clojure.lang.Var")))) 
                                      (83 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (86 (checkcast (class "clojure.lang.IFn"))) 
                                      (89 (getstatic (fieldCP "const__4" "clojure.test$test_var" (class "clojure.lang.Var")))) 
                                      (92 (getstatic (fieldCP "const__5" "clojure.test$test_var" (class "clojure.lang.Var")))) 
                                      (95 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (98 (checkcast (class "clojure.lang.IFn"))) 
                                      (101 (getstatic (fieldCP "const__4" "clojure.test$test_var" (class "clojure.lang.Var")))) 
                                      (104 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (107 (aload_1)) 
                                      (108 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (113 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (118 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (123 (pop)) 
                                      (124 (getstatic (fieldCP "const__6" "clojure.test$test_var" (class "clojure.lang.Var")))) ;;at TAG_6
                                      (127 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (130 (checkcast (class "clojure.lang.IFn"))) 
                                      (133 (iconst_4)) 
                                      (134 (anewarray (class "java.lang.Object"))) 
                                      (137 (dup)) 
                                      (138 (iconst_0)) 
                                      (139 (getstatic (fieldCP "const__7" "clojure.test$test_var" (class "clojure.lang.Keyword")))) 
                                      (142 (aastore)) 
                                      (143 (dup)) 
                                      (144 (iconst_1)) 
                                      (145 (getstatic (fieldCP "const__8" "clojure.test$test_var" (class "clojure.lang.Keyword")))) 
                                      (148 (aastore)) 
                                      (149 (dup)) 
                                      (150 (iconst_2)) 
                                      (151 (getstatic (fieldCP "const__9" "clojure.test$test_var" (class "clojure.lang.Keyword")))) 
                                      (154 (aastore)) 
                                      (155 (dup)) 
                                      (156 (iconst_3)) 
                                      (157 (aload_1)) 
                                      (158 (aastore)) 
                                      (159 (invokestatic (methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (162 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (167 (pop)) 
                                      (168 (getstatic (fieldCP "const__10" "clojure.test$test_var" (class "clojure.lang.Var")))) 
                                      (171 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (174 (checkcast (class "clojure.lang.IFn"))) 
                                      (177 (getstatic (fieldCP "const__0" "clojure.test$test_var" (class "clojure.lang.Keyword")))) 
                                      (180 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (185 (pop)) 
                                      (186 (new (class "clojure.test$test_var$fn__7145"))) 
                                      (189 (dup)) 
                                      (190 (aload_3)) 
                                      (191 (aconst_null)) 
                                      (192 (astore_3)) 
                                      (193 (invokespecial (methodCP "<init>" "clojure.test$test_var$fn__7145" ((class "java.lang.Object")) void))) 
                                      (196 (checkcast (class "clojure.lang.IFn"))) 
                                      (199 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (204 (pop)) 
                                      (205 (getstatic (fieldCP "const__6" "clojure.test$test_var" (class "clojure.lang.Var")))) 
                                      (208 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (211 (checkcast (class "clojure.lang.IFn"))) 
                                      (214 (iconst_4)) 
                                      (215 (anewarray (class "java.lang.Object"))) 
                                      (218 (dup)) 
                                      (219 (iconst_0)) 
                                      (220 (getstatic (fieldCP "const__7" "clojure.test$test_var" (class "clojure.lang.Keyword")))) 
                                      (223 (aastore)) 
                                      (224 (dup)) 
                                      (225 (iconst_1)) 
                                      (226 (getstatic (fieldCP "const__11" "clojure.test$test_var" (class "clojure.lang.Keyword")))) 
                                      (229 (aastore)) 
                                      (230 (dup)) 
                                      (231 (iconst_2)) 
                                      (232 (getstatic (fieldCP "const__9" "clojure.test$test_var" (class "clojure.lang.Keyword")))) 
                                      (235 (aastore)) 
                                      (236 (dup)) 
                                      (237 (iconst_3)) 
                                      (238 (aload_1)) 
                                      (239 (aconst_null)) 
                                      (240 (astore_1)) 
                                      (241 (aastore)) 
                                      (242 (invokestatic (methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (245 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (250 (astore 4)) 
                                      (252 (getstatic (fieldCP "const__12" "clojure.test$test_var" (class "clojure.lang.Var")))) ;;at TAG_7
                                      (255 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (258 (checkcast (class "clojure.lang.IFn"))) 
                                      (261 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (266 (pop)) 
                                      (267 (goto 290)) ;;to TAG_4
                                      (270 (astore 5)) ;;at TAG_8
                                      (272 (getstatic (fieldCP "const__12" "clojure.test$test_var" (class "clojure.lang.Var")))) 
                                      (275 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (278 (checkcast (class "clojure.lang.IFn"))) 
                                      (281 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (286 (pop)) 
                                      (287 (aload 5)) 
                                      (289 (athrow)) 
                                      (290 (aload 4)) ;;at TAG_4
                                      (292 (goto 297)) ;;to TAG_5
                                      (295 (pop)) ;;at TAG_2
                                      (296 (aconst_null)) ;;at TAG_3
                                      (297 (areturn)) ;;at TAG_5
                                      (endofcode 298))
                                   (Exceptions 
                                     (handler 124 252  270 (class "java.lang.Throwable")))
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.test$test_var" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *test$test_var-class-table*
  (make-static-class-decls 
   *clojure.test$test_var*))

(defconst *package-name-map* 
  ("clojure.test$test_var" . "clojure"))

