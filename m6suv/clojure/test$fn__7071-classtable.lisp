; test$fn__7071-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.test$fn__7071*
 (make-class-def
      '(class "clojure.test$fn__7071"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "push-thread-bindings")
                        (STRING  "hash-map")
                        (STRING  "*out*")
                        (STRING  "clojure.test")
                        (STRING  "*test-out*")
                        (STRING  "println")
                        (STRING  "test")
                        (STRING  "+")
                        (STRING  "pass")
                        (STRING  "fail")
                        (STRING  "error")
                        (STRING  "pop-thread-bindings")
                        (STRING  "\nRan")
                        (STRING  "tests containing")
                        (STRING  "assertions.")
                        (STRING  "failures,")
                        (STRING  "errors."))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__1__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__1__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__2__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__2__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__3__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__3__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__4__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__4__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__5__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__5__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 260)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "push-thread-bindings"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.test$fn__7071" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "hash-map"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.test$fn__7071" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "*out*"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.test$fn__7071" (class "clojure.lang.Var"))))
                                      (39 (ldc 4))        ;;STRING:: "clojure.test"
                                      (41 (ldc 5))        ;;STRING:: "*test-out*"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.test$fn__7071" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 6))        ;;STRING:: "println"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.test$fn__7071" (class "clojure.lang.Var"))))
                                      (65 (aconst_null))
                                      (66 (ldc 7))        ;;STRING:: "test"
                                      (68 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (71 (checkcast (class "clojure.lang.Keyword")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.test$fn__7071" (class "clojure.lang.Keyword"))))
                                      (77 (ldc 0))        ;;STRING:: "clojure.core"
                                      (79 (ldc 8))        ;;STRING:: "+"
                                      (81 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (84 (checkcast (class "clojure.lang.Var")))
                                      (87 (putstatic (fieldCP "const__6" "clojure.test$fn__7071" (class "clojure.lang.Var"))))
                                      (90 (aconst_null))
                                      (91 (ldc 9))        ;;STRING:: "pass"
                                      (93 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (96 (checkcast (class "clojure.lang.Keyword")))
                                      (99 (putstatic (fieldCP "const__7" "clojure.test$fn__7071" (class "clojure.lang.Keyword"))))
                                      (102 (aconst_null))
                                      (103 (ldc 10))      ;;STRING:: "fail"
                                      (105 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (108 (checkcast (class "clojure.lang.Keyword")))
                                      (111 (putstatic (fieldCP "const__8" "clojure.test$fn__7071" (class "clojure.lang.Keyword"))))
                                      (114 (aconst_null))
                                      (115 (ldc 11))      ;;STRING:: "error"
                                      (117 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (120 (checkcast (class "clojure.lang.Keyword")))
                                      (123 (putstatic (fieldCP "const__9" "clojure.test$fn__7071" (class "clojure.lang.Keyword"))))
                                      (126 (ldc 0))       ;;STRING:: "clojure.core"
                                      (128 (ldc 12))      ;;STRING:: "pop-thread-bindings"
                                      (130 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (133 (checkcast (class "clojure.lang.Var")))
                                      (136 (putstatic (fieldCP "const__10" "clojure.test$fn__7071" (class "clojure.lang.Var"))))
                                      (139 (new (class "clojure.lang.KeywordLookupSite")))
                                      (142 (dup))
                                      (143 (aconst_null))
                                      (144 (ldc 7))       ;;STRING:: "test"
                                      (146 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (149 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (152 (dup))
                                      (153 (putstatic (fieldCP "__site__0__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite"))))
                                      (156 (putstatic (fieldCP "__thunk__0__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk"))))
                                      (159 (new (class "clojure.lang.KeywordLookupSite")))
                                      (162 (dup))
                                      (163 (aconst_null))
                                      (164 (ldc 9))       ;;STRING:: "pass"
                                      (166 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (169 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (172 (dup))
                                      (173 (putstatic (fieldCP "__site__1__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite"))))
                                      (176 (putstatic (fieldCP "__thunk__1__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk"))))
                                      (179 (new (class "clojure.lang.KeywordLookupSite")))
                                      (182 (dup))
                                      (183 (aconst_null))
                                      (184 (ldc 10))      ;;STRING:: "fail"
                                      (186 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (189 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (192 (dup))
                                      (193 (putstatic (fieldCP "__site__2__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite"))))
                                      (196 (putstatic (fieldCP "__thunk__2__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk"))))
                                      (199 (new (class "clojure.lang.KeywordLookupSite")))
                                      (202 (dup))
                                      (203 (aconst_null))
                                      (204 (ldc 11))      ;;STRING:: "error"
                                      (206 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (209 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (212 (dup))
                                      (213 (putstatic (fieldCP "__site__3__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite"))))
                                      (216 (putstatic (fieldCP "__thunk__3__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk"))))
                                      (219 (new (class "clojure.lang.KeywordLookupSite")))
                                      (222 (dup))
                                      (223 (aconst_null))
                                      (224 (ldc 10))      ;;STRING:: "fail"
                                      (226 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (229 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (232 (dup))
                                      (233 (putstatic (fieldCP "__site__4__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite"))))
                                      (236 (putstatic (fieldCP "__thunk__4__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk"))))
                                      (239 (new (class "clojure.lang.KeywordLookupSite")))
                                      (242 (dup))
                                      (243 (aconst_null))
                                      (244 (ldc 11))      ;;STRING:: "error"
                                      (246 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (249 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (252 (dup))
                                      (253 (putstatic (fieldCP "__site__5__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite"))))
                                      (256 (putstatic (fieldCP "__thunk__5__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk"))))
                                      (259 (return))
                                      (endofcode 260))
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
                                   (max_stack . 9) (max_locals . 4) (code_length . 370)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.test$fn__7071" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.test$fn__7071" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (checkcast (class "clojure.lang.IFn"))) 
                                      (18 (getstatic (fieldCP "const__2" "clojure.test$fn__7071" (class "clojure.lang.Var")))) 
                                      (21 (getstatic (fieldCP "const__3" "clojure.test$fn__7071" (class "clojure.lang.Var")))) 
                                      (24 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (27 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (32 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (37 (pop)) 
                                      (38 (getstatic (fieldCP "const__4" "clojure.test$fn__7071" (class "clojure.lang.Var")))) ;;at TAG_13
                                      (41 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (44 (checkcast (class "clojure.lang.IFn"))) 
                                      (47 (ldc 13)) ;;STRING:: "\nRan"
                                      (49 (getstatic (fieldCP "__thunk__0__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (52 (dup)) 
                                      (53 (aload_1)) 
                                      (54 (dup_x2)) 
                                      (55 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (60 (dup_x2)) 
                                      (61 (if_acmpeq 68)) ;;to TAG_0
                                      (64 (pop)) 
                                      (65 (goto 90))  ;;to TAG_1
                                      (68 (swap)) ;;at TAG_0
                                      (69 (pop)) 
                                      (70 (dup)) 
                                      (71 (getstatic (fieldCP "__site__0__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite")))) 
                                      (74 (swap)) 
                                      (75 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (80 (dup)) 
                                      (81 (putstatic (fieldCP "__thunk__0__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (84 (swap)) 
                                      (85 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (90 (ldc 14)) ;;at TAG_1;;STRING:: "tests containing"
                                      (92 (getstatic (fieldCP "__thunk__1__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (95 (dup)) 
                                      (96 (aload_1)) 
                                      (97 (dup_x2)) 
                                      (98 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (103 (dup_x2)) 
                                      (104 (if_acmpeq 111)) ;;to TAG_2
                                      (107 (pop)) 
                                      (108 (goto 133)) ;;to TAG_3
                                      (111 (swap)) ;;at TAG_2
                                      (112 (pop)) 
                                      (113 (dup)) 
                                      (114 (getstatic (fieldCP "__site__1__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite")))) 
                                      (117 (swap)) 
                                      (118 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (123 (dup)) 
                                      (124 (putstatic (fieldCP "__thunk__1__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (127 (swap)) 
                                      (128 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (133 (getstatic (fieldCP "__thunk__2__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) ;;at TAG_3
                                      (136 (dup)) 
                                      (137 (aload_1)) 
                                      (138 (dup_x2)) 
                                      (139 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (144 (dup_x2)) 
                                      (145 (if_acmpeq 152)) ;;to TAG_4
                                      (148 (pop)) 
                                      (149 (goto 174)) ;;to TAG_5
                                      (152 (swap)) ;;at TAG_4
                                      (153 (pop)) 
                                      (154 (dup)) 
                                      (155 (getstatic (fieldCP "__site__2__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite")))) 
                                      (158 (swap)) 
                                      (159 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (164 (dup)) 
                                      (165 (putstatic (fieldCP "__thunk__2__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (168 (swap)) 
                                      (169 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (174 (invokestatic (methodCP "add" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Number")))) ;;at TAG_5
                                      (177 (getstatic (fieldCP "__thunk__3__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (180 (dup)) 
                                      (181 (aload_1)) 
                                      (182 (dup_x2)) 
                                      (183 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (188 (dup_x2)) 
                                      (189 (if_acmpeq 196)) ;;to TAG_6
                                      (192 (pop)) 
                                      (193 (goto 218)) ;;to TAG_7
                                      (196 (swap)) ;;at TAG_6
                                      (197 (pop)) 
                                      (198 (dup)) 
                                      (199 (getstatic (fieldCP "__site__3__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite")))) 
                                      (202 (swap)) 
                                      (203 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (208 (dup)) 
                                      (209 (putstatic (fieldCP "__thunk__3__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (212 (swap)) 
                                      (213 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (218 (invokestatic (methodCP "add" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Number")))) ;;at TAG_7
                                      (221 (ldc 15)) ;;STRING:: "assertions."
                                      (223 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 6)) 
                                      (228 (pop)) 
                                      (229 (getstatic (fieldCP "const__4" "clojure.test$fn__7071" (class "clojure.lang.Var")))) 
                                      (232 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (235 (checkcast (class "clojure.lang.IFn"))) 
                                      (238 (getstatic (fieldCP "__thunk__4__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (241 (dup)) 
                                      (242 (aload_1)) 
                                      (243 (dup_x2)) 
                                      (244 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (249 (dup_x2)) 
                                      (250 (if_acmpeq 257)) ;;to TAG_8
                                      (253 (pop)) 
                                      (254 (goto 279)) ;;to TAG_9
                                      (257 (swap)) ;;at TAG_8
                                      (258 (pop)) 
                                      (259 (dup)) 
                                      (260 (getstatic (fieldCP "__site__4__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite")))) 
                                      (263 (swap)) 
                                      (264 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (269 (dup)) 
                                      (270 (putstatic (fieldCP "__thunk__4__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (273 (swap)) 
                                      (274 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (279 (ldc 16)) ;;at TAG_9;;STRING:: "failures,"
                                      (281 (getstatic (fieldCP "__thunk__5__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (284 (dup)) 
                                      (285 (aload_1)) 
                                      (286 (aconst_null)) 
                                      (287 (astore_1)) 
                                      (288 (dup_x2)) 
                                      (289 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (294 (dup_x2)) 
                                      (295 (if_acmpeq 302)) ;;to TAG_10
                                      (298 (pop)) 
                                      (299 (goto 324)) ;;to TAG_11
                                      (302 (swap)) ;;at TAG_10
                                      (303 (pop)) 
                                      (304 (dup)) 
                                      (305 (getstatic (fieldCP "__site__5__" "clojure.test$fn__7071" (class "clojure.lang.KeywordLookupSite")))) 
                                      (308 (swap)) 
                                      (309 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (314 (dup)) 
                                      (315 (putstatic (fieldCP "__thunk__5__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (318 (swap)) 
                                      (319 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (324 (ldc 17)) ;;at TAG_11;;STRING:: "errors."
                                      (326 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (331 (astore_2)) 
                                      (332 (getstatic (fieldCP "const__10" "clojure.test$fn__7071" (class "clojure.lang.Var")))) ;;at TAG_14
                                      (335 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (338 (checkcast (class "clojure.lang.IFn"))) 
                                      (341 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (346 (pop)) 
                                      (347 (goto 368)) ;;to TAG_12
                                      (350 (astore_3)) ;;at TAG_15
                                      (351 (getstatic (fieldCP "const__10" "clojure.test$fn__7071" (class "clojure.lang.Var")))) 
                                      (354 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (357 (checkcast (class "clojure.lang.IFn"))) 
                                      (360 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (365 (pop)) 
                                      (366 (aload_3)) 
                                      (367 (athrow)) 
                                      (368 (aload_2)) ;;at TAG_12
                                      (369 (areturn)) 
                                      (endofcode 370))
                                   (Exceptions 
                                     (handler 38 332  350 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 83)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 82 (0 . 5) (40 47 54 61 68 75))))  ;;to TAG_2;;to TAG_3;;to TAG_4;;to TAG_5;;to TAG_6;;to TAG_0;;to TAG_1
                                      (40 (aload_2)) ;;at TAG_1
                                      (41 (putstatic (fieldCP "__thunk__0__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (44 (goto 82)) ;;to TAG_0
                                      (47 (aload_2)) ;;at TAG_2
                                      (48 (putstatic (fieldCP "__thunk__1__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (51 (goto 82)) ;;to TAG_0
                                      (54 (aload_2)) ;;at TAG_3
                                      (55 (putstatic (fieldCP "__thunk__2__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (58 (goto 82)) ;;to TAG_0
                                      (61 (aload_2)) ;;at TAG_4
                                      (62 (putstatic (fieldCP "__thunk__3__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (65 (goto 82)) ;;to TAG_0
                                      (68 (aload_2)) ;;at TAG_5
                                      (69 (putstatic (fieldCP "__thunk__4__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (72 (goto 82)) ;;to TAG_0
                                      (75 (aload_2)) ;;at TAG_6
                                      (76 (putstatic (fieldCP "__thunk__5__" "clojure.test$fn__7071" (class "clojure.lang.ILookupThunk")))) 
                                      (79 (goto 82)) ;;to TAG_0
                                      (82 (return)) ;;at TAG_0
                                      (endofcode 83))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *test$fn__7071-class-table*
  (make-static-class-decls 
   *clojure.test$fn__7071*))

(defconst *package-name-map* 
  ("clojure.test$fn__7071" . "clojure"))

