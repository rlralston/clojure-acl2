; reflect$parse_flags$fn__9040-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.reflect$parse_flags$fn__9040*
 (make-class-def
      '(class "clojure.reflect$parse_flags$fn__9040"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "get")
                        (STRING  "contexts")
                        (STRING  "not")
                        (STRING  "zero?")
                        (STRING  "bit-and")
                        (STRING  "flag")
                        (STRING  "conj")
                        (STRING  "name"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__1__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__1__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__2__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__2__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "context" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "flags" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 162)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "get"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "contexts"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.Keyword"))))
                                      (25 (ldc 0))        ;;STRING:: "clojure.core"
                                      (27 (ldc 3))        ;;STRING:: "not"
                                      (29 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (32 (checkcast (class "clojure.lang.Var")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.Var"))))
                                      (38 (ldc 0))        ;;STRING:: "clojure.core"
                                      (40 (ldc 4))        ;;STRING:: "zero?"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.Var"))))
                                      (51 (ldc 0))        ;;STRING:: "clojure.core"
                                      (53 (ldc 5))        ;;STRING:: "bit-and"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.Var"))))
                                      (64 (aconst_null))
                                      (65 (ldc 6))        ;;STRING:: "flag"
                                      (67 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (70 (checkcast (class "clojure.lang.Keyword")))
                                      (73 (putstatic (fieldCP "const__5" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.Keyword"))))
                                      (76 (ldc 0))        ;;STRING:: "clojure.core"
                                      (78 (ldc 7))        ;;STRING:: "conj"
                                      (80 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (83 (checkcast (class "clojure.lang.Var")))
                                      (86 (putstatic (fieldCP "const__6" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.Var"))))
                                      (89 (aconst_null))
                                      (90 (ldc 8))        ;;STRING:: "name"
                                      (92 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (95 (checkcast (class "clojure.lang.Keyword")))
                                      (98 (putstatic (fieldCP "const__7" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.Keyword"))))
                                      (101 (new (class "clojure.lang.KeywordLookupSite")))
                                      (104 (dup))
                                      (105 (aconst_null))
                                      (106 (ldc 2))       ;;STRING:: "contexts"
                                      (108 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (111 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (114 (dup))
                                      (115 (putstatic (fieldCP "__site__0__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.KeywordLookupSite"))))
                                      (118 (putstatic (fieldCP "__thunk__0__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk"))))
                                      (121 (new (class "clojure.lang.KeywordLookupSite")))
                                      (124 (dup))
                                      (125 (aconst_null))
                                      (126 (ldc 6))       ;;STRING:: "flag"
                                      (128 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (131 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (134 (dup))
                                      (135 (putstatic (fieldCP "__site__1__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.KeywordLookupSite"))))
                                      (138 (putstatic (fieldCP "__thunk__1__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk"))))
                                      (141 (new (class "clojure.lang.KeywordLookupSite")))
                                      (144 (dup))
                                      (145 (aconst_null))
                                      (146 (ldc 8))       ;;STRING:: "name"
                                      (148 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (151 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (154 (dup))
                                      (155 (putstatic (fieldCP "__site__2__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.KeywordLookupSite"))))
                                      (158 (putstatic (fieldCP "__thunk__2__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk"))))
                                      (161 (return))
                                      (endofcode 162))
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
                                      (6 (putfield (fieldCP "context" "clojure.reflect$parse_flags$fn__9040" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "flags" "clojure.reflect$parse_flags$fn__9040" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 222)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk")))) 
                                      (3 (dup)) 
                                      (4 (aload_2)) 
                                      (5 (dup_x2)) 
                                      (6 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (11 (dup_x2)) 
                                      (12 (if_acmpeq 19)) ;;to TAG_0
                                      (15 (pop)) 
                                      (16 (goto 41))  ;;to TAG_1
                                      (19 (swap)) ;;at TAG_0
                                      (20 (pop)) 
                                      (21 (dup)) 
                                      (22 (getstatic (fieldCP "__site__0__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.KeywordLookupSite")))) 
                                      (25 (swap)) 
                                      (26 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (31 (dup)) 
                                      (32 (putstatic (fieldCP "__thunk__0__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk")))) 
                                      (35 (swap)) 
                                      (36 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (41 (aload_0)) ;;at TAG_1
                                      (42 (getfield (fieldCP "context" "clojure.reflect$parse_flags$fn__9040" (class "java.lang.Object")))) 
                                      (45 (invokestatic (methodCP "get" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (48 (astore_3)) 
                                      (49 (aload_3)) 
                                      (50 (dup)) 
                                      (51 (ifnull 140)) ;;to TAG_2
                                      (54 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (57 (if_acmpeq 141)) ;;to TAG_3
                                      (60 (getstatic (fieldCP "const__2" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.Var")))) 
                                      (63 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (66 (checkcast (class "clojure.lang.IFn"))) 
                                      (69 (aload_0)) 
                                      (70 (getfield (fieldCP "flags" "clojure.reflect$parse_flags$fn__9040" (class "java.lang.Object")))) 
                                      (73 (getstatic (fieldCP "__thunk__1__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk")))) 
                                      (76 (dup)) 
                                      (77 (aload_2)) 
                                      (78 (dup_x2)) 
                                      (79 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (84 (dup_x2)) 
                                      (85 (if_acmpeq 92)) ;;to TAG_4
                                      (88 (pop)) 
                                      (89 (goto 114)) ;;to TAG_5
                                      (92 (swap)) ;;at TAG_4
                                      (93 (pop)) 
                                      (94 (dup)) 
                                      (95 (getstatic (fieldCP "__site__1__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.KeywordLookupSite")))) 
                                      (98 (swap)) 
                                      (99 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (104 (dup)) 
                                      (105 (putstatic (fieldCP "__thunk__1__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk")))) 
                                      (108 (swap)) 
                                      (109 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (114 (invokestatic (methodCP "and" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) long))) ;;at TAG_5
                                      (117 (invokestatic (methodCP "isZero" "clojure.lang.Numbers" (long) boolean))) 
                                      (120 (ifeq 129)) ;;to TAG_6
                                      (123 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (126 (goto 132)) ;;to TAG_7
                                      (129 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_6
                                      (132 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_7
                                      (137 (goto 144)) ;;to TAG_8
                                      (140 (pop)) ;;at TAG_2
                                      (141 (aload_3)) ;;at TAG_3
                                      (142 (aconst_null)) 
                                      (143 (astore_3)) 
                                      (144 (dup)) ;;at TAG_8
                                      (145 (ifnull 217)) ;;to TAG_9
                                      (148 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (151 (if_acmpeq 218)) ;;to TAG_10
                                      (154 (getstatic (fieldCP "const__6" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.Var")))) 
                                      (157 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (160 (checkcast (class "clojure.lang.IFn"))) 
                                      (163 (aload_1)) 
                                      (164 (aconst_null)) 
                                      (165 (astore_1)) 
                                      (166 (getstatic (fieldCP "__thunk__2__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk")))) 
                                      (169 (dup)) 
                                      (170 (aload_2)) 
                                      (171 (aconst_null)) 
                                      (172 (astore_2)) 
                                      (173 (dup_x2)) 
                                      (174 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (179 (dup_x2)) 
                                      (180 (if_acmpeq 187)) ;;to TAG_11
                                      (183 (pop)) 
                                      (184 (goto 209)) ;;to TAG_12
                                      (187 (swap)) ;;at TAG_11
                                      (188 (pop)) 
                                      (189 (dup)) 
                                      (190 (getstatic (fieldCP "__site__2__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.KeywordLookupSite")))) 
                                      (193 (swap)) 
                                      (194 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (199 (dup)) 
                                      (200 (putstatic (fieldCP "__thunk__2__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk")))) 
                                      (203 (swap)) 
                                      (204 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (209 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) ;;at TAG_12
                                      (214 (goto 221)) ;;to TAG_13
                                      (217 (pop)) ;;at TAG_9
                                      (218 (aload_1)) ;;at TAG_10
                                      (219 (aconst_null)) 
                                      (220 (astore_1)) 
                                      (221 (areturn)) ;;at TAG_13
                                      (endofcode 222))
                                   (Exceptions )
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 50)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 49 (0 . 2) (28 35 42))))  ;;to TAG_2;;to TAG_3;;to TAG_0;;to TAG_1
                                      (28 (aload_2)) ;;at TAG_1
                                      (29 (putstatic (fieldCP "__thunk__0__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk")))) 
                                      (32 (goto 49)) ;;to TAG_0
                                      (35 (aload_2)) ;;at TAG_2
                                      (36 (putstatic (fieldCP "__thunk__1__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk")))) 
                                      (39 (goto 49)) ;;to TAG_0
                                      (42 (aload_2)) ;;at TAG_3
                                      (43 (putstatic (fieldCP "__thunk__2__" "clojure.reflect$parse_flags$fn__9040" (class "clojure.lang.ILookupThunk")))) 
                                      (46 (goto 49)) ;;to TAG_0
                                      (49 (return)) ;;at TAG_0
                                      (endofcode 50))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *reflect$parse_flags$fn__9040-class-table*
  (make-static-class-decls 
   *clojure.reflect$parse_flags$fn__9040*))

(defconst *package-name-map* 
  ("clojure.reflect$parse_flags$fn__9040" . "clojure"))

