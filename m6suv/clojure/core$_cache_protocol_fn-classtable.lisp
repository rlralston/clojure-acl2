; core$_cache_protocol_fn-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:46 CDT 2014.
;

(defconst *clojure.core$_cache_protocol_fn*
 (make-class-def
      '(class "clojure.core$_cache_protocol_fn"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "find-protocol-method")
                        (STRING  "str")
                        (STRING  "var")
                        (STRING  "nil?")
                        (STRING  "class")
                        (STRING  "expand-method-impl-cache")
                        (STRING  "No implementation of method: ")
                        (STRING  " of protocol: ")
                        (STRING  " found for class: ")
                        (STRING  "nil"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 98)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "find-protocol-method"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$_cache_protocol_fn" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "str"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$_cache_protocol_fn" (class "clojure.lang.Var"))))
                                      (26 (aconst_null))
                                      (27 (ldc 3))        ;;STRING:: "var"
                                      (29 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (32 (checkcast (class "clojure.lang.Keyword")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.core$_cache_protocol_fn" (class "clojure.lang.Keyword"))))
                                      (38 (ldc 0))        ;;STRING:: "clojure.core"
                                      (40 (ldc 4))        ;;STRING:: "nil?"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.core$_cache_protocol_fn" (class "clojure.lang.Var"))))
                                      (51 (ldc 0))        ;;STRING:: "clojure.core"
                                      (53 (ldc 5))        ;;STRING:: "class"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.core$_cache_protocol_fn" (class "clojure.lang.Var"))))
                                      (64 (ldc 0))        ;;STRING:: "clojure.core"
                                      (66 (ldc 6))        ;;STRING:: "expand-method-impl-cache"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.core$_cache_protocol_fn" (class "clojure.lang.Var"))))
                                      (77 (new (class "clojure.lang.KeywordLookupSite")))
                                      (80 (dup))
                                      (81 (aconst_null))
                                      (82 (ldc 3))        ;;STRING:: "var"
                                      (84 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (87 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (90 (dup))
                                      (91 (putstatic (fieldCP "__site__0__" "clojure.core$_cache_protocol_fn" (class "clojure.lang.KeywordLookupSite"))))
                                      (94 (putstatic (fieldCP "__thunk__0__" "clojure.core$_cache_protocol_fn" (class "clojure.lang.ILookupThunk"))))
                                      (97 (return))
                                      (endofcode 98))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 10) (max_locals . 7) (code_length . 265)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "clojure.lang.AFunction"))) 
                                      (4 (getfield (fieldCP "__methodImplCache" "clojure.lang.AFunction" (class "clojure.lang.MethodImplCache")))) 
                                      (7 (astore 5)) 
                                      (9 (aload_3)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_3)) 
                                      (12 (checkcast (class "java.lang.Class"))) 
                                      (15 (aload_2)) 
                                      (16 (invokevirtual (methodCP "isInstance" "java.lang.Class" ((class "java.lang.Object")) boolean))) 
                                      (19 (ifeq 31)) ;;to TAG_0
                                      (22 (aload 4)) 
                                      (24 (aconst_null)) 
                                      (25 (astore 4)) 
                                      (27 (goto 62))  ;;to TAG_1
                                      (30 (pop)) 
                                      (31 (getstatic (fieldCP "const__0" "clojure.core$_cache_protocol_fn" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (34 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (37 (checkcast (class "clojure.lang.IFn"))) 
                                      (40 (aload 5)) 
                                      (42 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (45 (getfield (fieldCP "protocol" "clojure.lang.MethodImplCache" (class "clojure.lang.IPersistentMap")))) 
                                      (48 (aload 5)) 
                                      (50 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (53 (getfield (fieldCP "methodk" "clojure.lang.MethodImplCache" (class "clojure.lang.Keyword")))) 
                                      (56 (aload_2)) 
                                      (57 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (62 (astore 6)) ;;at TAG_1
                                      (64 (aload 6)) 
                                      (66 (dup)) 
                                      (67 (ifnull 81)) ;;to TAG_2
                                      (70 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (73 (if_acmpeq 82)) ;;to TAG_3
                                      (76 (aconst_null)) 
                                      (77 (pop)) 
                                      (78 (goto 207)) ;;to TAG_4
                                      (81 (pop)) ;;at TAG_2
                                      (82 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_3
                                      (85 (dup)) 
                                      (86 (getstatic (fieldCP "const__1" "clojure.core$_cache_protocol_fn" (class "clojure.lang.Var")))) 
                                      (89 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (92 (checkcast (class "clojure.lang.IFn"))) 
                                      (95 (ldc 7)) ;;STRING:: "No implementation of method: "
                                      (97 (aload 5)) 
                                      (99 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (102 (getfield (fieldCP "methodk" "clojure.lang.MethodImplCache" (class "clojure.lang.Keyword")))) 
                                      (105 (ldc 8)) ;;STRING:: " of protocol: "
                                      (107 (getstatic (fieldCP "__thunk__0__" "clojure.core$_cache_protocol_fn" (class "clojure.lang.ILookupThunk")))) 
                                      (110 (dup)) 
                                      (111 (aload 5)) 
                                      (113 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (116 (getfield (fieldCP "protocol" "clojure.lang.MethodImplCache" (class "clojure.lang.IPersistentMap")))) 
                                      (119 (dup_x2)) 
                                      (120 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (125 (dup_x2)) 
                                      (126 (if_acmpeq 133)) ;;to TAG_5
                                      (129 (pop)) 
                                      (130 (goto 155)) ;;to TAG_6
                                      (133 (swap)) ;;at TAG_5
                                      (134 (pop)) 
                                      (135 (dup)) 
                                      (136 (getstatic (fieldCP "__site__0__" "clojure.core$_cache_protocol_fn" (class "clojure.lang.KeywordLookupSite")))) 
                                      (139 (swap)) 
                                      (140 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (145 (dup)) 
                                      (146 (putstatic (fieldCP "__thunk__0__" "clojure.core$_cache_protocol_fn" (class "clojure.lang.ILookupThunk")))) 
                                      (149 (swap)) 
                                      (150 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (155 (ldc 9)) ;;at TAG_6;;STRING:: " found for class: "
                                      (157 (aload_2)) 
                                      (158 (aconst_null)) 
                                      (159 (invokestatic (methodCP "identical" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (162 (ifeq 171)) ;;to TAG_7
                                      (165 (ldc 10)) ;;STRING:: "nil"
                                      (167 (goto 192)) ;;to TAG_8
                                      (170 (pop)) 
                                      (171 (getstatic (fieldCP "const__4" "clojure.core$_cache_protocol_fn" (class "clojure.lang.Var")))) ;;at TAG_7
                                      (174 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (177 (checkcast (class "clojure.lang.IFn"))) 
                                      (180 (aload_2)) 
                                      (181 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (186 (checkcast (class "java.lang.Class"))) 
                                      (189 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (192 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 7)) ;;at TAG_8
                                      (197 (checkcast (class "java.lang.String"))) 
                                      (200 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (203 (checkcast (class "java.lang.Throwable"))) 
                                      (206 (athrow)) 
                                      (207 (aload_1)) ;;at TAG_4
                                      (208 (aconst_null)) 
                                      (209 (astore_1)) 
                                      (210 (checkcast (class "clojure.lang.AFunction"))) 
                                      (213 (getstatic (fieldCP "const__5" "clojure.core$_cache_protocol_fn" (class "clojure.lang.Var")))) 
                                      (216 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (219 (checkcast (class "clojure.lang.IFn"))) 
                                      (222 (aload 5)) 
                                      (224 (aconst_null)) 
                                      (225 (astore 5)) 
                                      (227 (getstatic (fieldCP "const__4" "clojure.core$_cache_protocol_fn" (class "clojure.lang.Var")))) 
                                      (230 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (233 (checkcast (class "clojure.lang.IFn"))) 
                                      (236 (aload_2)) 
                                      (237 (aconst_null)) 
                                      (238 (astore_2)) 
                                      (239 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (244 (aload 6)) 
                                      (246 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (251 (dup_x1)) 
                                      (252 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (255 (putfield (fieldCP "__methodImplCache" "clojure.lang.AFunction" (class "clojure.lang.MethodImplCache")))) 
                                      (258 (pop)) 
                                      (259 (aload 6)) 
                                      (261 (aconst_null)) 
                                      (262 (astore 6)) 
                                      (264 (areturn)) 
                                      (endofcode 265))
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.core$_cache_protocol_fn" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *core$_cache_protocol_fn-class-table*
  (make-static-class-decls 
   *clojure.core$_cache_protocol_fn*))

(defconst *package-name-map* 
  ("clojure.core$_cache_protocol_fn" . "clojure"))

