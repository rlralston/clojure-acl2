; pprint$nl$fn__7544-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$nl$fn__7544*
 (make-class-def
      '(class "clojure.pprint$nl$fn__7544"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "alter")
                        (STRING  "deref")
                        (STRING  "assoc")
                        (STRING  "mode")
                        (STRING  "buffering")
                        (STRING  "pos")
                        (STRING  "clojure.pprint")
                        (STRING  "add-to-buffer")
                        (STRING  "make-nl-t")
                        (STRING  "logical-blocks"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__1__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__1__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "type" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "this" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 154)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "alter"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "deref"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "assoc"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var"))))
                                      (39 (aconst_null))
                                      (40 (ldc 4))        ;;STRING:: "mode"
                                      (42 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (45 (checkcast (class "clojure.lang.Keyword")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Keyword"))))
                                      (51 (aconst_null))
                                      (52 (ldc 5))        ;;STRING:: "buffering"
                                      (54 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (57 (checkcast (class "clojure.lang.Keyword")))
                                      (60 (putstatic (fieldCP "const__4" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Keyword"))))
                                      (63 (aconst_null))
                                      (64 (ldc 6))        ;;STRING:: "pos"
                                      (66 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (69 (checkcast (class "clojure.lang.Keyword")))
                                      (72 (putstatic (fieldCP "const__5" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Keyword"))))
                                      (75 (ldc 7))        ;;STRING:: "clojure.pprint"
                                      (77 (ldc 8))        ;;STRING:: "add-to-buffer"
                                      (79 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (82 (checkcast (class "clojure.lang.Var")))
                                      (85 (putstatic (fieldCP "const__6" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var"))))
                                      (88 (ldc 7))        ;;STRING:: "clojure.pprint"
                                      (90 (ldc 9))        ;;STRING:: "make-nl-t"
                                      (92 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (95 (checkcast (class "clojure.lang.Var")))
                                      (98 (putstatic (fieldCP "const__7" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var"))))
                                      (101 (aconst_null))
                                      (102 (ldc 10))      ;;STRING:: "logical-blocks"
                                      (104 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (107 (checkcast (class "clojure.lang.Keyword")))
                                      (110 (putstatic (fieldCP "const__8" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Keyword"))))
                                      (113 (new (class "clojure.lang.KeywordLookupSite")))
                                      (116 (dup))
                                      (117 (aconst_null))
                                      (118 (ldc 6))       ;;STRING:: "pos"
                                      (120 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (123 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (126 (dup))
                                      (127 (putstatic (fieldCP "__site__0__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.KeywordLookupSite"))))
                                      (130 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.ILookupThunk"))))
                                      (133 (new (class "clojure.lang.KeywordLookupSite")))
                                      (136 (dup))
                                      (137 (aconst_null))
                                      (138 (ldc 10))      ;;STRING:: "logical-blocks"
                                      (140 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (143 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (146 (dup))
                                      (147 (putstatic (fieldCP "__site__1__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.KeywordLookupSite"))))
                                      (150 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.ILookupThunk"))))
                                      (153 (return))
                                      (endofcode 154))
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
                                      (6 (putfield (fieldCP "type" "clojure.pprint$nl$fn__7544" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "this" "clojure.pprint$nl$fn__7544" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 2) (code_length . 231)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (checkcast (class "clojure.lang.IFn"))) 
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "this" "clojure.pprint$nl$fn__7544" (class "java.lang.Object")))) 
                                      (22 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (27 (getstatic (fieldCP "const__2" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var")))) 
                                      (30 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (33 (getstatic (fieldCP "const__3" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Keyword")))) 
                                      (36 (getstatic (fieldCP "const__4" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Keyword")))) 
                                      (39 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (44 (pop)) 
                                      (45 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.ILookupThunk")))) 
                                      (48 (dup)) 
                                      (49 (getstatic (fieldCP "const__1" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var")))) 
                                      (52 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (55 (checkcast (class "clojure.lang.IFn"))) 
                                      (58 (getstatic (fieldCP "const__1" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var")))) 
                                      (61 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (64 (checkcast (class "clojure.lang.IFn"))) 
                                      (67 (aload_0)) 
                                      (68 (getfield (fieldCP "this" "clojure.pprint$nl$fn__7544" (class "java.lang.Object")))) 
                                      (71 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (76 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (81 (dup_x2)) 
                                      (82 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (87 (dup_x2)) 
                                      (88 (if_acmpeq 95)) ;;to TAG_0
                                      (91 (pop)) 
                                      (92 (goto 117)) ;;to TAG_1
                                      (95 (swap)) ;;at TAG_0
                                      (96 (pop)) 
                                      (97 (dup)) 
                                      (98 (getstatic (fieldCP "__site__0__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.KeywordLookupSite")))) 
                                      (101 (swap)) 
                                      (102 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (107 (dup)) 
                                      (108 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.ILookupThunk")))) 
                                      (111 (swap)) 
                                      (112 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (117 (astore_1)) ;;at TAG_1
                                      (118 (getstatic (fieldCP "const__6" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var")))) 
                                      (121 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (124 (checkcast (class "clojure.lang.IFn"))) 
                                      (127 (aload_0)) 
                                      (128 (getfield (fieldCP "this" "clojure.pprint$nl$fn__7544" (class "java.lang.Object")))) 
                                      (131 (getstatic (fieldCP "const__7" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var")))) 
                                      (134 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (137 (checkcast (class "clojure.lang.IFn"))) 
                                      (140 (aload_0)) 
                                      (141 (getfield (fieldCP "type" "clojure.pprint$nl$fn__7544" (class "java.lang.Object")))) 
                                      (144 (getstatic (fieldCP "__thunk__1__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.ILookupThunk")))) 
                                      (147 (dup)) 
                                      (148 (getstatic (fieldCP "const__1" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var")))) 
                                      (151 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (154 (checkcast (class "clojure.lang.IFn"))) 
                                      (157 (getstatic (fieldCP "const__1" "clojure.pprint$nl$fn__7544" (class "clojure.lang.Var")))) 
                                      (160 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (163 (checkcast (class "clojure.lang.IFn"))) 
                                      (166 (aload_0)) 
                                      (167 (getfield (fieldCP "this" "clojure.pprint$nl$fn__7544" (class "java.lang.Object")))) 
                                      (170 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (175 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (180 (dup_x2)) 
                                      (181 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (186 (dup_x2)) 
                                      (187 (if_acmpeq 194))  ;;to TAG_2
                                      (190 (pop)) 
                                      (191 (goto 216)) ;;to TAG_3
                                      (194 (swap)) ;;at TAG_2
                                      (195 (pop)) 
                                      (196 (dup)) 
                                      (197 (getstatic (fieldCP "__site__1__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.KeywordLookupSite")))) 
                                      (200 (swap)) 
                                      (201 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (206 (dup)) 
                                      (207 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.ILookupThunk")))) 
                                      (210 (swap)) 
                                      (211 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (216 (aload_1)) ;;at TAG_3
                                      (217 (aload_1)) 
                                      (218 (aconst_null)) 
                                      (219 (astore_1)) 
                                      (220 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (225 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (230 (areturn)) 
                                      (endofcode 231))
                                   (Exceptions )
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 39)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 38 (0 . 1) (24 31))))  ;;to TAG_2;;to TAG_0;;to TAG_1
                                      (24 (aload_2)) ;;at TAG_1
                                      (25 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.ILookupThunk")))) 
                                      (28 (goto 38)) ;;to TAG_0
                                      (31 (aload_2)) ;;at TAG_2
                                      (32 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$nl$fn__7544" (class "clojure.lang.ILookupThunk")))) 
                                      (35 (goto 38)) ;;to TAG_0
                                      (38 (return)) ;;at TAG_0
                                      (endofcode 39))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$nl$fn__7544-class-table*
  (make-static-class-decls 
   *clojure.pprint$nl$fn__7544*))

(defconst *package-name-map* 
  ("clojure.pprint$nl$fn__7544" . "clojure"))
