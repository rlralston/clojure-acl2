; pprint$write_buffered_output-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$write_buffered_output*
 (make-class-def
      '(class "clojure.pprint$write_buffered_output"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.pprint")
                        (STRING  "write-line")
                        (STRING  "buffer")
                        (STRING  "clojure.core")
                        (STRING  "deref")
                        (STRING  "write-tokens")
                        (STRING  "alter")
                        (STRING  "assoc"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                      (0 (ldc 0))         ;;STRING:: "clojure.pprint"
                                      (2 (ldc 1))         ;;STRING:: "write-line"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "buffer"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$write_buffered_output" (class "clojure.lang.Keyword"))))
                                      (25 (ldc 3))        ;;STRING:: "clojure.core"
                                      (27 (ldc 4))        ;;STRING:: "deref"
                                      (29 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (32 (checkcast (class "clojure.lang.Var")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var"))))
                                      (38 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (40 (ldc 5))        ;;STRING:: "write-tokens"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var"))))
                                      (51 (ldc 3))        ;;STRING:: "clojure.core"
                                      (53 (ldc 6))        ;;STRING:: "alter"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var"))))
                                      (64 (ldc 3))        ;;STRING:: "clojure.core"
                                      (66 (ldc 7))        ;;STRING:: "assoc"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var"))))
                                      (77 (new (class "clojure.lang.KeywordLookupSite")))
                                      (80 (dup))
                                      (81 (aconst_null))
                                      (82 (ldc 2))        ;;STRING:: "buffer"
                                      (84 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (87 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (90 (dup))
                                      (91 (putstatic (fieldCP "__site__0__" "clojure.pprint$write_buffered_output" (class "clojure.lang.KeywordLookupSite"))))
                                      (94 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$write_buffered_output" (class "clojure.lang.ILookupThunk"))))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 172)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (pop)) 
                                      (16 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$write_buffered_output" (class "clojure.lang.ILookupThunk")))) 
                                      (19 (dup)) 
                                      (20 (getstatic (fieldCP "const__2" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var")))) 
                                      (23 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (26 (checkcast (class "clojure.lang.IFn"))) 
                                      (29 (getstatic (fieldCP "const__2" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var")))) 
                                      (32 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (35 (checkcast (class "clojure.lang.IFn"))) 
                                      (38 (aload_1)) 
                                      (39 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (44 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (49 (dup_x2)) 
                                      (50 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (55 (dup_x2)) 
                                      (56 (if_acmpeq 63)) ;;to TAG_0
                                      (59 (pop)) 
                                      (60 (goto 85)) ;;to TAG_1
                                      (63 (swap)) ;;at TAG_0
                                      (64 (pop)) 
                                      (65 (dup)) 
                                      (66 (getstatic (fieldCP "__site__0__" "clojure.pprint$write_buffered_output" (class "clojure.lang.KeywordLookupSite")))) 
                                      (69 (swap)) 
                                      (70 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (75 (dup)) 
                                      (76 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$write_buffered_output" (class "clojure.lang.ILookupThunk")))) 
                                      (79 (swap)) 
                                      (80 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (85 (astore_2)) ;;at TAG_1
                                      (86 (aload_2)) 
                                      (87 (dup)) 
                                      (88 (ifnull 169))  ;;to TAG_2
                                      (91 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (94 (if_acmpeq 170)) ;;to TAG_3
                                      (97 (aload_2)) 
                                      (98 (aconst_null)) 
                                      (99 (astore_2)) 
                                      (100 (astore_3)) 
                                      (101 (getstatic (fieldCP "const__3" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var")))) 
                                      (104 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (107 (checkcast (class "clojure.lang.IFn"))) 
                                      (110 (aload_1)) 
                                      (111 (aload_3)) 
                                      (112 (aconst_null)) 
                                      (113 (astore_3)) 
                                      (114 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (117 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (122 (pop)) 
                                      (123 (getstatic (fieldCP "const__4" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var")))) 
                                      (126 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (129 (checkcast (class "clojure.lang.IFn"))) 
                                      (132 (getstatic (fieldCP "const__2" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var")))) 
                                      (135 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (138 (checkcast (class "clojure.lang.IFn"))) 
                                      (141 (aload_1)) 
                                      (142 (aconst_null)) 
                                      (143 (astore_1)) 
                                      (144 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (149 (getstatic (fieldCP "const__5" "clojure.pprint$write_buffered_output" (class "clojure.lang.Var")))) 
                                      (152 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (155 (getstatic (fieldCP "const__1" "clojure.pprint$write_buffered_output" (class "clojure.lang.Keyword")))) 
                                      (158 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector")))) 
                                      (161 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (166 (goto 171)) ;;to TAG_4
                                      (169 (pop)) ;;at TAG_2
                                      (170 (aconst_null)) ;;at TAG_3
                                      (171 (areturn)) ;;at TAG_4
                                      (endofcode 172))
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$write_buffered_output" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$write_buffered_output-class-table*
  (make-static-class-decls 
   *clojure.pprint$write_buffered_output*))

(defconst *package-name-map* 
  ("clojure.pprint$write_buffered_output" . "clojure"))
