; LispReader$MetaReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.LispReader$MetaReader*
 (make-class-def
      '(class "clojure.lang.LispReader$MetaReader"
            "clojure.lang.AFn"
            (constant_pool
                        (STRING  "Metadata must be Symbol,Keyword,String or Map")
                        (STRING  "Metadata can only be applied to IMetas"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 11) (code_length . 316)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.io.PushbackReader"))) 
                                      (4 (astore_3)) 
                                      (5 (iconst_m1)) 
                                      (6 (istore 4)) 
                                      (8 (iconst_m1)) 
                                      (9 (istore 5)) 
                                      (11 (aload_3)) 
                                      (12 (instanceof (class "clojure.lang.LineNumberingPushbackReader"))) 
                                      (15 (ifeq 38)) ;;to TAG_0
                                      (18 (aload_3)) 
                                      (19 (checkcast (class "clojure.lang.LineNumberingPushbackReader"))) 
                                      (22 (invokevirtual (methodCP "getLineNumber" "clojure.lang.LineNumberingPushbackReader" () int))) 
                                      (25 (istore 4)) 
                                      (27 (aload_3)) 
                                      (28 (checkcast (class "clojure.lang.LineNumberingPushbackReader"))) 
                                      (31 (invokevirtual (methodCP "getColumnNumber" "clojure.lang.LineNumberingPushbackReader" () int))) 
                                      (34 (iconst_1)) 
                                      (35 (isub)) 
                                      (36 (istore 5)) 
                                      (38 (aload_3)) ;;at TAG_0
                                      (39 (iconst_1)) 
                                      (40 (aconst_null)) 
                                      (41 (iconst_1)) 
                                      (42 (invokestatic (methodCP "read" "clojure.lang.LispReader" ((class "java.io.PushbackReader") boolean (class "java.lang.Object") boolean) (class "java.lang.Object")))) 
                                      (45 (astore 6)) 
                                      (47 (aload 6)) 
                                      (49 (instanceof (class "clojure.lang.Symbol"))) 
                                      (52 (ifne 63))  ;;to TAG_1
                                      (55 (aload 6)) 
                                      (57 (instanceof (class "java.lang.String"))) 
                                      (60 (ifeq 86)) ;;to TAG_2
                                      (63 (iconst_2)) ;;at TAG_1
                                      (64 (anewarray (class "java.lang.Object"))) 
                                      (67 (dup)) 
                                      (68 (iconst_0)) 
                                      (69 (getstatic (fieldCP "TAG_KEY" "clojure.lang.RT" (class "clojure.lang.Keyword")))) 
                                      (72 (aastore)) 
                                      (73 (dup)) 
                                      (74 (iconst_1)) 
                                      (75 (aload 6)) 
                                      (77 (aastore)) 
                                      (78 (invokestatic (methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (81 (astore 6)) 
                                      (83 (goto 135)) ;;to TAG_3
                                      (86 (aload 6)) ;;at TAG_2
                                      (88 (instanceof (class "clojure.lang.Keyword"))) 
                                      (91 (ifeq 117)) ;;to TAG_4
                                      (94 (iconst_2)) 
                                      (95 (anewarray (class "java.lang.Object"))) 
                                      (98 (dup)) 
                                      (99 (iconst_0)) 
                                      (100 (aload 6)) 
                                      (102 (aastore)) 
                                      (103 (dup)) 
                                      (104 (iconst_1)) 
                                      (105 (getstatic (fieldCP "T" "clojure.lang.RT" (class "java.lang.Boolean")))) 
                                      (108 (aastore)) 
                                      (109 (invokestatic (methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (112 (astore 6)) 
                                      (114 (goto 135)) ;;to TAG_3
                                      (117 (aload 6)) ;;at TAG_4
                                      (119 (instanceof (class "clojure.lang.IPersistentMap"))) 
                                      (122 (ifne 135)) ;;to TAG_3
                                      (125 (new (class "java.lang.IllegalArgumentException"))) 
                                      (128 (dup)) 
                                      (129 (ldc 0)) ;;STRING:: "Metadata must be Symbol,Keyword,String or Map"
                                      (131 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (134 (athrow)) 
                                      (135 (aload_3)) ;;at TAG_3
                                      (136 (iconst_1)) 
                                      (137 (aconst_null)) 
                                      (138 (iconst_1)) 
                                      (139 (invokestatic (methodCP "read" "clojure.lang.LispReader" ((class "java.io.PushbackReader") boolean (class "java.lang.Object") boolean) (class "java.lang.Object")))) 
                                      (142 (astore 7)) 
                                      (144 (aload 7)) 
                                      (146 (instanceof (class "clojure.lang.IMeta"))) 
                                      (149 (ifeq 306)) ;;to TAG_5
                                      (152 (iload 4)) 
                                      (154 (iconst_m1)) 
                                      (155 (if_icmpeq 199)) ;;to TAG_6
                                      (158 (aload 7)) 
                                      (160 (instanceof (class "clojure.lang.ISeq"))) 
                                      (163 (ifeq 199)) ;;to TAG_6
                                      (166 (aload 6)) 
                                      (168 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (171 (getstatic (fieldCP "LINE_KEY" "clojure.lang.RT" (class "clojure.lang.Keyword")))) 
                                      (174 (iload 4)) 
                                      (176 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (179 (invokeinterface (methodCP "assoc" "clojure.lang.IPersistentMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.IPersistentMap")) 3)) 
                                      (184 (getstatic (fieldCP "COLUMN_KEY" "clojure.lang.RT" (class "clojure.lang.Keyword")))) 
                                      (187 (iload 5)) 
                                      (189 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (192 (invokeinterface (methodCP "assoc" "clojure.lang.IPersistentMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.IPersistentMap")) 3)) 
                                      (197 (astore 6)) 
                                      (199 (aload 7)) ;;at TAG_6
                                      (201 (instanceof (class "clojure.lang.IReference"))) 
                                      (204 (ifeq 226)) ;;to TAG_7
                                      (207 (aload 7)) 
                                      (209 (checkcast (class "clojure.lang.IReference"))) 
                                      (212 (aload 6)) 
                                      (214 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (217 (invokeinterface (methodCP "resetMeta" "clojure.lang.IReference" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.IPersistentMap")) 2)) 
                                      (222 (pop)) 
                                      (223 (aload 7)) 
                                      (225 (areturn)) 
                                      (226 (aload 7)) ;;at TAG_7
                                      (228 (invokestatic (methodCP "meta" "clojure.lang.RT" ((class "java.lang.Object")) (class "clojure.lang.IPersistentMap")))) 
                                      (231 (astore 8)) 
                                      (233 (aload 6)) 
                                      (235 (invokestatic (methodCP "seq" "clojure.lang.RT" ((class "java.lang.Object")) (class "clojure.lang.ISeq")))) 
                                      (238 (astore 9)) 
                                      (240 (aload 9)) ;;at TAG_9
                                      (242 (ifnull 290)) ;;to TAG_8
                                      (245 (aload 9)) 
                                      (247 (invokeinterface (methodCP "first" "clojure.lang.ISeq" () (class "java.lang.Object")) 1)) 
                                      (252 (checkcast (class "clojure.lang.IMapEntry"))) 
                                      (255 (astore 10)) 
                                      (257 (aload 8)) 
                                      (259 (aload 10)) 
                                      (261 (invokeinterface (methodCP "getKey" "clojure.lang.IMapEntry" () (class "java.lang.Object")) 1)) 
                                      (266 (aload 10)) 
                                      (268 (invokeinterface (methodCP "getValue" "clojure.lang.IMapEntry" () (class "java.lang.Object")) 1)) 
                                      (273 (invokestatic (methodCP "assoc" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.Associative")))) 
                                      (276 (astore 8)) 
                                      (278 (aload 9)) 
                                      (280 (invokeinterface (methodCP "next" "clojure.lang.ISeq" () (class "clojure.lang.ISeq")) 1)) 
                                      (285 (astore 9)) 
                                      (287 (goto 240)) ;;to TAG_9
                                      (290 (aload 7)) ;;at TAG_8
                                      (292 (checkcast (class "clojure.lang.IObj"))) 
                                      (295 (aload 8)) 
                                      (297 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (300 (invokeinterface (methodCP "withMeta" "clojure.lang.IObj" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.IObj")) 2)) 
                                      (305 (areturn)) 
                                      (306 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_5
                                      (309 (dup)) 
                                      (310 (ldc 1)) ;;STRING:: "Metadata can only be applied to IMetas"
                                      (312 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (315 (athrow)) 
                                      (endofcode 316))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *LispReader$MetaReader-class-table*
  (make-static-class-decls 
   *clojure.lang.LispReader$MetaReader*))

(defconst *package-name-map* 
  ("clojure.lang.LispReader$MetaReader" . "clojure.lang"))

