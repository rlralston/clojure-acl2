; reflect$access_flag-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.reflect$access_flag*
 (make-class-def
      '(class "clojure.reflect$access_flag"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "nthnext")
                        (LONG 2)
                        (STRING  "name")
                        (STRING  "flag")
                        (STRING  "contexts")
                        (STRING  "set")
                        (STRING  "map")
                        (STRING  "keyword"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 125)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.reflect$access_flag" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.reflect$access_flag" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.reflect$access_flag" (class "java.lang.Object"))))
                                      (27 (ldc 0))        ;;STRING:: "clojure.core"
                                      (29 (ldc 2))        ;;STRING:: "nthnext"
                                      (31 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (34 (checkcast (class "clojure.lang.Var")))
                                      (37 (putstatic (fieldCP "const__3" "clojure.reflect$access_flag" (class "clojure.lang.Var"))))
                                      (40 (ldc2_w 3))     ;; LONG:: "2"
                                      (43 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (46 (putstatic (fieldCP "const__4" "clojure.reflect$access_flag" (class "java.lang.Object"))))
                                      (49 (aconst_null))
                                      (50 (ldc 4))        ;;STRING:: "name"
                                      (52 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (55 (checkcast (class "clojure.lang.Keyword")))
                                      (58 (putstatic (fieldCP "const__5" "clojure.reflect$access_flag" (class "clojure.lang.Keyword"))))
                                      (61 (aconst_null))
                                      (62 (ldc 5))        ;;STRING:: "flag"
                                      (64 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (67 (checkcast (class "clojure.lang.Keyword")))
                                      (70 (putstatic (fieldCP "const__6" "clojure.reflect$access_flag" (class "clojure.lang.Keyword"))))
                                      (73 (aconst_null))
                                      (74 (ldc 6))        ;;STRING:: "contexts"
                                      (76 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (79 (checkcast (class "clojure.lang.Keyword")))
                                      (82 (putstatic (fieldCP "const__7" "clojure.reflect$access_flag" (class "clojure.lang.Keyword"))))
                                      (85 (ldc 0))        ;;STRING:: "clojure.core"
                                      (87 (ldc 7))        ;;STRING:: "set"
                                      (89 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (92 (checkcast (class "clojure.lang.Var")))
                                      (95 (putstatic (fieldCP "const__8" "clojure.reflect$access_flag" (class "clojure.lang.Var"))))
                                      (98 (ldc 0))        ;;STRING:: "clojure.core"
                                      (100 (ldc 8))       ;;STRING:: "map"
                                      (102 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (105 (checkcast (class "clojure.lang.Var")))
                                      (108 (putstatic (fieldCP "const__9" "clojure.reflect$access_flag" (class "clojure.lang.Var"))))
                                      (111 (ldc 0))       ;;STRING:: "clojure.core"
                                      (113 (ldc 9))       ;;STRING:: "keyword"
                                      (115 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (118 (checkcast (class "clojure.lang.Var")))
                                      (121 (putstatic (fieldCP "const__10" "clojure.reflect$access_flag" (class "clojure.lang.Var"))))
                                      (124 (return))
                                      (endofcode 125))
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
                                   (max_stack . 8) (max_locals . 6) (code_length . 130)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (astore_2))
                                      (4 (aload_2))
                                      (5 (lconst_0))
                                      (6 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (9 (aconst_null))
                                      (10 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (13 (astore_3))
                                      (14 (aload_2))
                                      (15 (lconst_1))
                                      (16 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (19 (aconst_null))
                                      (20 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (23 (astore 4))
                                      (25 (getstatic (fieldCP "const__3" "clojure.reflect$access_flag" (class "clojure.lang.Var"))))
                                      (28 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (31 (checkcast (class "clojure.lang.IFn")))
                                      (34 (aload_2))
                                      (35 (aconst_null))
                                      (36 (astore_2))
                                      (37 (getstatic (fieldCP "const__4" "clojure.reflect$access_flag" (class "java.lang.Object"))))
                                      (40 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (45 (astore 5))
                                      (47 (bipush 6))
                                      (49 (anewarray (class "java.lang.Object")))
                                      (52 (dup))
                                      (53 (iconst_0))
                                      (54 (getstatic (fieldCP "const__5" "clojure.reflect$access_flag" (class "clojure.lang.Keyword"))))
                                      (57 (aastore))
                                      (58 (dup))
                                      (59 (iconst_1))
                                      (60 (aload_3))
                                      (61 (aconst_null))
                                      (62 (astore_3))
                                      (63 (aastore))
                                      (64 (dup))
                                      (65 (iconst_2))
                                      (66 (getstatic (fieldCP "const__6" "clojure.reflect$access_flag" (class "clojure.lang.Keyword"))))
                                      (69 (aastore))
                                      (70 (dup))
                                      (71 (iconst_3))
                                      (72 (aload 4))
                                      (74 (aconst_null))
                                      (75 (astore 4))
                                      (77 (aastore))
                                      (78 (dup))
                                      (79 (iconst_4))
                                      (80 (getstatic (fieldCP "const__7" "clojure.reflect$access_flag" (class "clojure.lang.Keyword"))))
                                      (83 (aastore))
                                      (84 (dup))
                                      (85 (iconst_5))
                                      (86 (getstatic (fieldCP "const__8" "clojure.reflect$access_flag" (class "clojure.lang.Var"))))
                                      (89 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (92 (checkcast (class "clojure.lang.IFn")))
                                      (95 (getstatic (fieldCP "const__9" "clojure.reflect$access_flag" (class "clojure.lang.Var"))))
                                      (98 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (101 (checkcast (class "clojure.lang.IFn")))
                                      (104 (getstatic (fieldCP "const__10" "clojure.reflect$access_flag" (class "clojure.lang.Var"))))
                                      (107 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (110 (aload 5))
                                      (112 (aconst_null))
                                      (113 (astore 5))
                                      (115 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (120 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (125 (aastore))
                                      (126 (invokestatic
					(methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (129 (areturn))
                                      (endofcode 130))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *reflect$access_flag-class-table*
  (make-static-class-decls 
   *clojure.reflect$access_flag*))

(defconst *package-name-map* 
  ("clojure.reflect$access_flag" . "clojure"))

