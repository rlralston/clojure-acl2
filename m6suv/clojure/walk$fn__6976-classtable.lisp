; walk$fn__6976-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.walk$fn__6976*
 (make-class-def
      '(class "clojure.walk$fn__6976"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "commute")
                        (STRING  "deref")
                        (STRING  "*loaded-libs*")
                        (STRING  "conj")
                        (STRING  "clojure.walk")
                        (STRING  "doc")
                        (STRING  "This file defines a generic tree walker for Clojure data\nstructures.  It takes any data structure (list, vector, map, set,\nseq), calls a function on every element, and uses the return value\nof the function in place of the original.  This makes it fairly\neasy to write recursive search-and-replace functions, as shown in\nthe examples.\n\nNote: \"walk\" supports all Clojure data structures EXCEPT maps\ncreated with sorted-map-by.  There is no (obvious) way to retrieve\nthe sorting function.")
                        (STRING  "author")
                        (STRING  "Stuart Sierra"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 6) (max_locals . 0) (code_length . 111)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "commute"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.walk$fn__6976" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "deref"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.walk$fn__6976" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "*loaded-libs*"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.walk$fn__6976" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "conj"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.walk$fn__6976" (class "clojure.lang.Var"))))
                                      (52 (aconst_null))
                                      (53 (ldc 5))        ;;STRING:: "clojure.walk"
                                      (55 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (58 (checkcast (class "clojure.lang.IObj")))
                                      (61 (iconst_4))
                                      (62 (anewarray (class "java.lang.Object")))
                                      (65 (dup))
                                      (66 (iconst_0))
                                      (67 (aconst_null))
                                      (68 (ldc 6))        ;;STRING:: "doc"
                                      (70 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (73 (aastore))
                                      (74 (dup))
                                      (75 (iconst_1))
                                      (76 (ldc 7))        ;;STRING:: "This file defines a generic tree walker for Clojure data\nstructures.  It takes any data structure (list, vector, map, set,\nseq), calls a function on every element, and uses the return value\nof the function in place of the original.  This makes it fairly\neasy to write recursive search-and-replace functions, as shown in\nthe examples.\n\nNote: \"walk\" supports all Clojure data structures EXCEPT maps\ncreated with sorted-map-by.  There is no (obvious) way to retrieve\nthe sorting function."
                                      (78 (aastore))
                                      (79 (dup))
                                      (80 (iconst_2))
                                      (81 (aconst_null))
                                      (82 (ldc 8))        ;;STRING:: "author"
                                      (84 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (87 (aastore))
                                      (88 (dup))
                                      (89 (iconst_3))
                                      (90 (ldc 9))        ;;STRING:: "Stuart Sierra"
                                      (92 (aastore))
                                      (93 (invokestatic
					(methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (96 (checkcast (class "clojure.lang.IPersistentMap")))
                                      (99 (invokeinterface
					(methodCP "withMeta" "clojure.lang.IObj" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.IObj")) 2))
                                      (104 (checkcast (class "clojure.lang.AFn")))
                                      (107 (putstatic (fieldCP "const__4" "clojure.walk$fn__6976" (class "clojure.lang.AFn"))))
                                      (110 (return))
                                      (endofcode 111))
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
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 41)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.walk$fn__6976" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.walk$fn__6976" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (getstatic (fieldCP "const__2" "clojure.walk$fn__6976" (class "clojure.lang.Var"))))
                                      (21 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (26 (getstatic (fieldCP "const__3" "clojure.walk$fn__6976" (class "clojure.lang.Var"))))
                                      (29 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (32 (getstatic (fieldCP "const__4" "clojure.walk$fn__6976" (class "clojure.lang.AFn"))))
                                      (35 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (40 (areturn))
                                      (endofcode 41))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *walk$fn__6976-class-table*
  (make-static-class-decls 
   *clojure.walk$fn__6976*))

(defconst *package-name-map* 
  ("clojure.walk$fn__6976" . "clojure"))

