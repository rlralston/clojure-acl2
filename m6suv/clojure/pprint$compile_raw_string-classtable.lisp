; pprint$compile_raw_string-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$compile_raw_string*
 (make-class-def
      '(class "clojure.pprint$compile_raw_string"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "struct")
                        (STRING  "clojure.pprint")
                        (STRING  "compiled-directive")
                        (STRING  "string"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 39)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "struct"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$compile_raw_string" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (15 (ldc 3))        ;;STRING:: "compiled-directive"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$compile_raw_string" (class "clojure.lang.Var"))))
                                      (26 (aconst_null))
                                      (27 (ldc 4))        ;;STRING:: "string"
                                      (29 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (32 (checkcast (class "clojure.lang.Keyword")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.pprint$compile_raw_string" (class "clojure.lang.Keyword"))))
                                      (38 (return))
                                      (endofcode 39))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 3) (code_length . 52)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$compile_raw_string" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.pprint$compile_raw_string" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (new (class "clojure.pprint$compile_raw_string$fn__8123")))
                                      (18 (dup))
                                      (19 (aload_1))
                                      (20 (invokespecial
					(methodCP "<init>" "clojure.pprint$compile_raw_string$fn__8123" ((class "java.lang.Object")) void)))
                                      (23 (aconst_null))
                                      (24 (iconst_2))
                                      (25 (anewarray (class "java.lang.Object")))
                                      (28 (dup))
                                      (29 (iconst_0))
                                      (30 (getstatic (fieldCP "const__2" "clojure.pprint$compile_raw_string" (class "clojure.lang.Keyword"))))
                                      (33 (aastore))
                                      (34 (dup))
                                      (35 (iconst_1))
                                      (36 (aload_1))
                                      (37 (aconst_null))
                                      (38 (astore_1))
                                      (39 (aastore))
                                      (40 (invokestatic
					(methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (43 (aload_2))
                                      (44 (aconst_null))
                                      (45 (astore_2))
                                      (46 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 6))
                                      (51 (areturn))
                                      (endofcode 52))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$compile_raw_string-class-table*
  (make-static-class-decls 
   *clojure.pprint$compile_raw_string*))

(defconst *package-name-map* 
  ("clojure.pprint$compile_raw_string" . "clojure"))

