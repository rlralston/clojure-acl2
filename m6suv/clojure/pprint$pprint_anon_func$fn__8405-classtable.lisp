; pprint$pprint_anon_func$fn__8405-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$pprint_anon_func$fn__8405*
 (make-class-def
      '(class "clojure.pprint$pprint_anon_func$fn__8405"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "vector")
                        (STRING  "str"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 35)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "vector"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pprint_anon_func$fn__8405" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "str"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$pprint_anon_func$fn__8405" (class "clojure.lang.Var"))))
                                      (26 (bipush 37))
                                      (28 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (31 (putstatic (fieldCP "const__2" "clojure.pprint$pprint_anon_func$fn__8405" (class "java.lang.Object"))))
                                      (34 (return))
                                      (endofcode 35))
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
                                   (max_stack . 6) (max_locals . 3) (code_length . 38)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$pprint_anon_func$fn__8405" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_1))
                                      (10 (aconst_null))
                                      (11 (astore_1))
                                      (12 (getstatic (fieldCP "const__1" "clojure.pprint$pprint_anon_func$fn__8405" (class "clojure.lang.Var"))))
                                      (15 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (18 (checkcast (class "clojure.lang.IFn")))
                                      (21 (getstatic (fieldCP "const__2" "clojure.pprint$pprint_anon_func$fn__8405" (class "java.lang.Object"))))
                                      (24 (aload_2))
                                      (25 (aconst_null))
                                      (26 (astore_2))
                                      (27 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (32 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (37 (areturn))
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$pprint_anon_func$fn__8405-class-table*
  (make-static-class-decls 
   *clojure.pprint$pprint_anon_func$fn__8405*))

(defconst *package-name-map* 
  ("clojure.pprint$pprint_anon_func$fn__8405" . "clojure"))

