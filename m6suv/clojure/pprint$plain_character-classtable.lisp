; pprint$plain_character-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$plain_character*
 (make-class-def
      '(class "clojure.pprint$plain_character"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.pprint")
                        (STRING  "next-arg")
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "print"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 54)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.pprint"
                                      (2 (ldc 1))         ;;STRING:: "next-arg"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$plain_character" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "nth"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$plain_character" (class "clojure.lang.Var"))))
                                      (26 (lconst_0))
                                      (27 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (30 (putstatic (fieldCP "const__2" "clojure.pprint$plain_character" (class "java.lang.Object"))))
                                      (33 (lconst_1))
                                      (34 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (37 (putstatic (fieldCP "const__3" "clojure.pprint$plain_character" (class "java.lang.Object"))))
                                      (40 (ldc 2))        ;;STRING:: "clojure.core"
                                      (42 (ldc 4))        ;;STRING:: "print"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.pprint$plain_character" (class "clojure.lang.Var"))))
                                      (53 (return))
                                      (endofcode 54))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 7) (code_length . 72)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$plain_character" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_2))
                                      (10 (aconst_null))
                                      (11 (astore_2))
                                      (12 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (17 (astore 4))
                                      (19 (aload 4))
                                      (21 (lconst_0))
                                      (22 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (25 (aconst_null))
                                      (26 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (29 (astore 5))
                                      (31 (aload 4))
                                      (33 (aconst_null))
                                      (34 (astore 4))
                                      (36 (lconst_1))
                                      (37 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (40 (aconst_null))
                                      (41 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (44 (astore 6))
                                      (46 (getstatic (fieldCP "const__4" "clojure.pprint$plain_character" (class "clojure.lang.Var"))))
                                      (49 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (52 (checkcast (class "clojure.lang.IFn")))
                                      (55 (aload 5))
                                      (57 (aconst_null))
                                      (58 (astore 5))
                                      (60 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (65 (pop))
                                      (66 (aload 6))
                                      (68 (aconst_null))
                                      (69 (astore 6))
                                      (71 (areturn))
                                      (endofcode 72))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$plain_character-class-table*
  (make-static-class-decls 
   *clojure.pprint$plain_character*))

(defconst *package-name-map* 
  ("clojure.pprint$plain_character" . "clojure"))

