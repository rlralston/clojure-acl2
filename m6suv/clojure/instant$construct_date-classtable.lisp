; instant$construct_date-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:49 CDT 2014.
;

(defconst *clojure.instant$construct_date*
 (make-class-def
      '(class "clojure.instant$construct_date"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.instant")
                        (STRING  "construct-calendar"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.instant"
                                      (2 (ldc 1))         ;;STRING:: "construct-calendar"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.instant$construct_date" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 12) (max_locals . 11) (code_length . 65)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.instant$construct_date" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_1))
                                      (10 (aconst_null))
                                      (11 (astore_1))
                                      (12 (aload_2))
                                      (13 (aconst_null))
                                      (14 (astore_2))
                                      (15 (aload_3))
                                      (16 (aconst_null))
                                      (17 (astore_3))
                                      (18 (aload 4))
                                      (20 (aconst_null))
                                      (21 (astore 4))
                                      (23 (aload 5))
                                      (25 (aconst_null))
                                      (26 (astore 5))
                                      (28 (aload 6))
                                      (30 (aconst_null))
                                      (31 (astore 6))
                                      (33 (aload 7))
                                      (35 (aconst_null))
                                      (36 (astore 7))
                                      (38 (aload 8))
                                      (40 (aconst_null))
                                      (41 (astore 8))
                                      (43 (aload 9))
                                      (45 (aconst_null))
                                      (46 (astore 9))
                                      (48 (aload 10))
                                      (50 (aconst_null))
                                      (51 (astore 10))
                                      (53 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 11))
                                      (58 (checkcast (class "java.util.Calendar")))
                                      (61 (invokevirtual
					(methodCP "getTime" "java.util.Calendar" () (class "java.util.Date"))))
                                      (64 (areturn))
                                      (endofcode 65))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *instant$construct_date-class-table*
  (make-static-class-decls 
   *clojure.instant$construct_date*))

(defconst *package-name-map* 
  ("clojure.instant$construct_date" . "clojure"))
