; core$root_directory-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$root_directory*
 (make-class-def
      '(class "clojure.core$root_directory"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "root-resource")
                        (STRING  "subs")
                        (STRING  "/"))
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
                                   (max_stack . 2) (max_locals . 0) (code_length . 34)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "root-resource"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$root_directory" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "subs"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$root_directory" (class "clojure.lang.Var"))))
                                      (26 (lconst_0))
                                      (27 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (30 (putstatic (fieldCP "const__2" "clojure.core$root_directory" (class "java.lang.Object"))))
                                      (33 (return))
                                      (endofcode 34))
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
                                   (max_stack . 5) (max_locals . 3) (code_length . 54)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$root_directory" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_1))
                                      (10 (aconst_null))
                                      (11 (astore_1))
                                      (12 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (17 (astore_2))
                                      (18 (getstatic (fieldCP "const__1" "clojure.core$root_directory" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (checkcast (class "clojure.lang.IFn")))
                                      (27 (aload_2))
                                      (28 (getstatic (fieldCP "const__2" "clojure.core$root_directory" (class "java.lang.Object"))))
                                      (31 (aload_2))
                                      (32 (aconst_null))
                                      (33 (astore_2))
                                      (34 (checkcast (class "java.lang.String")))
                                      (37 (ldc 3))        ;;STRING:: "/"
                                      (39 (checkcast (class "java.lang.String")))
                                      (42 (invokevirtual
					(methodCP "lastIndexOf" "java.lang.String" ((class "java.lang.String")) int)))
                                      (45 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (48 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (53 (areturn))
                                      (endofcode 54))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$root_directory-class-table*
  (make-static-class-decls 
   *clojure.core$root_directory*))

(defconst *package-name-map* 
  ("clojure.core$root_directory" . "clojure"))

