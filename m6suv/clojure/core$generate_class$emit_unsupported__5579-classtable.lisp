; core$generate_class$emit_unsupported__5579-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$generate_class$emit_unsupported__5579*
 (make-class-def
      '(class "clojure.core$generate_class$emit_unsupported__5579"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "str")
                        (STRING  " (")
                        (STRING  "/")
                        (STRING  " not defined?)"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "prefix" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "ex_type" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "impl_pkg_name" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "str"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$generate_class$emit_unsupported__5579" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "prefix" "clojure.core$generate_class$emit_unsupported__5579" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "ex_type" "clojure.core$generate_class$emit_unsupported__5579" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "impl_pkg_name" "clojure.core$generate_class$emit_unsupported__5579" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 10) (max_locals . 3) (code_length . 65)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "clojure.asm.commons.GeneratorAdapter")))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "ex_type" "clojure.core$generate_class$emit_unsupported__5579" (class "java.lang.Object"))))
                                      (10 (checkcast (class "clojure.asm.Type")))
                                      (13 (getstatic (fieldCP "const__0" "clojure.core$generate_class$emit_unsupported__5579" (class "clojure.lang.Var"))))
                                      (16 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (19 (checkcast (class "clojure.lang.IFn")))
                                      (22 (aload_2))
                                      (23 (checkcast (class "clojure.asm.commons.Method")))
                                      (26 (invokevirtual
					(methodCP "getName" "clojure.asm.commons.Method" () (class "java.lang.String"))))
                                      (29 (ldc 2))        ;;STRING:: " ("
                                      (31 (aload_0))
                                      (32 (getfield (fieldCP "impl_pkg_name" "clojure.core$generate_class$emit_unsupported__5579" (class "java.lang.Object"))))
                                      (35 (ldc 3))        ;;STRING:: "/"
                                      (37 (aload_0))
                                      (38 (getfield (fieldCP "prefix" "clojure.core$generate_class$emit_unsupported__5579" (class "java.lang.Object"))))
                                      (41 (aload_2))
                                      (42 (aconst_null))
                                      (43 (astore_2))
                                      (44 (checkcast (class "clojure.asm.commons.Method")))
                                      (47 (invokevirtual
					(methodCP "getName" "clojure.asm.commons.Method" () (class "java.lang.String"))))
                                      (50 (ldc 4))        ;;STRING:: " not defined?)"
                                      (52 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 8))
                                      (57 (checkcast (class "java.lang.String")))
                                      (60 (invokevirtual
					(methodCP "throwException" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String")) void)))
                                      (63 (aconst_null))
                                      (64 (areturn))
                                      (endofcode 65))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$generate_class$emit_unsupported__5579-class-table*
  (make-static-class-decls 
   *clojure.core$generate_class$emit_unsupported__5579*))

(defconst *package-name-map* 
  ("clojure.core$generate_class$emit_unsupported__5579" . "clojure"))
