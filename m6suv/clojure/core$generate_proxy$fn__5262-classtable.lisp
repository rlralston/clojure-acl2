; core$generate_proxy$fn__5262-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:43 CDT 2014.
;

(defconst *clojure.core$generate_proxy$fn__5262*
 (make-class-def
      '(class "clojure.core$generate_proxy$fn__5262"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields
                        (field "super_type" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 0) (max_locals . 0) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "super_type" "clojure.core$generate_proxy$fn__5262" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 67)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "clojure.asm.commons.GeneratorAdapter")))
                                      (4 (invokevirtual
					(methodCP "loadThis" "clojure.asm.commons.GeneratorAdapter" () void)))
                                      (7 (aconst_null))
                                      (8 (pop))
                                      (9 (aload_1))
                                      (10 (checkcast (class "clojure.asm.commons.GeneratorAdapter")))
                                      (13 (invokevirtual
					(methodCP "loadArgs" "clojure.asm.commons.GeneratorAdapter" () void)))
                                      (16 (aconst_null))
                                      (17 (pop))
                                      (18 (aload_1))
                                      (19 (aconst_null))
                                      (20 (astore_1))
                                      (21 (checkcast (class "clojure.asm.MethodAdapter")))
                                      (24 (getstatic (fieldCP "INVOKESPECIAL" "clojure.asm.Opcodes" int)))
                                      (27 (aload_0))
                                      (28 (getfield (fieldCP "super_type" "clojure.core$generate_proxy$fn__5262" (class "java.lang.Object"))))
                                      (31 (checkcast (class "clojure.asm.Type")))
                                      (34 (invokevirtual
					(methodCP "getInternalName" "clojure.asm.Type" () (class "java.lang.String"))))
                                      (37 (checkcast (class "java.lang.String")))
                                      (40 (aload_2))
                                      (41 (checkcast (class "clojure.asm.commons.Method")))
                                      (44 (invokevirtual
					(methodCP "getName" "clojure.asm.commons.Method" () (class "java.lang.String"))))
                                      (47 (checkcast (class "java.lang.String")))
                                      (50 (aload_2))
                                      (51 (aconst_null))
                                      (52 (astore_2))
                                      (53 (checkcast (class "clojure.asm.commons.Method")))
                                      (56 (invokevirtual
					(methodCP "getDescriptor" "clojure.asm.commons.Method" () (class "java.lang.String"))))
                                      (59 (checkcast (class "java.lang.String")))
                                      (62 (invokevirtual
					(methodCP "visitMethodInsn" "clojure.asm.MethodAdapter" (int (class "java.lang.String") (class "java.lang.String") (class "java.lang.String")) void)))
                                      (65 (aconst_null))
                                      (66 (areturn))
                                      (endofcode 67))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$generate_proxy$fn__5262-class-table*
  (make-static-class-decls 
   *clojure.core$generate_proxy$fn__5262*))

(defconst *package-name-map* 
  ("clojure.core$generate_proxy$fn__5262" . "clojure"))
