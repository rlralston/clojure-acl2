; SpreadGeneric$S1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.invoke.SpreadGeneric$S1*
 (make-class-def
      '(class "java.lang.invoke.SpreadGeneric$S1"
            "java.lang.invoke.SpreadGeneric$Adapter"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.invoke.SpreadGeneric"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.invoke.SpreadGeneric$Adapter" ((class "java.lang.invoke.SpreadGeneric")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.invoke.SpreadGeneric") (class "java.lang.invoke.MethodHandle"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.lang.invoke.SpreadGeneric$Adapter" ((class "java.lang.invoke.SpreadGeneric") (class "java.lang.invoke.MethodHandle")) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "makeInstance"
                              (parameters (class "java.lang.invoke.SpreadGeneric") (class "java.lang.invoke.MethodHandle"))
                              (returntype . (class "java.lang.invoke.SpreadGeneric$S1"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 10)
                                   (parsedcode
                                      (0 (new (class "java.lang.invoke.SpreadGeneric$S1")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aload_2))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.invoke.SpreadGeneric$S1" ((class "java.lang.invoke.SpreadGeneric") (class "java.lang.invoke.MethodHandle")) void)))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_S0"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_2))
                                      (2 (iconst_0))
                                      (3 (invokespecial
					(methodCP "check" "java.lang.invoke.SpreadGeneric$Adapter" ((class "java.lang.Object") int) (class "java.lang.Object"))))
                                      (6 (astore_2))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "target" "java.lang.invoke.SpreadGeneric$S1" (class "java.lang.invoke.MethodHandle"))))
                                      (11 (aload_1))
                                      (12 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (15 (areturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_S1"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iconst_1))
                                      (3 (invokespecial
					(methodCP "check" "java.lang.invoke.SpreadGeneric$Adapter" ((class "java.lang.Object") int) (class "java.lang.Object"))))
                                      (6 (astore_1))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "target" "java.lang.invoke.SpreadGeneric$S1" (class "java.lang.invoke.MethodHandle"))))
                                      (11 (aload_0))
                                      (12 (aload_1))
                                      (13 (iconst_0))
                                      (14 (invokespecial
					(methodCP "select" "java.lang.invoke.SpreadGeneric$Adapter" ((class "java.lang.Object") int) (class "java.lang.Object"))))
                                      (17 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "makeInstance"
                              (parameters (class "java.lang.invoke.SpreadGeneric") (class "java.lang.invoke.MethodHandle"))
                              (returntype . (class "java.lang.invoke.SpreadGeneric$Adapter"))
                              (accessflags  *class*  *protected*  *volatile* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokevirtual
					(methodCP "makeInstance" "java.lang.invoke.SpreadGeneric$S1" ((class "java.lang.invoke.SpreadGeneric") (class "java.lang.invoke.MethodHandle")) (class "java.lang.invoke.SpreadGeneric$S1"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *SpreadGeneric$S1-class-table*
  (make-static-class-decls 
   *java.lang.invoke.SpreadGeneric$S1*))

(defconst *package-name-map* 
  ("java.lang.invoke.SpreadGeneric$S1" . "java.lang.invoke"))

