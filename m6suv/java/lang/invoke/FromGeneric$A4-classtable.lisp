; FromGeneric$A4-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:34 CDT 2014.
;

(defconst *java.lang.invoke.FromGeneric$A4*
 (make-class-def
      '(class "java.lang.invoke.FromGeneric$A4"
            "java.lang.invoke.FromGeneric$Adapter"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.invoke.MethodHandle"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.invoke.FromGeneric$Adapter" ((class "java.lang.invoke.MethodHandle")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (aload_3))
                                      (4 (aload 4))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.invoke.FromGeneric$Adapter" ((class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle")) void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "makeInstance"
                              (parameters (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle"))
                              (returntype . (class "java.lang.invoke.FromGeneric$A4"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 13)
                                   (parsedcode
                                      (0 (new (class "java.lang.invoke.FromGeneric$A4")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aload_2))
                                      (6 (aload_3))
                                      (7 (aload 4))
                                      (9 (invokespecial
					(methodCP "<init>" "java.lang.invoke.FromGeneric$A4" ((class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle")) void)))
                                      (12 (areturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_L4"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "invoker" "java.lang.invoke.FromGeneric$A4" (class "java.lang.invoke.MethodHandle"))))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "target" "java.lang.invoke.FromGeneric$A4" (class "java.lang.invoke.MethodHandle"))))
                                      (9 (aload_1))
                                      (10 (aload_2))
                                      (11 (aload_3))
                                      (12 (aload 4))
                                      (14 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.invoke.MethodHandle") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (17 (invokevirtual
					(methodCP "convert_L" "java.lang.invoke.FromGeneric$A4" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_I4"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "invoker" "java.lang.invoke.FromGeneric$A4" (class "java.lang.invoke.MethodHandle"))))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "target" "java.lang.invoke.FromGeneric$A4" (class "java.lang.invoke.MethodHandle"))))
                                      (9 (aload_1))
                                      (10 (aload_2))
                                      (11 (aload_3))
                                      (12 (aload 4))
                                      (14 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.invoke.MethodHandle") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) int)))
                                      (17 (invokevirtual
					(methodCP "convert_I" "java.lang.invoke.FromGeneric$A4" (int) (class "java.lang.Object"))))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_J4"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "invoker" "java.lang.invoke.FromGeneric$A4" (class "java.lang.invoke.MethodHandle"))))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "target" "java.lang.invoke.FromGeneric$A4" (class "java.lang.invoke.MethodHandle"))))
                                      (9 (aload_1))
                                      (10 (aload_2))
                                      (11 (aload_3))
                                      (12 (aload 4))
                                      (14 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.invoke.MethodHandle") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) long)))
                                      (17 (invokevirtual
					(methodCP "convert_J" "java.lang.invoke.FromGeneric$A4" (long) (class "java.lang.Object"))))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_F4"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "invoker" "java.lang.invoke.FromGeneric$A4" (class "java.lang.invoke.MethodHandle"))))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "target" "java.lang.invoke.FromGeneric$A4" (class "java.lang.invoke.MethodHandle"))))
                                      (9 (aload_1))
                                      (10 (aload_2))
                                      (11 (aload_3))
                                      (12 (aload 4))
                                      (14 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.invoke.MethodHandle") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) float)))
                                      (17 (invokevirtual
					(methodCP "convert_F" "java.lang.invoke.FromGeneric$A4" (float) (class "java.lang.Object"))))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_D4"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "invoker" "java.lang.invoke.FromGeneric$A4" (class "java.lang.invoke.MethodHandle"))))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "target" "java.lang.invoke.FromGeneric$A4" (class "java.lang.invoke.MethodHandle"))))
                                      (9 (aload_1))
                                      (10 (aload_2))
                                      (11 (aload_3))
                                      (12 (aload 4))
                                      (14 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.invoke.MethodHandle") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) double)))
                                      (17 (invokevirtual
					(methodCP "convert_D" "java.lang.invoke.FromGeneric$A4" (double) (class "java.lang.Object"))))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "makeInstance"
                              (parameters (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle"))
                              (returntype . (class "java.lang.invoke.FromGeneric$Adapter"))
                              (accessflags  *class*  *protected*  *volatile* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (aload_3))
                                      (4 (aload 4))
                                      (6 (invokevirtual
					(methodCP "makeInstance" "java.lang.invoke.FromGeneric$A4" ((class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle")) (class "java.lang.invoke.FromGeneric$A4"))))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *FromGeneric$A4-class-table*
  (make-static-class-decls 
   *java.lang.invoke.FromGeneric$A4*))

(defconst *package-name-map* 
  ("java.lang.invoke.FromGeneric$A4" . "java.lang.invoke"))

