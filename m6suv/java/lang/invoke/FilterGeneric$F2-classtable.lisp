; FilterGeneric$F2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:34 CDT 2014.
;

(defconst *java.lang.invoke.FilterGeneric$F2*
 (make-class-def
      '(class "java.lang.invoke.FilterGeneric$F2"
            "java.lang.invoke.FilterGeneric$Adapter"
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
					(methodCP "<init>" "java.lang.invoke.FilterGeneric$Adapter" ((class "java.lang.invoke.MethodHandle")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (aload_3))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.invoke.FilterGeneric$Adapter" ((class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle")) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "makeInstance"
                              (parameters (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle"))
                              (returntype . (class "java.lang.invoke.FilterGeneric$F2"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.lang.invoke.FilterGeneric$F2")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aload_2))
                                      (6 (aload_3))
                                      (7 (invokespecial
					(methodCP "<init>" "java.lang.invoke.FilterGeneric$F2" ((class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle")) void)))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_V0"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "filter" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (8 (aload_1))
                                      (9 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (12 (aload_2))
                                      (13 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_V1"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (aload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "filter" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (9 (aload_2))
                                      (10 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (13 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_F0"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "filter" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (8 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" () (class "java.lang.Object"))))
                                      (11 (aload_1))
                                      (12 (aload_2))
                                      (13 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_F1"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "filter" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (8 (aload_1))
                                      (9 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (12 (aload_1))
                                      (13 (aload_2))
                                      (14 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (17 (areturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_F2"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "filter" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (8 (aload_1))
                                      (9 (aload_2))
                                      (10 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (13 (aload_1))
                                      (14 (aload_2))
                                      (15 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_C0"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "filter" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (8 (aload_1))
                                      (9 (aload_2))
                                      (10 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (13 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_C1"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (aload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "filter" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (9 (aload_2))
                                      (10 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (13 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_C2"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (aload_1))
                                      (5 (aload_2))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "filter" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (10 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" () (class "java.lang.Object"))))
                                      (13 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke_Y0"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 35)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (aload_1))
                                      (7 (aastore))
                                      (8 (dup))
                                      (9 (iconst_1))
                                      (10 (aload_2))
                                      (11 (aastore))
                                      (12 (astore_3))
                                      (13 (aload_0))
                                      (14 (getfield (fieldCP "filter" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (17 (aload_3))
                                      (18 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((array (class "java.lang.Object"))) void)))
                                      (21 (aload_0))
                                      (22 (getfield (fieldCP "target" "java.lang.invoke.FilterGeneric$F2" (class "java.lang.invoke.MethodHandle"))))
                                      (25 (aload_3))
                                      (26 (iconst_0))
                                      (27 (aaload))
                                      (28 (aload_3))
                                      (29 (iconst_1))
                                      (30 (aaload))
                                      (31 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (34 (areturn))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "makeInstance"
                              (parameters (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle"))
                              (returntype . (class "java.lang.invoke.FilterGeneric$Adapter"))
                              (accessflags  *class*  *protected*  *volatile* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (aload_3))
                                      (4 (invokevirtual
					(methodCP "makeInstance" "java.lang.invoke.FilterGeneric$F2" ((class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle")) (class "java.lang.invoke.FilterGeneric$F2"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *FilterGeneric$F2-class-table*
  (make-static-class-decls 
   *java.lang.invoke.FilterGeneric$F2*))

(defconst *package-name-map* 
  ("java.lang.invoke.FilterGeneric$F2" . "java.lang.invoke"))

