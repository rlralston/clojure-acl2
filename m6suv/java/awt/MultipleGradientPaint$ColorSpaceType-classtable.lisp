; MultipleGradientPaint$ColorSpaceType-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.MultipleGradientPaint$ColorSpaceType*
 (make-class-def
      '(class "java.awt.MultipleGradientPaint$ColorSpaceType"
            "java.lang.Enum"
            (constant_pool
                        (STRING  "SRGB")
                        (STRING  "LINEAR_RGB"))
            (fields
                        (field "SRGB" (class "java.awt.MultipleGradientPaint$ColorSpaceType") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "LINEAR_RGB" (class "java.awt.MultipleGradientPaint$ColorSpaceType") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "$VALUES" (array (class "java.awt.MultipleGradientPaint$ColorSpaceType")) (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "values"
                              (parameters )
                              (returntype . (array (class "java.awt.MultipleGradientPaint$ColorSpaceType")))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 10)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$VALUES" "java.awt.MultipleGradientPaint$ColorSpaceType" (array (class "java.awt.MultipleGradientPaint$ColorSpaceType")))))
                                      (3 (invokevirtual
					(methodCP "clone" "java.awt.MultipleGradientPaint$ColorSpaceType[]" () (class "java.lang.Object"))))
                                      (6 (checkcast (array (class "java.awt.MultipleGradientPaint$ColorSpaceType"))))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "valueOf"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.awt.MultipleGradientPaint$ColorSpaceType"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (ldc_w ))
                                      (3 (aload_0))
                                      (4 (invokestatic
					(methodCP "valueOf" "java.lang.Enum" ((class "java.lang.Class") (class "java.lang.String")) (class "java.lang.Enum"))))
                                      (7 (checkcast (class "java.awt.MultipleGradientPaint$ColorSpaceType")))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.lang.Enum" ((class "java.lang.String") int) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 46)
                                   (parsedcode
                                      (0 (new (class "java.awt.MultipleGradientPaint$ColorSpaceType")))
                                      (3 (dup))
                                      (4 (ldc 0))         ;;STRING:: "SRGB"
                                      (6 (iconst_0))
                                      (7 (invokespecial
					(methodCP "<init>" "java.awt.MultipleGradientPaint$ColorSpaceType" ((class "java.lang.String") int) void)))
                                      (10 (putstatic (fieldCP "SRGB" "java.awt.MultipleGradientPaint$ColorSpaceType" (class "java.awt.MultipleGradientPaint$ColorSpaceType"))))
                                      (13 (new (class "java.awt.MultipleGradientPaint$ColorSpaceType")))
                                      (16 (dup))
                                      (17 (ldc 1))        ;;STRING:: "LINEAR_RGB"
                                      (19 (iconst_1))
                                      (20 (invokespecial
					(methodCP "<init>" "java.awt.MultipleGradientPaint$ColorSpaceType" ((class "java.lang.String") int) void)))
                                      (23 (putstatic (fieldCP "LINEAR_RGB" "java.awt.MultipleGradientPaint$ColorSpaceType" (class "java.awt.MultipleGradientPaint$ColorSpaceType"))))
                                      (26 (iconst_2))
                                      (27 (anewarray (class "java.awt.MultipleGradientPaint$ColorSpaceType")))
                                      (30 (dup))
                                      (31 (iconst_0))
                                      (32 (getstatic (fieldCP "SRGB" "java.awt.MultipleGradientPaint$ColorSpaceType" (class "java.awt.MultipleGradientPaint$ColorSpaceType"))))
                                      (35 (aastore))
                                      (36 (dup))
                                      (37 (iconst_1))
                                      (38 (getstatic (fieldCP "LINEAR_RGB" "java.awt.MultipleGradientPaint$ColorSpaceType" (class "java.awt.MultipleGradientPaint$ColorSpaceType"))))
                                      (41 (aastore))
                                      (42 (putstatic (fieldCP "$VALUES" "java.awt.MultipleGradientPaint$ColorSpaceType" (array (class "java.awt.MultipleGradientPaint$ColorSpaceType")))))
                                      (45 (return))
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *MultipleGradientPaint$ColorSpaceType-class-table*
  (make-static-class-decls 
   *java.awt.MultipleGradientPaint$ColorSpaceType*))

(defconst *package-name-map* 
  ("java.awt.MultipleGradientPaint$ColorSpaceType" . "java.awt"))

