; MultipleGradientPaint$CycleMethod-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.MultipleGradientPaint$CycleMethod*
 (make-class-def
      '(class "java.awt.MultipleGradientPaint$CycleMethod"
            "java.lang.Enum"
            (constant_pool
                        (STRING  "NO_CYCLE")
                        (STRING  "REFLECT")
                        (STRING  "REPEAT"))
            (fields
                        (field "NO_CYCLE" (class "java.awt.MultipleGradientPaint$CycleMethod") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "REFLECT" (class "java.awt.MultipleGradientPaint$CycleMethod") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "REPEAT" (class "java.awt.MultipleGradientPaint$CycleMethod") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "$VALUES" (array (class "java.awt.MultipleGradientPaint$CycleMethod")) (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "values"
                              (parameters )
                              (returntype . (array (class "java.awt.MultipleGradientPaint$CycleMethod")))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 10)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$VALUES" "java.awt.MultipleGradientPaint$CycleMethod" (array (class "java.awt.MultipleGradientPaint$CycleMethod")))))
                                      (3 (invokevirtual
					(methodCP "clone" "java.awt.MultipleGradientPaint$CycleMethod[]" () (class "java.lang.Object"))))
                                      (6 (checkcast (array (class "java.awt.MultipleGradientPaint$CycleMethod"))))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "valueOf"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.awt.MultipleGradientPaint$CycleMethod"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (ldc_w ))
                                      (3 (aload_0))
                                      (4 (invokestatic
					(methodCP "valueOf" "java.lang.Enum" ((class "java.lang.Class") (class "java.lang.String")) (class "java.lang.Enum"))))
                                      (7 (checkcast (class "java.awt.MultipleGradientPaint$CycleMethod")))
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 65)
                                   (parsedcode
                                      (0 (new (class "java.awt.MultipleGradientPaint$CycleMethod")))
                                      (3 (dup))
                                      (4 (ldc 0))         ;;STRING:: "NO_CYCLE"
                                      (6 (iconst_0))
                                      (7 (invokespecial
					(methodCP "<init>" "java.awt.MultipleGradientPaint$CycleMethod" ((class "java.lang.String") int) void)))
                                      (10 (putstatic (fieldCP "NO_CYCLE" "java.awt.MultipleGradientPaint$CycleMethod" (class "java.awt.MultipleGradientPaint$CycleMethod"))))
                                      (13 (new (class "java.awt.MultipleGradientPaint$CycleMethod")))
                                      (16 (dup))
                                      (17 (ldc 1))        ;;STRING:: "REFLECT"
                                      (19 (iconst_1))
                                      (20 (invokespecial
					(methodCP "<init>" "java.awt.MultipleGradientPaint$CycleMethod" ((class "java.lang.String") int) void)))
                                      (23 (putstatic (fieldCP "REFLECT" "java.awt.MultipleGradientPaint$CycleMethod" (class "java.awt.MultipleGradientPaint$CycleMethod"))))
                                      (26 (new (class "java.awt.MultipleGradientPaint$CycleMethod")))
                                      (29 (dup))
                                      (30 (ldc 2))        ;;STRING:: "REPEAT"
                                      (32 (iconst_2))
                                      (33 (invokespecial
					(methodCP "<init>" "java.awt.MultipleGradientPaint$CycleMethod" ((class "java.lang.String") int) void)))
                                      (36 (putstatic (fieldCP "REPEAT" "java.awt.MultipleGradientPaint$CycleMethod" (class "java.awt.MultipleGradientPaint$CycleMethod"))))
                                      (39 (iconst_3))
                                      (40 (anewarray (class "java.awt.MultipleGradientPaint$CycleMethod")))
                                      (43 (dup))
                                      (44 (iconst_0))
                                      (45 (getstatic (fieldCP "NO_CYCLE" "java.awt.MultipleGradientPaint$CycleMethod" (class "java.awt.MultipleGradientPaint$CycleMethod"))))
                                      (48 (aastore))
                                      (49 (dup))
                                      (50 (iconst_1))
                                      (51 (getstatic (fieldCP "REFLECT" "java.awt.MultipleGradientPaint$CycleMethod" (class "java.awt.MultipleGradientPaint$CycleMethod"))))
                                      (54 (aastore))
                                      (55 (dup))
                                      (56 (iconst_2))
                                      (57 (getstatic (fieldCP "REPEAT" "java.awt.MultipleGradientPaint$CycleMethod" (class "java.awt.MultipleGradientPaint$CycleMethod"))))
                                      (60 (aastore))
                                      (61 (putstatic (fieldCP "$VALUES" "java.awt.MultipleGradientPaint$CycleMethod" (array (class "java.awt.MultipleGradientPaint$CycleMethod")))))
                                      (64 (return))
                                      (endofcode 65))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *MultipleGradientPaint$CycleMethod-class-table*
  (make-static-class-decls 
   *java.awt.MultipleGradientPaint$CycleMethod*))

(defconst *package-name-map* 
  ("java.awt.MultipleGradientPaint$CycleMethod" . "java.awt"))

