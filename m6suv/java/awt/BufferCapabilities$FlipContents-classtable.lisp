; BufferCapabilities$FlipContents-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:24 CDT 2014.
;

(defconst *java.awt.BufferCapabilities$FlipContents*
 (make-class-def
      '(class "java.awt.BufferCapabilities$FlipContents"
            "java.awt.AttributeValue"
            (constant_pool
                        (STRING  "undefined")
                        (STRING  "background")
                        (STRING  "prior")
                        (STRING  "copied"))
            (fields
                        (field "I_UNDEFINED" int (accessflags  *class*  *private*  *static* ) -1)
                        (field "I_BACKGROUND" int (accessflags  *class*  *private*  *static* ) -1)
                        (field "I_PRIOR" int (accessflags  *class*  *private*  *static* ) -1)
                        (field "I_COPIED" int (accessflags  *class*  *private*  *static* ) -1)
                        (field "NAMES" (array (class "java.lang.String")) (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "UNDEFINED" (class "java.awt.BufferCapabilities$FlipContents") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "BACKGROUND" (class "java.awt.BufferCapabilities$FlipContents") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "PRIOR" (class "java.awt.BufferCapabilities$FlipContents") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "COPIED" (class "java.awt.BufferCapabilities$FlipContents") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (getstatic (fieldCP "NAMES" "java.awt.BufferCapabilities$FlipContents" (array (class "java.lang.String")))))
                                      (5 (invokespecial
					(methodCP "<init>" "java.awt.AttributeValue" (int (array (class "java.lang.String"))) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "toString" "java.awt.AttributeValue" () (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "hashCode" "java.awt.AttributeValue" () int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 96)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (putstatic (fieldCP "I_UNDEFINED" "java.awt.BufferCapabilities$FlipContents" int)))
                                      (4 (iconst_1))
                                      (5 (putstatic (fieldCP "I_BACKGROUND" "java.awt.BufferCapabilities$FlipContents" int)))
                                      (8 (iconst_2))
                                      (9 (putstatic (fieldCP "I_PRIOR" "java.awt.BufferCapabilities$FlipContents" int)))
                                      (12 (iconst_3))
                                      (13 (putstatic (fieldCP "I_COPIED" "java.awt.BufferCapabilities$FlipContents" int)))
                                      (16 (iconst_4))
                                      (17 (anewarray (class "java.lang.String")))
                                      (20 (dup))
                                      (21 (iconst_0))
                                      (22 (ldc 0))        ;;STRING:: "undefined"
                                      (24 (aastore))
                                      (25 (dup))
                                      (26 (iconst_1))
                                      (27 (ldc 1))        ;;STRING:: "background"
                                      (29 (aastore))
                                      (30 (dup))
                                      (31 (iconst_2))
                                      (32 (ldc 2))        ;;STRING:: "prior"
                                      (34 (aastore))
                                      (35 (dup))
                                      (36 (iconst_3))
                                      (37 (ldc 3))        ;;STRING:: "copied"
                                      (39 (aastore))
                                      (40 (putstatic (fieldCP "NAMES" "java.awt.BufferCapabilities$FlipContents" (array (class "java.lang.String")))))
                                      (43 (new (class "java.awt.BufferCapabilities$FlipContents")))
                                      (46 (dup))
                                      (47 (getstatic (fieldCP "I_UNDEFINED" "java.awt.BufferCapabilities$FlipContents" int)))
                                      (50 (invokespecial
					(methodCP "<init>" "java.awt.BufferCapabilities$FlipContents" (int) void)))
                                      (53 (putstatic (fieldCP "UNDEFINED" "java.awt.BufferCapabilities$FlipContents" (class "java.awt.BufferCapabilities$FlipContents"))))
                                      (56 (new (class "java.awt.BufferCapabilities$FlipContents")))
                                      (59 (dup))
                                      (60 (getstatic (fieldCP "I_BACKGROUND" "java.awt.BufferCapabilities$FlipContents" int)))
                                      (63 (invokespecial
					(methodCP "<init>" "java.awt.BufferCapabilities$FlipContents" (int) void)))
                                      (66 (putstatic (fieldCP "BACKGROUND" "java.awt.BufferCapabilities$FlipContents" (class "java.awt.BufferCapabilities$FlipContents"))))
                                      (69 (new (class "java.awt.BufferCapabilities$FlipContents")))
                                      (72 (dup))
                                      (73 (getstatic (fieldCP "I_PRIOR" "java.awt.BufferCapabilities$FlipContents" int)))
                                      (76 (invokespecial
					(methodCP "<init>" "java.awt.BufferCapabilities$FlipContents" (int) void)))
                                      (79 (putstatic (fieldCP "PRIOR" "java.awt.BufferCapabilities$FlipContents" (class "java.awt.BufferCapabilities$FlipContents"))))
                                      (82 (new (class "java.awt.BufferCapabilities$FlipContents")))
                                      (85 (dup))
                                      (86 (getstatic (fieldCP "I_COPIED" "java.awt.BufferCapabilities$FlipContents" int)))
                                      (89 (invokespecial
					(methodCP "<init>" "java.awt.BufferCapabilities$FlipContents" (int) void)))
                                      (92 (putstatic (fieldCP "COPIED" "java.awt.BufferCapabilities$FlipContents" (class "java.awt.BufferCapabilities$FlipContents"))))
                                      (95 (return))
                                      (endofcode 96))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *BufferCapabilities$FlipContents-class-table*
  (make-static-class-decls 
   *java.awt.BufferCapabilities$FlipContents*))

(defconst *package-name-map* 
  ("java.awt.BufferCapabilities$FlipContents" . "java.awt"))
