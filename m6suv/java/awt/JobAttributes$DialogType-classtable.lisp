; JobAttributes$DialogType-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.JobAttributes$DialogType*
 (make-class-def
      '(class "java.awt.JobAttributes$DialogType"
            "java.awt.AttributeValue"
            (constant_pool
                        (INT 0)
                        (INT 1)
                        (INT 2)
                        (STRING  "common")
                        (STRING  "native")
                        (STRING  "none"))
            (fields
                        (field "I_COMMON" int (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "I_NATIVE" int (accessflags  *class*  *final*  *private*  *static* ) 1)
                        (field "I_NONE" int (accessflags  *class*  *final*  *private*  *static* ) 2)
                        (field "NAMES" (array (class "java.lang.String")) (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "COMMON" (class "java.awt.JobAttributes$DialogType") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "NATIVE" (class "java.awt.JobAttributes$DialogType") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "NONE" (class "java.awt.JobAttributes$DialogType") (accessflags  *class*  *final*  *public*  *static* ) -1))
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
                                      (2 (getstatic (fieldCP "NAMES" "java.awt.JobAttributes$DialogType" (array (class "java.lang.String")))))
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 56)
                                   (parsedcode
                                      (0 (iconst_3))
                                      (1 (anewarray (class "java.lang.String")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (ldc 3))         ;;STRING:: "common"
                                      (8 (aastore))
                                      (9 (dup))
                                      (10 (iconst_1))
                                      (11 (ldc 4))        ;;STRING:: "native"
                                      (13 (aastore))
                                      (14 (dup))
                                      (15 (iconst_2))
                                      (16 (ldc 5))        ;;STRING:: "none"
                                      (18 (aastore))
                                      (19 (putstatic (fieldCP "NAMES" "java.awt.JobAttributes$DialogType" (array (class "java.lang.String")))))
                                      (22 (new (class "java.awt.JobAttributes$DialogType")))
                                      (25 (dup))
                                      (26 (iconst_0))
                                      (27 (invokespecial
					(methodCP "<init>" "java.awt.JobAttributes$DialogType" (int) void)))
                                      (30 (putstatic (fieldCP "COMMON" "java.awt.JobAttributes$DialogType" (class "java.awt.JobAttributes$DialogType"))))
                                      (33 (new (class "java.awt.JobAttributes$DialogType")))
                                      (36 (dup))
                                      (37 (iconst_1))
                                      (38 (invokespecial
					(methodCP "<init>" "java.awt.JobAttributes$DialogType" (int) void)))
                                      (41 (putstatic (fieldCP "NATIVE" "java.awt.JobAttributes$DialogType" (class "java.awt.JobAttributes$DialogType"))))
                                      (44 (new (class "java.awt.JobAttributes$DialogType")))
                                      (47 (dup))
                                      (48 (iconst_2))
                                      (49 (invokespecial
					(methodCP "<init>" "java.awt.JobAttributes$DialogType" (int) void)))
                                      (52 (putstatic (fieldCP "NONE" "java.awt.JobAttributes$DialogType" (class "java.awt.JobAttributes$DialogType"))))
                                      (55 (return))
                                      (endofcode 56))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *JobAttributes$DialogType-class-table*
  (make-static-class-decls 
   *java.awt.JobAttributes$DialogType*))

(defconst *package-name-map* 
  ("java.awt.JobAttributes$DialogType" . "java.awt"))
