; JobAttributes$DestinationType-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.JobAttributes$DestinationType*
 (make-class-def
      '(class "java.awt.JobAttributes$DestinationType"
            "java.awt.AttributeValue"
            (constant_pool
                        (INT 0)
                        (INT 1)
                        (STRING  "file")
                        (STRING  "printer"))
            (fields
                        (field "I_FILE" int (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "I_PRINTER" int (accessflags  *class*  *final*  *private*  *static* ) 1)
                        (field "NAMES" (array (class "java.lang.String")) (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "FILE" (class "java.awt.JobAttributes$DestinationType") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "PRINTER" (class "java.awt.JobAttributes$DestinationType") (accessflags  *class*  *final*  *public*  *static* ) -1))
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
                                      (2 (getstatic (fieldCP "NAMES" "java.awt.JobAttributes$DestinationType" (array (class "java.lang.String")))))
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 40)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (anewarray (class "java.lang.String")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (ldc 2))         ;;STRING:: "file"
                                      (8 (aastore))
                                      (9 (dup))
                                      (10 (iconst_1))
                                      (11 (ldc 3))        ;;STRING:: "printer"
                                      (13 (aastore))
                                      (14 (putstatic (fieldCP "NAMES" "java.awt.JobAttributes$DestinationType" (array (class "java.lang.String")))))
                                      (17 (new (class "java.awt.JobAttributes$DestinationType")))
                                      (20 (dup))
                                      (21 (iconst_0))
                                      (22 (invokespecial
					(methodCP "<init>" "java.awt.JobAttributes$DestinationType" (int) void)))
                                      (25 (putstatic (fieldCP "FILE" "java.awt.JobAttributes$DestinationType" (class "java.awt.JobAttributes$DestinationType"))))
                                      (28 (new (class "java.awt.JobAttributes$DestinationType")))
                                      (31 (dup))
                                      (32 (iconst_1))
                                      (33 (invokespecial
					(methodCP "<init>" "java.awt.JobAttributes$DestinationType" (int) void)))
                                      (36 (putstatic (fieldCP "PRINTER" "java.awt.JobAttributes$DestinationType" (class "java.awt.JobAttributes$DestinationType"))))
                                      (39 (return))
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *JobAttributes$DestinationType-class-table*
  (make-static-class-decls 
   *java.awt.JobAttributes$DestinationType*))

(defconst *package-name-map* 
  ("java.awt.JobAttributes$DestinationType" . "java.awt"))

