; ProcessBuilder$Redirect$5-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.ProcessBuilder$Redirect$5*
 (make-class-def
      '(class "java.lang.ProcessBuilder$Redirect$5"
            "java.lang.ProcessBuilder$Redirect"
            (constant_pool
                        (STRING  "redirect to append to file \"")
                        (STRING  "\""))
            (fields
                        (field "val$file" (class "java.io.File") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.File"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$file" "java.lang.ProcessBuilder$Redirect$5" (class "java.io.File"))))
                                      (5 (aload_0))
                                      (6 (aconst_null))
                                      (7 (invokespecial
					(methodCP "<init>" "java.lang.ProcessBuilder$Redirect" ((class "java.lang.ProcessBuilder$1")) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "type"
                              (parameters )
                              (returntype . (class "java.lang.ProcessBuilder$Redirect$Type"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "APPEND" "java.lang.ProcessBuilder$Redirect$Type" (class "java.lang.ProcessBuilder$Redirect$Type"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "file"
                              (parameters )
                              (returntype . (class "java.io.File"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$file" "java.lang.ProcessBuilder$Redirect$5" (class "java.io.File"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 28)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (ldc 0))         ;;STRING:: "redirect to append to file \""
                                      (9 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "val$file" "java.lang.ProcessBuilder$Redirect$5" (class "java.io.File"))))
                                      (16 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (19 (ldc 1))        ;;STRING:: "\""
                                      (21 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (24 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (27 (areturn))
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "append"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *ProcessBuilder$Redirect$5-class-table*
  (make-static-class-decls 
   *java.lang.ProcessBuilder$Redirect$5*))

(defconst *package-name-map* 
  ("java.lang.ProcessBuilder$Redirect$5" . "java.lang"))

