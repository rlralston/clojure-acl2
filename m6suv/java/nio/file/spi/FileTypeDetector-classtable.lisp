; FileTypeDetector-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.spi.FileTypeDetector*
 (make-class-def
      '(class "java.nio.file.spi.FileTypeDetector"
            "java.lang.Object"
            (constant_pool
                        (STRING  "fileTypeDetector"))
            (fields)
            (methods
                        (method "checkPermission"
                              (parameters )
                              (returntype . (class "java.lang.Void"))
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 23)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getSecurityManager" "java.lang.System" () (class "java.lang.SecurityManager")))) 
                                      (3 (astore_0)) 
                                      (4 (aload_0)) 
                                      (5 (ifnull 21))  ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (new (class "java.lang.RuntimePermission"))) 
                                      (12 (dup)) 
                                      (13 (ldc 0)) ;;STRING:: "fileTypeDetector"
                                      (15 (invokespecial (methodCP "<init>" "java.lang.RuntimePermission" ((class "java.lang.String")) void))) 
                                      (18 (invokevirtual (methodCP "checkPermission" "java.lang.SecurityManager" ((class "java.security.Permission")) void))) 
                                      (21 (aconst_null)) ;;at TAG_0
                                      (22 (areturn)) 
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Void"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokestatic
					(methodCP "checkPermission" "java.nio.file.spi.FileTypeDetector" () (class "java.lang.Void"))))
                                      (4 (invokespecial
					(methodCP "<init>" "java.nio.file.spi.FileTypeDetector" ((class "java.lang.Void")) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "probeContentType"
                              (parameters (class "java.nio.file.Path"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *FileTypeDetector-class-table*
  (make-static-class-decls 
   *java.nio.file.spi.FileTypeDetector*))

(defconst *package-name-map* 
  ("java.nio.file.spi.FileTypeDetector" . "java.nio.file.spi"))

