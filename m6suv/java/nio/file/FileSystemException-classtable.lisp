; FileSystemException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.FileSystemException*
 (make-class-def
      '(class "java.nio.file.FileSystemException"
            "java.io.IOException"
            (constant_pool
                        (LONG -3055425747967319812)
                        (STRING  " -> ")
                        (STRING  ": "))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *static* ) 0)
                        (field "file" (class "java.lang.String") (accessflags  *class*  *final*  *private* ) -1)
                        (field "other" (class "java.lang.String") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aconst_null))
                                      (2 (checkcast (class "java.lang.String")))
                                      (5 (invokespecial
					(methodCP "<init>" "java.io.IOException" ((class "java.lang.String")) void)))
                                      (8 (aload_0))
                                      (9 (aload_1))
                                      (10 (putfield (fieldCP "file" "java.nio.file.FileSystemException" (class "java.lang.String"))))
                                      (13 (aload_0))
                                      (14 (aconst_null))
                                      (15 (putfield (fieldCP "other" "java.nio.file.FileSystemException" (class "java.lang.String"))))
                                      (18 (return))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_3))
                                      (2 (invokespecial
					(methodCP "<init>" "java.io.IOException" ((class "java.lang.String")) void)))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (putfield (fieldCP "file" "java.nio.file.FileSystemException" (class "java.lang.String"))))
                                      (10 (aload_0))
                                      (11 (aload_2))
                                      (12 (putfield (fieldCP "other" "java.nio.file.FileSystemException" (class "java.lang.String"))))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFile"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "file" "java.nio.file.FileSystemException" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getOtherFile"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "other" "java.nio.file.FileSystemException" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getReason"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "getMessage" "java.io.IOException" () (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMessage"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 94)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "file" "java.nio.file.FileSystemException" (class "java.lang.String")))) 
                                      (4 (ifnonnull 19)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "other" "java.nio.file.FileSystemException" (class "java.lang.String")))) 
                                      (11 (ifnonnull 19)) ;;to TAG_0
                                      (14 (aload_0)) 
                                      (15 (invokevirtual (methodCP "getReason" "java.nio.file.FileSystemException" () (class "java.lang.String")))) 
                                      (18 (areturn)) 
                                      (19 (new (class "java.lang.StringBuilder"))) ;;at TAG_0
                                      (22 (dup)) 
                                      (23 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (26 (astore_1)) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "file" "java.nio.file.FileSystemException" (class "java.lang.String")))) 
                                      (31 (ifnull 43)) ;;to TAG_1
                                      (34 (aload_1)) 
                                      (35 (aload_0)) 
                                      (36 (getfield (fieldCP "file" "java.nio.file.FileSystemException" (class "java.lang.String")))) 
                                      (39 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (42 (pop)) 
                                      (43 (aload_0)) ;;at TAG_1
                                      (44 (getfield (fieldCP "other" "java.nio.file.FileSystemException" (class "java.lang.String")))) 
                                      (47 (ifnull 66))  ;;to TAG_2
                                      (50 (aload_1)) 
                                      (51 (ldc 1)) ;;STRING:: " -> "
                                      (53 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (56 (pop)) 
                                      (57 (aload_1)) 
                                      (58 (aload_0)) 
                                      (59 (getfield (fieldCP "other" "java.nio.file.FileSystemException" (class "java.lang.String")))) 
                                      (62 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (65 (pop)) 
                                      (66 (aload_0)) ;;at TAG_2
                                      (67 (invokevirtual (methodCP "getReason" "java.nio.file.FileSystemException" () (class "java.lang.String")))) 
                                      (70 (ifnull 89)) ;;to TAG_3
                                      (73 (aload_1)) 
                                      (74 (ldc 2)) ;;STRING:: ": "
                                      (76 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (79 (pop)) 
                                      (80 (aload_1)) 
                                      (81 (aload_0)) 
                                      (82 (invokevirtual (methodCP "getReason" "java.nio.file.FileSystemException" () (class "java.lang.String")))) 
                                      (85 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (88 (pop)) 
                                      (89 (aload_1)) ;;at TAG_3
                                      (90 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (93 (areturn)) 
                                      (endofcode 94))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *FileSystemException-class-table*
  (make-static-class-decls 
   *java.nio.file.FileSystemException*))

(defconst *package-name-map* 
  ("java.nio.file.FileSystemException" . "java.nio.file"))

