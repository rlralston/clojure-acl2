; ByteOrder-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.ByteOrder*
 (make-class-def
      '(class "java.nio.ByteOrder"
            "java.lang.Object"
            (constant_pool
                        (STRING  "BIG_ENDIAN")
                        (STRING  "LITTLE_ENDIAN"))
            (fields
                        (field "name" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "BIG_ENDIAN" (class "java.nio.ByteOrder") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "LITTLE_ENDIAN" (class "java.nio.ByteOrder") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "name" "java.nio.ByteOrder" (class "java.lang.String"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nativeOrder"
                              (parameters )
                              (returntype . (class "java.nio.ByteOrder"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "byteOrder" "java.nio.Bits" () (class "java.nio.ByteOrder"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "name" "java.nio.ByteOrder" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 0) (code_length . 25)
                                   (parsedcode
                                      (0 (new (class "java.nio.ByteOrder")))
                                      (3 (dup))
                                      (4 (ldc 0))         ;;STRING:: "BIG_ENDIAN"
                                      (6 (invokespecial
					(methodCP "<init>" "java.nio.ByteOrder" ((class "java.lang.String")) void)))
                                      (9 (putstatic (fieldCP "BIG_ENDIAN" "java.nio.ByteOrder" (class "java.nio.ByteOrder"))))
                                      (12 (new (class "java.nio.ByteOrder")))
                                      (15 (dup))
                                      (16 (ldc 1))        ;;STRING:: "LITTLE_ENDIAN"
                                      (18 (invokespecial
					(methodCP "<init>" "java.nio.ByteOrder" ((class "java.lang.String")) void)))
                                      (21 (putstatic (fieldCP "LITTLE_ENDIAN" "java.nio.ByteOrder" (class "java.nio.ByteOrder"))))
                                      (24 (return))
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ByteOrder-class-table*
  (make-static-class-decls 
   *java.nio.ByteOrder*))

(defconst *package-name-map* 
  ("java.nio.ByteOrder" . "java.nio"))
