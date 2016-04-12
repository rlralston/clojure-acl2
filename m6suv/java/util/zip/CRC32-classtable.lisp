; CRC32-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.zip.CRC32*
 (make-class-def
      '(class "java.util.zip.CRC32"
            "java.lang.Object"
            (constant_pool
                        (LONG 4294967295))
            (fields
                        (field "crc" int (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "update"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "crc" "java.util.zip.CRC32" int)))
                                      (5 (iload_1))
                                      (6 (invokestatic
					(methodCP "update" "java.util.zip.CRC32" (int int) int)))
                                      (9 (putfield (fieldCP "crc" "java.util.zip.CRC32" int)))
                                      (12 (return))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "update"
                              (parameters (array byte) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 51)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 12)) ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (iload_2)) ;;at TAG_0
                                      (13 (iflt 28)) ;;to TAG_1
                                      (16 (iload_3)) 
                                      (17 (iflt 28)) ;;to TAG_1
                                      (20 (iload_2)) 
                                      (21 (aload_1)) 
                                      (22 (arraylength)) 
                                      (23 (iload_3)) 
                                      (24 (isub)) 
                                      (25 (if_icmple 36))  ;;to TAG_2
                                      (28 (new (class "java.lang.ArrayIndexOutOfBoundsException"))) ;;at TAG_1
                                      (31 (dup)) 
                                      (32 (invokespecial (methodCP "<init>" "java.lang.ArrayIndexOutOfBoundsException" () void))) 
                                      (35 (athrow)) 
                                      (36 (aload_0)) ;;at TAG_2
                                      (37 (aload_0)) 
                                      (38 (getfield (fieldCP "crc" "java.util.zip.CRC32" int))) 
                                      (41 (aload_1)) 
                                      (42 (iload_2)) 
                                      (43 (iload_3)) 
                                      (44 (invokestatic (methodCP "updateBytes" "java.util.zip.CRC32" (int (array byte) int int) int))) 
                                      (47 (putfield (fieldCP "crc" "java.util.zip.CRC32" int))) 
                                      (50 (return)) 
                                      (endofcode 51))
                                   (Exceptions )
                                   (StackMap )))
                        (method "update"
                              (parameters (array byte))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "crc" "java.util.zip.CRC32" int)))
                                      (5 (aload_1))
                                      (6 (iconst_0))
                                      (7 (aload_1))
                                      (8 (arraylength))
                                      (9 (invokestatic
					(methodCP "updateBytes" "java.util.zip.CRC32" (int (array byte) int int) int)))
                                      (12 (putfield (fieldCP "crc" "java.util.zip.CRC32" int)))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reset"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iconst_0))
                                      (2 (putfield (fieldCP "crc" "java.util.zip.CRC32" int)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getValue"
                              (parameters )
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "crc" "java.util.zip.CRC32" int)))
                                      (4 (i2l))
                                      (5 (ldc2_w 0))      ;; LONG:: "4294967295"
                                      (8 (land))
                                      (9 (lreturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "update"
                              (parameters int int)
                              (returntype . int)
                              (accessflags  *class*  *native*  *private*  *static* )
                              (code))
                        (method "updateBytes"
                              (parameters int (array byte) int int)
                              (returntype . int)
                              (accessflags  *class*  *native*  *private*  *static* )
                              (code)))
            (interfaces "java.util.zip.Checksum")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *CRC32-class-table*
  (make-static-class-decls 
   *java.util.zip.CRC32*))

(defconst *package-name-map* 
  ("java.util.zip.CRC32" . "java.util.zip"))
