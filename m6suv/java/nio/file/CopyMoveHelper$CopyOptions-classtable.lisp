; CopyMoveHelper$CopyOptions-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.CopyMoveHelper$CopyOptions*
 (make-class-def
      '(class "java.nio.file.CopyMoveHelper$CopyOptions"
            "java.lang.Object"
            (constant_pool
                        (STRING  "\n")
                        (STRING  "\n is not a recognized copy option"))
            (fields
                        (field "replaceExisting" boolean (accessflags  *class* ) -1)
                        (field "copyAttributes" boolean (accessflags  *class* ) -1)
                        (field "followLinks" boolean (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "replaceExisting" "java.nio.file.CopyMoveHelper$CopyOptions" boolean)))
                                      (9 (aload_0))
                                      (10 (iconst_0))
                                      (11 (putfield (fieldCP "copyAttributes" "java.nio.file.CopyMoveHelper$CopyOptions" boolean)))
                                      (14 (aload_0))
                                      (15 (iconst_1))
                                      (16 (putfield (fieldCP "followLinks" "java.nio.file.CopyMoveHelper$CopyOptions" boolean)))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "parse"
                              (parameters (array (class "java.nio.file.CopyOption")))
                              (returntype . (class "java.nio.file.CopyMoveHelper$CopyOptions"))
                              (accessflags  *class*  *static*  *transient* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 130)
                                   (parsedcode
                                      (0 (new (class "java.nio.file.CopyMoveHelper$CopyOptions"))) 
                                      (3 (dup)) 
                                      (4 (invokespecial (methodCP "<init>" "java.nio.file.CopyMoveHelper$CopyOptions" () void))) 
                                      (7 (astore_1)) 
                                      (8 (aload_0)) 
                                      (9 (astore_2)) 
                                      (10 (aload_2)) 
                                      (11 (arraylength)) 
                                      (12 (istore_3)) 
                                      (13 (iconst_0)) 
                                      (14 (istore 4)) 
                                      (16 (iload 4)) ;;at TAG_6
                                      (18 (iload_3)) 
                                      (19 (if_icmpge 128)) ;;to TAG_0
                                      (22 (aload_2)) 
                                      (23 (iload 4)) 
                                      (25 (aaload)) 
                                      (26 (astore 5)) 
                                      (28 (aload 5)) 
                                      (30 (getstatic (fieldCP "REPLACE_EXISTING" "java.nio.file.StandardCopyOption" (class "java.nio.file.StandardCopyOption")))) 
                                      (33 (if_acmpne 44)) ;;to TAG_1
                                      (36 (aload_1)) 
                                      (37 (iconst_1)) 
                                      (38 (putfield (fieldCP "replaceExisting" "java.nio.file.CopyMoveHelper$CopyOptions" boolean))) 
                                      (41 (goto 122))  ;;to TAG_2
                                      (44 (aload 5)) ;;at TAG_1
                                      (46 (getstatic (fieldCP "NOFOLLOW_LINKS" "java.nio.file.LinkOption" (class "java.nio.file.LinkOption")))) 
                                      (49 (if_acmpne 60)) ;;to TAG_3
                                      (52 (aload_1)) 
                                      (53 (iconst_0)) 
                                      (54 (putfield (fieldCP "followLinks" "java.nio.file.CopyMoveHelper$CopyOptions" boolean))) 
                                      (57 (goto 122))  ;;to TAG_2
                                      (60 (aload 5)) ;;at TAG_3
                                      (62 (getstatic (fieldCP "COPY_ATTRIBUTES" "java.nio.file.StandardCopyOption" (class "java.nio.file.StandardCopyOption")))) 
                                      (65 (if_acmpne 76)) ;;to TAG_4
                                      (68 (aload_1)) 
                                      (69 (iconst_1)) 
                                      (70 (putfield (fieldCP "copyAttributes" "java.nio.file.CopyMoveHelper$CopyOptions" boolean))) 
                                      (73 (goto 122))  ;;to TAG_2
                                      (76 (aload 5)) ;;at TAG_4
                                      (78 (ifnonnull 89)) ;;to TAG_5
                                      (81 (new (class "java.lang.NullPointerException"))) 
                                      (84 (dup)) 
                                      (85 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (88 (athrow)) 
                                      (89 (new (class "java.lang.UnsupportedOperationException"))) ;;at TAG_5
                                      (92 (dup)) 
                                      (93 (new (class "java.lang.StringBuilder"))) 
                                      (96 (dup)) 
                                      (97 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (100 (ldc 0)) ;;STRING:: "\n"
                                      (102 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (105 (aload 5)) 
                                      (107 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (110 (ldc 1)) ;;STRING:: "\n is not a recognized copy option"
                                      (112 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (115 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (118 (invokespecial (methodCP "<init>" "java.lang.UnsupportedOperationException" ((class "java.lang.String")) void))) 
                                      (121 (athrow)) 
                                      (122 (iinc 4 1)) ;;at TAG_2
                                      (125 (goto 16)) ;;to TAG_6
                                      (128 (aload_1)) ;;at TAG_0
                                      (129 (areturn)) 
                                      (endofcode 130))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *CopyMoveHelper$CopyOptions-class-table*
  (make-static-class-decls 
   *java.nio.file.CopyMoveHelper$CopyOptions*))

(defconst *package-name-map* 
  ("java.nio.file.CopyMoveHelper$CopyOptions" . "java.nio.file"))
