; LinkOption-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.LinkOption*
 (make-class-def
      '(class "java.nio.file.LinkOption"
            "java.lang.Enum"
            (constant_pool
                        (STRING  "NOFOLLOW_LINKS"))
            (fields
                        (field "NOFOLLOW_LINKS" (class "java.nio.file.LinkOption") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "$VALUES" (array (class "java.nio.file.LinkOption")) (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "values"
                              (parameters )
                              (returntype . (array (class "java.nio.file.LinkOption")))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 10)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$VALUES" "java.nio.file.LinkOption" (array (class "java.nio.file.LinkOption")))))
                                      (3 (invokevirtual
					(methodCP "clone" "java.nio.file.LinkOption[]" () (class "java.lang.Object"))))
                                      (6 (checkcast (array (class "java.nio.file.LinkOption"))))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "valueOf"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.nio.file.LinkOption"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (ldc_w ))
                                      (3 (aload_0))
                                      (4 (invokestatic
					(methodCP "valueOf" "java.lang.Enum" ((class "java.lang.Class") (class "java.lang.String")) (class "java.lang.Enum"))))
                                      (7 (checkcast (class "java.nio.file.LinkOption")))
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (new (class "java.nio.file.LinkOption")))
                                      (3 (dup))
                                      (4 (ldc 0))         ;;STRING:: "NOFOLLOW_LINKS"
                                      (6 (iconst_0))
                                      (7 (invokespecial
					(methodCP "<init>" "java.nio.file.LinkOption" ((class "java.lang.String") int) void)))
                                      (10 (putstatic (fieldCP "NOFOLLOW_LINKS" "java.nio.file.LinkOption" (class "java.nio.file.LinkOption"))))
                                      (13 (iconst_1))
                                      (14 (anewarray (class "java.nio.file.LinkOption")))
                                      (17 (dup))
                                      (18 (iconst_0))
                                      (19 (getstatic (fieldCP "NOFOLLOW_LINKS" "java.nio.file.LinkOption" (class "java.nio.file.LinkOption"))))
                                      (22 (aastore))
                                      (23 (putstatic (fieldCP "$VALUES" "java.nio.file.LinkOption" (array (class "java.nio.file.LinkOption")))))
                                      (26 (return))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.nio.file.OpenOption" "java.nio.file.CopyOption")
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *LinkOption-class-table*
  (make-static-class-decls 
   *java.nio.file.LinkOption*))

(defconst *package-name-map* 
  ("java.nio.file.LinkOption" . "java.nio.file"))

