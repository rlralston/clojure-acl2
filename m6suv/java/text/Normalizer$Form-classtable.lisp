; Normalizer$Form-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.text.Normalizer$Form*
 (make-class-def
      '(class "java.text.Normalizer$Form"
            "java.lang.Enum"
            (constant_pool
                        (STRING  "NFD")
                        (STRING  "NFC")
                        (STRING  "NFKD")
                        (STRING  "NFKC"))
            (fields
                        (field "NFD" (class "java.text.Normalizer$Form") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "NFC" (class "java.text.Normalizer$Form") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "NFKD" (class "java.text.Normalizer$Form") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "NFKC" (class "java.text.Normalizer$Form") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "$VALUES" (array (class "java.text.Normalizer$Form")) (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "values"
                              (parameters )
                              (returntype . (array (class "java.text.Normalizer$Form")))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 10)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$VALUES" "java.text.Normalizer$Form" (array (class "java.text.Normalizer$Form")))))
                                      (3 (invokevirtual
					(methodCP "clone" "java.text.Normalizer$Form[]" () (class "java.lang.Object"))))
                                      (6 (checkcast (array (class "java.text.Normalizer$Form"))))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "valueOf"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.text.Normalizer$Form"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (ldc_w ))
                                      (3 (aload_0))
                                      (4 (invokestatic
					(methodCP "valueOf" "java.lang.Enum" ((class "java.lang.Class") (class "java.lang.String")) (class "java.lang.Enum"))))
                                      (7 (checkcast (class "java.text.Normalizer$Form")))
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 84)
                                   (parsedcode
                                      (0 (new (class "java.text.Normalizer$Form")))
                                      (3 (dup))
                                      (4 (ldc 0))         ;;STRING:: "NFD"
                                      (6 (iconst_0))
                                      (7 (invokespecial
					(methodCP "<init>" "java.text.Normalizer$Form" ((class "java.lang.String") int) void)))
                                      (10 (putstatic (fieldCP "NFD" "java.text.Normalizer$Form" (class "java.text.Normalizer$Form"))))
                                      (13 (new (class "java.text.Normalizer$Form")))
                                      (16 (dup))
                                      (17 (ldc 1))        ;;STRING:: "NFC"
                                      (19 (iconst_1))
                                      (20 (invokespecial
					(methodCP "<init>" "java.text.Normalizer$Form" ((class "java.lang.String") int) void)))
                                      (23 (putstatic (fieldCP "NFC" "java.text.Normalizer$Form" (class "java.text.Normalizer$Form"))))
                                      (26 (new (class "java.text.Normalizer$Form")))
                                      (29 (dup))
                                      (30 (ldc 2))        ;;STRING:: "NFKD"
                                      (32 (iconst_2))
                                      (33 (invokespecial
					(methodCP "<init>" "java.text.Normalizer$Form" ((class "java.lang.String") int) void)))
                                      (36 (putstatic (fieldCP "NFKD" "java.text.Normalizer$Form" (class "java.text.Normalizer$Form"))))
                                      (39 (new (class "java.text.Normalizer$Form")))
                                      (42 (dup))
                                      (43 (ldc 3))        ;;STRING:: "NFKC"
                                      (45 (iconst_3))
                                      (46 (invokespecial
					(methodCP "<init>" "java.text.Normalizer$Form" ((class "java.lang.String") int) void)))
                                      (49 (putstatic (fieldCP "NFKC" "java.text.Normalizer$Form" (class "java.text.Normalizer$Form"))))
                                      (52 (iconst_4))
                                      (53 (anewarray (class "java.text.Normalizer$Form")))
                                      (56 (dup))
                                      (57 (iconst_0))
                                      (58 (getstatic (fieldCP "NFD" "java.text.Normalizer$Form" (class "java.text.Normalizer$Form"))))
                                      (61 (aastore))
                                      (62 (dup))
                                      (63 (iconst_1))
                                      (64 (getstatic (fieldCP "NFC" "java.text.Normalizer$Form" (class "java.text.Normalizer$Form"))))
                                      (67 (aastore))
                                      (68 (dup))
                                      (69 (iconst_2))
                                      (70 (getstatic (fieldCP "NFKD" "java.text.Normalizer$Form" (class "java.text.Normalizer$Form"))))
                                      (73 (aastore))
                                      (74 (dup))
                                      (75 (iconst_3))
                                      (76 (getstatic (fieldCP "NFKC" "java.text.Normalizer$Form" (class "java.text.Normalizer$Form"))))
                                      (79 (aastore))
                                      (80 (putstatic (fieldCP "$VALUES" "java.text.Normalizer$Form" (array (class "java.text.Normalizer$Form")))))
                                      (83 (return))
                                      (endofcode 84))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Normalizer$Form-class-table*
  (make-static-class-decls 
   *java.text.Normalizer$Form*))

(defconst *package-name-map* 
  ("java.text.Normalizer$Form" . "java.text"))
