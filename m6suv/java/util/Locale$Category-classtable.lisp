; Locale$Category-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.Locale$Category*
 (make-class-def
      '(class "java.util.Locale$Category"
            "java.lang.Enum"
            (constant_pool
                        (STRING  "DISPLAY")
                        (STRING  "user.language.display")
                        (STRING  "user.script.display")
                        (STRING  "user.country.display")
                        (STRING  "user.variant.display")
                        (STRING  "FORMAT")
                        (STRING  "user.language.format")
                        (STRING  "user.script.format")
                        (STRING  "user.country.format")
                        (STRING  "user.variant.format"))
            (fields
                        (field "DISPLAY" (class "java.util.Locale$Category") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "FORMAT" (class "java.util.Locale$Category") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "languageKey" (class "java.lang.String") (accessflags  *class*  *final* ) -1)
                        (field "scriptKey" (class "java.lang.String") (accessflags  *class*  *final* ) -1)
                        (field "countryKey" (class "java.lang.String") (accessflags  *class*  *final* ) -1)
                        (field "variantKey" (class "java.lang.String") (accessflags  *class*  *final* ) -1)
                        (field "$VALUES" (array (class "java.util.Locale$Category")) (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "values"
                              (parameters )
                              (returntype . (array (class "java.util.Locale$Category")))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 10)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$VALUES" "java.util.Locale$Category" (array (class "java.util.Locale$Category")))))
                                      (3 (invokevirtual
					(methodCP "clone" "java.util.Locale$Category[]" () (class "java.lang.Object"))))
                                      (6 (checkcast (array (class "java.util.Locale$Category"))))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "valueOf"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.util.Locale$Category"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (ldc_w ))
                                      (3 (aload_0))
                                      (4 (invokestatic
					(methodCP "valueOf" "java.lang.Enum" ((class "java.lang.Class") (class "java.lang.String")) (class "java.lang.Enum"))))
                                      (7 (checkcast (class "java.util.Locale$Category")))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") int (class "java.lang.String") (class "java.lang.String") (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 7) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.lang.Enum" ((class "java.lang.String") int) void)))
                                      (6 (aload_0))
                                      (7 (aload_3))
                                      (8 (putfield (fieldCP "languageKey" "java.util.Locale$Category" (class "java.lang.String"))))
                                      (11 (aload_0))
                                      (12 (aload 4))
                                      (14 (putfield (fieldCP "scriptKey" "java.util.Locale$Category" (class "java.lang.String"))))
                                      (17 (aload_0))
                                      (18 (aload 5))
                                      (20 (putfield (fieldCP "countryKey" "java.util.Locale$Category" (class "java.lang.String"))))
                                      (23 (aload_0))
                                      (24 (aload 6))
                                      (26 (putfield (fieldCP "variantKey" "java.util.Locale$Category" (class "java.lang.String"))))
                                      (29 (return))
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 8) (max_locals . 0) (code_length . 62)
                                   (parsedcode
                                      (0 (new (class "java.util.Locale$Category")))
                                      (3 (dup))
                                      (4 (ldc 0))         ;;STRING:: "DISPLAY"
                                      (6 (iconst_0))
                                      (7 (ldc 1))         ;;STRING:: "user.language.display"
                                      (9 (ldc 2))         ;;STRING:: "user.script.display"
                                      (11 (ldc 3))        ;;STRING:: "user.country.display"
                                      (13 (ldc 4))        ;;STRING:: "user.variant.display"
                                      (15 (invokespecial
					(methodCP "<init>" "java.util.Locale$Category" ((class "java.lang.String") int (class "java.lang.String") (class "java.lang.String") (class "java.lang.String") (class "java.lang.String")) void)))
                                      (18 (putstatic (fieldCP "DISPLAY" "java.util.Locale$Category" (class "java.util.Locale$Category"))))
                                      (21 (new (class "java.util.Locale$Category")))
                                      (24 (dup))
                                      (25 (ldc 5))        ;;STRING:: "FORMAT"
                                      (27 (iconst_1))
                                      (28 (ldc 6))        ;;STRING:: "user.language.format"
                                      (30 (ldc 7))        ;;STRING:: "user.script.format"
                                      (32 (ldc 8))        ;;STRING:: "user.country.format"
                                      (34 (ldc 9))        ;;STRING:: "user.variant.format"
                                      (36 (invokespecial
					(methodCP "<init>" "java.util.Locale$Category" ((class "java.lang.String") int (class "java.lang.String") (class "java.lang.String") (class "java.lang.String") (class "java.lang.String")) void)))
                                      (39 (putstatic (fieldCP "FORMAT" "java.util.Locale$Category" (class "java.util.Locale$Category"))))
                                      (42 (iconst_2))
                                      (43 (anewarray (class "java.util.Locale$Category")))
                                      (46 (dup))
                                      (47 (iconst_0))
                                      (48 (getstatic (fieldCP "DISPLAY" "java.util.Locale$Category" (class "java.util.Locale$Category"))))
                                      (51 (aastore))
                                      (52 (dup))
                                      (53 (iconst_1))
                                      (54 (getstatic (fieldCP "FORMAT" "java.util.Locale$Category" (class "java.util.Locale$Category"))))
                                      (57 (aastore))
                                      (58 (putstatic (fieldCP "$VALUES" "java.util.Locale$Category" (array (class "java.util.Locale$Category")))))
                                      (61 (return))
                                      (endofcode 62))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Locale$Category-class-table*
  (make-static-class-decls 
   *java.util.Locale$Category*))

(defconst *package-name-map* 
  ("java.util.Locale$Category" . "java.util"))

