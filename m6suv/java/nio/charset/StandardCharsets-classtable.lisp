; StandardCharsets-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.charset.StandardCharsets*
 (make-class-def
      '(class "java.nio.charset.StandardCharsets"
            "java.lang.Object"
            (constant_pool
                        (STRING  "No java.nio.charset.StandardCharsets instances for you!")
                        (STRING  "US-ASCII")
                        (STRING  "ISO-8859-1")
                        (STRING  "UTF-8")
                        (STRING  "UTF-16BE")
                        (STRING  "UTF-16LE")
                        (STRING  "UTF-16"))
            (fields
                        (field "US_ASCII" (class "java.nio.charset.Charset") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "ISO_8859_1" (class "java.nio.charset.Charset") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "UTF_8" (class "java.nio.charset.Charset") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "UTF_16BE" (class "java.nio.charset.Charset") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "UTF_16LE" (class "java.nio.charset.Charset") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "UTF_16" (class "java.nio.charset.Charset") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (new (class "java.lang.AssertionError")))
                                      (7 (dup))
                                      (8 (ldc 0))         ;;STRING:: "No java.nio.charset.StandardCharsets instances for you!"
                                      (10 (invokespecial
					(methodCP "<init>" "java.lang.AssertionError" ((class "java.lang.Object")) void)))
                                      (13 (athrow))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 49)
                                   (parsedcode
                                      (0 (ldc 1))         ;;STRING:: "US-ASCII"
                                      (2 (invokestatic
					(methodCP "forName" "java.nio.charset.Charset" ((class "java.lang.String")) (class "java.nio.charset.Charset"))))
                                      (5 (putstatic (fieldCP "US_ASCII" "java.nio.charset.StandardCharsets" (class "java.nio.charset.Charset"))))
                                      (8 (ldc 2))         ;;STRING:: "ISO-8859-1"
                                      (10 (invokestatic
					(methodCP "forName" "java.nio.charset.Charset" ((class "java.lang.String")) (class "java.nio.charset.Charset"))))
                                      (13 (putstatic (fieldCP "ISO_8859_1" "java.nio.charset.StandardCharsets" (class "java.nio.charset.Charset"))))
                                      (16 (ldc 3))        ;;STRING:: "UTF-8"
                                      (18 (invokestatic
					(methodCP "forName" "java.nio.charset.Charset" ((class "java.lang.String")) (class "java.nio.charset.Charset"))))
                                      (21 (putstatic (fieldCP "UTF_8" "java.nio.charset.StandardCharsets" (class "java.nio.charset.Charset"))))
                                      (24 (ldc 4))        ;;STRING:: "UTF-16BE"
                                      (26 (invokestatic
					(methodCP "forName" "java.nio.charset.Charset" ((class "java.lang.String")) (class "java.nio.charset.Charset"))))
                                      (29 (putstatic (fieldCP "UTF_16BE" "java.nio.charset.StandardCharsets" (class "java.nio.charset.Charset"))))
                                      (32 (ldc 5))        ;;STRING:: "UTF-16LE"
                                      (34 (invokestatic
					(methodCP "forName" "java.nio.charset.Charset" ((class "java.lang.String")) (class "java.nio.charset.Charset"))))
                                      (37 (putstatic (fieldCP "UTF_16LE" "java.nio.charset.StandardCharsets" (class "java.nio.charset.Charset"))))
                                      (40 (ldc 6))        ;;STRING:: "UTF-16"
                                      (42 (invokestatic
					(methodCP "forName" "java.nio.charset.Charset" ((class "java.lang.String")) (class "java.nio.charset.Charset"))))
                                      (45 (putstatic (fieldCP "UTF_16" "java.nio.charset.StandardCharsets" (class "java.nio.charset.Charset"))))
                                      (48 (return))
                                      (endofcode 49))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *StandardCharsets-class-table*
  (make-static-class-decls 
   *java.nio.charset.StandardCharsets*))

(defconst *package-name-map* 
  ("java.nio.charset.StandardCharsets" . "java.nio.charset"))

