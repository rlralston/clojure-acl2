; Authenticator$RequestorType-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.Authenticator$RequestorType*
 (make-class-def
      '(class "java.net.Authenticator$RequestorType"
            "java.lang.Enum"
            (constant_pool
                        (STRING  "PROXY")
                        (STRING  "SERVER"))
            (fields
                        (field "PROXY" (class "java.net.Authenticator$RequestorType") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "SERVER" (class "java.net.Authenticator$RequestorType") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "$VALUES" (array (class "java.net.Authenticator$RequestorType")) (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "values"
                              (parameters )
                              (returntype . (array (class "java.net.Authenticator$RequestorType")))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 10)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$VALUES" "java.net.Authenticator$RequestorType" (array (class "java.net.Authenticator$RequestorType")))))
                                      (3 (invokevirtual
					(methodCP "clone" "java.net.Authenticator$RequestorType[]" () (class "java.lang.Object"))))
                                      (6 (checkcast (array (class "java.net.Authenticator$RequestorType"))))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "valueOf"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.net.Authenticator$RequestorType"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (ldc_w ))
                                      (3 (aload_0))
                                      (4 (invokestatic
					(methodCP "valueOf" "java.lang.Enum" ((class "java.lang.Class") (class "java.lang.String")) (class "java.lang.Enum"))))
                                      (7 (checkcast (class "java.net.Authenticator$RequestorType")))
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 46)
                                   (parsedcode
                                      (0 (new (class "java.net.Authenticator$RequestorType")))
                                      (3 (dup))
                                      (4 (ldc 0))         ;;STRING:: "PROXY"
                                      (6 (iconst_0))
                                      (7 (invokespecial
					(methodCP "<init>" "java.net.Authenticator$RequestorType" ((class "java.lang.String") int) void)))
                                      (10 (putstatic (fieldCP "PROXY" "java.net.Authenticator$RequestorType" (class "java.net.Authenticator$RequestorType"))))
                                      (13 (new (class "java.net.Authenticator$RequestorType")))
                                      (16 (dup))
                                      (17 (ldc 1))        ;;STRING:: "SERVER"
                                      (19 (iconst_1))
                                      (20 (invokespecial
					(methodCP "<init>" "java.net.Authenticator$RequestorType" ((class "java.lang.String") int) void)))
                                      (23 (putstatic (fieldCP "SERVER" "java.net.Authenticator$RequestorType" (class "java.net.Authenticator$RequestorType"))))
                                      (26 (iconst_2))
                                      (27 (anewarray (class "java.net.Authenticator$RequestorType")))
                                      (30 (dup))
                                      (31 (iconst_0))
                                      (32 (getstatic (fieldCP "PROXY" "java.net.Authenticator$RequestorType" (class "java.net.Authenticator$RequestorType"))))
                                      (35 (aastore))
                                      (36 (dup))
                                      (37 (iconst_1))
                                      (38 (getstatic (fieldCP "SERVER" "java.net.Authenticator$RequestorType" (class "java.net.Authenticator$RequestorType"))))
                                      (41 (aastore))
                                      (42 (putstatic (fieldCP "$VALUES" "java.net.Authenticator$RequestorType" (array (class "java.net.Authenticator$RequestorType")))))
                                      (45 (return))
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Authenticator$RequestorType-class-table*
  (make-static-class-decls 
   *java.net.Authenticator$RequestorType*))

(defconst *package-name-map* 
  ("java.net.Authenticator$RequestorType" . "java.net"))

