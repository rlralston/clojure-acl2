; TimeZone$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.TimeZone$1*
 (make-class-def
      '(class "java.util.TimeZone$1"
            "java.lang.Object"
            (constant_pool
                        (STRING  "user.timezone"))
            (fields
                        (field "val$id" (class "java.lang.String") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$id" "java.util.TimeZone$1" (class "java.lang.String"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "user.timezone"
                                      (2 (aload_0))
                                      (3 (getfield (fieldCP "val$id" "java.util.TimeZone$1" (class "java.lang.String"))))
                                      (6 (invokestatic
					(methodCP "setProperty" "java.lang.System" ((class "java.lang.String") (class "java.lang.String")) (class "java.lang.String"))))
                                      (9 (pop))
                                      (10 (aconst_null))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *TimeZone$1-class-table*
  (make-static-class-decls 
   *java.util.TimeZone$1*))

(defconst *package-name-map* 
  ("java.util.TimeZone$1" . "java.util"))
