; Preferences$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.prefs.Preferences$1*
 (make-class-def
      '(class "java.util.prefs.Preferences$1"
            "java.lang.Object"
            (constant_pool
                        (STRING  "java.util.prefs.PreferencesFactory"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
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
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "java.util.prefs.PreferencesFactory"
                                      (2 (invokestatic
					(methodCP "getProperty" "java.lang.System" ((class "java.lang.String")) (class "java.lang.String"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "run" "java.util.prefs.Preferences$1" () (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Preferences$1-class-table*
  (make-static-class-decls 
   *java.util.prefs.Preferences$1*))

(defconst *package-name-map* 
  ("java.util.prefs.Preferences$1" . "java.util.prefs"))

