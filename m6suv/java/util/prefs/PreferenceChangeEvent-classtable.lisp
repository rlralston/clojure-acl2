; PreferenceChangeEvent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.prefs.PreferenceChangeEvent*
 (make-class-def
      '(class "java.util.prefs.PreferenceChangeEvent"
            "java.util.EventObject"
            (constant_pool
                        (LONG 793724513368024975)
                        (STRING  "Not serializable."))
            (fields
                        (field "key" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "newValue" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.prefs.Preferences") (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.EventObject" ((class "java.lang.Object")) void)))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "key" "java.util.prefs.PreferenceChangeEvent" (class "java.lang.String"))))
                                      (10 (aload_0))
                                      (11 (aload_3))
                                      (12 (putfield (fieldCP "newValue" "java.util.prefs.PreferenceChangeEvent" (class "java.lang.String"))))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getNode"
                              (parameters )
                              (returntype . (class "java.util.prefs.Preferences"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getSource" "java.util.prefs.PreferenceChangeEvent" () (class "java.lang.Object"))))
                                      (4 (checkcast (class "java.util.prefs.Preferences")))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getKey"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "key" "java.util.prefs.PreferenceChangeEvent" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getNewValue"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "newValue" "java.util.prefs.PreferenceChangeEvent" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writeObject"
                              (parameters (class "java.io.ObjectOutputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (new (class "java.io.NotSerializableException")))
                                      (3 (dup))
                                      (4 (ldc 1))         ;;STRING:: "Not serializable."
                                      (6 (invokespecial
					(methodCP "<init>" "java.io.NotSerializableException" ((class "java.lang.String")) void)))
                                      (9 (athrow))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readObject"
                              (parameters (class "java.io.ObjectInputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (new (class "java.io.NotSerializableException")))
                                      (3 (dup))
                                      (4 (ldc 1))         ;;STRING:: "Not serializable."
                                      (6 (invokespecial
					(methodCP "<init>" "java.io.NotSerializableException" ((class "java.lang.String")) void)))
                                      (9 (athrow))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *PreferenceChangeEvent-class-table*
  (make-static-class-decls 
   *java.util.prefs.PreferenceChangeEvent*))

(defconst *package-name-map* 
  ("java.util.prefs.PreferenceChangeEvent" . "java.util.prefs"))
