; EventQueue$5-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.EventQueue$5*
 (make-class-def
      '(class "java.awt.EventQueue$5"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.awt.EventQueue") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.EventQueue"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.EventQueue$5" (class "java.awt.EventQueue"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.awt.EventDispatchThread"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 50)
                                   (parsedcode
                                      (0 (new (class "java.awt.EventDispatchThread")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "this$0" "java.awt.EventQueue$5" (class "java.awt.EventQueue"))))
                                      (8 (invokestatic
					(methodCP "access$200" "java.awt.EventQueue" ((class "java.awt.EventQueue")) (class "java.lang.ThreadGroup"))))
                                      (11 (aload_0))
                                      (12 (getfield (fieldCP "this$0" "java.awt.EventQueue$5" (class "java.awt.EventQueue"))))
                                      (15 (invokestatic
					(methodCP "access$300" "java.awt.EventQueue" ((class "java.awt.EventQueue")) (class "java.lang.String"))))
                                      (18 (aload_0))
                                      (19 (getfield (fieldCP "this$0" "java.awt.EventQueue$5" (class "java.awt.EventQueue"))))
                                      (22 (invokespecial
					(methodCP "<init>" "java.awt.EventDispatchThread" ((class "java.lang.ThreadGroup") (class "java.lang.String") (class "java.awt.EventQueue")) void)))
                                      (25 (astore_1))
                                      (26 (aload_1))
                                      (27 (aload_0))
                                      (28 (getfield (fieldCP "this$0" "java.awt.EventQueue$5" (class "java.awt.EventQueue"))))
                                      (31 (invokestatic
					(methodCP "access$400" "java.awt.EventQueue" ((class "java.awt.EventQueue")) (class "java.lang.ClassLoader"))))
                                      (34 (invokevirtual
					(methodCP "setContextClassLoader" "java.awt.EventDispatchThread" ((class "java.lang.ClassLoader")) void)))
                                      (37 (aload_1))
                                      (38 (bipush 6))
                                      (40 (invokevirtual
					(methodCP "setPriority" "java.awt.EventDispatchThread" (int) void)))
                                      (43 (aload_1))
                                      (44 (iconst_0))
                                      (45 (invokevirtual
					(methodCP "setDaemon" "java.awt.EventDispatchThread" (boolean) void)))
                                      (48 (aload_1))
                                      (49 (areturn))
                                      (endofcode 50))
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
					(methodCP "run" "java.awt.EventQueue$5" () (class "java.awt.EventDispatchThread"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *EventQueue$5-class-table*
  (make-static-class-decls 
   *java.awt.EventQueue$5*))

(defconst *package-name-map* 
  ("java.awt.EventQueue$5" . "java.awt"))

