; Component$5-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:24 CDT 2014.
;

(defconst *java.awt.Component$5*
 (make-class-def
      '(class "java.awt.Component$5"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$method" (class "java.lang.reflect.Method") (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "java.awt.Component") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.Component") (class "java.lang.reflect.Method"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.Component$5" (class "java.awt.Component"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$method" "java.awt.Component$5" (class "java.lang.reflect.Method"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$method" "java.awt.Component$5" (class "java.lang.reflect.Method"))))
                                      (4 (iconst_1))
                                      (5 (invokevirtual
					(methodCP "setAccessible" "java.lang.reflect.Method" (boolean) void)))
                                      (8 (aconst_null))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Component$5-class-table*
  (make-static-class-decls 
   *java.awt.Component$5*))

(defconst *package-name-map* 
  ("java.awt.Component$5" . "java.awt"))
