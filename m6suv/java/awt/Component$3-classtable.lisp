; Component$3-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:24 CDT 2014.
;

(defconst *java.awt.Component$3*
 (make-class-def
      '(class "java.awt.Component$3"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$clazz" (class "java.lang.Class") (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "java.awt.Component") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.Component") (class "java.lang.Class"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.Component$3" (class "java.awt.Component"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$clazz" "java.awt.Component$3" (class "java.lang.Class"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Boolean"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$clazz" "java.awt.Component$3" (class "java.lang.Class"))))
                                      (4 (invokestatic
					(methodCP "access$500" "java.awt.Component" ((class "java.lang.Class")) boolean)))
                                      (7 (invokestatic
					(methodCP "valueOf" "java.lang.Boolean" (boolean) (class "java.lang.Boolean"))))
                                      (10 (areturn))
                                      (endofcode 11))
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
					(methodCP "run" "java.awt.Component$3" () (class "java.lang.Boolean"))))
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


(defconst *Component$3-class-table*
  (make-static-class-decls 
   *java.awt.Component$3*))

(defconst *package-name-map* 
  ("java.awt.Component$3" . "java.awt"))
