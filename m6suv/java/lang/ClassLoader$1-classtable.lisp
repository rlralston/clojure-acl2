; ClassLoader$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.ClassLoader$1*
 (make-class-def
      '(class "java.lang.ClassLoader$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$sm" (class "java.lang.SecurityManager") (accessflags  *class*  *final* ) -1)
                        (field "val$name" (class "java.lang.String") (accessflags  *class*  *final* ) -1)
                        (field "val$i" int (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "java.lang.ClassLoader") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.ClassLoader") (class "java.lang.SecurityManager") (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.lang.ClassLoader$1" (class "java.lang.ClassLoader"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$sm" "java.lang.ClassLoader$1" (class "java.lang.SecurityManager"))))
                                      (10 (aload_0))
                                      (11 (aload_3))
                                      (12 (putfield (fieldCP "val$name" "java.lang.ClassLoader$1" (class "java.lang.String"))))
                                      (15 (aload_0))
                                      (16 (iload 4))
                                      (18 (putfield (fieldCP "val$i" "java.lang.ClassLoader$1" int)))
                                      (21 (aload_0))
                                      (22 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Void"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$sm" "java.lang.ClassLoader$1" (class "java.lang.SecurityManager"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "val$name" "java.lang.ClassLoader$1" (class "java.lang.String"))))
                                      (8 (iconst_0))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "val$i" "java.lang.ClassLoader$1" int)))
                                      (13 (invokevirtual
					(methodCP "substring" "java.lang.String" (int int) (class "java.lang.String"))))
                                      (16 (invokevirtual
					(methodCP "checkPackageAccess" "java.lang.SecurityManager" ((class "java.lang.String")) void)))
                                      (19 (aconst_null))
                                      (20 (areturn))
                                      (endofcode 21))
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
					(methodCP "run" "java.lang.ClassLoader$1" () (class "java.lang.Void"))))
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


(defconst *ClassLoader$1-class-table*
  (make-static-class-decls 
   *java.lang.ClassLoader$1*))

(defconst *package-name-map* 
  ("java.lang.ClassLoader$1" . "java.lang"))

