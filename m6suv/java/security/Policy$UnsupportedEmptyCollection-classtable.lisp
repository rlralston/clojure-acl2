; Policy$UnsupportedEmptyCollection-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.Policy$UnsupportedEmptyCollection*
 (make-class-def
      '(class "java.security.Policy$UnsupportedEmptyCollection"
            "java.security.PermissionCollection"
            (constant_pool)
            (fields
                        (field "perms" (class "java.security.Permissions") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.security.PermissionCollection" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "java.security.Permissions")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "java.security.Permissions" () void)))
                                      (12 (putfield (fieldCP "perms" "java.security.Policy$UnsupportedEmptyCollection" (class "java.security.Permissions"))))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "perms" "java.security.Policy$UnsupportedEmptyCollection" (class "java.security.Permissions"))))
                                      (19 (invokevirtual
					(methodCP "setReadOnly" "java.security.Permissions" () void)))
                                      (22 (return))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.security.Permission"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "perms" "java.security.Policy$UnsupportedEmptyCollection" (class "java.security.Permissions"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "add" "java.security.Permissions" ((class "java.security.Permission")) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "implies"
                              (parameters (class "java.security.Permission"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "perms" "java.security.Policy$UnsupportedEmptyCollection" (class "java.security.Permissions"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "implies" "java.security.Permissions" ((class "java.security.Permission")) boolean)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "elements"
                              (parameters )
                              (returntype . (class "java.util.Enumeration"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "perms" "java.security.Policy$UnsupportedEmptyCollection" (class "java.security.Permissions"))))
                                      (4 (invokevirtual
					(methodCP "elements" "java.security.Permissions" () (class "java.util.Enumeration"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Policy$UnsupportedEmptyCollection-class-table*
  (make-static-class-decls 
   *java.security.Policy$UnsupportedEmptyCollection*))

(defconst *package-name-map* 
  ("java.security.Policy$UnsupportedEmptyCollection" . "java.security"))
