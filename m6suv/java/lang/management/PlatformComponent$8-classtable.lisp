; PlatformComponent$8-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.management.PlatformComponent$8*
 (make-class-def
      '(class "java.lang.management.PlatformComponent$8"
            "java.lang.Object"
            (constant_pool)
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
                        (method "getMXBeans"
                              (parameters )
                              (returntype . (class "java.util.List"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 7)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getRuntimeMXBean" "sun.management.ManagementFactoryHelper" () (class "java.lang.management.RuntimeMXBean"))))
                                      (3 (invokestatic
					(methodCP "singletonList" "java.util.Collections" ((class "java.lang.Object")) (class "java.util.List"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.lang.management.PlatformComponent$MXBeanFetcher")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *PlatformComponent$8-class-table*
  (make-static-class-decls 
   *java.lang.management.PlatformComponent$8*))

(defconst *package-name-map* 
  ("java.lang.management.PlatformComponent$8" . "java.lang.management"))

