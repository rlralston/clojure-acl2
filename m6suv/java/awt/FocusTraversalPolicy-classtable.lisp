; FocusTraversalPolicy-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.FocusTraversalPolicy*
 (make-class-def
      '(class "java.awt.FocusTraversalPolicy"
            "java.lang.Object"
            (constant_pool
                        (STRING  "window cannot be equal to null."))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
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
                        (method "getComponentAfter"
                              (parameters (class "java.awt.Container") (class "java.awt.Component"))
                              (returntype . (class "java.awt.Component"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getComponentBefore"
                              (parameters (class "java.awt.Container") (class "java.awt.Component"))
                              (returntype . (class "java.awt.Component"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getFirstComponent"
                              (parameters (class "java.awt.Container"))
                              (returntype . (class "java.awt.Component"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getLastComponent"
                              (parameters (class "java.awt.Container"))
                              (returntype . (class "java.awt.Component"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getDefaultComponent"
                              (parameters (class "java.awt.Container"))
                              (returntype . (class "java.awt.Component"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getInitialComponent"
                              (parameters (class "java.awt.Window"))
                              (returntype . (class "java.awt.Component"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 14))  ;;to TAG_0
                                      (4 (new (class "java.lang.IllegalArgumentException"))) 
                                      (7 (dup)) 
                                      (8 (ldc 0)) ;;STRING:: "window cannot be equal to null."
                                      (10 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (13 (athrow)) 
                                      (14 (aload_0)) ;;at TAG_0
                                      (15 (aload_1)) 
                                      (16 (invokevirtual (methodCP "getDefaultComponent" "java.awt.FocusTraversalPolicy" ((class "java.awt.Container")) (class "java.awt.Component")))) 
                                      (19 (astore_2)) 
                                      (20 (aload_2)) 
                                      (21 (ifnonnull 33)) ;;to TAG_1
                                      (24 (aload_1)) 
                                      (25 (invokevirtual (methodCP "isFocusableWindow" "java.awt.Window" () boolean))) 
                                      (28 (ifeq 33)) ;;to TAG_1
                                      (31 (aload_1)) 
                                      (32 (astore_2)) 
                                      (33 (aload_2)) ;;at TAG_1
                                      (34 (areturn)) 
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *FocusTraversalPolicy-class-table*
  (make-static-class-decls 
   *java.awt.FocusTraversalPolicy*))

(defconst *package-name-map* 
  ("java.awt.FocusTraversalPolicy" . "java.awt"))

