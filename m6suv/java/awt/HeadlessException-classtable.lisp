; HeadlessException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:28 CDT 2014.
;

(defconst *java.awt.HeadlessException*
 (make-class-def
      '(class "java.awt.HeadlessException"
            "java.lang.UnsupportedOperationException"
            (constant_pool
                        (LONG 167183644944358563))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
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
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" ((class "java.lang.String")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMessage"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 40)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "getMessage" "java.lang.UnsupportedOperationException" () (class "java.lang.String")))) 
                                      (4 (astore_1)) 
                                      (5 (invokestatic (methodCP "getHeadlessMessage" "java.awt.GraphicsEnvironment" () (class "java.lang.String")))) 
                                      (8 (astore_2)) 
                                      (9 (aload_1)) 
                                      (10 (ifnonnull 15))  ;;to TAG_0
                                      (13 (aload_2)) 
                                      (14 (areturn)) 
                                      (15 (aload_2)) ;;at TAG_0
                                      (16 (ifnonnull 21)) ;;to TAG_1
                                      (19 (aload_1)) 
                                      (20 (areturn)) 
                                      (21 (new (class "java.lang.StringBuilder"))) ;;at TAG_1
                                      (24 (dup)) 
                                      (25 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (28 (aload_1)) 
                                      (29 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (32 (aload_2)) 
                                      (33 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (36 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (39 (areturn)) 
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *HeadlessException-class-table*
  (make-static-class-decls 
   *java.awt.HeadlessException*))

(defconst *package-name-map* 
  ("java.awt.HeadlessException" . "java.awt"))
