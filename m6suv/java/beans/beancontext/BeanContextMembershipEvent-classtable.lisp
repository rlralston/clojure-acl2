; BeanContextMembershipEvent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.beans.beancontext.BeanContextMembershipEvent*
 (make-class-def
      '(class "java.beans.beancontext.BeanContextMembershipEvent"
            "java.beans.beancontext.BeanContextEvent"
            (constant_pool
                        (LONG 3499346510334590959)
                        (STRING  "BeanContextMembershipEvent constructor:  changes is null.")
                        (STRING  "BeanContextMembershipEvent:  changes is null."))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "children" (class "java.util.Collection") (accessflags  *class*  *protected* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.beans.beancontext.BeanContext") (class "java.util.Collection"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "<init>" "java.beans.beancontext.BeanContextEvent" ((class "java.beans.beancontext.BeanContext")) void))) 
                                      (5 (aload_2)) 
                                      (6 (ifnonnull 19))  ;;to TAG_0
                                      (9 (new (class "java.lang.NullPointerException"))) 
                                      (12 (dup)) 
                                      (13 (ldc 1)) ;;STRING:: "BeanContextMembershipEvent constructor:  changes is null."
                                      (15 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (18 (athrow)) 
                                      (19 (aload_0)) ;;at TAG_0
                                      (20 (aload_2)) 
                                      (21 (putfield (fieldCP "children" "java.beans.beancontext.BeanContextMembershipEvent" (class "java.util.Collection")))) 
                                      (24 (return)) 
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.beans.beancontext.BeanContext") (array (class "java.lang.Object")))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "<init>" "java.beans.beancontext.BeanContextEvent" ((class "java.beans.beancontext.BeanContext")) void))) 
                                      (5 (aload_2)) 
                                      (6 (ifnonnull 19))  ;;to TAG_0
                                      (9 (new (class "java.lang.NullPointerException"))) 
                                      (12 (dup)) 
                                      (13 (ldc 2)) ;;STRING:: "BeanContextMembershipEvent:  changes is null."
                                      (15 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (18 (athrow)) 
                                      (19 (aload_0)) ;;at TAG_0
                                      (20 (aload_2)) 
                                      (21 (invokestatic (methodCP "asList" "java.util.Arrays" ((array (class "java.lang.Object"))) (class "java.util.List")))) 
                                      (24 (putfield (fieldCP "children" "java.beans.beancontext.BeanContextMembershipEvent" (class "java.util.Collection")))) 
                                      (27 (return)) 
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "children" "java.beans.beancontext.BeanContextMembershipEvent" (class "java.util.Collection"))))
                                      (4 (invokeinterface
					(methodCP "size" "java.util.Collection" () int) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "contains"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "children" "java.beans.beancontext.BeanContextMembershipEvent" (class "java.util.Collection"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "contains" "java.util.Collection" ((class "java.lang.Object")) boolean) 2))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toArray"
                              (parameters )
                              (returntype . (array (class "java.lang.Object")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "children" "java.beans.beancontext.BeanContextMembershipEvent" (class "java.util.Collection"))))
                                      (4 (invokeinterface
					(methodCP "toArray" "java.util.Collection" () (array (class "java.lang.Object"))) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "iterator"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "children" "java.beans.beancontext.BeanContextMembershipEvent" (class "java.util.Collection"))))
                                      (4 (invokeinterface
					(methodCP "iterator" "java.util.Collection" () (class "java.util.Iterator")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *BeanContextMembershipEvent-class-table*
  (make-static-class-decls 
   *java.beans.beancontext.BeanContextMembershipEvent*))

(defconst *package-name-map* 
  ("java.beans.beancontext.BeanContextMembershipEvent" . "java.beans.beancontext"))

