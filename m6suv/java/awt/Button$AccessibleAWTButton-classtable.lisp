; Button$AccessibleAWTButton-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:24 CDT 2014.
;

(defconst *java.awt.Button$AccessibleAWTButton*
 (make-class-def
      '(class "java.awt.Button$AccessibleAWTButton"
            "java.awt.Component$AccessibleAWTComponent"
            (constant_pool
                        (LONG -5932203980244017102)
                        (STRING  "click"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "this$0" (class "java.awt.Button") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.Button"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.Button$AccessibleAWTButton" (class "java.awt.Button"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (invokespecial
					(methodCP "<init>" "java.awt.Component$AccessibleAWTComponent" ((class "java.awt.Component")) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleName"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "accessibleName" "java.awt.Button$AccessibleAWTButton" (class "java.lang.String")))) 
                                      (4 (ifnull 12))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "accessibleName" "java.awt.Button$AccessibleAWTButton" (class "java.lang.String")))) 
                                      (11 (areturn)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (getfield (fieldCP "this$0" "java.awt.Button$AccessibleAWTButton" (class "java.awt.Button")))) 
                                      (16 (invokevirtual (methodCP "getLabel" "java.awt.Button" () (class "java.lang.String")))) 
                                      (19 (ifnonnull 27)) ;;to TAG_1
                                      (22 (aload_0)) 
                                      (23 (invokespecial (methodCP "getAccessibleName" "java.awt.Component$AccessibleAWTComponent" () (class "java.lang.String")))) 
                                      (26 (areturn)) 
                                      (27 (aload_0)) ;;at TAG_1
                                      (28 (getfield (fieldCP "this$0" "java.awt.Button$AccessibleAWTButton" (class "java.awt.Button")))) 
                                      (31 (invokevirtual (methodCP "getLabel" "java.awt.Button" () (class "java.lang.String")))) 
                                      (34 (areturn)) 
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleAction"
                              (parameters )
                              (returntype . (class "javax.accessibility.AccessibleAction"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleValue"
                              (parameters )
                              (returntype . (class "javax.accessibility.AccessibleValue"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleActionCount"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleActionDescription"
                              (parameters int)
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (ifne 7))  ;;to TAG_0
                                      (4 (ldc 1)) ;;STRING:: "click"
                                      (6 (areturn)) 
                                      (7 (aconst_null)) ;;at TAG_0
                                      (8 (areturn)) 
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doAccessibleAction"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 35)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (ifne 33))  ;;to TAG_0
                                      (4 (invokestatic (methodCP "getEventQueue" "java.awt.Toolkit" () (class "java.awt.EventQueue")))) 
                                      (7 (new (class "java.awt.event.ActionEvent"))) 
                                      (10 (dup)) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "this$0" "java.awt.Button$AccessibleAWTButton" (class "java.awt.Button")))) 
                                      (15 (sipush 1001)) 
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "this$0" "java.awt.Button$AccessibleAWTButton" (class "java.awt.Button")))) 
                                      (22 (invokevirtual (methodCP "getActionCommand" "java.awt.Button" () (class "java.lang.String")))) 
                                      (25 (invokespecial (methodCP "<init>" "java.awt.event.ActionEvent" ((class "java.lang.Object") int (class "java.lang.String")) void))) 
                                      (28 (invokevirtual (methodCP "postEvent" "java.awt.EventQueue" ((class "java.awt.AWTEvent")) void))) 
                                      (31 (iconst_1)) 
                                      (32 (ireturn)) 
                                      (33 (iconst_0)) ;;at TAG_0
                                      (34 (ireturn)) 
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCurrentAccessibleValue"
                              (parameters )
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setCurrentAccessibleValue"
                              (parameters (class "java.lang.Number"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMinimumAccessibleValue"
                              (parameters )
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMaximumAccessibleValue"
                              (parameters )
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleRole"
                              (parameters )
                              (returntype . (class "javax.accessibility.AccessibleRole"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "PUSH_BUTTON" "javax.accessibility.AccessibleRole" (class "javax.accessibility.AccessibleRole"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "javax.accessibility.AccessibleAction" "javax.accessibility.AccessibleValue")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Button$AccessibleAWTButton-class-table*
  (make-static-class-decls 
   *java.awt.Button$AccessibleAWTButton*))

(defconst *package-name-map* 
  ("java.awt.Button$AccessibleAWTButton" . "java.awt"))

