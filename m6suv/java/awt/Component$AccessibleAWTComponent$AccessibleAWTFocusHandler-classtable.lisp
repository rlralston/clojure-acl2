; Component$AccessibleAWTComponent$AccessibleAWTFocusHandler-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:24 CDT 2014.
;

(defconst *java.awt.Component$AccessibleAWTComponent$AccessibleAWTFocusHandler*
 (make-class-def
      '(class "java.awt.Component$AccessibleAWTComponent$AccessibleAWTFocusHandler"
            "java.lang.Object"
            (constant_pool
                        (STRING  "AccessibleState"))
            (fields
                        (field "this$1" (class "java.awt.Component$AccessibleAWTComponent") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.Component$AccessibleAWTComponent"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$1" "java.awt.Component$AccessibleAWTComponent$AccessibleAWTFocusHandler" (class "java.awt.Component$AccessibleAWTComponent"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "focusGained"
                              (parameters (class "java.awt.event.FocusEvent"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$1" "java.awt.Component$AccessibleAWTComponent$AccessibleAWTFocusHandler" (class "java.awt.Component$AccessibleAWTComponent")))) 
                                      (4 (getfield (fieldCP "this$0" "java.awt.Component$AccessibleAWTComponent" (class "java.awt.Component")))) 
                                      (7 (getfield (fieldCP "accessibleContext" "java.awt.Component" (class "javax.accessibility.AccessibleContext")))) 
                                      (10 (ifnull 32))  ;;to TAG_0
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "this$1" "java.awt.Component$AccessibleAWTComponent$AccessibleAWTFocusHandler" (class "java.awt.Component$AccessibleAWTComponent")))) 
                                      (17 (getfield (fieldCP "this$0" "java.awt.Component$AccessibleAWTComponent" (class "java.awt.Component")))) 
                                      (20 (getfield (fieldCP "accessibleContext" "java.awt.Component" (class "javax.accessibility.AccessibleContext")))) 
                                      (23 (ldc 0)) ;;STRING:: "AccessibleState"
                                      (25 (aconst_null)) 
                                      (26 (getstatic (fieldCP "FOCUSED" "javax.accessibility.AccessibleState" (class "javax.accessibility.AccessibleState")))) 
                                      (29 (invokevirtual (methodCP "firePropertyChange" "javax.accessibility.AccessibleContext" ((class "java.lang.String") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (32 (return)) ;;at TAG_0
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap )))
                        (method "focusLost"
                              (parameters (class "java.awt.event.FocusEvent"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$1" "java.awt.Component$AccessibleAWTComponent$AccessibleAWTFocusHandler" (class "java.awt.Component$AccessibleAWTComponent")))) 
                                      (4 (getfield (fieldCP "this$0" "java.awt.Component$AccessibleAWTComponent" (class "java.awt.Component")))) 
                                      (7 (getfield (fieldCP "accessibleContext" "java.awt.Component" (class "javax.accessibility.AccessibleContext")))) 
                                      (10 (ifnull 32))  ;;to TAG_0
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "this$1" "java.awt.Component$AccessibleAWTComponent$AccessibleAWTFocusHandler" (class "java.awt.Component$AccessibleAWTComponent")))) 
                                      (17 (getfield (fieldCP "this$0" "java.awt.Component$AccessibleAWTComponent" (class "java.awt.Component")))) 
                                      (20 (getfield (fieldCP "accessibleContext" "java.awt.Component" (class "javax.accessibility.AccessibleContext")))) 
                                      (23 (ldc 0)) ;;STRING:: "AccessibleState"
                                      (25 (getstatic (fieldCP "FOCUSED" "javax.accessibility.AccessibleState" (class "javax.accessibility.AccessibleState")))) 
                                      (28 (aconst_null)) 
                                      (29 (invokevirtual (methodCP "firePropertyChange" "javax.accessibility.AccessibleContext" ((class "java.lang.String") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (32 (return)) ;;at TAG_0
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.event.FocusListener")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Component$AccessibleAWTComponent$AccessibleAWTFocusHandler-class-table*
  (make-static-class-decls 
   *java.awt.Component$AccessibleAWTComponent$AccessibleAWTFocusHandler*))

(defconst *package-name-map* 
  ("java.awt.Component$AccessibleAWTComponent$AccessibleAWTFocusHandler" . "java.awt"))

