; Frame$AccessibleAWTFrame-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.Frame$AccessibleAWTFrame*
 (make-class-def
      '(class "java.awt.Frame$AccessibleAWTFrame"
            "java.awt.Window$AccessibleAWTWindow"
            (constant_pool
                        (LONG -6172960752956030250))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "this$0" (class "java.awt.Frame") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.Frame"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.Frame$AccessibleAWTFrame" (class "java.awt.Frame"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (invokespecial
					(methodCP "<init>" "java.awt.Window$AccessibleAWTWindow" ((class "java.awt.Window")) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleRole"
                              (parameters )
                              (returntype . (class "javax.accessibility.AccessibleRole"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "FRAME" "javax.accessibility.AccessibleRole" (class "javax.accessibility.AccessibleRole"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleStateSet"
                              (parameters )
                              (returntype . (class "javax.accessibility.AccessibleStateSet"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "getAccessibleStateSet" "java.awt.Window$AccessibleAWTWindow" () (class "javax.accessibility.AccessibleStateSet")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "this$0" "java.awt.Frame$AccessibleAWTFrame" (class "java.awt.Frame")))) 
                                      (9 (invokevirtual (methodCP "getFocusOwner" "java.awt.Frame" () (class "java.awt.Component")))) 
                                      (12 (ifnull 23))  ;;to TAG_0
                                      (15 (aload_1)) 
                                      (16 (getstatic (fieldCP "ACTIVE" "javax.accessibility.AccessibleState" (class "javax.accessibility.AccessibleState")))) 
                                      (19 (invokevirtual (methodCP "add" "javax.accessibility.AccessibleStateSet" ((class "javax.accessibility.AccessibleState")) boolean))) 
                                      (22 (pop)) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (getfield (fieldCP "this$0" "java.awt.Frame$AccessibleAWTFrame" (class "java.awt.Frame")))) 
                                      (27 (invokevirtual (methodCP "isResizable" "java.awt.Frame" () boolean))) 
                                      (30 (ifeq 41)) ;;to TAG_1
                                      (33 (aload_1)) 
                                      (34 (getstatic (fieldCP "RESIZABLE" "javax.accessibility.AccessibleState" (class "javax.accessibility.AccessibleState")))) 
                                      (37 (invokevirtual (methodCP "add" "javax.accessibility.AccessibleStateSet" ((class "javax.accessibility.AccessibleState")) boolean))) 
                                      (40 (pop)) 
                                      (41 (aload_1)) ;;at TAG_1
                                      (42 (areturn)) 
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Frame$AccessibleAWTFrame-class-table*
  (make-static-class-decls 
   *java.awt.Frame$AccessibleAWTFrame*))

(defconst *package-name-map* 
  ("java.awt.Frame$AccessibleAWTFrame" . "java.awt"))

