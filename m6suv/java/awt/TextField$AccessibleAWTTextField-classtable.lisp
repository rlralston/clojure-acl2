; TextField$AccessibleAWTTextField-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.TextField$AccessibleAWTTextField*
 (make-class-def
      '(class "java.awt.TextField$AccessibleAWTTextField"
            "java.awt.TextComponent$AccessibleAWTTextComponent"
            (constant_pool
                        (LONG 6219164359235943158))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "this$0" (class "java.awt.TextField") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.TextField"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.TextField$AccessibleAWTTextField" (class "java.awt.TextField"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (invokespecial
					(methodCP "<init>" "java.awt.TextComponent$AccessibleAWTTextComponent" ((class "java.awt.TextComponent")) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleStateSet"
                              (parameters )
                              (returntype . (class "javax.accessibility.AccessibleStateSet"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "getAccessibleStateSet" "java.awt.TextComponent$AccessibleAWTTextComponent" () (class "javax.accessibility.AccessibleStateSet"))))
                                      (4 (astore_1))
                                      (5 (aload_1))
                                      (6 (getstatic (fieldCP "SINGLE_LINE" "javax.accessibility.AccessibleState" (class "javax.accessibility.AccessibleState"))))
                                      (9 (invokevirtual
					(methodCP "add" "javax.accessibility.AccessibleStateSet" ((class "javax.accessibility.AccessibleState")) boolean)))
                                      (12 (pop))
                                      (13 (aload_1))
                                      (14 (areturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *TextField$AccessibleAWTTextField-class-table*
  (make-static-class-decls 
   *java.awt.TextField$AccessibleAWTTextField*))

(defconst *package-name-map* 
  ("java.awt.TextField$AccessibleAWTTextField" . "java.awt"))

