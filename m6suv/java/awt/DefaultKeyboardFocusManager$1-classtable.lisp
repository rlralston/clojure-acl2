; DefaultKeyboardFocusManager$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:25 CDT 2014.
;

(defconst *java.awt.DefaultKeyboardFocusManager$1*
 (make-class-def
      '(class "java.awt.DefaultKeyboardFocusManager$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$se" (class "java.awt.SentEvent") (accessflags  *class*  *final* ) -1)
                        (field "val$targetAppContext" (class "sun.awt.AppContext") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.SentEvent") (class "sun.awt.AppContext"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$se" "java.awt.DefaultKeyboardFocusManager$1" (class "java.awt.SentEvent"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$targetAppContext" "java.awt.DefaultKeyboardFocusManager$1" (class "sun.awt.AppContext"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "evaluate"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "val$se" "java.awt.DefaultKeyboardFocusManager$1" (class "java.awt.SentEvent")))) 
                                      (4 (getfield (fieldCP "dispatched" "java.awt.SentEvent" boolean))) 
                                      (7 (ifne 24))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "val$targetAppContext" "java.awt.DefaultKeyboardFocusManager$1" (class "sun.awt.AppContext")))) 
                                      (14 (invokevirtual (methodCP "isDisposed" "sun.awt.AppContext" () boolean))) 
                                      (17 (ifne 24))  ;;to TAG_0
                                      (20 (iconst_1)) 
                                      (21 (goto 25)) ;;to TAG_1
                                      (24 (iconst_0)) ;;at TAG_0
                                      (25 (ireturn)) ;;at TAG_1
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.Conditional")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *DefaultKeyboardFocusManager$1-class-table*
  (make-static-class-decls 
   *java.awt.DefaultKeyboardFocusManager$1*))

(defconst *package-name-map* 
  ("java.awt.DefaultKeyboardFocusManager$1" . "java.awt"))

