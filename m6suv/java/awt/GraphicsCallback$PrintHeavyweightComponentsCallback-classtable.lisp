; GraphicsCallback$PrintHeavyweightComponentsCallback-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback*
 (make-class-def
      '(class "java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback"
            "java.awt.GraphicsCallback"
            (constant_pool)
            (fields
                        (field "instance" (class "java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback") (accessflags  *class*  *private*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.awt.GraphicsCallback" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters (class "java.awt.Component") (class "java.awt.Graphics"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (getfield (fieldCP "peer" "java.awt.Component" (class "java.awt.peer.ComponentPeer")))) 
                                      (4 (instanceof (class "java.awt.peer.LightweightPeer"))) 
                                      (7 (ifeq 18))  ;;to TAG_0
                                      (10 (aload_1)) 
                                      (11 (aload_2)) 
                                      (12 (invokevirtual (methodCP "printHeavyweightComponents" "java.awt.Component" ((class "java.awt.Graphics")) void))) 
                                      (15 (goto 23)) ;;to TAG_1
                                      (18 (aload_1)) ;;at TAG_0
                                      (19 (aload_2)) 
                                      (20 (invokevirtual (methodCP "printAll" "java.awt.Component" ((class "java.awt.Graphics")) void))) 
                                      (23 (return)) ;;at TAG_1
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getInstance"
                              (parameters )
                              (returntype . (class "java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "instance" "java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback" (class "java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback" () void)))
                                      (7 (putstatic (fieldCP "instance" "java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback" (class "java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *GraphicsCallback$PrintHeavyweightComponentsCallback-class-table*
  (make-static-class-decls 
   *java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback*))

(defconst *package-name-map* 
  ("java.awt.GraphicsCallback$PrintHeavyweightComponentsCallback" . "java.awt"))

