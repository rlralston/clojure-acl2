; DragSourceContext$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:25 CDT 2014.
;

(defconst *java.awt.dnd.DragSourceContext$1*
 (make-class-def
      '(class "java.awt.dnd.DragSourceContext$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.awt.dnd.DragSourceContext") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.dnd.DragSourceContext"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.dnd.DragSourceContext$1" (class "java.awt.dnd.DragSourceContext"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getTransferDataFlavors"
                              (parameters )
                              (returntype . (array (class "java.awt.datatransfer.DataFlavor")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (anewarray (class "java.awt.datatransfer.DataFlavor")))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isDataFlavorSupported"
                              (parameters (class "java.awt.datatransfer.DataFlavor"))
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
                        (method "getTransferData"
                              (parameters (class "java.awt.datatransfer.DataFlavor"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (new (class "java.awt.datatransfer.UnsupportedFlavorException")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (invokespecial
					(methodCP "<init>" "java.awt.datatransfer.UnsupportedFlavorException" ((class "java.awt.datatransfer.DataFlavor")) void)))
                                      (8 (athrow))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.datatransfer.Transferable")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *DragSourceContext$1-class-table*
  (make-static-class-decls 
   *java.awt.dnd.DragSourceContext$1*))

(defconst *package-name-map* 
  ("java.awt.dnd.DragSourceContext$1" . "java.awt.dnd"))

