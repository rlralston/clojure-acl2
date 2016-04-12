; DropTargetContext-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.dnd.DropTargetContext*
 (make-class-def
      '(class "java.awt.dnd.DropTargetContext"
            "java.lang.Object"
            (constant_pool
                        (LONG -634158968993743371))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "dropTarget" (class "java.awt.dnd.DropTarget") (accessflags  *class*  *private* ) -1)
                        (field "dropTargetContextPeer" (class "java.awt.dnd.peer.DropTargetContextPeer") (accessflags  *class*  *private*  *transient* ) -1)
                        (field "transferable" (class "java.awt.datatransfer.Transferable") (accessflags  *class*  *private*  *transient* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.dnd.DropTarget"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "dropTarget" "java.awt.dnd.DropTargetContext" (class "java.awt.dnd.DropTarget"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDropTarget"
                              (parameters )
                              (returntype . (class "java.awt.dnd.DropTarget"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "dropTarget" "java.awt.dnd.DropTargetContext" (class "java.awt.dnd.DropTarget"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getComponent"
                              (parameters )
                              (returntype . (class "java.awt.Component"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "dropTarget" "java.awt.dnd.DropTargetContext" (class "java.awt.dnd.DropTarget"))))
                                      (4 (invokevirtual
					(methodCP "getComponent" "java.awt.dnd.DropTarget" () (class "java.awt.Component"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addNotify"
                              (parameters (class "java.awt.dnd.peer.DropTargetContextPeer"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "dropTargetContextPeer" "java.awt.dnd.DropTargetContext" (class "java.awt.dnd.peer.DropTargetContextPeer"))))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "removeNotify"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aconst_null))
                                      (2 (putfield (fieldCP "dropTargetContextPeer" "java.awt.dnd.DropTargetContext" (class "java.awt.dnd.peer.DropTargetContextPeer"))))
                                      (5 (aload_0))
                                      (6 (aconst_null))
                                      (7 (putfield (fieldCP "transferable" "java.awt.dnd.DropTargetContext" (class "java.awt.datatransfer.Transferable"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setTargetActions"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 52)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getDropTargetContextPeer" "java.awt.dnd.DropTargetContext" () (class "java.awt.dnd.peer.DropTargetContextPeer")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_2)) 
                                      (6 (ifnull 43)) ;;to TAG_0
                                      (9 (aload_2)) 
                                      (10 (dup)) 
                                      (11 (astore_3)) 
                                      (12 (monitorenter)) 
                                      (13 (aload_2)) ;;at TAG_3
                                      (14 (iload_1)) 
                                      (15 (invokeinterface (methodCP "setTargetActions" "java.awt.dnd.peer.DropTargetContextPeer" (int) void) 2)) 
                                      (20 (aload_0)) 
                                      (21 (invokevirtual (methodCP "getDropTarget" "java.awt.dnd.DropTargetContext" () (class "java.awt.dnd.DropTarget")))) 
                                      (24 (iload_1)) 
                                      (25 (invokevirtual (methodCP "doSetDefaultActions" "java.awt.dnd.DropTarget" (int) void))) 
                                      (28 (aload_3)) 
                                      (29 (monitorexit)) 
                                      (30 (goto 40)) ;;to TAG_1;;at TAG_4
                                      (33 (astore 4)) ;;at TAG_5
                                      (35 (aload_3)) 
                                      (36 (monitorexit)) 
                                      (37 (aload 4)) ;;at TAG_6
                                      (39 (athrow)) 
                                      (40 (goto 51))  ;;to TAG_2;;at TAG_1
                                      (43 (aload_0)) ;;at TAG_0
                                      (44 (invokevirtual (methodCP "getDropTarget" "java.awt.dnd.DropTargetContext" () (class "java.awt.dnd.DropTarget")))) 
                                      (47 (iload_1)) 
                                      (48 (invokevirtual (methodCP "doSetDefaultActions" "java.awt.dnd.DropTarget" (int) void))) 
                                      (51 (return)) ;;at TAG_2
                                      (endofcode 52))
                                   (Exceptions 
                                     (handler 13 30  33 (class "java.lang.Throwable"))
                                     (handler 33 37  33 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "getTargetActions"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getDropTargetContextPeer" "java.awt.dnd.DropTargetContext" () (class "java.awt.dnd.peer.DropTargetContextPeer")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_1)) 
                                      (6 (ifnull 18))  ;;to TAG_0
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "getTargetActions" "java.awt.dnd.peer.DropTargetContextPeer" () int) 1)) 
                                      (15 (goto 25)) ;;to TAG_1
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (getfield (fieldCP "dropTarget" "java.awt.dnd.DropTargetContext" (class "java.awt.dnd.DropTarget")))) 
                                      (22 (invokevirtual (methodCP "getDefaultActions" "java.awt.dnd.DropTarget" () int))) 
                                      (25 (ireturn)) ;;at TAG_1
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "dropComplete"
                              (parameters boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getDropTargetContextPeer" "java.awt.dnd.DropTargetContext" () (class "java.awt.dnd.peer.DropTargetContextPeer")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_2)) 
                                      (6 (ifnull 16))  ;;to TAG_0
                                      (9 (aload_2)) 
                                      (10 (iload_1)) 
                                      (11 (invokeinterface (methodCP "dropComplete" "java.awt.dnd.peer.DropTargetContextPeer" (boolean) void) 2)) 
                                      (16 (return)) ;;at TAG_0
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "acceptDrag"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getDropTargetContextPeer" "java.awt.dnd.DropTargetContext" () (class "java.awt.dnd.peer.DropTargetContextPeer")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_2)) 
                                      (6 (ifnull 16))  ;;to TAG_0
                                      (9 (aload_2)) 
                                      (10 (iload_1)) 
                                      (11 (invokeinterface (methodCP "acceptDrag" "java.awt.dnd.peer.DropTargetContextPeer" (int) void) 2)) 
                                      (16 (return)) ;;at TAG_0
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "rejectDrag"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getDropTargetContextPeer" "java.awt.dnd.DropTargetContext" () (class "java.awt.dnd.peer.DropTargetContextPeer")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_1)) 
                                      (6 (ifnull 15))  ;;to TAG_0
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "rejectDrag" "java.awt.dnd.peer.DropTargetContextPeer" () void) 1)) 
                                      (15 (return)) ;;at TAG_0
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "acceptDrop"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getDropTargetContextPeer" "java.awt.dnd.DropTargetContext" () (class "java.awt.dnd.peer.DropTargetContextPeer")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_2)) 
                                      (6 (ifnull 16))  ;;to TAG_0
                                      (9 (aload_2)) 
                                      (10 (iload_1)) 
                                      (11 (invokeinterface (methodCP "acceptDrop" "java.awt.dnd.peer.DropTargetContextPeer" (int) void) 2)) 
                                      (16 (return)) ;;at TAG_0
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "rejectDrop"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getDropTargetContextPeer" "java.awt.dnd.DropTargetContext" () (class "java.awt.dnd.peer.DropTargetContextPeer")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_1)) 
                                      (6 (ifnull 15))  ;;to TAG_0
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "rejectDrop" "java.awt.dnd.peer.DropTargetContextPeer" () void) 1)) 
                                      (15 (return)) ;;at TAG_0
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCurrentDataFlavors"
                              (parameters )
                              (returntype . (array (class "java.awt.datatransfer.DataFlavor")))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getDropTargetContextPeer" "java.awt.dnd.DropTargetContext" () (class "java.awt.dnd.peer.DropTargetContextPeer")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_1)) 
                                      (6 (ifnull 18))  ;;to TAG_0
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "getTransferDataFlavors" "java.awt.dnd.peer.DropTargetContextPeer" () (array (class "java.awt.datatransfer.DataFlavor"))) 1)) 
                                      (15 (goto 22)) ;;to TAG_1
                                      (18 (iconst_0)) ;;at TAG_0
                                      (19 (anewarray (class "java.awt.datatransfer.DataFlavor"))) 
                                      (22 (areturn)) ;;at TAG_1
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCurrentDataFlavorsAsList"
                              (parameters )
                              (returntype . (class "java.util.List"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getCurrentDataFlavors" "java.awt.dnd.DropTargetContext" () (array (class "java.awt.datatransfer.DataFlavor")))))
                                      (4 (invokestatic
					(methodCP "asList" "java.util.Arrays" ((array (class "java.lang.Object"))) (class "java.util.List"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isDataFlavorSupported"
                              (parameters (class "java.awt.datatransfer.DataFlavor"))
                              (returntype . boolean)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getCurrentDataFlavorsAsList" "java.awt.dnd.DropTargetContext" () (class "java.util.List"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "contains" "java.util.List" ((class "java.lang.Object")) boolean) 2))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getTransferable"
                              (parameters )
                              (returntype . (class "java.awt.datatransfer.Transferable"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 79)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getDropTargetContextPeer" "java.awt.dnd.DropTargetContext" () (class "java.awt.dnd.peer.DropTargetContextPeer")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_1)) 
                                      (6 (ifnonnull 17)) ;;to TAG_0
                                      (9 (new (class "java.awt.dnd.InvalidDnDOperationException"))) 
                                      (12 (dup)) 
                                      (13 (invokespecial (methodCP "<init>" "java.awt.dnd.InvalidDnDOperationException" () void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (getfield (fieldCP "transferable" "java.awt.dnd.DropTargetContext" (class "java.awt.datatransfer.Transferable")))) 
                                      (21 (ifnonnull 74)) ;;to TAG_1
                                      (24 (aload_1)) 
                                      (25 (invokeinterface (methodCP "getTransferable" "java.awt.dnd.peer.DropTargetContextPeer" () (class "java.awt.datatransfer.Transferable")) 1)) 
                                      (30 (astore_2)) 
                                      (31 (aload_1)) 
                                      (32 (invokeinterface (methodCP "isTransferableJVMLocal" "java.awt.dnd.peer.DropTargetContextPeer" () boolean) 1)) 
                                      (37 (istore_3)) 
                                      (38 (aload_0)) 
                                      (39 (dup)) 
                                      (40 (astore 4)) 
                                      (42 (monitorenter)) 
                                      (43 (aload_0)) ;;at TAG_3
                                      (44 (getfield (fieldCP "transferable" "java.awt.dnd.DropTargetContext" (class "java.awt.datatransfer.Transferable")))) 
                                      (47 (ifnonnull 60))  ;;to TAG_2
                                      (50 (aload_0)) 
                                      (51 (aload_0)) 
                                      (52 (aload_2)) 
                                      (53 (iload_3)) 
                                      (54 (invokevirtual (methodCP "createTransferableProxy" "java.awt.dnd.DropTargetContext" ((class "java.awt.datatransfer.Transferable") boolean) (class "java.awt.datatransfer.Transferable")))) 
                                      (57 (putfield (fieldCP "transferable" "java.awt.dnd.DropTargetContext" (class "java.awt.datatransfer.Transferable")))) 
                                      (60 (aload 4)) ;;at TAG_2
                                      (62 (monitorexit)) 
                                      (63 (goto 74)) ;;to TAG_1;;at TAG_4
                                      (66 (astore 5)) ;;at TAG_5
                                      (68 (aload 4)) 
                                      (70 (monitorexit)) 
                                      (71 (aload 5)) ;;at TAG_6
                                      (73 (athrow)) 
                                      (74 (aload_0)) ;;at TAG_1
                                      (75 (getfield (fieldCP "transferable" "java.awt.dnd.DropTargetContext" (class "java.awt.datatransfer.Transferable")))) 
                                      (78 (areturn)) 
                                      (endofcode 79))
                                   (Exceptions 
                                     (handler 43 63  66 (class "java.lang.Throwable"))
                                     (handler 66 71  66 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "getDropTargetContextPeer"
                              (parameters )
                              (returntype . (class "java.awt.dnd.peer.DropTargetContextPeer"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "dropTargetContextPeer" "java.awt.dnd.DropTargetContext" (class "java.awt.dnd.peer.DropTargetContextPeer"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "createTransferableProxy"
                              (parameters (class "java.awt.datatransfer.Transferable") boolean)
                              (returntype . (class "java.awt.datatransfer.Transferable"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.awt.dnd.DropTargetContext$TransferableProxy")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (iload_2))
                                      (7 (invokespecial
					(methodCP "<init>" "java.awt.dnd.DropTargetContext$TransferableProxy" ((class "java.awt.dnd.DropTargetContext") (class "java.awt.datatransfer.Transferable") boolean) void)))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *DropTargetContext-class-table*
  (make-static-class-decls 
   *java.awt.dnd.DropTargetContext*))

(defconst *package-name-map* 
  ("java.awt.dnd.DropTargetContext" . "java.awt.dnd"))
