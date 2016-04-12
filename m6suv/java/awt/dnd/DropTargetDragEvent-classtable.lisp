; DropTargetDragEvent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.dnd.DropTargetDragEvent*
 (make-class-def
      '(class "java.awt.dnd.DropTargetDragEvent"
            "java.awt.dnd.DropTargetEvent"
            (constant_pool
                        (LONG -8422265619058953682)
                        (STRING  "cursorLocn")
                        (INT 1073741824)
                        (STRING  "dropAction")
                        (INT -1073741828)
                        (STRING  "srcActions"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "location" (class "java.awt.Point") (accessflags  *class*  *private* ) -1)
                        (field "actions" int (accessflags  *class*  *private* ) -1)
                        (field "dropAction" int (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.dnd.DropTargetContext") (class "java.awt.Point") int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 101)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "<init>" "java.awt.dnd.DropTargetEvent" ((class "java.awt.dnd.DropTargetContext")) void))) 
                                      (5 (aload_2)) 
                                      (6 (ifnonnull 19)) ;;to TAG_0
                                      (9 (new (class "java.lang.NullPointerException"))) 
                                      (12 (dup)) 
                                      (13 (ldc 1)) ;;STRING:: "cursorLocn"
                                      (15 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (18 (athrow)) 
                                      (19 (iload_3)) ;;at TAG_0
                                      (20 (ifeq 66)) ;;to TAG_1
                                      (23 (iload_3)) 
                                      (24 (iconst_1)) 
                                      (25 (if_icmpeq 66)) ;;to TAG_1
                                      (28 (iload_3)) 
                                      (29 (iconst_2)) 
                                      (30 (if_icmpeq 66)) ;;to TAG_1
                                      (33 (iload_3)) 
                                      (34 (ldc 2)) ;;INT:: "1073741824"
                                      (36 (if_icmpeq 66)) ;;to TAG_1
                                      (39 (new (class "java.lang.IllegalArgumentException"))) 
                                      (42 (dup)) 
                                      (43 (new (class "java.lang.StringBuilder"))) 
                                      (46 (dup)) 
                                      (47 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (50 (ldc 3)) ;;STRING:: "dropAction"
                                      (52 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (55 (iload_3)) 
                                      (56 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (59 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (62 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (65 (athrow)) 
                                      (66 (iload 4)) ;;at TAG_1
                                      (68 (ldc 4)) ;;INT:: "-1073741828"
                                      (70 (iand)) 
                                      (71 (ifeq 84))  ;;to TAG_2
                                      (74 (new (class "java.lang.IllegalArgumentException"))) 
                                      (77 (dup)) 
                                      (78 (ldc 5)) ;;STRING:: "srcActions"
                                      (80 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (83 (athrow)) 
                                      (84 (aload_0)) ;;at TAG_2
                                      (85 (aload_2)) 
                                      (86 (putfield (fieldCP "location" "java.awt.dnd.DropTargetDragEvent" (class "java.awt.Point")))) 
                                      (89 (aload_0)) 
                                      (90 (iload 4)) 
                                      (92 (putfield (fieldCP "actions" "java.awt.dnd.DropTargetDragEvent" int))) 
                                      (95 (aload_0)) 
                                      (96 (iload_3)) 
                                      (97 (putfield (fieldCP "dropAction" "java.awt.dnd.DropTargetDragEvent" int))) 
                                      (100 (return)) 
                                      (endofcode 101))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLocation"
                              (parameters )
                              (returntype . (class "java.awt.Point"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "location" "java.awt.dnd.DropTargetDragEvent" (class "java.awt.Point"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCurrentDataFlavors"
                              (parameters )
                              (returntype . (array (class "java.awt.datatransfer.DataFlavor")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getDropTargetContext" "java.awt.dnd.DropTargetDragEvent" () (class "java.awt.dnd.DropTargetContext"))))
                                      (4 (invokevirtual
					(methodCP "getCurrentDataFlavors" "java.awt.dnd.DropTargetContext" () (array (class "java.awt.datatransfer.DataFlavor")))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCurrentDataFlavorsAsList"
                              (parameters )
                              (returntype . (class "java.util.List"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getDropTargetContext" "java.awt.dnd.DropTargetDragEvent" () (class "java.awt.dnd.DropTargetContext"))))
                                      (4 (invokevirtual
					(methodCP "getCurrentDataFlavorsAsList" "java.awt.dnd.DropTargetContext" () (class "java.util.List"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isDataFlavorSupported"
                              (parameters (class "java.awt.datatransfer.DataFlavor"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getDropTargetContext" "java.awt.dnd.DropTargetDragEvent" () (class "java.awt.dnd.DropTargetContext"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "isDataFlavorSupported" "java.awt.dnd.DropTargetContext" ((class "java.awt.datatransfer.DataFlavor")) boolean)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getSourceActions"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "actions" "java.awt.dnd.DropTargetDragEvent" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDropAction"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "dropAction" "java.awt.dnd.DropTargetDragEvent" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getTransferable"
                              (parameters )
                              (returntype . (class "java.awt.datatransfer.Transferable"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getDropTargetContext" "java.awt.dnd.DropTargetDragEvent" () (class "java.awt.dnd.DropTargetContext"))))
                                      (4 (invokevirtual
					(methodCP "getTransferable" "java.awt.dnd.DropTargetContext" () (class "java.awt.datatransfer.Transferable"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "acceptDrag"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getDropTargetContext" "java.awt.dnd.DropTargetDragEvent" () (class "java.awt.dnd.DropTargetContext"))))
                                      (4 (iload_1))
                                      (5 (invokevirtual
					(methodCP "acceptDrag" "java.awt.dnd.DropTargetContext" (int) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "rejectDrag"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getDropTargetContext" "java.awt.dnd.DropTargetDragEvent" () (class "java.awt.dnd.DropTargetContext"))))
                                      (4 (invokevirtual
					(methodCP "rejectDrag" "java.awt.dnd.DropTargetContext" () void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *DropTargetDragEvent-class-table*
  (make-static-class-decls 
   *java.awt.dnd.DropTargetDragEvent*))

(defconst *package-name-map* 
  ("java.awt.dnd.DropTargetDragEvent" . "java.awt.dnd"))

