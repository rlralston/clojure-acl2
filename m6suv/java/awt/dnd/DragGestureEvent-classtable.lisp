; DragGestureEvent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:25 CDT 2014.
;

(defconst *java.awt.dnd.DragGestureEvent*
 (make-class-def
      '(class "java.awt.dnd.DragGestureEvent"
            "java.util.EventObject"
            (constant_pool
                        (LONG 9080172649166731306)
                        (STRING  "null component")
                        (STRING  "null DragSource")
                        (STRING  "null or empty list of events")
                        (INT 1073741824)
                        (STRING  "bad action")
                        (STRING  "null origin")
                        (STRING  "dragSource")
                        (STRING  "component")
                        (STRING  "origin")
                        (STRING  "action")
                        (STRING  "events"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "events" (class "java.util.List") (accessflags  *class*  *private*  *transient* ) -1)
                        (field "dragSource" (class "java.awt.dnd.DragSource") (accessflags  *class*  *private* ) -1)
                        (field "component" (class "java.awt.Component") (accessflags  *class*  *private* ) -1)
                        (field "origin" (class "java.awt.Point") (accessflags  *class*  *private* ) -1)
                        (field "action" int (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.dnd.DragGestureRecognizer") int (class "java.awt.Point") (class "java.util.List"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 131)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "<init>" "java.util.EventObject" ((class "java.lang.Object")) void))) 
                                      (5 (aload_0)) 
                                      (6 (aload_1)) 
                                      (7 (invokevirtual (methodCP "getComponent" "java.awt.dnd.DragGestureRecognizer" () (class "java.awt.Component")))) 
                                      (10 (dup_x1)) 
                                      (11 (putfield (fieldCP "component" "java.awt.dnd.DragGestureEvent" (class "java.awt.Component")))) 
                                      (14 (ifnonnull 27)) ;;to TAG_0
                                      (17 (new (class "java.lang.IllegalArgumentException"))) 
                                      (20 (dup)) 
                                      (21 (ldc 1)) ;;STRING:: "null component"
                                      (23 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (26 (athrow)) 
                                      (27 (aload_0)) ;;at TAG_0
                                      (28 (aload_1)) 
                                      (29 (invokevirtual (methodCP "getDragSource" "java.awt.dnd.DragGestureRecognizer" () (class "java.awt.dnd.DragSource")))) 
                                      (32 (dup_x1)) 
                                      (33 (putfield (fieldCP "dragSource" "java.awt.dnd.DragGestureEvent" (class "java.awt.dnd.DragSource")))) 
                                      (36 (ifnonnull 49)) ;;to TAG_1
                                      (39 (new (class "java.lang.IllegalArgumentException"))) 
                                      (42 (dup)) 
                                      (43 (ldc 2)) ;;STRING:: "null DragSource"
                                      (45 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (48 (athrow)) 
                                      (49 (aload 4)) ;;at TAG_1
                                      (51 (ifnull 64))  ;;to TAG_2
                                      (54 (aload 4)) 
                                      (56 (invokeinterface (methodCP "isEmpty" "java.util.List" () boolean) 1)) 
                                      (61 (ifeq 74)) ;;to TAG_3
                                      (64 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_2
                                      (67 (dup)) 
                                      (68 (ldc 3)) ;;STRING:: "null or empty list of events"
                                      (70 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (73 (athrow)) 
                                      (74 (iload_2)) ;;at TAG_3
                                      (75 (iconst_1)) 
                                      (76 (if_icmpeq 100)) ;;to TAG_4
                                      (79 (iload_2)) 
                                      (80 (iconst_2)) 
                                      (81 (if_icmpeq 100)) ;;to TAG_4
                                      (84 (iload_2)) 
                                      (85 (ldc 4)) ;;INT:: "1073741824"
                                      (87 (if_icmpeq 100)) ;;to TAG_4
                                      (90 (new (class "java.lang.IllegalArgumentException"))) 
                                      (93 (dup)) 
                                      (94 (ldc 5)) ;;STRING:: "bad action"
                                      (96 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (99 (athrow)) 
                                      (100 (aload_3)) ;;at TAG_4
                                      (101 (ifnonnull 114)) ;;to TAG_5
                                      (104 (new (class "java.lang.IllegalArgumentException"))) 
                                      (107 (dup)) 
                                      (108 (ldc 6)) ;;STRING:: "null origin"
                                      (110 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (113 (athrow)) 
                                      (114 (aload_0)) ;;at TAG_5
                                      (115 (aload 4)) 
                                      (117 (putfield (fieldCP "events" "java.awt.dnd.DragGestureEvent" (class "java.util.List")))) 
                                      (120 (aload_0)) 
                                      (121 (iload_2)) 
                                      (122 (putfield (fieldCP "action" "java.awt.dnd.DragGestureEvent" int))) 
                                      (125 (aload_0)) 
                                      (126 (aload_3)) 
                                      (127 (putfield (fieldCP "origin" "java.awt.dnd.DragGestureEvent" (class "java.awt.Point")))) 
                                      (130 (return)) 
                                      (endofcode 131))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getSourceAsDragGestureRecognizer"
                              (parameters )
                              (returntype . (class "java.awt.dnd.DragGestureRecognizer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getSource" "java.awt.dnd.DragGestureEvent" () (class "java.lang.Object"))))
                                      (4 (checkcast (class "java.awt.dnd.DragGestureRecognizer")))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getComponent"
                              (parameters )
                              (returntype . (class "java.awt.Component"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "component" "java.awt.dnd.DragGestureEvent" (class "java.awt.Component"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDragSource"
                              (parameters )
                              (returntype . (class "java.awt.dnd.DragSource"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "dragSource" "java.awt.dnd.DragGestureEvent" (class "java.awt.dnd.DragSource"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDragOrigin"
                              (parameters )
                              (returntype . (class "java.awt.Point"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "origin" "java.awt.dnd.DragGestureEvent" (class "java.awt.Point"))))
                                      (4 (areturn))
                                      (endofcode 5))
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
                                      (1 (getfield (fieldCP "events" "java.awt.dnd.DragGestureEvent" (class "java.util.List"))))
                                      (4 (invokeinterface
					(methodCP "iterator" "java.util.List" () (class "java.util.Iterator")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
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
                                      (1 (getfield (fieldCP "events" "java.awt.dnd.DragGestureEvent" (class "java.util.List"))))
                                      (4 (invokeinterface
					(methodCP "toArray" "java.util.List" () (array (class "java.lang.Object"))) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toArray"
                              (parameters (array (class "java.lang.Object")))
                              (returntype . (array (class "java.lang.Object")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "events" "java.awt.dnd.DragGestureEvent" (class "java.util.List"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "toArray" "java.util.List" ((array (class "java.lang.Object"))) (array (class "java.lang.Object"))) 2))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDragAction"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "action" "java.awt.dnd.DragGestureEvent" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getTriggerEvent"
                              (parameters )
                              (returntype . (class "java.awt.event.InputEvent"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getSourceAsDragGestureRecognizer" "java.awt.dnd.DragGestureEvent" () (class "java.awt.dnd.DragGestureRecognizer"))))
                                      (4 (invokevirtual
					(methodCP "getTriggerEvent" "java.awt.dnd.DragGestureRecognizer" () (class "java.awt.event.InputEvent"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "startDrag"
                              (parameters (class "java.awt.Cursor") (class "java.awt.datatransfer.Transferable"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "dragSource" "java.awt.dnd.DragGestureEvent" (class "java.awt.dnd.DragSource"))))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (aload_2))
                                      (7 (aconst_null))
                                      (8 (invokevirtual
					(methodCP "startDrag" "java.awt.dnd.DragSource" ((class "java.awt.dnd.DragGestureEvent") (class "java.awt.Cursor") (class "java.awt.datatransfer.Transferable") (class "java.awt.dnd.DragSourceListener")) void)))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "startDrag"
                              (parameters (class "java.awt.Cursor") (class "java.awt.datatransfer.Transferable") (class "java.awt.dnd.DragSourceListener"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "dragSource" "java.awt.dnd.DragGestureEvent" (class "java.awt.dnd.DragSource"))))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (aload_2))
                                      (7 (aload_3))
                                      (8 (invokevirtual
					(methodCP "startDrag" "java.awt.dnd.DragSource" ((class "java.awt.dnd.DragGestureEvent") (class "java.awt.Cursor") (class "java.awt.datatransfer.Transferable") (class "java.awt.dnd.DragSourceListener")) void)))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "startDrag"
                              (parameters (class "java.awt.Cursor") (class "java.awt.Image") (class "java.awt.Point") (class "java.awt.datatransfer.Transferable") (class "java.awt.dnd.DragSourceListener"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 6) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "dragSource" "java.awt.dnd.DragGestureEvent" (class "java.awt.dnd.DragSource"))))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (aload_2))
                                      (7 (aload_3))
                                      (8 (aload 4))
                                      (10 (aload 5))
                                      (12 (invokevirtual
					(methodCP "startDrag" "java.awt.dnd.DragSource" ((class "java.awt.dnd.DragGestureEvent") (class "java.awt.Cursor") (class "java.awt.Image") (class "java.awt.Point") (class "java.awt.datatransfer.Transferable") (class "java.awt.dnd.DragSourceListener")) void)))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writeObject"
                              (parameters (class "java.io.ObjectOutputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "defaultWriteObject" "java.io.ObjectOutputStream" () void))) 
                                      (4 (aload_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "events" "java.awt.dnd.DragGestureEvent" (class "java.util.List")))) 
                                      (9 (invokestatic (methodCP "test" "java.awt.dnd.SerializationTester" ((class "java.lang.Object")) boolean))) 
                                      (12 (ifeq 22))  ;;to TAG_0
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "events" "java.awt.dnd.DragGestureEvent" (class "java.util.List")))) 
                                      (19 (goto 23)) ;;to TAG_1
                                      (22 (aconst_null)) ;;at TAG_0
                                      (23 (invokevirtual (methodCP "writeObject" "java.io.ObjectOutputStream" ((class "java.lang.Object")) void))) ;;at TAG_1
                                      (26 (return)) 
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readObject"
                              (parameters (class "java.io.ObjectInputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 102)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "readFields" "java.io.ObjectInputStream" () (class "java.io.ObjectInputStream$GetField")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_0)) 
                                      (6 (aload_2)) 
                                      (7 (ldc 7)) ;;STRING:: "dragSource"
                                      (9 (aconst_null)) 
                                      (10 (invokevirtual (methodCP "get" "java.io.ObjectInputStream$GetField" ((class "java.lang.String") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (13 (checkcast (class "java.awt.dnd.DragSource"))) 
                                      (16 (putfield (fieldCP "dragSource" "java.awt.dnd.DragGestureEvent" (class "java.awt.dnd.DragSource")))) 
                                      (19 (aload_0)) 
                                      (20 (aload_2)) 
                                      (21 (ldc 8)) ;;STRING:: "component"
                                      (23 (aconst_null)) 
                                      (24 (invokevirtual (methodCP "get" "java.io.ObjectInputStream$GetField" ((class "java.lang.String") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (27 (checkcast (class "java.awt.Component"))) 
                                      (30 (putfield (fieldCP "component" "java.awt.dnd.DragGestureEvent" (class "java.awt.Component")))) 
                                      (33 (aload_0)) 
                                      (34 (aload_2)) 
                                      (35 (ldc 9)) ;;STRING:: "origin"
                                      (37 (aconst_null)) 
                                      (38 (invokevirtual (methodCP "get" "java.io.ObjectInputStream$GetField" ((class "java.lang.String") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (41 (checkcast (class "java.awt.Point"))) 
                                      (44 (putfield (fieldCP "origin" "java.awt.dnd.DragGestureEvent" (class "java.awt.Point")))) 
                                      (47 (aload_0)) 
                                      (48 (aload_2)) 
                                      (49 (ldc 10)) ;;STRING:: "action"
                                      (51 (iconst_0)) 
                                      (52 (invokevirtual (methodCP "get" "java.io.ObjectInputStream$GetField" ((class "java.lang.String") int) int))) 
                                      (55 (putfield (fieldCP "action" "java.awt.dnd.DragGestureEvent" int))) 
                                      (58 (aload_0)) ;;at TAG_2
                                      (59 (aload_2)) 
                                      (60 (ldc 11)) ;;STRING:: "events"
                                      (62 (aconst_null)) 
                                      (63 (invokevirtual (methodCP "get" "java.io.ObjectInputStream$GetField" ((class "java.lang.String") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (66 (checkcast (class "java.util.List"))) 
                                      (69 (putfield (fieldCP "events" "java.awt.dnd.DragGestureEvent" (class "java.util.List")))) 
                                      (72 (goto 87)) ;;to TAG_0;;at TAG_3
                                      (75 (astore_3)) ;;at TAG_4
                                      (76 (aload_0)) 
                                      (77 (aload_1)) 
                                      (78 (invokevirtual (methodCP "readObject" "java.io.ObjectInputStream" () (class "java.lang.Object")))) 
                                      (81 (checkcast (class "java.util.List"))) 
                                      (84 (putfield (fieldCP "events" "java.awt.dnd.DragGestureEvent" (class "java.util.List")))) 
                                      (87 (aload_0)) ;;at TAG_0
                                      (88 (getfield (fieldCP "events" "java.awt.dnd.DragGestureEvent" (class "java.util.List")))) 
                                      (91 (ifnonnull 101)) ;;to TAG_1
                                      (94 (aload_0)) 
                                      (95 (getstatic (fieldCP "EMPTY_LIST" "java.util.Collections" (class "java.util.List")))) 
                                      (98 (putfield (fieldCP "events" "java.awt.dnd.DragGestureEvent" (class "java.util.List")))) 
                                      (101 (return)) ;;at TAG_1
                                      (endofcode 102))
                                   (Exceptions 
                                     (handler 58 72  75 (class "java.lang.IllegalArgumentException")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *DragGestureEvent-class-table*
  (make-static-class-decls 
   *java.awt.dnd.DragGestureEvent*))

(defconst *package-name-map* 
  ("java.awt.dnd.DragGestureEvent" . "java.awt.dnd"))

