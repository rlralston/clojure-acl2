; PopupMenu-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.PopupMenu*
 (make-class-def
      '(class "java.awt.PopupMenu"
            "java.awt.Menu"
            (constant_pool
                        (STRING  "popup")
                        (LONG -4620452533522760060)
                        (STRING  "")
                        (STRING  "parent is null")
                        (STRING  "PopupMenus with non-Component parents cannot be shown")
                        (STRING  "origin not in parent\ns hierarchy")
                        (STRING  "parent not showing on screen"))
            (fields
                        (field "base" (class "java.lang.String") (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "nameCounter" int (accessflags  *class*  *static* ) -1)
                        (field "isTrayIconPopup" boolean (accessflags  *class*  *transient* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (ldc 2))         ;;STRING:: ""
                                      (3 (invokespecial
					(methodCP "<init>" "java.awt.PopupMenu" ((class "java.lang.String")) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.awt.Menu" ((class "java.lang.String")) void)))
                                      (5 (aload_0))
                                      (6 (iconst_0))
                                      (7 (putfield (fieldCP "isTrayIconPopup" "java.awt.PopupMenu" boolean)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getParent"
                              (parameters )
                              (returntype . (class "java.awt.MenuContainer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "isTrayIconPopup" "java.awt.PopupMenu" boolean))) 
                                      (4 (ifeq 9))  ;;to TAG_0
                                      (7 (aconst_null)) 
                                      (8 (areturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (invokespecial (methodCP "getParent" "java.awt.Menu" () (class "java.awt.MenuContainer")))) 
                                      (13 (areturn)) 
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "constructComponentName"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 41)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (dup)) 
                                      (4 (astore_1)) 
                                      (5 (monitorenter)) 
                                      (6 (new (class "java.lang.StringBuilder"))) ;;at TAG_0
                                      (9 (dup)) 
                                      (10 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (13 (ldc 0)) ;;STRING:: "popup"
                                      (15 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (18 (getstatic (fieldCP "nameCounter" "java.awt.PopupMenu" int))) 
                                      (21 (dup)) 
                                      (22 (iconst_1)) 
                                      (23 (iadd)) 
                                      (24 (putstatic (fieldCP "nameCounter" "java.awt.PopupMenu" int))) 
                                      (27 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (30 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (33 (aload_1)) 
                                      (34 (monitorexit)) 
                                      (35 (areturn)) ;;at TAG_1
                                      (36 (astore_2)) ;;at TAG_2
                                      (37 (aload_1)) 
                                      (38 (monitorexit)) 
                                      (39 (aload_2)) ;;at TAG_3
                                      (40 (athrow)) 
                                      (endofcode 41))
                                   (Exceptions 
                                     (handler 6 35  36 (class "java.lang.Throwable"))
                                     (handler 36 39  36 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "addNotify"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 98)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getTreeLock" "java.awt.PopupMenu" () (class "java.lang.Object")))) 
                                      (4 (dup)) 
                                      (5 (astore_1)) 
                                      (6 (monitorenter)) 
                                      (7 (aload_0)) ;;at TAG_5
                                      (8 (getfield (fieldCP "parent" "java.awt.PopupMenu" (class "java.awt.MenuContainer")))) 
                                      (11 (ifnull 31)) ;;to TAG_0
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "parent" "java.awt.PopupMenu" (class "java.awt.MenuContainer")))) 
                                      (18 (instanceof (class "java.awt.Component"))) 
                                      (21 (ifne 31)) ;;to TAG_0
                                      (24 (aload_0)) 
                                      (25 (invokespecial (methodCP "addNotify" "java.awt.Menu" () void))) 
                                      (28 (goto 85))  ;;to TAG_1
                                      (31 (aload_0)) ;;at TAG_0
                                      (32 (getfield (fieldCP "peer" "java.awt.PopupMenu" (class "java.awt.peer.MenuComponentPeer")))) 
                                      (35 (ifnonnull 49)) ;;to TAG_2
                                      (38 (aload_0)) 
                                      (39 (invokestatic (methodCP "getDefaultToolkit" "java.awt.Toolkit" () (class "java.awt.Toolkit")))) 
                                      (42 (aload_0)) 
                                      (43 (invokevirtual (methodCP "createPopupMenu" "java.awt.Toolkit" ((class "java.awt.PopupMenu")) (class "java.awt.peer.PopupMenuPeer")))) 
                                      (46 (putfield (fieldCP "peer" "java.awt.PopupMenu" (class "java.awt.peer.MenuComponentPeer")))) 
                                      (49 (aload_0)) ;;at TAG_2
                                      (50 (invokevirtual (methodCP "getItemCount" "java.awt.PopupMenu" () int))) 
                                      (53 (istore_2)) 
                                      (54 (iconst_0)) 
                                      (55 (istore_3)) 
                                      (56 (iload_3)) ;;at TAG_3
                                      (57 (iload_2)) 
                                      (58 (if_icmpge 85))  ;;to TAG_1
                                      (61 (aload_0)) 
                                      (62 (iload_3)) 
                                      (63 (invokevirtual (methodCP "getItem" "java.awt.PopupMenu" (int) (class "java.awt.MenuItem")))) 
                                      (66 (astore 4)) 
                                      (68 (aload 4)) 
                                      (70 (aload_0)) 
                                      (71 (putfield (fieldCP "parent" "java.awt.MenuItem" (class "java.awt.MenuContainer")))) 
                                      (74 (aload 4)) 
                                      (76 (invokevirtual (methodCP "addNotify" "java.awt.MenuItem" () void))) 
                                      (79 (iinc 3 1)) 
                                      (82 (goto 56)) ;;to TAG_3
                                      (85 (aload_1)) ;;at TAG_1
                                      (86 (monitorexit)) 
                                      (87 (goto 97)) ;;to TAG_4;;at TAG_6
                                      (90 (astore 5)) ;;at TAG_7
                                      (92 (aload_1)) 
                                      (93 (monitorexit)) 
                                      (94 (aload 5)) ;;at TAG_8
                                      (96 (athrow)) 
                                      (97 (return)) ;;at TAG_4
                                      (endofcode 98))
                                   (Exceptions 
                                     (handler 7 87  90 (class "java.lang.Throwable"))
                                     (handler 90 94  90 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "show"
                              (parameters (class "java.awt.Component") int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 11) (max_locals . 8) (code_length . 187)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "parent" "java.awt.PopupMenu" (class "java.awt.MenuContainer")))) 
                                      (4 (astore 4)) 
                                      (6 (aload 4)) 
                                      (8 (ifnonnull 21)) ;;to TAG_0
                                      (11 (new (class "java.lang.NullPointerException"))) 
                                      (14 (dup)) 
                                      (15 (ldc 3)) ;;STRING:: "parent is null"
                                      (17 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (20 (athrow)) 
                                      (21 (aload 4)) ;;at TAG_0
                                      (23 (instanceof (class "java.awt.Component"))) 
                                      (26 (ifne 39))  ;;to TAG_1
                                      (29 (new (class "java.lang.IllegalArgumentException"))) 
                                      (32 (dup)) 
                                      (33 (ldc 4)) ;;STRING:: "PopupMenus with non-Component parents cannot be shown"
                                      (35 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (38 (athrow)) 
                                      (39 (aload 4)) ;;at TAG_1
                                      (41 (checkcast (class "java.awt.Component"))) 
                                      (44 (astore 5)) 
                                      (46 (aload 5)) 
                                      (48 (aload_1)) 
                                      (49 (if_acmpeq 92)) ;;to TAG_2
                                      (52 (aload 5)) 
                                      (54 (instanceof (class "java.awt.Container"))) 
                                      (57 (ifeq 82)) ;;to TAG_3
                                      (60 (aload 5)) 
                                      (62 (checkcast (class "java.awt.Container"))) 
                                      (65 (aload_1)) 
                                      (66 (invokevirtual (methodCP "isAncestorOf" "java.awt.Container" ((class "java.awt.Component")) boolean))) 
                                      (69 (ifne 92)) ;;to TAG_2
                                      (72 (new (class "java.lang.IllegalArgumentException"))) 
                                      (75 (dup)) 
                                      (76 (ldc 5)) ;;STRING:: "origin not in parent\ns hierarchy"
                                      (78 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (81 (athrow)) 
                                      (82 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_3
                                      (85 (dup)) 
                                      (86 (ldc 5)) ;;STRING:: "origin not in parent\ns hierarchy"
                                      (88 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (91 (athrow)) 
                                      (92 (aload 5)) ;;at TAG_2
                                      (94 (invokevirtual (methodCP "getPeer" "java.awt.Component" () (class "java.awt.peer.ComponentPeer")))) 
                                      (97 (ifnull 108)) ;;to TAG_4
                                      (100 (aload 5)) 
                                      (102 (invokevirtual (methodCP "isShowing" "java.awt.Component" () boolean))) 
                                      (105 (ifne 118)) ;;to TAG_5
                                      (108 (new (class "java.lang.RuntimeException"))) ;;at TAG_4
                                      (111 (dup)) 
                                      (112 (ldc 6)) ;;STRING:: "parent not showing on screen"
                                      (114 (invokespecial (methodCP "<init>" "java.lang.RuntimeException" ((class "java.lang.String")) void))) 
                                      (117 (athrow)) 
                                      (118 (aload_0)) ;;at TAG_5
                                      (119 (getfield (fieldCP "peer" "java.awt.PopupMenu" (class "java.awt.peer.MenuComponentPeer")))) 
                                      (122 (ifnonnull 129)) ;;to TAG_6
                                      (125 (aload_0)) 
                                      (126 (invokevirtual (methodCP "addNotify" "java.awt.PopupMenu" () void))) 
                                      (129 (aload_0)) ;;at TAG_6
                                      (130 (invokevirtual (methodCP "getTreeLock" "java.awt.PopupMenu" () (class "java.lang.Object")))) 
                                      (133 (dup)) 
                                      (134 (astore 6)) 
                                      (136 (monitorenter)) 
                                      (137 (aload_0)) ;;at TAG_9
                                      (138 (getfield (fieldCP "peer" "java.awt.PopupMenu" (class "java.awt.peer.MenuComponentPeer")))) 
                                      (141 (ifnull 172)) ;;to TAG_7
                                      (144 (aload_0)) 
                                      (145 (getfield (fieldCP "peer" "java.awt.PopupMenu" (class "java.awt.peer.MenuComponentPeer")))) 
                                      (148 (checkcast (class "java.awt.peer.PopupMenuPeer"))) 
                                      (151 (new (class "java.awt.Event"))) 
                                      (154 (dup)) 
                                      (155 (aload_1)) 
                                      (156 (lconst_0)) 
                                      (157 (sipush 501)) 
                                      (160 (iload_2)) 
                                      (161 (iload_3)) 
                                      (162 (iconst_0)) 
                                      (163 (iconst_0)) 
                                      (164 (invokespecial (methodCP "<init>" "java.awt.Event" ((class "java.lang.Object") long int int int int int) void))) 
                                      (167 (invokeinterface (methodCP "show" "java.awt.peer.PopupMenuPeer" ((class "java.awt.Event")) void) 2)) 
                                      (172 (aload 6)) ;;at TAG_7
                                      (174 (monitorexit)) 
                                      (175 (goto 186)) ;;to TAG_8;;at TAG_10
                                      (178 (astore 7)) ;;at TAG_11
                                      (180 (aload 6)) 
                                      (182 (monitorexit)) 
                                      (183 (aload 7)) ;;at TAG_12
                                      (185 (athrow)) 
                                      (186 (return)) ;;at TAG_8
                                      (endofcode 187))
                                   (Exceptions 
                                     (handler 137 175  178 (class "java.lang.Throwable"))
                                     (handler 178 183  178 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "getAccessibleContext"
                              (parameters )
                              (returntype . (class "javax.accessibility.AccessibleContext"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "accessibleContext" "java.awt.PopupMenu" (class "javax.accessibility.AccessibleContext")))) 
                                      (4 (ifnonnull 19))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (new (class "java.awt.PopupMenu$AccessibleAWTPopupMenu"))) 
                                      (11 (dup)) 
                                      (12 (aload_0)) 
                                      (13 (invokespecial (methodCP "<init>" "java.awt.PopupMenu$AccessibleAWTPopupMenu" ((class "java.awt.PopupMenu")) void))) 
                                      (16 (putfield (fieldCP "accessibleContext" "java.awt.PopupMenu" (class "javax.accessibility.AccessibleContext")))) 
                                      (19 (aload_0)) ;;at TAG_0
                                      (20 (getfield (fieldCP "accessibleContext" "java.awt.PopupMenu" (class "javax.accessibility.AccessibleContext")))) 
                                      (23 (areturn)) 
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 15)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (putstatic (fieldCP "nameCounter" "java.awt.PopupMenu" int)))
                                      (4 (new (class "java.awt.PopupMenu$1")))
                                      (7 (dup))
                                      (8 (invokespecial
					(methodCP "<init>" "java.awt.PopupMenu$1" () void)))
                                      (11 (invokestatic
					(methodCP "setPopupMenuAccessor" "sun.awt.AWTAccessor" ((class "sun.awt.AWTAccessor$PopupMenuAccessor")) void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *PopupMenu-class-table*
  (make-static-class-decls 
   *java.awt.PopupMenu*))

(defconst *package-name-map* 
  ("java.awt.PopupMenu" . "java.awt"))

