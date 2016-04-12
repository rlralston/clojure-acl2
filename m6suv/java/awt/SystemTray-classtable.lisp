; SystemTray-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.SystemTray*
 (make-class-def
      '(class "java.awt.SystemTray"
            "java.lang.Object"
            (constant_pool
                        (STRING  "The system tray is not supported on the current platform.")
                        (STRING  "adding null TrayIcon")
                        (STRING  "adding TrayIcon that is already added")
                        (STRING  "trayIcons"))
            (fields
                        (field "systemTray" (class "java.awt.SystemTray") (accessflags  *class*  *private*  *static* ) -1)
                        (field "currentIconID" int (accessflags  *class*  *private* ) -1)
                        (field "peer" (class "java.awt.peer.SystemTrayPeer") (accessflags  *class*  *private*  *transient* ) -1)
                        (field "EMPTY_TRAY_ARRAY" (array (class "java.awt.TrayIcon")) (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "currentIconID" "java.awt.SystemTray" int)))
                                      (9 (aload_0))
                                      (10 (invokevirtual
					(methodCP "addNotify" "java.awt.SystemTray" () void)))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getSystemTray"
                              (parameters )
                              (returntype . (class "java.awt.SystemTray"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 0) (code_length . 40)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "checkSystemTrayAllowed" "java.awt.SystemTray" () void))) 
                                      (3 (invokestatic (methodCP "isHeadless" "java.awt.GraphicsEnvironment" () boolean))) 
                                      (6 (ifeq 17))  ;;to TAG_0
                                      (9 (new (class "java.awt.HeadlessException"))) 
                                      (12 (dup)) 
                                      (13 (invokespecial (methodCP "<init>" "java.awt.HeadlessException" () void))) 
                                      (16 (athrow)) 
                                      (17 (invokestatic (methodCP "initializeSystemTrayIfNeeded" "java.awt.SystemTray" () void))) ;;at TAG_0
                                      (20 (invokestatic (methodCP "isSupported" "java.awt.SystemTray" () boolean))) 
                                      (23 (ifne 36)) ;;to TAG_1
                                      (26 (new (class "java.lang.UnsupportedOperationException"))) 
                                      (29 (dup)) 
                                      (30 (ldc 0)) ;;STRING:: "The system tray is not supported on the current platform."
                                      (32 (invokespecial (methodCP "<init>" "java.lang.UnsupportedOperationException" ((class "java.lang.String")) void))) 
                                      (35 (athrow)) 
                                      (36 (getstatic (fieldCP "systemTray" "java.awt.SystemTray" (class "java.awt.SystemTray")))) ;;at TAG_1
                                      (39 (areturn)) 
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isSupported"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 39)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getDefaultToolkit" "java.awt.Toolkit" () (class "java.awt.Toolkit")))) 
                                      (3 (astore_0)) 
                                      (4 (aload_0)) 
                                      (5 (instanceof (class "sun.awt.SunToolkit"))) 
                                      (8 (ifeq 22))  ;;to TAG_0
                                      (11 (invokestatic (methodCP "initializeSystemTrayIfNeeded" "java.awt.SystemTray" () void))) 
                                      (14 (aload_0)) 
                                      (15 (checkcast (class "sun.awt.SunToolkit"))) 
                                      (18 (invokevirtual (methodCP "isTraySupported" "sun.awt.SunToolkit" () boolean))) 
                                      (21 (ireturn)) 
                                      (22 (aload_0)) ;;at TAG_0
                                      (23 (instanceof (class "sun.awt.HeadlessToolkit"))) 
                                      (26 (ifeq 37)) ;;to TAG_1
                                      (29 (aload_0)) 
                                      (30 (checkcast (class "sun.awt.HeadlessToolkit"))) 
                                      (33 (invokevirtual (methodCP "isTraySupported" "sun.awt.HeadlessToolkit" () boolean))) 
                                      (36 (ireturn)) 
                                      (37 (iconst_0)) ;;at TAG_1
                                      (38 (ireturn)) 
                                      (endofcode 39))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.awt.TrayIcon"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 7) (code_length . 167)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 14)) ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (ldc 1)) ;;STRING:: "adding null TrayIcon"
                                      (10 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (13 (athrow)) 
                                      (14 (aconst_null)) ;;at TAG_0
                                      (15 (astore_2)) 
                                      (16 (aconst_null)) 
                                      (17 (astore_3)) 
                                      (18 (aconst_null)) 
                                      (19 (astore 4)) 
                                      (21 (aload_0)) 
                                      (22 (dup)) 
                                      (23 (astore 5)) 
                                      (25 (monitorenter)) 
                                      (26 (getstatic (fieldCP "systemTray" "java.awt.SystemTray" (class "java.awt.SystemTray")))) ;;at TAG_5
                                      (29 (invokevirtual (methodCP "getTrayIcons" "java.awt.SystemTray" () (array (class "java.awt.TrayIcon"))))) 
                                      (32 (astore_2)) 
                                      (33 (invokestatic (methodCP "getAppContext" "sun.awt.AppContext" () (class "sun.awt.AppContext")))) 
                                      (36 (ldc_w )) 
                                      (39 (invokevirtual (methodCP "get" "sun.awt.AppContext" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (42 (checkcast (class "java.util.Vector"))) 
                                      (45 (astore 4)) 
                                      (47 (aload 4)) 
                                      (49 (ifnonnull 77))  ;;to TAG_1
                                      (52 (new (class "java.util.Vector"))) 
                                      (55 (dup)) 
                                      (56 (iconst_3)) 
                                      (57 (invokespecial (methodCP "<init>" "java.util.Vector" (int) void))) 
                                      (60 (astore 4)) 
                                      (62 (invokestatic (methodCP "getAppContext" "sun.awt.AppContext" () (class "sun.awt.AppContext")))) 
                                      (65 (ldc_w )) 
                                      (68 (aload 4)) 
                                      (70 (invokevirtual (methodCP "put" "sun.awt.AppContext" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (73 (pop)) 
                                      (74 (goto 96)) ;;to TAG_2
                                      (77 (aload 4)) ;;at TAG_1
                                      (79 (aload_1)) 
                                      (80 (invokevirtual (methodCP "contains" "java.util.Vector" ((class "java.lang.Object")) boolean))) 
                                      (83 (ifeq 96)) ;;to TAG_2
                                      (86 (new (class "java.lang.IllegalArgumentException"))) 
                                      (89 (dup)) 
                                      (90 (ldc 2)) ;;STRING:: "adding TrayIcon that is already added"
                                      (92 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (95 (athrow)) 
                                      (96 (aload 4)) ;;at TAG_2
                                      (98 (aload_1)) 
                                      (99 (invokevirtual (methodCP "add" "java.util.Vector" ((class "java.lang.Object")) boolean))) 
                                      (102 (pop)) 
                                      (103 (getstatic (fieldCP "systemTray" "java.awt.SystemTray" (class "java.awt.SystemTray")))) 
                                      (106 (invokevirtual (methodCP "getTrayIcons" "java.awt.SystemTray" () (array (class "java.awt.TrayIcon"))))) 
                                      (109 (astore_3)) 
                                      (110 (aload_1)) 
                                      (111 (aload_0)) 
                                      (112 (dup)) 
                                      (113 (getfield (fieldCP "currentIconID" "java.awt.SystemTray" int))) 
                                      (116 (iconst_1)) 
                                      (117 (iadd)) 
                                      (118 (dup_x1)) 
                                      (119 (putfield (fieldCP "currentIconID" "java.awt.SystemTray" int))) 
                                      (122 (invokevirtual (methodCP "setID" "java.awt.TrayIcon" (int) void))) 
                                      (125 (aload 5)) 
                                      (127 (monitorexit)) 
                                      (128 (goto 139)) ;;to TAG_3;;at TAG_6
                                      (131 (astore 6)) ;;at TAG_7
                                      (133 (aload 5)) 
                                      (135 (monitorexit)) 
                                      (136 (aload 6)) ;;at TAG_8
                                      (138 (athrow)) 
                                      (139 (aload_1)) ;;at TAG_3
                                      (140 (invokevirtual (methodCP "addNotify" "java.awt.TrayIcon" () void))) 
                                      (143 (goto 158)) ;;to TAG_4;;at TAG_9
                                      (146 (astore 5)) ;;at TAG_10
                                      (148 (aload 4)) 
                                      (150 (aload_1)) 
                                      (151 (invokevirtual (methodCP "remove" "java.util.Vector" ((class "java.lang.Object")) boolean))) 
                                      (154 (pop)) 
                                      (155 (aload 5)) 
                                      (157 (athrow)) 
                                      (158 (aload_0)) ;;at TAG_4
                                      (159 (ldc 3)) ;;STRING:: "trayIcons"
                                      (161 (aload_2)) 
                                      (162 (aload_3)) 
                                      (163 (invokespecial (methodCP "firePropertyChange" "java.awt.SystemTray" ((class "java.lang.String") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (166 (return)) 
                                      (endofcode 167))
                                   (Exceptions 
                                     (handler 26 128  131 (class "java.lang.Throwable"))
                                     (handler 131 136  131 (class "java.lang.Throwable"))
                                     (handler 139 143  146 (class "java.awt.AWTException")))
                                   (StackMap )))
                        (method "remove"
                              (parameters (class "java.awt.TrayIcon"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 7) (code_length . 87)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 5)) ;;to TAG_0
                                      (4 (return)) 
                                      (5 (aconst_null)) ;;at TAG_0
                                      (6 (astore_2)) 
                                      (7 (aconst_null)) 
                                      (8 (astore_3)) 
                                      (9 (aload_0)) 
                                      (10 (dup)) 
                                      (11 (astore 4)) 
                                      (13 (monitorenter)) 
                                      (14 (getstatic (fieldCP "systemTray" "java.awt.SystemTray" (class "java.awt.SystemTray")))) ;;at TAG_4
                                      (17 (invokevirtual (methodCP "getTrayIcons" "java.awt.SystemTray" () (array (class "java.awt.TrayIcon"))))) 
                                      (20 (astore_2)) 
                                      (21 (invokestatic (methodCP "getAppContext" "sun.awt.AppContext" () (class "sun.awt.AppContext")))) 
                                      (24 (ldc_w )) 
                                      (27 (invokevirtual (methodCP "get" "sun.awt.AppContext" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (30 (checkcast (class "java.util.Vector"))) 
                                      (33 (astore 5)) 
                                      (35 (aload 5)) 
                                      (37 (ifnull 49))  ;;to TAG_1
                                      (40 (aload 5)) 
                                      (42 (aload_1)) 
                                      (43 (invokevirtual (methodCP "remove" "java.util.Vector" ((class "java.lang.Object")) boolean))) 
                                      (46 (ifne 53)) ;;to TAG_2
                                      (49 (aload 4)) ;;at TAG_1
                                      (51 (monitorexit)) 
                                      (52 (return)) ;;at TAG_5
                                      (53 (aload_1)) ;;at TAG_2
                                      (54 (invokevirtual (methodCP "removeNotify" "java.awt.TrayIcon" () void))) 
                                      (57 (getstatic (fieldCP "systemTray" "java.awt.SystemTray" (class "java.awt.SystemTray")))) 
                                      (60 (invokevirtual (methodCP "getTrayIcons" "java.awt.SystemTray" () (array (class "java.awt.TrayIcon"))))) 
                                      (63 (astore_3)) 
                                      (64 (aload 4)) 
                                      (66 (monitorexit)) 
                                      (67 (goto 78)) ;;to TAG_3;;at TAG_7
                                      (70 (astore 6)) ;;at TAG_6
                                      (72 (aload 4)) 
                                      (74 (monitorexit)) 
                                      (75 (aload 6)) ;;at TAG_8
                                      (77 (athrow)) 
                                      (78 (aload_0)) ;;at TAG_3
                                      (79 (ldc 3)) ;;STRING:: "trayIcons"
                                      (81 (aload_2)) 
                                      (82 (aload_3)) 
                                      (83 (invokespecial (methodCP "firePropertyChange" "java.awt.SystemTray" ((class "java.lang.String") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (86 (return)) 
                                      (endofcode 87))
                                   (Exceptions 
                                     (handler 14 52  70 (class "java.lang.Throwable"))
                                     (handler 53 67  70 (class "java.lang.Throwable"))
                                     (handler 70 75  70 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "getTrayIcons"
                              (parameters )
                              (returntype . (array (class "java.awt.TrayIcon")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 39)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getAppContext" "sun.awt.AppContext" () (class "sun.awt.AppContext")))) 
                                      (3 (ldc_w )) 
                                      (6 (invokevirtual (methodCP "get" "sun.awt.AppContext" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (9 (checkcast (class "java.util.Vector"))) 
                                      (12 (astore_1)) 
                                      (13 (aload_1)) 
                                      (14 (ifnull 35))  ;;to TAG_0
                                      (17 (aload_1)) 
                                      (18 (aload_1)) 
                                      (19 (invokevirtual (methodCP "size" "java.util.Vector" () int))) 
                                      (22 (anewarray (class "java.awt.TrayIcon"))) 
                                      (25 (invokevirtual (methodCP "toArray" "java.util.Vector" ((array (class "java.lang.Object"))) (array (class "java.lang.Object"))))) 
                                      (28 (checkcast (array (class "java.awt.TrayIcon")))) 
                                      (31 (checkcast (array (class "java.awt.TrayIcon")))) 
                                      (34 (areturn)) 
                                      (35 (getstatic (fieldCP "EMPTY_TRAY_ARRAY" "java.awt.SystemTray" (array (class "java.awt.TrayIcon"))))) ;;at TAG_0
                                      (38 (areturn)) 
                                      (endofcode 39))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getTrayIconSize"
                              (parameters )
                              (returntype . (class "java.awt.Dimension"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "peer" "java.awt.SystemTray" (class "java.awt.peer.SystemTrayPeer"))))
                                      (4 (invokeinterface
					(methodCP "getTrayIconSize" "java.awt.peer.SystemTrayPeer" () (class "java.awt.Dimension")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addPropertyChangeListener"
                              (parameters (class "java.lang.String") (class "java.beans.PropertyChangeListener"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (ifnonnull 5))  ;;to TAG_0
                                      (4 (return)) 
                                      (5 (aload_0)) ;;at TAG_0
                                      (6 (invokespecial (methodCP "getCurrentChangeSupport" "java.awt.SystemTray" () (class "java.beans.PropertyChangeSupport")))) 
                                      (9 (aload_1)) 
                                      (10 (aload_2)) 
                                      (11 (invokevirtual (methodCP "addPropertyChangeListener" "java.beans.PropertyChangeSupport" ((class "java.lang.String") (class "java.beans.PropertyChangeListener")) void))) 
                                      (14 (return)) 
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "removePropertyChangeListener"
                              (parameters (class "java.lang.String") (class "java.beans.PropertyChangeListener"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (ifnonnull 5))  ;;to TAG_0
                                      (4 (return)) 
                                      (5 (aload_0)) ;;at TAG_0
                                      (6 (invokespecial (methodCP "getCurrentChangeSupport" "java.awt.SystemTray" () (class "java.beans.PropertyChangeSupport")))) 
                                      (9 (aload_1)) 
                                      (10 (aload_2)) 
                                      (11 (invokevirtual (methodCP "removePropertyChangeListener" "java.beans.PropertyChangeSupport" ((class "java.lang.String") (class "java.beans.PropertyChangeListener")) void))) 
                                      (14 (return)) 
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPropertyChangeListeners"
                              (parameters (class "java.lang.String"))
                              (returntype . (array (class "java.beans.PropertyChangeListener")))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "getCurrentChangeSupport" "java.awt.SystemTray" () (class "java.beans.PropertyChangeSupport"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "getPropertyChangeListeners" "java.beans.PropertyChangeSupport" ((class "java.lang.String")) (array (class "java.beans.PropertyChangeListener")))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "firePropertyChange"
                              (parameters (class "java.lang.String") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (ifnull 17))  ;;to TAG_0
                                      (4 (aload_3)) 
                                      (5 (ifnull 17))  ;;to TAG_0
                                      (8 (aload_2)) 
                                      (9 (aload_3)) 
                                      (10 (invokevirtual (methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean))) 
                                      (13 (ifeq 17))  ;;to TAG_0
                                      (16 (return)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (invokespecial (methodCP "getCurrentChangeSupport" "java.awt.SystemTray" () (class "java.beans.PropertyChangeSupport")))) 
                                      (21 (aload_1)) 
                                      (22 (aload_2)) 
                                      (23 (aload_3)) 
                                      (24 (invokevirtual (methodCP "firePropertyChange" "java.beans.PropertyChangeSupport" ((class "java.lang.String") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (27 (return)) 
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCurrentChangeSupport"
                              (parameters )
                              (returntype . (class "java.beans.PropertyChangeSupport"))
                              (accessflags  *class*  *private*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 39)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getAppContext" "sun.awt.AppContext" () (class "sun.awt.AppContext")))) 
                                      (3 (ldc_w )) 
                                      (6 (invokevirtual (methodCP "get" "sun.awt.AppContext" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (9 (checkcast (class "java.beans.PropertyChangeSupport"))) 
                                      (12 (astore_1)) 
                                      (13 (aload_1)) 
                                      (14 (ifnonnull 37))  ;;to TAG_0
                                      (17 (new (class "java.beans.PropertyChangeSupport"))) 
                                      (20 (dup)) 
                                      (21 (aload_0)) 
                                      (22 (invokespecial (methodCP "<init>" "java.beans.PropertyChangeSupport" ((class "java.lang.Object")) void))) 
                                      (25 (astore_1)) 
                                      (26 (invokestatic (methodCP "getAppContext" "sun.awt.AppContext" () (class "sun.awt.AppContext")))) 
                                      (29 (ldc_w )) 
                                      (32 (aload_1)) 
                                      (33 (invokevirtual (methodCP "put" "sun.awt.AppContext" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (36 (pop)) 
                                      (37 (aload_1)) ;;at TAG_0
                                      (38 (areturn)) 
                                      (endofcode 39))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addNotify"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 57)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "peer" "java.awt.SystemTray" (class "java.awt.peer.SystemTrayPeer")))) 
                                      (4 (ifnonnull 56))  ;;to TAG_0
                                      (7 (invokestatic (methodCP "getDefaultToolkit" "java.awt.Toolkit" () (class "java.awt.Toolkit")))) 
                                      (10 (astore_1)) 
                                      (11 (aload_1)) 
                                      (12 (instanceof (class "sun.awt.SunToolkit"))) 
                                      (15 (ifeq 35)) ;;to TAG_1
                                      (18 (aload_0)) 
                                      (19 (invokestatic (methodCP "getDefaultToolkit" "java.awt.Toolkit" () (class "java.awt.Toolkit")))) 
                                      (22 (checkcast (class "sun.awt.SunToolkit"))) 
                                      (25 (aload_0)) 
                                      (26 (invokevirtual (methodCP "createSystemTray" "sun.awt.SunToolkit" ((class "java.awt.SystemTray")) (class "java.awt.peer.SystemTrayPeer")))) 
                                      (29 (putfield (fieldCP "peer" "java.awt.SystemTray" (class "java.awt.peer.SystemTrayPeer")))) 
                                      (32 (goto 56))  ;;to TAG_0
                                      (35 (aload_1)) ;;at TAG_1
                                      (36 (instanceof (class "sun.awt.HeadlessToolkit"))) 
                                      (39 (ifeq 56))  ;;to TAG_0
                                      (42 (aload_0)) 
                                      (43 (invokestatic (methodCP "getDefaultToolkit" "java.awt.Toolkit" () (class "java.awt.Toolkit")))) 
                                      (46 (checkcast (class "sun.awt.HeadlessToolkit"))) 
                                      (49 (aload_0)) 
                                      (50 (invokevirtual (methodCP "createSystemTray" "sun.awt.HeadlessToolkit" ((class "java.awt.SystemTray")) (class "java.awt.peer.SystemTrayPeer")))) 
                                      (53 (putfield (fieldCP "peer" "java.awt.SystemTray" (class "java.awt.peer.SystemTrayPeer")))) 
                                      (56 (return)) ;;at TAG_0
                                      (endofcode 57))
                                   (Exceptions )
                                   (StackMap )))
                        (method "checkSystemTrayAllowed"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 16)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getSecurityManager" "java.lang.System" () (class "java.lang.SecurityManager")))) 
                                      (3 (astore_0)) 
                                      (4 (aload_0)) 
                                      (5 (ifnull 15))  ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (getstatic (fieldCP "ACCESS_SYSTEM_TRAY_PERMISSION" "sun.security.util.SecurityConstants$AWT" (class "java.security.Permission")))) 
                                      (12 (invokevirtual (methodCP "checkPermission" "java.lang.SecurityManager" ((class "java.security.Permission")) void))) 
                                      (15 (return)) ;;at TAG_0
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "initializeSystemTrayIfNeeded"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 33)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (dup)) 
                                      (4 (astore_0)) 
                                      (5 (monitorenter)) 
                                      (6 (getstatic (fieldCP "systemTray" "java.awt.SystemTray" (class "java.awt.SystemTray")))) ;;at TAG_2
                                      (9 (ifnonnull 22)) ;;to TAG_0
                                      (12 (new (class "java.awt.SystemTray"))) 
                                      (15 (dup)) 
                                      (16 (invokespecial (methodCP "<init>" "java.awt.SystemTray" () void))) 
                                      (19 (putstatic (fieldCP "systemTray" "java.awt.SystemTray" (class "java.awt.SystemTray")))) 
                                      (22 (aload_0)) ;;at TAG_0
                                      (23 (monitorexit)) 
                                      (24 (goto 32)) ;;to TAG_1;;at TAG_3
                                      (27 (astore_1)) ;;at TAG_4
                                      (28 (aload_0)) 
                                      (29 (monitorexit)) 
                                      (30 (aload_1)) ;;at TAG_5
                                      (31 (athrow)) 
                                      (32 (return)) ;;at TAG_1
                                      (endofcode 33))
                                   (Exceptions 
                                     (handler 6 24  27 (class "java.lang.Throwable"))
                                     (handler 27 30  27 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 8)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (anewarray (class "java.awt.TrayIcon")))
                                      (4 (putstatic (fieldCP "EMPTY_TRAY_ARRAY" "java.awt.SystemTray" (array (class "java.awt.TrayIcon")))))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *SystemTray-class-table*
  (make-static-class-decls 
   *java.awt.SystemTray*))

(defconst *package-name-map* 
  ("java.awt.SystemTray" . "java.awt"))
