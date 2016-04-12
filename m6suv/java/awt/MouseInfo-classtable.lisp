; MouseInfo-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.MouseInfo*
 (make-class-def
      '(class "java.awt.MouseInfo"
            "java.lang.Object"
            (constant_pool
                        (STRING  "awt.mouse.numButtons")
                        (STRING  "awt.mouse.numButtons is not an integer property"))
            (fields
                        (field "$assertionsDisabled" boolean (accessflags  *class*  *final*  *static* ) -1))
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
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPointerInfo"
                              (parameters )
                              (returntype . (class "java.awt.PointerInfo"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 8) (code_length . 143)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "isHeadless" "java.awt.GraphicsEnvironment" () boolean))) 
                                      (3 (ifeq 14)) ;;to TAG_0
                                      (6 (new (class "java.awt.HeadlessException"))) 
                                      (9 (dup)) 
                                      (10 (invokespecial (methodCP "<init>" "java.awt.HeadlessException" () void))) 
                                      (13 (athrow)) 
                                      (14 (invokestatic (methodCP "getSecurityManager" "java.lang.System" () (class "java.lang.SecurityManager")))) ;;at TAG_0
                                      (17 (astore_0)) 
                                      (18 (aload_0)) 
                                      (19 (ifnull 29)) ;;to TAG_1
                                      (22 (aload_0)) 
                                      (23 (getstatic (fieldCP "WATCH_MOUSE_PERMISSION" "sun.security.util.SecurityConstants$AWT" (class "java.security.Permission")))) 
                                      (26 (invokevirtual (methodCP "checkPermission" "java.lang.SecurityManager" ((class "java.security.Permission")) void))) 
                                      (29 (new (class "java.awt.Point"))) ;;at TAG_1
                                      (32 (dup)) 
                                      (33 (iconst_0)) 
                                      (34 (iconst_0)) 
                                      (35 (invokespecial (methodCP "<init>" "java.awt.Point" (int int) void))) 
                                      (38 (astore_1)) 
                                      (39 (invokestatic (methodCP "getDefaultToolkit" "java.awt.Toolkit" () (class "java.awt.Toolkit")))) 
                                      (42 (invokevirtual (methodCP "getMouseInfoPeer" "java.awt.Toolkit" () (class "java.awt.peer.MouseInfoPeer")))) 
                                      (45 (aload_1)) 
                                      (46 (invokeinterface (methodCP "fillPointWithCoords" "java.awt.peer.MouseInfoPeer" ((class "java.awt.Point")) int) 2)) 
                                      (51 (istore_2)) 
                                      (52 (invokestatic (methodCP "getLocalGraphicsEnvironment" "java.awt.GraphicsEnvironment" () (class "java.awt.GraphicsEnvironment")))) 
                                      (55 (invokevirtual (methodCP "getScreenDevices" "java.awt.GraphicsEnvironment" () (array (class "java.awt.GraphicsDevice"))))) 
                                      (58 (astore_3)) 
                                      (59 (aconst_null)) 
                                      (60 (astore 4)) 
                                      (62 (aload_3)) 
                                      (63 (invokestatic (methodCP "areScreenDevicesIndependent" "java.awt.MouseInfo" ((array (class "java.awt.GraphicsDevice"))) boolean))) 
                                      (66 (ifeq 85))  ;;to TAG_2
                                      (69 (new (class "java.awt.PointerInfo"))) 
                                      (72 (dup)) 
                                      (73 (aload_3)) 
                                      (74 (iload_2)) 
                                      (75 (aaload)) 
                                      (76 (aload_1)) 
                                      (77 (invokespecial (methodCP "<init>" "java.awt.PointerInfo" ((class "java.awt.GraphicsDevice") (class "java.awt.Point")) void))) 
                                      (80 (astore 4)) 
                                      (82 (goto 140)) ;;to TAG_3
                                      (85 (iconst_0)) ;;at TAG_2
                                      (86 (istore 5)) 
                                      (88 (iload 5)) ;;at TAG_5
                                      (90 (aload_3)) 
                                      (91 (arraylength)) 
                                      (92 (if_icmpge 140)) ;;to TAG_3
                                      (95 (aload_3)) 
                                      (96 (iload 5)) 
                                      (98 (aaload)) 
                                      (99 (invokevirtual (methodCP "getDefaultConfiguration" "java.awt.GraphicsDevice" () (class "java.awt.GraphicsConfiguration")))) 
                                      (102 (astore 6)) 
                                      (104 (aload 6)) 
                                      (106 (invokevirtual (methodCP "getBounds" "java.awt.GraphicsConfiguration" () (class "java.awt.Rectangle")))) 
                                      (109 (astore 7)) 
                                      (111 (aload 7)) 
                                      (113 (aload_1)) 
                                      (114 (invokevirtual (methodCP "contains" "java.awt.Rectangle" ((class "java.awt.Point")) boolean))) 
                                      (117 (ifeq 134)) ;;to TAG_4
                                      (120 (new (class "java.awt.PointerInfo"))) 
                                      (123 (dup)) 
                                      (124 (aload_3)) 
                                      (125 (iload 5)) 
                                      (127 (aaload)) 
                                      (128 (aload_1)) 
                                      (129 (invokespecial (methodCP "<init>" "java.awt.PointerInfo" ((class "java.awt.GraphicsDevice") (class "java.awt.Point")) void))) 
                                      (132 (astore 4)) 
                                      (134 (iinc 5 1)) ;;at TAG_4
                                      (137 (goto 88)) ;;to TAG_5
                                      (140 (aload 4)) ;;at TAG_3
                                      (142 (areturn)) 
                                      (endofcode 143))
                                   (Exceptions )
                                   (StackMap )))
                        (method "areScreenDevicesIndependent"
                              (parameters (array (class "java.awt.GraphicsDevice")))
                              (returntype . boolean)
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 42)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (iload_1)) ;;at TAG_3
                                      (3 (aload_0)) 
                                      (4 (arraylength)) 
                                      (5 (if_icmpge 40)) ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (iload_1)) 
                                      (10 (aaload)) 
                                      (11 (invokevirtual (methodCP "getDefaultConfiguration" "java.awt.GraphicsDevice" () (class "java.awt.GraphicsConfiguration")))) 
                                      (14 (invokevirtual (methodCP "getBounds" "java.awt.GraphicsConfiguration" () (class "java.awt.Rectangle")))) 
                                      (17 (astore_2)) 
                                      (18 (aload_2)) 
                                      (19 (getfield (fieldCP "x" "java.awt.Rectangle" int))) 
                                      (22 (ifne 32)) ;;to TAG_1
                                      (25 (aload_2)) 
                                      (26 (getfield (fieldCP "y" "java.awt.Rectangle" int))) 
                                      (29 (ifeq 34))  ;;to TAG_2
                                      (32 (iconst_0)) ;;at TAG_1
                                      (33 (ireturn)) 
                                      (34 (iinc 1 1)) ;;at TAG_2
                                      (37 (goto 2)) ;;to TAG_3
                                      (40 (iconst_1)) ;;at TAG_0
                                      (41 (ireturn)) 
                                      (endofcode 42))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getNumberOfButtons"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 56)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "isHeadless" "java.awt.GraphicsEnvironment" () boolean))) 
                                      (3 (ifeq 14)) ;;to TAG_0
                                      (6 (new (class "java.awt.HeadlessException"))) 
                                      (9 (dup)) 
                                      (10 (invokespecial (methodCP "<init>" "java.awt.HeadlessException" () void))) 
                                      (13 (athrow)) 
                                      (14 (invokestatic (methodCP "getDefaultToolkit" "java.awt.Toolkit" () (class "java.awt.Toolkit")))) ;;at TAG_0
                                      (17 (ldc 0)) ;;STRING:: "awt.mouse.numButtons"
                                      (19 (invokevirtual (methodCP "getDesktopProperty" "java.awt.Toolkit" ((class "java.lang.String")) (class "java.lang.Object")))) 
                                      (22 (astore_0)) 
                                      (23 (aload_0)) 
                                      (24 (instanceof (class "java.lang.Integer"))) 
                                      (27 (ifeq 38)) ;;to TAG_1
                                      (30 (aload_0)) 
                                      (31 (checkcast (class "java.lang.Integer"))) 
                                      (34 (invokevirtual (methodCP "intValue" "java.lang.Integer" () int))) 
                                      (37 (ireturn)) 
                                      (38 (getstatic (fieldCP "$assertionsDisabled" "java.awt.MouseInfo" boolean))) ;;at TAG_1
                                      (41 (ifne 54))  ;;to TAG_2
                                      (44 (new (class "java.lang.AssertionError"))) 
                                      (47 (dup)) 
                                      (48 (ldc 1)) ;;STRING:: "awt.mouse.numButtons is not an integer property"
                                      (50 (invokespecial (methodCP "<init>" "java.lang.AssertionError" ((class "java.lang.Object")) void))) 
                                      (53 (athrow)) 
                                      (54 (iconst_0)) ;;at TAG_2
                                      (55 (ireturn)) 
                                      (endofcode 56))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 18)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (invokevirtual (methodCP "desiredAssertionStatus" "java.lang.Class" () boolean))) 
                                      (6 (ifne 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.awt.MouseInfo" boolean))) ;;at TAG_1
                                      (17 (return)) 
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *MouseInfo-class-table*
  (make-static-class-decls 
   *java.awt.MouseInfo*))

(defconst *package-name-map* 
  ("java.awt.MouseInfo" . "java.awt"))
