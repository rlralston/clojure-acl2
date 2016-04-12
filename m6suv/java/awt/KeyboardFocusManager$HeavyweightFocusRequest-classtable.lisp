; KeyboardFocusManager$HeavyweightFocusRequest-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.KeyboardFocusManager$HeavyweightFocusRequest*
 (make-class-def
      '(class "java.awt.KeyboardFocusManager$HeavyweightFocusRequest"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Assertion (heavyweight != null) failed")
                        (STRING  "Assertion (this != HeavyweightFocusRequest.CLEAR_GLOBAL_FOCUS_OWNER) failed")
                        (STRING  "Assertion (descendant != null) failed")
                        (STRING  "HeavyweightFocusRequest[heavweight=")
                        (STRING  ",lightweightRequests=")
                        (STRING  "[")
                        (STRING  ",")
                        (STRING  "]"))
            (fields
                        (field "heavyweight" (class "java.awt.Component") (accessflags  *class*  *final* ) -1)
                        (field "lightweightRequests" (class "java.util.LinkedList") (accessflags  *class*  *final* ) -1)
                        (field "CLEAR_GLOBAL_FOCUS_OWNER" (class "java.awt.KeyboardFocusManager$HeavyweightFocusRequest") (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (putfield (fieldCP "heavyweight" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.awt.Component"))))
                                      (9 (aload_0))
                                      (10 (aconst_null))
                                      (11 (putfield (fieldCP "lightweightRequests" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.util.LinkedList"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.awt.Component") (class "java.awt.Component") boolean (class "sun.awt.CausedFocusEvent$Cause"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 54)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (invokestatic (methodCP "access$000" "java.awt.KeyboardFocusManager" () (class "sun.util.logging.PlatformLogger")))) 
                                      (7 (sipush 500)) 
                                      (10 (invokevirtual (methodCP "isLoggable" "sun.util.logging.PlatformLogger" (int) boolean))) 
                                      (13 (ifeq 28))  ;;to TAG_0
                                      (16 (aload_1)) 
                                      (17 (ifnonnull 28))  ;;to TAG_0
                                      (20 (invokestatic (methodCP "access$000" "java.awt.KeyboardFocusManager" () (class "sun.util.logging.PlatformLogger")))) 
                                      (23 (ldc 0)) ;;STRING:: "Assertion (heavyweight != null) failed"
                                      (25 (invokevirtual (methodCP "fine" "sun.util.logging.PlatformLogger" ((class "java.lang.String")) void))) 
                                      (28 (aload_0)) ;;at TAG_0
                                      (29 (aload_1)) 
                                      (30 (putfield (fieldCP "heavyweight" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.awt.Component")))) 
                                      (33 (aload_0)) 
                                      (34 (new (class "java.util.LinkedList"))) 
                                      (37 (dup)) 
                                      (38 (invokespecial (methodCP "<init>" "java.util.LinkedList" () void))) 
                                      (41 (putfield (fieldCP "lightweightRequests" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.util.LinkedList")))) 
                                      (44 (aload_0)) 
                                      (45 (aload_2)) 
                                      (46 (iload_3)) 
                                      (47 (aload 4)) 
                                      (49 (invokevirtual (methodCP "addLightweightRequest" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" ((class "java.awt.Component") boolean (class "sun.awt.CausedFocusEvent$Cause")) boolean))) 
                                      (52 (pop)) 
                                      (53 (return)) 
                                      (endofcode 54))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addLightweightRequest"
                              (parameters (class "java.awt.Component") boolean (class "sun.awt.CausedFocusEvent$Cause"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 96)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "access$000" "java.awt.KeyboardFocusManager" () (class "sun.util.logging.PlatformLogger")))) 
                                      (3 (sipush 500)) 
                                      (6 (invokevirtual (methodCP "isLoggable" "sun.util.logging.PlatformLogger" (int) boolean))) 
                                      (9 (ifeq 39)) ;;to TAG_0
                                      (12 (aload_0)) 
                                      (13 (getstatic (fieldCP "CLEAR_GLOBAL_FOCUS_OWNER" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.awt.KeyboardFocusManager$HeavyweightFocusRequest")))) 
                                      (16 (if_acmpne 27)) ;;to TAG_1
                                      (19 (invokestatic (methodCP "access$000" "java.awt.KeyboardFocusManager" () (class "sun.util.logging.PlatformLogger")))) 
                                      (22 (ldc 1)) ;;STRING:: "Assertion (this != HeavyweightFocusRequest.CLEAR_GLOBAL_FOCUS_OWNER) failed"
                                      (24 (invokevirtual (methodCP "fine" "sun.util.logging.PlatformLogger" ((class "java.lang.String")) void))) 
                                      (27 (aload_1)) ;;at TAG_1
                                      (28 (ifnonnull 39)) ;;to TAG_0
                                      (31 (invokestatic (methodCP "access$000" "java.awt.KeyboardFocusManager" () (class "sun.util.logging.PlatformLogger")))) 
                                      (34 (ldc 2)) ;;STRING:: "Assertion (descendant != null) failed"
                                      (36 (invokevirtual (methodCP "fine" "sun.util.logging.PlatformLogger" ((class "java.lang.String")) void))) 
                                      (39 (aload_0)) ;;at TAG_0
                                      (40 (getfield (fieldCP "lightweightRequests" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.util.LinkedList")))) 
                                      (43 (invokevirtual (methodCP "size" "java.util.LinkedList" () int))) 
                                      (46 (ifle 65))  ;;to TAG_2
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "lightweightRequests" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.util.LinkedList")))) 
                                      (53 (invokevirtual (methodCP "getLast" "java.util.LinkedList" () (class "java.lang.Object")))) 
                                      (56 (checkcast (class "java.awt.KeyboardFocusManager$LightweightFocusRequest"))) 
                                      (59 (getfield (fieldCP "component" "java.awt.KeyboardFocusManager$LightweightFocusRequest" (class "java.awt.Component")))) 
                                      (62 (goto 66)) ;;to TAG_3
                                      (65 (aconst_null)) ;;at TAG_2
                                      (66 (astore 4)) ;;at TAG_3
                                      (68 (aload_1)) 
                                      (69 (aload 4)) 
                                      (71 (if_acmpeq 94)) ;;to TAG_4
                                      (74 (aload_0)) 
                                      (75 (getfield (fieldCP "lightweightRequests" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.util.LinkedList")))) 
                                      (78 (new (class "java.awt.KeyboardFocusManager$LightweightFocusRequest"))) 
                                      (81 (dup)) 
                                      (82 (aload_1)) 
                                      (83 (iload_2)) 
                                      (84 (aload_3)) 
                                      (85 (invokespecial (methodCP "<init>" "java.awt.KeyboardFocusManager$LightweightFocusRequest" ((class "java.awt.Component") boolean (class "sun.awt.CausedFocusEvent$Cause")) void))) 
                                      (88 (invokevirtual (methodCP "add" "java.util.LinkedList" ((class "java.lang.Object")) boolean))) 
                                      (91 (pop)) 
                                      (92 (iconst_1)) 
                                      (93 (ireturn)) 
                                      (94 (iconst_0)) ;;at TAG_4
                                      (95 (ireturn)) 
                                      (endofcode 96))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFirstLightweightRequest"
                              (parameters )
                              (returntype . (class "java.awt.KeyboardFocusManager$LightweightFocusRequest"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getstatic (fieldCP "CLEAR_GLOBAL_FOCUS_OWNER" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.awt.KeyboardFocusManager$HeavyweightFocusRequest")))) 
                                      (4 (if_acmpne 9))  ;;to TAG_0
                                      (7 (aconst_null)) 
                                      (8 (areturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (getfield (fieldCP "lightweightRequests" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.util.LinkedList")))) 
                                      (13 (invokevirtual (methodCP "getFirst" "java.util.LinkedList" () (class "java.lang.Object")))) 
                                      (16 (checkcast (class "java.awt.KeyboardFocusManager$LightweightFocusRequest"))) 
                                      (19 (areturn)) 
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 201)
                                   (parsedcode
                                      (0 (iconst_1)) 
                                      (1 (istore_1)) 
                                      (2 (new (class "java.lang.StringBuilder"))) 
                                      (5 (dup)) 
                                      (6 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (9 (ldc 3)) ;;STRING:: "HeavyweightFocusRequest[heavweight="
                                      (11 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "heavyweight" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.awt.Component")))) 
                                      (18 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (21 (ldc 4)) ;;STRING:: ",lightweightRequests="
                                      (23 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (26 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (29 (astore_2)) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "lightweightRequests" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.util.LinkedList")))) 
                                      (34 (ifnonnull 59)) ;;to TAG_0
                                      (37 (new (class "java.lang.StringBuilder"))) 
                                      (40 (dup)) 
                                      (41 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (44 (aload_2)) 
                                      (45 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (48 (aconst_null)) 
                                      (49 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (52 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (55 (astore_2)) 
                                      (56 (goto 179)) ;;to TAG_1
                                      (59 (new (class "java.lang.StringBuilder"))) ;;at TAG_0
                                      (62 (dup)) 
                                      (63 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (66 (aload_2)) 
                                      (67 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (70 (ldc 5)) ;;STRING:: "["
                                      (72 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (75 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (78 (astore_2)) 
                                      (79 (aload_0)) 
                                      (80 (getfield (fieldCP "lightweightRequests" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.util.LinkedList")))) 
                                      (83 (invokevirtual (methodCP "iterator" "java.util.LinkedList" () (class "java.util.Iterator")))) 
                                      (86 (astore_3)) 
                                      (87 (aload_3)) ;;at TAG_5
                                      (88 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (93 (ifeq 159))  ;;to TAG_2
                                      (96 (aload_3)) 
                                      (97 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (102 (checkcast (class "java.awt.KeyboardFocusManager$LightweightFocusRequest"))) 
                                      (105 (astore 4)) 
                                      (107 (iload_1)) 
                                      (108 (ifeq 116)) ;;to TAG_3
                                      (111 (iconst_0)) 
                                      (112 (istore_1)) 
                                      (113 (goto 136)) ;;to TAG_4
                                      (116 (new (class "java.lang.StringBuilder"))) ;;at TAG_3
                                      (119 (dup)) 
                                      (120 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (123 (aload_2)) 
                                      (124 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (127 (ldc 6)) ;;STRING:: ","
                                      (129 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (132 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (135 (astore_2)) 
                                      (136 (new (class "java.lang.StringBuilder"))) ;;at TAG_4
                                      (139 (dup)) 
                                      (140 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (143 (aload_2)) 
                                      (144 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (147 (aload 4)) 
                                      (149 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (152 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (155 (astore_2)) 
                                      (156 (goto 87)) ;;to TAG_5
                                      (159 (new (class "java.lang.StringBuilder"))) ;;at TAG_2
                                      (162 (dup)) 
                                      (163 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (166 (aload_2)) 
                                      (167 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (170 (ldc 7)) ;;STRING:: "]"
                                      (172 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (175 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (178 (astore_2)) 
                                      (179 (new (class "java.lang.StringBuilder"))) ;;at TAG_1
                                      (182 (dup)) 
                                      (183 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (186 (aload_2)) 
                                      (187 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (190 (ldc 7)) ;;STRING:: "]"
                                      (192 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (195 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (198 (astore_2)) 
                                      (199 (aload_2)) 
                                      (200 (areturn)) 
                                      (endofcode 201))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.awt.KeyboardFocusManager$HeavyweightFocusRequest")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" () void)))
                                      (7 (putstatic (fieldCP "CLEAR_GLOBAL_FOCUS_OWNER" "java.awt.KeyboardFocusManager$HeavyweightFocusRequest" (class "java.awt.KeyboardFocusManager$HeavyweightFocusRequest"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *KeyboardFocusManager$HeavyweightFocusRequest-class-table*
  (make-static-class-decls 
   *java.awt.KeyboardFocusManager$HeavyweightFocusRequest*))

(defconst *package-name-map* 
  ("java.awt.KeyboardFocusManager$HeavyweightFocusRequest" . "java.awt"))

