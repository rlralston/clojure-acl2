; MenuShortcut-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.MenuShortcut*
 (make-class-def
      '(class "java.awt.MenuShortcut"
            "java.lang.Object"
            (constant_pool
                        (LONG 143448358473180225)
                        (STRING  "+")
                        (STRING  "key=")
                        (STRING  ",usesShiftModifier"))
            (fields
                        (field "key" int (accessflags  *class* ) -1)
                        (field "usesShift" boolean (accessflags  *class* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (iconst_0))
                                      (3 (invokespecial
					(methodCP "<init>" "java.awt.MenuShortcut" (int boolean) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters int boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (putfield (fieldCP "key" "java.awt.MenuShortcut" int)))
                                      (9 (aload_0))
                                      (10 (iload_2))
                                      (11 (putfield (fieldCP "usesShift" "java.awt.MenuShortcut" boolean)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getKey"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "key" "java.awt.MenuShortcut" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "usesShiftModifier"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "usesShift" "java.awt.MenuShortcut" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.awt.MenuShortcut"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnull 30))  ;;to TAG_0
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "getKey" "java.awt.MenuShortcut" () int))) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "key" "java.awt.MenuShortcut" int))) 
                                      (12 (if_icmpne 30))  ;;to TAG_0
                                      (15 (aload_1)) 
                                      (16 (invokevirtual (methodCP "usesShiftModifier" "java.awt.MenuShortcut" () boolean))) 
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "usesShift" "java.awt.MenuShortcut" boolean))) 
                                      (23 (if_icmpne 30))  ;;to TAG_0
                                      (26 (iconst_1)) 
                                      (27 (goto 31)) ;;to TAG_1
                                      (30 (iconst_0)) ;;at TAG_0
                                      (31 (ireturn)) ;;at TAG_1
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.awt.MenuShortcut"))) 
                                      (4 (ifeq 16))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (aload_1)) 
                                      (9 (checkcast (class "java.awt.MenuShortcut"))) 
                                      (12 (invokevirtual (methodCP "equals" "java.awt.MenuShortcut" ((class "java.awt.MenuShortcut")) boolean))) 
                                      (15 (ireturn)) 
                                      (16 (iconst_0)) ;;at TAG_0
                                      (17 (ireturn)) 
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "usesShift" "java.awt.MenuShortcut" boolean))) 
                                      (4 (ifeq 16))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "key" "java.awt.MenuShortcut" int))) 
                                      (11 (iconst_m1)) 
                                      (12 (ixor)) 
                                      (13 (goto 20)) ;;to TAG_1
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (getfield (fieldCP "key" "java.awt.MenuShortcut" int))) 
                                      (20 (ireturn)) ;;at TAG_1
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 59)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (invokestatic (methodCP "isHeadless" "java.awt.GraphicsEnvironment" () boolean))) 
                                      (5 (ifne 15))  ;;to TAG_0
                                      (8 (invokestatic (methodCP "getDefaultToolkit" "java.awt.Toolkit" () (class "java.awt.Toolkit")))) 
                                      (11 (invokevirtual (methodCP "getMenuShortcutKeyMask" "java.awt.Toolkit" () int))) 
                                      (14 (istore_1)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (invokevirtual (methodCP "usesShiftModifier" "java.awt.MenuShortcut" () boolean))) 
                                      (19 (ifeq 26)) ;;to TAG_1
                                      (22 (iload_1)) 
                                      (23 (iconst_1)) 
                                      (24 (ior)) 
                                      (25 (istore_1)) 
                                      (26 (new (class "java.lang.StringBuilder"))) ;;at TAG_1
                                      (29 (dup)) 
                                      (30 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (33 (iload_1)) 
                                      (34 (invokestatic (methodCP "getKeyModifiersText" "java.awt.event.KeyEvent" (int) (class "java.lang.String")))) 
                                      (37 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (40 (ldc 1)) ;;STRING:: "+"
                                      (42 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "key" "java.awt.MenuShortcut" int))) 
                                      (49 (invokestatic (methodCP "getKeyText" "java.awt.event.KeyEvent" (int) (class "java.lang.String")))) 
                                      (52 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (55 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (58 (areturn)) 
                                      (endofcode 59))
                                   (Exceptions )
                                   (StackMap )))
                        (method "paramString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 52)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder"))) 
                                      (3 (dup)) 
                                      (4 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (7 (ldc 2)) ;;STRING:: "key="
                                      (9 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "key" "java.awt.MenuShortcut" int))) 
                                      (16 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (19 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (22 (astore_1)) 
                                      (23 (aload_0)) 
                                      (24 (invokevirtual (methodCP "usesShiftModifier" "java.awt.MenuShortcut" () boolean))) 
                                      (27 (ifeq 50))  ;;to TAG_0
                                      (30 (new (class "java.lang.StringBuilder"))) 
                                      (33 (dup)) 
                                      (34 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (37 (aload_1)) 
                                      (38 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (41 (ldc 3)) ;;STRING:: ",usesShiftModifier"
                                      (43 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (46 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (49 (astore_1)) 
                                      (50 (aload_1)) ;;at TAG_0
                                      (51 (areturn)) 
                                      (endofcode 52))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *MenuShortcut-class-table*
  (make-static-class-decls 
   *java.awt.MenuShortcut*))

(defconst *package-name-map* 
  ("java.awt.MenuShortcut" . "java.awt"))

