; MouseWheelEvent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.event.MouseWheelEvent*
 (make-class-def
      '(class "java.awt.event.MouseWheelEvent"
            "java.awt.event.MouseEvent"
            (constant_pool
                        (INT 0)
                        (INT 1)
                        (LONG 6459879390515399677)
                        (STRING  "WHEEL_UNIT_SCROLL")
                        (STRING  "WHEEL_BLOCK_SCROLL")
                        (STRING  "unknown scroll type")
                        (STRING  ",scrollType=")
                        (STRING  ",scrollAmount=")
                        (STRING  ",wheelRotation=")
                        (STRING  ",preciseWheelRotation="))
            (fields
                        (field "WHEEL_UNIT_SCROLL" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "WHEEL_BLOCK_SCROLL" int (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "scrollType" int (accessflags  *class* ) -1)
                        (field "scrollAmount" int (accessflags  *class* ) -1)
                        (field "wheelRotation" int (accessflags  *class* ) -1)
                        (field "preciseWheelRotation" double (accessflags  *class* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 2))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.Component") int long int int int int boolean int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 15) (max_locals . 13) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (lload_3))
                                      (4 (iload 5))
                                      (6 (iload 6))
                                      (8 (iload 7))
                                      (10 (iconst_0))
                                      (11 (iconst_0))
                                      (12 (iload 8))
                                      (14 (iload 9))
                                      (16 (iload 10))
                                      (18 (iload 11))
                                      (20 (iload 12))
                                      (22 (invokespecial
					(methodCP "<init>" "java.awt.event.MouseWheelEvent" ((class "java.awt.Component") int long int int int int int int boolean int int int) void)))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.awt.Component") int long int int int int int int boolean int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 17) (max_locals . 15) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (lload_3))
                                      (4 (iload 5))
                                      (6 (iload 6))
                                      (8 (iload 7))
                                      (10 (iload 8))
                                      (12 (iload 9))
                                      (14 (iload 10))
                                      (16 (iload 11))
                                      (18 (iload 12))
                                      (20 (iload 13))
                                      (22 (iload 14))
                                      (24 (iload 14))
                                      (26 (i2d))
                                      (27 (invokespecial
					(methodCP "<init>" "java.awt.event.MouseWheelEvent" ((class "java.awt.Component") int long int int int int int int boolean int int int double) void)))
                                      (30 (return))
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.awt.Component") int long int int int int int int boolean int int int double)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 13) (max_locals . 17) (code_length . 47)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (lload_3))
                                      (4 (iload 5))
                                      (6 (iload 6))
                                      (8 (iload 7))
                                      (10 (iload 8))
                                      (12 (iload 9))
                                      (14 (iload 10))
                                      (16 (iload 11))
                                      (18 (iconst_0))
                                      (19 (invokespecial
					(methodCP "<init>" "java.awt.event.MouseEvent" ((class "java.awt.Component") int long int int int int int int boolean int) void)))
                                      (22 (aload_0))
                                      (23 (iload 12))
                                      (25 (putfield (fieldCP "scrollType" "java.awt.event.MouseWheelEvent" int)))
                                      (28 (aload_0))
                                      (29 (iload 13))
                                      (31 (putfield (fieldCP "scrollAmount" "java.awt.event.MouseWheelEvent" int)))
                                      (34 (aload_0))
                                      (35 (iload 14))
                                      (37 (putfield (fieldCP "wheelRotation" "java.awt.event.MouseWheelEvent" int)))
                                      (40 (aload_0))
                                      (41 (dload 15))
                                      (43 (putfield (fieldCP "preciseWheelRotation" "java.awt.event.MouseWheelEvent" double)))
                                      (46 (return))
                                      (endofcode 47))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getScrollType"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "scrollType" "java.awt.event.MouseWheelEvent" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getScrollAmount"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "scrollAmount" "java.awt.event.MouseWheelEvent" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getWheelRotation"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "wheelRotation" "java.awt.event.MouseWheelEvent" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPreciseWheelRotation"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "preciseWheelRotation" "java.awt.event.MouseWheelEvent" double)))
                                      (4 (dreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getUnitsToScroll"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "scrollAmount" "java.awt.event.MouseWheelEvent" int)))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "wheelRotation" "java.awt.event.MouseWheelEvent" int)))
                                      (8 (imul))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "paramString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 95)
                                   (parsedcode
                                      (0 (aconst_null)) 
                                      (1 (astore_1)) 
                                      (2 (aload_0)) 
                                      (3 (invokevirtual (methodCP "getScrollType" "java.awt.event.MouseWheelEvent" () int))) 
                                      (6 (ifne 15)) ;;to TAG_0
                                      (9 (ldc 3)) ;;STRING:: "WHEEL_UNIT_SCROLL"
                                      (11 (astore_1)) 
                                      (12 (goto 32)) ;;to TAG_1
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (invokevirtual (methodCP "getScrollType" "java.awt.event.MouseWheelEvent" () int))) 
                                      (19 (iconst_1)) 
                                      (20 (if_icmpne 29))  ;;to TAG_2
                                      (23 (ldc 4)) ;;STRING:: "WHEEL_BLOCK_SCROLL"
                                      (25 (astore_1)) 
                                      (26 (goto 32)) ;;to TAG_1
                                      (29 (ldc 5)) ;;at TAG_2;;STRING:: "unknown scroll type"
                                      (31 (astore_1)) 
                                      (32 (new (class "java.lang.StringBuilder"))) ;;at TAG_1
                                      (35 (dup)) 
                                      (36 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (39 (aload_0)) 
                                      (40 (invokespecial (methodCP "paramString" "java.awt.event.MouseEvent" () (class "java.lang.String")))) 
                                      (43 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (46 (ldc 6)) ;;STRING:: ",scrollType="
                                      (48 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (51 (aload_1)) 
                                      (52 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (55 (ldc 7)) ;;STRING:: ",scrollAmount="
                                      (57 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (60 (aload_0)) 
                                      (61 (invokevirtual (methodCP "getScrollAmount" "java.awt.event.MouseWheelEvent" () int))) 
                                      (64 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (67 (ldc 8)) ;;STRING:: ",wheelRotation="
                                      (69 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (72 (aload_0)) 
                                      (73 (invokevirtual (methodCP "getWheelRotation" "java.awt.event.MouseWheelEvent" () int))) 
                                      (76 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (79 (ldc 9)) ;;STRING:: ",preciseWheelRotation="
                                      (81 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (84 (aload_0)) 
                                      (85 (invokevirtual (methodCP "getPreciseWheelRotation" "java.awt.event.MouseWheelEvent" () double))) 
                                      (88 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (double) (class "java.lang.StringBuilder")))) 
                                      (91 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (94 (areturn)) 
                                      (endofcode 95))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *MouseWheelEvent-class-table*
  (make-static-class-decls 
   *java.awt.event.MouseWheelEvent*))

(defconst *package-name-map* 
  ("java.awt.event.MouseWheelEvent" . "java.awt.event"))

