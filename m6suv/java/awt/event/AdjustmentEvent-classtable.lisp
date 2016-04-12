; AdjustmentEvent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.event.AdjustmentEvent*
 (make-class-def
      '(class "java.awt.event.AdjustmentEvent"
            "java.awt.AWTEvent"
            (constant_pool
                        (INT 601)
                        (INT 1)
                        (INT 2)
                        (INT 3)
                        (INT 4)
                        (INT 5)
                        (LONG 5700290645205279921)
                        (STRING  "ADJUSTMENT_VALUE_CHANGED")
                        (STRING  "unknown type")
                        (STRING  "UNIT_INCREMENT")
                        (STRING  "UNIT_DECREMENT")
                        (STRING  "BLOCK_INCREMENT")
                        (STRING  "BLOCK_DECREMENT")
                        (STRING  "TRACK")
                        (STRING  ",adjType=")
                        (STRING  ",value=")
                        (STRING  ",isAdjusting="))
            (fields
                        (field "ADJUSTMENT_FIRST" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "ADJUSTMENT_LAST" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "ADJUSTMENT_VALUE_CHANGED" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "UNIT_INCREMENT" int (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "UNIT_DECREMENT" int (accessflags  *class*  *final*  *public*  *static* ) 2)
                        (field "BLOCK_DECREMENT" int (accessflags  *class*  *final*  *public*  *static* ) 3)
                        (field "BLOCK_INCREMENT" int (accessflags  *class*  *final*  *public*  *static* ) 4)
                        (field "TRACK" int (accessflags  *class*  *final*  *public*  *static* ) 5)
                        (field "adjustable" (class "java.awt.Adjustable") (accessflags  *class* ) -1)
                        (field "value" int (accessflags  *class* ) -1)
                        (field "adjustmentType" int (accessflags  *class* ) -1)
                        (field "isAdjusting" boolean (accessflags  *class* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 6))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.Adjustable") int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (iload_3))
                                      (4 (iload 4))
                                      (6 (iconst_0))
                                      (7 (invokespecial
					(methodCP "<init>" "java.awt.event.AdjustmentEvent" ((class "java.awt.Adjustable") int int int boolean) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.awt.Adjustable") int int int boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 29)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.awt.AWTEvent" ((class "java.lang.Object") int) void)))
                                      (6 (aload_0))
                                      (7 (aload_1))
                                      (8 (putfield (fieldCP "adjustable" "java.awt.event.AdjustmentEvent" (class "java.awt.Adjustable"))))
                                      (11 (aload_0))
                                      (12 (iload_3))
                                      (13 (putfield (fieldCP "adjustmentType" "java.awt.event.AdjustmentEvent" int)))
                                      (16 (aload_0))
                                      (17 (iload 4))
                                      (19 (putfield (fieldCP "value" "java.awt.event.AdjustmentEvent" int)))
                                      (22 (aload_0))
                                      (23 (iload 5))
                                      (25 (putfield (fieldCP "isAdjusting" "java.awt.event.AdjustmentEvent" boolean)))
                                      (28 (return))
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAdjustable"
                              (parameters )
                              (returntype . (class "java.awt.Adjustable"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "adjustable" "java.awt.event.AdjustmentEvent" (class "java.awt.Adjustable"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getValue"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.awt.event.AdjustmentEvent" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAdjustmentType"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "adjustmentType" "java.awt.event.AdjustmentEvent" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getValueIsAdjusting"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "isAdjusting" "java.awt.event.AdjustmentEvent" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "paramString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 153)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "id" "java.awt.event.AdjustmentEvent" int))) 
                                      (4 (lookupswitch (lookupswitchinfo 30 1 ((601 . 24)))))  ;;to TAG_1;;to TAG_0
                                      (24 (ldc 7)) ;;at TAG_1;;STRING:: "ADJUSTMENT_VALUE_CHANGED"
                                      (26 (astore_1)) 
                                      (27 (goto 33)) ;;to TAG_2
                                      (30 (ldc 8)) ;;at TAG_0;;STRING:: "unknown type"
                                      (32 (astore_1)) 
                                      (33 (aload_0)) ;;at TAG_2
                                      (34 (getfield (fieldCP "adjustmentType" "java.awt.event.AdjustmentEvent" int))) 
                                      (37 (tableswitch (tableswitchinfo 102 (1 . 5) (72 78 90 84 96)))) ;;to TAG_3;;to TAG_4;;to TAG_5;;to TAG_6;;to TAG_7;;to TAG_8
                                      (72 (ldc 9)) ;;at TAG_4;;STRING:: "UNIT_INCREMENT"
                                      (74 (astore_2)) 
                                      (75 (goto 105)) ;;to TAG_9
                                      (78 (ldc 10)) ;;at TAG_5;;STRING:: "UNIT_DECREMENT"
                                      (80 (astore_2)) 
                                      (81 (goto 105)) ;;to TAG_9
                                      (84 (ldc 11)) ;;at TAG_7;;STRING:: "BLOCK_INCREMENT"
                                      (86 (astore_2)) 
                                      (87 (goto 105)) ;;to TAG_9
                                      (90 (ldc 12)) ;;at TAG_6;;STRING:: "BLOCK_DECREMENT"
                                      (92 (astore_2)) 
                                      (93 (goto 105)) ;;to TAG_9
                                      (96 (ldc 13)) ;;at TAG_8;;STRING:: "TRACK"
                                      (98 (astore_2)) 
                                      (99 (goto 105)) ;;to TAG_9
                                      (102 (ldc 8)) ;;at TAG_3;;STRING:: "unknown type"
                                      (104 (astore_2)) 
                                      (105 (new (class "java.lang.StringBuilder"))) ;;at TAG_9
                                      (108 (dup)) 
                                      (109 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (112 (aload_1)) 
                                      (113 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (116 (ldc 14)) ;;STRING:: ",adjType="
                                      (118 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (121 (aload_2)) 
                                      (122 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (125 (ldc 15)) ;;STRING:: ",value="
                                      (127 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (130 (aload_0)) 
                                      (131 (getfield (fieldCP "value" "java.awt.event.AdjustmentEvent" int))) 
                                      (134 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (137 (ldc 16)) ;;STRING:: ",isAdjusting="
                                      (139 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (142 (aload_0)) 
                                      (143 (getfield (fieldCP "isAdjusting" "java.awt.event.AdjustmentEvent" boolean))) 
                                      (146 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (boolean) (class "java.lang.StringBuilder")))) 
                                      (149 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (152 (areturn)) 
                                      (endofcode 153))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AdjustmentEvent-class-table*
  (make-static-class-decls 
   *java.awt.event.AdjustmentEvent*))

(defconst *package-name-map* 
  ("java.awt.event.AdjustmentEvent" . "java.awt.event"))

