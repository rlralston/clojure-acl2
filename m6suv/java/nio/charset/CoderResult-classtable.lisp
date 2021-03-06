; CoderResult-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.charset.CoderResult*
 (make-class-def
      '(class "java.nio.charset.CoderResult"
            "java.lang.Object"
            (constant_pool
                        (INT 0)
                        (INT 1)
                        (INT 2)
                        (INT 3)
                        (STRING  "[")
                        (STRING  "]")
                        (STRING  "UNDERFLOW")
                        (STRING  "OVERFLOW")
                        (STRING  "MALFORMED")
                        (STRING  "UNMAPPABLE"))
            (fields
                        (field "CR_UNDERFLOW" int (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "CR_OVERFLOW" int (accessflags  *class*  *final*  *private*  *static* ) 1)
                        (field "CR_ERROR_MIN" int (accessflags  *class*  *final*  *private*  *static* ) 2)
                        (field "CR_MALFORMED" int (accessflags  *class*  *final*  *private*  *static* ) 2)
                        (field "CR_UNMAPPABLE" int (accessflags  *class*  *final*  *private*  *static* ) 3)
                        (field "names" (array (class "java.lang.String")) (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "type" int (accessflags  *class*  *final*  *private* ) -1)
                        (field "length" int (accessflags  *class*  *final*  *private* ) -1)
                        (field "UNDERFLOW" (class "java.nio.charset.CoderResult") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "OVERFLOW" (class "java.nio.charset.CoderResult") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "malformedCache" (class "java.nio.charset.CoderResult$Cache") (accessflags  *class*  *private*  *static* ) -1)
                        (field "unmappableCache" (class "java.nio.charset.CoderResult$Cache") (accessflags  *class*  *private*  *static* ) -1)
                        (field "$assertionsDisabled" boolean (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (putfield (fieldCP "type" "java.nio.charset.CoderResult" int)))
                                      (9 (aload_0))
                                      (10 (iload_2))
                                      (11 (putfield (fieldCP "length" "java.nio.charset.CoderResult" int)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 52)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "names" "java.nio.charset.CoderResult" (array (class "java.lang.String"))))) 
                                      (3 (aload_0)) 
                                      (4 (getfield (fieldCP "type" "java.nio.charset.CoderResult" int))) 
                                      (7 (aaload)) 
                                      (8 (astore_1)) 
                                      (9 (aload_0)) 
                                      (10 (invokevirtual (methodCP "isError" "java.nio.charset.CoderResult" () boolean))) 
                                      (13 (ifeq 50))  ;;to TAG_0
                                      (16 (new (class "java.lang.StringBuilder"))) 
                                      (19 (dup)) 
                                      (20 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (23 (aload_1)) 
                                      (24 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (27 (ldc 4)) ;;STRING:: "["
                                      (29 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "length" "java.nio.charset.CoderResult" int))) 
                                      (36 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (39 (ldc 5)) ;;STRING:: "]"
                                      (41 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (44 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (47 (goto 51)) ;;to TAG_1
                                      (50 (aload_1)) ;;at TAG_0
                                      (51 (areturn)) ;;at TAG_1
                                      (endofcode 52))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isUnderflow"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "type" "java.nio.charset.CoderResult" int))) 
                                      (4 (ifne 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isOverflow"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "type" "java.nio.charset.CoderResult" int))) 
                                      (4 (iconst_1)) 
                                      (5 (if_icmpne 12))  ;;to TAG_0
                                      (8 (iconst_1)) 
                                      (9 (goto 13)) ;;to TAG_1
                                      (12 (iconst_0)) ;;at TAG_0
                                      (13 (ireturn)) ;;at TAG_1
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isError"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "type" "java.nio.charset.CoderResult" int))) 
                                      (4 (iconst_2)) 
                                      (5 (if_icmplt 12))  ;;to TAG_0
                                      (8 (iconst_1)) 
                                      (9 (goto 13)) ;;to TAG_1
                                      (12 (iconst_0)) ;;at TAG_0
                                      (13 (ireturn)) ;;at TAG_1
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isMalformed"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "type" "java.nio.charset.CoderResult" int))) 
                                      (4 (iconst_2)) 
                                      (5 (if_icmpne 12))  ;;to TAG_0
                                      (8 (iconst_1)) 
                                      (9 (goto 13)) ;;to TAG_1
                                      (12 (iconst_0)) ;;at TAG_0
                                      (13 (ireturn)) ;;at TAG_1
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isUnmappable"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "type" "java.nio.charset.CoderResult" int))) 
                                      (4 (iconst_3)) 
                                      (5 (if_icmpne 12))  ;;to TAG_0
                                      (8 (iconst_1)) 
                                      (9 (goto 13)) ;;to TAG_1
                                      (12 (iconst_0)) ;;at TAG_0
                                      (13 (ireturn)) ;;at TAG_1
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "length"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isError" "java.nio.charset.CoderResult" () boolean))) 
                                      (4 (ifne 15))  ;;to TAG_0
                                      (7 (new (class "java.lang.UnsupportedOperationException"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.lang.UnsupportedOperationException" () void))) 
                                      (14 (athrow)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "length" "java.nio.charset.CoderResult" int))) 
                                      (19 (ireturn)) 
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "malformedForLength"
                              (parameters int)
                              (returntype . (class "java.nio.charset.CoderResult"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "malformedCache" "java.nio.charset.CoderResult" (class "java.nio.charset.CoderResult$Cache"))))
                                      (3 (iload_0))
                                      (4 (invokestatic
					(methodCP "access$200" "java.nio.charset.CoderResult$Cache" ((class "java.nio.charset.CoderResult$Cache") int) (class "java.nio.charset.CoderResult"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "unmappableForLength"
                              (parameters int)
                              (returntype . (class "java.nio.charset.CoderResult"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "unmappableCache" "java.nio.charset.CoderResult" (class "java.nio.charset.CoderResult$Cache"))))
                                      (3 (iload_0))
                                      (4 (invokestatic
					(methodCP "access$200" "java.nio.charset.CoderResult$Cache" ((class "java.nio.charset.CoderResult$Cache") int) (class "java.nio.charset.CoderResult"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "throwException"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 91)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "type" "java.nio.charset.CoderResult" int))) 
                                      (4 (tableswitch (tableswitchinfo 76 (0 . 3) (36 44 52 64))))  ;;to TAG_2;;to TAG_3;;to TAG_4;;to TAG_0;;to TAG_1
                                      (36 (new (class "java.nio.BufferUnderflowException"))) ;;at TAG_1
                                      (39 (dup)) 
                                      (40 (invokespecial (methodCP "<init>" "java.nio.BufferUnderflowException" () void))) 
                                      (43 (athrow)) 
                                      (44 (new (class "java.nio.BufferOverflowException"))) ;;at TAG_2
                                      (47 (dup)) 
                                      (48 (invokespecial (methodCP "<init>" "java.nio.BufferOverflowException" () void))) 
                                      (51 (athrow)) 
                                      (52 (new (class "java.nio.charset.MalformedInputException"))) ;;at TAG_3
                                      (55 (dup)) 
                                      (56 (aload_0)) 
                                      (57 (getfield (fieldCP "length" "java.nio.charset.CoderResult" int))) 
                                      (60 (invokespecial (methodCP "<init>" "java.nio.charset.MalformedInputException" (int) void))) 
                                      (63 (athrow)) 
                                      (64 (new (class "java.nio.charset.UnmappableCharacterException"))) ;;at TAG_4
                                      (67 (dup)) 
                                      (68 (aload_0)) 
                                      (69 (getfield (fieldCP "length" "java.nio.charset.CoderResult" int))) 
                                      (72 (invokespecial (methodCP "<init>" "java.nio.charset.UnmappableCharacterException" (int) void))) 
                                      (75 (athrow)) 
                                      (76 (getstatic (fieldCP "$assertionsDisabled" "java.nio.charset.CoderResult" boolean))) ;;at TAG_0
                                      (79 (ifne 90)) ;;to TAG_5
                                      (82 (new (class "java.lang.AssertionError"))) 
                                      (85 (dup)) 
                                      (86 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (89 (athrow)) 
                                      (90 (return)) ;;at TAG_5
                                      (endofcode 91))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters int int (class "java.nio.charset.CoderResult$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (iload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.nio.charset.CoderResult" (int int) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 89)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (invokevirtual (methodCP "desiredAssertionStatus" "java.lang.Class" () boolean))) 
                                      (6 (ifne 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.nio.charset.CoderResult" boolean))) ;;at TAG_1
                                      (17 (iconst_4)) 
                                      (18 (anewarray (class "java.lang.String"))) 
                                      (21 (dup)) 
                                      (22 (iconst_0)) 
                                      (23 (ldc 6)) ;;STRING:: "UNDERFLOW"
                                      (25 (aastore)) 
                                      (26 (dup)) 
                                      (27 (iconst_1)) 
                                      (28 (ldc 7)) ;;STRING:: "OVERFLOW"
                                      (30 (aastore)) 
                                      (31 (dup)) 
                                      (32 (iconst_2)) 
                                      (33 (ldc 8)) ;;STRING:: "MALFORMED"
                                      (35 (aastore)) 
                                      (36 (dup)) 
                                      (37 (iconst_3)) 
                                      (38 (ldc 9)) ;;STRING:: "UNMAPPABLE"
                                      (40 (aastore)) 
                                      (41 (putstatic (fieldCP "names" "java.nio.charset.CoderResult" (array (class "java.lang.String"))))) 
                                      (44 (new (class "java.nio.charset.CoderResult"))) 
                                      (47 (dup)) 
                                      (48 (iconst_0)) 
                                      (49 (iconst_0)) 
                                      (50 (invokespecial (methodCP "<init>" "java.nio.charset.CoderResult" (int int) void))) 
                                      (53 (putstatic (fieldCP "UNDERFLOW" "java.nio.charset.CoderResult" (class "java.nio.charset.CoderResult")))) 
                                      (56 (new (class "java.nio.charset.CoderResult"))) 
                                      (59 (dup)) 
                                      (60 (iconst_1)) 
                                      (61 (iconst_0)) 
                                      (62 (invokespecial (methodCP "<init>" "java.nio.charset.CoderResult" (int int) void))) 
                                      (65 (putstatic (fieldCP "OVERFLOW" "java.nio.charset.CoderResult" (class "java.nio.charset.CoderResult")))) 
                                      (68 (new (class "java.nio.charset.CoderResult$1"))) 
                                      (71 (dup)) 
                                      (72 (invokespecial (methodCP "<init>" "java.nio.charset.CoderResult$1" () void))) 
                                      (75 (putstatic (fieldCP "malformedCache" "java.nio.charset.CoderResult" (class "java.nio.charset.CoderResult$Cache")))) 
                                      (78 (new (class "java.nio.charset.CoderResult$2"))) 
                                      (81 (dup)) 
                                      (82 (invokespecial (methodCP "<init>" "java.nio.charset.CoderResult$2" () void))) 
                                      (85 (putstatic (fieldCP "unmappableCache" "java.nio.charset.CoderResult" (class "java.nio.charset.CoderResult$Cache")))) 
                                      (88 (return)) 
                                      (endofcode 89))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *CoderResult-class-table*
  (make-static-class-decls 
   *java.nio.charset.CoderResult*))

(defconst *package-name-map* 
  ("java.nio.charset.CoderResult" . "java.nio.charset"))

