; Short-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:36 CDT 2014.
;

(defconst *java.lang.Short*
 (make-class-def
      '(class "java.lang.Short"
            "java.lang.Number"
            (constant_pool
                        (INT -32768)
                        (INT 32767)
                        (INT 16)
                        (LONG 7515723908773894738)
                        (STRING  "Value out of range. Value:\"")
                        (STRING  "\" Radix:")
                        (STRING  "Value ")
                        (STRING  " out of range from input ")
                        (INT 65280)
                        (STRING  "short"))
            (fields
                        (field "MIN_VALUE" short (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "MAX_VALUE" short (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "TYPE" (class "java.lang.Class") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "value" short (accessflags  *class*  *final*  *private* ) -1)
                        (field "SIZE" int (accessflags  *class*  *final*  *public*  *static* ) 2)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 3))
            (methods
                        (method "toString"
                              (parameters short)
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 7)
                                   (parsedcode
                                      (0 (iload_0))
                                      (1 (bipush 10))
                                      (3 (invokestatic
					(methodCP "toString" "java.lang.Integer" (int int) (class "java.lang.String"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "parseShort"
                              (parameters (class "java.lang.String") int)
                              (returntype . short)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 59)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (iload_1)) 
                                      (2 (invokestatic (methodCP "parseInt" "java.lang.Integer" ((class "java.lang.String") int) int))) 
                                      (5 (istore_2)) 
                                      (6 (iload_2)) 
                                      (7 (sipush -32768)) 
                                      (10 (if_icmplt 20))  ;;to TAG_0
                                      (13 (iload_2)) 
                                      (14 (sipush 32767)) 
                                      (17 (if_icmple 56)) ;;to TAG_1
                                      (20 (new (class "java.lang.NumberFormatException"))) ;;at TAG_0
                                      (23 (dup)) 
                                      (24 (new (class "java.lang.StringBuilder"))) 
                                      (27 (dup)) 
                                      (28 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (31 (ldc 4)) ;;STRING:: "Value out of range. Value:\""
                                      (33 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (36 (aload_0)) 
                                      (37 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (40 (ldc 5)) ;;STRING:: "\" Radix:"
                                      (42 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (45 (iload_1)) 
                                      (46 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (49 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (52 (invokespecial (methodCP "<init>" "java.lang.NumberFormatException" ((class "java.lang.String")) void))) 
                                      (55 (athrow)) 
                                      (56 (iload_2)) ;;at TAG_1
                                      (57 (i2s)) 
                                      (58 (ireturn)) 
                                      (endofcode 59))
                                   (Exceptions )
                                   (StackMap )))
                        (method "parseShort"
                              (parameters (class "java.lang.String"))
                              (returntype . short)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (bipush 10))
                                      (3 (invokestatic
					(methodCP "parseShort" "java.lang.Short" ((class "java.lang.String") int) short)))
                                      (6 (ireturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "valueOf"
                              (parameters (class "java.lang.String") int)
                              (returntype . (class "java.lang.Short"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (invokestatic
					(methodCP "parseShort" "java.lang.Short" ((class "java.lang.String") int) short)))
                                      (5 (invokestatic
					(methodCP "valueOf" "java.lang.Short" (short) (class "java.lang.Short"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "valueOf"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Short"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (bipush 10))
                                      (3 (invokestatic
					(methodCP "valueOf" "java.lang.Short" ((class "java.lang.String") int) (class "java.lang.Short"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "valueOf"
                              (parameters short)
                              (returntype . (class "java.lang.Short"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 33)
                                   (parsedcode
                                      (0 (iload_0)) 
                                      (1 (istore_2)) 
                                      (2 (iload_2)) 
                                      (3 (bipush -128)) 
                                      (5 (if_icmplt 24))  ;;to TAG_0
                                      (8 (iload_2)) 
                                      (9 (bipush 127)) 
                                      (11 (if_icmpgt 24))  ;;to TAG_0
                                      (14 (getstatic (fieldCP "cache" "java.lang.Short$ShortCache" (array (class "java.lang.Short"))))) 
                                      (17 (iload_2)) 
                                      (18 (sipush 128)) 
                                      (21 (iadd)) 
                                      (22 (aaload)) 
                                      (23 (areturn)) 
                                      (24 (new (class "java.lang.Short"))) ;;at TAG_0
                                      (27 (dup)) 
                                      (28 (iload_0)) 
                                      (29 (invokespecial (methodCP "<init>" "java.lang.Short" (short) void))) 
                                      (32 (areturn)) 
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap )))
                        (method "decode"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Short"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 64)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokestatic (methodCP "decode" "java.lang.Integer" ((class "java.lang.String")) (class "java.lang.Integer")))) 
                                      (4 (invokevirtual (methodCP "intValue" "java.lang.Integer" () int))) 
                                      (7 (istore_1)) 
                                      (8 (iload_1)) 
                                      (9 (sipush -32768)) 
                                      (12 (if_icmplt 22))  ;;to TAG_0
                                      (15 (iload_1)) 
                                      (16 (sipush 32767)) 
                                      (19 (if_icmple 58)) ;;to TAG_1
                                      (22 (new (class "java.lang.NumberFormatException"))) ;;at TAG_0
                                      (25 (dup)) 
                                      (26 (new (class "java.lang.StringBuilder"))) 
                                      (29 (dup)) 
                                      (30 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (33 (ldc 6)) ;;STRING:: "Value "
                                      (35 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (38 (iload_1)) 
                                      (39 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (42 (ldc 7)) ;;STRING:: " out of range from input "
                                      (44 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (47 (aload_0)) 
                                      (48 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (51 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (54 (invokespecial (methodCP "<init>" "java.lang.NumberFormatException" ((class "java.lang.String")) void))) 
                                      (57 (athrow)) 
                                      (58 (iload_1)) ;;at TAG_1
                                      (59 (i2s)) 
                                      (60 (invokestatic (methodCP "valueOf" "java.lang.Short" (short) (class "java.lang.Short")))) 
                                      (63 (areturn)) 
                                      (endofcode 64))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters short)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Number" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (putfield (fieldCP "value" "java.lang.Short" short)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Number" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (bipush 10))
                                      (8 (invokestatic
					(methodCP "parseShort" "java.lang.Short" ((class "java.lang.String") int) short)))
                                      (11 (putfield (fieldCP "value" "java.lang.Short" short)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "byteValue"
                              (parameters )
                              (returntype . byte)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.lang.Short" short)))
                                      (4 (i2b))
                                      (5 (ireturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "shortValue"
                              (parameters )
                              (returntype . short)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.lang.Short" short)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "intValue"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.lang.Short" short)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "longValue"
                              (parameters )
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.lang.Short" short)))
                                      (4 (i2l))
                                      (5 (lreturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "floatValue"
                              (parameters )
                              (returntype . float)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.lang.Short" short)))
                                      (4 (i2f))
                                      (5 (freturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doubleValue"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.lang.Short" short)))
                                      (4 (i2d))
                                      (5 (dreturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.lang.Short" short)))
                                      (4 (invokestatic
					(methodCP "toString" "java.lang.Integer" (int) (class "java.lang.String"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.lang.Short" short)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 29)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.lang.Short"))) 
                                      (4 (ifeq 27)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "value" "java.lang.Short" short))) 
                                      (11 (aload_1)) 
                                      (12 (checkcast (class "java.lang.Short"))) 
                                      (15 (invokevirtual (methodCP "shortValue" "java.lang.Short" () short))) 
                                      (18 (if_icmpne 25)) ;;to TAG_1
                                      (21 (iconst_1)) 
                                      (22 (goto 26))  ;;to TAG_2
                                      (25 (iconst_0)) ;;at TAG_1
                                      (26 (ireturn)) ;;at TAG_2
                                      (27 (iconst_0)) ;;at TAG_0
                                      (28 (ireturn)) 
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compareTo"
                              (parameters (class "java.lang.Short"))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.lang.Short" short)))
                                      (4 (aload_1))
                                      (5 (getfield (fieldCP "value" "java.lang.Short" short)))
                                      (8 (invokestatic
					(methodCP "compare" "java.lang.Short" (short short) int)))
                                      (11 (ireturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compare"
                              (parameters short short)
                              (returntype . int)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 4)
                                   (parsedcode
                                      (0 (iload_0))
                                      (1 (iload_1))
                                      (2 (isub))
                                      (3 (ireturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reverseBytes"
                              (parameters short)
                              (returntype . short)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (iload_0))
                                      (1 (ldc 8))         ;;INT:: "65280"
                                      (3 (iand))
                                      (4 (bipush 8))
                                      (6 (ishr))
                                      (7 (iload_0))
                                      (8 (bipush 8))
                                      (10 (ishl))
                                      (11 (ior))
                                      (12 (i2s))
                                      (13 (ireturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compareTo"
                              (parameters (class "java.lang.Object"))
                              (returntype . int)
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (checkcast (class "java.lang.Short")))
                                      (5 (invokevirtual
					(methodCP "compareTo" "java.lang.Short" ((class "java.lang.Short")) int)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 9)
                                   (parsedcode
                                      (0 (ldc 9))         ;;STRING:: "short"
                                      (2 (invokestatic
					(methodCP "getPrimitiveClass" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class"))))
                                      (5 (putstatic (fieldCP "TYPE" "java.lang.Short" (class "java.lang.Class"))))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.lang.Comparable")
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Short-class-table*
  (make-static-class-decls 
   *java.lang.Short*))

(defconst *package-name-map* 
  ("java.lang.Short" . "java.lang"))
