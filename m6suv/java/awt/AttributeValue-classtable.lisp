; AttributeValue-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:23 CDT 2014.
;

(defconst *java.awt.AttributeValue*
 (make-class-def
      '(class "java.awt.AttributeValue"
            "java.lang.Object"
            (constant_pool
                        (STRING  "value = ")
                        (STRING  ", names = ")
                        (STRING  "Assertion failed")
                        (STRING  "java.awt.AttributeValue"))
            (fields
                        (field "log" (class "sun.util.logging.PlatformLogger") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "value" int (accessflags  *class*  *final*  *private* ) -1)
                        (field "names" (array (class "java.lang.String")) (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters int (array (class "java.lang.String")))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 95)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (getstatic (fieldCP "log" "java.awt.AttributeValue" (class "sun.util.logging.PlatformLogger")))) 
                                      (7 (sipush 300)) 
                                      (10 (invokevirtual (methodCP "isLoggable" "sun.util.logging.PlatformLogger" (int) boolean))) 
                                      (13 (ifeq 50)) ;;to TAG_0
                                      (16 (getstatic (fieldCP "log" "java.awt.AttributeValue" (class "sun.util.logging.PlatformLogger")))) 
                                      (19 (new (class "java.lang.StringBuilder"))) 
                                      (22 (dup)) 
                                      (23 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (26 (ldc 0)) ;;STRING:: "value = "
                                      (28 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (31 (iload_1)) 
                                      (32 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (35 (ldc 1)) ;;STRING:: ", names = "
                                      (37 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (40 (aload_2)) 
                                      (41 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (44 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (47 (invokevirtual (methodCP "finest" "sun.util.logging.PlatformLogger" ((class "java.lang.String")) void))) 
                                      (50 (getstatic (fieldCP "log" "java.awt.AttributeValue" (class "sun.util.logging.PlatformLogger")))) ;;at TAG_0
                                      (53 (sipush 400)) 
                                      (56 (invokevirtual (methodCP "isLoggable" "sun.util.logging.PlatformLogger" (int) boolean))) 
                                      (59 (ifeq 84)) ;;to TAG_1
                                      (62 (iload_1)) 
                                      (63 (iflt 76))  ;;to TAG_2
                                      (66 (aload_2)) 
                                      (67 (ifnull 76))  ;;to TAG_2
                                      (70 (iload_1)) 
                                      (71 (aload_2)) 
                                      (72 (arraylength)) 
                                      (73 (if_icmplt 84)) ;;to TAG_1
                                      (76 (getstatic (fieldCP "log" "java.awt.AttributeValue" (class "sun.util.logging.PlatformLogger")))) ;;at TAG_2
                                      (79 (ldc 2)) ;;STRING:: "Assertion failed"
                                      (81 (invokevirtual (methodCP "finer" "sun.util.logging.PlatformLogger" ((class "java.lang.String")) void))) 
                                      (84 (aload_0)) ;;at TAG_1
                                      (85 (iload_1)) 
                                      (86 (putfield (fieldCP "value" "java.awt.AttributeValue" int))) 
                                      (89 (aload_0)) 
                                      (90 (aload_2)) 
                                      (91 (putfield (fieldCP "names" "java.awt.AttributeValue" (array (class "java.lang.String"))))) 
                                      (94 (return)) 
                                      (endofcode 95))
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
                                      (1 (getfield (fieldCP "value" "java.awt.AttributeValue" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "names" "java.awt.AttributeValue" (array (class "java.lang.String")))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "value" "java.awt.AttributeValue" int)))
                                      (8 (aaload))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 9)
                                   (parsedcode
                                      (0 (ldc 3))         ;;STRING:: "java.awt.AttributeValue"
                                      (2 (invokestatic
					(methodCP "getLogger" "sun.util.logging.PlatformLogger" ((class "java.lang.String")) (class "sun.util.logging.PlatformLogger"))))
                                      (5 (putstatic (fieldCP "log" "java.awt.AttributeValue" (class "sun.util.logging.PlatformLogger"))))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AttributeValue-class-table*
  (make-static-class-decls 
   *java.awt.AttributeValue*))

(defconst *package-name-map* 
  ("java.awt.AttributeValue" . "java.awt"))

