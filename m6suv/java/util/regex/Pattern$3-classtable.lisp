; Pattern$3-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.Pattern$3*
 (make-class-def
      '(class "java.util.regex.Pattern$3"
            "java.util.regex.Pattern$CharProperty"
            (constant_pool)
            (fields
                        (field "val$lower" int (accessflags  *class*  *final* ) -1)
                        (field "val$upper" int (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "java.util.regex.Pattern") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.regex.Pattern") int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.regex.Pattern$3" (class "java.util.regex.Pattern"))))
                                      (5 (aload_0))
                                      (6 (iload_2))
                                      (7 (putfield (fieldCP "val$lower" "java.util.regex.Pattern$3" int)))
                                      (10 (aload_0))
                                      (11 (iload_3))
                                      (12 (putfield (fieldCP "val$upper" "java.util.regex.Pattern$3" int)))
                                      (15 (aload_0))
                                      (16 (aconst_null))
                                      (17 (invokespecial
					(methodCP "<init>" "java.util.regex.Pattern$CharProperty" ((class "java.util.regex.Pattern$1")) void)))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isSatisfiedBy"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 64)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "val$lower" "java.util.regex.Pattern$3" int))) 
                                      (4 (iload_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "val$upper" "java.util.regex.Pattern$3" int))) 
                                      (9 (invokestatic (methodCP "access$200" "java.util.regex.Pattern" (int int int) boolean))) 
                                      (12 (ifne 58)) ;;to TAG_0
                                      (15 (iload_1)) 
                                      (16 (invokestatic (methodCP "isAscii" "java.util.regex.ASCII" (int) boolean))) 
                                      (19 (ifeq 62)) ;;to TAG_1
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "val$lower" "java.util.regex.Pattern$3" int))) 
                                      (26 (iload_1)) 
                                      (27 (invokestatic (methodCP "toUpper" "java.util.regex.ASCII" (int) int))) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "val$upper" "java.util.regex.Pattern$3" int))) 
                                      (34 (invokestatic (methodCP "access$200" "java.util.regex.Pattern" (int int int) boolean))) 
                                      (37 (ifne 58)) ;;to TAG_0
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "val$lower" "java.util.regex.Pattern$3" int))) 
                                      (44 (iload_1)) 
                                      (45 (invokestatic (methodCP "toLower" "java.util.regex.ASCII" (int) int))) 
                                      (48 (aload_0)) 
                                      (49 (getfield (fieldCP "val$upper" "java.util.regex.Pattern$3" int))) 
                                      (52 (invokestatic (methodCP "access$200" "java.util.regex.Pattern" (int int int) boolean))) 
                                      (55 (ifeq 62)) ;;to TAG_1
                                      (58 (iconst_1)) ;;at TAG_0
                                      (59 (goto 63))  ;;to TAG_2
                                      (62 (iconst_0)) ;;at TAG_1
                                      (63 (ireturn)) ;;at TAG_2
                                      (endofcode 64))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Pattern$3-class-table*
  (make-static-class-decls 
   *java.util.regex.Pattern$3*))

(defconst *package-name-map* 
  ("java.util.regex.Pattern$3" . "java.util.regex"))

