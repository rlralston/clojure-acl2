; ProcessBuilder$Redirect-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.ProcessBuilder$Redirect*
 (make-class-def
      '(class "java.lang.ProcessBuilder$Redirect"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "PIPE" (class "java.lang.ProcessBuilder$Redirect") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "INHERIT" (class "java.lang.ProcessBuilder$Redirect") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "$assertionsDisabled" boolean (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "type"
                              (parameters )
                              (returntype . (class "java.lang.ProcessBuilder$Redirect$Type"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "file"
                              (parameters )
                              (returntype . (class "java.io.File"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "append"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "from"
                              (parameters (class "java.io.File"))
                              (returntype . (class "java.lang.ProcessBuilder$Redirect"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (ifnonnull 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (new (class "java.lang.ProcessBuilder$Redirect$3"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (aload_0)) 
                                      (17 (invokespecial (methodCP "<init>" "java.lang.ProcessBuilder$Redirect$3" ((class "java.io.File")) void))) 
                                      (20 (areturn)) 
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "to"
                              (parameters (class "java.io.File"))
                              (returntype . (class "java.lang.ProcessBuilder$Redirect"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (ifnonnull 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (new (class "java.lang.ProcessBuilder$Redirect$4"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (aload_0)) 
                                      (17 (invokespecial (methodCP "<init>" "java.lang.ProcessBuilder$Redirect$4" ((class "java.io.File")) void))) 
                                      (20 (areturn)) 
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "appendTo"
                              (parameters (class "java.io.File"))
                              (returntype . (class "java.lang.ProcessBuilder$Redirect"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (ifnonnull 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (new (class "java.lang.ProcessBuilder$Redirect$5"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (aload_0)) 
                                      (17 (invokespecial (methodCP "<init>" "java.lang.ProcessBuilder$Redirect$5" ((class "java.io.File")) void))) 
                                      (20 (areturn)) 
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 67)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aload_0)) 
                                      (2 (if_acmpne 7)) ;;to TAG_0
                                      (5 (iconst_1)) 
                                      (6 (ireturn)) 
                                      (7 (aload_1)) ;;at TAG_0
                                      (8 (instanceof (class "java.lang.ProcessBuilder$Redirect"))) 
                                      (11 (ifne 16)) ;;to TAG_1
                                      (14 (iconst_0)) 
                                      (15 (ireturn)) 
                                      (16 (aload_1)) ;;at TAG_1
                                      (17 (checkcast (class "java.lang.ProcessBuilder$Redirect"))) 
                                      (20 (astore_2)) 
                                      (21 (aload_2)) 
                                      (22 (invokevirtual (methodCP "type" "java.lang.ProcessBuilder$Redirect" () (class "java.lang.ProcessBuilder$Redirect$Type")))) 
                                      (25 (aload_0)) 
                                      (26 (invokevirtual (methodCP "type" "java.lang.ProcessBuilder$Redirect" () (class "java.lang.ProcessBuilder$Redirect$Type")))) 
                                      (29 (if_acmpeq 34))  ;;to TAG_2
                                      (32 (iconst_0)) 
                                      (33 (ireturn)) 
                                      (34 (getstatic (fieldCP "$assertionsDisabled" "java.lang.ProcessBuilder$Redirect" boolean))) ;;at TAG_2
                                      (37 (ifne 55)) ;;to TAG_3
                                      (40 (aload_0)) 
                                      (41 (invokevirtual (methodCP "file" "java.lang.ProcessBuilder$Redirect" () (class "java.io.File")))) 
                                      (44 (ifnonnull 55)) ;;to TAG_3
                                      (47 (new (class "java.lang.AssertionError"))) 
                                      (50 (dup)) 
                                      (51 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (54 (athrow)) 
                                      (55 (aload_0)) ;;at TAG_3
                                      (56 (invokevirtual (methodCP "file" "java.lang.ProcessBuilder$Redirect" () (class "java.io.File")))) 
                                      (59 (aload_2)) 
                                      (60 (invokevirtual (methodCP "file" "java.lang.ProcessBuilder$Redirect" () (class "java.io.File")))) 
                                      (63 (invokevirtual (methodCP "equals" "java.io.File" ((class "java.lang.Object")) boolean))) 
                                      (66 (ireturn)) 
                                      (endofcode 67))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "file" "java.lang.ProcessBuilder$Redirect" () (class "java.io.File")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_1)) 
                                      (6 (ifnonnull 14))  ;;to TAG_0
                                      (9 (aload_0)) 
                                      (10 (invokespecial (methodCP "hashCode" "java.lang.Object" () int))) 
                                      (13 (ireturn)) 
                                      (14 (aload_1)) ;;at TAG_0
                                      (15 (invokevirtual (methodCP "hashCode" "java.io.File" () int))) 
                                      (18 (ireturn)) 
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
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
                        (method "<init>"
                              (parameters (class "java.lang.ProcessBuilder$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.ProcessBuilder$Redirect" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 38)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (invokevirtual (methodCP "desiredAssertionStatus" "java.lang.Class" () boolean))) 
                                      (6 (ifne 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.lang.ProcessBuilder$Redirect" boolean))) ;;at TAG_1
                                      (17 (new (class "java.lang.ProcessBuilder$Redirect$1"))) 
                                      (20 (dup)) 
                                      (21 (invokespecial (methodCP "<init>" "java.lang.ProcessBuilder$Redirect$1" () void))) 
                                      (24 (putstatic (fieldCP "PIPE" "java.lang.ProcessBuilder$Redirect" (class "java.lang.ProcessBuilder$Redirect")))) 
                                      (27 (new (class "java.lang.ProcessBuilder$Redirect$2"))) 
                                      (30 (dup)) 
                                      (31 (invokespecial (methodCP "<init>" "java.lang.ProcessBuilder$Redirect$2" () void))) 
                                      (34 (putstatic (fieldCP "INHERIT" "java.lang.ProcessBuilder$Redirect" (class "java.lang.ProcessBuilder$Redirect")))) 
                                      (37 (return)) 
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ProcessBuilder$Redirect-class-table*
  (make-static-class-decls 
   *java.lang.ProcessBuilder$Redirect*))

(defconst *package-name-map* 
  ("java.lang.ProcessBuilder$Redirect" . "java.lang"))

