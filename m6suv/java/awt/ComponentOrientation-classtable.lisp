; ComponentOrientation-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:25 CDT 2014.
;

(defconst *java.awt.ComponentOrientation*
 (make-class-def
      '(class "java.awt.ComponentOrientation"
            "java.lang.Object"
            (constant_pool
                        (LONG -4113291392143563828)
                        (INT 1)
                        (INT 2)
                        (INT 4)
                        (STRING  "iw")
                        (STRING  "ar")
                        (STRING  "fa")
                        (STRING  "ur")
                        (STRING  "Orientation"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "UNK_BIT" int (accessflags  *class*  *final*  *private*  *static* ) 1)
                        (field "HORIZ_BIT" int (accessflags  *class*  *final*  *private*  *static* ) 2)
                        (field "LTR_BIT" int (accessflags  *class*  *final*  *private*  *static* ) 3)
                        (field "LEFT_TO_RIGHT" (class "java.awt.ComponentOrientation") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "RIGHT_TO_LEFT" (class "java.awt.ComponentOrientation") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "UNKNOWN" (class "java.awt.ComponentOrientation") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "orientation" int (accessflags  *class*  *private* ) -1))
            (methods
                        (method "isHorizontal"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "orientation" "java.awt.ComponentOrientation" int))) 
                                      (4 (iconst_2)) 
                                      (5 (iand)) 
                                      (6 (ifeq 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (ireturn)) ;;at TAG_1
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isLeftToRight"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "orientation" "java.awt.ComponentOrientation" int))) 
                                      (4 (iconst_4)) 
                                      (5 (iand)) 
                                      (6 (ifeq 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (ireturn)) ;;at TAG_1
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getOrientation"
                              (parameters (class "java.util.Locale"))
                              (returntype . (class "java.awt.ComponentOrientation"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 49)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getLanguage" "java.util.Locale" () (class "java.lang.String")))) 
                                      (4 (astore_1)) 
                                      (5 (ldc 4)) ;;STRING:: "iw"
                                      (7 (aload_1)) 
                                      (8 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (11 (ifne 41))  ;;to TAG_0
                                      (14 (ldc 5)) ;;STRING:: "ar"
                                      (16 (aload_1)) 
                                      (17 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (20 (ifne 41))  ;;to TAG_0
                                      (23 (ldc 6)) ;;STRING:: "fa"
                                      (25 (aload_1)) 
                                      (26 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (29 (ifne 41))  ;;to TAG_0
                                      (32 (ldc 7)) ;;STRING:: "ur"
                                      (34 (aload_1)) 
                                      (35 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (38 (ifeq 45)) ;;to TAG_1
                                      (41 (getstatic (fieldCP "RIGHT_TO_LEFT" "java.awt.ComponentOrientation" (class "java.awt.ComponentOrientation")))) ;;at TAG_0
                                      (44 (areturn)) 
                                      (45 (getstatic (fieldCP "LEFT_TO_RIGHT" "java.awt.ComponentOrientation" (class "java.awt.ComponentOrientation")))) ;;at TAG_1
                                      (48 (areturn)) 
                                      (endofcode 49))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getOrientation"
                              (parameters (class "java.util.ResourceBundle"))
                              (returntype . (class "java.awt.ComponentOrientation"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 41)
                                   (parsedcode
                                      (0 (aconst_null)) 
                                      (1 (astore_1)) 
                                      (2 (aload_0)) ;;at TAG_3
                                      (3 (ldc 8)) ;;STRING:: "Orientation"
                                      (5 (invokevirtual (methodCP "getObject" "java.util.ResourceBundle" ((class "java.lang.String")) (class "java.lang.Object")))) 
                                      (8 (checkcast (class "java.awt.ComponentOrientation"))) 
                                      (11 (astore_1)) 
                                      (12 (goto 16)) ;;to TAG_0;;at TAG_4
                                      (15 (astore_2)) ;;at TAG_5
                                      (16 (aload_1)) ;;at TAG_0
                                      (17 (ifnonnull 28)) ;;to TAG_1
                                      (20 (aload_0)) 
                                      (21 (invokevirtual (methodCP "getLocale" "java.util.ResourceBundle" () (class "java.util.Locale")))) 
                                      (24 (invokestatic (methodCP "getOrientation" "java.awt.ComponentOrientation" ((class "java.util.Locale")) (class "java.awt.ComponentOrientation")))) 
                                      (27 (astore_1)) 
                                      (28 (aload_1)) ;;at TAG_1
                                      (29 (ifnonnull 39))  ;;to TAG_2
                                      (32 (invokestatic (methodCP "getDefault" "java.util.Locale" () (class "java.util.Locale")))) 
                                      (35 (invokestatic (methodCP "getOrientation" "java.awt.ComponentOrientation" ((class "java.util.Locale")) (class "java.awt.ComponentOrientation")))) 
                                      (38 (astore_1)) 
                                      (39 (aload_1)) ;;at TAG_2
                                      (40 (areturn)) 
                                      (endofcode 41))
                                   (Exceptions 
                                     (handler 2 12  15 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "<init>"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (putfield (fieldCP "orientation" "java.awt.ComponentOrientation" int)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 0) (code_length . 36)
                                   (parsedcode
                                      (0 (new (class "java.awt.ComponentOrientation")))
                                      (3 (dup))
                                      (4 (bipush 6))
                                      (6 (invokespecial
					(methodCP "<init>" "java.awt.ComponentOrientation" (int) void)))
                                      (9 (putstatic (fieldCP "LEFT_TO_RIGHT" "java.awt.ComponentOrientation" (class "java.awt.ComponentOrientation"))))
                                      (12 (new (class "java.awt.ComponentOrientation")))
                                      (15 (dup))
                                      (16 (iconst_2))
                                      (17 (invokespecial
					(methodCP "<init>" "java.awt.ComponentOrientation" (int) void)))
                                      (20 (putstatic (fieldCP "RIGHT_TO_LEFT" "java.awt.ComponentOrientation" (class "java.awt.ComponentOrientation"))))
                                      (23 (new (class "java.awt.ComponentOrientation")))
                                      (26 (dup))
                                      (27 (bipush 7))
                                      (29 (invokespecial
					(methodCP "<init>" "java.awt.ComponentOrientation" (int) void)))
                                      (32 (putstatic (fieldCP "UNKNOWN" "java.awt.ComponentOrientation" (class "java.awt.ComponentOrientation"))))
                                      (35 (return))
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ComponentOrientation-class-table*
  (make-static-class-decls 
   *java.awt.ComponentOrientation*))

(defconst *package-name-map* 
  ("java.awt.ComponentOrientation" . "java.awt"))

