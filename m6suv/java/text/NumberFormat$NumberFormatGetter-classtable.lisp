; NumberFormat$NumberFormatGetter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.text.NumberFormat$NumberFormatGetter*
 (make-class-def
      '(class "java.text.NumberFormat$NumberFormatGetter"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "INSTANCE" (class "java.text.NumberFormat$NumberFormatGetter") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "$assertionsDisabled" boolean (accessflags  *class*  *final*  *static* ) -1))
            (methods
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
                        (method "getObject"
                              (parameters (class "java.text.spi.NumberFormatProvider") (class "java.util.Locale") (class "java.lang.String") (array (class "java.lang.Object")))
                              (returntype . (class "java.text.NumberFormat"))
                              (accessflags  *class*  *public*  *transient* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 110)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$assertionsDisabled" "java.text.NumberFormat$NumberFormatGetter" boolean))) 
                                      (3 (ifne 21)) ;;to TAG_0
                                      (6 (aload 4)) 
                                      (8 (arraylength)) 
                                      (9 (iconst_1)) 
                                      (10 (if_icmpeq 21)) ;;to TAG_0
                                      (13 (new (class "java.lang.AssertionError"))) 
                                      (16 (dup)) 
                                      (17 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (20 (athrow)) 
                                      (21 (aload 4)) ;;at TAG_0
                                      (23 (iconst_0)) 
                                      (24 (aaload)) 
                                      (25 (checkcast (class "java.lang.Integer"))) 
                                      (28 (invokevirtual (methodCP "intValue" "java.lang.Integer" () int))) 
                                      (31 (istore 5)) 
                                      (33 (iload 5)) 
                                      (35 (tableswitch (tableswitchinfo 92 (0 . 4) (68 80 74 92 86))))  ;;to TAG_2;;to TAG_3;;to TAG_4;;to TAG_5;;to TAG_1
                                      (68 (aload_1)) ;;at TAG_2
                                      (69 (aload_2)) 
                                      (70 (invokevirtual (methodCP "getNumberInstance" "java.text.spi.NumberFormatProvider" ((class "java.util.Locale")) (class "java.text.NumberFormat")))) 
                                      (73 (areturn)) 
                                      (74 (aload_1)) ;;at TAG_4
                                      (75 (aload_2)) 
                                      (76 (invokevirtual (methodCP "getPercentInstance" "java.text.spi.NumberFormatProvider" ((class "java.util.Locale")) (class "java.text.NumberFormat")))) 
                                      (79 (areturn)) 
                                      (80 (aload_1)) ;;at TAG_3
                                      (81 (aload_2)) 
                                      (82 (invokevirtual (methodCP "getCurrencyInstance" "java.text.spi.NumberFormatProvider" ((class "java.util.Locale")) (class "java.text.NumberFormat")))) 
                                      (85 (areturn)) 
                                      (86 (aload_1)) ;;at TAG_5
                                      (87 (aload_2)) 
                                      (88 (invokevirtual (methodCP "getIntegerInstance" "java.text.spi.NumberFormatProvider" ((class "java.util.Locale")) (class "java.text.NumberFormat")))) 
                                      (91 (areturn)) 
                                      (92 (getstatic (fieldCP "$assertionsDisabled" "java.text.NumberFormat$NumberFormatGetter" boolean))) ;;at TAG_1
                                      (95 (ifne 108)) ;;to TAG_6
                                      (98 (new (class "java.lang.AssertionError"))) 
                                      (101 (dup)) 
                                      (102 (iload 5)) 
                                      (104 (invokespecial (methodCP "<init>" "java.lang.AssertionError" (int) void))) 
                                      (107 (athrow)) 
                                      (108 (aconst_null)) ;;at TAG_6
                                      (109 (areturn)) 
                                      (endofcode 110))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getObject"
                              (parameters (class "java.lang.Object") (class "java.util.Locale") (class "java.lang.String") (array (class "java.lang.Object")))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (checkcast (class "java.text.spi.NumberFormatProvider")))
                                      (5 (aload_2))
                                      (6 (aload_3))
                                      (7 (aload 4))
                                      (9 (invokevirtual
					(methodCP "getObject" "java.text.NumberFormat$NumberFormatGetter" ((class "java.text.spi.NumberFormatProvider") (class "java.util.Locale") (class "java.lang.String") (array (class "java.lang.Object"))) (class "java.text.NumberFormat"))))
                                      (12 (areturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$000"
                              (parameters )
                              (returntype . (class "java.text.NumberFormat$NumberFormatGetter"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "INSTANCE" "java.text.NumberFormat$NumberFormatGetter" (class "java.text.NumberFormat$NumberFormatGetter"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 28)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (invokevirtual (methodCP "desiredAssertionStatus" "java.lang.Class" () boolean))) 
                                      (6 (ifne 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.text.NumberFormat$NumberFormatGetter" boolean))) ;;at TAG_1
                                      (17 (new (class "java.text.NumberFormat$NumberFormatGetter"))) 
                                      (20 (dup)) 
                                      (21 (invokespecial (methodCP "<init>" "java.text.NumberFormat$NumberFormatGetter" () void))) 
                                      (24 (putstatic (fieldCP "INSTANCE" "java.text.NumberFormat$NumberFormatGetter" (class "java.text.NumberFormat$NumberFormatGetter")))) 
                                      (27 (return)) 
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "sun.util.LocaleServiceProviderPool$LocalizedObjectGetter")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *NumberFormat$NumberFormatGetter-class-table*
  (make-static-class-decls 
   *java.text.NumberFormat$NumberFormatGetter*))

(defconst *package-name-map* 
  ("java.text.NumberFormat$NumberFormatGetter" . "java.text"))
