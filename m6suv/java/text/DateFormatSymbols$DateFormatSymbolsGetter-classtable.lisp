; DateFormatSymbols$DateFormatSymbolsGetter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.text.DateFormatSymbols$DateFormatSymbolsGetter*
 (make-class-def
      '(class "java.text.DateFormatSymbols$DateFormatSymbolsGetter"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "INSTANCE" (class "java.text.DateFormatSymbols$DateFormatSymbolsGetter") (accessflags  *class*  *final*  *private*  *static* ) -1)
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
                              (parameters (class "java.text.spi.DateFormatSymbolsProvider") (class "java.util.Locale") (class "java.lang.String") (array (class "java.lang.Object")))
                              (returntype . (class "java.text.DateFormatSymbols"))
                              (accessflags  *class*  *public*  *transient* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$assertionsDisabled" "java.text.DateFormatSymbols$DateFormatSymbolsGetter" boolean))) 
                                      (3 (ifne 20))  ;;to TAG_0
                                      (6 (aload 4)) 
                                      (8 (arraylength)) 
                                      (9 (ifeq 20))  ;;to TAG_0
                                      (12 (new (class "java.lang.AssertionError"))) 
                                      (15 (dup)) 
                                      (16 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (19 (athrow)) 
                                      (20 (aload_1)) ;;at TAG_0
                                      (21 (aload_2)) 
                                      (22 (invokevirtual (methodCP "getInstance" "java.text.spi.DateFormatSymbolsProvider" ((class "java.util.Locale")) (class "java.text.DateFormatSymbols")))) 
                                      (25 (areturn)) 
                                      (endofcode 26))
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
                                      (2 (checkcast (class "java.text.spi.DateFormatSymbolsProvider")))
                                      (5 (aload_2))
                                      (6 (aload_3))
                                      (7 (aload 4))
                                      (9 (invokevirtual
					(methodCP "getObject" "java.text.DateFormatSymbols$DateFormatSymbolsGetter" ((class "java.text.spi.DateFormatSymbolsProvider") (class "java.util.Locale") (class "java.lang.String") (array (class "java.lang.Object"))) (class "java.text.DateFormatSymbols"))))
                                      (12 (areturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$000"
                              (parameters )
                              (returntype . (class "java.text.DateFormatSymbols$DateFormatSymbolsGetter"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "INSTANCE" "java.text.DateFormatSymbols$DateFormatSymbolsGetter" (class "java.text.DateFormatSymbols$DateFormatSymbolsGetter"))))
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
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.text.DateFormatSymbols$DateFormatSymbolsGetter" boolean))) ;;at TAG_1
                                      (17 (new (class "java.text.DateFormatSymbols$DateFormatSymbolsGetter"))) 
                                      (20 (dup)) 
                                      (21 (invokespecial (methodCP "<init>" "java.text.DateFormatSymbols$DateFormatSymbolsGetter" () void))) 
                                      (24 (putstatic (fieldCP "INSTANCE" "java.text.DateFormatSymbols$DateFormatSymbolsGetter" (class "java.text.DateFormatSymbols$DateFormatSymbolsGetter")))) 
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


(defconst *DateFormatSymbols$DateFormatSymbolsGetter-class-table*
  (make-static-class-decls 
   *java.text.DateFormatSymbols$DateFormatSymbolsGetter*))

(defconst *package-name-map* 
  ("java.text.DateFormatSymbols$DateFormatSymbolsGetter" . "java.text"))

