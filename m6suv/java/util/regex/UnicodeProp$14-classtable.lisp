; UnicodeProp$14-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.UnicodeProp$14*
 (make-class-def
      '(class "java.util.regex.UnicodeProp$14"
            "java.util.regex.UnicodeProp"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (aconst_null))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp" ((class "java.lang.String") int (class "java.util.regex.UnicodeProp$1")) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "is"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 26)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "ALPHABETIC" "java.util.regex.UnicodeProp$14" (class "java.util.regex.UnicodeProp")))) 
                                      (3 (iload_1)) 
                                      (4 (invokevirtual (methodCP "is" "java.util.regex.UnicodeProp" (int) boolean))) 
                                      (7 (ifne 20)) ;;to TAG_0
                                      (10 (getstatic (fieldCP "DIGIT" "java.util.regex.UnicodeProp$14" (class "java.util.regex.UnicodeProp")))) 
                                      (13 (iload_1)) 
                                      (14 (invokevirtual (methodCP "is" "java.util.regex.UnicodeProp" (int) boolean))) 
                                      (17 (ifeq 24)) ;;to TAG_1
                                      (20 (iconst_1)) ;;at TAG_0
                                      (21 (goto 25))  ;;to TAG_2
                                      (24 (iconst_0)) ;;at TAG_1
                                      (25 (ireturn)) ;;at TAG_2
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *UnicodeProp$14-class-table*
  (make-static-class-decls 
   *java.util.regex.UnicodeProp$14*))

(defconst *package-name-map* 
  ("java.util.regex.UnicodeProp$14" . "java.util.regex"))
