; Pattern$Dot-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.Pattern$Dot*
 (make-class-def
      '(class "java.util.regex.Pattern$Dot"
            "java.util.regex.Pattern$CharProperty"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aconst_null))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.regex.Pattern$CharProperty" ((class "java.util.regex.Pattern$1")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isSatisfiedBy"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 34)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (bipush 10)) 
                                      (3 (if_icmpeq 32))  ;;to TAG_0
                                      (6 (iload_1)) 
                                      (7 (bipush 13)) 
                                      (9 (if_icmpeq 32))  ;;to TAG_0
                                      (12 (iload_1)) 
                                      (13 (iconst_1)) 
                                      (14 (ior)) 
                                      (15 (sipush 8233)) 
                                      (18 (if_icmpeq 32))  ;;to TAG_0
                                      (21 (iload_1)) 
                                      (22 (sipush 133)) 
                                      (25 (if_icmpeq 32))  ;;to TAG_0
                                      (28 (iconst_1)) 
                                      (29 (goto 33)) ;;to TAG_1
                                      (32 (iconst_0)) ;;at TAG_0
                                      (33 (ireturn)) ;;at TAG_1
                                      (endofcode 34))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Pattern$Dot-class-table*
  (make-static-class-decls 
   *java.util.regex.Pattern$Dot*))

(defconst *package-name-map* 
  ("java.util.regex.Pattern$Dot" . "java.util.regex"))
