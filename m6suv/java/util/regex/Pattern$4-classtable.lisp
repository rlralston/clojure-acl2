; Pattern$4-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.Pattern$4*
 (make-class-def
      '(class "java.util.regex.Pattern$4"
            "java.util.regex.Pattern$Node"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.regex.Pattern$Node" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "match"
                              (parameters (class "java.util.regex.Matcher") int (class "java.lang.CharSequence"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 14)
                                   (parsedcode
                                      (0 (iload_2)) 
                                      (1 (aload_1)) 
                                      (2 (getfield (fieldCP "lookbehindTo" "java.util.regex.Matcher" int))) 
                                      (5 (if_icmpne 12))  ;;to TAG_0
                                      (8 (iconst_1)) 
                                      (9 (goto 13)) ;;to TAG_1
                                      (12 (iconst_0)) ;;at TAG_0
                                      (13 (ireturn)) ;;at TAG_1
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Pattern$4-class-table*
  (make-static-class-decls 
   *java.util.regex.Pattern$4*))

(defconst *package-name-map* 
  ("java.util.regex.Pattern$4" . "java.util.regex"))

