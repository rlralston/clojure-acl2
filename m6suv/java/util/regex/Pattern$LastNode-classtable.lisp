; Pattern$LastNode-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.Pattern$LastNode*
 (make-class-def
      '(class "java.util.regex.Pattern$LastNode"
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
                                   (max_stack . 3) (max_locals . 4) (code_length . 45)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (getfield (fieldCP "acceptMode" "java.util.regex.Matcher" int))) 
                                      (4 (iconst_1)) 
                                      (5 (if_icmpne 18))  ;;to TAG_0
                                      (8 (iload_2)) 
                                      (9 (aload_1)) 
                                      (10 (getfield (fieldCP "to" "java.util.regex.Matcher" int))) 
                                      (13 (if_icmpeq 18))  ;;to TAG_0
                                      (16 (iconst_0)) 
                                      (17 (ireturn)) 
                                      (18 (aload_1)) ;;at TAG_0
                                      (19 (iload_2)) 
                                      (20 (putfield (fieldCP "last" "java.util.regex.Matcher" int))) 
                                      (23 (aload_1)) 
                                      (24 (getfield (fieldCP "groups" "java.util.regex.Matcher" (array int)))) 
                                      (27 (iconst_0)) 
                                      (28 (aload_1)) 
                                      (29 (getfield (fieldCP "first" "java.util.regex.Matcher" int))) 
                                      (32 (iastore)) 
                                      (33 (aload_1)) 
                                      (34 (getfield (fieldCP "groups" "java.util.regex.Matcher" (array int)))) 
                                      (37 (iconst_1)) 
                                      (38 (aload_1)) 
                                      (39 (getfield (fieldCP "last" "java.util.regex.Matcher" int))) 
                                      (42 (iastore)) 
                                      (43 (iconst_1)) 
                                      (44 (ireturn)) 
                                      (endofcode 45))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Pattern$LastNode-class-table*
  (make-static-class-decls 
   *java.util.regex.Pattern$LastNode*))

(defconst *package-name-map* 
  ("java.util.regex.Pattern$LastNode" . "java.util.regex"))

