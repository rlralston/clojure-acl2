; Pattern$StartS-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.Pattern$StartS*
 (make-class-def
      '(class "java.util.regex.Pattern$StartS"
            "java.util.regex.Pattern$Start"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters (class "java.util.regex.Pattern$Node"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.regex.Pattern$Start" ((class "java.util.regex.Pattern$Node")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "match"
                              (parameters (class "java.util.regex.Matcher") int (class "java.lang.CharSequence"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 138)
                                   (parsedcode
                                      (0 (iload_2)) 
                                      (1 (aload_1)) 
                                      (2 (getfield (fieldCP "to" "java.util.regex.Matcher" int))) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "minLength" "java.util.regex.Pattern$StartS" int))) 
                                      (9 (isub)) 
                                      (10 (if_icmple 20)) ;;to TAG_0
                                      (13 (aload_1)) 
                                      (14 (iconst_1)) 
                                      (15 (putfield (fieldCP "hitEnd" "java.util.regex.Matcher" boolean))) 
                                      (18 (iconst_0)) 
                                      (19 (ireturn)) 
                                      (20 (aload_1)) ;;at TAG_0
                                      (21 (getfield (fieldCP "to" "java.util.regex.Matcher" int))) 
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "minLength" "java.util.regex.Pattern$StartS" int))) 
                                      (28 (isub)) 
                                      (29 (istore 4)) 
                                      (31 (iload_2)) ;;at TAG_4
                                      (32 (iload 4)) 
                                      (34 (if_icmpgt 131)) ;;to TAG_1
                                      (37 (aload_0)) 
                                      (38 (getfield (fieldCP "next" "java.util.regex.Pattern$StartS" (class "java.util.regex.Pattern$Node")))) 
                                      (41 (aload_1)) 
                                      (42 (iload_2)) 
                                      (43 (aload_3)) 
                                      (44 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (47 (ifeq 77))  ;;to TAG_2
                                      (50 (aload_1)) 
                                      (51 (iload_2)) 
                                      (52 (putfield (fieldCP "first" "java.util.regex.Matcher" int))) 
                                      (55 (aload_1)) 
                                      (56 (getfield (fieldCP "groups" "java.util.regex.Matcher" (array int)))) 
                                      (59 (iconst_0)) 
                                      (60 (aload_1)) 
                                      (61 (getfield (fieldCP "first" "java.util.regex.Matcher" int))) 
                                      (64 (iastore)) 
                                      (65 (aload_1)) 
                                      (66 (getfield (fieldCP "groups" "java.util.regex.Matcher" (array int)))) 
                                      (69 (iconst_1)) 
                                      (70 (aload_1)) 
                                      (71 (getfield (fieldCP "last" "java.util.regex.Matcher" int))) 
                                      (74 (iastore)) 
                                      (75 (iconst_1)) 
                                      (76 (ireturn)) 
                                      (77 (iload_2)) ;;at TAG_2
                                      (78 (iload 4)) 
                                      (80 (if_icmpne 86)) ;;to TAG_3
                                      (83 (goto 131)) ;;to TAG_1
                                      (86 (aload_3)) ;;at TAG_3
                                      (87 (iload_2)) 
                                      (88 (iinc 2 1)) 
                                      (91 (invokeinterface (methodCP "charAt" "java.lang.CharSequence" (int) char) 2)) 
                                      (96 (invokestatic (methodCP "isHighSurrogate" "java.lang.Character" (char) boolean))) 
                                      (99 (ifeq 31)) ;;to TAG_4
                                      (102 (iload_2)) 
                                      (103 (aload_3)) 
                                      (104 (invokeinterface (methodCP "length" "java.lang.CharSequence" () int) 1)) 
                                      (109 (if_icmpge 31)) ;;to TAG_4
                                      (112 (aload_3)) 
                                      (113 (iload_2)) 
                                      (114 (invokeinterface (methodCP "charAt" "java.lang.CharSequence" (int) char) 2)) 
                                      (119 (invokestatic (methodCP "isLowSurrogate" "java.lang.Character" (char) boolean))) 
                                      (122 (ifeq 31)) ;;to TAG_4
                                      (125 (iinc 2 1)) 
                                      (128 (goto 31)) ;;to TAG_4
                                      (131 (aload_1)) ;;at TAG_1
                                      (132 (iconst_1)) 
                                      (133 (putfield (fieldCP "hitEnd" "java.util.regex.Matcher" boolean))) 
                                      (136 (iconst_0)) 
                                      (137 (ireturn)) 
                                      (endofcode 138))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Pattern$StartS-class-table*
  (make-static-class-decls 
   *java.util.regex.Pattern$StartS*))

(defconst *package-name-map* 
  ("java.util.regex.Pattern$StartS" . "java.util.regex"))

