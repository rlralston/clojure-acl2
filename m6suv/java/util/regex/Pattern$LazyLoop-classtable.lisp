; Pattern$LazyLoop-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.Pattern$LazyLoop*
 (make-class-def
      '(class "java.util.regex.Pattern$LazyLoop"
            "java.util.regex.Pattern$Loop"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (iload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.util.regex.Pattern$Loop" (int int) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "match"
                              (parameters (class "java.util.regex.Matcher") int (class "java.lang.CharSequence"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 158)
                                   (parsedcode
                                      (0 (iload_2)) 
                                      (1 (aload_1)) 
                                      (2 (getfield (fieldCP "locals" "java.util.regex.Matcher" (array int)))) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "beginIndex" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (9 (iaload)) 
                                      (10 (if_icmple 147)) ;;to TAG_0
                                      (13 (aload_1)) 
                                      (14 (getfield (fieldCP "locals" "java.util.regex.Matcher" (array int)))) 
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "countIndex" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (21 (iaload)) 
                                      (22 (istore 4)) 
                                      (24 (iload 4)) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "cmin" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (30 (if_icmpge 77)) ;;to TAG_1
                                      (33 (aload_1)) 
                                      (34 (getfield (fieldCP "locals" "java.util.regex.Matcher" (array int)))) 
                                      (37 (aload_0)) 
                                      (38 (getfield (fieldCP "countIndex" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (41 (iload 4)) 
                                      (43 (iconst_1)) 
                                      (44 (iadd)) 
                                      (45 (iastore)) 
                                      (46 (aload_0)) 
                                      (47 (getfield (fieldCP "body" "java.util.regex.Pattern$LazyLoop" (class "java.util.regex.Pattern$Node")))) 
                                      (50 (aload_1)) 
                                      (51 (iload_2)) 
                                      (52 (aload_3)) 
                                      (53 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (56 (istore 5)) 
                                      (58 (iload 5)) 
                                      (60 (ifne 74))  ;;to TAG_2
                                      (63 (aload_1)) 
                                      (64 (getfield (fieldCP "locals" "java.util.regex.Matcher" (array int)))) 
                                      (67 (aload_0)) 
                                      (68 (getfield (fieldCP "countIndex" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (71 (iload 4)) 
                                      (73 (iastore)) 
                                      (74 (iload 5)) ;;at TAG_2
                                      (76 (ireturn)) 
                                      (77 (aload_0)) ;;at TAG_1
                                      (78 (getfield (fieldCP "next" "java.util.regex.Pattern$LazyLoop" (class "java.util.regex.Pattern$Node")))) 
                                      (81 (aload_1)) 
                                      (82 (iload_2)) 
                                      (83 (aload_3)) 
                                      (84 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (87 (ifeq 92)) ;;to TAG_3
                                      (90 (iconst_1)) 
                                      (91 (ireturn)) 
                                      (92 (iload 4)) ;;at TAG_3
                                      (94 (aload_0)) 
                                      (95 (getfield (fieldCP "cmax" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (98 (if_icmpge 145)) ;;to TAG_4
                                      (101 (aload_1)) 
                                      (102 (getfield (fieldCP "locals" "java.util.regex.Matcher" (array int)))) 
                                      (105 (aload_0)) 
                                      (106 (getfield (fieldCP "countIndex" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (109 (iload 4)) 
                                      (111 (iconst_1)) 
                                      (112 (iadd)) 
                                      (113 (iastore)) 
                                      (114 (aload_0)) 
                                      (115 (getfield (fieldCP "body" "java.util.regex.Pattern$LazyLoop" (class "java.util.regex.Pattern$Node")))) 
                                      (118 (aload_1)) 
                                      (119 (iload_2)) 
                                      (120 (aload_3)) 
                                      (121 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (124 (istore 5)) 
                                      (126 (iload 5)) 
                                      (128 (ifne 142)) ;;to TAG_5
                                      (131 (aload_1)) 
                                      (132 (getfield (fieldCP "locals" "java.util.regex.Matcher" (array int)))) 
                                      (135 (aload_0)) 
                                      (136 (getfield (fieldCP "countIndex" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (139 (iload 4)) 
                                      (141 (iastore)) 
                                      (142 (iload 5)) ;;at TAG_5
                                      (144 (ireturn)) 
                                      (145 (iconst_0)) ;;at TAG_4
                                      (146 (ireturn)) 
                                      (147 (aload_0)) ;;at TAG_0
                                      (148 (getfield (fieldCP "next" "java.util.regex.Pattern$LazyLoop" (class "java.util.regex.Pattern$Node")))) 
                                      (151 (aload_1)) 
                                      (152 (iload_2)) 
                                      (153 (aload_3)) 
                                      (154 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (157 (ireturn)) 
                                      (endofcode 158))
                                   (Exceptions )
                                   (StackMap )))
                        (method "matchInit"
                              (parameters (class "java.util.regex.Matcher") int (class "java.lang.CharSequence"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 110)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (getfield (fieldCP "locals" "java.util.regex.Matcher" (array int)))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "countIndex" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (8 (iaload)) 
                                      (9 (istore 4)) 
                                      (11 (iconst_0)) 
                                      (12 (istore 5)) 
                                      (14 (iconst_0)) 
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "cmin" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (19 (if_icmpge 47)) ;;to TAG_0
                                      (22 (aload_1)) 
                                      (23 (getfield (fieldCP "locals" "java.util.regex.Matcher" (array int)))) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "countIndex" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (30 (iconst_1)) 
                                      (31 (iastore)) 
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "body" "java.util.regex.Pattern$LazyLoop" (class "java.util.regex.Pattern$Node")))) 
                                      (36 (aload_1)) 
                                      (37 (iload_2)) 
                                      (38 (aload_3)) 
                                      (39 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (42 (istore 5)) 
                                      (44 (goto 96)) ;;to TAG_1
                                      (47 (aload_0)) ;;at TAG_0
                                      (48 (getfield (fieldCP "next" "java.util.regex.Pattern$LazyLoop" (class "java.util.regex.Pattern$Node")))) 
                                      (51 (aload_1)) 
                                      (52 (iload_2)) 
                                      (53 (aload_3)) 
                                      (54 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (57 (ifeq 66))  ;;to TAG_2
                                      (60 (iconst_1)) 
                                      (61 (istore 5)) 
                                      (63 (goto 96)) ;;to TAG_1
                                      (66 (iconst_0)) ;;at TAG_2
                                      (67 (aload_0)) 
                                      (68 (getfield (fieldCP "cmax" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (71 (if_icmpge 96)) ;;to TAG_1
                                      (74 (aload_1)) 
                                      (75 (getfield (fieldCP "locals" "java.util.regex.Matcher" (array int)))) 
                                      (78 (aload_0)) 
                                      (79 (getfield (fieldCP "countIndex" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (82 (iconst_1)) 
                                      (83 (iastore)) 
                                      (84 (aload_0)) 
                                      (85 (getfield (fieldCP "body" "java.util.regex.Pattern$LazyLoop" (class "java.util.regex.Pattern$Node")))) 
                                      (88 (aload_1)) 
                                      (89 (iload_2)) 
                                      (90 (aload_3)) 
                                      (91 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (94 (istore 5)) 
                                      (96 (aload_1)) ;;at TAG_1
                                      (97 (getfield (fieldCP "locals" "java.util.regex.Matcher" (array int)))) 
                                      (100 (aload_0)) 
                                      (101 (getfield (fieldCP "countIndex" "java.util.regex.Pattern$LazyLoop" int))) 
                                      (104 (iload 4)) 
                                      (106 (iastore)) 
                                      (107 (iload 5)) 
                                      (109 (ireturn)) 
                                      (endofcode 110))
                                   (Exceptions )
                                   (StackMap )))
                        (method "study"
                              (parameters (class "java.util.regex.Pattern$TreeInfo"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (iconst_0))
                                      (2 (putfield (fieldCP "maxValid" "java.util.regex.Pattern$TreeInfo" boolean)))
                                      (5 (aload_1))
                                      (6 (iconst_0))
                                      (7 (putfield (fieldCP "deterministic" "java.util.regex.Pattern$TreeInfo" boolean)))
                                      (10 (iconst_0))
                                      (11 (ireturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Pattern$LazyLoop-class-table*
  (make-static-class-decls 
   *java.util.regex.Pattern$LazyLoop*))

(defconst *package-name-map* 
  ("java.util.regex.Pattern$LazyLoop" . "java.util.regex"))

