; Pattern$BnMS-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.Pattern$BnMS*
 (make-class-def
      '(class "java.util.regex.Pattern$BnMS"
            "java.util.regex.Pattern$BnM"
            (constant_pool)
            (fields
                        (field "lengthInChars" int (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (array int) (array int) (array int) (class "java.util.regex.Pattern$Node"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 5) (max_locals . 6) (code_length . 48)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (aload_2)) 
                                      (3 (aload_3)) 
                                      (4 (aload 4)) 
                                      (6 (invokespecial (methodCP "<init>" "java.util.regex.Pattern$BnM" ((array int) (array int) (array int) (class "java.util.regex.Pattern$Node")) void))) 
                                      (9 (iconst_0)) 
                                      (10 (istore 5)) 
                                      (12 (iload 5)) ;;at TAG_1
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "buffer" "java.util.regex.Pattern$BnMS" (array int)))) 
                                      (18 (arraylength)) 
                                      (19 (if_icmpge 47))  ;;to TAG_0
                                      (22 (aload_0)) 
                                      (23 (dup)) 
                                      (24 (getfield (fieldCP "lengthInChars" "java.util.regex.Pattern$BnMS" int))) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "buffer" "java.util.regex.Pattern$BnMS" (array int)))) 
                                      (31 (iload 5)) 
                                      (33 (iaload)) 
                                      (34 (invokestatic (methodCP "charCount" "java.lang.Character" (int) int))) 
                                      (37 (iadd)) 
                                      (38 (putfield (fieldCP "lengthInChars" "java.util.regex.Pattern$BnMS" int))) 
                                      (41 (iinc 5 1)) 
                                      (44 (goto 12)) ;;to TAG_1
                                      (47 (return)) ;;at TAG_0
                                      (endofcode 48))
                                   (Exceptions )
                                   (StackMap )))
                        (method "match"
                              (parameters (class "java.util.regex.Matcher") int (class "java.lang.CharSequence"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 11) (code_length . 197)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "buffer" "java.util.regex.Pattern$BnMS" (array int)))) 
                                      (4 (astore 4)) 
                                      (6 (aload 4)) 
                                      (8 (arraylength)) 
                                      (9 (istore 5)) 
                                      (11 (aload_1)) 
                                      (12 (getfield (fieldCP "to" "java.util.regex.Matcher" int))) 
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "lengthInChars" "java.util.regex.Pattern$BnMS" int))) 
                                      (19 (isub)) 
                                      (20 (istore 6)) 
                                      (22 (iload_2)) ;;at TAG_3
                                      (23 (iload 6)) 
                                      (25 (if_icmpgt 190)) ;;to TAG_0
                                      (28 (aload_3)) 
                                      (29 (iload_2)) 
                                      (30 (iload 5)) 
                                      (32 (invokestatic (methodCP "access$300" "java.util.regex.Pattern" ((class "java.lang.CharSequence") int int) int))) 
                                      (35 (istore 8)) 
                                      (37 (iload 5)) 
                                      (39 (iconst_1)) 
                                      (40 (isub)) 
                                      (41 (istore 9)) 
                                      (43 (iload 8)) ;;at TAG_4
                                      (45 (ifle 124)) ;;to TAG_1
                                      (48 (aload_3)) 
                                      (49 (iload_2)) 
                                      (50 (iload 8)) 
                                      (52 (iadd)) 
                                      (53 (invokestatic (methodCP "codePointBefore" "java.lang.Character" ((class "java.lang.CharSequence") int) int))) 
                                      (56 (istore 7)) 
                                      (58 (iload 7)) 
                                      (60 (aload 4)) 
                                      (62 (iload 9)) 
                                      (64 (iaload)) 
                                      (65 (if_icmpeq 108))  ;;to TAG_2
                                      (68 (iload 9)) 
                                      (70 (iconst_1)) 
                                      (71 (iadd)) 
                                      (72 (aload_0)) 
                                      (73 (getfield (fieldCP "lastOcc" "java.util.regex.Pattern$BnMS" (array int)))) 
                                      (76 (iload 7)) 
                                      (78 (bipush 127)) 
                                      (80 (iand)) 
                                      (81 (iaload)) 
                                      (82 (isub)) 
                                      (83 (aload_0)) 
                                      (84 (getfield (fieldCP "optoSft" "java.util.regex.Pattern$BnMS" (array int)))) 
                                      (87 (iload 9)) 
                                      (89 (iaload)) 
                                      (90 (invokestatic (methodCP "max" "java.lang.Math" (int int) int))) 
                                      (93 (istore 10)) 
                                      (95 (iload_2)) 
                                      (96 (aload_3)) 
                                      (97 (iload_2)) 
                                      (98 (iload 10)) 
                                      (100 (invokestatic (methodCP "access$300" "java.util.regex.Pattern" ((class "java.lang.CharSequence") int int) int))) 
                                      (103 (iadd)) 
                                      (104 (istore_2)) 
                                      (105 (goto 22)) ;;to TAG_3
                                      (108 (iload 8)) ;;at TAG_2
                                      (110 (iload 7)) 
                                      (112 (invokestatic (methodCP "charCount" "java.lang.Character" (int) int))) 
                                      (115 (isub)) 
                                      (116 (istore 8)) 
                                      (118 (iinc 9 -1)) 
                                      (121 (goto 43)) ;;to TAG_4
                                      (124 (aload_1)) ;;at TAG_1
                                      (125 (iload_2)) 
                                      (126 (putfield (fieldCP "first" "java.util.regex.Matcher" int))) 
                                      (129 (aload_0)) 
                                      (130 (getfield (fieldCP "next" "java.util.regex.Pattern$BnMS" (class "java.util.regex.Pattern$Node")))) 
                                      (133 (aload_1)) 
                                      (134 (iload_2)) 
                                      (135 (aload_0)) 
                                      (136 (getfield (fieldCP "lengthInChars" "java.util.regex.Pattern$BnMS" int))) 
                                      (139 (iadd)) 
                                      (140 (aload_3)) 
                                      (141 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (144 (istore 8)) 
                                      (146 (iload 8)) 
                                      (148 (ifeq 178)) ;;to TAG_5
                                      (151 (aload_1)) 
                                      (152 (iload_2)) 
                                      (153 (putfield (fieldCP "first" "java.util.regex.Matcher" int))) 
                                      (156 (aload_1)) 
                                      (157 (getfield (fieldCP "groups" "java.util.regex.Matcher" (array int)))) 
                                      (160 (iconst_0)) 
                                      (161 (aload_1)) 
                                      (162 (getfield (fieldCP "first" "java.util.regex.Matcher" int))) 
                                      (165 (iastore)) 
                                      (166 (aload_1)) 
                                      (167 (getfield (fieldCP "groups" "java.util.regex.Matcher" (array int)))) 
                                      (170 (iconst_1)) 
                                      (171 (aload_1)) 
                                      (172 (getfield (fieldCP "last" "java.util.regex.Matcher" int))) 
                                      (175 (iastore)) 
                                      (176 (iconst_1)) 
                                      (177 (ireturn)) 
                                      (178 (iload_2)) ;;at TAG_5
                                      (179 (aload_3)) 
                                      (180 (iload_2)) 
                                      (181 (iconst_1)) 
                                      (182 (invokestatic (methodCP "access$300" "java.util.regex.Pattern" ((class "java.lang.CharSequence") int int) int))) 
                                      (185 (iadd)) 
                                      (186 (istore_2)) 
                                      (187 (goto 22)) ;;to TAG_3
                                      (190 (aload_1)) ;;at TAG_0
                                      (191 (iconst_1)) 
                                      (192 (putfield (fieldCP "hitEnd" "java.util.regex.Matcher" boolean))) 
                                      (195 (iconst_0)) 
                                      (196 (ireturn)) 
                                      (endofcode 197))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Pattern$BnMS-class-table*
  (make-static-class-decls 
   *java.util.regex.Pattern$BnMS*))

(defconst *package-name-map* 
  ("java.util.regex.Pattern$BnMS" . "java.util.regex"))

