; Pattern$Branch-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.Pattern$Branch*
 (make-class-def
      '(class "java.util.regex.Pattern$Branch"
            "java.util.regex.Pattern$Node"
            (constant_pool
                        (INT 2147483647))
            (fields
                        (field "atoms" (array (class "java.util.regex.Pattern$Node")) (accessflags  *class* ) -1)
                        (field "size" int (accessflags  *class* ) -1)
                        (field "conn" (class "java.util.regex.Pattern$Node") (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.regex.Pattern$Node") (class "java.util.regex.Pattern$Node") (class "java.util.regex.Pattern$Node"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 37)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.regex.Pattern$Node" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_2))
                                      (6 (anewarray (class "java.util.regex.Pattern$Node")))
                                      (9 (putfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node")))))
                                      (12 (aload_0))
                                      (13 (iconst_2))
                                      (14 (putfield (fieldCP "size" "java.util.regex.Pattern$Branch" int)))
                                      (17 (aload_0))
                                      (18 (aload_3))
                                      (19 (putfield (fieldCP "conn" "java.util.regex.Pattern$Branch" (class "java.util.regex.Pattern$Node"))))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node")))))
                                      (26 (iconst_0))
                                      (27 (aload_1))
                                      (28 (aastore))
                                      (29 (aload_0))
                                      (30 (getfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node")))))
                                      (33 (iconst_1))
                                      (34 (aload_2))
                                      (35 (aastore))
                                      (36 (return))
                                      (endofcode 37))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.util.regex.Pattern$Node"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 61)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "size" "java.util.regex.Pattern$Branch" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node"))))) 
                                      (8 (arraylength)) 
                                      (9 (if_icmplt 43))  ;;to TAG_0
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node"))))) 
                                      (16 (arraylength)) 
                                      (17 (iconst_2)) 
                                      (18 (imul)) 
                                      (19 (anewarray (class "java.util.regex.Pattern$Node"))) 
                                      (22 (astore_2)) 
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node"))))) 
                                      (27 (iconst_0)) 
                                      (28 (aload_2)) 
                                      (29 (iconst_0)) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node"))))) 
                                      (34 (arraylength)) 
                                      (35 (invokestatic (methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void))) 
                                      (38 (aload_0)) 
                                      (39 (aload_2)) 
                                      (40 (putfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node"))))) 
                                      (43 (aload_0)) ;;at TAG_0
                                      (44 (getfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node"))))) 
                                      (47 (aload_0)) 
                                      (48 (dup)) 
                                      (49 (getfield (fieldCP "size" "java.util.regex.Pattern$Branch" int))) 
                                      (52 (dup_x1)) 
                                      (53 (iconst_1)) 
                                      (54 (iadd)) 
                                      (55 (putfield (fieldCP "size" "java.util.regex.Pattern$Branch" int))) 
                                      (58 (aload_1)) 
                                      (59 (aastore)) 
                                      (60 (return)) 
                                      (endofcode 61))
                                   (Exceptions )
                                   (StackMap )))
                        (method "match"
                              (parameters (class "java.util.regex.Matcher") int (class "java.lang.CharSequence"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 66)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore 4)) 
                                      (3 (iload 4)) ;;at TAG_3
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "size" "java.util.regex.Pattern$Branch" int))) 
                                      (9 (if_icmpge 64)) ;;to TAG_0
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node"))))) 
                                      (16 (iload 4)) 
                                      (18 (aaload)) 
                                      (19 (ifnonnull 40)) ;;to TAG_1
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "conn" "java.util.regex.Pattern$Branch" (class "java.util.regex.Pattern$Node")))) 
                                      (26 (getfield (fieldCP "next" "java.util.regex.Pattern$Node" (class "java.util.regex.Pattern$Node")))) 
                                      (29 (aload_1)) 
                                      (30 (iload_2)) 
                                      (31 (aload_3)) 
                                      (32 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (35 (ifeq 58))  ;;to TAG_2
                                      (38 (iconst_1)) 
                                      (39 (ireturn)) 
                                      (40 (aload_0)) ;;at TAG_1
                                      (41 (getfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node"))))) 
                                      (44 (iload 4)) 
                                      (46 (aaload)) 
                                      (47 (aload_1)) 
                                      (48 (iload_2)) 
                                      (49 (aload_3)) 
                                      (50 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (53 (ifeq 58))  ;;to TAG_2
                                      (56 (iconst_1)) 
                                      (57 (ireturn)) 
                                      (58 (iinc 4 1)) ;;at TAG_2
                                      (61 (goto 3)) ;;to TAG_3
                                      (64 (iconst_0)) ;;at TAG_0
                                      (65 (ireturn)) 
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap )))
                        (method "study"
                              (parameters (class "java.util.regex.Pattern$TreeInfo"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 8) (code_length . 162)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (getfield (fieldCP "minLength" "java.util.regex.Pattern$TreeInfo" int))) 
                                      (4 (istore_2)) 
                                      (5 (aload_1)) 
                                      (6 (getfield (fieldCP "maxLength" "java.util.regex.Pattern$TreeInfo" int))) 
                                      (9 (istore_3)) 
                                      (10 (aload_1)) 
                                      (11 (getfield (fieldCP "maxValid" "java.util.regex.Pattern$TreeInfo" boolean))) 
                                      (14 (istore 4)) 
                                      (16 (ldc 0)) ;;INT:: "2147483647"
                                      (18 (istore 5)) 
                                      (20 (iconst_m1)) 
                                      (21 (istore 6)) 
                                      (23 (iconst_0)) 
                                      (24 (istore 7)) 
                                      (26 (iload 7)) ;;at TAG_2
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "size" "java.util.regex.Pattern$Branch" int))) 
                                      (32 (if_icmpge 98)) ;;to TAG_0
                                      (35 (aload_1)) 
                                      (36 (invokevirtual (methodCP "reset" "java.util.regex.Pattern$TreeInfo" () void))) 
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node"))))) 
                                      (43 (iload 7)) 
                                      (45 (aaload)) 
                                      (46 (ifnull 61)) ;;to TAG_1
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "atoms" "java.util.regex.Pattern$Branch" (array (class "java.util.regex.Pattern$Node"))))) 
                                      (53 (iload 7)) 
                                      (55 (aaload)) 
                                      (56 (aload_1)) 
                                      (57 (invokevirtual (methodCP "study" "java.util.regex.Pattern$Node" ((class "java.util.regex.Pattern$TreeInfo")) boolean))) 
                                      (60 (pop)) 
                                      (61 (iload 5)) ;;at TAG_1
                                      (63 (aload_1)) 
                                      (64 (getfield (fieldCP "minLength" "java.util.regex.Pattern$TreeInfo" int))) 
                                      (67 (invokestatic (methodCP "min" "java.lang.Math" (int int) int))) 
                                      (70 (istore 5)) 
                                      (72 (iload 6)) 
                                      (74 (aload_1)) 
                                      (75 (getfield (fieldCP "maxLength" "java.util.regex.Pattern$TreeInfo" int))) 
                                      (78 (invokestatic (methodCP "max" "java.lang.Math" (int int) int))) 
                                      (81 (istore 6)) 
                                      (83 (iload 4)) 
                                      (85 (aload_1)) 
                                      (86 (getfield (fieldCP "maxValid" "java.util.regex.Pattern$TreeInfo" boolean))) 
                                      (89 (iand)) 
                                      (90 (istore 4)) 
                                      (92 (iinc 7 1)) 
                                      (95 (goto 26))  ;;to TAG_2
                                      (98 (iload_2)) ;;at TAG_0
                                      (99 (iload 5)) 
                                      (101 (iadd)) 
                                      (102 (istore_2)) 
                                      (103 (iload_3)) 
                                      (104 (iload 6)) 
                                      (106 (iadd)) 
                                      (107 (istore_3)) 
                                      (108 (aload_1)) 
                                      (109 (invokevirtual (methodCP "reset" "java.util.regex.Pattern$TreeInfo" () void))) 
                                      (112 (aload_0)) 
                                      (113 (getfield (fieldCP "conn" "java.util.regex.Pattern$Branch" (class "java.util.regex.Pattern$Node")))) 
                                      (116 (getfield (fieldCP "next" "java.util.regex.Pattern$Node" (class "java.util.regex.Pattern$Node")))) 
                                      (119 (aload_1)) 
                                      (120 (invokevirtual (methodCP "study" "java.util.regex.Pattern$Node" ((class "java.util.regex.Pattern$TreeInfo")) boolean))) 
                                      (123 (pop)) 
                                      (124 (aload_1)) 
                                      (125 (dup)) 
                                      (126 (getfield (fieldCP "minLength" "java.util.regex.Pattern$TreeInfo" int))) 
                                      (129 (iload_2)) 
                                      (130 (iadd)) 
                                      (131 (putfield (fieldCP "minLength" "java.util.regex.Pattern$TreeInfo" int))) 
                                      (134 (aload_1)) 
                                      (135 (dup)) 
                                      (136 (getfield (fieldCP "maxLength" "java.util.regex.Pattern$TreeInfo" int))) 
                                      (139 (iload_3)) 
                                      (140 (iadd)) 
                                      (141 (putfield (fieldCP "maxLength" "java.util.regex.Pattern$TreeInfo" int))) 
                                      (144 (aload_1)) 
                                      (145 (dup)) 
                                      (146 (getfield (fieldCP "maxValid" "java.util.regex.Pattern$TreeInfo" boolean))) 
                                      (149 (iload 4)) 
                                      (151 (iand)) 
                                      (152 (putfield (fieldCP "maxValid" "java.util.regex.Pattern$TreeInfo" boolean))) 
                                      (155 (aload_1)) 
                                      (156 (iconst_0)) 
                                      (157 (putfield (fieldCP "deterministic" "java.util.regex.Pattern$TreeInfo" boolean))) 
                                      (160 (iconst_0)) 
                                      (161 (ireturn)) 
                                      (endofcode 162))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Pattern$Branch-class-table*
  (make-static-class-decls 
   *java.util.regex.Pattern$Branch*))

(defconst *package-name-map* 
  ("java.util.regex.Pattern$Branch" . "java.util.regex"))

