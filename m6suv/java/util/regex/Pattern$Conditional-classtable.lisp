; Pattern$Conditional-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.Pattern$Conditional*
 (make-class-def
      '(class "java.util.regex.Pattern$Conditional"
            "java.util.regex.Pattern$Node"
            (constant_pool)
            (fields
                        (field "cond" (class "java.util.regex.Pattern$Node") (accessflags  *class* ) -1)
                        (field "yes" (class "java.util.regex.Pattern$Node") (accessflags  *class* ) -1)
                        (field "not" (class "java.util.regex.Pattern$Node") (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.regex.Pattern$Node") (class "java.util.regex.Pattern$Node") (class "java.util.regex.Pattern$Node"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.regex.Pattern$Node" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "cond" "java.util.regex.Pattern$Conditional" (class "java.util.regex.Pattern$Node"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "yes" "java.util.regex.Pattern$Conditional" (class "java.util.regex.Pattern$Node"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "not" "java.util.regex.Pattern$Conditional" (class "java.util.regex.Pattern$Node"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "match"
                              (parameters (class "java.util.regex.Matcher") int (class "java.lang.CharSequence"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "cond" "java.util.regex.Pattern$Conditional" (class "java.util.regex.Pattern$Node")))) 
                                      (4 (aload_1)) 
                                      (5 (iload_2)) 
                                      (6 (aload_3)) 
                                      (7 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (10 (ifeq 24))  ;;to TAG_0
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "yes" "java.util.regex.Pattern$Conditional" (class "java.util.regex.Pattern$Node")))) 
                                      (17 (aload_1)) 
                                      (18 (iload_2)) 
                                      (19 (aload_3)) 
                                      (20 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (23 (ireturn)) 
                                      (24 (aload_0)) ;;at TAG_0
                                      (25 (getfield (fieldCP "not" "java.util.regex.Pattern$Conditional" (class "java.util.regex.Pattern$Node")))) 
                                      (28 (aload_1)) 
                                      (29 (iload_2)) 
                                      (30 (aload_3)) 
                                      (31 (invokevirtual (methodCP "match" "java.util.regex.Pattern$Node" ((class "java.util.regex.Matcher") int (class "java.lang.CharSequence")) boolean))) 
                                      (34 (ireturn)) 
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "study"
                              (parameters (class "java.util.regex.Pattern$TreeInfo"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 8) (code_length . 118)
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
                                      (16 (aload_1))
                                      (17 (invokevirtual
					(methodCP "reset" "java.util.regex.Pattern$TreeInfo" () void)))
                                      (20 (aload_0))
                                      (21 (getfield (fieldCP "yes" "java.util.regex.Pattern$Conditional" (class "java.util.regex.Pattern$Node"))))
                                      (24 (aload_1))
                                      (25 (invokevirtual
					(methodCP "study" "java.util.regex.Pattern$Node" ((class "java.util.regex.Pattern$TreeInfo")) boolean)))
                                      (28 (pop))
                                      (29 (aload_1))
                                      (30 (getfield (fieldCP "minLength" "java.util.regex.Pattern$TreeInfo" int)))
                                      (33 (istore 5))
                                      (35 (aload_1))
                                      (36 (getfield (fieldCP "maxLength" "java.util.regex.Pattern$TreeInfo" int)))
                                      (39 (istore 6))
                                      (41 (aload_1))
                                      (42 (getfield (fieldCP "maxValid" "java.util.regex.Pattern$TreeInfo" boolean)))
                                      (45 (istore 7))
                                      (47 (aload_1))
                                      (48 (invokevirtual
					(methodCP "reset" "java.util.regex.Pattern$TreeInfo" () void)))
                                      (51 (aload_0))
                                      (52 (getfield (fieldCP "not" "java.util.regex.Pattern$Conditional" (class "java.util.regex.Pattern$Node"))))
                                      (55 (aload_1))
                                      (56 (invokevirtual
					(methodCP "study" "java.util.regex.Pattern$Node" ((class "java.util.regex.Pattern$TreeInfo")) boolean)))
                                      (59 (pop))
                                      (60 (aload_1))
                                      (61 (iload_2))
                                      (62 (iload 5))
                                      (64 (aload_1))
                                      (65 (getfield (fieldCP "minLength" "java.util.regex.Pattern$TreeInfo" int)))
                                      (68 (invokestatic
					(methodCP "min" "java.lang.Math" (int int) int)))
                                      (71 (iadd))
                                      (72 (putfield (fieldCP "minLength" "java.util.regex.Pattern$TreeInfo" int)))
                                      (75 (aload_1))
                                      (76 (iload_3))
                                      (77 (iload 6))
                                      (79 (aload_1))
                                      (80 (getfield (fieldCP "maxLength" "java.util.regex.Pattern$TreeInfo" int)))
                                      (83 (invokestatic
					(methodCP "max" "java.lang.Math" (int int) int)))
                                      (86 (iadd))
                                      (87 (putfield (fieldCP "maxLength" "java.util.regex.Pattern$TreeInfo" int)))
                                      (90 (aload_1))
                                      (91 (iload 4))
                                      (93 (iload 7))
                                      (95 (iand))
                                      (96 (aload_1))
                                      (97 (getfield (fieldCP "maxValid" "java.util.regex.Pattern$TreeInfo" boolean)))
                                      (100 (iand))
                                      (101 (putfield (fieldCP "maxValid" "java.util.regex.Pattern$TreeInfo" boolean)))
                                      (104 (aload_1))
                                      (105 (iconst_0))
                                      (106 (putfield (fieldCP "deterministic" "java.util.regex.Pattern$TreeInfo" boolean)))
                                      (109 (aload_0))
                                      (110 (getfield (fieldCP "next" "java.util.regex.Pattern$Conditional" (class "java.util.regex.Pattern$Node"))))
                                      (113 (aload_1))
                                      (114 (invokevirtual
					(methodCP "study" "java.util.regex.Pattern$Node" ((class "java.util.regex.Pattern$TreeInfo")) boolean)))
                                      (117 (ireturn))
                                      (endofcode 118))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Pattern$Conditional-class-table*
  (make-static-class-decls 
   *java.util.regex.Pattern$Conditional*))

(defconst *package-name-map* 
  ("java.util.regex.Pattern$Conditional" . "java.util.regex"))

