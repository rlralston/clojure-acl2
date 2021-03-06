; StringBufferInputStream-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.StringBufferInputStream*
 (make-class-def
      '(class "java.io.StringBufferInputStream"
            "java.io.InputStream"
            (constant_pool)
            (fields
                        (field "buffer" (class "java.lang.String") (accessflags  *class*  *protected* ) -1)
                        (field "pos" int (accessflags  *class*  *protected* ) -1)
                        (field "count" int (accessflags  *class*  *protected* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.io.InputStream" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "buffer" "java.io.StringBufferInputStream" (class "java.lang.String"))))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (invokevirtual
					(methodCP "length" "java.lang.String" () int)))
                                      (14 (putfield (fieldCP "count" "java.io.StringBufferInputStream" int)))
                                      (17 (return))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "count" "java.io.StringBufferInputStream" int))) 
                                      (8 (if_icmpge 36))  ;;to TAG_0
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "buffer" "java.io.StringBufferInputStream" (class "java.lang.String")))) 
                                      (15 (aload_0)) 
                                      (16 (dup)) 
                                      (17 (getfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (20 (dup_x1)) 
                                      (21 (iconst_1)) 
                                      (22 (iadd)) 
                                      (23 (putfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (26 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (29 (sipush 255)) 
                                      (32 (iand)) 
                                      (33 (goto 37)) ;;to TAG_1
                                      (36 (iconst_m1)) ;;at TAG_0
                                      (37 (ireturn)) ;;at TAG_1
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters (array byte) int int)
                              (returntype . int)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 7) (max_locals . 6) (code_length . 135)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 12)) ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (iload_2)) ;;at TAG_0
                                      (13 (iflt 40)) ;;to TAG_1
                                      (16 (iload_2)) 
                                      (17 (aload_1)) 
                                      (18 (arraylength)) 
                                      (19 (if_icmpgt 40)) ;;to TAG_1
                                      (22 (iload_3)) 
                                      (23 (iflt 40)) ;;to TAG_1
                                      (26 (iload_2)) 
                                      (27 (iload_3)) 
                                      (28 (iadd)) 
                                      (29 (aload_1)) 
                                      (30 (arraylength)) 
                                      (31 (if_icmpgt 40)) ;;to TAG_1
                                      (34 (iload_2)) 
                                      (35 (iload_3)) 
                                      (36 (iadd)) 
                                      (37 (ifge 48))  ;;to TAG_2
                                      (40 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_1
                                      (43 (dup)) 
                                      (44 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (47 (athrow)) 
                                      (48 (aload_0)) ;;at TAG_2
                                      (49 (getfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (52 (aload_0)) 
                                      (53 (getfield (fieldCP "count" "java.io.StringBufferInputStream" int))) 
                                      (56 (if_icmplt 61)) ;;to TAG_3
                                      (59 (iconst_m1)) 
                                      (60 (ireturn)) 
                                      (61 (aload_0)) ;;at TAG_3
                                      (62 (getfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (65 (iload_3)) 
                                      (66 (iadd)) 
                                      (67 (aload_0)) 
                                      (68 (getfield (fieldCP "count" "java.io.StringBufferInputStream" int))) 
                                      (71 (if_icmple 84)) ;;to TAG_4
                                      (74 (aload_0)) 
                                      (75 (getfield (fieldCP "count" "java.io.StringBufferInputStream" int))) 
                                      (78 (aload_0)) 
                                      (79 (getfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (82 (isub)) 
                                      (83 (istore_3)) 
                                      (84 (iload_3)) ;;at TAG_4
                                      (85 (ifgt 90)) ;;to TAG_5
                                      (88 (iconst_0)) 
                                      (89 (ireturn)) 
                                      (90 (aload_0)) ;;at TAG_5
                                      (91 (getfield (fieldCP "buffer" "java.io.StringBufferInputStream" (class "java.lang.String")))) 
                                      (94 (astore 4)) 
                                      (96 (iload_3)) 
                                      (97 (istore 5)) 
                                      (99 (iinc 5 -1)) ;;at TAG_7
                                      (102 (iload 5)) 
                                      (104 (iflt 133)) ;;to TAG_6
                                      (107 (aload_1)) 
                                      (108 (iload_2)) 
                                      (109 (iinc 2 1)) 
                                      (112 (aload 4)) 
                                      (114 (aload_0)) 
                                      (115 (dup)) 
                                      (116 (getfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (119 (dup_x1)) 
                                      (120 (iconst_1)) 
                                      (121 (iadd)) 
                                      (122 (putfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (125 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (128 (i2b)) 
                                      (129 (bastore)) 
                                      (130 (goto 99)) ;;to TAG_7
                                      (133 (iload_3)) ;;at TAG_6
                                      (134 (ireturn)) 
                                      (endofcode 135))
                                   (Exceptions )
                                   (StackMap )))
                        (method "skip"
                              (parameters long)
                              (returntype . long)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 48)
                                   (parsedcode
                                      (0 (lload_1)) 
                                      (1 (lconst_0)) 
                                      (2 (lcmp)) 
                                      (3 (ifge 8))  ;;to TAG_0
                                      (6 (lconst_0)) 
                                      (7 (lreturn)) 
                                      (8 (lload_1)) ;;at TAG_0
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "count" "java.io.StringBufferInputStream" int))) 
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (17 (isub)) 
                                      (18 (i2l)) 
                                      (19 (lcmp)) 
                                      (20 (ifle 34)) ;;to TAG_1
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "count" "java.io.StringBufferInputStream" int))) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (31 (isub)) 
                                      (32 (i2l)) 
                                      (33 (lstore_1)) 
                                      (34 (aload_0)) ;;at TAG_1
                                      (35 (dup)) 
                                      (36 (getfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (39 (i2l)) 
                                      (40 (lload_1)) 
                                      (41 (ladd)) 
                                      (42 (l2i)) 
                                      (43 (putfield (fieldCP "pos" "java.io.StringBufferInputStream" int))) 
                                      (46 (lload_1)) 
                                      (47 (lreturn)) 
                                      (endofcode 48))
                                   (Exceptions )
                                   (StackMap )))
                        (method "available"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "count" "java.io.StringBufferInputStream" int)))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "pos" "java.io.StringBufferInputStream" int)))
                                      (8 (isub))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reset"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iconst_0))
                                      (2 (putfield (fieldCP "pos" "java.io.StringBufferInputStream" int)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Deprecated")
              (attribute "RuntimeVisibleAnnotations")))))


(defconst *StringBufferInputStream-class-table*
  (make-static-class-decls 
   *java.io.StringBufferInputStream*))

(defconst *package-name-map* 
  ("java.io.StringBufferInputStream" . "java.io"))

