; PageFormat-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.print.PageFormat*
 (make-class-def
      '(class "java.awt.print.PageFormat"
            "java.lang.Object"
            (constant_pool
                        (INT 0)
                        (INT 1)
                        (INT 2)
                        (STRING  "unrecognized orientation")
                        (DOUBLE "-1.0"))
            (fields
                        (field "LANDSCAPE" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "PORTRAIT" int (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "REVERSE_LANDSCAPE" int (accessflags  *class*  *final*  *public*  *static* ) 2)
                        (field "mPaper" (class "java.awt.print.Paper") (accessflags  *class*  *private* ) -1)
                        (field "mOrientation" int (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_1))
                                      (6 (putfield (fieldCP "mOrientation" "java.awt.print.PageFormat" int)))
                                      (9 (aload_0))
                                      (10 (new (class "java.awt.print.Paper")))
                                      (13 (dup))
                                      (14 (invokespecial
					(methodCP "<init>" "java.awt.print.Paper" () void)))
                                      (17 (putfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper"))))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clone"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 34)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (invokespecial (methodCP "clone" "java.lang.Object" () (class "java.lang.Object")))) 
                                      (4 (checkcast (class "java.awt.print.PageFormat"))) 
                                      (7 (astore_1)) 
                                      (8 (aload_1)) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (13 (invokevirtual (methodCP "clone" "java.awt.print.Paper" () (class "java.lang.Object")))) 
                                      (16 (checkcast (class "java.awt.print.Paper"))) 
                                      (19 (putfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (22 (goto 32)) ;;to TAG_0;;at TAG_2
                                      (25 (astore_2)) ;;at TAG_3
                                      (26 (aload_2)) 
                                      (27 (invokevirtual (methodCP "printStackTrace" "java.lang.CloneNotSupportedException" () void))) 
                                      (30 (aconst_null)) 
                                      (31 (astore_1)) 
                                      (32 (aload_1)) ;;at TAG_0
                                      (33 (areturn)) 
                                      (endofcode 34))
                                   (Exceptions 
                                     (handler 0 22  25 (class "java.lang.CloneNotSupportedException")))
                                   (StackMap )))
                        (method "getWidth"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getOrientation" "java.awt.print.PageFormat" () int))) 
                                      (4 (istore_3)) 
                                      (5 (iload_3)) 
                                      (6 (iconst_1)) 
                                      (7 (if_icmpne 21))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (14 (invokevirtual (methodCP "getWidth" "java.awt.print.Paper" () double))) 
                                      (17 (dstore_1)) 
                                      (18 (goto 29)) ;;to TAG_1
                                      (21 (aload_0)) ;;at TAG_0
                                      (22 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (25 (invokevirtual (methodCP "getHeight" "java.awt.print.Paper" () double))) 
                                      (28 (dstore_1)) 
                                      (29 (dload_1)) ;;at TAG_1
                                      (30 (dreturn)) 
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getHeight"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getOrientation" "java.awt.print.PageFormat" () int))) 
                                      (4 (istore_3)) 
                                      (5 (iload_3)) 
                                      (6 (iconst_1)) 
                                      (7 (if_icmpne 21))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (14 (invokevirtual (methodCP "getHeight" "java.awt.print.Paper" () double))) 
                                      (17 (dstore_1)) 
                                      (18 (goto 29)) ;;to TAG_1
                                      (21 (aload_0)) ;;at TAG_0
                                      (22 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (25 (invokevirtual (methodCP "getWidth" "java.awt.print.Paper" () double))) 
                                      (28 (dstore_1)) 
                                      (29 (dload_1)) ;;at TAG_1
                                      (30 (dreturn)) 
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getImageableX"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 93)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getOrientation" "java.awt.print.PageFormat" () int))) 
                                      (4 (tableswitch (tableswitchinfo 81 (0 . 2) (32 59 70))))  ;;to TAG_2;;to TAG_3;;to TAG_0;;to TAG_1
                                      (32 (aload_0)) ;;at TAG_1
                                      (33 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (36 (invokevirtual (methodCP "getHeight" "java.awt.print.Paper" () double))) 
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (43 (invokevirtual (methodCP "getImageableY" "java.awt.print.Paper" () double))) 
                                      (46 (aload_0)) 
                                      (47 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (50 (invokevirtual (methodCP "getImageableHeight" "java.awt.print.Paper" () double))) 
                                      (53 (dadd)) 
                                      (54 (dsub)) 
                                      (55 (dstore_1)) 
                                      (56 (goto 91)) ;;to TAG_4
                                      (59 (aload_0)) ;;at TAG_2
                                      (60 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (63 (invokevirtual (methodCP "getImageableX" "java.awt.print.Paper" () double))) 
                                      (66 (dstore_1)) 
                                      (67 (goto 91)) ;;to TAG_4
                                      (70 (aload_0)) ;;at TAG_3
                                      (71 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (74 (invokevirtual (methodCP "getImageableY" "java.awt.print.Paper" () double))) 
                                      (77 (dstore_1)) 
                                      (78 (goto 91)) ;;to TAG_4
                                      (81 (new (class "java.lang.InternalError"))) ;;at TAG_0
                                      (84 (dup)) 
                                      (85 (ldc 3)) ;;STRING:: "unrecognized orientation"
                                      (87 (invokespecial (methodCP "<init>" "java.lang.InternalError" ((class "java.lang.String")) void))) 
                                      (90 (athrow)) 
                                      (91 (dload_1)) ;;at TAG_4
                                      (92 (dreturn)) 
                                      (endofcode 93))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getImageableY"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 93)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getOrientation" "java.awt.print.PageFormat" () int))) 
                                      (4 (tableswitch (tableswitchinfo 81 (0 . 2) (32 43 54))))  ;;to TAG_2;;to TAG_3;;to TAG_0;;to TAG_1
                                      (32 (aload_0)) ;;at TAG_1
                                      (33 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (36 (invokevirtual (methodCP "getImageableX" "java.awt.print.Paper" () double))) 
                                      (39 (dstore_1)) 
                                      (40 (goto 91)) ;;to TAG_4
                                      (43 (aload_0)) ;;at TAG_2
                                      (44 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (47 (invokevirtual (methodCP "getImageableY" "java.awt.print.Paper" () double))) 
                                      (50 (dstore_1)) 
                                      (51 (goto 91)) ;;to TAG_4
                                      (54 (aload_0)) ;;at TAG_3
                                      (55 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (58 (invokevirtual (methodCP "getWidth" "java.awt.print.Paper" () double))) 
                                      (61 (aload_0)) 
                                      (62 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (65 (invokevirtual (methodCP "getImageableX" "java.awt.print.Paper" () double))) 
                                      (68 (aload_0)) 
                                      (69 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (72 (invokevirtual (methodCP "getImageableWidth" "java.awt.print.Paper" () double))) 
                                      (75 (dadd)) 
                                      (76 (dsub)) 
                                      (77 (dstore_1)) 
                                      (78 (goto 91)) ;;to TAG_4
                                      (81 (new (class "java.lang.InternalError"))) ;;at TAG_0
                                      (84 (dup)) 
                                      (85 (ldc 3)) ;;STRING:: "unrecognized orientation"
                                      (87 (invokespecial (methodCP "<init>" "java.lang.InternalError" ((class "java.lang.String")) void))) 
                                      (90 (athrow)) 
                                      (91 (dload_1)) ;;at TAG_4
                                      (92 (dreturn)) 
                                      (endofcode 93))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getImageableWidth"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 29)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getOrientation" "java.awt.print.PageFormat" () int))) 
                                      (4 (iconst_1)) 
                                      (5 (if_icmpne 19))  ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (12 (invokevirtual (methodCP "getImageableWidth" "java.awt.print.Paper" () double))) 
                                      (15 (dstore_1)) 
                                      (16 (goto 27)) ;;to TAG_1
                                      (19 (aload_0)) ;;at TAG_0
                                      (20 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (23 (invokevirtual (methodCP "getImageableHeight" "java.awt.print.Paper" () double))) 
                                      (26 (dstore_1)) 
                                      (27 (dload_1)) ;;at TAG_1
                                      (28 (dreturn)) 
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getImageableHeight"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 29)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getOrientation" "java.awt.print.PageFormat" () int))) 
                                      (4 (iconst_1)) 
                                      (5 (if_icmpne 19))  ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (12 (invokevirtual (methodCP "getImageableHeight" "java.awt.print.Paper" () double))) 
                                      (15 (dstore_1)) 
                                      (16 (goto 27)) ;;to TAG_1
                                      (19 (aload_0)) ;;at TAG_0
                                      (20 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (23 (invokevirtual (methodCP "getImageableWidth" "java.awt.print.Paper" () double))) 
                                      (26 (dstore_1)) 
                                      (27 (dload_1)) ;;at TAG_1
                                      (28 (dreturn)) 
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPaper"
                              (parameters )
                              (returntype . (class "java.awt.print.Paper"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper"))))
                                      (4 (invokevirtual
					(methodCP "clone" "java.awt.print.Paper" () (class "java.lang.Object"))))
                                      (7 (checkcast (class "java.awt.print.Paper")))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setPaper"
                              (parameters (class "java.awt.print.Paper"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "clone" "java.awt.print.Paper" () (class "java.lang.Object"))))
                                      (5 (checkcast (class "java.awt.print.Paper")))
                                      (8 (putfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper"))))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setOrientation"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 27)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (iload_1)) 
                                      (2 (if_icmpgt 18))  ;;to TAG_0
                                      (5 (iload_1)) 
                                      (6 (iconst_2)) 
                                      (7 (if_icmpgt 18))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (iload_1)) 
                                      (12 (putfield (fieldCP "mOrientation" "java.awt.print.PageFormat" int))) 
                                      (15 (goto 26)) ;;to TAG_1
                                      (18 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_0
                                      (21 (dup)) 
                                      (22 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" () void))) 
                                      (25 (athrow)) 
                                      (26 (return)) ;;at TAG_1
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getOrientation"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "mOrientation" "java.awt.print.PageFormat" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMatrix"
                              (parameters )
                              (returntype . (array double))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 143)
                                   (parsedcode
                                      (0 (bipush 6)) 
                                      (2 (newarray DOUBLE)) 
                                      (4 (astore_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "mOrientation" "java.awt.print.PageFormat" int))) 
                                      (9 (tableswitch (tableswitchinfo 133 (0 . 2) (36 71 98))))  ;;to TAG_2;;to TAG_3;;to TAG_0;;to TAG_1
                                      (36 (aload_1)) ;;at TAG_1
                                      (37 (iconst_0)) 
                                      (38 (dconst_0)) 
                                      (39 (dastore)) 
                                      (40 (aload_1)) 
                                      (41 (iconst_1)) 
                                      (42 (ldc2_w 4)) ;; DOUBLE:: "-1.0"
                                      (45 (dastore)) 
                                      (46 (aload_1)) 
                                      (47 (iconst_2)) 
                                      (48 (dconst_1)) 
                                      (49 (dastore)) 
                                      (50 (aload_1)) 
                                      (51 (iconst_3)) 
                                      (52 (dconst_0)) 
                                      (53 (dastore)) 
                                      (54 (aload_1)) 
                                      (55 (iconst_4)) 
                                      (56 (dconst_0)) 
                                      (57 (dastore)) 
                                      (58 (aload_1)) 
                                      (59 (iconst_5)) 
                                      (60 (aload_0)) 
                                      (61 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (64 (invokevirtual (methodCP "getHeight" "java.awt.print.Paper" () double))) 
                                      (67 (dastore)) 
                                      (68 (goto 141)) ;;to TAG_4
                                      (71 (aload_1)) ;;at TAG_2
                                      (72 (iconst_0)) 
                                      (73 (dconst_1)) 
                                      (74 (dastore)) 
                                      (75 (aload_1)) 
                                      (76 (iconst_1)) 
                                      (77 (dconst_0)) 
                                      (78 (dastore)) 
                                      (79 (aload_1)) 
                                      (80 (iconst_2)) 
                                      (81 (dconst_0)) 
                                      (82 (dastore)) 
                                      (83 (aload_1)) 
                                      (84 (iconst_3)) 
                                      (85 (dconst_1)) 
                                      (86 (dastore)) 
                                      (87 (aload_1)) 
                                      (88 (iconst_4)) 
                                      (89 (dconst_0)) 
                                      (90 (dastore)) 
                                      (91 (aload_1)) 
                                      (92 (iconst_5)) 
                                      (93 (dconst_0)) 
                                      (94 (dastore)) 
                                      (95 (goto 141)) ;;to TAG_4
                                      (98 (aload_1)) ;;at TAG_3
                                      (99 (iconst_0)) 
                                      (100 (dconst_0)) 
                                      (101 (dastore)) 
                                      (102 (aload_1)) 
                                      (103 (iconst_1)) 
                                      (104 (dconst_1)) 
                                      (105 (dastore)) 
                                      (106 (aload_1)) 
                                      (107 (iconst_2)) 
                                      (108 (ldc2_w 4)) ;; DOUBLE:: "-1.0"
                                      (111 (dastore)) 
                                      (112 (aload_1)) 
                                      (113 (iconst_3)) 
                                      (114 (dconst_0)) 
                                      (115 (dastore)) 
                                      (116 (aload_1)) 
                                      (117 (iconst_4)) 
                                      (118 (aload_0)) 
                                      (119 (getfield (fieldCP "mPaper" "java.awt.print.PageFormat" (class "java.awt.print.Paper")))) 
                                      (122 (invokevirtual (methodCP "getWidth" "java.awt.print.Paper" () double))) 
                                      (125 (dastore)) 
                                      (126 (aload_1)) 
                                      (127 (iconst_5)) 
                                      (128 (dconst_0)) 
                                      (129 (dastore)) 
                                      (130 (goto 141)) ;;to TAG_4
                                      (133 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_0
                                      (136 (dup)) 
                                      (137 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" () void))) 
                                      (140 (athrow)) 
                                      (141 (aload_1)) ;;at TAG_4
                                      (142 (areturn)) 
                                      (endofcode 143))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.lang.Cloneable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *PageFormat-class-table*
  (make-static-class-decls 
   *java.awt.print.PageFormat*))

(defconst *package-name-map* 
  ("java.awt.print.PageFormat" . "java.awt.print"))
