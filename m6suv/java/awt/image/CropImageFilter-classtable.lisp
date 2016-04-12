; CropImageFilter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:28 CDT 2014.
;

(defconst *java.awt.image.CropImageFilter*
 (make-class-def
      '(class "java.awt.image.CropImageFilter"
            "java.awt.image.ImageFilter"
            (constant_pool
                        (STRING  "croprect")
                        (INT 2147483647)
                        (INT -2147483648))
            (fields
                        (field "cropX" int (accessflags  *class* ) -1)
                        (field "cropY" int (accessflags  *class* ) -1)
                        (field "cropW" int (accessflags  *class* ) -1)
                        (field "cropH" int (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters int int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.awt.image.ImageFilter" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (putfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int)))
                                      (9 (aload_0))
                                      (10 (iload_2))
                                      (11 (putfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int)))
                                      (14 (aload_0))
                                      (15 (iload_3))
                                      (16 (putfield (fieldCP "cropW" "java.awt.image.CropImageFilter" int)))
                                      (19 (aload_0))
                                      (20 (iload 4))
                                      (22 (putfield (fieldCP "cropH" "java.awt.image.CropImageFilter" int)))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setProperties"
                              (parameters (class "java.util.Hashtable"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 3) (code_length . 44)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokevirtual
					(methodCP "clone" "java.util.Hashtable" () (class "java.lang.Object"))))
                                      (4 (checkcast (class "java.util.Hashtable")))
                                      (7 (astore_2))
                                      (8 (aload_2))
                                      (9 (ldc 0))         ;;STRING:: "croprect"
                                      (11 (new (class "java.awt.Rectangle")))
                                      (14 (dup))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int)))
                                      (19 (aload_0))
                                      (20 (getfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int)))
                                      (23 (aload_0))
                                      (24 (getfield (fieldCP "cropW" "java.awt.image.CropImageFilter" int)))
                                      (27 (aload_0))
                                      (28 (getfield (fieldCP "cropH" "java.awt.image.CropImageFilter" int)))
                                      (31 (invokespecial
					(methodCP "<init>" "java.awt.Rectangle" (int int int int) void)))
                                      (34 (invokevirtual
					(methodCP "put" "java.util.Hashtable" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (37 (pop))
                                      (38 (aload_0))
                                      (39 (aload_2))
                                      (40 (invokespecial
					(methodCP "setProperties" "java.awt.image.ImageFilter" ((class "java.util.Hashtable")) void)))
                                      (43 (return))
                                      (endofcode 44))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setDimensions"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "consumer" "java.awt.image.CropImageFilter" (class "java.awt.image.ImageConsumer"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "cropW" "java.awt.image.CropImageFilter" int)))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "cropH" "java.awt.image.CropImageFilter" int)))
                                      (12 (invokeinterface
					(methodCP "setDimensions" "java.awt.image.ImageConsumer" (int int) void) 3))
                                      (17 (return))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setPixels"
                              (parameters int int int int (class "java.awt.image.ColorModel") (array byte) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 10) (max_locals . 13) (code_length . 173)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (istore 9)) 
                                      (3 (iload 9)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int))) 
                                      (9 (if_icmpge 18)) ;;to TAG_0
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int))) 
                                      (16 (istore 9)) 
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (iload_1)) 
                                      (20 (iload_3)) 
                                      (21 (invokespecial (methodCP "addWithoutOverflow" "java.awt.image.CropImageFilter" (int int) int))) 
                                      (24 (istore 10)) 
                                      (26 (iload 10)) 
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int))) 
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "cropW" "java.awt.image.CropImageFilter" int))) 
                                      (36 (iadd)) 
                                      (37 (if_icmple 51)) ;;to TAG_1
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int))) 
                                      (44 (aload_0)) 
                                      (45 (getfield (fieldCP "cropW" "java.awt.image.CropImageFilter" int))) 
                                      (48 (iadd)) 
                                      (49 (istore 10)) 
                                      (51 (iload_2)) ;;at TAG_1
                                      (52 (istore 11)) 
                                      (54 (iload 11)) 
                                      (56 (aload_0)) 
                                      (57 (getfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int))) 
                                      (60 (if_icmpge 69))  ;;to TAG_2
                                      (63 (aload_0)) 
                                      (64 (getfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int))) 
                                      (67 (istore 11)) 
                                      (69 (aload_0)) ;;at TAG_2
                                      (70 (iload_2)) 
                                      (71 (iload 4)) 
                                      (73 (invokespecial (methodCP "addWithoutOverflow" "java.awt.image.CropImageFilter" (int int) int))) 
                                      (76 (istore 12)) 
                                      (78 (iload 12)) 
                                      (80 (aload_0)) 
                                      (81 (getfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int))) 
                                      (84 (aload_0)) 
                                      (85 (getfield (fieldCP "cropH" "java.awt.image.CropImageFilter" int))) 
                                      (88 (iadd)) 
                                      (89 (if_icmple 103)) ;;to TAG_3
                                      (92 (aload_0)) 
                                      (93 (getfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int))) 
                                      (96 (aload_0)) 
                                      (97 (getfield (fieldCP "cropH" "java.awt.image.CropImageFilter" int))) 
                                      (100 (iadd)) 
                                      (101 (istore 12)) 
                                      (103 (iload 9)) ;;at TAG_3
                                      (105 (iload 10)) 
                                      (107 (if_icmpge 117)) ;;to TAG_4
                                      (110 (iload 11)) 
                                      (112 (iload 12)) 
                                      (114 (if_icmplt 118)) ;;to TAG_5
                                      (117 (return)) ;;at TAG_4
                                      (118 (aload_0)) ;;at TAG_5
                                      (119 (getfield (fieldCP "consumer" "java.awt.image.CropImageFilter" (class "java.awt.image.ImageConsumer")))) 
                                      (122 (iload 9)) 
                                      (124 (aload_0)) 
                                      (125 (getfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int))) 
                                      (128 (isub)) 
                                      (129 (iload 11)) 
                                      (131 (aload_0)) 
                                      (132 (getfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int))) 
                                      (135 (isub)) 
                                      (136 (iload 10)) 
                                      (138 (iload 9)) 
                                      (140 (isub)) 
                                      (141 (iload 12)) 
                                      (143 (iload 11)) 
                                      (145 (isub)) 
                                      (146 (aload 5)) 
                                      (148 (aload 6)) 
                                      (150 (iload 7)) 
                                      (152 (iload 11)) 
                                      (154 (iload_2)) 
                                      (155 (isub)) 
                                      (156 (iload 8)) 
                                      (158 (imul)) 
                                      (159 (iadd)) 
                                      (160 (iload 9)) 
                                      (162 (iload_1)) 
                                      (163 (isub)) 
                                      (164 (iadd)) 
                                      (165 (iload 8)) 
                                      (167 (invokeinterface (methodCP "setPixels" "java.awt.image.ImageConsumer" (int int int int (class "java.awt.image.ColorModel") (array byte) int int) void) 9)) 
                                      (172 (return)) 
                                      (endofcode 173))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setPixels"
                              (parameters int int int int (class "java.awt.image.ColorModel") (array int) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 10) (max_locals . 13) (code_length . 173)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (istore 9)) 
                                      (3 (iload 9)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int))) 
                                      (9 (if_icmpge 18)) ;;to TAG_0
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int))) 
                                      (16 (istore 9)) 
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (iload_1)) 
                                      (20 (iload_3)) 
                                      (21 (invokespecial (methodCP "addWithoutOverflow" "java.awt.image.CropImageFilter" (int int) int))) 
                                      (24 (istore 10)) 
                                      (26 (iload 10)) 
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int))) 
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "cropW" "java.awt.image.CropImageFilter" int))) 
                                      (36 (iadd)) 
                                      (37 (if_icmple 51)) ;;to TAG_1
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int))) 
                                      (44 (aload_0)) 
                                      (45 (getfield (fieldCP "cropW" "java.awt.image.CropImageFilter" int))) 
                                      (48 (iadd)) 
                                      (49 (istore 10)) 
                                      (51 (iload_2)) ;;at TAG_1
                                      (52 (istore 11)) 
                                      (54 (iload 11)) 
                                      (56 (aload_0)) 
                                      (57 (getfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int))) 
                                      (60 (if_icmpge 69))  ;;to TAG_2
                                      (63 (aload_0)) 
                                      (64 (getfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int))) 
                                      (67 (istore 11)) 
                                      (69 (aload_0)) ;;at TAG_2
                                      (70 (iload_2)) 
                                      (71 (iload 4)) 
                                      (73 (invokespecial (methodCP "addWithoutOverflow" "java.awt.image.CropImageFilter" (int int) int))) 
                                      (76 (istore 12)) 
                                      (78 (iload 12)) 
                                      (80 (aload_0)) 
                                      (81 (getfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int))) 
                                      (84 (aload_0)) 
                                      (85 (getfield (fieldCP "cropH" "java.awt.image.CropImageFilter" int))) 
                                      (88 (iadd)) 
                                      (89 (if_icmple 103)) ;;to TAG_3
                                      (92 (aload_0)) 
                                      (93 (getfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int))) 
                                      (96 (aload_0)) 
                                      (97 (getfield (fieldCP "cropH" "java.awt.image.CropImageFilter" int))) 
                                      (100 (iadd)) 
                                      (101 (istore 12)) 
                                      (103 (iload 9)) ;;at TAG_3
                                      (105 (iload 10)) 
                                      (107 (if_icmpge 117)) ;;to TAG_4
                                      (110 (iload 11)) 
                                      (112 (iload 12)) 
                                      (114 (if_icmplt 118)) ;;to TAG_5
                                      (117 (return)) ;;at TAG_4
                                      (118 (aload_0)) ;;at TAG_5
                                      (119 (getfield (fieldCP "consumer" "java.awt.image.CropImageFilter" (class "java.awt.image.ImageConsumer")))) 
                                      (122 (iload 9)) 
                                      (124 (aload_0)) 
                                      (125 (getfield (fieldCP "cropX" "java.awt.image.CropImageFilter" int))) 
                                      (128 (isub)) 
                                      (129 (iload 11)) 
                                      (131 (aload_0)) 
                                      (132 (getfield (fieldCP "cropY" "java.awt.image.CropImageFilter" int))) 
                                      (135 (isub)) 
                                      (136 (iload 10)) 
                                      (138 (iload 9)) 
                                      (140 (isub)) 
                                      (141 (iload 12)) 
                                      (143 (iload 11)) 
                                      (145 (isub)) 
                                      (146 (aload 5)) 
                                      (148 (aload 6)) 
                                      (150 (iload 7)) 
                                      (152 (iload 11)) 
                                      (154 (iload_2)) 
                                      (155 (isub)) 
                                      (156 (iload 8)) 
                                      (158 (imul)) 
                                      (159 (iadd)) 
                                      (160 (iload 9)) 
                                      (162 (iload_1)) 
                                      (163 (isub)) 
                                      (164 (iadd)) 
                                      (165 (iload 8)) 
                                      (167 (invokeinterface (methodCP "setPixels" "java.awt.image.ImageConsumer" (int int int int (class "java.awt.image.ColorModel") (array int) int int) void) 9)) 
                                      (172 (return)) 
                                      (endofcode 173))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addWithoutOverflow"
                              (parameters int int)
                              (returntype . int)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 39)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (iload_2)) 
                                      (2 (iadd)) 
                                      (3 (istore_3)) 
                                      (4 (iload_1)) 
                                      (5 (ifle 22))  ;;to TAG_0
                                      (8 (iload_2)) 
                                      (9 (ifle 22))  ;;to TAG_0
                                      (12 (iload_3)) 
                                      (13 (ifge 22))  ;;to TAG_0
                                      (16 (ldc 1)) ;;INT:: "2147483647"
                                      (18 (istore_3)) 
                                      (19 (goto 37)) ;;to TAG_1
                                      (22 (iload_1)) ;;at TAG_0
                                      (23 (ifge 37)) ;;to TAG_1
                                      (26 (iload_2)) 
                                      (27 (ifge 37)) ;;to TAG_1
                                      (30 (iload_3)) 
                                      (31 (ifle 37)) ;;to TAG_1
                                      (34 (ldc 2)) ;;INT:: "-2147483648"
                                      (36 (istore_3)) 
                                      (37 (iload_3)) ;;at TAG_1
                                      (38 (ireturn)) 
                                      (endofcode 39))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *CropImageFilter-class-table*
  (make-static-class-decls 
   *java.awt.image.CropImageFilter*))

(defconst *package-name-map* 
  ("java.awt.image.CropImageFilter" . "java.awt.image"))
