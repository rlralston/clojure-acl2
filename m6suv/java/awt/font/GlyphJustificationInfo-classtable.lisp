; GlyphJustificationInfo-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.font.GlyphJustificationInfo*
 (make-class-def
      '(class "java.awt.font.GlyphJustificationInfo"
            "java.lang.Object"
            (constant_pool
                        (INT 0)
                        (INT 1)
                        (INT 2)
                        (INT 3)
                        (STRING  "weight is negative")
                        (STRING  "Invalid grow priority")
                        (STRING  "growLeftLimit is negative")
                        (STRING  "growRightLimit is negative")
                        (STRING  "Invalid shrink priority")
                        (STRING  "shrinkLeftLimit is negative")
                        (STRING  "shrinkRightLimit is negative"))
            (fields
                        (field "PRIORITY_KASHIDA" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "PRIORITY_WHITESPACE" int (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "PRIORITY_INTERCHAR" int (accessflags  *class*  *final*  *public*  *static* ) 2)
                        (field "PRIORITY_NONE" int (accessflags  *class*  *final*  *public*  *static* ) 3)
                        (field "weight" float (accessflags  *class*  *final*  *public* ) -1)
                        (field "growPriority" int (accessflags  *class*  *final*  *public* ) -1)
                        (field "growAbsorb" boolean (accessflags  *class*  *final*  *public* ) -1)
                        (field "growLeftLimit" float (accessflags  *class*  *final*  *public* ) -1)
                        (field "growRightLimit" float (accessflags  *class*  *final*  *public* ) -1)
                        (field "shrinkPriority" int (accessflags  *class*  *final*  *public* ) -1)
                        (field "shrinkAbsorb" boolean (accessflags  *class*  *final*  *public* ) -1)
                        (field "shrinkLeftLimit" float (accessflags  *class*  *final*  *public* ) -1)
                        (field "shrinkRightLimit" float (accessflags  *class*  *final*  *public* ) -1))
            (methods
                        (method "<init>"
                              (parameters float boolean int float float boolean int float float)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 10) (code_length . 175)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (fload_1)) 
                                      (5 (fconst_0)) 
                                      (6 (fcmpg)) 
                                      (7 (ifge 20)) ;;to TAG_0
                                      (10 (new (class "java.lang.IllegalArgumentException"))) 
                                      (13 (dup)) 
                                      (14 (ldc 4)) ;;STRING:: "weight is negative"
                                      (16 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (19 (athrow)) 
                                      (20 (iload_3)) ;;at TAG_0
                                      (21 (invokestatic (methodCP "priorityIsValid" "java.awt.font.GlyphJustificationInfo" (int) boolean))) 
                                      (24 (ifne 37)) ;;to TAG_1
                                      (27 (new (class "java.lang.IllegalArgumentException"))) 
                                      (30 (dup)) 
                                      (31 (ldc 5)) ;;STRING:: "Invalid grow priority"
                                      (33 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (36 (athrow)) 
                                      (37 (fload 4)) ;;at TAG_1
                                      (39 (fconst_0)) 
                                      (40 (fcmpg)) 
                                      (41 (ifge 54))  ;;to TAG_2
                                      (44 (new (class "java.lang.IllegalArgumentException"))) 
                                      (47 (dup)) 
                                      (48 (ldc 6)) ;;STRING:: "growLeftLimit is negative"
                                      (50 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (53 (athrow)) 
                                      (54 (fload 5)) ;;at TAG_2
                                      (56 (fconst_0)) 
                                      (57 (fcmpg)) 
                                      (58 (ifge 71)) ;;to TAG_3
                                      (61 (new (class "java.lang.IllegalArgumentException"))) 
                                      (64 (dup)) 
                                      (65 (ldc 7)) ;;STRING:: "growRightLimit is negative"
                                      (67 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (70 (athrow)) 
                                      (71 (iload 7)) ;;at TAG_3
                                      (73 (invokestatic (methodCP "priorityIsValid" "java.awt.font.GlyphJustificationInfo" (int) boolean))) 
                                      (76 (ifne 89)) ;;to TAG_4
                                      (79 (new (class "java.lang.IllegalArgumentException"))) 
                                      (82 (dup)) 
                                      (83 (ldc 8)) ;;STRING:: "Invalid shrink priority"
                                      (85 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (88 (athrow)) 
                                      (89 (fload 8)) ;;at TAG_4
                                      (91 (fconst_0)) 
                                      (92 (fcmpg)) 
                                      (93 (ifge 106)) ;;to TAG_5
                                      (96 (new (class "java.lang.IllegalArgumentException"))) 
                                      (99 (dup)) 
                                      (100 (ldc 9)) ;;STRING:: "shrinkLeftLimit is negative"
                                      (102 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (105 (athrow)) 
                                      (106 (fload 9)) ;;at TAG_5
                                      (108 (fconst_0)) 
                                      (109 (fcmpg)) 
                                      (110 (ifge 123)) ;;to TAG_6
                                      (113 (new (class "java.lang.IllegalArgumentException"))) 
                                      (116 (dup)) 
                                      (117 (ldc 10)) ;;STRING:: "shrinkRightLimit is negative"
                                      (119 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (122 (athrow)) 
                                      (123 (aload_0)) ;;at TAG_6
                                      (124 (fload_1)) 
                                      (125 (putfield (fieldCP "weight" "java.awt.font.GlyphJustificationInfo" float))) 
                                      (128 (aload_0)) 
                                      (129 (iload_2)) 
                                      (130 (putfield (fieldCP "growAbsorb" "java.awt.font.GlyphJustificationInfo" boolean))) 
                                      (133 (aload_0)) 
                                      (134 (iload_3)) 
                                      (135 (putfield (fieldCP "growPriority" "java.awt.font.GlyphJustificationInfo" int))) 
                                      (138 (aload_0)) 
                                      (139 (fload 4)) 
                                      (141 (putfield (fieldCP "growLeftLimit" "java.awt.font.GlyphJustificationInfo" float))) 
                                      (144 (aload_0)) 
                                      (145 (fload 5)) 
                                      (147 (putfield (fieldCP "growRightLimit" "java.awt.font.GlyphJustificationInfo" float))) 
                                      (150 (aload_0)) 
                                      (151 (iload 6)) 
                                      (153 (putfield (fieldCP "shrinkAbsorb" "java.awt.font.GlyphJustificationInfo" boolean))) 
                                      (156 (aload_0)) 
                                      (157 (iload 7)) 
                                      (159 (putfield (fieldCP "shrinkPriority" "java.awt.font.GlyphJustificationInfo" int))) 
                                      (162 (aload_0)) 
                                      (163 (fload 8)) 
                                      (165 (putfield (fieldCP "shrinkLeftLimit" "java.awt.font.GlyphJustificationInfo" float))) 
                                      (168 (aload_0)) 
                                      (169 (fload 9)) 
                                      (171 (putfield (fieldCP "shrinkRightLimit" "java.awt.font.GlyphJustificationInfo" float))) 
                                      (174 (return)) 
                                      (endofcode 175))
                                   (Exceptions )
                                   (StackMap )))
                        (method "priorityIsValid"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (iload_0)) 
                                      (1 (iflt 13))  ;;to TAG_0
                                      (4 (iload_0)) 
                                      (5 (iconst_3)) 
                                      (6 (if_icmpgt 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (ireturn)) ;;at TAG_1
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *GlyphJustificationInfo-class-table*
  (make-static-class-decls 
   *java.awt.font.GlyphJustificationInfo*))

(defconst *package-name-map* 
  ("java.awt.font.GlyphJustificationInfo" . "java.awt.font"))

