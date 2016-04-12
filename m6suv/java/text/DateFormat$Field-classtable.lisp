; DateFormat$Field-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.text.DateFormat$Field*
 (make-class-def
      '(class "java.text.DateFormat$Field"
            "java.text.Format$Field"
            (constant_pool
                        (LONG 7441350119349544720)
                        (STRING  "Unknown Calendar constant ")
                        (STRING  "subclass didn\nt correctly implement readResolve")
                        (STRING  "unknown attribute name")
                        (STRING  "era")
                        (STRING  "year")
                        (STRING  "month")
                        (STRING  "day of month")
                        (STRING  "hour of day 1")
                        (STRING  "hour of day")
                        (STRING  "minute")
                        (STRING  "second")
                        (STRING  "millisecond")
                        (STRING  "day of week")
                        (STRING  "day of year")
                        (STRING  "day of week in month")
                        (STRING  "week of year")
                        (STRING  "week of month")
                        (STRING  "am pm")
                        (STRING  "hour 1")
                        (STRING  "hour")
                        (STRING  "time zone"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "instanceMap" (class "java.util.Map") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "calendarToFieldMapping" (array (class "java.text.DateFormat$Field")) (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "calendarField" int (accessflags  *class*  *private* ) -1)
                        (field "ERA" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "YEAR" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "MONTH" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "DAY_OF_MONTH" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "HOUR_OF_DAY1" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "HOUR_OF_DAY0" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "MINUTE" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "SECOND" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "MILLISECOND" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "DAY_OF_WEEK" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "DAY_OF_YEAR" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "DAY_OF_WEEK_IN_MONTH" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "WEEK_OF_YEAR" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "WEEK_OF_MONTH" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "AM_PM" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "HOUR1" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "HOUR0" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "TIME_ZONE" (class "java.text.DateFormat$Field") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "ofCalendarField"
                              (parameters int)
                              (returntype . (class "java.text.DateFormat$Field"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 45)
                                   (parsedcode
                                      (0 (iload_0)) 
                                      (1 (iflt 12))  ;;to TAG_0
                                      (4 (iload_0)) 
                                      (5 (getstatic (fieldCP "calendarToFieldMapping" "java.text.DateFormat$Field" (array (class "java.text.DateFormat$Field"))))) 
                                      (8 (arraylength)) 
                                      (9 (if_icmplt 39)) ;;to TAG_1
                                      (12 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (new (class "java.lang.StringBuilder"))) 
                                      (19 (dup)) 
                                      (20 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (23 (ldc 1)) ;;STRING:: "Unknown Calendar constant "
                                      (25 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (28 (iload_0)) 
                                      (29 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (32 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (35 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (38 (athrow)) 
                                      (39 (getstatic (fieldCP "calendarToFieldMapping" "java.text.DateFormat$Field" (array (class "java.text.DateFormat$Field"))))) ;;at TAG_1
                                      (42 (iload_0)) 
                                      (43 (aaload)) 
                                      (44 (areturn)) 
                                      (endofcode 45))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 42)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "<init>" "java.text.Format$Field" ((class "java.lang.String")) void))) 
                                      (5 (aload_0)) 
                                      (6 (iload_2)) 
                                      (7 (putfield (fieldCP "calendarField" "java.text.DateFormat$Field" int))) 
                                      (10 (aload_0)) 
                                      (11 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (14 (ldc_w )) 
                                      (17 (if_acmpne 41))  ;;to TAG_0
                                      (20 (getstatic (fieldCP "instanceMap" "java.text.DateFormat$Field" (class "java.util.Map")))) 
                                      (23 (aload_1)) 
                                      (24 (aload_0)) 
                                      (25 (invokeinterface (methodCP "put" "java.util.Map" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (30 (pop)) 
                                      (31 (iload_2)) 
                                      (32 (iflt 41))  ;;to TAG_0
                                      (35 (getstatic (fieldCP "calendarToFieldMapping" "java.text.DateFormat$Field" (array (class "java.text.DateFormat$Field"))))) 
                                      (38 (iload_2)) 
                                      (39 (aload_0)) 
                                      (40 (aastore)) 
                                      (41 (return)) ;;at TAG_0
                                      (endofcode 42))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCalendarField"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "calendarField" "java.text.DateFormat$Field" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readResolve"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 49)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (4 (ldc_w )) 
                                      (7 (if_acmpeq 20))  ;;to TAG_0
                                      (10 (new (class "java.io.InvalidObjectException"))) 
                                      (13 (dup)) 
                                      (14 (ldc 2)) ;;STRING:: "subclass didn\nt correctly implement readResolve"
                                      (16 (invokespecial (methodCP "<init>" "java.io.InvalidObjectException" ((class "java.lang.String")) void))) 
                                      (19 (athrow)) 
                                      (20 (getstatic (fieldCP "instanceMap" "java.text.DateFormat$Field" (class "java.util.Map")))) ;;at TAG_0
                                      (23 (aload_0)) 
                                      (24 (invokevirtual (methodCP "getName" "java.text.DateFormat$Field" () (class "java.lang.String")))) 
                                      (27 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (32 (astore_1)) 
                                      (33 (aload_1)) 
                                      (34 (ifnull 39)) ;;to TAG_1
                                      (37 (aload_1)) 
                                      (38 (areturn)) 
                                      (39 (new (class "java.io.InvalidObjectException"))) ;;at TAG_1
                                      (42 (dup)) 
                                      (43 (ldc 3)) ;;STRING:: "unknown attribute name"
                                      (45 (invokespecial (methodCP "<init>" "java.io.InvalidObjectException" ((class "java.lang.String")) void))) 
                                      (48 (athrow)) 
                                      (endofcode 49))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 264)
                                   (parsedcode
                                      (0 (new (class "java.util.HashMap")))
                                      (3 (dup))
                                      (4 (bipush 18))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.HashMap" (int) void)))
                                      (9 (putstatic (fieldCP "instanceMap" "java.text.DateFormat$Field" (class "java.util.Map"))))
                                      (12 (bipush 17))
                                      (14 (anewarray (class "java.text.DateFormat$Field")))
                                      (17 (putstatic (fieldCP "calendarToFieldMapping" "java.text.DateFormat$Field" (array (class "java.text.DateFormat$Field")))))
                                      (20 (new (class "java.text.DateFormat$Field")))
                                      (23 (dup))
                                      (24 (ldc 4))        ;;STRING:: "era"
                                      (26 (iconst_0))
                                      (27 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (30 (putstatic (fieldCP "ERA" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (33 (new (class "java.text.DateFormat$Field")))
                                      (36 (dup))
                                      (37 (ldc 5))        ;;STRING:: "year"
                                      (39 (iconst_1))
                                      (40 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (43 (putstatic (fieldCP "YEAR" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (46 (new (class "java.text.DateFormat$Field")))
                                      (49 (dup))
                                      (50 (ldc 6))        ;;STRING:: "month"
                                      (52 (iconst_2))
                                      (53 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (56 (putstatic (fieldCP "MONTH" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (59 (new (class "java.text.DateFormat$Field")))
                                      (62 (dup))
                                      (63 (ldc 7))        ;;STRING:: "day of month"
                                      (65 (iconst_5))
                                      (66 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (69 (putstatic (fieldCP "DAY_OF_MONTH" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (72 (new (class "java.text.DateFormat$Field")))
                                      (75 (dup))
                                      (76 (ldc 8))        ;;STRING:: "hour of day 1"
                                      (78 (iconst_m1))
                                      (79 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (82 (putstatic (fieldCP "HOUR_OF_DAY1" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (85 (new (class "java.text.DateFormat$Field")))
                                      (88 (dup))
                                      (89 (ldc 9))        ;;STRING:: "hour of day"
                                      (91 (bipush 11))
                                      (93 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (96 (putstatic (fieldCP "HOUR_OF_DAY0" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (99 (new (class "java.text.DateFormat$Field")))
                                      (102 (dup))
                                      (103 (ldc 10))      ;;STRING:: "minute"
                                      (105 (bipush 12))
                                      (107 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (110 (putstatic (fieldCP "MINUTE" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (113 (new (class "java.text.DateFormat$Field")))
                                      (116 (dup))
                                      (117 (ldc 11))      ;;STRING:: "second"
                                      (119 (bipush 13))
                                      (121 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (124 (putstatic (fieldCP "SECOND" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (127 (new (class "java.text.DateFormat$Field")))
                                      (130 (dup))
                                      (131 (ldc 12))      ;;STRING:: "millisecond"
                                      (133 (bipush 14))
                                      (135 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (138 (putstatic (fieldCP "MILLISECOND" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (141 (new (class "java.text.DateFormat$Field")))
                                      (144 (dup))
                                      (145 (ldc 13))      ;;STRING:: "day of week"
                                      (147 (bipush 7))
                                      (149 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (152 (putstatic (fieldCP "DAY_OF_WEEK" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (155 (new (class "java.text.DateFormat$Field")))
                                      (158 (dup))
                                      (159 (ldc 14))      ;;STRING:: "day of year"
                                      (161 (bipush 6))
                                      (163 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (166 (putstatic (fieldCP "DAY_OF_YEAR" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (169 (new (class "java.text.DateFormat$Field")))
                                      (172 (dup))
                                      (173 (ldc 15))      ;;STRING:: "day of week in month"
                                      (175 (bipush 8))
                                      (177 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (180 (putstatic (fieldCP "DAY_OF_WEEK_IN_MONTH" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (183 (new (class "java.text.DateFormat$Field")))
                                      (186 (dup))
                                      (187 (ldc 16))      ;;STRING:: "week of year"
                                      (189 (iconst_3))
                                      (190 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (193 (putstatic (fieldCP "WEEK_OF_YEAR" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (196 (new (class "java.text.DateFormat$Field")))
                                      (199 (dup))
                                      (200 (ldc 17))      ;;STRING:: "week of month"
                                      (202 (iconst_4))
                                      (203 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (206 (putstatic (fieldCP "WEEK_OF_MONTH" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (209 (new (class "java.text.DateFormat$Field")))
                                      (212 (dup))
                                      (213 (ldc 18))      ;;STRING:: "am pm"
                                      (215 (bipush 9))
                                      (217 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (220 (putstatic (fieldCP "AM_PM" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (223 (new (class "java.text.DateFormat$Field")))
                                      (226 (dup))
                                      (227 (ldc 19))      ;;STRING:: "hour 1"
                                      (229 (iconst_m1))
                                      (230 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (233 (putstatic (fieldCP "HOUR1" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (236 (new (class "java.text.DateFormat$Field")))
                                      (239 (dup))
                                      (240 (ldc 20))      ;;STRING:: "hour"
                                      (242 (bipush 10))
                                      (244 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (247 (putstatic (fieldCP "HOUR0" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (250 (new (class "java.text.DateFormat$Field")))
                                      (253 (dup))
                                      (254 (ldc 21))      ;;STRING:: "time zone"
                                      (256 (iconst_m1))
                                      (257 (invokespecial
					(methodCP "<init>" "java.text.DateFormat$Field" ((class "java.lang.String") int) void)))
                                      (260 (putstatic (fieldCP "TIME_ZONE" "java.text.DateFormat$Field" (class "java.text.DateFormat$Field"))))
                                      (263 (return))
                                      (endofcode 264))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *DateFormat$Field-class-table*
  (make-static-class-decls 
   *java.text.DateFormat$Field*))

(defconst *package-name-map* 
  ("java.text.DateFormat$Field" . "java.text"))

