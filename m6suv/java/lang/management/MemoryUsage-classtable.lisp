; MemoryUsage-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.management.MemoryUsage*
 (make-class-def
      '(class "java.lang.management.MemoryUsage"
            "java.lang.Object"
            (constant_pool
                        (LONG -1)
                        (STRING  "init parameter = ")
                        (STRING  " is negative but not -1.")
                        (STRING  "max parameter = ")
                        (STRING  "used parameter = ")
                        (STRING  " is negative.")
                        (STRING  "committed parameter = ")
                        (STRING  "used = ")
                        (STRING  " should be <= committed = ")
                        (STRING  "committed = ")
                        (STRING  " should be < max = ")
                        (STRING  "init = ")
                        (STRING  "(")
                        (STRING  "K) ")
                        (STRING  "max = ")
                        (STRING  "K)"))
            (fields
                        (field "init" long (accessflags  *class*  *final*  *private* ) -1)
                        (field "used" long (accessflags  *class*  *final*  *private* ) -1)
                        (field "committed" long (accessflags  *class*  *final*  *private* ) -1)
                        (field "max" long (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters long long long long)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 9) (code_length . 284)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (lload_1)) 
                                      (5 (ldc2_w 0)) ;; LONG:: "-1"
                                      (8 (lcmp)) 
                                      (9 (ifge 44)) ;;to TAG_0
                                      (12 (new (class "java.lang.IllegalArgumentException"))) 
                                      (15 (dup)) 
                                      (16 (new (class "java.lang.StringBuilder"))) 
                                      (19 (dup)) 
                                      (20 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (23 (ldc 1)) ;;STRING:: "init parameter = "
                                      (25 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (28 (lload_1)) 
                                      (29 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder")))) 
                                      (32 (ldc 2)) ;;STRING:: " is negative but not -1."
                                      (34 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (37 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (40 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (43 (athrow)) 
                                      (44 (lload 7)) ;;at TAG_0
                                      (46 (ldc2_w 0)) ;; LONG:: "-1"
                                      (49 (lcmp)) 
                                      (50 (ifge 86)) ;;to TAG_1
                                      (53 (new (class "java.lang.IllegalArgumentException"))) 
                                      (56 (dup)) 
                                      (57 (new (class "java.lang.StringBuilder"))) 
                                      (60 (dup)) 
                                      (61 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (64 (ldc 3)) ;;STRING:: "max parameter = "
                                      (66 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (69 (lload 7)) 
                                      (71 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder")))) 
                                      (74 (ldc 2)) ;;STRING:: " is negative but not -1."
                                      (76 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (79 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (82 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (85 (athrow)) 
                                      (86 (lload_3)) ;;at TAG_1
                                      (87 (lconst_0)) 
                                      (88 (lcmp)) 
                                      (89 (ifge 124))  ;;to TAG_2
                                      (92 (new (class "java.lang.IllegalArgumentException"))) 
                                      (95 (dup)) 
                                      (96 (new (class "java.lang.StringBuilder"))) 
                                      (99 (dup)) 
                                      (100 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (103 (ldc 4)) ;;STRING:: "used parameter = "
                                      (105 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (108 (lload_3)) 
                                      (109 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder")))) 
                                      (112 (ldc 5)) ;;STRING:: " is negative."
                                      (114 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (117 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (120 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (123 (athrow)) 
                                      (124 (lload 5)) ;;at TAG_2
                                      (126 (lconst_0)) 
                                      (127 (lcmp)) 
                                      (128 (ifge 164)) ;;to TAG_3
                                      (131 (new (class "java.lang.IllegalArgumentException"))) 
                                      (134 (dup)) 
                                      (135 (new (class "java.lang.StringBuilder"))) 
                                      (138 (dup)) 
                                      (139 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (142 (ldc 6)) ;;STRING:: "committed parameter = "
                                      (144 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (147 (lload 5)) 
                                      (149 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder")))) 
                                      (152 (ldc 5)) ;;STRING:: " is negative."
                                      (154 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (157 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (160 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (163 (athrow)) 
                                      (164 (lload_3)) ;;at TAG_3
                                      (165 (lload 5)) 
                                      (167 (lcmp)) 
                                      (168 (ifle 208)) ;;to TAG_4
                                      (171 (new (class "java.lang.IllegalArgumentException"))) 
                                      (174 (dup)) 
                                      (175 (new (class "java.lang.StringBuilder"))) 
                                      (178 (dup)) 
                                      (179 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (182 (ldc 7)) ;;STRING:: "used = "
                                      (184 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (187 (lload_3)) 
                                      (188 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder")))) 
                                      (191 (ldc 8)) ;;STRING:: " should be <= committed = "
                                      (193 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (196 (lload 5)) 
                                      (198 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder")))) 
                                      (201 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (204 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (207 (athrow)) 
                                      (208 (lload 7)) ;;at TAG_4
                                      (210 (lconst_0)) 
                                      (211 (lcmp)) 
                                      (212 (iflt 261)) ;;to TAG_5
                                      (215 (lload 5)) 
                                      (217 (lload 7)) 
                                      (219 (lcmp)) 
                                      (220 (ifle 261)) ;;to TAG_5
                                      (223 (new (class "java.lang.IllegalArgumentException"))) 
                                      (226 (dup)) 
                                      (227 (new (class "java.lang.StringBuilder"))) 
                                      (230 (dup)) 
                                      (231 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (234 (ldc 9)) ;;STRING:: "committed = "
                                      (236 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (239 (lload 5)) 
                                      (241 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder")))) 
                                      (244 (ldc 10)) ;;STRING:: " should be < max = "
                                      (246 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (249 (lload 7)) 
                                      (251 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder")))) 
                                      (254 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (257 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (260 (athrow)) 
                                      (261 (aload_0)) ;;at TAG_5
                                      (262 (lload_1)) 
                                      (263 (putfield (fieldCP "init" "java.lang.management.MemoryUsage" long))) 
                                      (266 (aload_0)) 
                                      (267 (lload_3)) 
                                      (268 (putfield (fieldCP "used" "java.lang.management.MemoryUsage" long))) 
                                      (271 (aload_0)) 
                                      (272 (lload 5)) 
                                      (274 (putfield (fieldCP "committed" "java.lang.management.MemoryUsage" long))) 
                                      (277 (aload_0)) 
                                      (278 (lload 7)) 
                                      (280 (putfield (fieldCP "max" "java.lang.management.MemoryUsage" long))) 
                                      (283 (return)) 
                                      (endofcode 284))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "javax.management.openmbean.CompositeData"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 41)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_1))
                                      (5 (invokestatic
					(methodCP "validateCompositeData" "sun.management.MemoryUsageCompositeData" ((class "javax.management.openmbean.CompositeData")) void)))
                                      (8 (aload_0))
                                      (9 (aload_1))
                                      (10 (invokestatic
					(methodCP "getInit" "sun.management.MemoryUsageCompositeData" ((class "javax.management.openmbean.CompositeData")) long)))
                                      (13 (putfield (fieldCP "init" "java.lang.management.MemoryUsage" long)))
                                      (16 (aload_0))
                                      (17 (aload_1))
                                      (18 (invokestatic
					(methodCP "getUsed" "sun.management.MemoryUsageCompositeData" ((class "javax.management.openmbean.CompositeData")) long)))
                                      (21 (putfield (fieldCP "used" "java.lang.management.MemoryUsage" long)))
                                      (24 (aload_0))
                                      (25 (aload_1))
                                      (26 (invokestatic
					(methodCP "getCommitted" "sun.management.MemoryUsageCompositeData" ((class "javax.management.openmbean.CompositeData")) long)))
                                      (29 (putfield (fieldCP "committed" "java.lang.management.MemoryUsage" long)))
                                      (32 (aload_0))
                                      (33 (aload_1))
                                      (34 (invokestatic
					(methodCP "getMax" "sun.management.MemoryUsageCompositeData" ((class "javax.management.openmbean.CompositeData")) long)))
                                      (37 (putfield (fieldCP "max" "java.lang.management.MemoryUsage" long)))
                                      (40 (return))
                                      (endofcode 41))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getInit"
                              (parameters )
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "init" "java.lang.management.MemoryUsage" long)))
                                      (4 (lreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getUsed"
                              (parameters )
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "used" "java.lang.management.MemoryUsage" long)))
                                      (4 (lreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCommitted"
                              (parameters )
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "committed" "java.lang.management.MemoryUsage" long)))
                                      (4 (lreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMax"
                              (parameters )
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "max" "java.lang.management.MemoryUsage" long)))
                                      (4 (lreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 201)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuffer")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuffer" () void)))
                                      (7 (astore_1))
                                      (8 (aload_1))
                                      (9 (new (class "java.lang.StringBuilder")))
                                      (12 (dup))
                                      (13 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (16 (ldc 11))       ;;STRING:: "init = "
                                      (18 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (21 (aload_0))
                                      (22 (getfield (fieldCP "init" "java.lang.management.MemoryUsage" long)))
                                      (25 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder"))))
                                      (28 (ldc 12))       ;;STRING:: "("
                                      (30 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (33 (aload_0))
                                      (34 (getfield (fieldCP "init" "java.lang.management.MemoryUsage" long)))
                                      (37 (bipush 10))
                                      (39 (lshr))
                                      (40 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder"))))
                                      (43 (ldc 13))       ;;STRING:: "K) "
                                      (45 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (48 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (51 (invokevirtual
					(methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer"))))
                                      (54 (pop))
                                      (55 (aload_1))
                                      (56 (new (class "java.lang.StringBuilder")))
                                      (59 (dup))
                                      (60 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (63 (ldc 7))        ;;STRING:: "used = "
                                      (65 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (68 (aload_0))
                                      (69 (getfield (fieldCP "used" "java.lang.management.MemoryUsage" long)))
                                      (72 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder"))))
                                      (75 (ldc 12))       ;;STRING:: "("
                                      (77 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (80 (aload_0))
                                      (81 (getfield (fieldCP "used" "java.lang.management.MemoryUsage" long)))
                                      (84 (bipush 10))
                                      (86 (lshr))
                                      (87 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder"))))
                                      (90 (ldc 13))       ;;STRING:: "K) "
                                      (92 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (95 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (98 (invokevirtual
					(methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer"))))
                                      (101 (pop))
                                      (102 (aload_1))
                                      (103 (new (class "java.lang.StringBuilder")))
                                      (106 (dup))
                                      (107 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (110 (ldc 9))       ;;STRING:: "committed = "
                                      (112 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (115 (aload_0))
                                      (116 (getfield (fieldCP "committed" "java.lang.management.MemoryUsage" long)))
                                      (119 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder"))))
                                      (122 (ldc 12))      ;;STRING:: "("
                                      (124 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (127 (aload_0))
                                      (128 (getfield (fieldCP "committed" "java.lang.management.MemoryUsage" long)))
                                      (131 (bipush 10))
                                      (133 (lshr))
                                      (134 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder"))))
                                      (137 (ldc 13))      ;;STRING:: "K) "
                                      (139 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (142 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (145 (invokevirtual
					(methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer"))))
                                      (148 (pop))
                                      (149 (aload_1))
                                      (150 (new (class "java.lang.StringBuilder")))
                                      (153 (dup))
                                      (154 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (157 (ldc 14))      ;;STRING:: "max = "
                                      (159 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (162 (aload_0))
                                      (163 (getfield (fieldCP "max" "java.lang.management.MemoryUsage" long)))
                                      (166 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder"))))
                                      (169 (ldc 12))      ;;STRING:: "("
                                      (171 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (174 (aload_0))
                                      (175 (getfield (fieldCP "max" "java.lang.management.MemoryUsage" long)))
                                      (178 (bipush 10))
                                      (180 (lshr))
                                      (181 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder"))))
                                      (184 (ldc 15))      ;;STRING:: "K)"
                                      (186 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (189 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (192 (invokevirtual
					(methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer"))))
                                      (195 (pop))
                                      (196 (aload_1))
                                      (197 (invokevirtual
					(methodCP "toString" "java.lang.StringBuffer" () (class "java.lang.String"))))
                                      (200 (areturn))
                                      (endofcode 201))
                                   (Exceptions )
                                   (StackMap )))
                        (method "from"
                              (parameters (class "javax.management.openmbean.CompositeData"))
                              (returntype . (class "java.lang.management.MemoryUsage"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (ifnonnull 6))  ;;to TAG_0
                                      (4 (aconst_null)) 
                                      (5 (areturn)) 
                                      (6 (aload_0)) ;;at TAG_0
                                      (7 (instanceof (class "sun.management.MemoryUsageCompositeData"))) 
                                      (10 (ifeq 21)) ;;to TAG_1
                                      (13 (aload_0)) 
                                      (14 (checkcast (class "sun.management.MemoryUsageCompositeData"))) 
                                      (17 (invokevirtual (methodCP "getMemoryUsage" "sun.management.MemoryUsageCompositeData" () (class "java.lang.management.MemoryUsage")))) 
                                      (20 (areturn)) 
                                      (21 (new (class "java.lang.management.MemoryUsage"))) ;;at TAG_1
                                      (24 (dup)) 
                                      (25 (aload_0)) 
                                      (26 (invokespecial (methodCP "<init>" "java.lang.management.MemoryUsage" ((class "javax.management.openmbean.CompositeData")) void))) 
                                      (29 (areturn)) 
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *MemoryUsage-class-table*
  (make-static-class-decls 
   *java.lang.management.MemoryUsage*))

(defconst *package-name-map* 
  ("java.lang.management.MemoryUsage" . "java.lang.management"))
