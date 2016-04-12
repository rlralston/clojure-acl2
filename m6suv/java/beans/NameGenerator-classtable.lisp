; NameGenerator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.NameGenerator*
 (make-class-def
      '(class "java.beans.NameGenerator"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Array")
                        (STRING  "null"))
            (fields
                        (field "valueToName" (class "java.util.Map") (accessflags  *class*  *private* ) -1)
                        (field "nameToCount" (class "java.util.Map") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "java.util.IdentityHashMap")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "java.util.IdentityHashMap" () void)))
                                      (12 (putfield (fieldCP "valueToName" "java.beans.NameGenerator" (class "java.util.Map"))))
                                      (15 (aload_0))
                                      (16 (new (class "java.util.HashMap")))
                                      (19 (dup))
                                      (20 (invokespecial
					(methodCP "<init>" "java.util.HashMap" () void)))
                                      (23 (putfield (fieldCP "nameToCount" "java.beans.NameGenerator" (class "java.util.Map"))))
                                      (26 (return))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "valueToName" "java.beans.NameGenerator" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "clear" "java.util.Map" () void) 1))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "nameToCount" "java.beans.NameGenerator" (class "java.util.Map"))))
                                      (13 (invokeinterface
					(methodCP "clear" "java.util.Map" () void) 1))
                                      (18 (return))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "unqualifiedClassName"
                              (parameters (class "java.lang.Class"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 51)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isArray" "java.lang.Class" () boolean))) 
                                      (4 (ifeq 33))  ;;to TAG_0
                                      (7 (new (class "java.lang.StringBuilder"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (14 (aload_0)) 
                                      (15 (invokevirtual (methodCP "getComponentType" "java.lang.Class" () (class "java.lang.Class")))) 
                                      (18 (invokestatic (methodCP "unqualifiedClassName" "java.beans.NameGenerator" ((class "java.lang.Class")) (class "java.lang.String")))) 
                                      (21 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (24 (ldc 0)) ;;STRING:: "Array"
                                      (26 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (29 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (32 (areturn)) 
                                      (33 (aload_0)) ;;at TAG_0
                                      (34 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (37 (astore_1)) 
                                      (38 (aload_1)) 
                                      (39 (aload_1)) 
                                      (40 (bipush 46)) 
                                      (42 (invokevirtual (methodCP "lastIndexOf" "java.lang.String" (int) int))) 
                                      (45 (iconst_1)) 
                                      (46 (iadd)) 
                                      (47 (invokevirtual (methodCP "substring" "java.lang.String" (int) (class "java.lang.String")))) 
                                      (50 (areturn)) 
                                      (endofcode 51))
                                   (Exceptions )
                                   (StackMap )))
                        (method "capitalize"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 47)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (ifnull 11))  ;;to TAG_0
                                      (4 (aload_0)) 
                                      (5 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (8 (ifne 13)) ;;to TAG_1
                                      (11 (aload_0)) ;;at TAG_0
                                      (12 (areturn)) 
                                      (13 (new (class "java.lang.StringBuilder"))) ;;at TAG_1
                                      (16 (dup)) 
                                      (17 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (20 (aload_0)) 
                                      (21 (iconst_0)) 
                                      (22 (iconst_1)) 
                                      (23 (invokevirtual (methodCP "substring" "java.lang.String" (int int) (class "java.lang.String")))) 
                                      (26 (getstatic (fieldCP "ENGLISH" "java.util.Locale" (class "java.util.Locale")))) 
                                      (29 (invokevirtual (methodCP "toUpperCase" "java.lang.String" ((class "java.util.Locale")) (class "java.lang.String")))) 
                                      (32 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (35 (aload_0)) 
                                      (36 (iconst_1)) 
                                      (37 (invokevirtual (methodCP "substring" "java.lang.String" (int) (class "java.lang.String")))) 
                                      (40 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (43 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (46 (areturn)) 
                                      (endofcode 47))
                                   (Exceptions )
                                   (StackMap )))
                        (method "instanceName"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 7) (code_length . 143)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 7)) ;;to TAG_0
                                      (4 (ldc 1)) ;;STRING:: "null"
                                      (6 (areturn)) 
                                      (7 (aload_1)) ;;at TAG_0
                                      (8 (instanceof (class "java.lang.Class"))) 
                                      (11 (ifeq 22)) ;;to TAG_1
                                      (14 (aload_1)) 
                                      (15 (checkcast (class "java.lang.Class"))) 
                                      (18 (invokestatic (methodCP "unqualifiedClassName" "java.beans.NameGenerator" ((class "java.lang.Class")) (class "java.lang.String")))) 
                                      (21 (areturn)) 
                                      (22 (aload_0)) ;;at TAG_1
                                      (23 (getfield (fieldCP "valueToName" "java.beans.NameGenerator" (class "java.util.Map")))) 
                                      (26 (aload_1)) 
                                      (27 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (32 (checkcast (class "java.lang.String"))) 
                                      (35 (astore_2)) 
                                      (36 (aload_2)) 
                                      (37 (ifnull 42))  ;;to TAG_2
                                      (40 (aload_2)) 
                                      (41 (areturn)) 
                                      (42 (aload_1)) ;;at TAG_2
                                      (43 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (46 (astore_3)) 
                                      (47 (aload_3)) 
                                      (48 (invokestatic (methodCP "unqualifiedClassName" "java.beans.NameGenerator" ((class "java.lang.Class")) (class "java.lang.String")))) 
                                      (51 (astore 4)) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "nameToCount" "java.beans.NameGenerator" (class "java.util.Map")))) 
                                      (57 (aload 4)) 
                                      (59 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (64 (astore 5)) 
                                      (66 (aload 5)) 
                                      (68 (ifnonnull 75)) ;;to TAG_3
                                      (71 (iconst_0)) 
                                      (72 (goto 85)) ;;to TAG_4
                                      (75 (aload 5)) ;;at TAG_3
                                      (77 (checkcast (class "java.lang.Integer"))) 
                                      (80 (invokevirtual (methodCP "intValue" "java.lang.Integer" () int))) 
                                      (83 (iconst_1)) 
                                      (84 (iadd)) 
                                      (85 (istore 6)) ;;at TAG_4
                                      (87 (aload_0)) 
                                      (88 (getfield (fieldCP "nameToCount" "java.beans.NameGenerator" (class "java.util.Map")))) 
                                      (91 (aload 4)) 
                                      (93 (new (class "java.lang.Integer"))) 
                                      (96 (dup)) 
                                      (97 (iload 6)) 
                                      (99 (invokespecial (methodCP "<init>" "java.lang.Integer" (int) void))) 
                                      (102 (invokeinterface (methodCP "put" "java.util.Map" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (107 (pop)) 
                                      (108 (new (class "java.lang.StringBuilder"))) 
                                      (111 (dup)) 
                                      (112 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (115 (aload 4)) 
                                      (117 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (120 (iload 6)) 
                                      (122 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (125 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (128 (astore_2)) 
                                      (129 (aload_0)) 
                                      (130 (getfield (fieldCP "valueToName" "java.beans.NameGenerator" (class "java.util.Map")))) 
                                      (133 (aload_1)) 
                                      (134 (aload_2)) 
                                      (135 (invokeinterface (methodCP "put" "java.util.Map" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (140 (pop)) 
                                      (141 (aload_2)) 
                                      (142 (areturn)) 
                                      (endofcode 143))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *NameGenerator-class-table*
  (make-static-class-decls 
   *java.beans.NameGenerator*))

(defconst *package-name-map* 
  ("java.beans.NameGenerator" . "java.beans"))

