; java_lang_Class_PersistenceDelegate-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.java_lang_Class_PersistenceDelegate*
 (make-class-def
      '(class "java.beans.java_lang_Class_PersistenceDelegate"
            "java.beans.PersistenceDelegate"
            (constant_pool
                        (STRING  "TYPE")
                        (STRING  "Unknown primitive type: ")
                        (STRING  "get")
                        (STRING  "")
                        (STRING  "getClass")
                        (STRING  "forName"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.beans.PersistenceDelegate" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "mutatesTo"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_2))
                                      (2 (invokevirtual
					(methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean)))
                                      (5 (ireturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "instantiate"
                              (parameters (class "java.lang.Object") (class "java.beans.Encoder"))
                              (returntype . (class "java.beans.Expression"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 9) (max_locals . 6) (code_length . 167)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.lang.Class"))) 
                                      (4 (astore_3)) 
                                      (5 (aload_3)) 
                                      (6 (invokevirtual (methodCP "isPrimitive" "java.lang.Class" () boolean))) 
                                      (9 (ifeq 80)) ;;to TAG_0
                                      (12 (aconst_null)) 
                                      (13 (astore 4)) 
                                      (15 (aload_3)) ;;at TAG_4
                                      (16 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (19 (invokestatic (methodCP "getType" "com.sun.beans.finder.PrimitiveWrapperMap" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (22 (ldc 0)) ;;STRING:: "TYPE"
                                      (24 (invokevirtual (methodCP "getDeclaredField" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.reflect.Field")))) 
                                      (27 (astore 4)) 
                                      (29 (goto 59)) ;;to TAG_1;;at TAG_5
                                      (32 (astore 5)) ;;at TAG_6
                                      (34 (getstatic (fieldCP "err" "java.lang.System" (class "java.io.PrintStream")))) 
                                      (37 (new (class "java.lang.StringBuilder"))) 
                                      (40 (dup)) 
                                      (41 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (44 (ldc 1)) ;;STRING:: "Unknown primitive type: "
                                      (46 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (49 (aload_3)) 
                                      (50 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (53 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (56 (invokevirtual (methodCP "println" "java.io.PrintStream" ((class "java.lang.String")) void))) 
                                      (59 (new (class "java.beans.Expression"))) ;;at TAG_1
                                      (62 (dup)) 
                                      (63 (aload_1)) 
                                      (64 (aload 4)) 
                                      (66 (ldc 2)) ;;STRING:: "get"
                                      (68 (iconst_1)) 
                                      (69 (anewarray (class "java.lang.Object"))) 
                                      (72 (dup)) 
                                      (73 (iconst_0)) 
                                      (74 (aconst_null)) 
                                      (75 (aastore)) 
                                      (76 (invokespecial (methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void))) 
                                      (79 (areturn)) 
                                      (80 (aload_1)) ;;at TAG_0
                                      (81 (ldc_w )) 
                                      (84 (if_acmpne 104))  ;;to TAG_2
                                      (87 (new (class "java.beans.Expression"))) 
                                      (90 (dup)) 
                                      (91 (aload_1)) 
                                      (92 (ldc 3)) ;;STRING:: ""
                                      (94 (ldc 4)) ;;STRING:: "getClass"
                                      (96 (iconst_0)) 
                                      (97 (anewarray (class "java.lang.Object"))) 
                                      (100 (invokespecial (methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void))) 
                                      (103 (areturn)) 
                                      (104 (aload_1)) ;;at TAG_2
                                      (105 (ldc_w )) 
                                      (108 (if_acmpne 129)) ;;to TAG_3
                                      (111 (new (class "java.beans.Expression"))) 
                                      (114 (dup)) 
                                      (115 (aload_1)) 
                                      (116 (ldc_w )) 
                                      (119 (ldc 4)) ;;STRING:: "getClass"
                                      (121 (iconst_0)) 
                                      (122 (anewarray (class "java.lang.Object"))) 
                                      (125 (invokespecial (methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void))) 
                                      (128 (areturn)) 
                                      (129 (new (class "java.beans.Expression"))) ;;at TAG_3
                                      (132 (dup)) 
                                      (133 (aload_1)) 
                                      (134 (ldc_w )) 
                                      (137 (ldc 5)) ;;STRING:: "forName"
                                      (139 (iconst_1)) 
                                      (140 (anewarray (class "java.lang.Object"))) 
                                      (143 (dup)) 
                                      (144 (iconst_0)) 
                                      (145 (aload_3)) 
                                      (146 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (149 (aastore)) 
                                      (150 (invokespecial (methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void))) 
                                      (153 (astore 4)) 
                                      (155 (aload 4)) 
                                      (157 (aload_3)) 
                                      (158 (invokevirtual (methodCP "getClassLoader" "java.lang.Class" () (class "java.lang.ClassLoader")))) 
                                      (161 (putfield (fieldCP "loader" "java.beans.Expression" (class "java.lang.ClassLoader")))) 
                                      (164 (aload 4)) 
                                      (166 (areturn)) 
                                      (endofcode 167))
                                   (Exceptions 
                                     (handler 15 29  32 (class "java.lang.NoSuchFieldException")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *java_lang_Class_PersistenceDelegate-class-table*
  (make-static-class-decls 
   *java.beans.java_lang_Class_PersistenceDelegate*))

(defconst *package-name-map* 
  ("java.beans.java_lang_Class_PersistenceDelegate" . "java.beans"))

