; string$replace_by-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.string$replace_by*
 (make-class-def
      '(class "clojure.string$replace_by"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "re-matcher")
                        (STRING  "re-groups"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "re-matcher"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.string$replace_by" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "re-groups"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.string$replace_by" (class "clojure.lang.Var"))))
                                      (26 (return))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 7) (code_length . 175)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.string$replace_by" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_2)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_2)) 
                                      (12 (aload_1)) 
                                      (13 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (18 (astore 4)) 
                                      (20 (aload 4)) 
                                      (22 (checkcast (class "java.util.regex.Matcher"))) 
                                      (25 (invokevirtual (methodCP "find" "java.util.regex.Matcher" () boolean))) 
                                      (28 (ifeq 171)) ;;to TAG_0
                                      (31 (new (class "java.lang.StringBuffer"))) 
                                      (34 (dup)) 
                                      (35 (aload_1)) 
                                      (36 (aconst_null)) 
                                      (37 (astore_1)) 
                                      (38 (checkcast (class "java.lang.CharSequence"))) 
                                      (41 (invokeinterface (methodCP "length" "java.lang.CharSequence" () int) 1)) 
                                      (46 (invokespecial (methodCP "<init>" "java.lang.StringBuffer" (int) void))) 
                                      (49 (astore 5)) 
                                      (51 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (54 (astore 6)) 
                                      (56 (aload 6)) ;;at TAG_5
                                      (58 (dup)) 
                                      (59 (ifnull 144)) ;;to TAG_1
                                      (62 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (65 (if_acmpeq 145))  ;;to TAG_2
                                      (68 (aload 4)) 
                                      (70 (checkcast (class "java.util.regex.Matcher"))) 
                                      (73 (aload 5)) 
                                      (75 (checkcast (class "java.lang.StringBuffer"))) 
                                      (78 (aload_3)) 
                                      (79 (checkcast (class "clojure.lang.IFn"))) 
                                      (82 (getstatic (fieldCP "const__1" "clojure.string$replace_by" (class "clojure.lang.Var")))) 
                                      (85 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (88 (checkcast (class "clojure.lang.IFn"))) 
                                      (91 (aload 4)) 
                                      (93 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (98 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (103 (checkcast (class "java.lang.String"))) 
                                      (106 (invokestatic (methodCP "quoteReplacement" "java.util.regex.Matcher" ((class "java.lang.String")) (class "java.lang.String")))) 
                                      (109 (checkcast (class "java.lang.String"))) 
                                      (112 (invokevirtual (methodCP "appendReplacement" "java.util.regex.Matcher" ((class "java.lang.StringBuffer") (class "java.lang.String")) (class "java.util.regex.Matcher")))) 
                                      (115 (pop)) 
                                      (116 (aload 4)) 
                                      (118 (checkcast (class "java.util.regex.Matcher"))) 
                                      (121 (invokevirtual (methodCP "find" "java.util.regex.Matcher" () boolean))) 
                                      (124 (ifeq 133)) ;;to TAG_3
                                      (127 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (130 (goto 136)) ;;to TAG_4
                                      (133 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_3
                                      (136 (astore 6)) ;;at TAG_4
                                      (138 (goto 56)) ;;to TAG_5
                                      (141 (goto 167)) ;;to TAG_6
                                      (144 (pop)) ;;at TAG_1
                                      (145 (aload 4)) ;;at TAG_2
                                      (147 (checkcast (class "java.util.regex.Matcher"))) 
                                      (150 (aload 5)) 
                                      (152 (checkcast (class "java.lang.StringBuffer"))) 
                                      (155 (invokevirtual (methodCP "appendTail" "java.util.regex.Matcher" ((class "java.lang.StringBuffer")) (class "java.lang.StringBuffer")))) 
                                      (158 (pop)) 
                                      (159 (aload 5)) 
                                      (161 (checkcast (class "java.lang.StringBuffer"))) 
                                      (164 (invokevirtual (methodCP "toString" "java.lang.StringBuffer" () (class "java.lang.String")))) 
                                      (167 (goto 174)) ;;to TAG_7;;at TAG_6
                                      (170 (pop)) 
                                      (171 (aload_1)) ;;at TAG_0
                                      (172 (aconst_null)) 
                                      (173 (astore_1)) 
                                      (174 (areturn)) ;;at TAG_7
                                      (endofcode 175))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *string$replace_by-class-table*
  (make-static-class-decls 
   *clojure.string$replace_by*))

(defconst *package-name-map* 
  ("clojure.string$replace_by" . "clojure"))

