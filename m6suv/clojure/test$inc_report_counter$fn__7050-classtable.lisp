; test$inc_report_counter$fn__7050-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.test$inc_report_counter$fn__7050*
 (make-class-def
      '(class "clojure.test$inc_report_counter$fn__7050"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "commute")
                        (STRING  "clojure.test")
                        (STRING  "*report-counters*")
                        (STRING  "assoc")
                        (STRING  "inc"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "name" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 60)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "commute"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.test$inc_report_counter$fn__7050" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.test"
                                      (15 (ldc 3))        ;;STRING:: "*report-counters*"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.test$inc_report_counter$fn__7050" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "assoc"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.test$inc_report_counter$fn__7050" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "inc"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.test$inc_report_counter$fn__7050" (class "clojure.lang.Var"))))
                                      (52 (lconst_0))
                                      (53 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (56 (putstatic (fieldCP "const__4" "clojure.test$inc_report_counter$fn__7050" (class "java.lang.Object"))))
                                      (59 (return))
                                      (endofcode 60))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "name" "clojure.test$inc_report_counter$fn__7050" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 74)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.test$inc_report_counter$fn__7050" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.test$inc_report_counter$fn__7050" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (getstatic (fieldCP "const__2" "clojure.test$inc_report_counter$fn__7050" (class "clojure.lang.Var")))) 
                                      (18 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "name" "clojure.test$inc_report_counter$fn__7050" (class "java.lang.Object")))) 
                                      (25 (getstatic (fieldCP "const__1" "clojure.test$inc_report_counter$fn__7050" (class "clojure.lang.Var")))) 
                                      (28 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (31 (checkcast (class "clojure.lang.IFn"))) 
                                      (34 (aload_0)) 
                                      (35 (getfield (fieldCP "name" "clojure.test$inc_report_counter$fn__7050" (class "java.lang.Object")))) 
                                      (38 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (43 (astore_1)) 
                                      (44 (aload_1)) 
                                      (45 (dup)) 
                                      (46 (ifnull 61)) ;;to TAG_0
                                      (49 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (52 (if_acmpeq 62)) ;;to TAG_1
                                      (55 (aload_1)) 
                                      (56 (aconst_null)) 
                                      (57 (astore_1)) 
                                      (58 (goto 65))  ;;to TAG_2
                                      (61 (pop)) ;;at TAG_0
                                      (62 (getstatic (fieldCP "const__4" "clojure.test$inc_report_counter$fn__7050" (class "java.lang.Object")))) ;;at TAG_1
                                      (65 (invokestatic (methodCP "inc" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) ;;at TAG_2
                                      (68 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (73 (areturn)) 
                                      (endofcode 74))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *test$inc_report_counter$fn__7050-class-table*
  (make-static-class-decls 
   *clojure.test$inc_report_counter$fn__7050*))

(defconst *package-name-map* 
  ("clojure.test$inc_report_counter$fn__7050" . "clojure"))

