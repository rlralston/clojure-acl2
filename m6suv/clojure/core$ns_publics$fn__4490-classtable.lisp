; core$ns_publics$fn__4490-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$ns_publics$fn__4490*
 (make-class-def
      '(class "clojure.core$ns_publics$fn__4490"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "instance?")
                        (STRING  "="))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "ns" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "instance?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$ns_publics$fn__4490" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "="
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$ns_publics$fn__4490" (class "clojure.lang.Var"))))
                                      (26 (return))
                                      (endofcode 27))
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
                                      (6 (putfield (fieldCP "ns" "clojure.core$ns_publics$fn__4490" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 84)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "clojure.lang.Var"))) 
                                      (4 (istore_2)) 
                                      (5 (iload_2)) 
                                      (6 (ifeq 70)) ;;to TAG_0
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "ns" "clojure.core$ns_publics$fn__4490" (class "java.lang.Object")))) 
                                      (13 (aload_1)) 
                                      (14 (checkcast (class "clojure.lang.Var"))) 
                                      (17 (getfield (fieldCP "ns" "clojure.lang.Var" (class "clojure.lang.Namespace")))) 
                                      (20 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (23 (istore_3)) 
                                      (24 (iload_3)) 
                                      (25 (ifeq 53)) ;;to TAG_1
                                      (28 (aload_1)) 
                                      (29 (aconst_null)) 
                                      (30 (astore_1)) 
                                      (31 (checkcast (class "clojure.lang.Var"))) 
                                      (34 (invokevirtual (methodCP "isPublic" "clojure.lang.Var" () boolean))) 
                                      (37 (ifeq 46))  ;;to TAG_2
                                      (40 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (43 (goto 49)) ;;to TAG_3
                                      (46 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_2
                                      (49 (goto 66)) ;;to TAG_4;;at TAG_3
                                      (52 (pop)) 
                                      (53 (iload_3)) ;;at TAG_1
                                      (54 (ifeq 63)) ;;to TAG_5
                                      (57 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (60 (goto 66)) ;;to TAG_4
                                      (63 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_5
                                      (66 (goto 83)) ;;to TAG_6;;at TAG_4
                                      (69 (pop)) 
                                      (70 (iload_2)) ;;at TAG_0
                                      (71 (ifeq 80)) ;;to TAG_7
                                      (74 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (77 (goto 83)) ;;to TAG_6
                                      (80 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_7
                                      (83 (areturn)) ;;at TAG_6
                                      (endofcode 84))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$ns_publics$fn__4490-class-table*
  (make-static-class-decls 
   *clojure.core$ns_publics$fn__4490*))

(defconst *package-name-map* 
  ("clojure.core$ns_publics$fn__4490" . "clojure"))

