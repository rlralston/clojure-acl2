; pprint$abort_QMARK_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:54 CDT 2014.
;

(defconst *clojure.pprint$abort_QMARK_*
 (make-class-def
      '(class "clojure.pprint$abort_QMARK_"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "first")
                        (STRING  "=")
                        (STRING  "up-arrow")
                        (STRING  "colon-up-arrow"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 51)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "first"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$abort_QMARK_" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "="
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$abort_QMARK_" (class "clojure.lang.Var"))))
                                      (26 (aconst_null))
                                      (27 (ldc 3))        ;;STRING:: "up-arrow"
                                      (29 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (32 (checkcast (class "clojure.lang.Keyword")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.pprint$abort_QMARK_" (class "clojure.lang.Keyword"))))
                                      (38 (aconst_null))
                                      (39 (ldc 4))        ;;STRING:: "colon-up-arrow"
                                      (41 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (44 (checkcast (class "clojure.lang.Keyword")))
                                      (47 (putstatic (fieldCP "const__3" "clojure.pprint$abort_QMARK_" (class "clojure.lang.Keyword"))))
                                      (50 (return))
                                      (endofcode 51))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 69)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$abort_QMARK_" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_1)) 
                                      (12 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (17 (astore_2)) 
                                      (18 (getstatic (fieldCP "const__2" "clojure.pprint$abort_QMARK_" (class "clojure.lang.Keyword")))) 
                                      (21 (aload_2)) 
                                      (22 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (25 (istore_3)) 
                                      (26 (iload_3)) 
                                      (27 (ifeq 47)) ;;to TAG_0
                                      (30 (iload_3)) 
                                      (31 (ifeq 40)) ;;to TAG_1
                                      (34 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (37 (goto 43))  ;;to TAG_2
                                      (40 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_1
                                      (43 (goto 68)) ;;to TAG_3;;at TAG_2
                                      (46 (pop)) 
                                      (47 (getstatic (fieldCP "const__3" "clojure.pprint$abort_QMARK_" (class "clojure.lang.Keyword")))) ;;at TAG_0
                                      (50 (aload_2)) 
                                      (51 (aconst_null)) 
                                      (52 (astore_2)) 
                                      (53 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (56 (ifeq 65)) ;;to TAG_4
                                      (59 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (62 (goto 68)) ;;to TAG_3
                                      (65 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_4
                                      (68 (areturn)) ;;at TAG_3
                                      (endofcode 69))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$abort_QMARK_-class-table*
  (make-static-class-decls 
   *clojure.pprint$abort_QMARK_*))

(defconst *package-name-map* 
  ("clojure.pprint$abort_QMARK_" . "clojure"))

