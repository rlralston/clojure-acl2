; data$fn__8964-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:48 CDT 2014.
;

(defconst *clojure.data$fn__8964*
 (make-class-def
      '(class "clojure.data$fn__8964"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "sequential")
                        (STRING  "atom")
                        (STRING  "getClass")
                        (STRING  "isArray"))
            (fields
                        (field "const__0" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 25)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (ldc 0))         ;;STRING:: "sequential"
                                      (3 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (6 (checkcast (class "clojure.lang.Keyword")))
                                      (9 (putstatic (fieldCP "const__0" "clojure.data$fn__8964" (class "clojure.lang.Keyword"))))
                                      (12 (aconst_null))
                                      (13 (ldc 1))        ;;STRING:: "atom"
                                      (15 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (18 (checkcast (class "clojure.lang.Keyword")))
                                      (21 (putstatic (fieldCP "const__1" "clojure.data$fn__8964" (class "clojure.lang.Keyword"))))
                                      (24 (return))
                                      (endofcode 25))
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
                                   (max_stack . 2) (max_locals . 2) (code_length . 34)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (ldc 2)) ;;STRING:: "getClass"
                                      (5 (invokestatic (methodCP "invokeNoArgInstanceMember" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object")))) 
                                      (8 (ldc 3)) ;;STRING:: "isArray"
                                      (10 (invokestatic (methodCP "invokeNoArgInstanceMember" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object")))) 
                                      (13 (dup)) 
                                      (14 (ifnull 29)) ;;to TAG_0
                                      (17 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (20 (if_acmpeq 30)) ;;to TAG_1
                                      (23 (getstatic (fieldCP "const__0" "clojure.data$fn__8964" (class "clojure.lang.Keyword")))) 
                                      (26 (goto 33))  ;;to TAG_2
                                      (29 (pop)) ;;at TAG_0
                                      (30 (getstatic (fieldCP "const__1" "clojure.data$fn__8964" (class "clojure.lang.Keyword")))) ;;at TAG_1
                                      (33 (areturn)) ;;at TAG_2
                                      (endofcode 34))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *data$fn__8964-class-table*
  (make-static-class-decls 
   *clojure.data$fn__8964*))

(defconst *package-name-map* 
  ("clojure.data$fn__8964" . "clojure"))

