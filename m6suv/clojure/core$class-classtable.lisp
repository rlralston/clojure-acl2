; core$class-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$class*
 (make-class-def
      '(class "clojure.core$class"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nil?"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nil?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$class" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
                                   (max_stack . 2) (max_locals . 2) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (invokestatic (methodCP "identical" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (5 (ifeq 15))  ;;to TAG_0
                                      (8 (aload_1)) 
                                      (9 (aconst_null)) 
                                      (10 (astore_1)) 
                                      (11 (goto 21)) ;;to TAG_1
                                      (14 (pop)) 
                                      (15 (aload_1)) ;;at TAG_0
                                      (16 (aconst_null)) 
                                      (17 (astore_1)) 
                                      (18 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (21 (areturn)) ;;at TAG_1
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$class-class-table*
  (make-static-class-decls 
   *clojure.core$class*))

(defconst *package-name-map* 
  ("clojure.core$class" . "clojure"))

