; core$re_pattern-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$re_pattern*
 (make-class-def
      '(class "clojure.core$re_pattern"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "instance?"))
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
                                      (2 (ldc 1))         ;;STRING:: "instance?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$re_pattern" (class "clojure.lang.Var"))))
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
                                   (max_stack . 2) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.util.regex.Pattern"))) 
                                      (4 (ifeq 14))  ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (aconst_null)) 
                                      (9 (astore_1)) 
                                      (10 (goto 23)) ;;to TAG_1
                                      (13 (pop)) 
                                      (14 (aload_1)) ;;at TAG_0
                                      (15 (aconst_null)) 
                                      (16 (astore_1)) 
                                      (17 (checkcast (class "java.lang.String"))) 
                                      (20 (invokestatic (methodCP "compile" "java.util.regex.Pattern" ((class "java.lang.String")) (class "java.util.regex.Pattern")))) 
                                      (23 (areturn)) ;;at TAG_1
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$re_pattern-class-table*
  (make-static-class-decls 
   *clojure.core$re_pattern*))

(defconst *package-name-map* 
  ("clojure.core$re_pattern" . "clojure"))

