; Util$3-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:53 CDT 2014.
;

(defconst *clojure.lang.Util$3*
 (make-class-def
      '(class "clojure.lang.Util$3"
            "java.lang.Object"
            (constant_pool)
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
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equiv"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (instanceof (class "java.lang.Number"))) 
                                      (4 (ifeq 19))  ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (checkcast (class "java.lang.Number"))) 
                                      (11 (aload_2)) 
                                      (12 (checkcast (class "java.lang.Number"))) 
                                      (15 (invokestatic (methodCP "equal" "clojure.lang.Numbers" ((class "java.lang.Number") (class "java.lang.Number")) boolean))) 
                                      (18 (ireturn)) 
                                      (19 (iconst_0)) ;;at TAG_0
                                      (20 (ireturn)) 
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.Util$EquivPred")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Util$3-class-table*
  (make-static-class-decls 
   *clojure.lang.Util$3*))

(defconst *package-name-map* 
  ("clojure.lang.Util$3" . "clojure.lang"))

