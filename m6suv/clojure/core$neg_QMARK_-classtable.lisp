; core$neg_QMARK_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$neg_QMARK_*
 (make-class-def
      '(class "clojure.core$neg_QMARK_"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields)
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 0) (max_locals . 0) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
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
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (invokestatic (methodCP "isNeg" "clojure.lang.Numbers" ((class "java.lang.Object")) boolean))) 
                                      (6 (ifeq 15))  ;;to TAG_0
                                      (9 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (12 (goto 18)) ;;to TAG_1
                                      (15 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (18 (areturn)) ;;at TAG_1
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$neg_QMARK_-class-table*
  (make-static-class-decls 
   *clojure.core$neg_QMARK_*))

(defconst *package-name-map* 
  ("clojure.core$neg_QMARK_" . "clojure"))

