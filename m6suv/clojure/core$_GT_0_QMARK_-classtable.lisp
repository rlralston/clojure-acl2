; core$_GT_0_QMARK_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:46 CDT 2014.
;

(defconst *clojure.core$_GT_0_QMARK_*
 (make-class-def
      '(class "clojure.core$_GT_0_QMARK_"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields
                        (field "const__0" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 8)
                                   (parsedcode
                                      (0 (lconst_0))
                                      (1 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (4 (putstatic (fieldCP "const__0" "clojure.core$_GT_0_QMARK_" (class "java.lang.Object"))))
                                      (7 (return))
                                      (endofcode 8))
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
                                   (max_stack . 3) (max_locals . 2) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (lconst_0)) 
                                      (4 (invokestatic (methodCP "gt" "clojure.lang.Numbers" ((class "java.lang.Object") long) boolean))) 
                                      (7 (ifeq 16))  ;;to TAG_0
                                      (10 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (13 (goto 19)) ;;to TAG_1
                                      (16 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (19 (areturn)) ;;at TAG_1
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$_GT_0_QMARK_-class-table*
  (make-static-class-decls 
   *clojure.core$_GT_0_QMARK_*))

(defconst *package-name-map* 
  ("clojure.core$_GT_0_QMARK_" . "clojure"))
