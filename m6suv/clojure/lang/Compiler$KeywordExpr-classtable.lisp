; Compiler$KeywordExpr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$KeywordExpr*
 (make-class-def
      '(class "clojure.lang.Compiler$KeywordExpr"
            "clojure.lang.Compiler$LiteralExpr"
            (constant_pool)
            (fields
                        (field "k" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.Keyword"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.Compiler$LiteralExpr" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "k" "clojure.lang.Compiler$KeywordExpr" (class "clojure.lang.Keyword"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "val"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "k" "clojure.lang.Compiler$KeywordExpr" (class "clojure.lang.Keyword"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "eval"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "k" "clojure.lang.Compiler$KeywordExpr" (class "clojure.lang.Keyword"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "emit"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (aload_3)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "k" "clojure.lang.Compiler$KeywordExpr" (class "clojure.lang.Keyword")))) 
                                      (6 (invokevirtual (methodCP "emitKeyword" "clojure.lang.Compiler$ObjExpr" ((class "clojure.asm.commons.GeneratorAdapter") (class "clojure.lang.Keyword")) void))) 
                                      (9 (aload_1)) 
                                      (10 (getstatic (fieldCP "STATEMENT" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (13 (if_acmpne 20))  ;;to TAG_0
                                      (16 (aload_3)) 
                                      (17 (invokevirtual (methodCP "pop" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (20 (return)) ;;at TAG_0
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasJavaClass"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getJavaClass"
                              (parameters )
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (ldc_w ))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$KeywordExpr-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$KeywordExpr*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$KeywordExpr" . "clojure.lang"))

