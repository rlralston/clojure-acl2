; LinkedHashMap$ValueIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.LinkedHashMap$ValueIterator*
 (make-class-def
      '(class "java.util.LinkedHashMap$ValueIterator"
            "java.util.LinkedHashMap$LinkedHashIterator"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.util.LinkedHashMap") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.LinkedHashMap"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.LinkedHashMap$ValueIterator" (class "java.util.LinkedHashMap"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (aconst_null))
                                      (8 (invokespecial
					(methodCP "<init>" "java.util.LinkedHashMap$LinkedHashIterator" ((class "java.util.LinkedHashMap") (class "java.util.LinkedHashMap$1")) void)))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "nextEntry" "java.util.LinkedHashMap$ValueIterator" () (class "java.util.LinkedHashMap$Entry"))))
                                      (4 (getfield (fieldCP "value" "java.util.LinkedHashMap$Entry" (class "java.lang.Object"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.LinkedHashMap") (class "java.util.LinkedHashMap$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.LinkedHashMap$ValueIterator" ((class "java.util.LinkedHashMap")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *LinkedHashMap$ValueIterator-class-table*
  (make-static-class-decls 
   *java.util.LinkedHashMap$ValueIterator*))

(defconst *package-name-map* 
  ("java.util.LinkedHashMap$ValueIterator" . "java.util"))

