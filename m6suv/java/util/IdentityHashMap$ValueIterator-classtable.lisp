; IdentityHashMap$ValueIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.IdentityHashMap$ValueIterator*
 (make-class-def
      '(class "java.util.IdentityHashMap$ValueIterator"
            "java.util.IdentityHashMap$IdentityHashMapIterator"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.util.IdentityHashMap") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.IdentityHashMap"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.IdentityHashMap$ValueIterator" (class "java.util.IdentityHashMap"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (aconst_null))
                                      (8 (invokespecial
					(methodCP "<init>" "java.util.IdentityHashMap$IdentityHashMapIterator" ((class "java.util.IdentityHashMap") (class "java.util.IdentityHashMap$1")) void)))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "traversalTable" "java.util.IdentityHashMap$ValueIterator" (array (class "java.lang.Object")))))
                                      (4 (aload_0))
                                      (5 (invokevirtual
					(methodCP "nextIndex" "java.util.IdentityHashMap$ValueIterator" () int)))
                                      (8 (iconst_1))
                                      (9 (iadd))
                                      (10 (aaload))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.IdentityHashMap") (class "java.util.IdentityHashMap$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.IdentityHashMap$ValueIterator" ((class "java.util.IdentityHashMap")) void)))
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


(defconst *IdentityHashMap$ValueIterator-class-table*
  (make-static-class-decls 
   *java.util.IdentityHashMap$ValueIterator*))

(defconst *package-name-map* 
  ("java.util.IdentityHashMap$ValueIterator" . "java.util"))

