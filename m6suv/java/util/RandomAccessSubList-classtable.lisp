; RandomAccessSubList-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.RandomAccessSubList*
 (make-class-def
      '(class "java.util.RandomAccessSubList"
            "java.util.SubList"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters (class "java.util.AbstractList") int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (iload_3))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.SubList" ((class "java.util.AbstractList") int int) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "subList"
                              (parameters int int)
                              (returntype . (class "java.util.List"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.util.RandomAccessSubList")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (iload_2))
                                      (7 (invokespecial
					(methodCP "<init>" "java.util.RandomAccessSubList" ((class "java.util.AbstractList") int int) void)))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.RandomAccess")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *RandomAccessSubList-class-table*
  (make-static-class-decls 
   *java.util.RandomAccessSubList*))

(defconst *package-name-map* 
  ("java.util.RandomAccessSubList" . "java.util"))
