; Collections$UnmodifiableList$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.Collections$UnmodifiableList$1*
 (make-class-def
      '(class "java.util.Collections$UnmodifiableList$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "i" (class "java.util.ListIterator") (accessflags  *class*  *final*  *private* ) -1)
                        (field "val$index" int (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "java.util.Collections$UnmodifiableList") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Collections$UnmodifiableList") int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.Collections$UnmodifiableList$1" (class "java.util.Collections$UnmodifiableList"))))
                                      (5 (aload_0))
                                      (6 (iload_2))
                                      (7 (putfield (fieldCP "val$index" "java.util.Collections$UnmodifiableList$1" int)))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (aload_0))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "this$0" "java.util.Collections$UnmodifiableList$1" (class "java.util.Collections$UnmodifiableList"))))
                                      (19 (getfield (fieldCP "list" "java.util.Collections$UnmodifiableList" (class "java.util.List"))))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "val$index" "java.util.Collections$UnmodifiableList$1" int)))
                                      (26 (invokeinterface
					(methodCP "listIterator" "java.util.List" (int) (class "java.util.ListIterator")) 2))
                                      (31 (putfield (fieldCP "i" "java.util.Collections$UnmodifiableList$1" (class "java.util.ListIterator"))))
                                      (34 (return))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.Collections$UnmodifiableList$1" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "hasNext" "java.util.ListIterator" () boolean) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.Collections$UnmodifiableList$1" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "next" "java.util.ListIterator" () (class "java.lang.Object")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasPrevious"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.Collections$UnmodifiableList$1" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "hasPrevious" "java.util.ListIterator" () boolean) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "previous"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.Collections$UnmodifiableList$1" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "previous" "java.util.ListIterator" () (class "java.lang.Object")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.Collections$UnmodifiableList$1" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "nextIndex" "java.util.ListIterator" () int) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "previousIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.Collections$UnmodifiableList$1" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "previousIndex" "java.util.ListIterator" () int) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "set"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.ListIterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$UnmodifiableList$1-class-table*
  (make-static-class-decls 
   *java.util.Collections$UnmodifiableList$1*))

(defconst *package-name-map* 
  ("java.util.Collections$UnmodifiableList$1" . "java.util"))
