; Collections$CheckedList$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.util.Collections$CheckedList$1*
 (make-class-def
      '(class "java.util.Collections$CheckedList$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$i" (class "java.util.ListIterator") (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "java.util.Collections$CheckedList") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Collections$CheckedList") (class "java.util.ListIterator"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.Collections$CheckedList$1" (class "java.util.Collections$CheckedList"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$i" "java.util.Collections$CheckedList$1" (class "java.util.ListIterator"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
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
                                      (1 (getfield (fieldCP "val$i" "java.util.Collections$CheckedList$1" (class "java.util.ListIterator"))))
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
                                      (1 (getfield (fieldCP "val$i" "java.util.Collections$CheckedList$1" (class "java.util.ListIterator"))))
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
                                      (1 (getfield (fieldCP "val$i" "java.util.Collections$CheckedList$1" (class "java.util.ListIterator"))))
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
                                      (1 (getfield (fieldCP "val$i" "java.util.Collections$CheckedList$1" (class "java.util.ListIterator"))))
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
                                      (1 (getfield (fieldCP "val$i" "java.util.Collections$CheckedList$1" (class "java.util.ListIterator"))))
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
                                      (1 (getfield (fieldCP "val$i" "java.util.Collections$CheckedList$1" (class "java.util.ListIterator"))))
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
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$i" "java.util.Collections$CheckedList$1" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "remove" "java.util.ListIterator" () void) 1))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "set"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.Collections$CheckedList$1" (class "java.util.Collections$CheckedList"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "typeCheck" "java.util.Collections$CheckedList" ((class "java.lang.Object")) void)))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "val$i" "java.util.Collections$CheckedList$1" (class "java.util.ListIterator"))))
                                      (12 (aload_1))
                                      (13 (invokeinterface
					(methodCP "set" "java.util.ListIterator" ((class "java.lang.Object")) void) 2))
                                      (18 (return))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.Collections$CheckedList$1" (class "java.util.Collections$CheckedList"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "typeCheck" "java.util.Collections$CheckedList" ((class "java.lang.Object")) void)))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "val$i" "java.util.Collections$CheckedList$1" (class "java.util.ListIterator"))))
                                      (12 (aload_1))
                                      (13 (invokeinterface
					(methodCP "add" "java.util.ListIterator" ((class "java.lang.Object")) void) 2))
                                      (18 (return))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.ListIterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$CheckedList$1-class-table*
  (make-static-class-decls 
   *java.util.Collections$CheckedList$1*))

(defconst *package-name-map* 
  ("java.util.Collections$CheckedList$1" . "java.util"))
