; CopyOnWriteArrayList$COWSubListIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator*
 (make-class-def
      '(class "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "i" (class "java.util.ListIterator") (accessflags  *class*  *final*  *private* ) -1)
                        (field "index" int (accessflags  *class*  *final*  *private* ) -1)
                        (field "offset" int (accessflags  *class*  *final*  *private* ) -1)
                        (field "size" int (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.List") int int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 34)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iload_2))
                                      (6 (putfield (fieldCP "index" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" int)))
                                      (9 (aload_0))
                                      (10 (iload_3))
                                      (11 (putfield (fieldCP "offset" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" int)))
                                      (14 (aload_0))
                                      (15 (iload 4))
                                      (17 (putfield (fieldCP "size" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" int)))
                                      (20 (aload_0))
                                      (21 (aload_1))
                                      (22 (iload_2))
                                      (23 (iload_3))
                                      (24 (iadd))
                                      (25 (invokeinterface
					(methodCP "listIterator" "java.util.List" (int) (class "java.util.ListIterator")) 2))
                                      (30 (putfield (fieldCP "i" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" (class "java.util.ListIterator"))))
                                      (33 (return))
                                      (endofcode 34))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "nextIndex" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" () int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "size" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" int))) 
                                      (8 (if_icmpge 15))  ;;to TAG_0
                                      (11 (iconst_1)) 
                                      (12 (goto 16)) ;;to TAG_1
                                      (15 (iconst_0)) ;;at TAG_0
                                      (16 (ireturn)) ;;at TAG_1
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "hasNext" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" () boolean))) 
                                      (4 (ifeq 17))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "i" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" (class "java.util.ListIterator")))) 
                                      (11 (invokeinterface (methodCP "next" "java.util.ListIterator" () (class "java.lang.Object")) 1)) 
                                      (16 (areturn)) 
                                      (17 (new (class "java.util.NoSuchElementException"))) ;;at TAG_0
                                      (20 (dup)) 
                                      (21 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (24 (athrow)) 
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasPrevious"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "previousIndex" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" () int))) 
                                      (4 (iflt 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "previous"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "hasPrevious" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" () boolean))) 
                                      (4 (ifeq 17))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "i" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" (class "java.util.ListIterator")))) 
                                      (11 (invokeinterface (methodCP "previous" "java.util.ListIterator" () (class "java.lang.Object")) 1)) 
                                      (16 (areturn)) 
                                      (17 (new (class "java.util.NoSuchElementException"))) ;;at TAG_0
                                      (20 (dup)) 
                                      (21 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (24 (athrow)) 
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "nextIndex" "java.util.ListIterator" () int) 1))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "offset" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" int)))
                                      (13 (isub))
                                      (14 (ireturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "previousIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "previousIndex" "java.util.ListIterator" () int) 1))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "offset" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" int)))
                                      (13 (isub))
                                      (14 (ireturn))
                                      (endofcode 15))
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
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *CopyOnWriteArrayList$COWSubListIterator-class-table*
  (make-static-class-decls 
   *java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator*))

(defconst *package-name-map* 
  ("java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" . "java.util.concurrent"))
