; CopyOnWriteArrayList$COWIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.CopyOnWriteArrayList$COWIterator*
 (make-class-def
      '(class "java.util.concurrent.CopyOnWriteArrayList$COWIterator"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "snapshot" (array (class "java.lang.Object")) (accessflags  *class*  *final*  *private* ) -1)
                        (field "cursor" int (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (array (class "java.lang.Object")) int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iload_2))
                                      (6 (putfield (fieldCP "cursor" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" int)))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (putfield (fieldCP "snapshot" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" (array (class "java.lang.Object")))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "cursor" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "snapshot" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" (array (class "java.lang.Object"))))) 
                                      (8 (arraylength)) 
                                      (9 (if_icmpge 16))  ;;to TAG_0
                                      (12 (iconst_1)) 
                                      (13 (goto 17)) ;;to TAG_1
                                      (16 (iconst_0)) ;;at TAG_0
                                      (17 (ireturn)) ;;at TAG_1
                                      (endofcode 18))
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
                                      (1 (getfield (fieldCP "cursor" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" int))) 
                                      (4 (ifle 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "hasNext" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" () boolean))) 
                                      (4 (ifne 15))  ;;to TAG_0
                                      (7 (new (class "java.util.NoSuchElementException"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (14 (athrow)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "snapshot" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" (array (class "java.lang.Object"))))) 
                                      (19 (aload_0)) 
                                      (20 (dup)) 
                                      (21 (getfield (fieldCP "cursor" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" int))) 
                                      (24 (dup_x1)) 
                                      (25 (iconst_1)) 
                                      (26 (iadd)) 
                                      (27 (putfield (fieldCP "cursor" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" int))) 
                                      (30 (aaload)) 
                                      (31 (areturn)) 
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "previous"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "hasPrevious" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" () boolean))) 
                                      (4 (ifne 15))  ;;to TAG_0
                                      (7 (new (class "java.util.NoSuchElementException"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (14 (athrow)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "snapshot" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" (array (class "java.lang.Object"))))) 
                                      (19 (aload_0)) 
                                      (20 (dup)) 
                                      (21 (getfield (fieldCP "cursor" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" int))) 
                                      (24 (iconst_1)) 
                                      (25 (isub)) 
                                      (26 (dup_x1)) 
                                      (27 (putfield (fieldCP "cursor" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" int))) 
                                      (30 (aaload)) 
                                      (31 (areturn)) 
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "cursor" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "previousIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "cursor" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" int)))
                                      (4 (iconst_1))
                                      (5 (isub))
                                      (6 (ireturn))
                                      (endofcode 7))
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
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array (class "java.lang.Object")) int (class "java.util.concurrent.CopyOnWriteArrayList$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" ((array (class "java.lang.Object")) int) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.ListIterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *CopyOnWriteArrayList$COWIterator-class-table*
  (make-static-class-decls 
   *java.util.concurrent.CopyOnWriteArrayList$COWIterator*))

(defconst *package-name-map* 
  ("java.util.concurrent.CopyOnWriteArrayList$COWIterator" . "java.util.concurrent"))
