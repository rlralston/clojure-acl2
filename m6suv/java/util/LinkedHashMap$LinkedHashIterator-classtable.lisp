; LinkedHashMap$LinkedHashIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.LinkedHashMap$LinkedHashIterator*
 (make-class-def
      '(class "java.util.LinkedHashMap$LinkedHashIterator"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "nextEntry" (class "java.util.LinkedHashMap$Entry") (accessflags  *class* ) -1)
                        (field "lastReturned" (class "java.util.LinkedHashMap$Entry") (accessflags  *class* ) -1)
                        (field "expectedModCount" int (accessflags  *class* ) -1)
                        (field "this$0" (class "java.util.LinkedHashMap") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.LinkedHashMap"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 40)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (aload_0))
                                      (11 (getfield (fieldCP "this$0" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap"))))
                                      (14 (invokestatic
					(methodCP "access$100" "java.util.LinkedHashMap" ((class "java.util.LinkedHashMap")) (class "java.util.LinkedHashMap$Entry"))))
                                      (17 (getfield (fieldCP "after" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry"))))
                                      (20 (putfield (fieldCP "nextEntry" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap$Entry"))))
                                      (23 (aload_0))
                                      (24 (aconst_null))
                                      (25 (putfield (fieldCP "lastReturned" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap$Entry"))))
                                      (28 (aload_0))
                                      (29 (aload_0))
                                      (30 (getfield (fieldCP "this$0" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap"))))
                                      (33 (getfield (fieldCP "modCount" "java.util.LinkedHashMap" int)))
                                      (36 (putfield (fieldCP "expectedModCount" "java.util.LinkedHashMap$LinkedHashIterator" int)))
                                      (39 (return))
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "nextEntry" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap$Entry")))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "this$0" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap")))) 
                                      (8 (invokestatic (methodCP "access$100" "java.util.LinkedHashMap" ((class "java.util.LinkedHashMap")) (class "java.util.LinkedHashMap$Entry")))) 
                                      (11 (if_acmpeq 18))  ;;to TAG_0
                                      (14 (iconst_1)) 
                                      (15 (goto 19)) ;;to TAG_1
                                      (18 (iconst_0)) ;;at TAG_0
                                      (19 (ireturn)) ;;at TAG_1
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 69)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "lastReturned" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap$Entry")))) 
                                      (4 (ifnonnull 15))  ;;to TAG_0
                                      (7 (new (class "java.lang.IllegalStateException"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" () void))) 
                                      (14 (athrow)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "this$0" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap")))) 
                                      (19 (getfield (fieldCP "modCount" "java.util.LinkedHashMap" int))) 
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "expectedModCount" "java.util.LinkedHashMap$LinkedHashIterator" int))) 
                                      (26 (if_icmpeq 37)) ;;to TAG_1
                                      (29 (new (class "java.util.ConcurrentModificationException"))) 
                                      (32 (dup)) 
                                      (33 (invokespecial (methodCP "<init>" "java.util.ConcurrentModificationException" () void))) 
                                      (36 (athrow)) 
                                      (37 (aload_0)) ;;at TAG_1
                                      (38 (getfield (fieldCP "this$0" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap")))) 
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "lastReturned" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap$Entry")))) 
                                      (45 (getfield (fieldCP "key" "java.util.LinkedHashMap$Entry" (class "java.lang.Object")))) 
                                      (48 (invokevirtual (methodCP "remove" "java.util.LinkedHashMap" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (51 (pop)) 
                                      (52 (aload_0)) 
                                      (53 (aconst_null)) 
                                      (54 (putfield (fieldCP "lastReturned" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap$Entry")))) 
                                      (57 (aload_0)) 
                                      (58 (aload_0)) 
                                      (59 (getfield (fieldCP "this$0" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap")))) 
                                      (62 (getfield (fieldCP "modCount" "java.util.LinkedHashMap" int))) 
                                      (65 (putfield (fieldCP "expectedModCount" "java.util.LinkedHashMap$LinkedHashIterator" int))) 
                                      (68 (return)) 
                                      (endofcode 69))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextEntry"
                              (parameters )
                              (returntype . (class "java.util.LinkedHashMap$Entry"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 64)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$0" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap")))) 
                                      (4 (getfield (fieldCP "modCount" "java.util.LinkedHashMap" int))) 
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "expectedModCount" "java.util.LinkedHashMap$LinkedHashIterator" int))) 
                                      (11 (if_icmpeq 22))  ;;to TAG_0
                                      (14 (new (class "java.util.ConcurrentModificationException"))) 
                                      (17 (dup)) 
                                      (18 (invokespecial (methodCP "<init>" "java.util.ConcurrentModificationException" () void))) 
                                      (21 (athrow)) 
                                      (22 (aload_0)) ;;at TAG_0
                                      (23 (getfield (fieldCP "nextEntry" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap$Entry")))) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "this$0" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap")))) 
                                      (30 (invokestatic (methodCP "access$100" "java.util.LinkedHashMap" ((class "java.util.LinkedHashMap")) (class "java.util.LinkedHashMap$Entry")))) 
                                      (33 (if_acmpne 44)) ;;to TAG_1
                                      (36 (new (class "java.util.NoSuchElementException"))) 
                                      (39 (dup)) 
                                      (40 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (43 (athrow)) 
                                      (44 (aload_0)) ;;at TAG_1
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "nextEntry" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap$Entry")))) 
                                      (49 (dup_x1)) 
                                      (50 (putfield (fieldCP "lastReturned" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap$Entry")))) 
                                      (53 (astore_1)) 
                                      (54 (aload_0)) 
                                      (55 (aload_1)) 
                                      (56 (getfield (fieldCP "after" "java.util.LinkedHashMap$Entry" (class "java.util.LinkedHashMap$Entry")))) 
                                      (59 (putfield (fieldCP "nextEntry" "java.util.LinkedHashMap$LinkedHashIterator" (class "java.util.LinkedHashMap$Entry")))) 
                                      (62 (aload_1)) 
                                      (63 (areturn)) 
                                      (endofcode 64))
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
					(methodCP "<init>" "java.util.LinkedHashMap$LinkedHashIterator" ((class "java.util.LinkedHashMap")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Iterator")
            (accessflags  *abstract*  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *LinkedHashMap$LinkedHashIterator-class-table*
  (make-static-class-decls 
   *java.util.LinkedHashMap$LinkedHashIterator*))

(defconst *package-name-map* 
  ("java.util.LinkedHashMap$LinkedHashIterator" . "java.util"))
