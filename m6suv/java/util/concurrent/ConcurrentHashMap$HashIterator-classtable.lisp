; ConcurrentHashMap$HashIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.ConcurrentHashMap$HashIterator*
 (make-class-def
      '(class "java.util.concurrent.ConcurrentHashMap$HashIterator"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "nextSegmentIndex" int (accessflags  *class* ) -1)
                        (field "nextTableIndex" int (accessflags  *class* ) -1)
                        (field "currentTable" (array (class "java.util.concurrent.ConcurrentHashMap$HashEntry")) (accessflags  *class* ) -1)
                        (field "nextEntry" (class "java.util.concurrent.ConcurrentHashMap$HashEntry") (accessflags  *class* ) -1)
                        (field "lastReturned" (class "java.util.concurrent.ConcurrentHashMap$HashEntry") (accessflags  *class* ) -1)
                        (field "this$0" (class "java.util.concurrent.ConcurrentHashMap") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.concurrent.ConcurrentHashMap"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (getfield (fieldCP "segments" "java.util.concurrent.ConcurrentHashMap" (array (class "java.util.concurrent.ConcurrentHashMap$Segment")))))
                                      (14 (arraylength))
                                      (15 (iconst_1))
                                      (16 (isub))
                                      (17 (putfield (fieldCP "nextSegmentIndex" "java.util.concurrent.ConcurrentHashMap$HashIterator" int)))
                                      (20 (aload_0))
                                      (21 (iconst_m1))
                                      (22 (putfield (fieldCP "nextTableIndex" "java.util.concurrent.ConcurrentHashMap$HashIterator" int)))
                                      (25 (aload_0))
                                      (26 (invokevirtual
					(methodCP "advance" "java.util.concurrent.ConcurrentHashMap$HashIterator" () void)))
                                      (29 (return))
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "advance"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 96)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (getfield (fieldCP "nextTableIndex" "java.util.concurrent.ConcurrentHashMap$HashIterator" int))) 
                                      (4 (iflt 36)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "currentTable" "java.util.concurrent.ConcurrentHashMap$HashIterator" (array (class "java.util.concurrent.ConcurrentHashMap$HashEntry"))))) 
                                      (12 (aload_0)) 
                                      (13 (dup)) 
                                      (14 (getfield (fieldCP "nextTableIndex" "java.util.concurrent.ConcurrentHashMap$HashIterator" int))) 
                                      (17 (dup_x1)) 
                                      (18 (iconst_1)) 
                                      (19 (isub)) 
                                      (20 (putfield (fieldCP "nextTableIndex" "java.util.concurrent.ConcurrentHashMap$HashIterator" int))) 
                                      (23 (invokestatic (methodCP "entryAt" "java.util.concurrent.ConcurrentHashMap" ((array (class "java.util.concurrent.ConcurrentHashMap$HashEntry")) int) (class "java.util.concurrent.ConcurrentHashMap$HashEntry")))) 
                                      (26 (dup_x1)) 
                                      (27 (putfield (fieldCP "nextEntry" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap$HashEntry")))) 
                                      (30 (ifnull 0)) ;;to TAG_1
                                      (33 (goto 95))  ;;to TAG_2
                                      (36 (aload_0)) ;;at TAG_0
                                      (37 (getfield (fieldCP "nextSegmentIndex" "java.util.concurrent.ConcurrentHashMap$HashIterator" int))) 
                                      (40 (iflt 95))  ;;to TAG_2
                                      (43 (aload_0)) 
                                      (44 (getfield (fieldCP "this$0" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap")))) 
                                      (47 (getfield (fieldCP "segments" "java.util.concurrent.ConcurrentHashMap" (array (class "java.util.concurrent.ConcurrentHashMap$Segment"))))) 
                                      (50 (aload_0)) 
                                      (51 (dup)) 
                                      (52 (getfield (fieldCP "nextSegmentIndex" "java.util.concurrent.ConcurrentHashMap$HashIterator" int))) 
                                      (55 (dup_x1)) 
                                      (56 (iconst_1)) 
                                      (57 (isub)) 
                                      (58 (putfield (fieldCP "nextSegmentIndex" "java.util.concurrent.ConcurrentHashMap$HashIterator" int))) 
                                      (61 (invokestatic (methodCP "segmentAt" "java.util.concurrent.ConcurrentHashMap" ((array (class "java.util.concurrent.ConcurrentHashMap$Segment")) int) (class "java.util.concurrent.ConcurrentHashMap$Segment")))) 
                                      (64 (astore_1)) 
                                      (65 (aload_1)) 
                                      (66 (ifnull 92)) ;;to TAG_3
                                      (69 (aload_0)) 
                                      (70 (aload_1)) 
                                      (71 (getfield (fieldCP "table" "java.util.concurrent.ConcurrentHashMap$Segment" (array (class "java.util.concurrent.ConcurrentHashMap$HashEntry"))))) 
                                      (74 (dup_x1)) 
                                      (75 (putfield (fieldCP "currentTable" "java.util.concurrent.ConcurrentHashMap$HashIterator" (array (class "java.util.concurrent.ConcurrentHashMap$HashEntry"))))) 
                                      (78 (ifnull 92)) ;;to TAG_3
                                      (81 (aload_0)) 
                                      (82 (aload_0)) 
                                      (83 (getfield (fieldCP "currentTable" "java.util.concurrent.ConcurrentHashMap$HashIterator" (array (class "java.util.concurrent.ConcurrentHashMap$HashEntry"))))) 
                                      (86 (arraylength)) 
                                      (87 (iconst_1)) 
                                      (88 (isub)) 
                                      (89 (putfield (fieldCP "nextTableIndex" "java.util.concurrent.ConcurrentHashMap$HashIterator" int))) 
                                      (92 (goto 0)) ;;to TAG_1;;at TAG_3
                                      (95 (return)) ;;at TAG_2
                                      (endofcode 96))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextEntry"
                              (parameters )
                              (returntype . (class "java.util.concurrent.ConcurrentHashMap$HashEntry"))
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 40)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "nextEntry" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap$HashEntry")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_1)) 
                                      (6 (ifnonnull 17))  ;;to TAG_0
                                      (9 (new (class "java.util.NoSuchElementException"))) 
                                      (12 (dup)) 
                                      (13 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (aload_1)) 
                                      (19 (putfield (fieldCP "lastReturned" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap$HashEntry")))) 
                                      (22 (aload_0)) 
                                      (23 (aload_1)) 
                                      (24 (getfield (fieldCP "next" "java.util.concurrent.ConcurrentHashMap$HashEntry" (class "java.util.concurrent.ConcurrentHashMap$HashEntry")))) 
                                      (27 (dup_x1)) 
                                      (28 (putfield (fieldCP "nextEntry" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap$HashEntry")))) 
                                      (31 (ifnonnull 38)) ;;to TAG_1
                                      (34 (aload_0)) 
                                      (35 (invokevirtual (methodCP "advance" "java.util.concurrent.ConcurrentHashMap$HashIterator" () void))) 
                                      (38 (aload_1)) ;;at TAG_1
                                      (39 (areturn)) 
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "nextEntry" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap$HashEntry")))) 
                                      (4 (ifnull 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasMoreElements"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "nextEntry" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap$HashEntry")))) 
                                      (4 (ifnull 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "lastReturned" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap$HashEntry")))) 
                                      (4 (ifnonnull 15))  ;;to TAG_0
                                      (7 (new (class "java.lang.IllegalStateException"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" () void))) 
                                      (14 (athrow)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "this$0" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap")))) 
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "lastReturned" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap$HashEntry")))) 
                                      (23 (getfield (fieldCP "key" "java.util.concurrent.ConcurrentHashMap$HashEntry" (class "java.lang.Object")))) 
                                      (26 (invokevirtual (methodCP "remove" "java.util.concurrent.ConcurrentHashMap" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (29 (pop)) 
                                      (30 (aload_0)) 
                                      (31 (aconst_null)) 
                                      (32 (putfield (fieldCP "lastReturned" "java.util.concurrent.ConcurrentHashMap$HashIterator" (class "java.util.concurrent.ConcurrentHashMap$HashEntry")))) 
                                      (35 (return)) 
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ConcurrentHashMap$HashIterator-class-table*
  (make-static-class-decls 
   *java.util.concurrent.ConcurrentHashMap$HashIterator*))

(defconst *package-name-map* 
  ("java.util.concurrent.ConcurrentHashMap$HashIterator" . "java.util.concurrent"))

