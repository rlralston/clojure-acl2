; EnumMap$EnumMapIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.EnumMap$EnumMapIterator*
 (make-class-def
      '(class "java.util.EnumMap$EnumMapIterator"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "index" int (accessflags  *class* ) -1)
                        (field "lastReturnedIndex" int (accessflags  *class* ) -1)
                        (field "this$0" (class "java.util.EnumMap") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.EnumMap"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.EnumMap$EnumMapIterator" (class "java.util.EnumMap"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (iconst_0))
                                      (11 (putfield (fieldCP "index" "java.util.EnumMap$EnumMapIterator" int)))
                                      (14 (aload_0))
                                      (15 (iconst_m1))
                                      (16 (putfield (fieldCP "lastReturnedIndex" "java.util.EnumMap$EnumMapIterator" int)))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 64)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (getfield (fieldCP "index" "java.util.EnumMap$EnumMapIterator" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "this$0" "java.util.EnumMap$EnumMapIterator" (class "java.util.EnumMap")))) 
                                      (8 (invokestatic (methodCP "access$600" "java.util.EnumMap" ((class "java.util.EnumMap")) (array (class "java.lang.Object"))))) 
                                      (11 (arraylength)) 
                                      (12 (if_icmpge 43)) ;;to TAG_0
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "this$0" "java.util.EnumMap$EnumMapIterator" (class "java.util.EnumMap")))) 
                                      (19 (invokestatic (methodCP "access$600" "java.util.EnumMap" ((class "java.util.EnumMap")) (array (class "java.lang.Object"))))) 
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "index" "java.util.EnumMap$EnumMapIterator" int))) 
                                      (26 (aaload)) 
                                      (27 (ifnonnull 43)) ;;to TAG_0
                                      (30 (aload_0)) 
                                      (31 (dup)) 
                                      (32 (getfield (fieldCP "index" "java.util.EnumMap$EnumMapIterator" int))) 
                                      (35 (iconst_1)) 
                                      (36 (iadd)) 
                                      (37 (putfield (fieldCP "index" "java.util.EnumMap$EnumMapIterator" int))) 
                                      (40 (goto 0)) ;;to TAG_1
                                      (43 (aload_0)) ;;at TAG_0
                                      (44 (getfield (fieldCP "index" "java.util.EnumMap$EnumMapIterator" int))) 
                                      (47 (aload_0)) 
                                      (48 (getfield (fieldCP "this$0" "java.util.EnumMap$EnumMapIterator" (class "java.util.EnumMap")))) 
                                      (51 (invokestatic (methodCP "access$600" "java.util.EnumMap" ((class "java.util.EnumMap")) (array (class "java.lang.Object"))))) 
                                      (54 (arraylength)) 
                                      (55 (if_icmpeq 62))  ;;to TAG_2
                                      (58 (iconst_1)) 
                                      (59 (goto 63)) ;;to TAG_3
                                      (62 (iconst_0)) ;;at TAG_2
                                      (63 (ireturn)) ;;at TAG_3
                                      (endofcode 64))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "checkLastReturnedIndex" "java.util.EnumMap$EnumMapIterator" () void))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "this$0" "java.util.EnumMap$EnumMapIterator" (class "java.util.EnumMap")))) 
                                      (8 (invokestatic (methodCP "access$600" "java.util.EnumMap" ((class "java.util.EnumMap")) (array (class "java.lang.Object"))))) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "lastReturnedIndex" "java.util.EnumMap$EnumMapIterator" int))) 
                                      (15 (aaload)) 
                                      (16 (ifnull 40))  ;;to TAG_0
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "this$0" "java.util.EnumMap$EnumMapIterator" (class "java.util.EnumMap")))) 
                                      (23 (invokestatic (methodCP "access$600" "java.util.EnumMap" ((class "java.util.EnumMap")) (array (class "java.lang.Object"))))) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "lastReturnedIndex" "java.util.EnumMap$EnumMapIterator" int))) 
                                      (30 (aconst_null)) 
                                      (31 (aastore)) 
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "this$0" "java.util.EnumMap$EnumMapIterator" (class "java.util.EnumMap")))) 
                                      (36 (invokestatic (methodCP "access$210" "java.util.EnumMap" ((class "java.util.EnumMap")) int))) 
                                      (39 (pop)) 
                                      (40 (aload_0)) ;;at TAG_0
                                      (41 (iconst_m1)) 
                                      (42 (putfield (fieldCP "lastReturnedIndex" "java.util.EnumMap$EnumMapIterator" int))) 
                                      (45 (return)) 
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap )))
                        (method "checkLastReturnedIndex"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "lastReturnedIndex" "java.util.EnumMap$EnumMapIterator" int))) 
                                      (4 (ifge 15))  ;;to TAG_0
                                      (7 (new (class "java.lang.IllegalStateException"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" () void))) 
                                      (14 (athrow)) 
                                      (15 (return)) ;;at TAG_0
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.EnumMap") (class "java.util.EnumMap$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.EnumMap$EnumMapIterator" ((class "java.util.EnumMap")) void)))
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


(defconst *EnumMap$EnumMapIterator-class-table*
  (make-static-class-decls 
   *java.util.EnumMap$EnumMapIterator*))

(defconst *package-name-map* 
  ("java.util.EnumMap$EnumMapIterator" . "java.util"))
