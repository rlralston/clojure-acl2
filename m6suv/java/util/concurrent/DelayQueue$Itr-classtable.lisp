; DelayQueue$Itr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.DelayQueue$Itr*
 (make-class-def
      '(class "java.util.concurrent.DelayQueue$Itr"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "array" (array (class "java.lang.Object")) (accessflags  *class*  *final* ) -1)
                        (field "cursor" int (accessflags  *class* ) -1)
                        (field "lastRet" int (accessflags  *class* ) -1)
                        (field "this$0" (class "java.util.concurrent.DelayQueue") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.concurrent.DelayQueue") (array (class "java.lang.Object")))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.concurrent.DelayQueue$Itr" (class "java.util.concurrent.DelayQueue"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (iconst_m1))
                                      (11 (putfield (fieldCP "lastRet" "java.util.concurrent.DelayQueue$Itr" int)))
                                      (14 (aload_0))
                                      (15 (aload_2))
                                      (16 (putfield (fieldCP "array" "java.util.concurrent.DelayQueue$Itr" (array (class "java.lang.Object")))))
                                      (19 (return))
                                      (endofcode 20))
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
                                      (1 (getfield (fieldCP "cursor" "java.util.concurrent.DelayQueue$Itr" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "array" "java.util.concurrent.DelayQueue$Itr" (array (class "java.lang.Object"))))) 
                                      (8 (arraylength)) 
                                      (9 (if_icmpge 16))  ;;to TAG_0
                                      (12 (iconst_1)) 
                                      (13 (goto 17)) ;;to TAG_1
                                      (16 (iconst_0)) ;;at TAG_0
                                      (17 (ireturn)) ;;at TAG_1
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.util.concurrent.Delayed"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 48)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "cursor" "java.util.concurrent.DelayQueue$Itr" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "array" "java.util.concurrent.DelayQueue$Itr" (array (class "java.lang.Object"))))) 
                                      (8 (arraylength)) 
                                      (9 (if_icmplt 20))  ;;to TAG_0
                                      (12 (new (class "java.util.NoSuchElementException"))) 
                                      (15 (dup)) 
                                      (16 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (19 (athrow)) 
                                      (20 (aload_0)) ;;at TAG_0
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "cursor" "java.util.concurrent.DelayQueue$Itr" int))) 
                                      (25 (putfield (fieldCP "lastRet" "java.util.concurrent.DelayQueue$Itr" int))) 
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "array" "java.util.concurrent.DelayQueue$Itr" (array (class "java.lang.Object"))))) 
                                      (32 (aload_0)) 
                                      (33 (dup)) 
                                      (34 (getfield (fieldCP "cursor" "java.util.concurrent.DelayQueue$Itr" int))) 
                                      (37 (dup_x1)) 
                                      (38 (iconst_1)) 
                                      (39 (iadd)) 
                                      (40 (putfield (fieldCP "cursor" "java.util.concurrent.DelayQueue$Itr" int))) 
                                      (43 (aaload)) 
                                      (44 (checkcast (class "java.util.concurrent.Delayed"))) 
                                      (47 (areturn)) 
                                      (endofcode 48))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 114)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "lastRet" "java.util.concurrent.DelayQueue$Itr" int))) 
                                      (4 (ifge 15)) ;;to TAG_0
                                      (7 (new (class "java.lang.IllegalStateException"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" () void))) 
                                      (14 (athrow)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "array" "java.util.concurrent.DelayQueue$Itr" (array (class "java.lang.Object"))))) 
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "lastRet" "java.util.concurrent.DelayQueue$Itr" int))) 
                                      (23 (aaload)) 
                                      (24 (astore_1)) 
                                      (25 (aload_0)) 
                                      (26 (iconst_m1)) 
                                      (27 (putfield (fieldCP "lastRet" "java.util.concurrent.DelayQueue$Itr" int))) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "this$0" "java.util.concurrent.DelayQueue$Itr" (class "java.util.concurrent.DelayQueue")))) 
                                      (34 (invokestatic (methodCP "access$000" "java.util.concurrent.DelayQueue" ((class "java.util.concurrent.DelayQueue")) (class "java.util.concurrent.locks.ReentrantLock")))) 
                                      (37 (invokevirtual (methodCP "lock" "java.util.concurrent.locks.ReentrantLock" () void))) 
                                      (40 (aload_0)) ;;at TAG_4
                                      (41 (getfield (fieldCP "this$0" "java.util.concurrent.DelayQueue$Itr" (class "java.util.concurrent.DelayQueue")))) 
                                      (44 (invokestatic (methodCP "access$100" "java.util.concurrent.DelayQueue" ((class "java.util.concurrent.DelayQueue")) (class "java.util.PriorityQueue")))) 
                                      (47 (invokevirtual (methodCP "iterator" "java.util.PriorityQueue" () (class "java.util.Iterator")))) 
                                      (50 (astore_2)) 
                                      (51 (aload_2)) ;;at TAG_2
                                      (52 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (57 (ifeq 87)) ;;to TAG_1
                                      (60 (aload_2)) 
                                      (61 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (66 (aload_1)) 
                                      (67 (if_acmpne 51))  ;;to TAG_2
                                      (70 (aload_2)) 
                                      (71 (invokeinterface (methodCP "remove" "java.util.Iterator" () void) 1)) 
                                      (76 (aload_0)) ;;at TAG_5
                                      (77 (getfield (fieldCP "this$0" "java.util.concurrent.DelayQueue$Itr" (class "java.util.concurrent.DelayQueue")))) 
                                      (80 (invokestatic (methodCP "access$000" "java.util.concurrent.DelayQueue" ((class "java.util.concurrent.DelayQueue")) (class "java.util.concurrent.locks.ReentrantLock")))) 
                                      (83 (invokevirtual (methodCP "unlock" "java.util.concurrent.locks.ReentrantLock" () void))) 
                                      (86 (return)) 
                                      (87 (aload_0)) ;;at TAG_1
                                      (88 (getfield (fieldCP "this$0" "java.util.concurrent.DelayQueue$Itr" (class "java.util.concurrent.DelayQueue")))) 
                                      (91 (invokestatic (methodCP "access$000" "java.util.concurrent.DelayQueue" ((class "java.util.concurrent.DelayQueue")) (class "java.util.concurrent.locks.ReentrantLock")))) 
                                      (94 (invokevirtual (methodCP "unlock" "java.util.concurrent.locks.ReentrantLock" () void))) 
                                      (97 (goto 113)) ;;to TAG_3
                                      (100 (astore_3)) ;;at TAG_6
                                      (101 (aload_0)) ;;at TAG_7
                                      (102 (getfield (fieldCP "this$0" "java.util.concurrent.DelayQueue$Itr" (class "java.util.concurrent.DelayQueue")))) 
                                      (105 (invokestatic (methodCP "access$000" "java.util.concurrent.DelayQueue" ((class "java.util.concurrent.DelayQueue")) (class "java.util.concurrent.locks.ReentrantLock")))) 
                                      (108 (invokevirtual (methodCP "unlock" "java.util.concurrent.locks.ReentrantLock" () void))) 
                                      (111 (aload_3)) 
                                      (112 (athrow)) 
                                      (113 (return)) ;;at TAG_3
                                      (endofcode 114))
                                   (Exceptions 
                                     (handler 40 76  100 (class "java.lang.Throwable"))
                                     (handler 100 101  100 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "next" "java.util.concurrent.DelayQueue$Itr" () (class "java.util.concurrent.Delayed"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Iterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *DelayQueue$Itr-class-table*
  (make-static-class-decls 
   *java.util.concurrent.DelayQueue$Itr*))

(defconst *package-name-map* 
  ("java.util.concurrent.DelayQueue$Itr" . "java.util.concurrent"))

