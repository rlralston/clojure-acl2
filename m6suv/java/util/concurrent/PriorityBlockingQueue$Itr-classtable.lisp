; PriorityBlockingQueue$Itr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.PriorityBlockingQueue$Itr*
 (make-class-def
      '(class "java.util.concurrent.PriorityBlockingQueue$Itr"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "array" (array (class "java.lang.Object")) (accessflags  *class*  *final* ) -1)
                        (field "cursor" int (accessflags  *class* ) -1)
                        (field "lastRet" int (accessflags  *class* ) -1)
                        (field "this$0" (class "java.util.concurrent.PriorityBlockingQueue") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.concurrent.PriorityBlockingQueue") (array (class "java.lang.Object")))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.concurrent.PriorityBlockingQueue$Itr" (class "java.util.concurrent.PriorityBlockingQueue"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (iconst_m1))
                                      (11 (putfield (fieldCP "lastRet" "java.util.concurrent.PriorityBlockingQueue$Itr" int)))
                                      (14 (aload_0))
                                      (15 (aload_2))
                                      (16 (putfield (fieldCP "array" "java.util.concurrent.PriorityBlockingQueue$Itr" (array (class "java.lang.Object")))))
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
                                      (1 (getfield (fieldCP "cursor" "java.util.concurrent.PriorityBlockingQueue$Itr" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "array" "java.util.concurrent.PriorityBlockingQueue$Itr" (array (class "java.lang.Object"))))) 
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
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 45)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "cursor" "java.util.concurrent.PriorityBlockingQueue$Itr" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "array" "java.util.concurrent.PriorityBlockingQueue$Itr" (array (class "java.lang.Object"))))) 
                                      (8 (arraylength)) 
                                      (9 (if_icmplt 20))  ;;to TAG_0
                                      (12 (new (class "java.util.NoSuchElementException"))) 
                                      (15 (dup)) 
                                      (16 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (19 (athrow)) 
                                      (20 (aload_0)) ;;at TAG_0
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "cursor" "java.util.concurrent.PriorityBlockingQueue$Itr" int))) 
                                      (25 (putfield (fieldCP "lastRet" "java.util.concurrent.PriorityBlockingQueue$Itr" int))) 
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "array" "java.util.concurrent.PriorityBlockingQueue$Itr" (array (class "java.lang.Object"))))) 
                                      (32 (aload_0)) 
                                      (33 (dup)) 
                                      (34 (getfield (fieldCP "cursor" "java.util.concurrent.PriorityBlockingQueue$Itr" int))) 
                                      (37 (dup_x1)) 
                                      (38 (iconst_1)) 
                                      (39 (iadd)) 
                                      (40 (putfield (fieldCP "cursor" "java.util.concurrent.PriorityBlockingQueue$Itr" int))) 
                                      (43 (aaload)) 
                                      (44 (areturn)) 
                                      (endofcode 45))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 37)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "lastRet" "java.util.concurrent.PriorityBlockingQueue$Itr" int))) 
                                      (4 (ifge 15))  ;;to TAG_0
                                      (7 (new (class "java.lang.IllegalStateException"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" () void))) 
                                      (14 (athrow)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "this$0" "java.util.concurrent.PriorityBlockingQueue$Itr" (class "java.util.concurrent.PriorityBlockingQueue")))) 
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "array" "java.util.concurrent.PriorityBlockingQueue$Itr" (array (class "java.lang.Object"))))) 
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "lastRet" "java.util.concurrent.PriorityBlockingQueue$Itr" int))) 
                                      (27 (aaload)) 
                                      (28 (invokestatic (methodCP "access$000" "java.util.concurrent.PriorityBlockingQueue" ((class "java.util.concurrent.PriorityBlockingQueue") (class "java.lang.Object")) void))) 
                                      (31 (aload_0)) 
                                      (32 (iconst_m1)) 
                                      (33 (putfield (fieldCP "lastRet" "java.util.concurrent.PriorityBlockingQueue$Itr" int))) 
                                      (36 (return)) 
                                      (endofcode 37))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Iterator")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *PriorityBlockingQueue$Itr-class-table*
  (make-static-class-decls 
   *java.util.concurrent.PriorityBlockingQueue$Itr*))

(defconst *package-name-map* 
  ("java.util.concurrent.PriorityBlockingQueue$Itr" . "java.util.concurrent"))
