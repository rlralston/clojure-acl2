; Vector$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.Vector$1*
 (make-class-def
      '(class "java.util.Vector$1"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Vector Enumeration"))
            (fields
                        (field "count" int (accessflags  *class* ) -1)
                        (field "this$0" (class "java.util.Vector") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Vector"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.Vector$1" (class "java.util.Vector"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (iconst_0))
                                      (11 (putfield (fieldCP "count" "java.util.Vector$1" int)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasMoreElements"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "count" "java.util.Vector$1" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "this$0" "java.util.Vector$1" (class "java.util.Vector")))) 
                                      (8 (getfield (fieldCP "elementCount" "java.util.Vector" int))) 
                                      (11 (if_icmpge 18))  ;;to TAG_0
                                      (14 (iconst_1)) 
                                      (15 (goto 19)) ;;to TAG_1
                                      (18 (iconst_0)) ;;at TAG_0
                                      (19 (ireturn)) ;;at TAG_1
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextElement"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 62)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$0" "java.util.Vector$1" (class "java.util.Vector")))) 
                                      (4 (dup)) 
                                      (5 (astore_1)) 
                                      (6 (monitorenter)) 
                                      (7 (aload_0)) ;;at TAG_2
                                      (8 (getfield (fieldCP "count" "java.util.Vector$1" int))) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "this$0" "java.util.Vector$1" (class "java.util.Vector")))) 
                                      (15 (getfield (fieldCP "elementCount" "java.util.Vector" int))) 
                                      (18 (if_icmpge 42)) ;;to TAG_0
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "this$0" "java.util.Vector$1" (class "java.util.Vector")))) 
                                      (25 (aload_0)) 
                                      (26 (dup)) 
                                      (27 (getfield (fieldCP "count" "java.util.Vector$1" int))) 
                                      (30 (dup_x1)) 
                                      (31 (iconst_1)) 
                                      (32 (iadd)) 
                                      (33 (putfield (fieldCP "count" "java.util.Vector$1" int))) 
                                      (36 (invokevirtual (methodCP "elementData" "java.util.Vector" (int) (class "java.lang.Object")))) 
                                      (39 (aload_1)) 
                                      (40 (monitorexit)) 
                                      (41 (areturn)) ;;at TAG_3
                                      (42 (aload_1)) ;;at TAG_0
                                      (43 (monitorexit)) 
                                      (44 (goto 52)) ;;to TAG_1;;at TAG_5
                                      (47 (astore_2)) ;;at TAG_4
                                      (48 (aload_1)) 
                                      (49 (monitorexit)) 
                                      (50 (aload_2)) ;;at TAG_6
                                      (51 (athrow)) 
                                      (52 (new (class "java.util.NoSuchElementException"))) ;;at TAG_1
                                      (55 (dup)) 
                                      (56 (ldc 0)) ;;STRING:: "Vector Enumeration"
                                      (58 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" ((class "java.lang.String")) void))) 
                                      (61 (athrow)) 
                                      (endofcode 62))
                                   (Exceptions 
                                     (handler 7 41  47 (class "java.lang.Throwable"))
                                     (handler 42 44  47 (class "java.lang.Throwable"))
                                     (handler 47 50  47 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces "java.util.Enumeration")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Vector$1-class-table*
  (make-static-class-decls 
   *java.util.Vector$1*))

(defconst *package-name-map* 
  ("java.util.Vector$1" . "java.util"))
