; SubList$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.SubList$1*
 (make-class-def
      '(class "java.util.SubList$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "i" (class "java.util.ListIterator") (accessflags  *class*  *final*  *private* ) -1)
                        (field "val$index" int (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "java.util.SubList") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.SubList") int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 41)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList"))))
                                      (5 (aload_0))
                                      (6 (iload_2))
                                      (7 (putfield (fieldCP "val$index" "java.util.SubList$1" int)))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (aload_0))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList"))))
                                      (19 (invokestatic
					(methodCP "access$100" "java.util.SubList" ((class "java.util.SubList")) (class "java.util.AbstractList"))))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "val$index" "java.util.SubList$1" int)))
                                      (26 (aload_0))
                                      (27 (getfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList"))))
                                      (30 (invokestatic
					(methodCP "access$000" "java.util.SubList" ((class "java.util.SubList")) int)))
                                      (33 (iadd))
                                      (34 (invokevirtual
					(methodCP "listIterator" "java.util.AbstractList" (int) (class "java.util.ListIterator"))))
                                      (37 (putfield (fieldCP "i" "java.util.SubList$1" (class "java.util.ListIterator"))))
                                      (40 (return))
                                      (endofcode 41))
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
                                      (1 (invokevirtual (methodCP "nextIndex" "java.util.SubList$1" () int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList")))) 
                                      (8 (invokestatic (methodCP "access$200" "java.util.SubList" ((class "java.util.SubList")) int))) 
                                      (11 (if_icmpge 18))  ;;to TAG_0
                                      (14 (iconst_1)) 
                                      (15 (goto 19)) ;;to TAG_1
                                      (18 (iconst_0)) ;;at TAG_0
                                      (19 (ireturn)) ;;at TAG_1
                                      (endofcode 20))
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
                                      (1 (invokevirtual (methodCP "hasNext" "java.util.SubList$1" () boolean))) 
                                      (4 (ifeq 17))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "i" "java.util.SubList$1" (class "java.util.ListIterator")))) 
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
                                      (1 (invokevirtual (methodCP "previousIndex" "java.util.SubList$1" () int))) 
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
                                      (1 (invokevirtual (methodCP "hasPrevious" "java.util.SubList$1" () boolean))) 
                                      (4 (ifeq 17))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "i" "java.util.SubList$1" (class "java.util.ListIterator")))) 
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
                                   (max_stack . 2) (max_locals . 1) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.SubList$1" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "nextIndex" "java.util.ListIterator" () int) 1))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList"))))
                                      (13 (invokestatic
					(methodCP "access$000" "java.util.SubList" ((class "java.util.SubList")) int)))
                                      (16 (isub))
                                      (17 (ireturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "previousIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.SubList$1" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "previousIndex" "java.util.ListIterator" () int) 1))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList"))))
                                      (13 (invokestatic
					(methodCP "access$000" "java.util.SubList" ((class "java.util.SubList")) int)))
                                      (16 (isub))
                                      (17 (ireturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.SubList$1" (class "java.util.ListIterator"))))
                                      (4 (invokeinterface
					(methodCP "remove" "java.util.ListIterator" () void) 1))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList"))))
                                      (13 (aload_0))
                                      (14 (getfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList"))))
                                      (17 (invokestatic
					(methodCP "access$100" "java.util.SubList" ((class "java.util.SubList")) (class "java.util.AbstractList"))))
                                      (20 (getfield (fieldCP "modCount" "java.util.AbstractList" int)))
                                      (23 (putfield (fieldCP "modCount" "java.util.SubList" int)))
                                      (26 (aload_0))
                                      (27 (getfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList"))))
                                      (30 (invokestatic
					(methodCP "access$210" "java.util.SubList" ((class "java.util.SubList")) int)))
                                      (33 (pop))
                                      (34 (return))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "set"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.SubList$1" (class "java.util.ListIterator"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "set" "java.util.ListIterator" ((class "java.lang.Object")) void) 2))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.SubList$1" (class "java.util.ListIterator"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "add" "java.util.ListIterator" ((class "java.lang.Object")) void) 2))
                                      (10 (aload_0))
                                      (11 (getfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList"))))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList"))))
                                      (18 (invokestatic
					(methodCP "access$100" "java.util.SubList" ((class "java.util.SubList")) (class "java.util.AbstractList"))))
                                      (21 (getfield (fieldCP "modCount" "java.util.AbstractList" int)))
                                      (24 (putfield (fieldCP "modCount" "java.util.SubList" int)))
                                      (27 (aload_0))
                                      (28 (getfield (fieldCP "this$0" "java.util.SubList$1" (class "java.util.SubList"))))
                                      (31 (invokestatic
					(methodCP "access$208" "java.util.SubList" ((class "java.util.SubList")) int)))
                                      (34 (pop))
                                      (35 (return))
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.ListIterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *SubList$1-class-table*
  (make-static-class-decls 
   *java.util.SubList$1*))

(defconst *package-name-map* 
  ("java.util.SubList$1" . "java.util"))
