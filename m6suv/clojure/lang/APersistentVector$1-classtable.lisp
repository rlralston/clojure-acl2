; APersistentVector$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.APersistentVector$1*
 (make-class-def
      '(class "clojure.lang.APersistentVector$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "nexti" int (accessflags  *class* ) -1)
                        (field "val$index" int (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "clojure.lang.APersistentVector") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.APersistentVector") int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "clojure.lang.APersistentVector$1" (class "clojure.lang.APersistentVector"))))
                                      (5 (aload_0))
                                      (6 (iload_2))
                                      (7 (putfield (fieldCP "val$index" "clojure.lang.APersistentVector$1" int)))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (aload_0))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "val$index" "clojure.lang.APersistentVector$1" int)))
                                      (19 (putfield (fieldCP "nexti" "clojure.lang.APersistentVector$1" int)))
                                      (22 (return))
                                      (endofcode 23))
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
                                      (1 (getfield (fieldCP "nexti" "clojure.lang.APersistentVector$1" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "this$0" "clojure.lang.APersistentVector$1" (class "clojure.lang.APersistentVector")))) 
                                      (8 (invokevirtual (methodCP "count" "clojure.lang.APersistentVector" () int))) 
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
                                   (max_stack . 5) (max_locals . 1) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "clojure.lang.APersistentVector$1" (class "clojure.lang.APersistentVector"))))
                                      (4 (aload_0))
                                      (5 (dup))
                                      (6 (getfield (fieldCP "nexti" "clojure.lang.APersistentVector$1" int)))
                                      (9 (dup_x1))
                                      (10 (iconst_1))
                                      (11 (iadd))
                                      (12 (putfield (fieldCP "nexti" "clojure.lang.APersistentVector$1" int)))
                                      (15 (invokevirtual
					(methodCP "nth" "clojure.lang.APersistentVector" (int) (class "java.lang.Object"))))
                                      (18 (areturn))
                                      (endofcode 19))
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
                                      (1 (getfield (fieldCP "nexti" "clojure.lang.APersistentVector$1" int))) 
                                      (4 (ifle 11))  ;;to TAG_0
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
                                   (max_stack . 4) (max_locals . 1) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "clojure.lang.APersistentVector$1" (class "clojure.lang.APersistentVector"))))
                                      (4 (aload_0))
                                      (5 (dup))
                                      (6 (getfield (fieldCP "nexti" "clojure.lang.APersistentVector$1" int)))
                                      (9 (iconst_1))
                                      (10 (isub))
                                      (11 (dup_x1))
                                      (12 (putfield (fieldCP "nexti" "clojure.lang.APersistentVector$1" int)))
                                      (15 (invokevirtual
					(methodCP "nth" "clojure.lang.APersistentVector" (int) (class "java.lang.Object"))))
                                      (18 (areturn))
                                      (endofcode 19))
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
                                      (1 (getfield (fieldCP "nexti" "clojure.lang.APersistentVector$1" int)))
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
                                      (1 (getfield (fieldCP "nexti" "clojure.lang.APersistentVector$1" int)))
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
                                   (StackMap ))))
            (interfaces "java.util.ListIterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *APersistentVector$1-class-table*
  (make-static-class-decls 
   *clojure.lang.APersistentVector$1*))

(defconst *package-name-map* 
  ("clojure.lang.APersistentVector$1" . "clojure.lang"))

