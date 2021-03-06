; PersistentTreeMap$NodeIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.PersistentTreeMap$NodeIterator*
 (make-class-def
      '(class "clojure.lang.PersistentTreeMap$NodeIterator"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "stack" (class "java.util.Stack") (accessflags  *class* ) -1)
                        (field "asc" boolean (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.PersistentTreeMap$Node") boolean)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "java.util.Stack")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "java.util.Stack" () void)))
                                      (12 (putfield (fieldCP "stack" "clojure.lang.PersistentTreeMap$NodeIterator" (class "java.util.Stack"))))
                                      (15 (aload_0))
                                      (16 (iload_2))
                                      (17 (putfield (fieldCP "asc" "clojure.lang.PersistentTreeMap$NodeIterator" boolean)))
                                      (20 (aload_0))
                                      (21 (aload_1))
                                      (22 (invokevirtual
					(methodCP "push" "clojure.lang.PersistentTreeMap$NodeIterator" ((class "clojure.lang.PersistentTreeMap$Node")) void)))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "push"
                              (parameters (class "clojure.lang.PersistentTreeMap$Node"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_1)) ;;at TAG_3
                                      (1 (ifnull 35)) ;;to TAG_0
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "stack" "clojure.lang.PersistentTreeMap$NodeIterator" (class "java.util.Stack")))) 
                                      (8 (aload_1)) 
                                      (9 (invokevirtual (methodCP "push" "java.util.Stack" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (12 (pop)) 
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "asc" "clojure.lang.PersistentTreeMap$NodeIterator" boolean))) 
                                      (17 (ifeq 27)) ;;to TAG_1
                                      (20 (aload_1)) 
                                      (21 (invokevirtual (methodCP "left" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (24 (goto 31))  ;;to TAG_2
                                      (27 (aload_1)) ;;at TAG_1
                                      (28 (invokevirtual (methodCP "right" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (31 (astore_1)) ;;at TAG_2
                                      (32 (goto 0)) ;;to TAG_3
                                      (35 (return)) ;;at TAG_0
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "stack" "clojure.lang.PersistentTreeMap$NodeIterator" (class "java.util.Stack")))) 
                                      (4 (invokevirtual (methodCP "isEmpty" "java.util.Stack" () boolean))) 
                                      (7 (ifne 14))  ;;to TAG_0
                                      (10 (iconst_1)) 
                                      (11 (goto 15)) ;;to TAG_1
                                      (14 (iconst_0)) ;;at TAG_0
                                      (15 (ireturn)) ;;at TAG_1
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "stack" "clojure.lang.PersistentTreeMap$NodeIterator" (class "java.util.Stack")))) 
                                      (4 (invokevirtual (methodCP "pop" "java.util.Stack" () (class "java.lang.Object")))) 
                                      (7 (checkcast (class "clojure.lang.PersistentTreeMap$Node"))) 
                                      (10 (astore_1)) 
                                      (11 (aload_0)) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "asc" "clojure.lang.PersistentTreeMap$NodeIterator" boolean))) 
                                      (16 (ifeq 26))  ;;to TAG_0
                                      (19 (aload_1)) 
                                      (20 (invokevirtual (methodCP "right" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (23 (goto 30)) ;;to TAG_1
                                      (26 (aload_1)) ;;at TAG_0
                                      (27 (invokevirtual (methodCP "left" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (30 (invokevirtual (methodCP "push" "clojure.lang.PersistentTreeMap$NodeIterator" ((class "clojure.lang.PersistentTreeMap$Node")) void))) ;;at TAG_1
                                      (33 (aload_1)) 
                                      (34 (areturn)) 
                                      (endofcode 35))
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
                                   (StackMap ))))
            (interfaces "java.util.Iterator")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *PersistentTreeMap$NodeIterator-class-table*
  (make-static-class-decls 
   *clojure.lang.PersistentTreeMap$NodeIterator*))

(defconst *package-name-map* 
  ("clojure.lang.PersistentTreeMap$NodeIterator" . "clojure.lang"))

