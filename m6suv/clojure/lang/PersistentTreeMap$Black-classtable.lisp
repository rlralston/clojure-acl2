; PersistentTreeMap$Black-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.PersistentTreeMap$Black*
 (make-class-def
      '(class "clojure.lang.PersistentTreeMap$Black"
            "clojure.lang.PersistentTreeMap$Node"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "clojure.lang.PersistentTreeMap$Node" ((class "java.lang.Object")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addLeft"
                              (parameters (class "clojure.lang.PersistentTreeMap$Node"))
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_0))
                                      (2 (invokevirtual
					(methodCP "balanceLeft" "clojure.lang.PersistentTreeMap$Node" ((class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addRight"
                              (parameters (class "clojure.lang.PersistentTreeMap$Node"))
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_0))
                                      (2 (invokevirtual
					(methodCP "balanceRight" "clojure.lang.PersistentTreeMap$Node" ((class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "removeLeft"
                              (parameters (class "clojure.lang.PersistentTreeMap$Node"))
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$Black" (class "java.lang.Object"))))
                                      (4 (aload_0))
                                      (5 (invokevirtual
					(methodCP "val" "clojure.lang.PersistentTreeMap$Black" () (class "java.lang.Object"))))
                                      (8 (aload_1))
                                      (9 (aload_0))
                                      (10 (invokevirtual
					(methodCP "right" "clojure.lang.PersistentTreeMap$Black" () (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (13 (invokestatic
					(methodCP "balanceLeftDel" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "removeRight"
                              (parameters (class "clojure.lang.PersistentTreeMap$Node"))
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$Black" (class "java.lang.Object"))))
                                      (4 (aload_0))
                                      (5 (invokevirtual
					(methodCP "val" "clojure.lang.PersistentTreeMap$Black" () (class "java.lang.Object"))))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "left" "clojure.lang.PersistentTreeMap$Black" () (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (12 (aload_1))
                                      (13 (invokestatic
					(methodCP "balanceRightDel" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "blacken"
                              (parameters )
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "redden"
                              (parameters )
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.PersistentTreeMap$Red")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$Black" (class "java.lang.Object"))))
                                      (8 (invokespecial
					(methodCP "<init>" "clojure.lang.PersistentTreeMap$Red" ((class "java.lang.Object")) void)))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "replace"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node"))
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_2))
                                      (2 (aload_3))
                                      (3 (aload 4))
                                      (5 (invokestatic
					(methodCP "black" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Black"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *PersistentTreeMap$Black-class-table*
  (make-static-class-decls 
   *clojure.lang.PersistentTreeMap$Black*))

(defconst *package-name-map* 
  ("clojure.lang.PersistentTreeMap$Black" . "clojure.lang"))
