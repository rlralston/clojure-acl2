; PersistentTreeMap$RedBranch-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.PersistentTreeMap$RedBranch*
 (make-class-def
      '(class "clojure.lang.PersistentTreeMap$RedBranch"
            "clojure.lang.PersistentTreeMap$Red"
            (constant_pool)
            (fields
                        (field "left" (class "clojure.lang.PersistentTreeMap$Node") (accessflags  *class*  *final* ) -1)
                        (field "right" (class "clojure.lang.PersistentTreeMap$Node") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "clojure.lang.PersistentTreeMap$Red" ((class "java.lang.Object")) void)))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (10 (aload_0))
                                      (11 (aload_3))
                                      (12 (putfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "left"
                              (parameters )
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "right"
                              (parameters )
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "balanceLeft"
                              (parameters (class "clojure.lang.PersistentTreeMap$Node"))
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 7) (max_locals . 2) (code_length . 126)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (4 (instanceof (class "clojure.lang.PersistentTreeMap$Red"))) 
                                      (7 (ifeq 48))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$RedBranch" (class "java.lang.Object")))) 
                                      (14 (aload_0)) 
                                      (15 (invokevirtual (methodCP "val" "clojure.lang.PersistentTreeMap$RedBranch" () (class "java.lang.Object")))) 
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (22 (invokevirtual (methodCP "blacken" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (25 (aload_1)) 
                                      (26 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$Node" (class "java.lang.Object")))) 
                                      (29 (aload_1)) 
                                      (30 (invokevirtual (methodCP "val" "clojure.lang.PersistentTreeMap$Node" () (class "java.lang.Object")))) 
                                      (33 (aload_0)) 
                                      (34 (getfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (37 (aload_1)) 
                                      (38 (invokevirtual (methodCP "right" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (41 (invokestatic (methodCP "black" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Black")))) 
                                      (44 (invokestatic (methodCP "red" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Red")))) 
                                      (47 (areturn)) 
                                      (48 (aload_0)) ;;at TAG_0
                                      (49 (getfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (52 (instanceof (class "clojure.lang.PersistentTreeMap$Red"))) 
                                      (55 (ifeq 120)) ;;to TAG_1
                                      (58 (aload_0)) 
                                      (59 (getfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (62 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$Node" (class "java.lang.Object")))) 
                                      (65 (aload_0)) 
                                      (66 (getfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (69 (invokevirtual (methodCP "val" "clojure.lang.PersistentTreeMap$Node" () (class "java.lang.Object")))) 
                                      (72 (aload_0)) 
                                      (73 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$RedBranch" (class "java.lang.Object")))) 
                                      (76 (aload_0)) 
                                      (77 (invokevirtual (methodCP "val" "clojure.lang.PersistentTreeMap$RedBranch" () (class "java.lang.Object")))) 
                                      (80 (aload_0)) 
                                      (81 (getfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (84 (aload_0)) 
                                      (85 (getfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (88 (invokevirtual (methodCP "left" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (91 (invokestatic (methodCP "black" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Black")))) 
                                      (94 (aload_1)) 
                                      (95 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$Node" (class "java.lang.Object")))) 
                                      (98 (aload_1)) 
                                      (99 (invokevirtual (methodCP "val" "clojure.lang.PersistentTreeMap$Node" () (class "java.lang.Object")))) 
                                      (102 (aload_0)) 
                                      (103 (getfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (106 (invokevirtual (methodCP "right" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (109 (aload_1)) 
                                      (110 (invokevirtual (methodCP "right" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (113 (invokestatic (methodCP "black" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Black")))) 
                                      (116 (invokestatic (methodCP "red" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Red")))) 
                                      (119 (areturn)) 
                                      (120 (aload_0)) ;;at TAG_1
                                      (121 (aload_1)) 
                                      (122 (invokespecial (methodCP "balanceLeft" "clojure.lang.PersistentTreeMap$Red" ((class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (125 (areturn)) 
                                      (endofcode 126))
                                   (Exceptions )
                                   (StackMap )))
                        (method "balanceRight"
                              (parameters (class "clojure.lang.PersistentTreeMap$Node"))
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 7) (max_locals . 2) (code_length . 126)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (4 (instanceof (class "clojure.lang.PersistentTreeMap$Red"))) 
                                      (7 (ifeq 48))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$RedBranch" (class "java.lang.Object")))) 
                                      (14 (aload_0)) 
                                      (15 (invokevirtual (methodCP "val" "clojure.lang.PersistentTreeMap$RedBranch" () (class "java.lang.Object")))) 
                                      (18 (aload_1)) 
                                      (19 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$Node" (class "java.lang.Object")))) 
                                      (22 (aload_1)) 
                                      (23 (invokevirtual (methodCP "val" "clojure.lang.PersistentTreeMap$Node" () (class "java.lang.Object")))) 
                                      (26 (aload_1)) 
                                      (27 (invokevirtual (methodCP "left" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (34 (invokestatic (methodCP "black" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Black")))) 
                                      (37 (aload_0)) 
                                      (38 (getfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (41 (invokevirtual (methodCP "blacken" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (44 (invokestatic (methodCP "red" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Red")))) 
                                      (47 (areturn)) 
                                      (48 (aload_0)) ;;at TAG_0
                                      (49 (getfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (52 (instanceof (class "clojure.lang.PersistentTreeMap$Red"))) 
                                      (55 (ifeq 120)) ;;to TAG_1
                                      (58 (aload_0)) 
                                      (59 (getfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (62 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$Node" (class "java.lang.Object")))) 
                                      (65 (aload_0)) 
                                      (66 (getfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (69 (invokevirtual (methodCP "val" "clojure.lang.PersistentTreeMap$Node" () (class "java.lang.Object")))) 
                                      (72 (aload_1)) 
                                      (73 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$Node" (class "java.lang.Object")))) 
                                      (76 (aload_1)) 
                                      (77 (invokevirtual (methodCP "val" "clojure.lang.PersistentTreeMap$Node" () (class "java.lang.Object")))) 
                                      (80 (aload_1)) 
                                      (81 (invokevirtual (methodCP "left" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (84 (aload_0)) 
                                      (85 (getfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (88 (invokevirtual (methodCP "left" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (91 (invokestatic (methodCP "black" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Black")))) 
                                      (94 (aload_0)) 
                                      (95 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$RedBranch" (class "java.lang.Object")))) 
                                      (98 (aload_0)) 
                                      (99 (invokevirtual (methodCP "val" "clojure.lang.PersistentTreeMap$RedBranch" () (class "java.lang.Object")))) 
                                      (102 (aload_0)) 
                                      (103 (getfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (106 (invokevirtual (methodCP "right" "clojure.lang.PersistentTreeMap$Node" () (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (109 (aload_0)) 
                                      (110 (getfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (113 (invokestatic (methodCP "black" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Black")))) 
                                      (116 (invokestatic (methodCP "red" "clojure.lang.PersistentTreeMap" ((class "java.lang.Object") (class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Red")))) 
                                      (119 (areturn)) 
                                      (120 (aload_0)) ;;at TAG_1
                                      (121 (aload_1)) 
                                      (122 (invokespecial (methodCP "balanceRight" "clojure.lang.PersistentTreeMap$Red" ((class "clojure.lang.PersistentTreeMap$Node")) (class "clojure.lang.PersistentTreeMap$Node")))) 
                                      (125 (areturn)) 
                                      (endofcode 126))
                                   (Exceptions )
                                   (StackMap )))
                        (method "blacken"
                              (parameters )
                              (returntype . (class "clojure.lang.PersistentTreeMap$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.PersistentTreeMap$BlackBranch")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "key" "clojure.lang.PersistentTreeMap$RedBranch" (class "java.lang.Object"))))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "left" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "right" "clojure.lang.PersistentTreeMap$RedBranch" (class "clojure.lang.PersistentTreeMap$Node"))))
                                      (16 (invokespecial
					(methodCP "<init>" "clojure.lang.PersistentTreeMap$BlackBranch" ((class "java.lang.Object") (class "clojure.lang.PersistentTreeMap$Node") (class "clojure.lang.PersistentTreeMap$Node")) void)))
                                      (19 (areturn))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *PersistentTreeMap$RedBranch-class-table*
  (make-static-class-decls 
   *clojure.lang.PersistentTreeMap$RedBranch*))

(defconst *package-name-map* 
  ("clojure.lang.PersistentTreeMap$RedBranch" . "clojure.lang"))
