; ObjectOutputStream$ReplaceTable-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.ObjectOutputStream$ReplaceTable*
 (make-class-def
      '(class "java.io.ObjectOutputStream$ReplaceTable"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "htab" (class "java.io.ObjectOutputStream$HandleTable") (accessflags  *class*  *final*  *private* ) -1)
                        (field "reps" (array (class "java.lang.Object")) (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters int float)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "java.io.ObjectOutputStream$HandleTable")))
                                      (8 (dup))
                                      (9 (iload_1))
                                      (10 (fload_2))
                                      (11 (invokespecial
					(methodCP "<init>" "java.io.ObjectOutputStream$HandleTable" (int float) void)))
                                      (14 (putfield (fieldCP "htab" "java.io.ObjectOutputStream$ReplaceTable" (class "java.io.ObjectOutputStream$HandleTable"))))
                                      (17 (aload_0))
                                      (18 (iload_1))
                                      (19 (anewarray (class "java.lang.Object")))
                                      (22 (putfield (fieldCP "reps" "java.io.ObjectOutputStream$ReplaceTable" (array (class "java.lang.Object")))))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "assign"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "htab" "java.io.ObjectOutputStream$ReplaceTable" (class "java.io.ObjectOutputStream$HandleTable")))) 
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "assign" "java.io.ObjectOutputStream$HandleTable" ((class "java.lang.Object")) int))) 
                                      (8 (istore_3)) 
                                      (9 (iload_3)) ;;at TAG_1
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "reps" "java.io.ObjectOutputStream$ReplaceTable" (array (class "java.lang.Object"))))) 
                                      (14 (arraylength)) 
                                      (15 (if_icmplt 25))  ;;to TAG_0
                                      (18 (aload_0)) 
                                      (19 (invokespecial (methodCP "grow" "java.io.ObjectOutputStream$ReplaceTable" () void))) 
                                      (22 (goto 9)) ;;to TAG_1
                                      (25 (aload_0)) ;;at TAG_0
                                      (26 (getfield (fieldCP "reps" "java.io.ObjectOutputStream$ReplaceTable" (array (class "java.lang.Object"))))) 
                                      (29 (iload_3)) 
                                      (30 (aload_2)) 
                                      (31 (aastore)) 
                                      (32 (return)) 
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap )))
                        (method "lookup"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "htab" "java.io.ObjectOutputStream$ReplaceTable" (class "java.io.ObjectOutputStream$HandleTable")))) 
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "lookup" "java.io.ObjectOutputStream$HandleTable" ((class "java.lang.Object")) int))) 
                                      (8 (istore_2)) 
                                      (9 (iload_2)) 
                                      (10 (iflt 22))  ;;to TAG_0
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "reps" "java.io.ObjectOutputStream$ReplaceTable" (array (class "java.lang.Object"))))) 
                                      (17 (iload_2)) 
                                      (18 (aaload)) 
                                      (19 (goto 23)) ;;to TAG_1
                                      (22 (aload_1)) ;;at TAG_0
                                      (23 (areturn)) ;;at TAG_1
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "reps" "java.io.ObjectOutputStream$ReplaceTable" (array (class "java.lang.Object")))))
                                      (4 (iconst_0))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "htab" "java.io.ObjectOutputStream$ReplaceTable" (class "java.io.ObjectOutputStream$HandleTable"))))
                                      (9 (invokevirtual
					(methodCP "size" "java.io.ObjectOutputStream$HandleTable" () int)))
                                      (12 (aconst_null))
                                      (13 (invokestatic
					(methodCP "fill" "java.util.Arrays" ((array (class "java.lang.Object")) int int (class "java.lang.Object")) void)))
                                      (16 (aload_0))
                                      (17 (getfield (fieldCP "htab" "java.io.ObjectOutputStream$ReplaceTable" (class "java.io.ObjectOutputStream$HandleTable"))))
                                      (20 (invokevirtual
					(methodCP "clear" "java.io.ObjectOutputStream$HandleTable" () void)))
                                      (23 (return))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "htab" "java.io.ObjectOutputStream$ReplaceTable" (class "java.io.ObjectOutputStream$HandleTable"))))
                                      (4 (invokevirtual
					(methodCP "size" "java.io.ObjectOutputStream$HandleTable" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "grow"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 34)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "reps" "java.io.ObjectOutputStream$ReplaceTable" (array (class "java.lang.Object")))))
                                      (4 (arraylength))
                                      (5 (iconst_1))
                                      (6 (ishl))
                                      (7 (iconst_1))
                                      (8 (iadd))
                                      (9 (anewarray (class "java.lang.Object")))
                                      (12 (astore_1))
                                      (13 (aload_0))
                                      (14 (getfield (fieldCP "reps" "java.io.ObjectOutputStream$ReplaceTable" (array (class "java.lang.Object")))))
                                      (17 (iconst_0))
                                      (18 (aload_1))
                                      (19 (iconst_0))
                                      (20 (aload_0))
                                      (21 (getfield (fieldCP "reps" "java.io.ObjectOutputStream$ReplaceTable" (array (class "java.lang.Object")))))
                                      (24 (arraylength))
                                      (25 (invokestatic
					(methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void)))
                                      (28 (aload_0))
                                      (29 (aload_1))
                                      (30 (putfield (fieldCP "reps" "java.io.ObjectOutputStream$ReplaceTable" (array (class "java.lang.Object")))))
                                      (33 (return))
                                      (endofcode 34))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ObjectOutputStream$ReplaceTable-class-table*
  (make-static-class-decls 
   *java.io.ObjectOutputStream$ReplaceTable*))

(defconst *package-name-map* 
  ("java.io.ObjectOutputStream$ReplaceTable" . "java.io"))

