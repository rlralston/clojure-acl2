; List$AccessibleAWTList-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.List$AccessibleAWTList*
 (make-class-def
      '(class "java.awt.List$AccessibleAWTList"
            "java.awt.Component$AccessibleAWTComponent"
            (constant_pool
                        (LONG 7924617370136012829))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "this$0" (class "java.awt.List") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.List"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (invokespecial
					(methodCP "<init>" "java.awt.Component$AccessibleAWTComponent" ((class "java.awt.Component")) void)))
                                      (10 (aload_1))
                                      (11 (aload_0))
                                      (12 (invokevirtual
					(methodCP "addActionListener" "java.awt.List" ((class "java.awt.event.ActionListener")) void)))
                                      (15 (aload_1))
                                      (16 (aload_0))
                                      (17 (invokevirtual
					(methodCP "addItemListener" "java.awt.List" ((class "java.awt.event.ItemListener")) void)))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "actionPerformed"
                              (parameters (class "java.awt.event.ActionEvent"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 0) (max_locals . 2) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
                                   (Exceptions )
                                   (StackMap )))
                        (method "itemStateChanged"
                              (parameters (class "java.awt.event.ItemEvent"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 0) (max_locals . 2) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleStateSet"
                              (parameters )
                              (returntype . (class "javax.accessibility.AccessibleStateSet"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "getAccessibleStateSet" "java.awt.Component$AccessibleAWTComponent" () (class "javax.accessibility.AccessibleStateSet")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (9 (invokevirtual (methodCP "isMultipleMode" "java.awt.List" () boolean))) 
                                      (12 (ifeq 23))  ;;to TAG_0
                                      (15 (aload_1)) 
                                      (16 (getstatic (fieldCP "MULTISELECTABLE" "javax.accessibility.AccessibleState" (class "javax.accessibility.AccessibleState")))) 
                                      (19 (invokevirtual (methodCP "add" "javax.accessibility.AccessibleStateSet" ((class "javax.accessibility.AccessibleState")) boolean))) 
                                      (22 (pop)) 
                                      (23 (aload_1)) ;;at TAG_0
                                      (24 (areturn)) 
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleRole"
                              (parameters )
                              (returntype . (class "javax.accessibility.AccessibleRole"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "LIST" "javax.accessibility.AccessibleRole" (class "javax.accessibility.AccessibleRole"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleAt"
                              (parameters (class "java.awt.Point"))
                              (returntype . (class "javax.accessibility.Accessible"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleChildrenCount"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List"))))
                                      (4 (invokevirtual
					(methodCP "getItemCount" "java.awt.List" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleChild"
                              (parameters int)
                              (returntype . (class "javax.accessibility.Accessible"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (4 (dup)) 
                                      (5 (astore_2)) 
                                      (6 (monitorenter)) 
                                      (7 (iload_1)) ;;at TAG_1
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (12 (invokevirtual (methodCP "getItemCount" "java.awt.List" () int))) 
                                      (15 (if_icmplt 22)) ;;to TAG_0
                                      (18 (aconst_null)) 
                                      (19 (aload_2)) 
                                      (20 (monitorexit)) 
                                      (21 (areturn)) ;;at TAG_2
                                      (22 (new (class "java.awt.List$AccessibleAWTList$AccessibleAWTListChild"))) ;;at TAG_0
                                      (25 (dup)) 
                                      (26 (aload_0)) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (31 (iload_1)) 
                                      (32 (invokespecial (methodCP "<init>" "java.awt.List$AccessibleAWTList$AccessibleAWTListChild" ((class "java.awt.List$AccessibleAWTList") (class "java.awt.List") int) void))) 
                                      (35 (aload_2)) 
                                      (36 (monitorexit)) 
                                      (37 (areturn)) ;;at TAG_4
                                      (38 (astore_3)) ;;at TAG_3
                                      (39 (aload_2)) 
                                      (40 (monitorexit)) 
                                      (41 (aload_3)) ;;at TAG_5
                                      (42 (athrow)) 
                                      (endofcode 43))
                                   (Exceptions 
                                     (handler 7 21  38 (class "java.lang.Throwable"))
                                     (handler 22 37  38 (class "java.lang.Throwable"))
                                     (handler 38 41  38 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "getAccessibleSelection"
                              (parameters )
                              (returntype . (class "javax.accessibility.AccessibleSelection"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleSelectionCount"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List"))))
                                      (4 (invokevirtual
					(methodCP "getSelectedIndexes" "java.awt.List" () (array int))))
                                      (7 (arraylength))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleSelection"
                              (parameters int)
                              (returntype . (class "javax.accessibility.Accessible"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 48)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (4 (dup)) 
                                      (5 (astore_2)) 
                                      (6 (monitorenter)) 
                                      (7 (aload_0)) ;;at TAG_2
                                      (8 (invokevirtual (methodCP "getAccessibleSelectionCount" "java.awt.List$AccessibleAWTList" () int))) 
                                      (11 (istore_3)) 
                                      (12 (iload_1)) 
                                      (13 (iflt 21)) ;;to TAG_0
                                      (16 (iload_1)) 
                                      (17 (iload_3)) 
                                      (18 (if_icmplt 25)) ;;to TAG_1
                                      (21 (aconst_null)) ;;at TAG_0
                                      (22 (aload_2)) 
                                      (23 (monitorexit)) 
                                      (24 (areturn)) ;;at TAG_3
                                      (25 (aload_0)) ;;at TAG_1
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (30 (invokevirtual (methodCP "getSelectedIndexes" "java.awt.List" () (array int)))) 
                                      (33 (iload_1)) 
                                      (34 (iaload)) 
                                      (35 (invokevirtual (methodCP "getAccessibleChild" "java.awt.List$AccessibleAWTList" (int) (class "javax.accessibility.Accessible")))) 
                                      (38 (aload_2)) 
                                      (39 (monitorexit)) 
                                      (40 (areturn)) ;;at TAG_5
                                      (41 (astore 4)) ;;at TAG_4
                                      (43 (aload_2)) 
                                      (44 (monitorexit)) 
                                      (45 (aload 4)) ;;at TAG_6
                                      (47 (athrow)) 
                                      (endofcode 48))
                                   (Exceptions 
                                     (handler 7 24  41 (class "java.lang.Throwable"))
                                     (handler 25 40  41 (class "java.lang.Throwable"))
                                     (handler 41 45  41 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "isAccessibleChildSelected"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List"))))
                                      (4 (iload_1))
                                      (5 (invokevirtual
					(methodCP "isIndexSelected" "java.awt.List" (int) boolean)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addAccessibleSelection"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List"))))
                                      (4 (iload_1))
                                      (5 (invokevirtual
					(methodCP "select" "java.awt.List" (int) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "removeAccessibleSelection"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List"))))
                                      (4 (iload_1))
                                      (5 (invokevirtual
					(methodCP "deselect" "java.awt.List" (int) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clearAccessibleSelection"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 60)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (4 (dup)) 
                                      (5 (astore_1)) 
                                      (6 (monitorenter)) 
                                      (7 (aload_0)) ;;at TAG_4
                                      (8 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (11 (invokevirtual (methodCP "getSelectedIndexes" "java.awt.List" () (array int)))) 
                                      (14 (astore_2)) 
                                      (15 (aload_2)) 
                                      (16 (ifnonnull 22)) ;;to TAG_0
                                      (19 (aload_1)) 
                                      (20 (monitorexit)) 
                                      (21 (return)) ;;at TAG_5
                                      (22 (aload_2)) ;;at TAG_0
                                      (23 (arraylength)) 
                                      (24 (iconst_1)) 
                                      (25 (isub)) 
                                      (26 (istore_3)) 
                                      (27 (iload_3)) ;;at TAG_2
                                      (28 (iflt 47))  ;;to TAG_1
                                      (31 (aload_0)) 
                                      (32 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (35 (aload_2)) 
                                      (36 (iload_3)) 
                                      (37 (iaload)) 
                                      (38 (invokevirtual (methodCP "deselect" "java.awt.List" (int) void))) 
                                      (41 (iinc 3 -1)) 
                                      (44 (goto 27)) ;;to TAG_2
                                      (47 (aload_1)) ;;at TAG_1
                                      (48 (monitorexit)) 
                                      (49 (goto 59)) ;;to TAG_3;;at TAG_7
                                      (52 (astore 4)) ;;at TAG_6
                                      (54 (aload_1)) 
                                      (55 (monitorexit)) 
                                      (56 (aload 4)) ;;at TAG_8
                                      (58 (athrow)) 
                                      (59 (return)) ;;at TAG_3
                                      (endofcode 60))
                                   (Exceptions 
                                     (handler 7 21  52 (class "java.lang.Throwable"))
                                     (handler 22 49  52 (class "java.lang.Throwable"))
                                     (handler 52 56  52 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "selectAllAccessibleSelection"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (4 (dup)) 
                                      (5 (astore_1)) 
                                      (6 (monitorenter)) 
                                      (7 (aload_0)) ;;at TAG_3
                                      (8 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (11 (invokevirtual (methodCP "getItemCount" "java.awt.List" () int))) 
                                      (14 (iconst_1)) 
                                      (15 (isub)) 
                                      (16 (istore_2)) 
                                      (17 (iload_2)) ;;at TAG_1
                                      (18 (iflt 35)) ;;to TAG_0
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "this$0" "java.awt.List$AccessibleAWTList" (class "java.awt.List")))) 
                                      (25 (iload_2)) 
                                      (26 (invokevirtual (methodCP "select" "java.awt.List" (int) void))) 
                                      (29 (iinc 2 -1)) 
                                      (32 (goto 17)) ;;to TAG_1
                                      (35 (aload_1)) ;;at TAG_0
                                      (36 (monitorexit)) 
                                      (37 (goto 45))  ;;to TAG_2;;at TAG_4
                                      (40 (astore_3)) ;;at TAG_5
                                      (41 (aload_1)) 
                                      (42 (monitorexit)) 
                                      (43 (aload_3)) ;;at TAG_6
                                      (44 (athrow)) 
                                      (45 (return)) ;;at TAG_2
                                      (endofcode 46))
                                   (Exceptions 
                                     (handler 7 37  40 (class "java.lang.Throwable"))
                                     (handler 40 43  40 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces "javax.accessibility.AccessibleSelection" "java.awt.event.ItemListener" "java.awt.event.ActionListener")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *List$AccessibleAWTList-class-table*
  (make-static-class-decls 
   *java.awt.List$AccessibleAWTList*))

(defconst *package-name-map* 
  ("java.awt.List$AccessibleAWTList" . "java.awt"))

