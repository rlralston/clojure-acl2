; DefaultFocusTraversalPolicy-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:25 CDT 2014.
;

(defconst *java.awt.DefaultFocusTraversalPolicy*
 (make-class-def
      '(class "java.awt.DefaultFocusTraversalPolicy"
            "java.awt.ContainerOrderFocusTraversalPolicy"
            (constant_pool
                        (LONG 8876966522510157497))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.awt.ContainerOrderFocusTraversalPolicy" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "accept"
                              (parameters (class "java.awt.Component"))
                              (returntype . boolean)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 4) (code_length . 111)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "isVisible" "java.awt.Component" () boolean))) 
                                      (4 (ifeq 21)) ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (invokevirtual (methodCP "isDisplayable" "java.awt.Component" () boolean))) 
                                      (11 (ifeq 21)) ;;to TAG_0
                                      (14 (aload_1)) 
                                      (15 (invokevirtual (methodCP "isEnabled" "java.awt.Component" () boolean))) 
                                      (18 (ifne 23))  ;;to TAG_1
                                      (21 (iconst_0)) ;;at TAG_0
                                      (22 (ireturn)) 
                                      (23 (aload_1)) ;;at TAG_1
                                      (24 (instanceof (class "java.awt.Window"))) 
                                      (27 (ifne 73)) ;;to TAG_2
                                      (30 (aload_1)) 
                                      (31 (invokevirtual (methodCP "getParent" "java.awt.Component" () (class "java.awt.Container")))) 
                                      (34 (astore_2)) 
                                      (35 (aload_2)) ;;at TAG_5
                                      (36 (ifnull 73)) ;;to TAG_2
                                      (39 (aload_2)) 
                                      (40 (invokevirtual (methodCP "isEnabled" "java.awt.Container" () boolean))) 
                                      (43 (ifne 55)) ;;to TAG_3
                                      (46 (aload_2)) 
                                      (47 (invokevirtual (methodCP "isLightweight" "java.awt.Container" () boolean))) 
                                      (50 (ifne 55)) ;;to TAG_3
                                      (53 (iconst_0)) 
                                      (54 (ireturn)) 
                                      (55 (aload_2)) ;;at TAG_3
                                      (56 (instanceof (class "java.awt.Window"))) 
                                      (59 (ifeq 65)) ;;to TAG_4
                                      (62 (goto 73)) ;;to TAG_2
                                      (65 (aload_2)) ;;at TAG_4
                                      (66 (invokevirtual (methodCP "getParent" "java.awt.Container" () (class "java.awt.Container")))) 
                                      (69 (astore_2)) 
                                      (70 (goto 35)) ;;to TAG_5
                                      (73 (aload_1)) ;;at TAG_2
                                      (74 (invokevirtual (methodCP "isFocusable" "java.awt.Component" () boolean))) 
                                      (77 (istore_2)) 
                                      (78 (aload_1)) 
                                      (79 (invokevirtual (methodCP "isFocusTraversableOverridden" "java.awt.Component" () boolean))) 
                                      (82 (ifeq 87)) ;;to TAG_6
                                      (85 (iload_2)) 
                                      (86 (ireturn)) 
                                      (87 (aload_1)) ;;at TAG_6
                                      (88 (invokevirtual (methodCP "getPeer" "java.awt.Component" () (class "java.awt.peer.ComponentPeer")))) 
                                      (91 (astore_3)) 
                                      (92 (aload_3)) 
                                      (93 (ifnull 109)) ;;to TAG_7
                                      (96 (aload_3)) 
                                      (97 (invokeinterface (methodCP "isFocusable" "java.awt.peer.ComponentPeer" () boolean) 1)) 
                                      (102 (ifeq 109)) ;;to TAG_7
                                      (105 (iconst_1)) 
                                      (106 (goto 110)) ;;to TAG_8
                                      (109 (iconst_0)) ;;at TAG_7
                                      (110 (ireturn)) ;;at TAG_8
                                      (endofcode 111))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *DefaultFocusTraversalPolicy-class-table*
  (make-static-class-decls 
   *java.awt.DefaultFocusTraversalPolicy*))

(defconst *package-name-map* 
  ("java.awt.DefaultFocusTraversalPolicy" . "java.awt"))
