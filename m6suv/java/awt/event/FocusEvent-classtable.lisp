; FocusEvent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.event.FocusEvent*
 (make-class-def
      '(class "java.awt.event.FocusEvent"
            "java.awt.event.ComponentEvent"
            (constant_pool
                        (INT 1004)
                        (INT 1005)
                        (LONG 523753786457416396)
                        (STRING  "FOCUS_GAINED")
                        (STRING  "FOCUS_LOST")
                        (STRING  "unknown type")
                        (STRING  ",temporary")
                        (STRING  ",permanent")
                        (STRING  ",opposite="))
            (fields
                        (field "FOCUS_FIRST" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "FOCUS_LAST" int (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "FOCUS_GAINED" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "FOCUS_LOST" int (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "temporary" boolean (accessflags  *class* ) -1)
                        (field "opposite" (class "java.awt.Component") (accessflags  *class*  *transient* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 2))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.Component") int boolean (class "java.awt.Component"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.awt.event.ComponentEvent" ((class "java.awt.Component") int) void)))
                                      (6 (aload_0))
                                      (7 (iload_3))
                                      (8 (putfield (fieldCP "temporary" "java.awt.event.FocusEvent" boolean)))
                                      (11 (aload_0))
                                      (12 (aload 4))
                                      (14 (putfield (fieldCP "opposite" "java.awt.event.FocusEvent" (class "java.awt.Component"))))
                                      (17 (return))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.awt.Component") int boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (iload_3))
                                      (4 (aconst_null))
                                      (5 (invokespecial
					(methodCP "<init>" "java.awt.event.FocusEvent" ((class "java.awt.Component") int boolean (class "java.awt.Component")) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.awt.Component") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (iconst_0))
                                      (4 (invokespecial
					(methodCP "<init>" "java.awt.event.FocusEvent" ((class "java.awt.Component") int boolean) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isTemporary"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "temporary" "java.awt.event.FocusEvent" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getOppositeComponent"
                              (parameters )
                              (returntype . (class "java.awt.Component"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "opposite" "java.awt.event.FocusEvent" (class "java.awt.Component")))) 
                                      (4 (ifnonnull 9)) ;;to TAG_0
                                      (7 (aconst_null)) 
                                      (8 (areturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (getfield (fieldCP "opposite" "java.awt.event.FocusEvent" (class "java.awt.Component")))) 
                                      (13 (invokestatic (methodCP "targetToAppContext" "sun.awt.SunToolkit" ((class "java.lang.Object")) (class "sun.awt.AppContext")))) 
                                      (16 (invokestatic (methodCP "getAppContext" "sun.awt.AppContext" () (class "sun.awt.AppContext")))) 
                                      (19 (if_acmpne 29)) ;;to TAG_1
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "opposite" "java.awt.event.FocusEvent" (class "java.awt.Component")))) 
                                      (26 (goto 30))  ;;to TAG_2
                                      (29 (aconst_null)) ;;at TAG_1
                                      (30 (areturn)) ;;at TAG_2
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "paramString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 91)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "id" "java.awt.event.FocusEvent" int))) 
                                      (4 (lookupswitch (lookupswitchinfo 44 2 ((1004 . 32) (1005 . 38)))))  ;;to TAG_2;;to TAG_0;;to TAG_1
                                      (32 (ldc 3)) ;;at TAG_1;;STRING:: "FOCUS_GAINED"
                                      (34 (astore_1)) 
                                      (35 (goto 47)) ;;to TAG_3
                                      (38 (ldc 4)) ;;at TAG_2;;STRING:: "FOCUS_LOST"
                                      (40 (astore_1)) 
                                      (41 (goto 47)) ;;to TAG_3
                                      (44 (ldc 5)) ;;at TAG_0;;STRING:: "unknown type"
                                      (46 (astore_1)) 
                                      (47 (new (class "java.lang.StringBuilder"))) ;;at TAG_3
                                      (50 (dup)) 
                                      (51 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (54 (aload_1)) 
                                      (55 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (58 (aload_0)) 
                                      (59 (getfield (fieldCP "temporary" "java.awt.event.FocusEvent" boolean))) 
                                      (62 (ifeq 70)) ;;to TAG_4
                                      (65 (ldc 6)) ;;STRING:: ",temporary"
                                      (67 (goto 72)) ;;to TAG_5
                                      (70 (ldc 7)) ;;at TAG_4;;STRING:: ",permanent"
                                      (72 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) ;;at TAG_5
                                      (75 (ldc 8)) ;;STRING:: ",opposite="
                                      (77 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (80 (aload_0)) 
                                      (81 (invokevirtual (methodCP "getOppositeComponent" "java.awt.event.FocusEvent" () (class "java.awt.Component")))) 
                                      (84 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (87 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (90 (areturn)) 
                                      (endofcode 91))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *FocusEvent-class-table*
  (make-static-class-decls 
   *java.awt.event.FocusEvent*))

(defconst *package-name-map* 
  ("java.awt.event.FocusEvent" . "java.awt.event"))

