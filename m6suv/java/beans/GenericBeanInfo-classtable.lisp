; GenericBeanInfo-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.GenericBeanInfo*
 (make-class-def
      '(class "java.beans.GenericBeanInfo"
            "java.beans.SimpleBeanInfo"
            (constant_pool)
            (fields
                        (field "beanDescriptor" (class "java.beans.BeanDescriptor") (accessflags  *class*  *private* ) -1)
                        (field "events" (array (class "java.beans.EventSetDescriptor")) (accessflags  *class*  *private* ) -1)
                        (field "defaultEvent" int (accessflags  *class*  *private* ) -1)
                        (field "properties" (array (class "java.beans.PropertyDescriptor")) (accessflags  *class*  *private* ) -1)
                        (field "defaultProperty" int (accessflags  *class*  *private* ) -1)
                        (field "methods" (array (class "java.beans.MethodDescriptor")) (accessflags  *class*  *private* ) -1)
                        (field "targetBeanInfoRef" (class "java.lang.ref.Reference") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.beans.BeanDescriptor") (array (class "java.beans.EventSetDescriptor")) int (array (class "java.beans.PropertyDescriptor")) int (array (class "java.beans.MethodDescriptor")) (class "java.beans.BeanInfo"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 8) (code_length . 51)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.beans.SimpleBeanInfo" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "beanDescriptor" "java.beans.GenericBeanInfo" (class "java.beans.BeanDescriptor"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "events" "java.beans.GenericBeanInfo" (array (class "java.beans.EventSetDescriptor")))))
                                      (14 (aload_0))
                                      (15 (iload_3))
                                      (16 (putfield (fieldCP "defaultEvent" "java.beans.GenericBeanInfo" int)))
                                      (19 (aload_0))
                                      (20 (aload 4))
                                      (22 (putfield (fieldCP "properties" "java.beans.GenericBeanInfo" (array (class "java.beans.PropertyDescriptor")))))
                                      (25 (aload_0))
                                      (26 (iload 5))
                                      (28 (putfield (fieldCP "defaultProperty" "java.beans.GenericBeanInfo" int)))
                                      (31 (aload_0))
                                      (32 (aload 6))
                                      (34 (putfield (fieldCP "methods" "java.beans.GenericBeanInfo" (array (class "java.beans.MethodDescriptor")))))
                                      (37 (aload_0))
                                      (38 (new (class "java.lang.ref.SoftReference")))
                                      (41 (dup))
                                      (42 (aload 7))
                                      (44 (invokespecial
					(methodCP "<init>" "java.lang.ref.SoftReference" ((class "java.lang.Object")) void)))
                                      (47 (putfield (fieldCP "targetBeanInfoRef" "java.beans.GenericBeanInfo" (class "java.lang.ref.Reference"))))
                                      (50 (return))
                                      (endofcode 51))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.beans.GenericBeanInfo"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 236)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.beans.SimpleBeanInfo" () void))) 
                                      (4 (aload_0)) 
                                      (5 (new (class "java.beans.BeanDescriptor"))) 
                                      (8 (dup)) 
                                      (9 (aload_1)) 
                                      (10 (getfield (fieldCP "beanDescriptor" "java.beans.GenericBeanInfo" (class "java.beans.BeanDescriptor")))) 
                                      (13 (invokespecial (methodCP "<init>" "java.beans.BeanDescriptor" ((class "java.beans.BeanDescriptor")) void))) 
                                      (16 (putfield (fieldCP "beanDescriptor" "java.beans.GenericBeanInfo" (class "java.beans.BeanDescriptor")))) 
                                      (19 (aload_1)) 
                                      (20 (getfield (fieldCP "events" "java.beans.GenericBeanInfo" (array (class "java.beans.EventSetDescriptor"))))) 
                                      (23 (ifnull 72)) ;;to TAG_0
                                      (26 (aload_1)) 
                                      (27 (getfield (fieldCP "events" "java.beans.GenericBeanInfo" (array (class "java.beans.EventSetDescriptor"))))) 
                                      (30 (arraylength)) 
                                      (31 (istore_2)) 
                                      (32 (aload_0)) 
                                      (33 (iload_2)) 
                                      (34 (anewarray (class "java.beans.EventSetDescriptor"))) 
                                      (37 (putfield (fieldCP "events" "java.beans.GenericBeanInfo" (array (class "java.beans.EventSetDescriptor"))))) 
                                      (40 (iconst_0)) 
                                      (41 (istore_3)) 
                                      (42 (iload_3)) ;;at TAG_1
                                      (43 (iload_2)) 
                                      (44 (if_icmpge 72)) ;;to TAG_0
                                      (47 (aload_0)) 
                                      (48 (getfield (fieldCP "events" "java.beans.GenericBeanInfo" (array (class "java.beans.EventSetDescriptor"))))) 
                                      (51 (iload_3)) 
                                      (52 (new (class "java.beans.EventSetDescriptor"))) 
                                      (55 (dup)) 
                                      (56 (aload_1)) 
                                      (57 (getfield (fieldCP "events" "java.beans.GenericBeanInfo" (array (class "java.beans.EventSetDescriptor"))))) 
                                      (60 (iload_3)) 
                                      (61 (aaload)) 
                                      (62 (invokespecial (methodCP "<init>" "java.beans.EventSetDescriptor" ((class "java.beans.EventSetDescriptor")) void))) 
                                      (65 (aastore)) 
                                      (66 (iinc 3 1)) 
                                      (69 (goto 42)) ;;to TAG_1
                                      (72 (aload_0)) ;;at TAG_0
                                      (73 (aload_1)) 
                                      (74 (getfield (fieldCP "defaultEvent" "java.beans.GenericBeanInfo" int))) 
                                      (77 (putfield (fieldCP "defaultEvent" "java.beans.GenericBeanInfo" int))) 
                                      (80 (aload_1)) 
                                      (81 (getfield (fieldCP "properties" "java.beans.GenericBeanInfo" (array (class "java.beans.PropertyDescriptor"))))) 
                                      (84 (ifnull 166))  ;;to TAG_2
                                      (87 (aload_1)) 
                                      (88 (getfield (fieldCP "properties" "java.beans.GenericBeanInfo" (array (class "java.beans.PropertyDescriptor"))))) 
                                      (91 (arraylength)) 
                                      (92 (istore_2)) 
                                      (93 (aload_0)) 
                                      (94 (iload_2)) 
                                      (95 (anewarray (class "java.beans.PropertyDescriptor"))) 
                                      (98 (putfield (fieldCP "properties" "java.beans.GenericBeanInfo" (array (class "java.beans.PropertyDescriptor"))))) 
                                      (101 (iconst_0)) 
                                      (102 (istore_3)) 
                                      (103 (iload_3)) ;;at TAG_5
                                      (104 (iload_2)) 
                                      (105 (if_icmpge 166))  ;;to TAG_2
                                      (108 (aload_1)) 
                                      (109 (getfield (fieldCP "properties" "java.beans.GenericBeanInfo" (array (class "java.beans.PropertyDescriptor"))))) 
                                      (112 (iload_3)) 
                                      (113 (aaload)) 
                                      (114 (astore 4)) 
                                      (116 (aload 4)) 
                                      (118 (instanceof (class "java.beans.IndexedPropertyDescriptor"))) 
                                      (121 (ifeq 145)) ;;to TAG_3
                                      (124 (aload_0)) 
                                      (125 (getfield (fieldCP "properties" "java.beans.GenericBeanInfo" (array (class "java.beans.PropertyDescriptor"))))) 
                                      (128 (iload_3)) 
                                      (129 (new (class "java.beans.IndexedPropertyDescriptor"))) 
                                      (132 (dup)) 
                                      (133 (aload 4)) 
                                      (135 (checkcast (class "java.beans.IndexedPropertyDescriptor"))) 
                                      (138 (invokespecial (methodCP "<init>" "java.beans.IndexedPropertyDescriptor" ((class "java.beans.IndexedPropertyDescriptor")) void))) 
                                      (141 (aastore)) 
                                      (142 (goto 160)) ;;to TAG_4
                                      (145 (aload_0)) ;;at TAG_3
                                      (146 (getfield (fieldCP "properties" "java.beans.GenericBeanInfo" (array (class "java.beans.PropertyDescriptor"))))) 
                                      (149 (iload_3)) 
                                      (150 (new (class "java.beans.PropertyDescriptor"))) 
                                      (153 (dup)) 
                                      (154 (aload 4)) 
                                      (156 (invokespecial (methodCP "<init>" "java.beans.PropertyDescriptor" ((class "java.beans.PropertyDescriptor")) void))) 
                                      (159 (aastore)) 
                                      (160 (iinc 3 1)) ;;at TAG_4
                                      (163 (goto 103)) ;;to TAG_5
                                      (166 (aload_0)) ;;at TAG_2
                                      (167 (aload_1)) 
                                      (168 (getfield (fieldCP "defaultProperty" "java.beans.GenericBeanInfo" int))) 
                                      (171 (putfield (fieldCP "defaultProperty" "java.beans.GenericBeanInfo" int))) 
                                      (174 (aload_1)) 
                                      (175 (getfield (fieldCP "methods" "java.beans.GenericBeanInfo" (array (class "java.beans.MethodDescriptor"))))) 
                                      (178 (ifnull 227)) ;;to TAG_6
                                      (181 (aload_1)) 
                                      (182 (getfield (fieldCP "methods" "java.beans.GenericBeanInfo" (array (class "java.beans.MethodDescriptor"))))) 
                                      (185 (arraylength)) 
                                      (186 (istore_2)) 
                                      (187 (aload_0)) 
                                      (188 (iload_2)) 
                                      (189 (anewarray (class "java.beans.MethodDescriptor"))) 
                                      (192 (putfield (fieldCP "methods" "java.beans.GenericBeanInfo" (array (class "java.beans.MethodDescriptor"))))) 
                                      (195 (iconst_0)) 
                                      (196 (istore_3)) 
                                      (197 (iload_3)) ;;at TAG_7
                                      (198 (iload_2)) 
                                      (199 (if_icmpge 227)) ;;to TAG_6
                                      (202 (aload_0)) 
                                      (203 (getfield (fieldCP "methods" "java.beans.GenericBeanInfo" (array (class "java.beans.MethodDescriptor"))))) 
                                      (206 (iload_3)) 
                                      (207 (new (class "java.beans.MethodDescriptor"))) 
                                      (210 (dup)) 
                                      (211 (aload_1)) 
                                      (212 (getfield (fieldCP "methods" "java.beans.GenericBeanInfo" (array (class "java.beans.MethodDescriptor"))))) 
                                      (215 (iload_3)) 
                                      (216 (aaload)) 
                                      (217 (invokespecial (methodCP "<init>" "java.beans.MethodDescriptor" ((class "java.beans.MethodDescriptor")) void))) 
                                      (220 (aastore)) 
                                      (221 (iinc 3 1)) 
                                      (224 (goto 197)) ;;to TAG_7
                                      (227 (aload_0)) ;;at TAG_6
                                      (228 (aload_1)) 
                                      (229 (getfield (fieldCP "targetBeanInfoRef" "java.beans.GenericBeanInfo" (class "java.lang.ref.Reference")))) 
                                      (232 (putfield (fieldCP "targetBeanInfoRef" "java.beans.GenericBeanInfo" (class "java.lang.ref.Reference")))) 
                                      (235 (return)) 
                                      (endofcode 236))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPropertyDescriptors"
                              (parameters )
                              (returntype . (array (class "java.beans.PropertyDescriptor")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "properties" "java.beans.GenericBeanInfo" (array (class "java.beans.PropertyDescriptor")))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDefaultPropertyIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "defaultProperty" "java.beans.GenericBeanInfo" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getEventSetDescriptors"
                              (parameters )
                              (returntype . (array (class "java.beans.EventSetDescriptor")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "events" "java.beans.GenericBeanInfo" (array (class "java.beans.EventSetDescriptor")))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDefaultEventIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "defaultEvent" "java.beans.GenericBeanInfo" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMethodDescriptors"
                              (parameters )
                              (returntype . (array (class "java.beans.MethodDescriptor")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "methods" "java.beans.GenericBeanInfo" (array (class "java.beans.MethodDescriptor")))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getBeanDescriptor"
                              (parameters )
                              (returntype . (class "java.beans.BeanDescriptor"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "beanDescriptor" "java.beans.GenericBeanInfo" (class "java.beans.BeanDescriptor"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getIcon"
                              (parameters int)
                              (returntype . (class "java.awt.Image"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 29)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "targetBeanInfoRef" "java.beans.GenericBeanInfo" (class "java.lang.ref.Reference")))) 
                                      (4 (invokevirtual (methodCP "get" "java.lang.ref.Reference" () (class "java.lang.Object")))) 
                                      (7 (checkcast (class "java.beans.BeanInfo"))) 
                                      (10 (astore_2)) 
                                      (11 (aload_2)) 
                                      (12 (ifnull 23))  ;;to TAG_0
                                      (15 (aload_2)) 
                                      (16 (iload_1)) 
                                      (17 (invokeinterface (methodCP "getIcon" "java.beans.BeanInfo" (int) (class "java.awt.Image")) 2)) 
                                      (22 (areturn)) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (iload_1)) 
                                      (25 (invokespecial (methodCP "getIcon" "java.beans.SimpleBeanInfo" (int) (class "java.awt.Image")))) 
                                      (28 (areturn)) 
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *GenericBeanInfo-class-table*
  (make-static-class-decls 
   *java.beans.GenericBeanInfo*))

(defconst *package-name-map* 
  ("java.beans.GenericBeanInfo" . "java.beans"))

