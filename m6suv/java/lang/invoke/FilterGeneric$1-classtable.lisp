; FilterGeneric$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:34 CDT 2014.
;

(defconst *java.lang.invoke.FilterGeneric$1*
 (make-class-def
      '(class "java.lang.invoke.FilterGeneric$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "$SwitchMap$java$lang$invoke$FilterGeneric$Kind" (array int) (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 70)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "values" "java.lang.invoke.FilterGeneric$Kind" () (array (class "java.lang.invoke.FilterGeneric$Kind"))))) 
                                      (3 (arraylength)) 
                                      (4 (newarray INT)) 
                                      (6 (putstatic (fieldCP "$SwitchMap$java$lang$invoke$FilterGeneric$Kind" "java.lang.invoke.FilterGeneric$1" (array int)))) 
                                      (9 (getstatic (fieldCP "$SwitchMap$java$lang$invoke$FilterGeneric$Kind" "java.lang.invoke.FilterGeneric$1" (array int)))) ;;at TAG_4
                                      (12 (getstatic (fieldCP "value" "java.lang.invoke.FilterGeneric$Kind" (class "java.lang.invoke.FilterGeneric$Kind")))) 
                                      (15 (invokevirtual (methodCP "ordinal" "java.lang.invoke.FilterGeneric$Kind" () int))) 
                                      (18 (iconst_1)) 
                                      (19 (iastore)) 
                                      (20 (goto 24)) ;;to TAG_0;;at TAG_5
                                      (23 (astore_0)) ;;at TAG_6
                                      (24 (getstatic (fieldCP "$SwitchMap$java$lang$invoke$FilterGeneric$Kind" "java.lang.invoke.FilterGeneric$1" (array int)))) ;;at TAG_0
                                      (27 (getstatic (fieldCP "flyby" "java.lang.invoke.FilterGeneric$Kind" (class "java.lang.invoke.FilterGeneric$Kind")))) 
                                      (30 (invokevirtual (methodCP "ordinal" "java.lang.invoke.FilterGeneric$Kind" () int))) 
                                      (33 (iconst_2)) 
                                      (34 (iastore)) 
                                      (35 (goto 39))  ;;to TAG_1;;at TAG_7
                                      (38 (astore_0)) ;;at TAG_8
                                      (39 (getstatic (fieldCP "$SwitchMap$java$lang$invoke$FilterGeneric$Kind" "java.lang.invoke.FilterGeneric$1" (array int)))) ;;at TAG_1
                                      (42 (getstatic (fieldCP "fold" "java.lang.invoke.FilterGeneric$Kind" (class "java.lang.invoke.FilterGeneric$Kind")))) 
                                      (45 (invokevirtual (methodCP "ordinal" "java.lang.invoke.FilterGeneric$Kind" () int))) 
                                      (48 (iconst_3)) 
                                      (49 (iastore)) 
                                      (50 (goto 54)) ;;to TAG_2;;at TAG_9
                                      (53 (astore_0)) ;;at TAG_10
                                      (54 (getstatic (fieldCP "$SwitchMap$java$lang$invoke$FilterGeneric$Kind" "java.lang.invoke.FilterGeneric$1" (array int)))) ;;at TAG_2
                                      (57 (getstatic (fieldCP "collect" "java.lang.invoke.FilterGeneric$Kind" (class "java.lang.invoke.FilterGeneric$Kind")))) 
                                      (60 (invokevirtual (methodCP "ordinal" "java.lang.invoke.FilterGeneric$Kind" () int))) 
                                      (63 (iconst_4)) 
                                      (64 (iastore)) 
                                      (65 (goto 69)) ;;to TAG_3;;at TAG_11
                                      (68 (astore_0)) ;;at TAG_12
                                      (69 (return)) ;;at TAG_3
                                      (endofcode 70))
                                   (Exceptions 
                                     (handler 9 20  23 (class "java.lang.NoSuchFieldError"))
                                     (handler 24 35  38 (class "java.lang.NoSuchFieldError"))
                                     (handler 39 50  53 (class "java.lang.NoSuchFieldError"))
                                     (handler 54 65  68 (class "java.lang.NoSuchFieldError")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *FilterGeneric$1-class-table*
  (make-static-class-decls 
   *java.lang.invoke.FilterGeneric$1*))

(defconst *package-name-map* 
  ("java.lang.invoke.FilterGeneric$1" . "java.lang.invoke"))

