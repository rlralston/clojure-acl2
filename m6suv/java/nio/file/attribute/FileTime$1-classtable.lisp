; FileTime$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.attribute.FileTime$1*
 (make-class-def
      '(class "java.nio.file.attribute.FileTime$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "$SwitchMap$java$util$concurrent$TimeUnit" (array int) (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 117)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "values" "java.util.concurrent.TimeUnit" () (array (class "java.util.concurrent.TimeUnit"))))) 
                                      (3 (arraylength)) 
                                      (4 (newarray INT)) 
                                      (6 (putstatic (fieldCP "$SwitchMap$java$util$concurrent$TimeUnit" "java.nio.file.attribute.FileTime$1" (array int)))) 
                                      (9 (getstatic (fieldCP "$SwitchMap$java$util$concurrent$TimeUnit" "java.nio.file.attribute.FileTime$1" (array int)))) ;;at TAG_7
                                      (12 (getstatic (fieldCP "DAYS" "java.util.concurrent.TimeUnit" (class "java.util.concurrent.TimeUnit")))) 
                                      (15 (invokevirtual (methodCP "ordinal" "java.util.concurrent.TimeUnit" () int))) 
                                      (18 (iconst_1)) 
                                      (19 (iastore)) 
                                      (20 (goto 24)) ;;to TAG_0;;at TAG_8
                                      (23 (astore_0)) ;;at TAG_9
                                      (24 (getstatic (fieldCP "$SwitchMap$java$util$concurrent$TimeUnit" "java.nio.file.attribute.FileTime$1" (array int)))) ;;at TAG_0
                                      (27 (getstatic (fieldCP "HOURS" "java.util.concurrent.TimeUnit" (class "java.util.concurrent.TimeUnit")))) 
                                      (30 (invokevirtual (methodCP "ordinal" "java.util.concurrent.TimeUnit" () int))) 
                                      (33 (iconst_2)) 
                                      (34 (iastore)) 
                                      (35 (goto 39)) ;;to TAG_1;;at TAG_10
                                      (38 (astore_0)) ;;at TAG_11
                                      (39 (getstatic (fieldCP "$SwitchMap$java$util$concurrent$TimeUnit" "java.nio.file.attribute.FileTime$1" (array int)))) ;;at TAG_1
                                      (42 (getstatic (fieldCP "MINUTES" "java.util.concurrent.TimeUnit" (class "java.util.concurrent.TimeUnit")))) 
                                      (45 (invokevirtual (methodCP "ordinal" "java.util.concurrent.TimeUnit" () int))) 
                                      (48 (iconst_3)) 
                                      (49 (iastore)) 
                                      (50 (goto 54)) ;;to TAG_2;;at TAG_12
                                      (53 (astore_0)) ;;at TAG_13
                                      (54 (getstatic (fieldCP "$SwitchMap$java$util$concurrent$TimeUnit" "java.nio.file.attribute.FileTime$1" (array int)))) ;;at TAG_2
                                      (57 (getstatic (fieldCP "SECONDS" "java.util.concurrent.TimeUnit" (class "java.util.concurrent.TimeUnit")))) 
                                      (60 (invokevirtual (methodCP "ordinal" "java.util.concurrent.TimeUnit" () int))) 
                                      (63 (iconst_4)) 
                                      (64 (iastore)) 
                                      (65 (goto 69)) ;;to TAG_3;;at TAG_14
                                      (68 (astore_0)) ;;at TAG_15
                                      (69 (getstatic (fieldCP "$SwitchMap$java$util$concurrent$TimeUnit" "java.nio.file.attribute.FileTime$1" (array int)))) ;;at TAG_3
                                      (72 (getstatic (fieldCP "MILLISECONDS" "java.util.concurrent.TimeUnit" (class "java.util.concurrent.TimeUnit")))) 
                                      (75 (invokevirtual (methodCP "ordinal" "java.util.concurrent.TimeUnit" () int))) 
                                      (78 (iconst_5)) 
                                      (79 (iastore)) 
                                      (80 (goto 84)) ;;to TAG_4;;at TAG_16
                                      (83 (astore_0)) ;;at TAG_17
                                      (84 (getstatic (fieldCP "$SwitchMap$java$util$concurrent$TimeUnit" "java.nio.file.attribute.FileTime$1" (array int)))) ;;at TAG_4
                                      (87 (getstatic (fieldCP "MICROSECONDS" "java.util.concurrent.TimeUnit" (class "java.util.concurrent.TimeUnit")))) 
                                      (90 (invokevirtual (methodCP "ordinal" "java.util.concurrent.TimeUnit" () int))) 
                                      (93 (bipush 6)) 
                                      (95 (iastore)) 
                                      (96 (goto 100)) ;;to TAG_5;;at TAG_18
                                      (99 (astore_0)) ;;at TAG_19
                                      (100 (getstatic (fieldCP "$SwitchMap$java$util$concurrent$TimeUnit" "java.nio.file.attribute.FileTime$1" (array int)))) ;;at TAG_5
                                      (103 (getstatic (fieldCP "NANOSECONDS" "java.util.concurrent.TimeUnit" (class "java.util.concurrent.TimeUnit")))) 
                                      (106 (invokevirtual (methodCP "ordinal" "java.util.concurrent.TimeUnit" () int))) 
                                      (109 (bipush 7)) 
                                      (111 (iastore)) 
                                      (112 (goto 116)) ;;to TAG_6;;at TAG_20
                                      (115 (astore_0)) ;;at TAG_21
                                      (116 (return)) ;;at TAG_6
                                      (endofcode 117))
                                   (Exceptions 
                                     (handler 9 20  23 (class "java.lang.NoSuchFieldError"))
                                     (handler 24 35  38 (class "java.lang.NoSuchFieldError"))
                                     (handler 39 50  53 (class "java.lang.NoSuchFieldError"))
                                     (handler 54 65  68 (class "java.lang.NoSuchFieldError"))
                                     (handler 69 80  83 (class "java.lang.NoSuchFieldError"))
                                     (handler 84 96  99 (class "java.lang.NoSuchFieldError"))
                                     (handler 100 112  115 (class "java.lang.NoSuchFieldError")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *FileTime$1-class-table*
  (make-static-class-decls 
   *java.nio.file.attribute.FileTime$1*))

(defconst *package-name-map* 
  ("java.nio.file.attribute.FileTime$1" . "java.nio.file.attribute"))

