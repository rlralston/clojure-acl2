; GraphicsDevice$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.GraphicsDevice$1*
 (make-class-def
      '(class "java.awt.GraphicsDevice$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "$SwitchMap$java$awt$GraphicsDevice$WindowTranslucency" (array int) (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 55)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "values" "java.awt.GraphicsDevice$WindowTranslucency" () (array (class "java.awt.GraphicsDevice$WindowTranslucency"))))) 
                                      (3 (arraylength)) 
                                      (4 (newarray INT)) 
                                      (6 (putstatic (fieldCP "$SwitchMap$java$awt$GraphicsDevice$WindowTranslucency" "java.awt.GraphicsDevice$1" (array int)))) 
                                      (9 (getstatic (fieldCP "$SwitchMap$java$awt$GraphicsDevice$WindowTranslucency" "java.awt.GraphicsDevice$1" (array int)))) ;;at TAG_3
                                      (12 (getstatic (fieldCP "PERPIXEL_TRANSPARENT" "java.awt.GraphicsDevice$WindowTranslucency" (class "java.awt.GraphicsDevice$WindowTranslucency")))) 
                                      (15 (invokevirtual (methodCP "ordinal" "java.awt.GraphicsDevice$WindowTranslucency" () int))) 
                                      (18 (iconst_1)) 
                                      (19 (iastore)) 
                                      (20 (goto 24)) ;;to TAG_0;;at TAG_4
                                      (23 (astore_0)) ;;at TAG_5
                                      (24 (getstatic (fieldCP "$SwitchMap$java$awt$GraphicsDevice$WindowTranslucency" "java.awt.GraphicsDevice$1" (array int)))) ;;at TAG_0
                                      (27 (getstatic (fieldCP "TRANSLUCENT" "java.awt.GraphicsDevice$WindowTranslucency" (class "java.awt.GraphicsDevice$WindowTranslucency")))) 
                                      (30 (invokevirtual (methodCP "ordinal" "java.awt.GraphicsDevice$WindowTranslucency" () int))) 
                                      (33 (iconst_2)) 
                                      (34 (iastore)) 
                                      (35 (goto 39))  ;;to TAG_1;;at TAG_6
                                      (38 (astore_0)) ;;at TAG_7
                                      (39 (getstatic (fieldCP "$SwitchMap$java$awt$GraphicsDevice$WindowTranslucency" "java.awt.GraphicsDevice$1" (array int)))) ;;at TAG_1
                                      (42 (getstatic (fieldCP "PERPIXEL_TRANSLUCENT" "java.awt.GraphicsDevice$WindowTranslucency" (class "java.awt.GraphicsDevice$WindowTranslucency")))) 
                                      (45 (invokevirtual (methodCP "ordinal" "java.awt.GraphicsDevice$WindowTranslucency" () int))) 
                                      (48 (iconst_3)) 
                                      (49 (iastore)) 
                                      (50 (goto 54)) ;;to TAG_2;;at TAG_8
                                      (53 (astore_0)) ;;at TAG_9
                                      (54 (return)) ;;at TAG_2
                                      (endofcode 55))
                                   (Exceptions 
                                     (handler 9 20  23 (class "java.lang.NoSuchFieldError"))
                                     (handler 24 35  38 (class "java.lang.NoSuchFieldError"))
                                     (handler 39 50  53 (class "java.lang.NoSuchFieldError")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *GraphicsDevice$1-class-table*
  (make-static-class-decls 
   *java.awt.GraphicsDevice$1*))

(defconst *package-name-map* 
  ("java.awt.GraphicsDevice$1" . "java.awt"))
