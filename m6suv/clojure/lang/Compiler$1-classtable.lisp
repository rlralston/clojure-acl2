; Compiler$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$1*
 (make-class-def
      '(class "clojure.lang.Compiler$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "$SwitchMap$clojure$lang$Compiler$PSTATE" (array int) (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 40)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "values" "clojure.lang.Compiler$PSTATE" () (array (class "clojure.lang.Compiler$PSTATE"))))) 
                                      (3 (arraylength)) 
                                      (4 (newarray INT)) 
                                      (6 (putstatic (fieldCP "$SwitchMap$clojure$lang$Compiler$PSTATE" "clojure.lang.Compiler$1" (array int)))) 
                                      (9 (getstatic (fieldCP "$SwitchMap$clojure$lang$Compiler$PSTATE" "clojure.lang.Compiler$1" (array int)))) ;;at TAG_2
                                      (12 (getstatic (fieldCP "REQ" "clojure.lang.Compiler$PSTATE" (class "clojure.lang.Compiler$PSTATE")))) 
                                      (15 (invokevirtual (methodCP "ordinal" "clojure.lang.Compiler$PSTATE" () int))) 
                                      (18 (iconst_1)) 
                                      (19 (iastore)) 
                                      (20 (goto 24)) ;;to TAG_0;;at TAG_3
                                      (23 (astore_0)) ;;at TAG_4
                                      (24 (getstatic (fieldCP "$SwitchMap$clojure$lang$Compiler$PSTATE" "clojure.lang.Compiler$1" (array int)))) ;;at TAG_0
                                      (27 (getstatic (fieldCP "REST" "clojure.lang.Compiler$PSTATE" (class "clojure.lang.Compiler$PSTATE")))) 
                                      (30 (invokevirtual (methodCP "ordinal" "clojure.lang.Compiler$PSTATE" () int))) 
                                      (33 (iconst_2)) 
                                      (34 (iastore)) 
                                      (35 (goto 39)) ;;to TAG_1;;at TAG_5
                                      (38 (astore_0)) ;;at TAG_6
                                      (39 (return)) ;;at TAG_1
                                      (endofcode 40))
                                   (Exceptions 
                                     (handler 9 20  23 (class "java.lang.NoSuchFieldError"))
                                     (handler 24 35  38 (class "java.lang.NoSuchFieldError")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Compiler$1-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$1*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$1" . "clojure.lang"))

