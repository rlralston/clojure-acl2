; FileTreeWalker$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.FileTreeWalker$1*
 (make-class-def
      '(class "java.nio.file.FileTreeWalker$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "$SwitchMap$java$nio$file$FileVisitOption" (array int) (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 25)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "values" "java.nio.file.FileVisitOption" () (array (class "java.nio.file.FileVisitOption"))))) 
                                      (3 (arraylength)) 
                                      (4 (newarray INT)) 
                                      (6 (putstatic (fieldCP "$SwitchMap$java$nio$file$FileVisitOption" "java.nio.file.FileTreeWalker$1" (array int)))) 
                                      (9 (getstatic (fieldCP "$SwitchMap$java$nio$file$FileVisitOption" "java.nio.file.FileTreeWalker$1" (array int)))) ;;at TAG_1
                                      (12 (getstatic (fieldCP "FOLLOW_LINKS" "java.nio.file.FileVisitOption" (class "java.nio.file.FileVisitOption")))) 
                                      (15 (invokevirtual (methodCP "ordinal" "java.nio.file.FileVisitOption" () int))) 
                                      (18 (iconst_1)) 
                                      (19 (iastore)) 
                                      (20 (goto 24)) ;;to TAG_0;;at TAG_2
                                      (23 (astore_0)) ;;at TAG_3
                                      (24 (return)) ;;at TAG_0
                                      (endofcode 25))
                                   (Exceptions 
                                     (handler 9 20  23 (class "java.lang.NoSuchFieldError")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *FileTreeWalker$1-class-table*
  (make-static-class-decls 
   *java.nio.file.FileTreeWalker$1*))

(defconst *package-name-map* 
  ("java.nio.file.FileTreeWalker$1" . "java.nio.file"))
