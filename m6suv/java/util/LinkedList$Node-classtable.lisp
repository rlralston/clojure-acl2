; LinkedList$Node-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.LinkedList$Node*
 (make-class-def
      '(class "java.util.LinkedList$Node"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "item" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "next" (class "java.util.LinkedList$Node") (accessflags  *class* ) -1)
                        (field "prev" (class "java.util.LinkedList$Node") (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.LinkedList$Node") (class "java.lang.Object") (class "java.util.LinkedList$Node"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_2))
                                      (6 (putfield (fieldCP "item" "java.util.LinkedList$Node" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_3))
                                      (11 (putfield (fieldCP "next" "java.util.LinkedList$Node" (class "java.util.LinkedList$Node"))))
                                      (14 (aload_0))
                                      (15 (aload_1))
                                      (16 (putfield (fieldCP "prev" "java.util.LinkedList$Node" (class "java.util.LinkedList$Node"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *LinkedList$Node-class-table*
  (make-static-class-decls 
   *java.util.LinkedList$Node*))

(defconst *package-name-map* 
  ("java.util.LinkedList$Node" . "java.util"))

