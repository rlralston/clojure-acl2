; KeywordLookupSite$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:51 CDT 2014.
;

(defconst *clojure.lang.KeywordLookupSite$1*
 (make-class-def
      '(class "clojure.lang.KeywordLookupSite$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$c" (class "java.lang.Class") (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.KeywordLookupSite") (class "java.lang.Class"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "clojure.lang.KeywordLookupSite$1" (class "clojure.lang.KeywordLookupSite"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$c" "clojure.lang.KeywordLookupSite$1" (class "java.lang.Class"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 34)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnull 32))  ;;to TAG_0
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "val$c" "clojure.lang.KeywordLookupSite$1" (class "java.lang.Class")))) 
                                      (12 (if_acmpne 32))  ;;to TAG_0
                                      (15 (aload_1)) 
                                      (16 (checkcast (class "clojure.lang.ILookup"))) 
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "this$0" "clojure.lang.KeywordLookupSite$1" (class "clojure.lang.KeywordLookupSite")))) 
                                      (23 (getfield (fieldCP "k" "clojure.lang.KeywordLookupSite" (class "clojure.lang.Keyword")))) 
                                      (26 (invokeinterface (methodCP "valAt" "clojure.lang.ILookup" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (31 (areturn)) 
                                      (32 (aload_0)) ;;at TAG_0
                                      (33 (areturn)) 
                                      (endofcode 34))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.ILookupThunk")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *KeywordLookupSite$1-class-table*
  (make-static-class-decls 
   *clojure.lang.KeywordLookupSite$1*))

(defconst *package-name-map* 
  ("clojure.lang.KeywordLookupSite$1" . "clojure.lang"))

