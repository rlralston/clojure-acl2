; CharacterDataPrivateUse-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.CharacterDataPrivateUse*
 (make-class-def
      '(class "java.lang.CharacterDataPrivateUse"
            "java.lang.CharacterData"
            (constant_pool
                        (INT 65534))
            (fields
                        (field "instance" (class "java.lang.CharacterData") (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "getProperties"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getType"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (ldc 0)) ;;INT:: "65534"
                                      (3 (iand)) 
                                      (4 (ldc 0)) ;;INT:: "65534"
                                      (6 (if_icmpne 13))  ;;to TAG_0
                                      (9 (iconst_0)) 
                                      (10 (goto 15)) ;;to TAG_1
                                      (13 (bipush 18)) ;;at TAG_0
                                      (15 (ireturn)) ;;at TAG_1
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isJavaIdentifierStart"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isJavaIdentifierPart"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isUnicodeIdentifierStart"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isUnicodeIdentifierPart"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isIdentifierIgnorable"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toLowerCase"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iload_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toUpperCase"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iload_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toTitleCase"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iload_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "digit"
                              (parameters int int)
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_m1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getNumericValue"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_m1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isWhitespace"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDirectionality"
                              (parameters int)
                              (returntype . byte)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (ldc 0)) ;;INT:: "65534"
                                      (3 (iand)) 
                                      (4 (ldc 0)) ;;INT:: "65534"
                                      (6 (if_icmpne 13))  ;;to TAG_0
                                      (9 (iconst_m1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (ireturn)) ;;at TAG_1
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isMirrored"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.CharacterData" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.lang.CharacterDataPrivateUse")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.CharacterDataPrivateUse" () void)))
                                      (7 (putstatic (fieldCP "instance" "java.lang.CharacterDataPrivateUse" (class "java.lang.CharacterData"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *CharacterDataPrivateUse-class-table*
  (make-static-class-decls 
   *java.lang.CharacterDataPrivateUse*))

(defconst *package-name-map* 
  ("java.lang.CharacterDataPrivateUse" . "java.lang"))

