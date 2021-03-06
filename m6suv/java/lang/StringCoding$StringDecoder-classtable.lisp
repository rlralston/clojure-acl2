; StringCoding$StringDecoder-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:36 CDT 2014.
;

(defconst *java.lang.StringCoding$StringDecoder*
 (make-class-def
      '(class "java.lang.StringCoding$StringDecoder"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "requestedCharsetName" (class "java.lang.String") (accessflags  *class*  *final*  *private* ) -1)
                        (field "cs" (class "java.nio.charset.Charset") (accessflags  *class*  *final*  *private* ) -1)
                        (field "cd" (class "java.nio.charset.CharsetDecoder") (accessflags  *class*  *final*  *private* ) -1)
                        (field "isTrusted" boolean (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.nio.charset.Charset") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 54)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_0)) 
                                      (5 (aload_2)) 
                                      (6 (putfield (fieldCP "requestedCharsetName" "java.lang.StringCoding$StringDecoder" (class "java.lang.String")))) 
                                      (9 (aload_0)) 
                                      (10 (aload_1)) 
                                      (11 (putfield (fieldCP "cs" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.Charset")))) 
                                      (14 (aload_0)) 
                                      (15 (aload_1)) 
                                      (16 (invokevirtual (methodCP "newDecoder" "java.nio.charset.Charset" () (class "java.nio.charset.CharsetDecoder")))) 
                                      (19 (getstatic (fieldCP "REPLACE" "java.nio.charset.CodingErrorAction" (class "java.nio.charset.CodingErrorAction")))) 
                                      (22 (invokevirtual (methodCP "onMalformedInput" "java.nio.charset.CharsetDecoder" ((class "java.nio.charset.CodingErrorAction")) (class "java.nio.charset.CharsetDecoder")))) 
                                      (25 (getstatic (fieldCP "REPLACE" "java.nio.charset.CodingErrorAction" (class "java.nio.charset.CodingErrorAction")))) 
                                      (28 (invokevirtual (methodCP "onUnmappableCharacter" "java.nio.charset.CharsetDecoder" ((class "java.nio.charset.CodingErrorAction")) (class "java.nio.charset.CharsetDecoder")))) 
                                      (31 (putfield (fieldCP "cd" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.CharsetDecoder")))) 
                                      (34 (aload_0)) 
                                      (35 (aload_1)) 
                                      (36 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (39 (invokevirtual (methodCP "getClassLoader0" "java.lang.Class" () (class "java.lang.ClassLoader")))) 
                                      (42 (ifnonnull 49))  ;;to TAG_0
                                      (45 (iconst_1)) 
                                      (46 (goto 50)) ;;to TAG_1
                                      (49 (iconst_0)) ;;at TAG_0
                                      (50 (putfield (fieldCP "isTrusted" "java.lang.StringCoding$StringDecoder" boolean))) ;;at TAG_1
                                      (53 (return)) 
                                      (endofcode 54))
                                   (Exceptions )
                                   (StackMap )))
                        (method "charsetName"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "cs" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.Charset")))) 
                                      (4 (instanceof (class "sun.nio.cs.HistoricallyNamedCharset"))) 
                                      (7 (ifeq 23))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "cs" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.Charset")))) 
                                      (14 (checkcast (class "sun.nio.cs.HistoricallyNamedCharset"))) 
                                      (17 (invokeinterface (methodCP "historicalName" "sun.nio.cs.HistoricallyNamedCharset" () (class "java.lang.String")) 1)) 
                                      (22 (areturn)) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (getfield (fieldCP "cs" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.Charset")))) 
                                      (27 (invokevirtual (methodCP "name" "java.nio.charset.Charset" () (class "java.lang.String")))) 
                                      (30 (areturn)) 
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "requestedCharsetName"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "requestedCharsetName" "java.lang.StringCoding$StringDecoder" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "decode"
                              (parameters (array byte) int int)
                              (returntype . (array char))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 5) (max_locals . 9) (code_length . 179)
                                   (parsedcode
                                      (0 (iload_3)) 
                                      (1 (aload_0)) 
                                      (2 (getfield (fieldCP "cd" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.CharsetDecoder")))) 
                                      (5 (invokevirtual (methodCP "maxCharsPerByte" "java.nio.charset.CharsetDecoder" () float))) 
                                      (8 (invokestatic (methodCP "access$000" "java.lang.StringCoding" (int float) int))) 
                                      (11 (istore 4)) 
                                      (13 (iload 4)) 
                                      (15 (newarray CHAR)) 
                                      (17 (astore 5)) 
                                      (19 (iload_3)) 
                                      (20 (ifne 26)) ;;to TAG_0
                                      (23 (aload 5)) 
                                      (25 (areturn)) 
                                      (26 (aload_0)) ;;at TAG_0
                                      (27 (getfield (fieldCP "cd" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.CharsetDecoder")))) 
                                      (30 (instanceof (class "sun.nio.cs.ArrayDecoder"))) 
                                      (33 (ifeq 71)) ;;to TAG_1
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "cd" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.CharsetDecoder")))) 
                                      (40 (checkcast (class "sun.nio.cs.ArrayDecoder"))) 
                                      (43 (aload_1)) 
                                      (44 (iload_2)) 
                                      (45 (iload_3)) 
                                      (46 (aload 5)) 
                                      (48 (invokeinterface (methodCP "decode" "sun.nio.cs.ArrayDecoder" ((array byte) int int (array char)) int) 5)) 
                                      (53 (istore 6)) 
                                      (55 (aload 5)) 
                                      (57 (iload 6)) 
                                      (59 (aload_0)) 
                                      (60 (getfield (fieldCP "cs" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.Charset")))) 
                                      (63 (aload_0)) 
                                      (64 (getfield (fieldCP "isTrusted" "java.lang.StringCoding$StringDecoder" boolean))) 
                                      (67 (invokestatic (methodCP "access$100" "java.lang.StringCoding" ((array char) int (class "java.nio.charset.Charset") boolean) (array char)))) 
                                      (70 (areturn)) 
                                      (71 (aload_0)) ;;at TAG_1
                                      (72 (getfield (fieldCP "cd" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.CharsetDecoder")))) 
                                      (75 (invokevirtual (methodCP "reset" "java.nio.charset.CharsetDecoder" () (class "java.nio.charset.CharsetDecoder")))) 
                                      (78 (pop)) 
                                      (79 (aload_1)) 
                                      (80 (iload_2)) 
                                      (81 (iload_3)) 
                                      (82 (invokestatic (methodCP "wrap" "java.nio.ByteBuffer" ((array byte) int int) (class "java.nio.ByteBuffer")))) 
                                      (85 (astore 6)) 
                                      (87 (aload 5)) 
                                      (89 (invokestatic (methodCP "wrap" "java.nio.CharBuffer" ((array char)) (class "java.nio.CharBuffer")))) 
                                      (92 (astore 7)) 
                                      (94 (aload_0)) ;;at TAG_5
                                      (95 (getfield (fieldCP "cd" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.CharsetDecoder")))) 
                                      (98 (aload 6)) 
                                      (100 (aload 7)) 
                                      (102 (iconst_1)) 
                                      (103 (invokevirtual (methodCP "decode" "java.nio.charset.CharsetDecoder" ((class "java.nio.ByteBuffer") (class "java.nio.CharBuffer") boolean) (class "java.nio.charset.CoderResult")))) 
                                      (106 (astore 8)) 
                                      (108 (aload 8)) 
                                      (110 (invokevirtual (methodCP "isUnderflow" "java.nio.charset.CoderResult" () boolean))) 
                                      (113 (ifne 121))  ;;to TAG_2
                                      (116 (aload 8)) 
                                      (118 (invokevirtual (methodCP "throwException" "java.nio.charset.CoderResult" () void))) 
                                      (121 (aload_0)) ;;at TAG_2
                                      (122 (getfield (fieldCP "cd" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.CharsetDecoder")))) 
                                      (125 (aload 7)) 
                                      (127 (invokevirtual (methodCP "flush" "java.nio.charset.CharsetDecoder" ((class "java.nio.CharBuffer")) (class "java.nio.charset.CoderResult")))) 
                                      (130 (astore 8)) 
                                      (132 (aload 8)) 
                                      (134 (invokevirtual (methodCP "isUnderflow" "java.nio.charset.CoderResult" () boolean))) 
                                      (137 (ifne 145)) ;;to TAG_3
                                      (140 (aload 8)) 
                                      (142 (invokevirtual (methodCP "throwException" "java.nio.charset.CoderResult" () void))) 
                                      (145 (goto 160)) ;;to TAG_4;;at TAG_3
                                      (148 (astore 8)) ;;at TAG_6
                                      (150 (new (class "java.lang.Error"))) 
                                      (153 (dup)) 
                                      (154 (aload 8)) 
                                      (156 (invokespecial (methodCP "<init>" "java.lang.Error" ((class "java.lang.Throwable")) void))) 
                                      (159 (athrow)) 
                                      (160 (aload 5)) ;;at TAG_4
                                      (162 (aload 7)) 
                                      (164 (invokevirtual (methodCP "position" "java.nio.CharBuffer" () int))) 
                                      (167 (aload_0)) 
                                      (168 (getfield (fieldCP "cs" "java.lang.StringCoding$StringDecoder" (class "java.nio.charset.Charset")))) 
                                      (171 (aload_0)) 
                                      (172 (getfield (fieldCP "isTrusted" "java.lang.StringCoding$StringDecoder" boolean))) 
                                      (175 (invokestatic (methodCP "access$100" "java.lang.StringCoding" ((array char) int (class "java.nio.charset.Charset") boolean) (array char)))) 
                                      (178 (areturn)) 
                                      (endofcode 179))
                                   (Exceptions 
                                     (handler 94 145  148 (class "java.nio.charset.CharacterCodingException")))
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.nio.charset.Charset") (class "java.lang.String") (class "java.lang.StringCoding$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.lang.StringCoding$StringDecoder" ((class "java.nio.charset.Charset") (class "java.lang.String")) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *StringCoding$StringDecoder-class-table*
  (make-static-class-decls 
   *java.lang.StringCoding$StringDecoder*))

(defconst *package-name-map* 
  ("java.lang.StringCoding$StringDecoder" . "java.lang"))

