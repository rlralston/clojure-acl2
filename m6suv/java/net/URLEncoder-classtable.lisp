; URLEncoder-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.net.URLEncoder*
 (make-class-def
      '(class "java.net.URLEncoder"
            "java.lang.Object"
            (constant_pool
                        (INT 32)
                        (STRING  "charsetName")
                        (INT 55296)
                        (INT 56319)
                        (INT 56320)
                        (INT 57343)
                        (STRING  "file.encoding"))
            (fields
                        (field "dontNeedEncoding" (class "java.util.BitSet") (accessflags  *class*  *static* ) -1)
                        (field "caseDiff" int (accessflags  *class*  *final*  *static* ) 0)
                        (field "dfltEncName" (class "java.lang.String") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "encode"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 16)
                                   (parsedcode
                                      (0 (aconst_null)) 
                                      (1 (astore_1)) 
                                      (2 (aload_0)) ;;at TAG_1
                                      (3 (getstatic (fieldCP "dfltEncName" "java.net.URLEncoder" (class "java.lang.String")))) 
                                      (6 (invokestatic (methodCP "encode" "java.net.URLEncoder" ((class "java.lang.String") (class "java.lang.String")) (class "java.lang.String")))) 
                                      (9 (astore_1)) 
                                      (10 (goto 14)) ;;to TAG_0;;at TAG_2
                                      (13 (astore_2)) ;;at TAG_3
                                      (14 (aload_1)) ;;at TAG_0
                                      (15 (areturn)) 
                                      (endofcode 16))
                                   (Exceptions 
                                     (handler 2 10  13 (class "java.io.UnsupportedEncodingException")))
                                   (StackMap )))
                        (method "encode"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 12) (code_length . 375)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_2)) 
                                      (2 (new (class "java.lang.StringBuffer"))) 
                                      (5 (dup)) 
                                      (6 (aload_0)) 
                                      (7 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (10 (invokespecial (methodCP "<init>" "java.lang.StringBuffer" (int) void))) 
                                      (13 (astore_3)) 
                                      (14 (new (class "java.io.CharArrayWriter"))) 
                                      (17 (dup)) 
                                      (18 (invokespecial (methodCP "<init>" "java.io.CharArrayWriter" () void))) 
                                      (21 (astore 5)) 
                                      (23 (aload_1)) 
                                      (24 (ifnonnull 37)) ;;to TAG_0
                                      (27 (new (class "java.lang.NullPointerException"))) 
                                      (30 (dup)) 
                                      (31 (ldc 1)) ;;STRING:: "charsetName"
                                      (33 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (36 (athrow)) 
                                      (37 (aload_1)) ;;at TAG_0
                                      (38 (invokestatic (methodCP "forName" "java.nio.charset.Charset" ((class "java.lang.String")) (class "java.nio.charset.Charset")))) 
                                      (41 (astore 4)) 
                                      (43 (goto 68)) ;;to TAG_1;;at TAG_15
                                      (46 (astore 6)) ;;at TAG_16
                                      (48 (new (class "java.io.UnsupportedEncodingException"))) 
                                      (51 (dup)) 
                                      (52 (aload_1)) 
                                      (53 (invokespecial (methodCP "<init>" "java.io.UnsupportedEncodingException" ((class "java.lang.String")) void))) 
                                      (56 (athrow)) 
                                      (57 (astore 6)) ;;at TAG_17
                                      (59 (new (class "java.io.UnsupportedEncodingException"))) 
                                      (62 (dup)) 
                                      (63 (aload_1)) 
                                      (64 (invokespecial (methodCP "<init>" "java.io.UnsupportedEncodingException" ((class "java.lang.String")) void))) 
                                      (67 (athrow)) 
                                      (68 (iconst_0)) ;;at TAG_1
                                      (69 (istore 6)) 
                                      (71 (iload 6)) ;;at TAG_12
                                      (73 (aload_0)) 
                                      (74 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (77 (if_icmpge 362)) ;;to TAG_2
                                      (80 (aload_0)) 
                                      (81 (iload 6)) 
                                      (83 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (86 (istore 7)) 
                                      (88 (getstatic (fieldCP "dontNeedEncoding" "java.net.URLEncoder" (class "java.util.BitSet")))) 
                                      (91 (iload 7)) 
                                      (93 (invokevirtual (methodCP "get" "java.util.BitSet" (int) boolean))) 
                                      (96 (ifeq 126)) ;;to TAG_3
                                      (99 (iload 7)) 
                                      (101 (bipush 32)) 
                                      (103 (if_icmpne 112)) ;;to TAG_4
                                      (106 (bipush 43)) 
                                      (108 (istore 7)) 
                                      (110 (iconst_1)) 
                                      (111 (istore_2)) 
                                      (112 (aload_3)) ;;at TAG_4
                                      (113 (iload 7)) 
                                      (115 (i2c)) 
                                      (116 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (119 (pop)) 
                                      (120 (iinc 6 1)) 
                                      (123 (goto 359)) ;;to TAG_5
                                      (126 (aload 5)) ;;at TAG_3
                                      (128 (iload 7)) 
                                      (130 (invokevirtual (methodCP "write" "java.io.CharArrayWriter" (int) void))) 
                                      (133 (iload 7)) 
                                      (135 (ldc 2)) ;;INT:: "55296"
                                      (137 (if_icmplt 192)) ;;to TAG_6
                                      (140 (iload 7)) 
                                      (142 (ldc 3)) ;;INT:: "56319"
                                      (144 (if_icmpgt 192)) ;;to TAG_6
                                      (147 (iload 6)) 
                                      (149 (iconst_1)) 
                                      (150 (iadd)) 
                                      (151 (aload_0)) 
                                      (152 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (155 (if_icmpge 192)) ;;to TAG_6
                                      (158 (aload_0)) 
                                      (159 (iload 6)) 
                                      (161 (iconst_1)) 
                                      (162 (iadd)) 
                                      (163 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (166 (istore 8)) 
                                      (168 (iload 8)) 
                                      (170 (ldc 4)) ;;INT:: "56320"
                                      (172 (if_icmplt 192)) ;;to TAG_6
                                      (175 (iload 8)) 
                                      (177 (ldc 5)) ;;INT:: "57343"
                                      (179 (if_icmpgt 192)) ;;to TAG_6
                                      (182 (aload 5)) 
                                      (184 (iload 8)) 
                                      (186 (invokevirtual (methodCP "write" "java.io.CharArrayWriter" (int) void))) 
                                      (189 (iinc 6 1)) 
                                      (192 (iinc 6 1)) ;;at TAG_6
                                      (195 (iload 6)) 
                                      (197 (aload_0)) 
                                      (198 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (201 (if_icmpge 222)) ;;to TAG_7
                                      (204 (getstatic (fieldCP "dontNeedEncoding" "java.net.URLEncoder" (class "java.util.BitSet")))) 
                                      (207 (aload_0)) 
                                      (208 (iload 6)) 
                                      (210 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (213 (dup)) 
                                      (214 (istore 7)) 
                                      (216 (invokevirtual (methodCP "get" "java.util.BitSet" (int) boolean))) 
                                      (219 (ifeq 126)) ;;to TAG_3
                                      (222 (aload 5)) ;;at TAG_7
                                      (224 (invokevirtual (methodCP "flush" "java.io.CharArrayWriter" () void))) 
                                      (227 (new (class "java.lang.String"))) 
                                      (230 (dup)) 
                                      (231 (aload 5)) 
                                      (233 (invokevirtual (methodCP "toCharArray" "java.io.CharArrayWriter" () (array char)))) 
                                      (236 (invokespecial (methodCP "<init>" "java.lang.String" ((array char)) void))) 
                                      (239 (astore 8)) 
                                      (241 (aload 8)) 
                                      (243 (aload 4)) 
                                      (245 (invokevirtual (methodCP "getBytes" "java.lang.String" ((class "java.nio.charset.Charset")) (array byte)))) 
                                      (248 (astore 9)) 
                                      (250 (iconst_0)) 
                                      (251 (istore 10)) 
                                      (253 (iload 10)) ;;at TAG_11
                                      (255 (aload 9)) 
                                      (257 (arraylength)) 
                                      (258 (if_icmpge 352)) ;;to TAG_8
                                      (261 (aload_3)) 
                                      (262 (bipush 37)) 
                                      (264 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (267 (pop)) 
                                      (268 (aload 9)) 
                                      (270 (iload 10)) 
                                      (272 (baload)) 
                                      (273 (iconst_4)) 
                                      (274 (ishr)) 
                                      (275 (bipush 15)) 
                                      (277 (iand)) 
                                      (278 (bipush 16)) 
                                      (280 (invokestatic (methodCP "forDigit" "java.lang.Character" (int int) char))) 
                                      (283 (istore 11)) 
                                      (285 (iload 11)) 
                                      (287 (invokestatic (methodCP "isLetter" "java.lang.Character" (char) boolean))) 
                                      (290 (ifeq 301)) ;;to TAG_9
                                      (293 (iload 11)) 
                                      (295 (bipush 32)) 
                                      (297 (isub)) 
                                      (298 (i2c)) 
                                      (299 (istore 11)) 
                                      (301 (aload_3)) ;;at TAG_9
                                      (302 (iload 11)) 
                                      (304 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (307 (pop)) 
                                      (308 (aload 9)) 
                                      (310 (iload 10)) 
                                      (312 (baload)) 
                                      (313 (bipush 15)) 
                                      (315 (iand)) 
                                      (316 (bipush 16)) 
                                      (318 (invokestatic (methodCP "forDigit" "java.lang.Character" (int int) char))) 
                                      (321 (istore 11)) 
                                      (323 (iload 11)) 
                                      (325 (invokestatic (methodCP "isLetter" "java.lang.Character" (char) boolean))) 
                                      (328 (ifeq 339)) ;;to TAG_10
                                      (331 (iload 11)) 
                                      (333 (bipush 32)) 
                                      (335 (isub)) 
                                      (336 (i2c)) 
                                      (337 (istore 11)) 
                                      (339 (aload_3)) ;;at TAG_10
                                      (340 (iload 11)) 
                                      (342 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (345 (pop)) 
                                      (346 (iinc 10 1)) 
                                      (349 (goto 253)) ;;to TAG_11
                                      (352 (aload 5)) ;;at TAG_8
                                      (354 (invokevirtual (methodCP "reset" "java.io.CharArrayWriter" () void))) 
                                      (357 (iconst_1)) 
                                      (358 (istore_2)) 
                                      (359 (goto 71)) ;;to TAG_12;;at TAG_5
                                      (362 (iload_2)) ;;at TAG_2
                                      (363 (ifeq 373))  ;;to TAG_13
                                      (366 (aload_3)) 
                                      (367 (invokevirtual (methodCP "toString" "java.lang.StringBuffer" () (class "java.lang.String")))) 
                                      (370 (goto 374)) ;;to TAG_14
                                      (373 (aload_0)) ;;at TAG_13
                                      (374 (areturn)) ;;at TAG_14
                                      (endofcode 375))
                                   (Exceptions 
                                     (handler 37 43  46 (class "java.nio.charset.IllegalCharsetNameException"))
                                     (handler 37 43  57 (class "java.nio.charset.UnsupportedCharsetException")))
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 142)
                                   (parsedcode
                                      (0 (aconst_null)) 
                                      (1 (putstatic (fieldCP "dfltEncName" "java.net.URLEncoder" (class "java.lang.String")))) 
                                      (4 (new (class "java.util.BitSet"))) 
                                      (7 (dup)) 
                                      (8 (sipush 256)) 
                                      (11 (invokespecial (methodCP "<init>" "java.util.BitSet" (int) void))) 
                                      (14 (putstatic (fieldCP "dontNeedEncoding" "java.net.URLEncoder" (class "java.util.BitSet")))) 
                                      (17 (bipush 97)) 
                                      (19 (istore_0)) 
                                      (20 (iload_0)) ;;at TAG_1
                                      (21 (bipush 122)) 
                                      (23 (if_icmpgt 39)) ;;to TAG_0
                                      (26 (getstatic (fieldCP "dontNeedEncoding" "java.net.URLEncoder" (class "java.util.BitSet")))) 
                                      (29 (iload_0)) 
                                      (30 (invokevirtual (methodCP "set" "java.util.BitSet" (int) void))) 
                                      (33 (iinc 0 1)) 
                                      (36 (goto 20)) ;;to TAG_1
                                      (39 (bipush 65)) ;;at TAG_0
                                      (41 (istore_0)) 
                                      (42 (iload_0)) ;;at TAG_3
                                      (43 (bipush 90)) 
                                      (45 (if_icmpgt 61))  ;;to TAG_2
                                      (48 (getstatic (fieldCP "dontNeedEncoding" "java.net.URLEncoder" (class "java.util.BitSet")))) 
                                      (51 (iload_0)) 
                                      (52 (invokevirtual (methodCP "set" "java.util.BitSet" (int) void))) 
                                      (55 (iinc 0 1)) 
                                      (58 (goto 42)) ;;to TAG_3
                                      (61 (bipush 48)) ;;at TAG_2
                                      (63 (istore_0)) 
                                      (64 (iload_0)) ;;at TAG_5
                                      (65 (bipush 57)) 
                                      (67 (if_icmpgt 83)) ;;to TAG_4
                                      (70 (getstatic (fieldCP "dontNeedEncoding" "java.net.URLEncoder" (class "java.util.BitSet")))) 
                                      (73 (iload_0)) 
                                      (74 (invokevirtual (methodCP "set" "java.util.BitSet" (int) void))) 
                                      (77 (iinc 0 1)) 
                                      (80 (goto 64)) ;;to TAG_5
                                      (83 (getstatic (fieldCP "dontNeedEncoding" "java.net.URLEncoder" (class "java.util.BitSet")))) ;;at TAG_4
                                      (86 (bipush 32)) 
                                      (88 (invokevirtual (methodCP "set" "java.util.BitSet" (int) void))) 
                                      (91 (getstatic (fieldCP "dontNeedEncoding" "java.net.URLEncoder" (class "java.util.BitSet")))) 
                                      (94 (bipush 45)) 
                                      (96 (invokevirtual (methodCP "set" "java.util.BitSet" (int) void))) 
                                      (99 (getstatic (fieldCP "dontNeedEncoding" "java.net.URLEncoder" (class "java.util.BitSet")))) 
                                      (102 (bipush 95)) 
                                      (104 (invokevirtual (methodCP "set" "java.util.BitSet" (int) void))) 
                                      (107 (getstatic (fieldCP "dontNeedEncoding" "java.net.URLEncoder" (class "java.util.BitSet")))) 
                                      (110 (bipush 46)) 
                                      (112 (invokevirtual (methodCP "set" "java.util.BitSet" (int) void))) 
                                      (115 (getstatic (fieldCP "dontNeedEncoding" "java.net.URLEncoder" (class "java.util.BitSet")))) 
                                      (118 (bipush 42)) 
                                      (120 (invokevirtual (methodCP "set" "java.util.BitSet" (int) void))) 
                                      (123 (new (class "sun.security.action.GetPropertyAction"))) 
                                      (126 (dup)) 
                                      (127 (ldc 6)) ;;STRING:: "file.encoding"
                                      (129 (invokespecial (methodCP "<init>" "sun.security.action.GetPropertyAction" ((class "java.lang.String")) void))) 
                                      (132 (invokestatic (methodCP "doPrivileged" "java.security.AccessController" ((class "java.security.PrivilegedAction")) (class "java.lang.Object")))) 
                                      (135 (checkcast (class "java.lang.String"))) 
                                      (138 (putstatic (fieldCP "dfltEncName" "java.net.URLEncoder" (class "java.lang.String")))) 
                                      (141 (return)) 
                                      (endofcode 142))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *URLEncoder-class-table*
  (make-static-class-decls 
   *java.net.URLEncoder*))

(defconst *package-name-map* 
  ("java.net.URLEncoder" . "java.net"))

