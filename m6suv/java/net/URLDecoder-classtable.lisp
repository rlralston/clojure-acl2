; URLDecoder-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.net.URLDecoder*
 (make-class-def
      '(class "java.net.URLDecoder"
            "java.lang.Object"
            (constant_pool
                        (STRING  "URLDecoder: empty string enc parameter")
                        (STRING  "URLDecoder: Illegal hex characters in escape (%) pattern - negative value")
                        (STRING  "URLDecoder: Incomplete trailing escape (%) pattern")
                        (STRING  "URLDecoder: Illegal hex characters in escape (%) pattern - "))
            (fields
                        (field "dfltEncName" (class "java.lang.String") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
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
                        (method "decode"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 16)
                                   (parsedcode
                                      (0 (aconst_null)) 
                                      (1 (astore_1)) 
                                      (2 (aload_0)) ;;at TAG_1
                                      (3 (getstatic (fieldCP "dfltEncName" "java.net.URLDecoder" (class "java.lang.String")))) 
                                      (6 (invokestatic (methodCP "decode" "java.net.URLDecoder" ((class "java.lang.String") (class "java.lang.String")) (class "java.lang.String")))) 
                                      (9 (astore_1)) 
                                      (10 (goto 14)) ;;to TAG_0;;at TAG_2
                                      (13 (astore_2)) ;;at TAG_3
                                      (14 (aload_1)) ;;at TAG_0
                                      (15 (areturn)) 
                                      (endofcode 16))
                                   (Exceptions 
                                     (handler 2 10  13 (class "java.io.UnsupportedEncodingException")))
                                   (StackMap )))
                        (method "decode"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 7) (max_locals . 10) (code_length . 321)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_2)) 
                                      (2 (aload_0)) 
                                      (3 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (6 (istore_3)) 
                                      (7 (new (class "java.lang.StringBuffer"))) 
                                      (10 (dup)) 
                                      (11 (iload_3)) 
                                      (12 (sipush 500)) 
                                      (15 (if_icmple 24)) ;;to TAG_0
                                      (18 (iload_3)) 
                                      (19 (iconst_2)) 
                                      (20 (idiv)) 
                                      (21 (goto 25)) ;;to TAG_1
                                      (24 (iload_3)) ;;at TAG_0
                                      (25 (invokespecial (methodCP "<init>" "java.lang.StringBuffer" (int) void))) ;;at TAG_1
                                      (28 (astore 4)) 
                                      (30 (iconst_0)) 
                                      (31 (istore 5)) 
                                      (33 (aload_1)) 
                                      (34 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (37 (ifne 50)) ;;to TAG_2
                                      (40 (new (class "java.io.UnsupportedEncodingException"))) 
                                      (43 (dup)) 
                                      (44 (ldc 0)) ;;STRING:: "URLDecoder: empty string enc parameter"
                                      (46 (invokespecial (methodCP "<init>" "java.io.UnsupportedEncodingException" ((class "java.lang.String")) void))) 
                                      (49 (athrow)) 
                                      (50 (aconst_null)) ;;at TAG_2
                                      (51 (astore 7)) 
                                      (53 (iload 5)) ;;at TAG_7
                                      (55 (iload_3)) 
                                      (56 (if_icmpge 307)) ;;to TAG_3
                                      (59 (aload_0)) 
                                      (60 (iload 5)) 
                                      (62 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (65 (istore 6)) 
                                      (67 (iload 6)) 
                                      (69 (lookupswitch (lookupswitchinfo 293 2 ((37 . 112) (43 . 96))))) ;;to TAG_5;;to TAG_6;;to TAG_4
                                      (96 (aload 4)) ;;at TAG_6
                                      (98 (bipush 32)) 
                                      (100 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (103 (pop)) 
                                      (104 (iinc 5 1)) 
                                      (107 (iconst_1)) 
                                      (108 (istore_2)) 
                                      (109 (goto 53)) ;;to TAG_7
                                      (112 (aload 7)) ;;at TAG_5
                                      (114 (ifnonnull 127)) ;;to TAG_8
                                      (117 (iload_3)) 
                                      (118 (iload 5)) 
                                      (120 (isub)) 
                                      (121 (iconst_3)) 
                                      (122 (idiv)) 
                                      (123 (newarray BYTE)) 
                                      (125 (astore 7)) 
                                      (127 (iconst_0)) ;;at TAG_8
                                      (128 (istore 8)) 
                                      (130 (iload 5)) ;;at TAG_12
                                      (132 (iconst_2)) 
                                      (133 (iadd)) 
                                      (134 (iload_3)) 
                                      (135 (if_icmpge 210)) ;;to TAG_9
                                      (138 (iload 6)) 
                                      (140 (bipush 37)) 
                                      (142 (if_icmpne 210)) ;;to TAG_9
                                      (145 (aload_0)) 
                                      (146 (iload 5)) 
                                      (148 (iconst_1)) 
                                      (149 (iadd)) 
                                      (150 (iload 5)) 
                                      (152 (iconst_3)) 
                                      (153 (iadd)) 
                                      (154 (invokevirtual (methodCP "substring" "java.lang.String" (int int) (class "java.lang.String")))) 
                                      (157 (bipush 16)) 
                                      (159 (invokestatic (methodCP "parseInt" "java.lang.Integer" ((class "java.lang.String") int) int))) 
                                      (162 (istore 9)) 
                                      (164 (iload 9)) 
                                      (166 (ifge 179)) ;;to TAG_10
                                      (169 (new (class "java.lang.IllegalArgumentException"))) 
                                      (172 (dup)) 
                                      (173 (ldc 1)) ;;STRING:: "URLDecoder: Illegal hex characters in escape (%) pattern - negative value"
                                      (175 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (178 (athrow)) 
                                      (179 (aload 7)) ;;at TAG_10
                                      (181 (iload 8)) 
                                      (183 (iinc 8 1)) 
                                      (186 (iload 9)) 
                                      (188 (i2b)) 
                                      (189 (bastore)) 
                                      (190 (iinc 5 3)) 
                                      (193 (iload 5)) 
                                      (195 (iload_3)) 
                                      (196 (if_icmpge 207)) ;;to TAG_11
                                      (199 (aload_0)) 
                                      (200 (iload 5)) 
                                      (202 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (205 (istore 6)) 
                                      (207 (goto 130)) ;;to TAG_12;;at TAG_11
                                      (210 (iload 5)) ;;at TAG_9
                                      (212 (iload_3)) 
                                      (213 (if_icmpge 233))  ;;to TAG_13
                                      (216 (iload 6)) 
                                      (218 (bipush 37)) 
                                      (220 (if_icmpne 233))  ;;to TAG_13
                                      (223 (new (class "java.lang.IllegalArgumentException"))) 
                                      (226 (dup)) 
                                      (227 (ldc 2)) ;;STRING:: "URLDecoder: Incomplete trailing escape (%) pattern"
                                      (229 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (232 (athrow)) 
                                      (233 (aload 4)) ;;at TAG_13
                                      (235 (new (class "java.lang.String"))) 
                                      (238 (dup)) 
                                      (239 (aload 7)) 
                                      (241 (iconst_0)) 
                                      (242 (iload 8)) 
                                      (244 (aload_1)) 
                                      (245 (invokespecial (methodCP "<init>" "java.lang.String" ((array byte) int int (class "java.lang.String")) void))) 
                                      (248 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (251 (pop)) 
                                      (252 (goto 288)) ;;to TAG_14;;at TAG_17
                                      (255 (astore 8)) ;;at TAG_18
                                      (257 (new (class "java.lang.IllegalArgumentException"))) 
                                      (260 (dup)) 
                                      (261 (new (class "java.lang.StringBuilder"))) 
                                      (264 (dup)) 
                                      (265 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (268 (ldc 3)) ;;STRING:: "URLDecoder: Illegal hex characters in escape (%) pattern - "
                                      (270 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (273 (aload 8)) 
                                      (275 (invokevirtual (methodCP "getMessage" "java.lang.NumberFormatException" () (class "java.lang.String")))) 
                                      (278 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (281 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (284 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (287 (athrow)) 
                                      (288 (iconst_1)) ;;at TAG_14
                                      (289 (istore_2)) 
                                      (290 (goto 53)) ;;to TAG_7
                                      (293 (aload 4)) ;;at TAG_4
                                      (295 (iload 6)) 
                                      (297 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (300 (pop)) 
                                      (301 (iinc 5 1)) 
                                      (304 (goto 53)) ;;to TAG_7
                                      (307 (iload_2)) ;;at TAG_3
                                      (308 (ifeq 319)) ;;to TAG_15
                                      (311 (aload 4)) 
                                      (313 (invokevirtual (methodCP "toString" "java.lang.StringBuffer" () (class "java.lang.String")))) 
                                      (316 (goto 320)) ;;to TAG_16
                                      (319 (aload_0)) ;;at TAG_15
                                      (320 (areturn)) ;;at TAG_16
                                      (endofcode 321))
                                   (Exceptions 
                                     (handler 112 252  255 (class "java.lang.NumberFormatException")))
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 7)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "dfltEncName" "java.net.URLEncoder" (class "java.lang.String"))))
                                      (3 (putstatic (fieldCP "dfltEncName" "java.net.URLDecoder" (class "java.lang.String"))))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *URLDecoder-class-table*
  (make-static-class-decls 
   *java.net.URLDecoder*))

(defconst *package-name-map* 
  ("java.net.URLDecoder" . "java.net"))

