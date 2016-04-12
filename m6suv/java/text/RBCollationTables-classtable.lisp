; RBCollationTables-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.text.RBCollationTables*
 (make-class-def
      '(class "java.text.RBCollationTables"
            "java.lang.Object"
            (constant_pool
                        (INT 2113929216)
                        (INT 2130706432)
                        (INT -1)
                        (INT -65536)
                        (INT 65280)
                        (INT 255)
                        (INT -256)
                        (INT 16)
                        (INT 8))
            (fields
                        (field "EXPANDCHARINDEX" int (accessflags  *class*  *final*  *static* ) 0)
                        (field "CONTRACTCHARINDEX" int (accessflags  *class*  *final*  *static* ) 1)
                        (field "UNMAPPED" int (accessflags  *class*  *final*  *static* ) 2)
                        (field "PRIMARYORDERMASK" int (accessflags  *class*  *final*  *static* ) 3)
                        (field "SECONDARYORDERMASK" int (accessflags  *class*  *final*  *static* ) 4)
                        (field "TERTIARYORDERMASK" int (accessflags  *class*  *final*  *static* ) 5)
                        (field "PRIMARYDIFFERENCEONLY" int (accessflags  *class*  *final*  *static* ) 3)
                        (field "SECONDARYDIFFERENCEONLY" int (accessflags  *class*  *final*  *static* ) 6)
                        (field "PRIMARYORDERSHIFT" int (accessflags  *class*  *final*  *static* ) 7)
                        (field "SECONDARYORDERSHIFT" int (accessflags  *class*  *final*  *static* ) 8)
                        (field "rules" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "frenchSec" boolean (accessflags  *class*  *private* ) -1)
                        (field "seAsianSwapping" boolean (accessflags  *class*  *private* ) -1)
                        (field "mapping" (class "sun.text.UCompactIntArray") (accessflags  *class*  *private* ) -1)
                        (field "contractTable" (class "java.util.Vector") (accessflags  *class*  *private* ) -1)
                        (field "expandTable" (class "java.util.Vector") (accessflags  *class*  *private* ) -1)
                        (field "contractFlags" (class "sun.text.IntHashtable") (accessflags  *class*  *private* ) -1)
                        (field "maxSecOrder" short (accessflags  *class*  *private* ) -1)
                        (field "maxTerOrder" short (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 78)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (putfield (fieldCP "rules" "java.text.RBCollationTables" (class "java.lang.String"))))
                                      (9 (aload_0))
                                      (10 (iconst_0))
                                      (11 (putfield (fieldCP "frenchSec" "java.text.RBCollationTables" boolean)))
                                      (14 (aload_0))
                                      (15 (iconst_0))
                                      (16 (putfield (fieldCP "seAsianSwapping" "java.text.RBCollationTables" boolean)))
                                      (19 (aload_0))
                                      (20 (aconst_null))
                                      (21 (putfield (fieldCP "mapping" "java.text.RBCollationTables" (class "sun.text.UCompactIntArray"))))
                                      (24 (aload_0))
                                      (25 (aconst_null))
                                      (26 (putfield (fieldCP "contractTable" "java.text.RBCollationTables" (class "java.util.Vector"))))
                                      (29 (aload_0))
                                      (30 (aconst_null))
                                      (31 (putfield (fieldCP "expandTable" "java.text.RBCollationTables" (class "java.util.Vector"))))
                                      (34 (aload_0))
                                      (35 (aconst_null))
                                      (36 (putfield (fieldCP "contractFlags" "java.text.RBCollationTables" (class "sun.text.IntHashtable"))))
                                      (39 (aload_0))
                                      (40 (iconst_0))
                                      (41 (putfield (fieldCP "maxSecOrder" "java.text.RBCollationTables" short)))
                                      (44 (aload_0))
                                      (45 (iconst_0))
                                      (46 (putfield (fieldCP "maxTerOrder" "java.text.RBCollationTables" short)))
                                      (49 (aload_0))
                                      (50 (aload_1))
                                      (51 (putfield (fieldCP "rules" "java.text.RBCollationTables" (class "java.lang.String"))))
                                      (54 (new (class "java.text.RBTableBuilder")))
                                      (57 (dup))
                                      (58 (new (class "java.text.RBCollationTables$BuildAPI")))
                                      (61 (dup))
                                      (62 (aload_0))
                                      (63 (aconst_null))
                                      (64 (invokespecial
					(methodCP "<init>" "java.text.RBCollationTables$BuildAPI" ((class "java.text.RBCollationTables") (class "java.text.RBCollationTables$1")) void)))
                                      (67 (invokespecial
					(methodCP "<init>" "java.text.RBTableBuilder" ((class "java.text.RBCollationTables$BuildAPI")) void)))
                                      (70 (astore_3))
                                      (71 (aload_3))
                                      (72 (aload_1))
                                      (73 (iload_2))
                                      (74 (invokevirtual
					(methodCP "build" "java.text.RBTableBuilder" ((class "java.lang.String") int) void)))
                                      (77 (return))
                                      (endofcode 78))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRules"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "rules" "java.text.RBCollationTables" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isFrenchSec"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "frenchSec" "java.text.RBCollationTables" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isSEAsianSwapping"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "seAsianSwapping" "java.text.RBCollationTables" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getContractValues"
                              (parameters int)
                              (returntype . (class "java.util.Vector"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "mapping" "java.text.RBCollationTables" (class "sun.text.UCompactIntArray"))))
                                      (4 (iload_1))
                                      (5 (invokevirtual
					(methodCP "elementAt" "sun.text.UCompactIntArray" (int) int)))
                                      (8 (istore_2))
                                      (9 (aload_0))
                                      (10 (iload_2))
                                      (11 (ldc 1))        ;;INT:: "2130706432"
                                      (13 (isub))
                                      (14 (invokespecial
					(methodCP "getContractValuesImpl" "java.text.RBCollationTables" (int) (class "java.util.Vector"))))
                                      (17 (areturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getContractValuesImpl"
                              (parameters int)
                              (returntype . (class "java.util.Vector"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (iflt 16))  ;;to TAG_0
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "contractTable" "java.text.RBCollationTables" (class "java.util.Vector")))) 
                                      (8 (iload_1)) 
                                      (9 (invokevirtual (methodCP "elementAt" "java.util.Vector" (int) (class "java.lang.Object")))) 
                                      (12 (checkcast (class "java.util.Vector"))) 
                                      (15 (areturn)) 
                                      (16 (aconst_null)) ;;at TAG_0
                                      (17 (areturn)) 
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "usedInContractSeq"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "contractFlags" "java.text.RBCollationTables" (class "sun.text.IntHashtable")))) 
                                      (4 (iload_1)) 
                                      (5 (invokevirtual (methodCP "get" "sun.text.IntHashtable" (int) int))) 
                                      (8 (iconst_1)) 
                                      (9 (if_icmpne 16))  ;;to TAG_0
                                      (12 (iconst_1)) 
                                      (13 (goto 17)) ;;to TAG_1
                                      (16 (iconst_0)) ;;at TAG_0
                                      (17 (ireturn)) ;;at TAG_1
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMaxExpansion"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 71)
                                   (parsedcode
                                      (0 (iconst_1)) 
                                      (1 (istore_2)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "expandTable" "java.text.RBCollationTables" (class "java.util.Vector")))) 
                                      (6 (ifnull 69)) ;;to TAG_0
                                      (9 (iconst_0)) 
                                      (10 (istore_3)) 
                                      (11 (iload_3)) ;;at TAG_2
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "expandTable" "java.text.RBCollationTables" (class "java.util.Vector")))) 
                                      (16 (invokevirtual (methodCP "size" "java.util.Vector" () int))) 
                                      (19 (if_icmpge 69)) ;;to TAG_0
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "expandTable" "java.text.RBCollationTables" (class "java.util.Vector")))) 
                                      (26 (iload_3)) 
                                      (27 (invokevirtual (methodCP "elementAt" "java.util.Vector" (int) (class "java.lang.Object")))) 
                                      (30 (checkcast (array int))) 
                                      (33 (checkcast (array int))) 
                                      (36 (astore 4)) 
                                      (38 (aload 4)) 
                                      (40 (arraylength)) 
                                      (41 (istore 5)) 
                                      (43 (iload 5)) 
                                      (45 (iload_2)) 
                                      (46 (if_icmple 63)) ;;to TAG_1
                                      (49 (aload 4)) 
                                      (51 (iload 5)) 
                                      (53 (iconst_1)) 
                                      (54 (isub)) 
                                      (55 (iaload)) 
                                      (56 (iload_1)) 
                                      (57 (if_icmpne 63)) ;;to TAG_1
                                      (60 (iload 5)) 
                                      (62 (istore_2)) 
                                      (63 (iinc 3 1)) ;;at TAG_1
                                      (66 (goto 11))  ;;to TAG_2
                                      (69 (iload_2)) ;;at TAG_0
                                      (70 (ireturn)) 
                                      (endofcode 71))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getExpandValueList"
                              (parameters int)
                              (returntype . (array int))
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "expandTable" "java.text.RBCollationTables" (class "java.util.Vector"))))
                                      (4 (iload_1))
                                      (5 (ldc 0))         ;;INT:: "2113929216"
                                      (7 (isub))
                                      (8 (invokevirtual
					(methodCP "elementAt" "java.util.Vector" (int) (class "java.lang.Object"))))
                                      (11 (checkcast (array int)))
                                      (14 (checkcast (array int)))
                                      (17 (areturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getUnicodeOrder"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "mapping" "java.text.RBCollationTables" (class "sun.text.UCompactIntArray"))))
                                      (4 (iload_1))
                                      (5 (invokevirtual
					(methodCP "elementAt" "sun.text.UCompactIntArray" (int) int)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMaxSecOrder"
                              (parameters )
                              (returntype . short)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "maxSecOrder" "java.text.RBCollationTables" short)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMaxTerOrder"
                              (parameters )
                              (returntype . short)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "maxTerOrder" "java.text.RBCollationTables" short)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reverse"
                              (parameters (class "java.lang.StringBuffer") int int)
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 49)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (istore_3)) 
                                      (2 (iload_2)) 
                                      (3 (iconst_1)) 
                                      (4 (isub)) 
                                      (5 (istore 5)) 
                                      (7 (iload_3)) ;;at TAG_1
                                      (8 (iload 5)) 
                                      (10 (if_icmpge 48))  ;;to TAG_0
                                      (13 (aload_0)) 
                                      (14 (iload_3)) 
                                      (15 (invokevirtual (methodCP "charAt" "java.lang.StringBuffer" (int) char))) 
                                      (18 (istore 4)) 
                                      (20 (aload_0)) 
                                      (21 (iload_3)) 
                                      (22 (aload_0)) 
                                      (23 (iload 5)) 
                                      (25 (invokevirtual (methodCP "charAt" "java.lang.StringBuffer" (int) char))) 
                                      (28 (invokevirtual (methodCP "setCharAt" "java.lang.StringBuffer" (int char) void))) 
                                      (31 (aload_0)) 
                                      (32 (iload 5)) 
                                      (34 (iload 4)) 
                                      (36 (invokevirtual (methodCP "setCharAt" "java.lang.StringBuffer" (int char) void))) 
                                      (39 (iinc 3 1)) 
                                      (42 (iinc 5 -1)) 
                                      (45 (goto 7)) ;;to TAG_1
                                      (48 (return)) ;;at TAG_0
                                      (endofcode 49))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getEntry"
                              (parameters (class "java.util.Vector") (class "java.lang.String") boolean)
                              (returntype . int)
                              (accessflags  *class*  *final*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 51)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_3)) 
                                      (2 (iload_3)) ;;at TAG_2
                                      (3 (aload_0)) 
                                      (4 (invokevirtual (methodCP "size" "java.util.Vector" () int))) 
                                      (7 (if_icmpge 49)) ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (iload_3)) 
                                      (12 (invokevirtual (methodCP "elementAt" "java.util.Vector" (int) (class "java.lang.Object")))) 
                                      (15 (checkcast (class "java.text.EntryPair"))) 
                                      (18 (astore 4)) 
                                      (20 (aload 4)) 
                                      (22 (getfield (fieldCP "fwd" "java.text.EntryPair" boolean))) 
                                      (25 (iload_2)) 
                                      (26 (if_icmpne 43)) ;;to TAG_1
                                      (29 (aload 4)) 
                                      (31 (getfield (fieldCP "entryName" "java.text.EntryPair" (class "java.lang.String")))) 
                                      (34 (aload_1)) 
                                      (35 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (38 (ifeq 43)) ;;to TAG_1
                                      (41 (iload_3)) 
                                      (42 (ireturn)) 
                                      (43 (iinc 3 1)) ;;at TAG_1
                                      (46 (goto 2))  ;;to TAG_2
                                      (49 (iconst_m1)) ;;at TAG_0
                                      (50 (ireturn)) 
                                      (endofcode 51))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$102"
                              (parameters (class "java.text.RBCollationTables") boolean)
                              (returntype . boolean)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (dup_x1))
                                      (3 (putfield (fieldCP "frenchSec" "java.text.RBCollationTables" boolean)))
                                      (6 (ireturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$202"
                              (parameters (class "java.text.RBCollationTables") boolean)
                              (returntype . boolean)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (dup_x1))
                                      (3 (putfield (fieldCP "seAsianSwapping" "java.text.RBCollationTables" boolean)))
                                      (6 (ireturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$302"
                              (parameters (class "java.text.RBCollationTables") (class "sun.text.UCompactIntArray"))
                              (returntype . (class "sun.text.UCompactIntArray"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (dup_x1))
                                      (3 (putfield (fieldCP "mapping" "java.text.RBCollationTables" (class "sun.text.UCompactIntArray"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$402"
                              (parameters (class "java.text.RBCollationTables") (class "java.util.Vector"))
                              (returntype . (class "java.util.Vector"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (dup_x1))
                                      (3 (putfield (fieldCP "contractTable" "java.text.RBCollationTables" (class "java.util.Vector"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$502"
                              (parameters (class "java.text.RBCollationTables") (class "java.util.Vector"))
                              (returntype . (class "java.util.Vector"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (dup_x1))
                                      (3 (putfield (fieldCP "expandTable" "java.text.RBCollationTables" (class "java.util.Vector"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$602"
                              (parameters (class "java.text.RBCollationTables") (class "sun.text.IntHashtable"))
                              (returntype . (class "sun.text.IntHashtable"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (dup_x1))
                                      (3 (putfield (fieldCP "contractFlags" "java.text.RBCollationTables" (class "sun.text.IntHashtable"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$702"
                              (parameters (class "java.text.RBCollationTables") short)
                              (returntype . short)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (dup_x1))
                                      (3 (putfield (fieldCP "maxSecOrder" "java.text.RBCollationTables" short)))
                                      (6 (ireturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$802"
                              (parameters (class "java.text.RBCollationTables") short)
                              (returntype . short)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (dup_x1))
                                      (3 (putfield (fieldCP "maxTerOrder" "java.text.RBCollationTables" short)))
                                      (6 (ireturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *RBCollationTables-class-table*
  (make-static-class-decls 
   *java.text.RBCollationTables*))

(defconst *package-name-map* 
  ("java.text.RBCollationTables" . "java.text"))
