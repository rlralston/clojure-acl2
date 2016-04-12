#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")
(include-book "../lang/Number")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *java.math.BigInteger*
  (make-class-decl
   "java.math.BigInteger"  ; class name
   '("java.lang.Number" "java.lang.Object")   ; superclass
   ; interfaces
   '()   
   ; fields
   '("signum"              ; int  
     "mag"                 ; (array int) 
     "bitCount"            ; int
     "bitLength"           ; int 
     "lowestSetBit"        ; int 
     "firstNonzeroIntNum") ; int          
   ; static fields                       
   '("LONG_MASK"
     "bitsPerDigit"
     "SMALL_PRIME_THRESHOLD"
     "DEFAULT_PRIME_CERTAINTY"
     "SMALL_PRIME_PRODUCT"
     "staticRandom"
     "MAX_CONSTANT"
     "posConst"
     "negConst"
     "ZERO"
     "ONE"
     "TWO"
     "TEN"
     "bnExpModThreshTable"
     "zeros"
     "digitsPerLong"
     "longRadix"
     "digitsPerInt"
     "intRadix"
     "serialVersionUID"
     "serialPersistentFields"
     "unsafe"
     "signumOffset"
     "magOffset"
     "$assertionsDisabled")     
   ; constant pool (Seems to be numbered by index)
   '((LONG 4294967295)
    (INT 95)
    (INT 100)
    (INT 16)
    (LONG -8287574255936472291)
    (STRING "Zero length BigInteger")
    (STRING "Invalid signum value")
    (STRING "signum-magnitude mismatch")
    (STRING "Radix out of range")
    (STRING "Illegal embedded sign character")
    (STRING "Illegal digit")
    (STRING "numBits must be non-negative")
    (LONG 7)
    (LONG 8)
    (STRING "bitLength < 2")
    (LONG 3)
    (LONG 5)
    (LONG 11)
    (LONG 13)
    (LONG 17)
    (LONG 19)
    (LONG 23)
    (LONG 29)
    (LONG 31)
    (LONG 37)
    (LONG 41)
    (STRING "start < 0: ")
    (INT 2147483646)
    (LONG 16)
    (LONG -16)
    (LONG -9223372036854775808)
    (STRING "Negative exponent")
    (STRING "BigInteger: modulus not positive")
    (INT 65537)
    (INT -2147483648)
    (STRING "Shift distance of Integer.MIN_VALUE not supported.")
    (STRING "Negative bit address")
    (STRING "0")
    (STRING "signum")
    (STRING "magnitude")
    (STRING "BigInteger: Invalid signum value")
    (STRING "BigInteger: Signum not present in stream")
    (STRING "BigInteger: signum-magnitude mismatch")
    (STRING "BigInteger: Magnitude not present in stream")
    (STRING "bitCount")
    (STRING "bitLength")
    (STRING "lowestSetBit")
    (STRING "firstNonzeroByteNum")
    (LONG 1024)
    (LONG 1624)
    (LONG 2048)
    (LONG 2378)
    (LONG 2648)
    (LONG 2875)
    (LONG 3072)
    (LONG 3247)
    (LONG 3402)
    (LONG 3543)
    (LONG 3672)
    (LONG 3790)
    (LONG 3899)
    (LONG 4001)
    (LONG 4096)
    (LONG 4186)
    (LONG 4271)
    (LONG 4350)
    (LONG 4426)
    (LONG 4498)
    (LONG 4567)
    (LONG 4633)
    (LONG 4696)
    (LONG 4756)
    (LONG 4814)
    (LONG 4870)
    (LONG 4923)
    (LONG 4975)
    (LONG 5025)
    (LONG 5074)
    (LONG 5120)
    (LONG 5166)
    (LONG 5210)
    (LONG 5253)
    (LONG 5295)
    (LONG 152125131763605)
    (LONG 2)
    (LONG 10)
    (INT 2147483647)    
    (STRING "000000000000000000000000000000000000000000000000000000000000000")
    (LONG 4611686018427387904)
    (LONG 4052555153018976267)
    (LONG 7450580596923828125)
    (LONG 4738381338321616896)
    (LONG 3909821048582988049)
    (LONG 1152921504606846976)
    (LONG 1350851717672992089)
    (LONG 1000000000000000000)
    (LONG 5559917313492231481)
    (LONG 2218611106740436992)
    (LONG 8650415919381337933)
    (LONG 2177953337809371136)
    (LONG 6568408355712890625)
    (LONG 2862423051509815793)
    (LONG 6746640616477458432)
    (LONG 799006685782884121)
    (LONG 1638400000000000000)
    (LONG 3243919932521508681)
    (LONG 6221821273427820544)
    (LONG 504036361936467383)
    (LONG 876488338465357824)
    (LONG 1490116119384765625)
    (LONG 2481152873203736576)
    (LONG 6502111422497947648)
    (LONG 353814783205469041)
    (LONG 531441000000000000)
    (LONG 787662783788549761)
    (LONG 1667889514952984961)
    (LONG 2386420683693101056)
    (LONG 3379220508056640625)
    (INT 1073741824)
    (INT 1162261467)
    (INT 1220703125)
    (INT 362797056)
    (INT 1977326743)
    (INT 387420489)
    (INT 1000000000)
    (INT 214358881)
    (INT 429981696)
    (INT 815730721)
    (INT 1475789056)
    (INT 170859375)
    (INT 268435456)
    (INT 410338673)
    (INT 612220032)
    (INT 893871739)
    (INT 1280000000)
    (INT 1801088541)
    (INT 113379904)
    (INT 148035889)
    (INT 191102976)
    (INT 244140625)
    (INT 308915776)
    (INT 481890304)
    (INT 594823321)
    (INT 729000000)
    (INT 887503681)
    (INT 1291467969)
    (INT 1544804416)
    (INT 1838265625)
    (INT 60466176)
    (STRING "mag"))                               
   (list ; methods
    ; private constructor (int[], int)
    '("<init>" (|INT[]| INT)                           
      (ALOAD_0)                                      ; 0 : aload_0
      (INVOKESPECIAL "java.lang.Number" "<init>" 0)  ; 1 : invokespecial #796 // Method java/lang/Number."<init>":()V
      (ALOAD_0)                                      ; 4 : aload_0
      (ALOAD_1)                                      ; 5 : aload_1
      (ARRAYLENGTH)                                  ; 6 : arraylength
      (IFNE 7)                                       ; 7 : ifne          14
      (ICONST_0)                                     ; 10: iconst_0
      (GOTO 4)                                       ; 11: goto          15
      (ILOAD_2)                                      ; 14: iload_2
      (PUTFIELD "java.math.BigInteger" "signum" nil) ; 15: putfield      #746                // Field signum:I
      (ALOAD_0)                                      ; 18: aload_0       
      (ALOAD_1)                                      ; 19: aload_1       
      (PUTFIELD "java.math.BigInteger" "mag" nil)    ; 20: putfield      #754                // Field mag:[I
      (RETURN))                                      ; 23: return

    '("<init>_long" (LONG)
      (ALOAD_0)
      (INVOKESPECIAL "java.lang.Number" "<init>" 0)
      (LLOAD_1)
      (LCONST_0)
      (LCMP)
      (IFGE 14)
      (LLOAD_1)
      (LNEG)
      (LSTORE_1)
      (ALOAD_0)
      (ICONST_M1)
      (PUTFIELD "java.math.BigInteger" "signum" NIL)
      (GOTO 8)
      (ALOAD_0)
      (ICONST_1)
      (PUTFIELD "java.math.BigInteger" "signum" NIL)
      (LLOAD_1)
      (BIPUSH 32)
      (LUSHR)
      (L2I)
      (ISTORE_3)
      (ILOAD_3)
      (IFNE 21)
      (ALOAD_0)
      (ICONST_1)
      (NEWARRAY)
      (PUTFIELD "java.math.BigInteger" "mag" NIL)
      (ALOAD_0)
      (GETFIELD "java.math.BigInteger" "mag" NIL)
      (ICONST_0)
      (LLOAD_1)
      (L2I)
      (IASTORE)
      (GOTO 25)
      (ALOAD_0)
      (ICONST_2)
      (NEWARRAY)
      (PUTFIELD "java.math.BigInteger" "mag" NIL)
      (ALOAD_0)
      (GETFIELD "java.math.BigInteger" "mag" NIL)
      (ICONST_0)
      (ILOAD_3)
      (IASTORE)
      (ALOAD_0)
      (GETFIELD "java.math.BigInteger" "mag" NIL)
      (ICONST_1)
      (LLOAD_1)
      (L2I)
      (IASTORE)
      (RETURN))    
    
    '("signInt" ()                          
      (ALOAD_0)                                      ; 0: aload_0    
      (GETFIELD "java.math.BigInteger" "signum" nil) ; 1: getfield "signum" "java.math.BigInteger" 
      (IFGE 7)                                       ; 4: ifge 11  ;;to TAG_0
      (ICONST_M1)                                    ; 7: iconst_m1 
      (GOTO 4)                                       ; 8: goto 12 ;;to TAG_1
      (ICONST_0)                                     ; 11: iconst_0 ;;at TAG_0
      (IRETURN))                                     ; 12: ireturn ;;at TAG_1
    
    '("firstNonzeroIntNum" ()                          
      (ALOAD_0)                                                  ; 0: aload_0
      (GETFIELD "java.math.BigInteger" "firstNonzeroIntNum" nil) ; 1: getfield "firstNonzeroIntNum" "java.math.BigInteger"       
      (ICONST_2)                                                 ; 4: iconst_2 
      (ISUB)                                                     ; 5: isub 
      (ISTORE_1)                                                 ; 6: istore_1 
      (ILOAD_1)                                                  ; 7: iload_1 
      (BIPUSH -2)                                                ; 8: bipush -2 
      (IF_CMPNE 47)                                              ; 10: if_icmpne 57 ;;to TAG_0
      (ICONST_0)                                                 ; 13: iconst_0 
      (ISTORE_1)                                                 ; 14: istore_1 
      (ALOAD_0)                                                  ; 15: aload_0 
      (GETFIELD "java.math.BigInteger" "mag" nil)                ; 16: getfield "mag" "java.math.BigInteger" 
      (ARRAYLENGTH)                                              ; 19: arraylength 
      (ISTORE_3)                                                 ; 20: istore_3 
      (ILOAD_3)                                                  ; 21: iload_3 
      (ICONST_1)                                                 ; 22: iconst_1 
      (ISUB)                                                     ; 23: isub 
      (ISTORE_2)                                                 ; 24: istore_2 
      (ILOAD_2)                                                  ; 25: iload_2 ;;at TAG_2
      (IFLT 18)                                                  ; 26: iflt 44 ;;to TAG_1
      (ALOAD_0)                                                  ; 29: aload_0 
      (GETFIELD "java.math.BigInteger" "mag" nil)                ; 30: getfield "mag" "java.math.BigInteger" 
      (ILOAD_2)                                                  ; 33: iload_2 
      (IALOAD)                                                   ; 34: iaload 
      (IFNE 9)                                                   ; 35: ifne 44 ;;to TAG_1
      (IINC 2 -1)                                                ; 38: iinc 2 -1 
      (GOTO -16)                                                 ; 41: goto 25  ;;to TAG_2
      (ILOAD_3)                                                  ; 44: iload_3 ;;at TAG_1
      (ILOAD_2)                                                  ; 45: iload_2 
      (ISUB)                                                     ; 46: isub 
      (ICONST_1)                                                 ; 47: iconst_1 
      (ISUB)                                                     ; 48: isub 
      (ISTORE_1)                                                 ; 49: istore_1 
      (ALOAD_0)                                                  ; 50: aload_0 
      (ILOAD_1)                                                  ; 51: iload_1        
      (ICONST_2)                                                 ; 52: iconst_2          
      (IADD)                                                     ; 53: iadd 
      (PUTFIELD "java.math.BigInteger" "firstNonzeroIntNum" nil) ; 54: putfield "firstNonzeroIntNum" "java.math.BigInteger" 
      (ILOAD_1)                                                  ; 57: iload_1 ;;at TAG_0
      (IRETURN))                                                 ; 58: ireturn 
      
      
    '("getInt" (INT)                          
      (ILOAD_1)                                                       ; 0: iload_1    
      (IFGE 5)                                                        ; 1: ifge 6 ;;to TAG_0
      (ICONST_0)                                                      ; 4: iconst_0 
      (IRETURN)                                                       ; 5: ireturn 
      (ILOAD_1)                                                       ; 6: iload_1 
      (ALOAD_0)                                                       ; 7: aload_0 
      (GETFIELD "java.math.BigInteger" "mag" nil)                     ; 8: getfield "mag" "java.math.BigInteger"  
      (ARRAYLENGTH)                                                   ; 11: arraylength 
      (IF_ICMPLT 8)                                                   ; 12: if_icmplt 20 ;;to TAG_1
      (ALOAD_0)                                                       ; 15: aload_0 
      (INVOKESPECIAL "java.math.BigInteger" "signInt" 0)              ; 16: invokespecial "signInt" "java.math.BigInteger" () int))) 
      (IRETURN)                                                       ; 19: ireturn 
      (ALOAD_0)                                                       ; 20: aload_0 ;;at TAG_1
      (GETFIELD "java.math.BigInteger" "mag" nil)                     ; 21: (getfield getfield "mag" "java.math.BigInteger"  
      (ALOAD_0)                                                       ; 24: aload_0
      (GETFIELD "java.math.BigInteger" "mag" nil)                     ; 25: getfield "mag" "java.math.BigInteger"       
      (ARRAYLENGTH)                                                   ; 28: arraylength 
      (ILOAD_1)                                                       ; 29: iload_1 
      (ISUB)                                                          ; 30: isub 
      (ICONST_1)                                                      ; 31: iconst_1 
      (ISUB)                                                          ; 32: isub 
      (IALOAD)                                                        ; 33: iaload 
      (ISTORE_2)                                                      ; 34: istore_2 
      (ALOAD_0)                                                       ; 35: aload_0 
      (GETFIELD "java.math.BigInteger" "signum" nil)                  ; 36: getfield "signum" "java.math.BigInteger" 
      (IFLT 7)                                                        ; 39: iflt 46  ;;to TAG_2
      (ILOAD_2)                                                       ; 42: iload_2 
      (GOTO 19)                                                       ; 43: goto 62 ;;to TAG_3
      (ILOAD_1)                                                       ; 46: iload_1 ;;at TAG_2
      (ALOAD_0)                                                       ; 47: aload_0 
      (INVOKESPECIAL "java.math.BigInteger" "firstNonzeroIntNum" 0)   ; 48: invokespecial "firstNonzeroIntNum" "java.math.BigInteger" () int))) 
      (IF_ICMPGT 8)                                                   ; 51: if_icmpgt 59 ;;to TAG_4
      (ILOAD_2)                                                       ; 54: iload_2 
      (INEG)                                                          ; 55: ineg 
      (GOTO 6)                                                        ; 56: goto 62 ;;to TAG_3
      (ILOAD_2)                                                       ; 59: iload_2 ;;at TAG_4
      (ICONST_M1)                                                     ; 60: iconst_m1 
      (IXOR)                                                          ; 61: ixor 
      (IRETURN))                                                      ; 62: ireturn ;;at TAG_3    
      
    '("longValue" ()                          
      (LCONST_0)                                          ; 0: lconst_0
      (LSTORE_1)                                          ; 1: lstore_1      
      (ICONST_1)                                          ; 2: iconst_1      
      (ISTORE_3)                                          ; 3: istore_3
      (ILOAD_3)                                           ; 4: iload_3
      (IFLT 25)                                           ; 5: iflt          30
      (LLOAD_1)                                           ; 8: lload_1
      (BIPUSH 32)                                         ; 9: bipush        32
      (LSHL)                                              ; 11: lshl          
      (ALOAD_0)                                           ; 12: aload_0       
      (ILOAD_3)                                           ; 13: iload_3       
      (INVOKESPECIAL "java.math.BigInteger" "getInt" 1)   ; 14: invokespecial #820                // Method getInt:(I)I
      (I2L)                                               ; 17: i2l           
      (LDC2_W 0)                                          ; 18: ldc2_w        #437                // long 4294967295l
      (LAND)                                              ; 21: land          
      (LADD)                                              ; 22: ladd          
      (LSTORE_1)                                          ; 23: lstore_1      
      (IINC 3 -1)                                         ; 24: iinc          3, -1
      (GOTO -23)                                          ; 27: goto          4
      (LLOAD_1)                                           ; 30: lload_1       
      (LRETURN))                                          ; 31: lreturn    

    '("compareMagnitude" (java.math.BigInteger)                          
      (ALOAD_0)                                   ; 0: aload_0       
      (GETFIELD "java.math.BigInteger" "mag" nil) ; 1: getfield      #754                // Field mag:[I
      (ASTORE_2)                                  ; 4: astore_2      
      (ALOAD_2)                                   ; 5: aload_2       
      (ARRAYLENGTH)                               ; 6: arraylength   
      (ISTORE_3)                                  ; 7: istore_3      
      (ALOAD_1)                                   ; 8: aload_1       
      (GETFIELD "java.math.BigInteger" "mag" nil) ; 9: getfield      #754                // Field mag:[I
      (ASTORE 4)                                  ; 12: astore        4
      (ALOAD 4)                                   ; 14: aload         4
      (ARRAYLENGTH)                               ; 16: arraylength   
      (ISTORE 5)                                  ; 17: istore        5
      (ILOAD_3)                                   ; 19: iload_3       
      (ILOAD 5)                                   ; 20: iload         5
      (IF_ICMPGE 5)                               ; 22: if_icmpge     27
      (ICONST_M1)                                 ; 25: iconst_m1     
      (IRETURN)                                   ; 26: ireturn       
      (ILOAD_3)                                   ; 27: iload_3       
      (ILOAD 5)                                   ; 28: iload         5
      (IF_ICMPLE 5)                               ; 30: if_icmple     35
      (ICONST_1)                                  ; 33: iconst_1      
      (IRETURN)                                   ; 34: ireturn       
      (ICONST_0)                                  ; 35: iconst_0      
      (ISTORE 6)                                  ; 36: istore        6
      (ILOAD 6)                                   ; 38: iload         6
      (ILOAD_3)                                   ; 40: iload_3       
      (IF_ICMPGE 53)                              ; 41: if_icmpge     94
      (ALOAD_2)                                   ; 44: aload_2       
      (ILOAD 6)                                   ; 45: iload         6
      (IALOAD)                                    ; 47: iaload        
      (ISTORE 7)                                  ; 48: istore        7
      (ALOAD 4)                                   ; 50: aload         4
      (ILOAD 6)                                   ; 52: iload         6
      (IALOAD)                                    ; 54: iaload        
      (ISTORE 8)                                  ; 55: istore        8
      (ILOAD 7)                                   ; 57: iload         7
      (ILOAD 8)                                   ; 59: iload         8
      (IF_ICMPEQ 27)                              ; 61: if_icmpeq     88
      (ILOAD 7)                                   ; 64: iload         7
      (I2L)                                       ; 66: i2l           
      (LDC2_W 0)                                  ; 67: ldc2_w        #437                // long 4294967295l
      (LAND)                                      ; 70: land          
      (ILOAD 8)                                   ; 71: iload         8
      (I2L)                                       ; 73: i2l           
      (LDC2_W 0)                                  ; 74: ldc2_w        #437                // long 4294967295l
      (LAND)                                      ; 77: land          
      (LCMP)                                      ; 78: lcmp          
      (IFGE)                                      ; 79: ifge          86
      (ICONST_M1)                                 ; 82: iconst_m1     
      (GOTO 87)                                   ; 83: goto          87
      (ICONST_1)                                  ; 86: iconst_1      
      (IRETURN)                                   ; 87: ireturn       
      (IINC 6 1)                                  ; 88: iinc          6, 1
      (GOTO 38)                                   ; 91: goto          38
      (ICONST_0)                                  ; 94: iconst_0      
      (IRETURN))                                  ; 95: ireturn             
    
    '("trustedStripLeadingZeroInts" (|INT[]|)                         
      (ALOAD_0)                                         ; 0: aload_0
      (ARRAYLENGTH)                                     ; 1: arraylength   
      (ISTORE_1)                                        ; 2: istore_1      
      (ICONST_0)                                        ; 3: iconst_0      
      (ISTORE_2)                                        ; 4: istore_2      
      (ILOAD_2)                                         ; 5: iload_2       
      (ILOAD_1)                                         ; 6: iload_1       
      (IF_ICMPGE 15)                                    ; 7: if_icmpge     22
      (ALOAD_0)                                         ; 10: aload_0       
      (ILOAD_2)                                         ; 11: iload_2       
      (IALOAD)                                          ; 12: iaload        
      (IFNE 9)                                          ; 13: ifne          22
      (IINC 2 1)                                        ; 16: iinc          2, 1
      (GOTO -14)                                        ; 19: goto          5
      (ILOAD_2)                                         ; 22: iload_2       
      (IFNE 7)                                          ; 23: ifne          30
      (ALOAD_0)                                         ; 26: aload_0       
      (GOTO 9)                                          ; 27: goto          36
      (ALOAD_0)                                         ; 30: aload_0       
      (ILOAD_2)                                         ; 31: iload_2       
      (ILOAD_1)                                         ; 32: iload_1       
      (INVOKESTATIC "java.util.Arrays" "copyOfRange" 3) ; 33: invokestatic  #893                // Method java/util/Arrays.copyOfRange:([III)[I
      (ARETURN))                                        ; 36: areturn        
      
    '("bitLengthForInt" (INT)                          
      (BIPUSH 32)                                                 ; 0: bipush        32
      (ILOAD_0)                                                   ; 2: iload_0       
      (INVOKESTATIC "java.lang.Integer" "numberOfLeadingZeros" 1) ; 3: invokestatic  #789                // Method java/lang/Integer.numberOfLeadingZeros:(I)I
      (ISUB)                                                      ; 6: isub          
      (IRETURN))                                                  ; 7: ireturn       

    '("bitLength" (INT)                          
      (aload_0)                                                 ; 0  : aload_0
      (getfield "java.math.BigInteger" "bitLength" nil)         ; 1  : getfield "bitLength" "java.math.BigInteger" int 
      (iconst_1)                                                ; 4  : iconst_1 
      (isub)                                                    ; 5  : isub 
      (istore_1)                                                ; 6  : istore_1 
      (iload_1)                                                 ; 7  : iload_1 
      (iconst_m1)                                               ; 8  : iconst_m1 
      (if_icmpne 129)                                           ; 9  : if_icmpne 138 ;;to TAG_0
      (aload_0)                                                 ; 12 : aload_0 
      (getfield "java.math.BigInteger" "mag" nil)               ; 13 : getfield "mag" "java.math.BigInteger" (array int) 
      (astore_2)                                                ; 16 : astore_2 
      (aload_2)                                                 ; 17 : aload_2 
      (arraylength)                                             ; 18 : arraylength 
      (istore_3)                                                ; 19 : istore_3 
      (iload_3)                                                 ; 20 : iload_3 
      (ifne 8)                                                  ; 21 : ifne 29  ;;to TAG_1
      (iconst_0)                                                ; 24 : iconst_0 
      (istore_1)                                                ; 25 : istore_1 
      (goto 105)                                                ; 26 : goto 131 ;;to TAG_2
      (iload_3)                                                 ; 29 : iload_3 ;;at TAG_1
      (iconst_1)                                                ; 30 : iconst_1 
      (isub)                                                    ; 31 : isub 
      (iconst_5)                                                ; 32 : iconst_5 
      (ishl)                                                    ; 33 : ishl 
      (aload_0)                                                 ; 34 : aload_0 
      (getfield "java.math.BigInteger" "mag" nil)               ; 35 : getfield "mag" "java.math.BigInteger" (array int) 
      (iconst_0)                                                ; 38 : iconst_0 
      (iaload)                                                  ; 39 : iaload 
      (invokestatic "java.math.BigInteger" "bitLengthForInt" 1) ; 40 : invokestatic (methodCP "bitLengthForInt" "java.math.BigInteger" (int) int) 
      (iadd)                                                    ; 43 : iadd 
      (istore 4)                                                ; 44 : istore 4 
      (aload_0)                                                 ; 46 : aload_0 
      (getfield "java.math.BigInteger" "signum" nil)            ; 47 : getfield "signum" "java.math.BigInteger" int 
      (ifge 78)                                                 ; 50 : ifge 128 ;;to TAG_3
      (aload_0)                                                 ; 53 : aload_0 
      (getfield "java.math.BigInteger" "mag" nil)               ; 54 : getfield "mag" "java.math.BigInteger" (array int) 
      (iconst_0)                                                ; 57 : iconst_0 
      (iaload)                                                  ; 58 : iaload 
      (invokestatic "java.lang.Integer" "bitCount" 1)           ; 59 : invokestatic "bitCount" "java.lang.Integer" (int) int 
      (iconst_1)                                                ; 62 : iconst_1 
      (if_icmpne 7)                                             ; 63 : if_icmpne 70 ;;to TAG_4
      (iconst_1)                                                ; 66 : iconst_1 
      (goto 4)                                                  ; 67 : goto 71 ;;to TAG_5
      (iconst_0)                                                ; 70 : iconst_0 ;;at TAG_4
      (istore 5)                                                ; 71 : istore 5 ;;at TAG_5
      (iconst_1)                                                ; 73 : iconst_1 
      (istore 6)                                                ; 74 : istore 6 
      (iload 6)                                                 ; 76 : iload 6 ;;at TAG_9
      (iload_3)                                                 ; 78 : iload_3 
      (if_icmpge 31)                                            ; 79 : if_icmpge 110 ;;to TAG_6
      (iload 5)                                                 ; 82 : iload 5 
      (ifeq 26)                                                 ; 84 : ifeq 110 ;;to TAG_6
      (aload_0)                                                 ; 87 : aload_0 
      (getfield "java.math.BigInteger" "mag" nil)               ; 88 : getfield "mag" "java.math.BigInteger" (array int) 
      (iload 6)                                                 ; 91 : iload 6 
      (iaload)                                                  ; 93 : iaload 
      (ifne 7)                                                  ; 94 : ifne 101 ;;to TAG_7
      (iconst_1)                                                ; 97 : iconst_1 
      (goto 4)                                                  ; 98 : goto 102 ;;to TAG_8
      (iconst_0)                                                ; 101: iconst_0 ;;at TAG_7
      (istore 5)                                                ; 102: istore 5 ;;at TAG_8
      (iinc 6 1)                                                ; 104: iinc 6 1 
      (goto 31)                                                 ; 107: goto 76 ;;to TAG_9
      (iload 5)                                                 ; 110: iload 5 ;;at TAG_6
      (ifeq 10)                                                 ; 112: ifeq 122 ;;to TAG_10
      (iload 4)                                                 ; 115: iload 4 
      (iconst_1)                                                ; 117: iconst_1 
      (isub)                                                    ; 118: isub 
      (goto 5)                                                  ; 119: goto 124 ;;to TAG_11
      (iload 4)                                                 ; 122: iload 4 ;;at TAG_10
      (istore_1)                                                ; 124: istore_1 ;;at TAG_11
      (goto 6)                                                  ; 125: goto 131 ;;to TAG_2
      (iload 4)                                                 ; 128: iload 4 ;;at TAG_3
      (istore_1)                                                ; 130: istore_1 
      (aload_0)                                                 ; 131: aload_0 ;;at TAG_2
      (iload_1)                                                 ; 132: iload_1 
      (iconst_1)                                                ; 133: iconst_1 
      (iadd)                                                    ; 134: iadd 
      (putfield "java.math.BigInteger" "bitLength" nil)         ; 135: putfield "bitLength" "java.math.BigInteger" int 
      (iload_1)                                                 ; 138: iload_1 ;;at TAG_0
      (ireturn))                                                ; 139: ireturn     
    
    '("add" (java.math.BigInteger)                          
      (ALOAD_1)                                                             ; 0: aload_1       
      (GETFIELD "java.math.BigInteger" "signum" nil)                        ; 1: getfield      #746                // Field signum:I
      (IFNE 5)                                                              ; 4: ifne          9
      (ALOAD_0)                                                             ; 7: aload_0       
      (ARETURN)                                                             ; 8: areturn       
      (ALOAD_0)                                                             ; 9: aload_0       
      (GETFIELD "java.math.BigInteger" "signum" nil)                        ; 10: getfield      #746                // Field signum:I
      (IFNE 5)                                                              ; 13: ifne          18
      (ALOAD_1)                                                             ; 16: aload_1       
      (ARETURN)                                                             ; 17: areturn       
      (ALOAD_1)                                                             ; 18: aload_1       
      (GETFIELD "java.math.BigInteger" "signum" nil)                        ; 19: getfield      #746                // Field signum:I
      (ALOAD_0)                                                             ; 22: aload_0       
      (GETFIELD "java.math.BigInteger" "signum" nil)                        ; 23: getfield      #746                // Field signum:I
      (IF_ICMPNE 26)                                                        ; 26: if_icmpne     52
      (NEW "java.math.BigInteger")                                          ; 29: new           #535                // class java/math/BigInteger
      (DUP)                                                                 ; 32: dup           
      (ALOAD_0)                                                             ; 33: aload_0       
      (GETFIELD "java.math.BigInteger" "mag" nil)                           ; 34: getfield      #754                // Field mag:[I
      (ALOAD_1)                                                             ; 37: aload_1       
      (GETFIELD "java.math.BigInteger" "mag" nil)                           ; 38: getfield      #754                // Field mag:[I
      (INVOKESTATIC "java.math.BigInteger" "add_ia.ia" 2)                   ; 41: invokestatic  #842                // Method add:([I[I)[I
      (ALOAD_0)                                                             ; 44: aload_0       
      (GETFIELD "java.math.BigInteger" "signum" nil)                        ; 45: getfield      #746                // Field signum:I
      (INVOKESPECIAL "java.math.BigInteger" "<init>" 2)                     ; 48: invokespecial #834                // Method "<init>":([II)V
      (ARETURN)                                                             ; 51: areturn       
      (ALOAD_0)                                                             ; 52: aload_0       
      (ALOAD_1)                                                             ; 53: aload_1       
      (INVOKESPECIAL "java.math.BigInteger" "compareMagnitude" 1)           ; 54: invokevirtual #861                // Method compareMagnitude:(Ljava/math/BigInteger;)I
      (ISTORE_2)                                                            ; 57: istore_2      
      (ILOAD_2)                                                             ; 58: iload_2       
      (IFNE 7)                                                              ; 59: ifne          66
      (GETSTATIC "java.math.BigInteger" "ZERO" nil)                         ; 62: getstatic     #762                // Field ZERO:Ljava/math/BigInteger;
      (ARETURN)                                                             ; 65: areturn       
      (ILOAD_2)                                                             ; 66: iload_2       
      (IFLE 17)                                                             ; 67: ifle          84
      (ALOAD_0)                                                             ; 70: aload_0       
      (GETFIELD "java.math.BigInteger" "mag" nil)                           ; 71: getfield      #754                // Field mag:[I
      (ALOAD_1)                                                             ; 74: aload_1       
      (GETFIELD "java.math.BigInteger" "mag" nil)                           ; 75: getfield      #754                // Field mag:[I
      (INVOKESTATIC "java.math.BigInteger" "subtract" 2)                    ; 78: invokestatic  #843                // Method subtract:([I[I)[I
      (GOTO 95)                                                             ; 81: goto          95
      (ALOAD_1)                                                             ; 84: aload_1       
      (GETFIELD "java.math.BigInteger" "mag" nil)                           ; 85: getfield      #754                // Field mag:[I
      (ALOAD_0)                                                             ; 88: aload_0       
      (GETFIELD "java.math.BigInteger" "mag" nil)                           ; 89: getfield      #754                // Field mag:[I
      (INVOKESTATIC "java.math.BigInteger" "subtract" 2)                    ; 92: invokestatic  #843                // Method subtract:([I[I)[I
      (ASTORE_3)                                                            ; 95: astore_3      
      (ALOAD_3)                                                             ; 96: aload_3       
      (INVOKESTATIC "java.math.BigInteger" "trustedStripLeadingZeroInts" 1) ; 97: invokestatic  #832                // Method trustedStripLeadingZeroInts:([I)[I
      (ASTORE_3)                                                            ; 100: astore_3      
      (NEW "java.math.BigInteger")                                          ; 101: new           #535                // class java/math/BigInteger
      (DUP)                                                                 ; 104: dup           
      (ALOAD_3)                                                             ; 105: aload_3       
      (ILOAD_2)                                                             ; 106: iload_2       
      (ALOAD_0)                                                             ; 107: aload_0       
      (GETFIELD "java.math.BigInteger" "signum" nil)                        ; 108: getfield      #746                // Field signum:I
      (IF_ICMPNE 7)                                                         ; 111: if_icmpne     118
      (ICONST_1)                                                            ; 114: iconst_1      
      (GOTO 119)                                                            ; 115: goto          119
      (ICONST_M1)                                                           ; 118: iconst_m1     
      (INVOKESPECIAL "java.math.BigInteger" "<init>" 2)                     ; 119: invokespecial #834                // Method "<init>":([II)V
      (ARETURN))                                                            ; 122: areturn       
    
    '("add_ia.ia" (|INT[]| |INT[]|)
      (ALOAD_0)    ; 0: aload_0
      (ARRAYLENGTH)    ; 1: arraylength
      (ALOAD_1)    ; 2: aload_1
      (ARRAYLENGTH)    ; 3: arraylength
      (IF_ICMPGE 9)    ; 4: if_icmpge     13
      (ALOAD_0)    ; 7: aload_0
      (ASTORE_2)    ; 8: astore_2
      (ALOAD_1)    ; 9: aload_1
      (ASTORE_0)    ; 10: astore_0
      (ALOAD_2)    ; 11: aload_2
      (ASTORE_1)    ; 12: astore_1
      (ALOAD_0)    ; 13: aload_0
      (ARRAYLENGTH)    ; 14: arraylength
      (ISTORE_2)    ; 15: istore_2
      (ALOAD_1)    ; 16: aload_1
      (ARRAYLENGTH)    ; 17: arraylength
      (ISTORE_3)    ; 18: istore_3
      (ILOAD_2)    ; 19: iload_2
      (NEWARRAY)    ; 20: newarray       int
      (ASTORE 4)    ; 22: astore        4
      (LCONST_0)    ; 24: lconst_0
      (LSTORE 5)    ; 25: lstore        5
      
      ; First Loop      
      (ILOAD_3)    ; 27: iload_3
      (IFLE 44)    ; 28: ifle          72
      (ALOAD_0)    ; 31: aload_0
      (IINC 2 -1)    ; 32: iinc          2, -1
      (ILOAD_2)    ; 35: iload_2
      (IALOAD)    ; 36: iaload
      (I2L)    ; 37: i2l
      (LDC2_W 0)    ; 38: ldc2_w        #437                // long 4294967295l
      (LAND)    ; 41: land
      (ALOAD_1)    ; 42: aload_1
      (IINC 3 -1)    ; 43: iinc          3, -1
      (ILOAD_3)    ; 46: iload_3
      (IALOAD)    ; 47: iaload
      (I2L)    ; 48: i2l
      (LDC2_W 0)    ; 49: ldc2_w        #437                // long 4294967295l
      (LAND)    ; 52: land
      (LADD)    ; 53: ladd
      (LLOAD 5)    ; 54: lload         5
      (BIPUSH 32)    ; 56: bipush        32
      (LUSHR)    ; 58: lushr
      (LADD)    ; 59: ladd
      (LSTORE 5)    ; 60: lstore        5
      (ALOAD 4)    ; 62: aload         4
      (ILOAD_2)    ; 64: iload_2
      (LLOAD 5)    ; 65: lload         5
      (L2I)    ; 67: l2i
      (IASTORE)    ; 68: iastore
      (GOTO -42)    ; 69: goto          27
      
      ; boolean carry = (sum >>> 32 != 0)
      (LLOAD 5)    ; 72: lload         5
      (BIPUSH 32)    ; 74: bipush        32
      (LUSHR)    ; 76: lushr
      (LCONST_0)    ; 77: lconst_0
      (LCMP)    ; 78: lcmp
      (IFEQ 7)    ; 79: ifeq          86
      (ICONST_1)    ; 82: iconst_1
      (GOTO 4)    ; 83: goto          87
      (ICONST_0)    ; 86: iconst_0
      (ISTORE 7)    ; 87: istore        7
      
      ; Second loop
      (ILOAD_2)    ; 89: iload_2
      (IFLE 34)    ; 90: ifle          124
      (ILOAD 7)    ; 93: iload         7
      (IFEQ 29)    ; 95: ifeq          124
      (ALOAD 4)    ; 98: aload         4
      (IINC 2 -1)    ; 100: iinc          2, -1
      (ILOAD_2)    ; 103: iload_2
      (ALOAD_0)    ; 104: aload_0
      (ILOAD_2)    ; 105: iload_2
      (IALOAD)    ; 106: iaload
      (ICONST_1)    ; 107: iconst_1
      (IADD)    ; 108: iadd
      (DUP_X2)    ; 109: dup_x2
      (IASTORE)    ; 110: iastore
      (IFNE 7)    ; 111: ifne          118
      (ICONST_1)    ; 114: iconst_1
      (GOTO 4)    ; 115: goto          119
      (ICONST_0)    ; 118: iconst_0
      (ISTORE 7)    ; 119: istore        7
      (GOTO -32)    ; 121: goto          89
      
      ; Third loop
      (ILOAD_2)    ; 124: iload_2
      (IFLE 16)    ; 125: ifle          141
      (ALOAD 4)    ; 128: aload         4
      (IINC 2 -1)    ; 130: iinc          2, -1
      (ILOAD_2)    ; 133: iload_2
      (ALOAD_0)    ; 134: aload_0
      (ILOAD_2)    ; 135: iload_2
      (IALOAD)    ; 136: iaload
      (IASTORE)    ; 137: iastore
      (GOTO -14)    ; 138: goto          124
      
      ; if (carry)
      (ILOAD 7)    ; 141: iload         7
      (IFEQ 32)    ; 143: ifeq          175
      (ALOAD 4)    ; 146: aload         4
      (ARRAYLENGTH)    ; 148: arraylength
      (ICONST_1)    ; 149: iconst_1
      (IADD)    ; 150: iadd
      (NEWARRAY)    ; 151: newarray       int
      (ASTORE 8)    ; 153: astore        8
      (ALOAD 4)    ; 155: aload         4
      (ICONST_0)    ; 157: iconst_0
      (ALOAD 8)    ; 158: aload         8
      (ICONST_1)    ; 160: iconst_1
      (ALOAD 4)    ; 161: aload         4
      (ARRAYLENGTH)    ; 163: arraylength
      (INVOKESTATIC "java.lang.System" "arraycopy" 5)    ; 164: invokestatic  #809                // Method java/lang/System.arraycopy:(Ljava/lang/Object;ILjava/lang/Object;II)V
      (ALOAD 8)    ; 167: aload         8
      (ICONST_0)    ; 169: iconst_0
      (ICONST_1)    ; 170: iconst_1
      (IASTORE)    ; 171: iastore
      (ALOAD 8)    ; 172: aload         8
      (ARETURN)    ; 174: areturn
      
      ; Not carry
      (ALOAD 4)    ; 175: aload         4
      (ARETURN))

    '("subtract" (|INT[]| |INT[]|)
      (ALOAD_0)     ; 0: aload_0
      (ARRAYLENGTH) ; 1: arraylength   
      (ISTORE_2)    ; 2: istore_2      
      (ILOAD_2)     ; 3: iload_2       
      (NEWARRAY)    ; 4: newarray       int
      (ASTORE_3)    ; 6: astore_3      
      (ALOAD_1)     ; 7: aload_1       
      (ARRAYLENGTH) ; 8: arraylength   
      (ISTORE 4)    ; 9: istore        4
      (LCONST_0)    ; 11: lconst_0      
      (LSTORE 5)    ; 12: lstore        5
      (ILOAD 4)     ; 14: iload         4
      (IFLE 44)     ; 16: ifle          60
      (ALOAD_0)     ; 19: aload_0       
      (IINC 2 -1)   ; 20: iinc          2, -1
      (ILOAD_2)     ; 23: iload_2       
      (IALOAD)      ; 24: iaload        
      (I2L)         ; 25: i2l           
      (LDC2_W 0)    ; 26: ldc2_w        #437                // long 4294967295l
      (LAND)        ; 29: land          
      (ALOAD_1)     ; 30: aload_1       
      (IINC 4 -1)   ; 31: iinc          4, -1
      (ILOAD 4)     ; 34: iload         4
      (IALOAD)      ; 36: iaload        
      (I2L)         ; 37: i2l           
      (LDC2_W 0)    ; 38: ldc2_w        #437                // long 4294967295l
      (LAND)        ; 41: land          
      (LSUB)        ; 42: lsub          
      (LLOAD 5)     ; 43: lload         5
      (BIPUSH 32)   ; 45: bipush        32
      (LSHR)        ; 47: lshr          
      (LADD)        ; 48: ladd          
      (LSTORE 5)    ; 49: lstore        5
      (ALOAD_3)     ; 51: aload_3       
      (ILOAD_2)     ; 52: iload_2       
      (LLOAD 5)     ; 53: lload         5
      (L2I)         ; 55: l2i           
      (IASTORE)     ; 56: iastore       
      (GOTO -43)    ; 57: goto          14
      (LLOAD 5)     ; 60: lload         5
      (BIPUSH 32)   ; 62: bipush        32
      (LSHR)        ; 64: lshr          
      (LCONST_0)    ; 65: lconst_0      
      (LCMP)        ; 66: lcmp          
      (IFEQ 7)      ; 67: ifeq          74
      (ICONST_1)    ; 70: iconst_1      
      (GOTO 4)      ; 71: goto          75
      (ICONST_0)    ; 74: iconst_0      
      (ISTORE 7)    ; 75: istore        7
      (ILOAD_2)     ; 77: iload_2       
      (IFLE 34)     ; 78: ifle          112
      (ILOAD)       ; 81: iload         7
      (IFEQ 29)     ; 83: ifeq          112
      (ALOAD_3)     ; 86: aload_3       
      (IINC 2 -1)   ; 87: iinc          2, -1
      (ILOAD_2)     ; 90: iload_2       
      (ALOAD_0)     ; 91: aload_0       
      (ILOAD_2)     ; 92: iload_2       
      (IALOAD)      ; 93: iaload        
      (ICONST_1)    ; 94: iconst_1      
      (ISUB)        ; 95: isub          
      (DUP_X2)      ; 96: dup_x2        
      (IASTORE)     ; 97: iastore       
      (ICONST_M1)   ; 98: iconst_m1     
      (IF_ICMPNE 7) ; 99: if_icmpne     106
      (ICONST_1)    ; 102: iconst_1      
      (GOTO 4)      ; 103: goto          107
      (ICONST_0)    ; 106: iconst_0      
      (ISTORE 7)    ; 107: istore        7
      (GOTO -32)    ; 109: goto          77
      (ILOAD_2)     ; 112: iload_2       
      (IFLE 15)     ; 113: ifle          128
      (ALOAD_3)     ; 116: aload_3       
      (IINC 2 -1)   ; 117: iinc          2, -1
      (ILOAD_2)     ; 120: iload_2       
      (ALOAD_0)     ; 121: aload_0       
      (ILOAD_2)     ; 122: iload_2       
      (IALOAD)      ; 123: iaload        
      (IASTORE)     ; 124: iastore       
      (GOTO -13)    ; 125: goto          112
      (ALOAD_3)     ; 128: aload_3       
      (ARETURN))    ; 129: areturn    
    
    '("valueOf" (long)
      (lload_0)                                              ; 0 : lload_0
      (lconst_0)                                             ; 1 : lconst_0 
      (lcmp)                                                 ; 2 : lcmp 
      (ifne 7)                                               ; 3 : ifne 10 ;;to TAG_0
      (getstatic "java.math.BigInteger" "ZERO" nil)          ; 6 : getstatic "ZERO" "java.math.BigInteger" (class "java.math.BigInteger") 
      (areturn)                                              ; 9 : areturn 
      (lload_0)                                              ; 10: lload_0 ;;at TAG_0
      (lconst_0)                                             ; 11: lconst_0 
      (lcmp)                                                 ; 12: lcmp 
      (ifle 18)                                              ; 13: ifle 31 ;;to TAG_1
      (lload_0)                                              ; 16: lload_0 
      (ldc2_w 28)                                            ; 17: ldc2_w 28 ;; LONG:: "16"
      (lcmp)                                                 ; 20: lcmp 
      (ifgt 10)                                              ; 21: ifgt 31 ;;to TAG_1
      (getstatic "java.math.BigInteger" "posConst" nil)      ; 24: getstatic "posConst" "java.math.BigInteger" (array (class "java.math.BigInteger")) 
      (lload_0)                                              ; 27: lload_0 
      (l2i)                                                  ; 28: l2i 
      (aaload)                                               ; 29: aaload
      (areturn)                                              ; 30: areturn 
      (lload_0)                                              ; 31: lload_0 ;;at TAG_1
      (lconst_0)                                             ; 32: lconst_0 
      (lcmp)                                                 ; 33: lcmp 
      (ifge 19)                                              ; 34: ifge 53  ;;to TAG_2
      (lload_0)                                              ; 37: lload_0 
      (ldc2_w 29)                                            ; 38: ldc2_w 29 ;; LONG:: "-16"
      (lcmp)                                                 ; 41: lcmp 
      (iflt 11)                                              ; 42: iflt 53  ;;to TAG_2
      (getstatic "java.math.BigInteger" "negConst" nil)      ; 45: getstatic "negConst" "java.math.BigInteger" (array (class "java.math.BigInteger")) 
      (lload_0)                                              ; 48: lload_0 
      (lneg)                                                 ; 49: lneg 
      (l2i)                                                  ; 50: l2i 
      (aaload)                                               ; 51: aaload 
      (areturn)                                              ; 52: areturn 
      (new "java.math.BigInteger")                           ; 53: new "java.math.BigInteger" ;;at TAG_2
      (dup)                                                  ; 56: dup 
      (lload_0)                                              ; 57: lload_0
      (invokespecial "java.math.BigInteger" "<init>_long" 2) ; 58: invokespecial "<init>" "java.math.BigInteger" (long) void) 
      (areturn))                                             ; 61: areturn
    
    '("multiply" (java.math.BigInteger)
      (ALOAD_1)
      (GETFIELD "java.math.BigInteger" "signum" NIL)
      (IFEQ 10)
      (ALOAD_0)
      (GETFIELD "java.math.BigInteger" "signum" NIL)
      (IFNE 7)
      (GETSTATIC "java.math.BigInteger" "ZERO" NIL)
      (ARETURN)
      (ALOAD_0)
      (ALOAD_0)
      (GETFIELD "java.math.BigInteger" "mag" NIL)
      (ALOAD_0)
      (GETFIELD "java.math.BigInteger" "mag" NIL)
      (ARRAYLENGTH)
      (ALOAD_1)
      (GETFIELD "java.math.BigInteger" "mag" NIL)
      (ALOAD_1)
      (GETFIELD "java.math.BigInteger" "mag" NIL)
      (ARRAYLENGTH)
      (ACONST_NULL)
      (INVOKESPECIAL "java.math.BigInteger" "multiplyToLen" 5)
      (ASTORE_2)
      (ALOAD_2)
      (INVOKESTATIC "java.math.BigInteger" "trustedStripLeadingZeroInts" 1)
      (ASTORE_2)
      (NEW "java.math.BigInteger")
      (DUP)
      (ALOAD_2)
      (ALOAD_0)
      (GETFIELD "java.math.BigInteger" "signum" NIL)
      (ALOAD_1)
      (GETFIELD "java.math.BigInteger" "signum" NIL)
      (IF_ICMPNE 7)
      (ICONST_1)
      (GOTO 4)
      (ICONST_M1)
      (INVOKESPECIAL "java.math.BigInteger" "<init>" 2)
      (ARETURN))
    
    '("multiplyToLen" ((ARRAY INT) INT (ARRAY INT) INT (ARRAY INT))
      (ILOAD_2)
      (ICONST_1)
      (ISUB)
      (ISTORE 6)
      (ILOAD 4)
      (ICONST_1)
      (ISUB)
      (ISTORE 7)
      (ALOAD 5)
      (IFNULL 13)
      (ALOAD 5)
      (ARRAYLENGTH)
      (ILOAD_2)
      (ILOAD 4)
      (IADD)
      (IF_ICMPGE 11)
      (ILOAD_2)
      (ILOAD 4)
      (IADD)
      (NEWARRAY)
      (ASTORE 5)
      (LCONST_0)
      (LSTORE 8)
      (ILOAD 7)
      (ISTORE 10)
      (ILOAD 7)
      (ICONST_1)
      (IADD)
      (ILOAD 6)
      (IADD)
      (ISTORE 11)
      (ILOAD 10)
      (IFLT 51)
      (ALOAD_3)
      (ILOAD 10)
      (IALOAD)
      (I2L)
      (LDC2_W 0)
      (LAND)
      (ALOAD_1)
      (ILOAD 6)
      (IALOAD)
      (I2L)
      (LDC2_W 0)
      (LAND)
      (LMUL)
      (LLOAD 8)
      (LADD)
      (LSTORE 12)
      (ALOAD 5)
      (ILOAD 11)
      (LLOAD 12)
      (L2I)
      (IASTORE)
      (LLOAD 12)
      (BIPUSH 32)
      (LUSHR)
      (LSTORE 8)
      (IINC 10 -1)
      (IINC 11 -1)
      (GOTO -50)
      (ALOAD 5)
      (ILOAD 6)
      (LLOAD 8)
      (L2I)
      (IASTORE)
      (ILOAD 6)
      (ICONST_1)
      (ISUB)
      (ISTORE 10)
      (ILOAD 10)
      (IFLT 97)
      (LCONST_0)
      (LSTORE 8)
      (ILOAD 7)
      (ISTORE 11)
      (ILOAD 7)
      (ICONST_1)
      (IADD)
      (ILOAD 10)
      (IADD)
      (ISTORE 12)
      (ILOAD 11)
      (IFLT 62)
      (ALOAD_3)
      (ILOAD 11)
      (IALOAD)
      (I2L)
      (LDC2_W 0)
      (LAND)
      (ALOAD_1)
      (ILOAD 10)
      (IALOAD)
      (I2L)
      (LDC2_W 0)
      (LAND)
      (LMUL)
      (ALOAD 5)
      (ILOAD 12)
      (IALOAD)
      (I2L)
      (LDC2_W 0)
      (LAND)
      (LADD)
      (LLOAD 8)
      (LADD)
      (LSTORE 13)
      (ALOAD 5)
      (ILOAD 12)
      (LLOAD 13)
      (L2I)
      (IASTORE)
      (LLOAD 13)
      (BIPUSH 32)
      (LUSHR)
      (LSTORE 8)
      (IINC 11 -1)
      (IINC 12 -1)
      (GOTO -61)
      (ALOAD 5)
      (ILOAD 10)
      (LLOAD 8)
      (L2I)
      (IASTORE)
      (IINC 10 -1)
      (GOTO -96)
      (ALOAD 5)
      (ARETURN))
    
    '("<clinit>" ()
      ;(LDC_W)
      ;(INVOKEVIRTUAL "java.lang.Class" "desiredAssertionStatus" 0)
      ;(IFNE 7)
      ;(ICONST_1)
      ;(GOTO 4)
      ;(ICONST_0)
      ;(PUTSTATIC "java.math.BigInteger" "$assertionsDisabled" NIL)
      (BIPUSH 37)
      (NEWARRAY)
      (DUP)
      (ICONST_0)
      (LCONST_0)
      (LASTORE)
      (DUP)
      (ICONST_1)
      (LCONST_0)
      (LASTORE)
      (DUP)
      (ICONST_2)
      (LDC2_W 48)
      (LASTORE)
      (DUP)
      (ICONST_3)
      (LDC2_W 49)
      (LASTORE)
      (DUP)
      (ICONST_4)
      (LDC2_W 50)
      (LASTORE)
      (DUP)
      (ICONST_5)
      (LDC2_W 51)
      (LASTORE)
      (DUP)
      (BIPUSH 6)
      (LDC2_W 52)
      (LASTORE)
      (DUP)
      (BIPUSH 7)
      (LDC2_W 53)
      (LASTORE)
      (DUP)
      (BIPUSH 8)
      (LDC2_W 54)
      (LASTORE)
      (DUP)
      (BIPUSH 9)
      (LDC2_W 55)
      (LASTORE)
      (DUP)
      (BIPUSH 10)
      (LDC2_W 56)
      (LASTORE)
      (DUP)
      (BIPUSH 11)
      (LDC2_W 57)
      (LASTORE)
      (DUP)
      (BIPUSH 12)
      (LDC2_W 58)
      (LASTORE)
      (DUP)
      (BIPUSH 13)
      (LDC2_W 59)
      (LASTORE)
      (DUP)
      (BIPUSH 14)
      (LDC2_W 60)
      (LASTORE)
      (DUP)
      (BIPUSH 15)
      (LDC2_W 61)
      (LASTORE)
      (DUP)
      (BIPUSH 16)
      (LDC2_W 62)
      (LASTORE)
      (DUP)
      (BIPUSH 17)
      (LDC2_W 63)
      (LASTORE)
      (DUP)
      (BIPUSH 18)
      (LDC2_W 64)
      (LASTORE)
      (DUP)
      (BIPUSH 19)
      (LDC2_W 65)
      (LASTORE)
      (DUP)
      (BIPUSH 20)
      (LDC2_W 66)
      (LASTORE)
      (DUP)
      (BIPUSH 21)
      (LDC2_W 67)
      (LASTORE)
      (DUP)
      (BIPUSH 22)
      (LDC2_W 68)
      (LASTORE)
      (DUP)
      (BIPUSH 23)
      (LDC2_W 69)
      (LASTORE)
      (DUP)
      (BIPUSH 24)
      (LDC2_W 70)
      (LASTORE)
      (DUP)
      (BIPUSH 25)
      (LDC2_W 71)
      (LASTORE)
      (DUP)
      (BIPUSH 26)
      (LDC2_W 72)
      (LASTORE)
      (DUP)
      (BIPUSH 27)
      (LDC2_W 73)
      (LASTORE)
      (DUP)
      (BIPUSH 28)
      (LDC2_W 74)
      (LASTORE)
      (DUP)
      (BIPUSH 29)
      (LDC2_W 75)
      (LASTORE)
      (DUP)
      (BIPUSH 30)
      (LDC2_W 76)
      (LASTORE)
      (DUP)
      (BIPUSH 31)
      (LDC2_W 77)
      (LASTORE)
      (DUP)
      (BIPUSH 32)
      (LDC2_W 78)
      (LASTORE)
      (DUP)
      (BIPUSH 33)
      (LDC2_W 79)
      (LASTORE)
      (DUP)
      (BIPUSH 34)
      (LDC2_W 80)
      (LASTORE)
      (DUP)
      (BIPUSH 35)
      (LDC2_W 81)
      (LASTORE)
      (DUP)
      (BIPUSH 36)
      (LDC2_W 82)
      (LASTORE)
      (PUTSTATIC "java.math.BigInteger" "bitsPerDigit" NIL)
      (LDC2_W 83)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (PUTSTATIC "java.math.BigInteger" "SMALL_PRIME_PRODUCT" NIL)
      (BIPUSH 17)
      (ANEWARRAY "java.math.BigInteger")
      (PUTSTATIC "java.math.BigInteger" "posConst" NIL)
      (BIPUSH 17)
      (ANEWARRAY "java.math.BigInteger")
      (PUTSTATIC "java.math.BigInteger" "negConst" NIL)
      (ICONST_1)
      (ISTORE_0)
      (ILOAD_0)
      (BIPUSH 16)
      (IF_ICMPGT 45)
      (ICONST_1)
      (NEWARRAY)
      (ASTORE_1)
      (ALOAD_1)
      (ICONST_0)
      (ILOAD_0)
      (IASTORE)
      (GETSTATIC "java.math.BigInteger" "posConst" NIL)
      (ILOAD_0)
      (NEW "java.math.BigInteger")
      (DUP)
      (ALOAD_1)
      (ICONST_1)
      (INVOKESPECIAL "java.math.BigInteger" "<init>" 2)
      (AASTORE)
      (GETSTATIC "java.math.BigInteger" "negConst" NIL)
      (ILOAD_0)
      (NEW "java.math.BigInteger")
      (DUP)
      (ALOAD_1)
      (ICONST_M1)
      (INVOKESPECIAL "java.math.BigInteger" "<init>" 2)
      (AASTORE)
      (IINC 0 1)
      (GOTO -45)
      (NEW "java.math.BigInteger")
      (DUP)
      (ICONST_0)
      (NEWARRAY)
      (ICONST_0)
      (INVOKESPECIAL "java.math.BigInteger" "<init>" 2)
      (PUTSTATIC "java.math.BigInteger" "ZERO" NIL)
      (LCONST_1)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (PUTSTATIC "java.math.BigInteger" "ONE" NIL)
      (LDC2_W 84)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (PUTSTATIC "java.math.BigInteger" "TWO" NIL)
      (LDC2_W 85)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (PUTSTATIC "java.math.BigInteger" "TEN" NIL)
      (BIPUSH 7)
      (NEWARRAY)
      (DUP)
      (ICONST_0)
      (BIPUSH 7)
      (IASTORE)
      (DUP)
      (ICONST_1)
      (BIPUSH 25)
      (IASTORE)
      (DUP)
      (ICONST_2)
      (BIPUSH 81)
      (IASTORE)
      (DUP)
      (ICONST_3)
      (SIPUSH 241)
      (IASTORE)
      (DUP)
      (ICONST_4)
      (SIPUSH 673)
      (IASTORE)
      (DUP)
      (ICONST_5)
      (SIPUSH 1793)
      (IASTORE)
      (DUP)
      (BIPUSH 6)
      (LDC_W 86)
      (IASTORE)
      (PUTSTATIC "java.math.BigInteger" "bnExpModThreshTable" NIL)
      (BIPUSH 64)
      (ANEWARRAY "java.lang.String")
      (PUTSTATIC "java.math.BigInteger" "zeros" NIL)
      (GETSTATIC "java.math.BigInteger" "zeros" NIL)
      (BIPUSH 63)
      (LDC_W 87)
      (AASTORE)
      (ICONST_0)
      (ISTORE_0)
      (ILOAD_0)
      (BIPUSH 63)
      (IF_ICMPGE 25)
      (GETSTATIC "java.math.BigInteger" "zeros" NIL)
      (ILOAD_0)
      (GETSTATIC "java.math.BigInteger" "zeros" NIL)
      (BIPUSH 63)
      (AALOAD)
      (ICONST_0)
      (ILOAD_0)
      (INVOKEVIRTUAL "java.lang.String" "substring" 2)
      (AASTORE)
      (IINC 0 1)
      (GOTO -25)
      (BIPUSH 37)
      (NEWARRAY)
      (DUP)
      (ICONST_0)
      (ICONST_0)
      (IASTORE)
      (DUP)
      (ICONST_1)
      (ICONST_0)
      (IASTORE)
      (DUP)
      (ICONST_2)
      (BIPUSH 62)
      (IASTORE)
      (DUP)
      (ICONST_3)
      (BIPUSH 39)
      (IASTORE)
      (DUP)
      (ICONST_4)
      (BIPUSH 31)
      (IASTORE)
      (DUP)
      (ICONST_5)
      (BIPUSH 27)
      (IASTORE)
      (DUP)
      (BIPUSH 6)
      (BIPUSH 24)
      (IASTORE)
      (DUP)
      (BIPUSH 7)
      (BIPUSH 22)
      (IASTORE)
      (DUP)
      (BIPUSH 8)
      (BIPUSH 20)
      (IASTORE)
      (DUP)
      (BIPUSH 9)
      (BIPUSH 19)
      (IASTORE)
      (DUP)
      (BIPUSH 10)
      (BIPUSH 18)
      (IASTORE)
      (DUP)
      (BIPUSH 11)
      (BIPUSH 18)
      (IASTORE)
      (DUP)
      (BIPUSH 12)
      (BIPUSH 17)
      (IASTORE)
      (DUP)
      (BIPUSH 13)
      (BIPUSH 17)
      (IASTORE)
      (DUP)
      (BIPUSH 14)
      (BIPUSH 16)
      (IASTORE)
      (DUP)
      (BIPUSH 15)
      (BIPUSH 16)
      (IASTORE)
      (DUP)
      (BIPUSH 16)
      (BIPUSH 15)
      (IASTORE)
      (DUP)
      (BIPUSH 17)
      (BIPUSH 15)
      (IASTORE)
      (DUP)
      (BIPUSH 18)
      (BIPUSH 15)
      (IASTORE)
      (DUP)
      (BIPUSH 19)
      (BIPUSH 14)
      (IASTORE)
      (DUP)
      (BIPUSH 20)
      (BIPUSH 14)
      (IASTORE)
      (DUP)
      (BIPUSH 21)
      (BIPUSH 14)
      (IASTORE)
      (DUP)
      (BIPUSH 22)
      (BIPUSH 14)
      (IASTORE)
      (DUP)
      (BIPUSH 23)
      (BIPUSH 13)
      (IASTORE)
      (DUP)
      (BIPUSH 24)
      (BIPUSH 13)
      (IASTORE)
      (DUP)
      (BIPUSH 25)
      (BIPUSH 13)
      (IASTORE)
      (DUP)
      (BIPUSH 26)
      (BIPUSH 13)
      (IASTORE)
      (DUP)
      (BIPUSH 27)
      (BIPUSH 13)
      (IASTORE)
      (DUP)
      (BIPUSH 28)
      (BIPUSH 13)
      (IASTORE)
      (DUP)
      (BIPUSH 29)
      (BIPUSH 12)
      (IASTORE)
      (DUP)
      (BIPUSH 30)
      (BIPUSH 12)
      (IASTORE)
      (DUP)
      (BIPUSH 31)
      (BIPUSH 12)
      (IASTORE)
      (DUP)
      (BIPUSH 32)
      (BIPUSH 12)
      (IASTORE)
      (DUP)
      (BIPUSH 33)
      (BIPUSH 12)
      (IASTORE)
      (DUP)
      (BIPUSH 34)
      (BIPUSH 12)
      (IASTORE)
      (DUP)
      (BIPUSH 35)
      (BIPUSH 12)
      (IASTORE)
      (DUP)
      (BIPUSH 36)
      (BIPUSH 12)
      (IASTORE)
      (PUTSTATIC "java.math.BigInteger" "digitsPerLong" NIL)
      (BIPUSH 37)
      (ANEWARRAY "java.math.BigInteger")
      (DUP)
      (ICONST_0)
      (ACONST_NULL)
      (AASTORE)
      (DUP)
      (ICONST_1)
      (ACONST_NULL)
      (AASTORE)
      (DUP)
      (ICONST_2)
      (LDC2_W 88)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (ICONST_3)
      (LDC2_W 89)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (ICONST_4)
      (LDC2_W 88)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (ICONST_5)
      (LDC2_W 90)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 6)
      (LDC2_W 91)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 7)
      (LDC2_W 92)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 8)
      (LDC2_W 93)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 9)
      (LDC2_W 94)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 10)
      (LDC2_W 95)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 11)
      (LDC2_W 96)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 12)
      (LDC2_W 97)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 13)
      (LDC2_W 98)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 14)
      (LDC2_W 99)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 15)
      (LDC2_W 100)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 16)
      (LDC2_W 93)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 17)
      (LDC2_W 101)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 18)
      (LDC2_W 102)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 19)
      (LDC2_W 103)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 20)
      (LDC2_W 104)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 21)
      (LDC2_W 105)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 22)
      (LDC2_W 106)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 23)
      (LDC2_W 107)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 24)
      (LDC2_W 108)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 25)
      (LDC2_W 109)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 26)
      (LDC2_W 110)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 27)
      (LDC2_W 89)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 28)
      (LDC2_W 111)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 29)
      (LDC2_W 112)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 30)
      (LDC2_W 113)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 31)
      (LDC2_W 114)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 32)
      (LDC2_W 93)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 33)
      (LDC2_W 115)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 34)
      (LDC2_W 116)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 35)
      (LDC2_W 117)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (DUP)
      (BIPUSH 36)
      (LDC2_W 91)
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2)
      (AASTORE)
      (PUTSTATIC "java.math.BigInteger" "longRadix" NIL)
      (BIPUSH 37)
      (NEWARRAY)
      (DUP)
      (ICONST_0)
      (ICONST_0)
      (IASTORE)
      (DUP)
      (ICONST_1)
      (ICONST_0)
      (IASTORE)
      (DUP)
      (ICONST_2)
      (BIPUSH 30)
      (IASTORE)
      (DUP)
      (ICONST_3)
      (BIPUSH 19)
      (IASTORE)
      (DUP)
      (ICONST_4)
      (BIPUSH 15)
      (IASTORE)
      (DUP)
      (ICONST_5)
      (BIPUSH 13)
      (IASTORE)
      (DUP)
      (BIPUSH 6)
      (BIPUSH 11)
      (IASTORE)
      (DUP)
      (BIPUSH 7)
      (BIPUSH 11)
      (IASTORE)
      (DUP)
      (BIPUSH 8)
      (BIPUSH 10)
      (IASTORE)
      (DUP)
      (BIPUSH 9)
      (BIPUSH 9)
      (IASTORE)
      (DUP)
      (BIPUSH 10)
      (BIPUSH 9)
      (IASTORE)
      (DUP)
      (BIPUSH 11)
      (BIPUSH 8)
      (IASTORE)
      (DUP)
      (BIPUSH 12)
      (BIPUSH 8)
      (IASTORE)
      (DUP)
      (BIPUSH 13)
      (BIPUSH 8)
      (IASTORE)
      (DUP)
      (BIPUSH 14)
      (BIPUSH 8)
      (IASTORE)
      (DUP)
      (BIPUSH 15)
      (BIPUSH 7)
      (IASTORE)
      (DUP)
      (BIPUSH 16)
      (BIPUSH 7)
      (IASTORE)
      (DUP)
      (BIPUSH 17)
      (BIPUSH 7)
      (IASTORE)
      (DUP)
      (BIPUSH 18)
      (BIPUSH 7)
      (IASTORE)
      (DUP)
      (BIPUSH 19)
      (BIPUSH 7)
      (IASTORE)
      (DUP)
      (BIPUSH 20)
      (BIPUSH 7)
      (IASTORE)
      (DUP)
      (BIPUSH 21)
      (BIPUSH 7)
      (IASTORE)
      (DUP)
      (BIPUSH 22)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 23)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 24)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 25)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 26)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 27)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 28)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 29)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 30)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 31)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 32)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 33)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 34)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 35)
      (BIPUSH 6)
      (IASTORE)
      (DUP)
      (BIPUSH 36)
      (ICONST_5)
      (IASTORE)
      (PUTSTATIC "java.math.BigInteger" "digitsPerInt" NIL)
      (BIPUSH 37)
      (NEWARRAY)
      (DUP)
      (ICONST_0)
      (ICONST_0)
      (IASTORE)
      (DUP)
      (ICONST_1)
      (ICONST_0)
      (IASTORE)
      (DUP)
      (ICONST_2)
      (LDC_W 118)
      (IASTORE)
      (DUP)
      (ICONST_3)
      (LDC_W 119)
      (IASTORE)
      (DUP)
      (ICONST_4)
      (LDC_W 118)
      (IASTORE)
      (DUP)
      (ICONST_5)
      (LDC_W 120)
      (IASTORE)
      (DUP)
      (BIPUSH 6)
      (LDC_W 121)
      (IASTORE)
      (DUP)
      (BIPUSH 7)
      (LDC_W 122)
      (IASTORE)
      (DUP)
      (BIPUSH 8)
      (LDC_W 118)
      (IASTORE)
      (DUP)
      (BIPUSH 9)
      (LDC_W 123)
      (IASTORE)
      (DUP)
      (BIPUSH 10)
      (LDC_W 124)
      (IASTORE)
      (DUP)
      (BIPUSH 11)
      (LDC_W 125)
      (IASTORE)
      (DUP)
      (BIPUSH 12)
      (LDC_W 126)
      (IASTORE)
      (DUP)
      (BIPUSH 13)
      (LDC_W 127)
      (IASTORE)
      (DUP)
      (BIPUSH 14)
      (LDC_W 128)
      (IASTORE)
      (DUP)
      (BIPUSH 15)
      (LDC_W 129)
      (IASTORE)
      (DUP)
      (BIPUSH 16)
      (LDC_W 130)
      (IASTORE)
      (DUP)
      (BIPUSH 17)
      (LDC_W 131)
      (IASTORE)
      (DUP)
      (BIPUSH 18)
      (LDC_W 132)
      (IASTORE)
      (DUP)
      (BIPUSH 19)
      (LDC_W 133)
      (IASTORE)
      (DUP)
      (BIPUSH 20)
      (LDC_W 134)
      (IASTORE)
      (DUP)
      (BIPUSH 21)
      (LDC_W 135)
      (IASTORE)
      (DUP)
      (BIPUSH 22)
      (LDC_W 136)
      (IASTORE)
      (DUP)
      (BIPUSH 23)
      (LDC_W 137)
      (IASTORE)
      (DUP)
      (BIPUSH 24)
      (LDC_W 138)
      (IASTORE)
      (DUP)
      (BIPUSH 25)
      (LDC_W 139)
      (IASTORE)
      (DUP)
      (BIPUSH 26)
      (LDC_W 140)
      (IASTORE)
      (DUP)
      (BIPUSH 27)
      (LDC_W 123)
      (IASTORE)
      (DUP)
      (BIPUSH 28)
      (LDC_W 141)
      (IASTORE)
      (DUP)
      (BIPUSH 29)
      (LDC_W 142)
      (IASTORE)
      (DUP)
      (BIPUSH 30)
      (LDC_W 143)
      (IASTORE)
      (DUP)
      (BIPUSH 31)
      (LDC_W 144)
      (IASTORE)
      (DUP)
      (BIPUSH 32)
      (LDC_W 118)
      (IASTORE)
      (DUP)
      (BIPUSH 33)
      (LDC_W 145)
      (IASTORE)
      (DUP)
      (BIPUSH 34)
      (LDC_W 146)
      (IASTORE)
      (DUP)
      (BIPUSH 35)
      (LDC_W 147)
      (IASTORE)
      (DUP)
      (BIPUSH 36)
      (LDC_W 148)
      (IASTORE)
      (PUTSTATIC "java.math.BigInteger" "intRadix" NIL)
      ;(BIPUSH 6)
      ;(ANEWARRAY ("java.io.ObjectStreamField"))
      ;(DUP)
      ;(ICONST_0)
      ;(NEW "java.io.ObjectStreamField")
      ;(DUP)
      ;(LDC 38)
      ;(GETSTATIC "java.lang.Integer" "TYPE" NIL)
      ;(INVOKESPECIAL "java.io.ObjectStreamField" "<init>" 2)
      ;(AASTORE)
      ;(DUP)
      ;(ICONST_1)
      ;(NEW "java.io.ObjectStreamField")
      ;(DUP)
      ;(LDC 39)
      ;(LDC_W)
      ;(INVOKESPECIAL "java.io.ObjectStreamField" "<init>" 2)
      ;(AASTORE)
      ;(DUP)
      ;(ICONST_2)
      ;(NEW "java.io.ObjectStreamField")
      ;(DUP)
      ;(LDC 44)
      ;(GETSTATIC "java.lang.Integer" "TYPE" NIL)
      ;(INVOKESPECIAL "java.io.ObjectStreamField" "<init>" 2)
      ;(AASTORE)
      ;(DUP)
      ;(ICONST_3)
      ;(NEW "java.io.ObjectStreamField")
      ;(DUP)
      ;(LDC 45)
      ;(GETSTATIC "java.lang.Integer" "TYPE" NIL)
      ;(INVOKESPECIAL "java.io.ObjectStreamField" "<init>" 2)
      ;(AASTORE)
      ;(DUP)
      ;(ICONST_4)
      ;(NEW "java.io.ObjectStreamField")
      ;(DUP)
      ;(LDC 47)
      ;(GETSTATIC "java.lang.Integer" "TYPE" NIL)
      ;(INVOKESPECIAL "java.io.ObjectStreamField" "<init>" 2)
      ;(AASTORE)
      ;(DUP)
      ;(ICONST_5)
      ;(NEW "java.io.ObjectStreamField")
      ;(DUP)
      ;(LDC 46)
      ;(GETSTATIC "java.lang.Integer" "TYPE" NIL)
      ;(INVOKESPECIAL "java.io.ObjectStreamField" "<init>" 2)
      ;(AASTORE)
      ;(PUTSTATIC "java.math.BigInteger" "serialPersistentFields" NIL)
      ;(INVOKESTATIC "sun.misc.Unsafe" "getUnsafe" 0)
      ;(PUTSTATIC "java.math.BigInteger" "unsafe" NIL)
      ;(GETSTATIC "java.math.BigInteger" "unsafe" NIL)
      ;(LDC_W)
      ;(LDC 38)
      ;(INVOKEVIRTUAL "java.lang.Class" "getDeclaredField" 1)
      ;(INVOKEVIRTUAL "sun.misc.Unsafe" "objectFieldOffset" 1)
      ;(PUTSTATIC "java.math.BigInteger" "signumOffset" T)
      ;(GETSTATIC "java.math.BigInteger" "unsafe" NIL)
      ;(LDC_W)
      ;(LDC_W 149)
      ;(INVOKEVIRTUAL "java.lang.Class" "getDeclaredField" 1)
      ;(INVOKEVIRTUAL "sun.misc.Unsafe" "objectFieldOffset" 1)
      ;(PUTSTATIC "java.math.BigInteger" "magOffset" T)
      ;(GOTO 13)
      ;(ASTORE_0)
      ;(NEW "java.lang.Error")
      ;(DUP)
      ;(ALOAD_0)
      ;(INVOKESPECIAL "java.lang.Error" "<init>" 1)
      ;(ATHROW)
      (RETURN))    
    )
   '(REF -1)))

(defconst *java.math.BigInteger.init-heap*
  '((0 . (("java.lang.Class" ("<name>" . "java.math.BigInteger"))
          ("java.lang.Object" ("monitor" . 0) ("mcount" . 0) ("wait-set" . 0))))
;; overflow
   (1 ("ARRAY" ("<array>" *ARRAY* "INT" 1 (7))))
   (2 ("java.math.BigInteger" ("signum" . 0)
                              ("mag" . 0)
                              ("bitCount" . 0)
                              ("bitLength" . 0)
                              ("lowestSetBit" . 0)
                              ("firstNonzeroIntNum" . 0)))))

(defconst *init-state* 
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      (push '(REF 1) (push 0 nil)) 
      '((INVOKESTATIC "java.math.BigInteger" "<init>" 2))
      "java.math.BigInteger")
     nil)
    *java.math.BigInteger.init-heap*
    (make-class-def (list *java.lang.Number* *java.math.BigInteger*))))

; (stack (top-frame 0 (step-n 6 0 *init-state*)))
; (next-inst (step-n 6 *init-state*))

(defconst *new-dup-state* 
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      (push '(REF 1) (push 0 nil)) 
      '((NEW "java.math.BigInteger")
        (DUP)
        (ALOAD_0)
        (INVOKESTATIC "java.math.BigInteger" "<init>" 2)
        (ALOAD_0)
        (PUTFIELD "java.lang.Object" "<name>" nil))
      "java.math.BigInteger")
     nil)
    *java.math.BigInteger.init-heap*
    (make-class-def (list *java.lang.Number* *java.math.BigInteger*))))

; (stack (top-frame 0 (step-n 1 0 *new-dup-state*)))
; (next-inst 0 (step-n 6 0 *new-dup-state*))

(defconst *init-state2* 
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      (push 0 (push '(REF 1) (push '(REF 2) nil))) 
      '((INVOKESTATIC "java.math.BigInteger" "<init>" 3))
      "java.math.BigInteger")
     nil)
    *java.math.BigInteger.init-heap*
    (make-class-def (list *java.lang.Number* *java.math.BigInteger*))))  
  
; (stack (top-frame 0 (step-n 1 0 *init-state2*)))
; (next-inst 0 (step-n 6 0 *init-state2*))

(defconst *java.math.BigInteger.subtract-heap*
  '((0 . (("java.lang.Class" ("<name>" . "java.math.BigInteger"))
          ("java.lang.Object" ("monitor" . 0) ("mcount" . 0) ("wait-set" . 0))))
;; overflow
   (1 ("ARRAY" ("<array>" *ARRAY* NIL 1 (4))))
   (2 ("ARRAY" ("<array>" *ARRAY* NIL 1 (7))))))

(defconst *subtract-state*
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      (push '(REF 1) (push '(REF 2) nil)) 
      '((INVOKESTATIC "java.math.BigInteger" "subtract" 2))
      "java.math.BigInteger")
     nil)
    *java.math.BigInteger.subtract-heap*
    (make-class-def (list *java.lang.Number* *java.math.BigInteger*))))

(defun refp (x)
  (equal (car x) 'REF))

(defun pre-<init> (jstate x0 x1 x2) 
  (let* ((heap (heap jstate)))
    (and (refp x0)
         (refp x1)
         (integerp x2)
         (equal (class-name-of-ref x1 heap) "ARRAY")
         (equal (class-name-of-ref x0 heap) "java.math.BigInteger")
         (equal (next-inst jstate)
                '(INVOKESTATIC "java.math.BigInteger" "<init>" 3)))))
         
(defun post-<init> (jstate x0 x1 x2) 
  (let* ((heap (heap jstate)))
    (and (equal (class-name-of-ref x0 heap) "java.math.BigInteger")
         (equal (field-value "java.math.BigInteger" "signum" (deref x0 heap))
                x2)
         (equal (field-value "java.math.BigInteger" "mag" (deref x0 heap))
                x1))))

;(set-gag-mode nil)

;(defthm init-BigInteger-1
;  (let* ((stack (stack (top-frame jstate)))
;         (x2 (top stack))
;         (x1 (top (pop stack)))
;         (x0 (top (pop (pop stack)))))
;  (implies (and (pre-<init> jstate x0 x1 x2)
;                (equal (bound? "java.math.BigInteger" (class-table jstate))
;                       *java.math.BigInteger*)
;                (equal (bound? "java.lang.Number" (class-table jstate))
;                       *java.lang.Number*))
;           (equal (next-inst (step-n 2 jstate))
;                  '(ALOAD_0)))))


;(defthm init-BigInteger-is-BigInteger
;  (let* ((stack (stack (top-frame jstate)))
;         (x2 (top stack))
;         (x1 (top (pop stack)))
;         (x0 (top (pop (pop stack)))))
;  (implies (and (pre-<init> jstate x0 x1 x2)
;                (equal (bound? "java.math.BigInteger" (class-table jstate))
;                       *java.math.BigInteger*)
;                (equal (bound? "java.lang.Number" (class-table jstate))
;                       *java.lang.Number*))
;           (post-<init> (run-to-return 0 100 jstate) x0 x1 x2))))

; (stack (top-frame 0 (step-n 1 0 *subtract-state*)))
; (next-inst 0 (step-n 39 0 *subtract-state*))

; (run-to-return 1 100 *subtract-state*)
; (stack (top-frame (run-to-return 1 100 *subtract-state*)))
; (next-inst 0 (run-to-return 1 100 *subtract-state*))
