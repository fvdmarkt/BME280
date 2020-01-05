\ Read the BME280 Pressure, Temperature and Humidity registers
\ Program for the MSP430G2553 launchpad or egelkit
\ F.L. van der Markt
\ 12-12-2019  
\ pressure-testen
\ Uservalue for Hsl, with the code of Albert Nijhof
\ If you want to change uservalues, example:
\    11 to Hsl
\    35 to Node-ID  etc.
\    USAVE
\ With USAVE the current values are copied from RAM to flash.
\ When you start the measurements with test, these uservalues
\ are restored to RAM by UREAD
\         
\ ?abort, ?error, catch, D>S of Albert Nijhof 
\ READ_INT reads now 16 bits, READ_WORD 16 bits >>1
\ P15 is not used anymore, but the full adc_P (20 bits)
\ DIGT*, DIGP* and DIGH* replaced with 3 arrays. 
\ Temperature and airpressure and humidity now calculated according
\ to the programs in the datasheet of the BME280/GY-BME280
\ No debug output, model approximation removed!
\ */ replaced by (u)scale routine
\ ----------------------------------------------------------------

\ user words:  
\ TEST  displays every 5 minutes values of temperature,pressure 
\        and humidity combined with some intermediate results
\ TEST1 reads 10 times the ID of the BME280

\ Load first the bitbang I2C routines for MSP430 code variant, 
\ without assembler: 'bitbang-i2c p22-p24.f', before loading this file!
\ Connect P2.4 to SDA and P2.2 to SCL
\ Remove also the jumper at p2.4 (LED Power)
\ Note: pullup resistors are already integrated in the BME280 sensor

HEX 

EC constant BME280 \  I2C-address ( hex EC) of the BME280
BME280 I2C?        \ should print -1 if I2C connections are OK

\ configuration-registers
E0 CONSTANT BME_RESET
F2 CONSTANT BME_HUMI
F3 CONSTANT BME_STATUS
F4 CONSTANT BME_CTRLM
F5 CONSTANT BME_CONFIG

: ARRAY ( n ccc -- )
create cells allot immediate
does> @ char 1- hx 0F and 2* + postpone literal ;

9 array DIGP
3 array DIGT
6 array DIGH

value P15       \ P15 = ADC_P>>5, 15-bit pressure
            
value T15       \ T15 = ADC_T>>5, 15-bit temperature
value TCEL      \ Temperature in degrees Celsius
value VAR1      \ used for compensation COMP-T
value VAR2      \ ..      
value VAR3
value T_FINE    \ ..

value H15       \ H15 = ADC_H/2, 15-bit humidity
value HUM_PM    \ Humidity in promille
value TMP

decimal

\ <><> Albert Nijhof ------>
\ user-values saved in flash with special procedures
value Hsl       6 to Hsl \ Height above sealevel in m
value Node-ID   53 to Node-ID
value Child-ID  2 to Child-ID
: USAVE ( -- )
   [ -1 , ]     \ wordt PROGRAMMA\
   adr Hsl 6 m, \ Inhoud values -> flash (ROM)
   0 ,          \ Afsluiter (nodig voor CHERE
   freeze 
;
: UREAD ( -- )
   chere cell-
   dup @ ?abort ( msg from UREAD ) \ beveiliging
   6 - adr Hsl 6 move \ ROM-waarden -> values in RAM
;
\ <------------

\ create a double variable, name in flash, value in ram
\ using the name of the variable, puts its ram-address on the stack
: 2variable ( 'name' -- )
  create 2 cells allot  \ reserve 2 cells, 4 bytes
  does> @               \ get ram-address
;

\ store double value in double variable
\ lo = low word, hi = high word
: 2!  ( lo hi adr -- )
  dup >r           \ S: lo hi adr     R: adr
  !                \ S: lo            R: adr
  r> 1 cells + !    
;

\ fetch value from double variable
: 2@ ( adr -- lo hi )
  dup              \ adr adr
  @                \ adr hi
  swap 1 cells +   \ hi adr+2
  @ swap           \ lo hi
;

: D-  ( D1 D2 -- D3 ) \ D3 = D1 - D2 
  DNEGATE D+ 
;

: M/ ( d n1 -- n2 ) \ rounded division
  dup >r 2/ s>d d+ r> fm/mod nip ( n2 = quot )  
;

\ <><> Albert Nijhof ------>
: D*/ ( dn1 x +y -- dn2 )
    >r dup abs >r   \ lo hi
    ?dnegate        \ teken van dn1 aanpassen als x<0
    tuck            \ teken bewaren
    dabs            \ +lo +hi
    swap r@ um*     \ +hi lo mi
    rot  r> um*     \ lo mi mi hi
    rot 0 d+        \ lo mi hi -- 48bits tussenresultaat
    r@ um/mod       \ lo rest +hi
    r> 2>r          \ lo rest
    r> um/mod       \ rest +lo
    nip r>          \ +lo +hi
    rot ?dnegate ;  \ lo hi -- teken terugzetten

(*
    dn1 maal x/y levert dn2 op.
    dn1 en x mogen negatief zijn, y moet positief zijn.
    Voorbeeld:
    -100000. 3000 4000 d*/ d. <rtn> -75000  OK
    Geen afrondingscorrectie.
*)


\ double d to 16-bits number 
\ only possible if d fits in a single number !
: D>S ( lo hi -- lo )
    over s>d        \ lo hi lo hi2
    nip             \ lo hi hi2
    <> ?abort 
;

: ?ERROR ( throw# -- )
    if ."  Error in " dup count hx 1F and type cr then ;
hex 
\  alle shifts ( d1 n -- d2 )   
code DLSHIFT   4706 ,
4437 ,  9346 ,  2405 ,  54A4 ,
   0 ,  6707 ,  8356 ,  23FB ,   next  end-code

code DRSHIFT   4706 ,
4437 ,  9346 ,  2405 ,  C312 ,
1007 ,  1024 ,  8356 ,  23FB ,   next  end-code

code DARSHIFT   4706 ,
4437 ,  9346 ,  2404 ,  1107 ,
1024 ,  8356 ,  23FC ,   next  end-code

decimal

: scale ( x num den -- result ) \ x, num, den  >0
   >r m*
   r@ 2/ s>d d+ \ x*num + den/2
   r> fm/mod nip
   dup 32767 u> if ." SCALE overflow! " dup . cr then
;

(*
: USCALE ( x teller noemer -- y ) \ unsigned */ met afronding
  \ alleen gebruiken als alle stackparameters positief zijn !
  >r um* 
  r@ 0 d2/ d+  \ heft van deler erbij
  r>  2dup u< 0= if ." USCALE overflow!" dup . cr then 
  um/mod nip 
; 
*)

\ <><>  <---  Albert Nijhof

\ variables for humidity and pressure calculations
variable t_finex  \ t_fine /10
2variable adc_H
2variable var_h
2variable vx1
2variable vx2
2variable vx3
2variable vx4
2variable adc_P 
2variable p100 \ pressure in hPa*100


hex

: READ-STATUS ( -- status )
    START-BIT BME280 BYTE-OUT     \ write-address BME280
    BME_STATUS BYTE-OUT           \ select register F3 status
    START-BIT
    BME280 1+ BYTE-OUT            \ read-address BME280
    BYTE-IN                       \ read F3 register
    NACK-BIT
    STOP-BIT
;
          
: WAIT-ADC-READY ( -- ) \ Wait until adc-conversion ready
  BEGIN
     10 MS
     READ-STATUS 8 AND  \ bit 3
     0 = KEY? OR
  UNTIL
;

: WAIT-NOCAL ( -- )     \ Wait until not reading calibration
  BEGIN
     10 MS
     READ-STATUS 1 AND  \ BIT 0
     0 = KEY? OR
  UNTIL
;

: CLEAR-DATA ( -- )
  0 TO P15
  0 TO T15
  0 TO H15
;

: SETUP-BME280 ( -- )
\ reset sensor
    START-BIT BME280 BYTE-OUT BME_RESET BYTE-OUT B6 BYTE-OUT STOP-BIT

\ configure humidity oversampling 4x 
    START-BIT BME280 BYTE-OUT BME_HUMI BYTE-OUT 3 BYTE-OUT STOP-BIT

\ configure oversampling 4x pres and 4x temp and normal mode
    START-BIT BME280 BYTE-OUT BME_CTRLM BYTE-OUT  6F BYTE-OUT STOP-BIT

\ configure for one measurement per 500 ms and filter off
    START-BIT BME280 BYTE-OUT BME_CONFIG BYTE-OUT 80 BYTE-OUT STOP-BIT
;


: BME280-ID ( -- ) \ should return 0x60
    START-BIT BME280 BYTE-OUT D0 BYTE-OUT \ select ID register
    START-BIT BME280 1+ BYTE-OUT          \ switch to READ-MODE
    BYTE-IN NACK-BIT                      \ Read ID
    STOP-BIT
    ." ID = " . cr
;


: READ-ALL ( -- ) \ Read all the registers with measured values 
    CLEAR-DATA
    WAIT-ADC-READY
    START-BIT BME280 BYTE-OUT         \ write address BME280
    F7 BYTE-OUT                       \ select register PRES_MSB
    START-BIT
    BME280 1+ BYTE-OUT                \ switch to read-mode
    BYTE-IN ACK-BIT                   \ F7 PRES_MSB  8 bits
    BYTE-IN ACK-BIT                   \ F8 PRES_LSB  8 bits
    BYTE-IN ACK-BIT                   \ F9 PRES_XLSB 4 highest bits
    BYTE-IN ACK-BIT                   \ FA TEMP_MSB
    BYTE-IN ACK-BIT                   \ FB TEMP_LSB
    BYTE-IN ACK-BIT DROP              \ FC TEMP_XLSB not used
    BYTE-IN ACK-BIT                   \ FD HUMI_MSB
    BYTE-IN NACK-BIT                  \ FE HUMI_LSB
    STOP-BIT
    1 rshift SWAP 7 lshift + ABS TO H15  \ HUM  >> 1  ( 15 bits left )
    1 rshift SWAP 7 lshift + ABS TO T15  \ TEMP >> 5  ( 15 bits left )
    4 rshift s>d vx1 2! ( pres_xlsb >> 4 )
    4 lshift s>d vx2 2! ( pres_lsb << 4 )
    s>d dm 12 dlshift vx2 2@ d+ vx1 2@ d+  ( pres_msb << 12 ) 
    2dup adc_P 2! ( full 20 bits in unsigned adc_P )
\   ." adc_P = " 2dup d. cr 
    5 drshift drop to P15 ( 15 bits left )
;

: READ-WORD ( -- n/2) \ Read 2 bytes and make to unsigned short /2
    BYTE-IN ACK-BIT   \ low byte
    BYTE-IN ACK-BIT   \ high byte
    7 LSHIFT SWAP 1 rshift + 
;

: READ-INT ( -- n)    \ Read 2 bytes for signed short
    BYTE-IN ACK-BIT   \ low byte
    BYTE-IN ACK-BIT   \ high byte
    8 LSHIFT + 
;

: READ-PCOMPS ( -- ) \ Read all compensation values for Pressure
    WAIT-NOCAL CR
    START-BIT BME280 BYTE-OUT         \ write address BME280
    8E BYTE-OUT                    \ select register 0x8E
    START-BIT
    BME280 1+ BYTE-OUT                \ switch to read-mode
    READ-WORD DIGP 1 ! \ 8E/8F  >>1
    READ-INT  DIGP 2 ! \ 90/91  
    READ-INT  DIGP 3 ! \ 92/93  
    READ-INT  DIGP 4 ! \ 94/95  
    READ-INT  DIGP 5 ! \ 96/97  
    READ-INT  DIGP 6 ! \ 98/99  
    READ-INT  DIGP 7 ! \ 9A/9B  
    READ-INT  DIGP 8 ! \ 9C/9D  
    READ-INT  DIGP 9 ! \ 9E/9F  

    NACK-BIT
    STOP-BIT


    ." DIGP1/2 = " DIGP 1 @ 6 .R  
    ."   DIGP2 = " DIGP 2 @ 6 .R 
    ."   DIGP3 = " DIGP 3 @ 6 .R  CR 
    ."   DIGP4 = " DIGP 4 @ 6 .R 
    ."   DIGP5 = " DIGP 5 @ 6 .R
    ."   DIGP6 = " DIGP 6 @ 6 .R  CR 
    ."   DIGP7 = " DIGP 7 @ 6 .R 
    ."   DIGP8 = " DIGP 8 @ 6 .R
    ."   DIGP9 = " DIGP 9 @ 6 .R  CR 
;    


: READ-TCOMPS ( -- ) \ Read all compensation values for temperature

    WAIT-NOCAL CR
    START-BIT BME280 BYTE-OUT         \ write address BME280
    88 BYTE-OUT                       \ select register 0x88
    START-BIT
    BME280 1+ BYTE-OUT                 \ switch to read-mode
    READ-WORD \ 88/89  DIGT1  
    READ-INT  \ 8A/8B  DIGT2
    READ-INT  \ 8C/8D  DIGT3
    NACK-BIT
    STOP-BIT

    DIGT 3 !  DIGT 2 !  DIGT 1 !

    ." DIGT1/2 = " DIGT 1 @ 6 .R
    ."   DIGT2 = " DIGT 2 @ 6 .R
    ."   DIGT3 = " DIGT 3 @ 6 .R  CR     
;    

: READ-HCOMPS ( -- ) \ Read all compensation values for humidity
    WAIT-NOCAL CR
    START-BIT BME280 BYTE-OUT         \ write address BME280
    HX A1 BYTE-OUT                    \ select register 0xA1
    START-BIT
    BME280 1+ BYTE-OUT                \ switch to read-mode
    BYTE-IN DIGH 1 !                  \ A1  8-bits 
    NACK-BIT
    STOP-BIT

    START-BIT BME280 BYTE-OUT         \ write address BME280
    E1 BYTE-OUT                       \ select register 0xE1
    START-BIT
    BME280 1+ BYTE-OUT                \ switch to read-mo
    READ-INT DIGH 2 !                 \ signed short 
    BYTE-IN ACK-BIT DIGH 3 !          \ E3  8-bits unsigned
    BYTE-IN ACK-BIT DIGH 4 !          \ E4  8-bits [11:4]
    BYTE-IN ACK-BIT TO TMP            \ E5  H5[3:0] H4[3:0]
    BYTE-IN DIGH 5 !                  \ E6  8-bits H5[11:4]
    BYTE-IN DIGH 6 !                  \ E7  8 bits H6 signed
    NACK-BIT
    STOP-BIT
    DIGH 4 @   4 lshift TMP 0F AND OR DIGH 4 !
    DIGH 4 @ 800 AND IF DIGH 4 @ 1000 - DIGH 4 ! THEN  \ signed 12 bit 
    DIGH 5 @   4 lshift TMP 4 rshift hx 0F AND OR DIGH 5 !
    DIGH 5 @ 800 AND IF DIGH 5 @ 1000 - DIGH 5 ! THEN  \ signed 12 bit 
    DIGH 6 @  80 AND IF DIGH 6 @  100 - DIGH 6 ! THEN  \ signed 8 bit 

    ."   DIGH1 = " DIGH 1 @ 6 .R 
    ."   DIGH2 = " DIGH 2 @ 6 .R 
    ."   DIGH3 = " DIGH 3 @ 6 .R CR
    ."   DIGH4 = " DIGH 4 @ 6 .R 
    ."   DIGH5 = " DIGH 5 @ 6 .R 
    ."   DIGH6 = " DIGH 6 @ 6 .R CR
;


decimal


: COMP-T ( -- ) \ calculate 10 * T-Celcius with the formula of pdf
\ T15 = adc_T >> 5
\ dig_T1 is divided by 2
\ TCEL = 10X temperature in Celsius
  T15 DIGT 1 @ - DIGT 2 @ 5120 scale TO VAR1  \ now divided by 10
  T15 DIGT 1 @ - DUP 1024 ( >>10 ) scale 
  DIGT 3 @ 16384 scale ( >> 14) 10 / TO VAR2 \ now divided by 10 
  VAR1 VAR2 +  TO T_FINE  \ T_FINE now divided by 10 
  T_FINE 5 128 scale 1+ 2/ TO TCEL
;
 

decimal
: .temp  ( -- )
\ T15 dup u. .  CR
   COMP-T TCEL \ calc. T_fine, TCEL 10 x temperature in Celsius 
   ." Temperature: " 
   s>d tuck dabs <# #  ch . hold #s rot sign #> \ string_adr length
   type ."  C" cr
;

: COMP-P \ calculate airpressure formula for BME280
\ here tcel is temp_in-Celsius *10, but needed X 100
\ only digp1 value is /2 
   tcel 10 * 256 m* -103. d+ ( -128+25 ) 50 m/ dup t_finex ! 
\  ." t_finex = " t_finex @ . cr
\  ." adc_P = " adc_P 2@ d. cr
   ( t_finex @ )  5 m* ( /10 --> /2 )  64000. d- 2dup vx1 2!
\  ." vx1 = " vx1 2@ d. cr
   4 M/ dup M* 1024. d+ DIGP 6 @ 2048 d*/ 2dup vx2 2!
\  ." vx2 = " vx2 2@ d. cr
   vx1 2@ digp 5 @ 2* 1 D*/ d+ 2dup vx2 2!
\  ." vx2 = " vx2 2@ d. cr
   d2/ d2/  DIGP 4 @ 8192 ( 2^13 ) m*  3 dlshift d+ vx2 2!
\  ." vx2 = " vx2 2@ d. cr
   vx1 2@ 4 M/ dup M* DIGP 3 @ 16384 d*/ d2/ d2/ vx3 2! 
\  ." vx3 = " vx3 2@ d. cr
   vx1 2@ DIGP 2 @ 2 d*/ vx3 2@ d+ 18 darshift 2dup vx1 2!
\  ." vx1 = " vx1 2@ d. cr
   32768. d+ DIGP 1 @ 16384 d*/ vx1 2! 
\   ." vx1 = " vx1 2@ d. cr
   vx1 2@ or 0= if exit then \ hi and lo or-ed should be <> 0
   1048576. adc_P 2@ d- vx2 2@ 12 darshift d- ( lo hi ) 
   3125 2 d*/ p100 2!  \ actually P100/2
\  ." P100/2 = " p100 2@ d. cr
   vx1 2@ d2/ d2/ ( corrrectie P100 ) vx3 2!
   p100 2@ vx3 2@ 1 darshift d+ ( halve of denominator added )
   vx3 2@  d>s dup >r abs
   1 swap d*/ r> ?dnegate 2dup p100 2! ( just in case denom vx1<0 )
\  ." P100 = " p100 2@ d. cr
   8 ( >>3 ) M/ dup M* 4096. D+ 13 darshift 
   DIGP 9 @ 4096 ( >>12 ) d*/ vx1 2! 
\  ." vx1 = " vx1 2@ d. cr
   p100 2@ d2/ d2/ DIGP 8 @ 8192 ( >>13 ) d*/ 2dup vx2 2!
\  ." vx2 = " vx2 2@ d. cr
   vx1 2@ D+ DIGP 7 @ s>d d+ 4 darshift p100 2@ d+ p100 2!
\  ." P100 = " p100 2@ d. cr
\   ." luchtdruk = " p100 2@ <# # # ch . hold #s #>  type ."  hPa " cr
; \ COMP-P


: COMP-H ( -- ) \ calculate relative humidity in promille, approximation
   H15 2 M* adc_H 2!
\   ." adc_H = " adc_H 2@ d. cr
   t_finex @ 10 m* 76800. d- var_h 2! 
\   ." t_finex = " t_finex @ . cr
\   ." var_h a = " var_h 2@ d. cr

   adc_H 2@  digh 4 @ 64 m* d- var_h 2@ digh 5 @ 1 d*/ 14 darshift d-
         1. d+ d2/ vx1 2!              
\   ." vx1 b = " vx1 2@ d. cr
     
   var_h 2@ digh 6 @ 1024 d*/ vx3 2!  
\   ." vx3 d = " vx3 2@ d. cr
    
   var_h 2@ digh 3 @ 2048 d*/ vx3 2@ d>s 1 d*/ 
       32768. d+ 10 darshift 2097152. d+ 2dup vx4 2!
\   ." vx4 e = " vx4 2@ d. cr
   
   7 darshift digh 2 @ 1 d*/ 64. d+ 7 darshift 2dup vx2 2!
\   ." vx2 f = " vx2 2@ d. cr  
 
   vx1 2@  d>s 16384 d*/ d2/ 2dup vx1 2! 
\   ." vx1/2^15 g = " vx1 2@ d. cr  \ 15 bits shifted right

   3 darshift d>s dup m* d2/ digh 1 @ 16 d*/  vx2 2!  
\  ." vx2 h = " vx2 2@ d. cr
   
   vx1 2@ 15 dlshift vx2 2@ d- 2dup vx3 2!
\   ." vx3 i = " vx3 2@ d. cr
   
   15 darshift  d>s  25 32 scale to hum_pm 
   \ 15 >> ipv 22 >> ( oude return was in %, dus *100 /128 ) 
\   ." HUM_PM *10 = " HUM_PM  . cr
   HUM_PM 100 < if 100 to HUM_PM then    \ >= 1%
   HUM_PM 9900 > if 9900 to HUM_PM then  \ <= 99%
   hum_pm 10 / to hum_pm ( promille )

; \ COMP-H
 
decimal
: .pres ( -- )
   ['] COMP-P catch dup ?error ?exit     \ i.p.v. COMP-P
   ." Pressure:    " 
   p100 2@  110. d- ( onnauwkeurigheid, afronding d*/ ?  )
   Hsl 12 M* d+ ( hoogte in m boven zee ) 10 M/  s>d 
   tuck dabs <# # ch . hold #s rot sign #> \ string_adr length
   TYPE ."  hPa " 
(*
   p100 2@ 100 m/ 
   dup  1033 >                 IF ." very dry"  THEN 
   dup  1022 > over 1034 < AND IF ." sunny"     THEN 
   dup  1006 > over 1023 < AND IF ." variable"  THEN 
   dup   987 > over 1007 < AND IF ." wind/rain" THEN 
         988 <                 IF ." storm"     THEN 
*)
   CR
; 


: .humi ( -- )
  
   ['] COMP-H catch dup ?error ?exit     \ i.p.v COMP-H  
   HUM_PM 5 + 10 / ( round to % ) 
   ." Humidity:    " . ch % emit cr
;

: wait ( #min -- )
   60 * begin
          1000 ms
          1 -
          dup 0= key? or
        until
        drop
;


: test ( -- ) 
    uread \ ROM values ==> to RAM
    decimal cr
    ." Height in m above sealevel (Hsl) = " Hsl . CR

    SETUP-I2C

    SETUP-BME280
    READ-PCOMPS
    READ-TCOMPS
    READ-HCOMPS
    begin
       read-all

       ." T15:" T15 . ." adc_P:" adc_P 2@ d. 
       ." H15:" H15 . CR
       .temp
       .pres
       .humi  cr
       5 wait 
       key? 
    until
    key drop .s
;

: test1 ( -- ) \ test reading the sensor-ID
   decimal
   SETUP-I2C
   SETUP-BME280
   10 0 do BME280-id space dm 500 ms loop cr 
;

CR
CHERE ' tools\ - u.  \ bytes flash used : 
IVECS CHERE - u.     \ bytes flash free :
HERE HX 200 - .      \ bytes ram used :
TIB HERE - .         \ bytes ram free :
 
shield BME280\
' BME280\ ' usave cell+ rom!
\ specify if needed defaultvalues for these uservalues
2   to Hsl \ 2 m above sea-level
53  to Node-ID 
1   to Child-ID 
USAVE
\ --- end of program


