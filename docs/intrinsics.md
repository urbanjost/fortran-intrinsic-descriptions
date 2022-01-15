# ABS

## __Name__

__abs__(3) - \[NUMERIC\] Absolute value

## __Syntax__
```fortran
  result = abs(a)

   TYPE(kind=KIND) elemental function abs(a)

   TYPE(kind=KIND),intent(in) :: a
```
where the TYPE and KIND is determined by the type and type attributes
of __a__, which may be any _real_, _integer_, or _complex_ value.

If the type of __a__ is _cmplx_ the type returned will be _real_ with
the same kind as the _real_ part of the input value.

Otherwise the returned type will be the same type as __a__.

## __Description__

__abs(a)__ computes the absolute value of numeric argument __a__.

In mathematics, the absolute value or modulus of a real number __x__,
denoted __|x|__, is the magnitude of __x__ without regard to its sign.

The absolute value of a number may be thought of as its distance from
zero, which is the definition used by __abs__(3) when dealing with
_complex_ values (_see below_).

## __Arguments__

  - __a__
    : the type of the argument shall be an _integer_, _real_, or _complex_
    scalar or array.

## __Returns__

If __a__ is of type _integer_ or _real_, the value of the result is
__|a|__ and of the same type and kind as the input argument.

(Take particular note) if __a__ is _complex_ with value __(x, y)__,
the result is a _real_ equal to a processor-dependent approximation to
__sqrt(x\*\*2 + y\*\*2)__
computed without undue overflow or underflow.

## __Examples__

Sample program:

```fortran
program demo_abs
implicit none
integer           :: i = -1
real              :: x = -1.0
complex           :: z = (-3.0,-4.0)
doubleprecision   :: rr = -45.78d+00
character(len=*),parameter :: &
 frmt =  '(1x,a15,1x," In: ",g0,            T51," Out: ",g0)', &
 frmtc = '(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)'
integer,parameter :: dp=kind(0.0d0)
integer,parameter :: sp=kind(0.0)

    write(*, frmt)  'integer         ',  i, abs(i)
    write(*, frmt)  'real            ',  x, abs(x)
    write(*, frmt)  'doubleprecision ', rr, abs(rr)
    write(*, frmtc) 'complex         ',  z, abs(z)
    !
    !
    write(*, *)
    write(*, *) 'abs is elemental: ', abs([20,  0,  -1,  -3,  100])
    write(*, *)
    write(*, *) 'abs range test : ', abs(huge(0)), abs(-huge(0))
    write(*, *) 'abs range test : ', abs(huge(0.0)), abs(-huge(0.0))
    write(*, *) 'abs range test : ', abs(tiny(0.0)), abs(-tiny(0.0))

    write(*, *) 'returned real kind:', cmplx(30.0_dp,40.0_dp,kind=dp), &
                                  kind(cmplx(30.0_dp,40.0_dp,kind=dp))
    write(*, *) 'returned real kind:', cmplx(30.0_dp,40.0_dp),&
                                  kind(cmplx(30.0_dp,40.0_dp))
    write(*, *) 'returned real kind:', cmplx(30.0_sp,40.0_sp),&
                                  kind(cmplx(30.0_sp,40.0_sp))

    write(*, *)
    write(*, *) 'distance of <XX,YY> from zero is', &
               & distance(30.0_dp,40.0_dp)

    contains

    real(kind=dp) elemental function distance(x,y)
    real(kind=dp),intent(in) :: x,y
       ! dusty corners:
       ! note that KIND=DP is NOT optional
       ! if the desired result is KIND=dp.
       ! See cmplx(3).
       distance=abs( cmplx(x,y,kind=dp) )
    end function distance
end program demo_abs
```
  Results:
```text
    integer          In: -1                        Out: 1
    real             In: -1.00000000               Out: 1.00000000
    doubleprecision  In: -45.780000000000001       Out: 45.780000000000001
    complex          In: (-3.00000000,-4.00000000) Out: 5.00000000

    abs is elemental:     20     0     1     3   100

    abs range test :   2147483647  2147483647
    abs range test :    3.40282347E+38   3.40282347E+38
    abs range test :    1.17549435E-38   1.17549435E-38
    returned real kind: (30.000000000000000,40.000000000000000) 8
    returned real kind: (30.0000000,40.0000000) 4
    returned real kind: (30.0000000,40.0000000) 4

    distance of <XX,YY> from zero is   50.000000000000000
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ACHAR

## __Name__

__achar__(3) - \[CHARACTER:CONVERSION\] returns a character in a specified position in the ASCII collating sequence

## __Syntax__
```fortran
  result = achar(i,kind=KIND)

    character(len=1) elemental function :: achar(i,kind=KIND)

    integer(kind=KIND),intent(in) :: i
    integer(kind=KIND),intent(in),optional :: kind
```
where KIND may be any supported kind value for _integer_ types.

## __Description__

__achar(i)__ returns the character located at position __i__ (commonly called the
_ADE_ or ASCII Decimal Equivalent) in the ASCII collating sequence.

The __achar__(3) function is often used for generating in-band escape
sequences to control terminal attributes.
```fortran
   write(*,'(*(a))')achar(27),'[2J'
```
will clear the screen on an ANSI-compatible terminal display, for
example.

## __Arguments__

  - __i__
    : the _integer_ value to convert to an ASCII character, in the range
    0 to 127.

  - __kind__
    : (optional) an _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is the requested character of type _character_ with a
length of one. If the __kind__ argument is present, the return value is of
the specified kind and of the default kind otherwise.

## __Examples__

```fortran
program demo_achar
use,intrinsic::iso_fortran_env,only:int8,int16,int32,int64
implicit none
integer :: i
   i=65
   write(*,'("decimal     =",i0)')i
   write(*,'("character   =",a1)')achar(i)
   write(*,'("binary      =",b0)')achar(i)
   write(*,'("octal       =",o0)')achar(i)
   write(*,'("hexadecimal =",z0)')achar(i)

   write(*,'(8(i3,1x,a,1x),/)')(i,achar(i), i=32,126)

   write(*,'(a)')upper('Mixed Case')
contains
! a classic use of achar(3) is to convert the case of a string

elemental pure function upper(str) result (string)
!
!$@(#) upper(3f): function to return a trimmed uppercase-only string
!
! input string to convert to all uppercase
character(*), intent(in)      :: str
! output string that contains no miniscule letters
character(len(str))           :: string
integer                       :: i, iend
integer,parameter             :: toupper = iachar('A')-iachar('a')
   iend=len_trim(str)
   ! initialize output string to trimmed input string
   string = str(:iend)
   ! process each letter in the string
   do concurrent (i = 1:iend)
       select case (str(i:i))
       ! located miniscule letter
       case ('a':'z')
          ! change miniscule to majuscule letter
          string(i:i) = achar(iachar(str(i:i))+toupper)
       end select
   enddo
end function upper
end program demo_achar
```
Results:
```
   decimal     =65
   character   =A
   binary      =1000001
   octal       =101
   hexadecimal =41
    32    33 !  34 "  35 #  36 $  37 %  38 &  39 '

    40 (  41 )  42 *  43 +  44 ,  45 -  46 .  47 /

    48 0  49 1  50 2  51 3  52 4  53 5  54 6  55 7

    56 8  57 9  58 :  59 ;  60 <  61 =  62 >  63 ?

    64 @  65 A  66 B  67 C  68 D  69 E  70 F  71 G

    72 H  73 I  74 J  75 K  76 L  77 M  78 N  79 O

    80 P  81 Q  82 R  83 S  84 T  85 U  86 V  87 W

    88 X  89 Y  90 Z  91 [  92 \  93 ]  94 ^  95 _

    96 `  97 a  98 b  99 c 100 d 101 e 102 f 103 g

   104 h 105 i 106 j 107 k 108 l 109 m 110 n 111 o

   112 p 113 q 114 r 115 s 116 t 117 u 118 v 119 w

   120 x 121 y 122 z 123 { 124 | 125 } 126 ~
   MIXED CASE
```
## __Note__

The ADEs (ASCII Decimal Equivalents) for ASCII are

```
*-------*-------*-------*-------*-------*-------*-------*-------*
| 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|
| 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |
| 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|
| 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |
| 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  ' |
| 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
| 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
| 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
| 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
| 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
| 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
| 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |
| 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
|104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
|112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
|120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|
*-------*-------*-------*-------*-------*-------*-------*-------*
```

## __Standard__

FORTRAN 77 and later, with KIND argument Fortran 2003 and later

## __See Also__

[__char__(3)](CHAR),
[__iachar__(3)](IACHAR),
[__ichar__(3)](ICHAR)

## __Resources__

- [ANSI escape sequences](https://en.wikipedia.org/wiki/ANSI_escape_code)
- [M_attr module](https://github.com/urbanjost/M_attr) for controlling ANSI-compatible terminals

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ACOSH

## __Name__

__acosh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic cosine function

## __Syntax__
```fortran
  result = acosh(x)

   TYPE(kind=KIND),elemental :: acosh

   TYPE(kind=KIND,intent(in) :: x
```
where TYPE may be _real_ or _complex_ and KIND may be any KIND supported
by the associated type.

## __Description__

__acosh(x)__ computes the inverse hyperbolic cosine of __x__ in radians.

## __Arguments__

  - __x__
    : the type shall be _real_ or _complex_.

## __Returns__

The return value has the same type and kind as __x__.

If __x__ is _complex_, the imaginary part of the result is in radians and
lies between

> __0 \<= aimag(acosh(x)) \<= PI__

## __Examples__

Sample program:

```fortran
program demo_acosh
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=dp), dimension(3) :: x = [ 1.0d0, 2.0d0, 3.0d0 ]
   write (*,*) acosh(x)
end program demo_acosh
```
  Results:
```text
 0.000000000000000E+000   1.31695789692482        1.76274717403909
```

## __Standard__

Fortran 2008 and later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [__cosh__(3)](COSH)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ACOS

## __Name__
__acos__(3) - \[MATHEMATICS:TRIGONOMETRIC\] arccosine (inverse cosine) function

## __Syntax__
```fortran
  result = acos(x)

   TYPE(kind=KIND),elemental :: acos

   TYPE(kind=KIND,intent(in) :: x
```
where __TYPE__ may be _real_ or _complex_ and __KIND__ may be any __KIND__ supported
by the associated type.

## __Description__

__acos(x)__ computes the arccosine of __x__ (inverse of __cos(x)__).

## __Arguments__

  - __x__
    : Must be type _real_ or _complex_. If the type is _real_, the value
    must satisfy |__x__| <= 1.

## __Returns__

The return value is of the same type and kind as __x__. The _real_ part of
the result is in radians and lies in the range __0 \<= acos(x%re) \<= PI__ .

## __Examples__
Sample program:
```fortran
program demo_acos
use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64) :: x = 0.866_real64
real(kind=real64),parameter :: d2r=acos(-1.0_real64)/180.0_real64

    print all,'acos(',x,') is ', acos(x)
    print all,'90 degrees is ', d2r*90.0_real64, ' radians'
    print all,'180 degrees is ', d2r*180.0_real64, ' radians'
    print all,'for reference &
    &PI ~ 3.14159265358979323846264338327950288419716939937510'
    print all,'elemental',acos([-1.0,-0.5,0.0,0.50,1.0])

end program demo_acos
```
  Results:
```text
   acos( .8660000000000000 ) is  .5236495809318289
   90 degrees is  1.570796326794897  radians
   180 degrees is  3.141592653589793  radians
   for reference PI ~ 3.14159265358979323846264338327950288419716939937510
   elemental 3.141593 2.094395 1.570796 1.047198 .000000
```
## __Standard__
FORTRAN 77 and later; for a _complex_ argument - Fortran 2008 and later

## __See Also__

 - [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

Inverse function: [__cos__(3](COS))

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ADJUSTL

## __Name__

__adjustl__(3) - \[CHARACTER:WHITESPACE\] Left-adjust a string

## __Syntax__
```fortran
    result = adjustl(string)

     character(len=(len(string)) elemental function adjustr(a)

     character(len=*),intent(in) :: string
```
## __Description__

__adjustl(string)__ will left-adjust a string by removing leading
spaces. Spaces are inserted at the end of the string as needed.

## __Arguments__

  - __string__
    : the type shall be _character_.

## __Returns__

The return value is of type _character_ and of the same kind as __string__
where leading spaces are removed and the same number of spaces are
inserted on the end of __string__.

## __Examples__

Sample program:

```fortran
program demo_adjustl
implicit none
character(len=20) :: str = '   sample string'
character(len=:),allocatable :: astr
    !
    ! basic use
    str = adjustl(str)
    write(*,'("[",a,"]")') str, trim(str)
    !
    ! an allocatable string stays the same length
    ! and is not trimmed.
    astr='    allocatable string   '
    write(*,'("[",a,"]")') adjustl(astr)
    !
end program demo_adjustl
```
Results:
```text
   [sample string       ]
   [sample string]
   [allocatable string       ]
```
## __Standard__

Fortran 95 and later

## __See Also__

[__adjustr__(3)](ADJUSTR)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ADJUSTR

## __Name__

__adjustr__(3) - \[CHARACTER:WHITESPACE\] Right-adjust a string

## __Syntax__
```fortran
    result = adjustr(string)

     elemental function adjustr(a)
     character(len=(len(string)) :: adjustr
     character(len=*),intent(in) :: string
```
## __Description__

__adjustr(string)__ right-adjusts a string by removing trailing
spaces. Spaces are inserted at the start of the string as needed to
retain the original length.

## __Arguments__

  - __string__
    : the type shall be _character_.

## __Returns__

The return value is of type _character_ and of the same kind as __string__
where trailing spaces are removed and the same number of spaces are
inserted at the start of __string__.

## __Examples__

Sample program:

```fortran
program demo_adjustr
implicit none
integer :: right
character(len=20) :: str = ' sample string '
character(len=:),allocatable :: str2
   ! print a short number line
   write(*,'(a)')repeat('1234567890',5)

   !
   ! basic usage
   !
   str = adjustr(str)
   write(*,'(a)') str

   !
   ! elemental
   !
   write(*,'(a)')adjustr([character(len=50) :: &
   '  first           ', &
   '     second       ', &
   '         third    ' ])

   write(*,'(a)')repeat('1234567890',5)
end program demo_adjustr
```
Results:
```text
   12345678901234567890123456789012345678901234567890
          sample string
                                                first
                                               second
                                                third
   12345678901234567890123456789012345678901234567890
```
## __Standard__

Fortran 95 and later

## __See Also__

[__adjustl__(3)](ADJUSTL)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# AIMAG

## __Name__

__aimag__(3) - \[TYPE:NUMERIC\] Imaginary part of complex number

## __Syntax__
```fortran
    result = aimag(z)

     complex(kind=KIND),elemental :: aimag

     complex(kind=KIND),intent(in) :: z
```
## __Description__

__aimag(z)__ yields the imaginary part of complex argument __z__.

## __Arguments__

  - __z__
    : The type of the argument shall be _complex_.

## __Returns__

The return value is of type _real_ with the kind type parameter of the
argument.

## __Examples__

Sample program:

```fortran
program demo_aimag
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
complex(kind=real32) z4
complex(kind=real64) z8
    z4 = cmplx(1.e0, 2.e0)
    z8 = cmplx(3.e0_real64, 4.e0_real64,kind=real64)
    print *, aimag(z4), aimag(z8)
    ! an elemental function can be passed an array
    print *
    print *, [z4,z4/2.0,z4+z4,z4**3]
    print *
    print *, aimag([z4,z4/2.0,z4+z4,z4**3])
end program demo_aimag
```
Results:
```text
  2.000000       4.00000000000000

 (1.000000,2.000000) (0.5000000,1.000000) (2.000000,4.000000)
 (-11.00000,-2.000000)

       2.000000       1.000000       4.000000      -2.000000
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# AINT

## __Name__

__aint__(3) - \[NUMERIC\] Truncate to a whole number

## __Syntax__
```fortran
result = aint(x)

   real(kind=kind(x)),elemental  :: aint

   real(kind=kind(x)),intent(in) :: x
```
or
```fortran
result = aint(x, KIND)

   real(kind=KIND),elemental     :: aint

   integer,intent(in),optional   :: KIND
   real(kind=kind(x)),intent(in) :: x
```
## __Description__

__aint(x, kind)__ truncates its argument to a whole number.

## __Arguments__

  - __x__
    : the type of the argument shall be _real_.

  - __kind__
    : (optional) an _integer_ initialization expression indicating the
    kind parameter of the result.

## __Returns__

The return value is of type _real_ with the kind type parameter of
the argument if the optional __kind__ is absent; otherwise, the kind
type parameter will be given by __kind__. If the magnitude of __x__
is less than one, __aint(x)__ returns zero. If the magnitude is equal
to or greater than one then it returns the largest whole number that
does not exceed its magnitude. The sign is the same as the sign of __x__.

## __Examples__

Sample program:

```fortran
program demo_aint
use, intrinsic :: iso_fortran_env, only : real32, real64
implicit none
real(kind=real32) :: x4
real(kind=real64) :: x8

   x4 = 4.3210_real32
   x8 = 4.3210_real64
   print *, aint(x4), aint(x8)
   print *
   ! elemental
   print *,aint([ &
    &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
    &  0.0,   &
    &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_aint
```
  Results:
```text
     4.00000000       4.0000000000000000

    -2.00000000      -2.00000000      -2.00000000      -2.00000000
    -1.00000000      -1.00000000      -0.00000000       0.00000000
     0.00000000       1.00000000       1.00000000       2.00000000
     2.00000000       2.00000000       2.00000000
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__anint__(3)](ANINT),
[__int__(3)](INT),
[__nint__(3)](NINT),
[__selected_int_kind__(3)](SELECTED_INT_KIND),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions
# ALL

## __Name__

__all__(3) - \[ARRAY REDUCTION\] determines if all the values are true

## __Syntax__
```fortran
result = all(mask, dim)
```
## __Description__

Logical conjunction of elements of __mask__ along dimension __dim__.

"__all(mask, dim)__" determines if all the values are true in __mask__
in the array along dimension __dim__.

## __Arguments__

  - __mask__
    : shall be a logical array. That is, the type of the argument shall be
    _logical_ and it shall not be scalar.

  - __dim__
    : (optional) __dim__ shall be a scalar integer with a value that lies
    between one and the rank of __mask__. The corresponding actual argument
    shall not be an optional dummy argument.

## __Returns__

"__all(mask)__" returns a scalar value of type _logical_ where the kind
type parameter is the same as the kind type parameter of __mask__. If
__dim__ is present, then __all(mask, dim)__ returns an array with the rank
of __mask__ minus 1. The shape is determined from the shape of __mask__
where the __dim__ dimension is elided.

 1.  __all(mask)__ is true if all elements of __mask__ are true. It also is
     true if __mask__ has zero size; otherwise, it is false.

 2.  If the rank of __mask__ is one, then __all(mask, dim)__ is equivalent
     to __all(mask)__. If the rank is greater than one, then __all(mask,
     dim)__ is determined by applying __all()__ to the array sections.

 3.  Result Characteristics. The result is of type _logical_ with the
     same kind type parameter as __mask__. It is scalar if __dim__
     is absent or __n = 1__; otherwise, the result has rank __n - 1__
     and shape __\[d1 , d2 , . . . , dDIM-1 , dDIM+1 , . . . , dn \]__
     where __\[d1 , d2 , . . . , dn \]__ is the shape of __mask__.

 4.  Result Value.

     Case (i):
               : The result of __all(mask)__ has the value true if all
               elements of __mask__ are true or if __mask__ has
               size zero, and the result has value false if any element
               of __mask__ is false.

     Case (ii):
               : If __mask__ has rank one, __all(mask,dim)__ is equal to
               __all(mask)__. Otherwise, the value of element __(s1 , s2 ,
               . . . , sdim-1 , sdim+1 , . . . , sn )__ of all __(mask,
               dim)__ is equal to __all(mask (s1 , s2 , . . . , sdim-1 ,
               :, sdim+1 , . . . , sn ))__.

## __Examples__

Sample program:

```fortran
program demo_all
implicit none
logical l
   l = all([.true., .true., .true.])
   print *, l
   call section

contains

subroutine section
integer a(2,3), b(2,3)
  a = 1
  b = 1
  b(2,2) = 2
  print *, all(a .eq. b, 1)
  print *, all(a .eq. b, 2)
end subroutine section
end program demo_all
```
Results:
```text
    T
    T F T
    T F
```
Case (i):

```text
     The value of all([.TRUE., .FALSE., .TRUE.]) is false.
```

Case (ii):

```text
                          1|3|5
   If B is the array      -+-+-
                          2|4|6

                          0|3|5
   and C is the array     -+-+-
                          7|4|8

   then all(B /= C, DIM = 1) is

      [true, false, false]
```

and __all(B /= C, DIM = 2)__ is

```
        [false, false].
```

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# ALLOCATED

## __Name__

__allocated__(3) - \[ARRAY INQUIRY\] Status of an allocatable entity

## __Syntax__
```fortran
  result = allocated(array)
```
   or
```fortran
  result = allocated(scalar)
```
## __Description__

__allocated(array)__ and __allocated(scalar)__ check the allocation
status of __array__ and __scalar__, respectively.

## __Arguments__

  - __array__
    : the argument shall be an _allocatable_ array.

  - __scalar__
    : the argument shall be an _allocatable_ scalar.

## __Returns__

The return value is a scalar _logical_ with the default logical kind type
parameter. If the argument is allocated then the result is .true.;
otherwise, it returns .false..

## __Examples__

Sample program:

```fortran
program demo_allocated
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
integer :: i = 4
real(kind=sp), allocatable :: x(:)

   if (allocated(x) .eqv. .false.) allocate(x(i))

end program demo_allocated
```

## __Standard__

Fortran 95 and later. Note, the scalar= keyword and allocatable
scalar entities are available in Fortran 2003 and later.

## __See Also__

[__move\_alloc__(3)](MOVE_ALLOC)

###### fortran-lang intrinsic descriptions
# ANINT

## __Name__

__anint__(3) - \[NUMERIC\] Nearest whole number

## __Syntax__
```fortran
result = anint(a, kind)
```
## __Description__

__anint(a \[, kind\])__ rounds its argument to the nearest whole number.

## __Arguments__

  - __a__
    : the type of the argument shall be _real_.

  - __kind__
    : (optional) an _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type real with the kind type parameter of the
argument if the optional __kind__ is absent; otherwise, the kind type
parameter will be given by __kind__. If __a__ is greater than zero, __anint(a)__
returns __aint(a + 0.5)__. If __a__ is less than or equal to zero then it
returns __aint(a - 0.5)__.

## __Examples__

Sample program:

```fortran
program demo_anint
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real32) :: x4
real(kind=real64) :: x8

   x4 = 1.234E0_real32
   x8 = 4.321_real64
   print *, anint(x4), dnint(x8)
   x8 = anint(x4,kind=real64)
   print *, x8
   print *
   ! elemental
   print *,anint([ &
    & -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
    &  0.0, &
    & +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_anint
```
  Results:
```text
    1.00000000       4.0000000000000000
    1.0000000000000000

   -3.00000000      -3.00000000      -2.00000000      -2.00000000
   -2.00000000      -1.00000000      -1.00000000       0.00000000
    1.00000000       1.00000000       2.00000000       2.00000000
    2.00000000       3.00000000       3.00000000
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__aint__(3)](AINT),
[__int__(3)](INT),
[__nint__(3)](NINT),
[__selected_int_kind__(3)](SELECTED_INT_KIND),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions
# ANY

## __Name__

__any__(3) - \[ARRAY REDUCTION\] determines if any of the values in the logical array are true.

## __Syntax__
```fortran
result = any(mask, dim)
```
## __Description__

__any(mask, dim)__ determines if any of the values in the logical
array __mask__ along dimension __dim__ are __.true.__.

## __Arguments__

  - __mask__
    : the type of the argument shall be _logical_ and it shall not be
    scalar.

  - __dim__
    : (optional) dim shall be a scalar integer with a value that lies
    between one and the rank of mask.

## __Returns__

__any(mask)__ returns a scalar value of type _logical_ where the kind type
parameter is the same as the kind type parameter of __mask__. If __dim__ is
present, then __any(mask, dim)__ returns an array with the rank of __mask__
minus 1. The shape is determined from the shape of __mask__ where the __dim__
dimension is elided.

1.  __any(mask)__ is true if any element of __mask__ is true; otherwise, it
    is __.false.__. It also is false if __mask__ has zero size.

2.  If the rank of __mask__ is one, then __any(mask, dim)__ is equivalent to
    __any(mask)__. If the rank is greater than one, then __any(mask,
    dim)__ is determined by applying __any()__ to the array sections.

## __Examples__

Sample program:
```fortran
program demo_any
implicit none
logical l
   l = any([.true., .true., .true.])
   print *, l
   call section
   contains
     subroutine section
     integer a(2,3), b(2,3)
       a = 1
       b = 1
       b(2,2) = 2
       print *, any(a .eq. b, 1)
       print *, any(a .eq. b, 2)
     end subroutine section
end program demo_any
```
  Results:
```text
    T
    T T T
    T T
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# ASINH

## __Name__

__asinh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic sine function

## __Syntax__
```fortran
result = asinh(x)

    elemental TYPE(kind=KIND) function asinh(x)
    TYPE(kind=KIND) :: x
```
Where the returned value has the kind of the input value
and TYPE may be _real_ or _complex_

## __Description__

__asinh(x)__ computes the inverse hyperbolic sine of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value is of the same type and kind as __x__. If __x__ is _complex_, the
imaginary part of the result is in radians and lies between
__-PI/2 \<= aimag(asinh(x)) \<= PI/2__.

## __Examples__

Sample program:

```fortran
program demo_asinh
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=dp), dimension(3) :: x = [ -1.0d0, 0.0d0, 1.0d0 ]

    write (*,*) asinh(x)

end program demo_asinh
```
  Results:
```text
  -0.88137358701954305  0.0000000000000000  0.88137358701954305
```

## __Standard__

Fortran 2008 and later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [__sinh__(3)](SINH)

###### fortran-lang intrinsic descriptions
# ASIN

## __Name__

__asin__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arcsine function

## __Syntax__
```fortran
result = asin(x)

    elemental TYPE(kind=KIND) function asin(x)
    TYPE(kind=KIND) :: x
```
where the returned value has the kind of the input value
and TYPE may be _real_ or _complex_

## __Description__

__asin(x)__ computes the arcsine of its argument __x__.

The arcsine is the inverse function of the sine function. It is commonly
used in trigonometry when trying to find the angle when the lengths of
the hypotenuse and the opposite side of a right triangle are known.

## __Arguments__

  - __x__
    : The type shall be either _real_ and a magnitude that is less than or
    equal to one; or be _complex_.

## __Returns__

  - __result__
    : The return value is of the same type and kind as __x__. The real part of
    the result is in radians and lies in the range __-PI/2 \<=
    asin(x) \<= PI/2__.

## __Examples__

The arcsine will allow you to find the measure of a right angle when you
know the ratio of the side opposite the angle to the hypotenuse.

So if you knew that a train track rose 1.25 vertical miles on a track
that was 50 miles long, you could determine the average angle of incline
of the track using the arcsine. Given

     sin(theta) = 1.25 miles/50 miles (opposite/hypotenuse)

```fortran
program demo_asin
use, intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
! value to convert degrees to radians
real(kind=dp),parameter :: D2R=acos(-1.0_dp)/180.0_dp
real(kind=dp)           :: angle, rise, run
character(len=*),parameter :: all='(*(g0,1x))'
  ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)
  ! then taking the arcsine of both sides of the equality yields
  ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)
  rise=1.250_dp
  run=50.00_dp
  angle = asin(rise/run)
  print all, 'angle of incline(radians) = ', angle
  angle = angle/D2R
  print all, 'angle of incline(degrees) = ', angle

  print all, 'percent grade=',rise/run*100.0_dp
end program demo_asin
```

Results:
```
    angle of incline(radians) =    2.5002604899361139E-002
    angle of incline(degrees) =    1.4325437375665075
    percent grade=   2.5000000000000000
```
The percentage grade is the slope, written as a percent. To calculate
the slope you divide the rise by the run. In the example the rise is
1.25 mile over a run of 50 miles so the slope is 1.25/50 = 0.025.
Written as a percent this is 2.5 %.

For the US, two and 1/2 percent is generally thought of as the upper
limit. This means a rise of 2.5 feet when going 100 feet forward. In
the US this was the maximum grade on the first major US railroad, the
Baltimore and Ohio. Note curves increase the frictional drag on a
train reducing the allowable grade.

## __Standard__

FORTRAN 77 and later, for a complex argument Fortran 2008 or later

## __See Also__

 - [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

Inverse function: [__sin__(3)](SIN)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ASSOCIATED

## __Name__

__associated__(3) - \[STATE\] Status of a pointer or pointer/target pair

## __Syntax__
```fortran
result = associated(pointer, target)
```
## __Description__

__associated(pointer \[, target\])__ determines the status of the
pointer __pointer__ or if __pointer__ is associated with the target __target__.

## __Arguments__

  - __pointer__
    : __pointer__ shall have the _pointer_ attribute and it can be of any type.

  - __target__
    : (Optional) __target__ shall be a pointer or a target. It must have the
    same type, kind type parameter, and array rank as __pointer__.

The association status of neither __pointer__ nor __target__ shall be undefined.

## __Returns__

__associated(pointer)__ returns a scalar value of type _logical_.
There are several cases:

1.  When the optional __target__ is not present then __associated(pointer)__
    is true if __pointer__ is associated with a target; otherwise, it
    returns false.

2.  If __target__ is present and a scalar target, the result is true if
    __target__ is not a zero-sized storage sequence and the target
    associated with __pointer__ occupies the same storage units. If __pointer__
    is disassociated, the result is false.

3.  If __target__ is present and an array target, the result is true if
    __target__ and __pointer__ have the same shape, are not zero-sized arrays,
    are arrays whose elements are not zero-sized storage sequences, and
    __target__ and __pointer__ occupy the same storage units in array element
    order.

    As in case 2, the result is false, if __pointer__ is disassociated.

4.  If __target__ is present and an scalar pointer, the result is true if
    __target__ is associated with __pointer__, the target associated with __target__
    are not zero-sized storage sequences and occupy the same storage
    units.

    The result is __.false.__, if either __target__ or __pointer__ is disassociated.

5.  If __target__ is present and an array pointer, the result is true if
    target associated with __pointer__ and the target associated with __target__
    have the same shape, are not zero-sized arrays, are arrays whose
    elements are not zero-sized storage sequences, and __target__ and
    __pointer__ occupy the same storage units in array element order. The
    result is false, if either __target__ or __pointer__ is disassociated.

## __Examples__

Sample program:

```fortran
program demo_associated
implicit none
real, target  :: tgt(2) = [1., 2.]
real, pointer :: ptr(:)
   ptr => tgt
   if (associated(ptr)     .eqv. .false.) &
   & stop 'POINTER NOT ASSOCIATED'
   if (associated(ptr,tgt) .eqv. .false.) &
   & stop 'POINTER NOT ASSOCIATED TO TARGET'
end program demo_associated
```
## __Standard__

Fortran 95 and later

## __See Also__

[__null__(3)](NULL)

###### fortran-lang intrinsic descriptions
# ATAN2

## __Name__

__atan2__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function

## __Syntax__
```fortran
result = atan2(y, x)
```
## __Description__

__atan2(y, x)__ computes the arctangent of the complex number
( __x__ + i __y__ ) .

This function can be used to transform from Cartesian into polar
coordinates and allows to determine the angle in the correct quadrant.
To convert from Cartesian Coordinates __(x,y)__ to polar coordinates

(r,theta): $$ \begin{aligned} r &= \sqrt{x**2 + y**2} \\ \theta
&= \tan**{__-1__}(y / x) \end{aligned} $$

## __Arguments__

  - __y__
    : The type shall be _real_.

  - __x__
    : The type and kind type parameter shall be the same as __y__. If __y__ is
    zero, then __x__ must be nonzero.

## __Returns__

The return value has the same type and kind type parameter as __y__. It is
the principal value of the complex number __(x + i, y)__. If x is nonzero,
then it lies in the range __-PI \<= atan(x) \<= PI__. The sign is
positive if __y__ is positive. If __y__ is zero, then the return value is zero
if __x__ is strictly positive, __PI__ if __x__ is negative and __y__ is positive zero
(or the processor does not handle signed zeros), and __-PI__ if __x__ is
negative and __Y__ is negative zero. Finally, if __x__ is zero, then the
magnitude of the result is __PI/2__.

## __Examples__

Sample program:

```fortran
program demo_atan2
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x = 1.e0_sp, y = 0.5e0_sp, z
   z = atan2(y,x)
   write(*,*)x,y,z
end program demo_atan2
```
Results:
```text
      1.00000000      0.500000000      0.463647604
```

## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# ATANH

## __Name__

__atanh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic tangent function

## __Syntax__
```fortran
result = atanh(x)
```
## __Description__

__atanh(x)__ computes the inverse hyperbolic tangent of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__. If __x__ is _complex_, the
imaginary part of the result is in radians and lies between

__-PI/2 \<= aimag(atanh(x)) \<= PI/2__

## __Examples__

Sample program:

```fortran
program demo_atanh
implicit none
real, dimension(3) :: x = [ -1.0, 0.0, 1.0 ]

   write (*,*) atanh(x)

end program demo_atanh
```
  Results:
```text
   -Infinity   0.00000000             Infinity
```

## __Standard__

Fortran 2008 and later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [__tanh__(3)](TANH)

###### fortran-lang intrinsic descriptions
# ATAN

## __Name__

__atan__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function

## __Syntax__
```fortran
  - result = __atan(y, x)__

   TYPE(kind=KIND):: atan
   TYPE(kind=KIND,intent(in) :: x
   TYPE(kind=KIND,intent(in),optional :: y
```
where __TYPE__ may be _real_ or _complex_ and __KIND__ may be any __KIND__ supported
by the associated type. If __y__ is present __x__ is _real`.

## __Description__

__atan(x)__ computes the arctangent of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_; if __y__ is present, __x__
      shall be _real_.

  - __y__
    : Shall be of the same type and kind as __x__. If __x__ is zero, __y__
    must not be zero.

## __Returns__

The returned value is of the same type and kind as __x__. If __y__ is
present, the result is identical to __atan2(y,x)__. Otherwise, it is the
arc tangent of __x__, where the real part of the result is in radians
and lies in the range
__-PI/2 \<= atan(x) \<= PI/2__

## __Examples__

Sample program:

```fortran
program demo_atan
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64),parameter :: &
 Deg_Per_Rad = 57.2957795130823208767981548_real64
real(kind=real64) :: x
    x=2.866_real64
    print all, atan(x)

    print all, atan( 2.0d0, 2.0d0),atan( 2.0d0, 2.0d0)*Deg_Per_Rad
    print all, atan( 2.0d0,-2.0d0),atan( 2.0d0,-2.0d0)*Deg_Per_Rad
    print all, atan(-2.0d0, 2.0d0),atan(-2.0d0, 2.0d0)*Deg_Per_Rad
    print all, atan(-2.0d0,-2.0d0),atan(-2.0d0,-2.0d0)*Deg_Per_Rad

end program demo_atan
```
  Results:
```text
   1.235085437457879
   .7853981633974483 45.00000000000000
   2.356194490192345 135.0000000000000
   -.7853981633974483 -45.00000000000000
   -2.356194490192345 -135.0000000000000
```
## __Standard__

FORTRAN 77 and later for a complex argument; and for two
arguments Fortran 2008 or later

## __See Also__

 - [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

[__atan2__(3)](ATAN2), [__tan__(3)](TAN)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ATOMIC\_ADD

## __Name__

__atomic\_add__(3) - \[ATOMIC\] Atomic ADD operation

## __Syntax__
```fortran
call atomic_add (atom, value, stat)
```
## __Description__

__atomic\_ad(atom, value)__ atomically adds the value of VAR to the
variable __atom__. When __stat__ is present and the invocation was successful,
it is assigned the value 0. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
ATOM, if the remote image has stopped, it is assigned the value of
iso\_fortran\_env's stat\_stopped\_image and if the remote image has
failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_add
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_add (atom[1], this_image())
end program demo_atomic_add
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_fetch\_add__(3)](ATOMIC_FETCH),
[__atomic\_and__(3)](ATOMIC_AND),
[__atomic\_or__(3)](ATOMIC_OR),
[__atomic\_xor__(3)](ATOMIC_XOR)
__iso\_fortran\_env__(3),

###### fortran-lang intrinsic descriptions
# ATOMIC\_AND

## __Name__

__atomic\_and__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise AND operation

## __Syntax__
```fortran
call atomic_and(atom, value, stat)
```
## __Description__

__atomic\_and(atom, value)__ atomically defines __atom__ with the bitwise
__and__ between the values of __atom__ and __value__. When __stat__ is present and the
invocation was successful, it is assigned the value 0. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed __atom__, if the remote image has stopped, it is
assigned the value of iso\_fortran\_env's stat\_stopped\_image and if
the remote image has failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_and
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_and(atom[1], int(b'10100011101'))
end program demo_atomic_and
```
## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_fetch\_and__(3)](ATOMIC_FETCH_AND),
[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_ref__(3)](ATOMIC_REF),
[__atomic\_cas__(3)](ATOMIC_CAS),
__iso\_fortran\_env__(3),
[__atomic\_add__(3)](ATOMIC_ADD),
[__atomic\_or__(3)](ATOMIC_OR),
[__atomic\_xor__(3)](ATOMIC_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC\_CAS

## __Name__

__atomic\_cas__(3) - \[ATOMIC\] Atomic compare and swap

## __Syntax__
```fortran
call atomic_cas (atom, old, compare, new, stat)
```
## __Description__

atomic\_cas compares the variable __atom__ with the value of __compare__; if the
value is the same, __atom__ is set to the value of __new__. Additionally, __old__ is
set to the value of __atom__ that was used for the comparison. When __stat__ is
present and the invocation was successful, it is assigned the value 0.
If it is present and the invocation has failed, it is assigned a
positive value; in particular, for a coindexed __atom__, if the remote image
has stopped, it is assigned the value of iso\_fortran\_env's
stat\_stopped\_image and if the remote image has failed, the value
stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of either integer type with
    atomic\_int\_kind kind or logical type with atomic\_logical\_kind
    kind.

  - __old__
    : Scalar of the same type and kind as __atom__.

  - __compare__
    : Scalar variable of the same type and kind as __atom__.

  - __new__
    : Scalar variable of the same type as __atom__. If kind is different, the
    value is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_cas
use iso_fortran_env
implicit none
logical(atomic_logical_kind) :: atom[*], prev
   call atomic_cas(atom[1], prev, .false., .true.)
end program demo_atomic_cas
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_ref__(3)](ATOMIC_REF),
[__iso\_fortran\_env__(3)]()

###### fortran-lang intrinsic descriptions
# ATOMIC\_DEFINE

## __Name__

__atomic\_define__(3) - \[ATOMIC\] Setting a variable atomically

## __Syntax__
```fortran
call atomic_define (atom, value, stat)

   subroutine atomic_define(atom, value, stat)
   TYPE(kind=KIND) :: atom
   TYPE(kind=KIND) :: value
   integer,intent(out),optional :: stat
```
## __Description__

__atomic\_define(atom, value)__ defines the variable __atom__ with the value
__value__ atomically. When __stat__ is present and the invocation was
successful, it is assigned the value __0__. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed __atom__, if the remote image has stopped, it is assigned
the value of iso\_fortran\_env's stat\_stopped\_image and if the remote
image has failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of either integer type with
    atomic\_int\_kind kind or logical type with atomic\_logical\_kind
    kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_define
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
    call atomic_define(atom[1], this_image())
end program demo_atomic_define
```
## __Standard__

Fortran 2008 and later; with __stat__, TS 18508 or later

## __See Also__

[__atomic\_ref__(3)](ATOMIC_REF),
[__atomic\_cas__(3)](ATOMIC_CAS),
__iso\_fortran\_env__(3),
[__atomic\_add__(3)](ATOMIC_ADD),
[__atomic\_and__(3)](ATOMIC_AND),
[__atomic\_or__(3)](ATOMIC_OR),
[__atomic\_xor__(3)](ATOMIC_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC\_FETCH\_ADD

## __Name__

__atomic\_fetch\_add__(3) - \[ATOMIC\] Atomic ADD operation with prior fetch

## __Syntax__
```fortran
call atomic_fetch_add(atom, value, old, stat)
```
## __Description__

__atomic\_fetch\_add(atom, value, old)__ atomically stores the value of
__atom__ in __old__ and adds the value of __var__ to the variable __atom__. When __stat__ is
present and the invocation was successful, it is assigned the value __0__.
If it is present and the invocation has failed, it is assigned a
positive value; in particular, for a coindexed __atom__, if the remote image
has stopped, it is assigned the value of iso\_fortran\_env's
stat\_stopped\_image and if the remote image has failed, the value
stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind. atomic\_logical\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __old__
    : Scalar of the same type and kind as __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_fetch_add
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_add(atom[1], this_image(), old)
end program demo_atomic_fetch_add
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_add__(3)](ATOMIC_ADD),
__iso\_fortran\_env__(3),

[__atomic\_fetch\_and__(3)](ATOMIC_FETCH_AND),
[__atomic\_fetch\_or__(3)](ATOMIC_FETCH_OR),

[__atomic\_fetch\_xor__(3)](ATOMIC_FETCH_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC\_FETCH\_AND

## __Name__

__atomic\_fetch\_and__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise AND operation with prior fetch

## __Syntax__
```fortran
call atomic_fetch_and(atom, value, old, stat)
```
## __Description__

__atomic\_fetch\_and(atom, value, old)__ atomically stores the value of
__atom__ in __old__ and defines __atom__ with the bitwise AND between the values of
__atom__ and __value__. When __stat__ is present and the invocation was successful,
it is assigned the value __0__. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
__atom__, if the remote image has stopped, it is assigned the value of
iso\_fortran\_env's stat\_stopped\_image and if the remote image has
failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __old__
    : Scalar of the same type and kind as __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_fetch_and
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_fetch_and (atom[1], int(b'10100011101'), old)
end program demo_atomic_fetch_and
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_and__(3)](ATOMIC_AND),
[__iso\_fortran\_env__(3)](),

[__atomic\_fetch\_add__(3)](ATOMIC_FETCH_ADD),
[__atomic\_fetch\_or__(3)](ATOMIC_FETCH_OR),

[__atomic\_fetch\_xor__(3)](ATOMIC_FETCH_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC\_FETCH\_OR

## __Name__

__atomic\_fetch\_or__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation with prior fetch

## __Syntax__
```fortran
call atomic_fetch_or(atom, value, old, stat)
```
## __Description__

__atomic\_fetch\_or(atom, value, old)__ atomically stores the value of
__atom__ in __old__ and defines __atom__ with the bitwise OR between the values of
__atom__ and __value__. When __stat__ is present and the invocation was successful,
it is assigned the value __0__. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
__atom__, if the remote image has stopped, it is assigned the value of
iso\_fortran\_env's stat\_stopped\_image and if the remote image has
failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __old__
    : Scalar of the same type and kind as __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_fetch_or
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_fetch_or(atom[1], int(b'10100011101'), old)
end program demo_atomic_fetch_or
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_or__(3)](ATOMIC_OR),
[__iso\_fortran\_env__(3)](),

[__atomic\_fetch\_add__(3)](ATOMIC_FETCH_ADD),
[__atomic\_fetch\_and__(3)](ATOMIC_FETCH_AND),

[__atomic\_fetch\_xor__(3)](ATOMIC_FETCH_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC\_FETCH\_XOR

## __Name__

__atomic\_fetch\_xor__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise XOR operation with prior fetch

## __Syntax__
```fortran
call atomic_fetch_xor (atom, value, old, stat)
```
## __Description__

__atomic\_fetch\_xor(atom, value, old)__ atomically stores the value of
__atom__ in __old__ and defines __atom__ with the bitwise __xor__ between the values of
__atom__ and __value__. When __stat__ is present and the invocation was successful,
it is assigned the value __0__. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
__atom__, if the remote image has stopped, it is assigned the value of
iso\_fortran\_env's stat\_stopped\_image and if the remote image has
failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __old__
    : Scalar of the same type and kind as __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_fetch_xor
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_fetch_xor (atom[1], int(b'10100011101'), old)
end program demo_atomic_fetch_xor
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_xor__(3)](ATOMIC_XOR),
[__iso\_fortran\_env__(3)](),

[__atomic\_fetch\_add__(3)](ATOMIC_FETCH_ADD),
[__atomic\_fetch\_and__(3)](ATOMIC_FETCH_AND),

[__atomic\_fetch\_or__(3)](ATOMIC_FETCH_OR)

###### fortran-lang intrinsic descriptions
# ATOMIC\_OR

## __Name__

__atomic\_or__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation

## __Syntax__
```fortran
call atomic_or__(atom, value, stat)
```
## __Description__

__atomic\_or(atom, value)__ atomically defines __atom__ with the bitwise __or__
between the values of __atom__ and __value__. When __stat__ is present and the
invocation was successful, it is assigned the value __0__. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed __atom__, if the remote image has stopped, it is
assigned the value of iso\_fortran\_env's stat\_stopped\_image and if
the remote image has failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_or
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_or(atom[1], int(b'10100011101'))
end program demo_atomic_or
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_fetch\_or__(3)](ATOMIC_FETCH),

[__iso\_fortran\_env__(3)](),
[__atomic\_add__(3)](ATOMIC_ADD),
[__atomic\_or__(3)](ATOMIC_OR),

[__atomic\_xor__(3)](ATOMIC_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC\_REF

## __Name__

__atomic\_ref__(3) - \[ATOMIC\] Obtaining the value of a variable atomically

## __Syntax__
```fortran
call atomic_ref(value, atom, stat)
```
## __Description__

__atomic\_ref(value, atom)__ atomically assigns the value of the
variable __atom__ to __value__. When __stat__ is present and the invocation was
successful, it is assigned the value __0__. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed __atom__, if the remote image has stopped, it is assigned
the value of iso\_fortran\_env's __stat\_stopped\_image__ and if the remote
image has failed, the value __stat\_failed\_image__.

## __Arguments__

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __atom__
    : Scalar coarray or coindexed variable of either integer type with
    atomic\_int\_kind kind or logical type with atomic\_logical\_kind
    kind.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_ref
use iso_fortran_env
implicit none
logical(atomic_logical_kind) :: atom[*]
logical :: val
   call atomic_ref( val, atom[1] )
   ! ```
   call atomic_ref( val, atom[1] )
   if (val) then
      print *, "Obtained"
   endif
end program demo_atomic_ref
```

## __Standard__

Fortran 2008 and later; with STAT, TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_cas__(3)](ATOMIC_CAS),
[__iso\_fortran\_env__(3)](),

[__atomic\_fetch\_add__(3)](ATOMIC_ADD),
[__atomic\_fetch\_and__(3)](ATOMIC_AND),

[__atomic\_fetch\_or__(3)](ATOMIC_OR),
[__atomic\_fetch\_xor__(3)](ATOMIC_XOR)

###### fortran-lang intrinsic descriptions
# ATOMIC\_XOR

## __Name__

__atomic\_xor__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation

## __Syntax__
```fortran
call atomic_xor(atom, value, stat)
```
## __Description__

__atomic\_xor(atom, value)__ atomically defines __atom__ with the bitwise
__xor__ between the values of __atom__ and __value__. When __stat__ is present and the
invocation was successful, it is assigned the value __0__. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed __atom__, if the remote image has stopped, it is
assigned the value of iso\_fortran\_env's stat\_stopped\_image and if
the remote image has failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_xor
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_xor(atom[1], int(b'10100011101'))
end program demo_atomic_xor
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_fetch\_xor__(3)](ATOMIC_FETCH),
[__iso\_fortran\_env__(3)](),
[__atomic\_add__(3)](ATOMIC_ADD),
[__atomic\_or__(3)](ATOMIC_OR),
[__atomic\_xor__(3)](ATOMIC_XOR)

###### fortran-lang intrinsic descriptions
# BESSEL\_J0

## __Name__

__bessel\_j0__(3) - \[MATHEMATICS\] Bessel function of the first kind of order 0

## __Syntax__
```fortran
    result = bessel_j0(x)
```
## __Description__

__bessel\_j0(x)__ computes the Bessel function of the first kind
of order __0__ of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_ and lies in the range
__-0.4027 \<= bessel(0,x) \<= 1__. It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besj0
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
   implicit none
   real(kind=real64) :: x = 0.0_real64
   x = bessel_j0(x)
   write(*,*)x
end program demo_besj0
```
  Results:
```text
      1.0000000000000000
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j1__(3)](BESSEL_J1),
[__bessel\_jn__(3)](BESSEL_JN),
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_y1__(3)](BESSEL_Y1),
[__bessel\_yn__(3)](BESSEL_YN)

###### fortran-lang intrinsic descriptions
# BESSEL\_J1

## __Name__

__bessel\_j1__(3) - \[MATHEMATICS\] Bessel function of the first kind of order 1

## __Syntax__
```fortran
    result = bessel_j1(x)
```
## __Description__

__bessel\_j1(x)__ computes the Bessel function of the first kind
of order __1__ of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_ and lies in the range
__-0.5818 \<= bessel(0,x) \<= 0.5818__ . It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besj1
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
   x = bessel_j1(x)
   write(*,*)x
end program demo_besj1
```
  Results:
```text
     0.44005058574493350
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_jn__(3)](BESSEL_JN),
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_y1__(3)](BESSEL_Y1),
[__bessel\_yn__(3)](BESSEL_YN)

###### fortran-lang intrinsic descriptions
# BESSEL\_JN

## __Name__

__bessel\_jn__(3) - \[MATHEMATICS\] Bessel function of the first kind

## __Syntax__
```fortran
  result = bessel_jn(n, x)

  result = bessel_jn(n1, n2, x)
```
## __Description__

__bessel\_jn(n, x)__ computes the Bessel function of the first
kind of order __n__ of __x__. If __n__ and __x__ are arrays, their ranks and shapes
shall conform.

__bessel\_jn(n1, n2, x)__ returns an array with the Bessel function\|Bessel functions
of the first kind of the orders __n1__ to __n2__.

## __Arguments__

  - __n__
    : Shall be a scalar or an array of type _integer_.

  - __n1__
    : Shall be a non-negative scalar of type _integer_.

  - __n2__
    : Shall be a non-negative scalar of type _integer_.

  - __x__
    : Shall be a scalar or an array of type _real_. For
    __bessel\_jn(n1, n2, x)__ it shall be scalar.

## __Returns__

The return value is a scalar of type _real_. It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besjn
use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
    x = bessel_jn(5,x)
    write(*,*)x
end program demo_besjn
```
  Results:
```text
      2.4975773021123450E-004
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_j1__(3)](BESSEL_J1),
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_y1__(3)](BESSEL_Y1),
[__bessel\_yn__(3)](BESSEL_YN)

###### fortran-lang intrinsic descriptions
# BESSEL\_Y0

## __Name__

__bessel\_y0__(3) - \[MATHEMATICS\] Bessel function of the second kind of order 0

## __Syntax__
```fortran
    result = bessel_y0(x)
```
## __Description__

__bessel\_y0(x)__ computes the Bessel function of the second
kind of order 0 of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_. It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besy0
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
  real(kind=real64) :: x = 0.0_real64
  x = bessel_y0(x)
  write(*,*)x
end program demo_besy0
```
  Results:
```text
                    -Infinity
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_j1__(3)](BESSEL_J1),
[__bessel\_jn__(3)](BESSEL_JN),
[__bessel\_y1__(3)](BESSEL_Y1),
[__bessel\_yn__(3)](BESSEL_YN)

###### fortran-lang intrinsic descriptions
# BESSEL\_Y1

## __Name__

__bessel\_y1__(3) - \[MATHEMATICS\] Bessel function of the second kind of order 1

## __Syntax__
```fortran
    result = bessel_y1(x)
```
## __Description__

__bessel\_y1(x)__ computes the Bessel function of the second
kind of order 1 of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is _real_. It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besy1
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
  real(kind=real64) :: x = 1.0_real64
  write(*,*)x, bessel_y1(x)
end program demo_besy1
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_j1__(3)](BESSEL_J1),
[__bessel\_jn__(3)](BESSEL_JN),
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_yn__(3)](BESSEL_YN)

###### fortran-lang intrinsic descriptions
# BESSEL\_YN

## __Name__

__bessel\_yn__(3) - \[MATHEMATICS\] Bessel function of the second kind

## __Syntax__
```fortran
  result = bessel_yn(n, x)

  result = bessel_yn(n1, n2, x)
```
## __Description__

__bessel\_yn(n, x)__ computes the Bessel function of the second
kind of order __n__ of __x__. If __n__ and __x__ are arrays, their ranks and shapes
shall conform.

__bessel\_yn(n1, n2, x)__ returns an array with the Bessel
function\|Bessel functions of the first kind of the orders __n1__ to __n2__.

## __Arguments__

  - __n__
    : Shall be a scalar or an array of type _integer_.

  - __n1__
    : Shall be a non-negative scalar of type _integer_.

  - __n2__
    : Shall be a non-negative scalar of type _integer_.

  - __x__
    : Shall be a scalar or an array of type _real_; for
    __bessel\_yn(n1, n2, x)__ it shall be scalar.

## __Returns__

The return value is _real_. It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besyn
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
  write(*,*) x,bessel_yn(5,x)
end program demo_besyn
```
  Results:
```text
      1.0000000000000000       -260.40586662581222
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_j1__(3)](BESSEL_J1),
[__bessel\_jn__(3)](BESSEL_JN),
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_y1__(3)](BESSEL_Y1)

###### fortran-lang intrinsic descriptions
# BGE

## __Name__

__bge__(3) - \[BIT:COMPARE\] Bitwise greater than or equal to

## __Syntax__
```fortran
    result = bge(i, j)
```
## __Description__

Determines whether an integer is bitwise greater than or equal to
another.

## __Arguments__

  - __i__
    : Shall be of _integer_ type.

  - __j__
    : Shall be of _integer_ type, and of the same kind as __i__.

## __Returns__

The return value is of type _logical_ and of the default kind.

## __Standard__

Fortran 2008 and later

## __See Also__

[__bgt__(3)](BGT),
[__ble__(3)](BLE),
[__blt__(3)](BIT)

###### fortran-lang intrinsic descriptions
# BGT

## __Name__

__bgt__(3) - \[BIT:COMPARE\] Bitwise greater than

## __Syntax__
```fortran
    result = bgt(i, j)
```
## __Description__

Determines whether an integer is bitwise greater than another.

## __Arguments__

  - __i__
    : Shall be of _integer_ type or a BOZ literal constant.

  - __j__
    : Shall be of _integer_ type, and of the same kind as __i__; or a BOZ
    literal constant.

## __Returns__

The return value is of type _logical_ and of the default kind. The result
is true if the sequence of bits represented by _i_ is greater than the
sequence of bits represented by _j_, otherwise the result is false.

## __Standard__

Fortran 2008 and later

## __See Also__

[__bge__(3),](BGE),
[__ble__(3),](BLE),
[__blt__(3)](BLT)

###### fortran-lang intrinsic descriptions
# BIT\_SIZE

## __Name__

__bit\_size__(3) - \[BIT:INQUIRY\] Bit size inquiry function

## __Syntax__
```fortran
    result = bit_size(i)

     function(kind=KIND) :: bit_size
     integer(kind=KIND),intent(in) :: ii
```
## __Description__

__bit\_size(i)__ returns the number of bits (integer precision plus sign
bit) represented by the type of the _integer_ __i__.  __i__ can be a
scalar or an array.

## __Arguments__

  - __i__
    : An _integer_ value of any kind to determine the size of in bits.
    Because only the type of the argument is examined, the argument need
    not be defined.

## __Returns__
    Returns the number of bits used to represent a value of the type
    of __i__.  The result is a _integer_ scalar of the same kind as __i__.

## __Examples__

Sample program:

```fortran
program demo_bit_size
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer(kind=int64)          :: answer
integer                      :: ilen
character(len=*),parameter   :: fmt='(*(g0,1x))'
    write(*,fmt)'default integer size is',bit_size(0),'bits'
    write(*,fmt)bit_size(bit_size(0_int8)), 'which is kind=',kind(0_int8)
    write(*,fmt)bit_size(bit_size(0_int16)),'which is kind=',kind(0_int16)
    write(*,fmt)bit_size(bit_size(0_int32)),'which is kind=',kind(0_int32)
    write(*,fmt)bit_size(bit_size(0_int64)),'which is kind=',kind(0_int64)

    ! Check size of value not explicitly defined.
    write(*,fmt) int(bit_size(answer))
end program demo_bit_size
```
  Typical Results:
```text
   default integer size is 32 bits
   8 which is kind= 1
   16 which is kind= 2
   32 which is kind= 4
   64 which is kind= 8
   64
```

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions (License: MIT) @urbanjost
# BLE

## __Name__

__ble__(3) - \[BIT:COMPARE\] Bitwise less than or equal to

## __Syntax__
```fortran
    result = ble(i, j)
```
## __Description__

Determines whether an integer is bitwise less than or equal to another.

## __Arguments__

  - __i__
    : Shall be of _integer_ type.

  - __j__
    : Shall be of _integer_ type, and of the same kind as __i__.

## __Returns__

The return value is of type _logical_ and of the default kind.

## __Standard__

Fortran 2008 and later

## __See Also__

[__bge__(3),](BGE),
[__bgt__(3),](BGT),
[__blt__(3)](BLT)

###### fortran-lang intrinsic descriptions
# BLT

## __Name__

__blt__(3) - \[BIT:COMPARE\] Bitwise less than

## __Syntax__
```fortran
    result = blt(i, j)
```
## __Description__

Determines whether an integer is bitwise less than another.

## __Arguments__

  - __i__
    : Shall be of _integer_ type.

  - __j__
    : Shall be of _integer_ type, and of the same kind as __i__.

## __Returns__

The return value is of type _logical_ and of the default kind.

## __Standard__

Fortran 2008 and later

## __See Also__

[__bge__(3)](BGE),
[__bgt__(3)](BGT),
[__ble__(3)](BLE)

###### fortran-lang intrinsic descriptions
# BTEST

## __Name__

__btest__(3) - \[BIT:INQUIRY\] Tests a bit of an _integer_ value.

## __Syntax__
```fortran
   result = btest(i, pos)

    integer(kind=KIND) elemental function btest(i,pos)
    integer,intent(in)  :: i
    logical,intent(out) :: pos
```
 where __KIND__ is any _integer_ kind supported by the programming environment.

## __Description__

__btest(i,pos)__ returns logical __.true.__ if the bit at __pos__ in __i__ is set.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __pos__
    : The bit position to query. it must be a valid position for the
    value __i__; ie.  __0 <= pos <= bit_size(i)__ .

    A value of zero refers to the least significant bit.

## __Returns__

   The result is a _logical_ that has the value __.true.__ if bit
   position __pos__ of __i__ has the value __1__ and the value
   __.false.__ if bit __pos__ of __i__ has the value __0__.

## __Examples__

Sample program:

```fortran
program demo_btest
implicit none
integer :: i, j, pos, a(2,2)
logical :: bool
character(len=*),parameter :: g='(*(g0))'

     i = 32768 + 1024 + 64
    write(*,'(a,i0,"=>",b32.32,/)')'Looking at the integer: ',i

    ! looking one bit at a time from LOW BIT TO HIGH BIT
    write(*,g)'from bit 0 to bit ',bit_size(i),'==>'
    do pos=0,bit_size(i)-1
        bool = btest(i, pos)
        write(*,'(l1)',advance='no')bool
    enddo
    write(*,*)

    ! a binary format the hard way.
    ! Note going from bit_size(i) to zero.
    write(*,*)
    write(*,g)'so for ',i,' with a bit size of ',bit_size(i)
    write(*,'(b32.32)')i
    write(*,g)merge('^','_',[(btest(i,j),j=bit_size(i)-1,0,-1)])
    write(*,*)
    write(*,g)'and for ',-i,' with a bit size of ',bit_size(i)
    write(*,'(b32.32)')-i
    write(*,g)merge('^','_',[(btest(-i,j),j=bit_size(i)-1,0,-1)])

    ! elemental:
    !
    a(1,:)=[ 1, 2 ]
    a(2,:)=[ 3, 4 ]
    write(*,*)
    write(*,'(a,/,*(i2,1x,i2,/))')'given the array a ...',a
    ! the second bit of all the values in a
    write(*,'(a,/,*(l2,1x,l2,/))')'the value of btest (a, 2)',btest(a,2)
    ! bits 1,2,3,4 of the value 2
    write(*,'(a,/,*(l2,1x,l2,/))')'the value of btest (2, a)',btest(2,a)
end program demo_btest
```
Results:
```text
Looking at the integer: 33856=>11111111111111110111101111000000

00000000000000001000010001000000
11111111111111110111101111000000
1000010001000000
11111111111111110111101111000000
from bit 0 to bit 32==>
FFFFFFTFFFTFFFFTFFFFFFFFFFFFFFFF

so for 33856 with a bit size of 32
00000000000000001000010001000000
________________^____^___^______

and for -33856 with a bit size of 32
11111111111111110111101111000000
^^^^^^^^^^^^^^^^_^^^^_^^^^______

given the array a ...
 1  3
 2  4

the value of btest (a, 2)
 F  F
 F  T

the value of btest (2, a)
 T  F
 F  F
```
## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR),
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# C\_ASSOCIATED

## __Name__

__c\_associated__(3) - \[ISO\_C\_BINDING\] Status of a C pointer

## __Syntax__
```fortran
result = c_associated(c_prt_1, c_ptr_2)
```
## __Description__

__c\_associated(c\_prt\_1\[, c\_ptr\_2\])__ determines the status of the
C pointer c\_ptr\_1 or if c\_ptr\_1 is associated with the target
c\_ptr\_2.

## __Arguments__

  - __c\_ptr\_1__
    : Scalar of the type c\_ptr or c\_funptr.

  - __c\_ptr\_2__
    : (Optional) Scalar of the same type as c\_ptr\_1.

## __Returns__

The return value is of type _logical_; it is .false. if either c\_ptr\_1
is a C NULL pointer or if c\_ptr1 and c\_ptr\_2 point to different
addresses.

## __Examples__

Sample program:

```fortran
program demo_c_associated

contains

subroutine association_test(a,b)
use iso_c_binding, only: c_associated, c_loc, c_ptr
implicit none
real, pointer :: a
type(c_ptr) :: b
   if(c_associated(b, c_loc(a))) &
      stop 'b and a do not point to same target'
end subroutine association_test

end program demo_c_associated
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_loc__(3)](C_LOC),
[__c\_funloc__(3)](C_FUNLOC),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
# CEILING

## __Name__

__ceiling__(3) - \[NUMERIC\] Integer ceiling function

## __Syntax__
```fortran
result = ceiling(a, kind)

   integer(kind=KIND) elemental function ceiling(a,kind)
   real(kind=ANY),intent(in)   :: a
   integer,intent(in),optional :: kind
```
## __Description__

__ceiling(a)__ returns the least integer greater than or equal to __a__.

## __Arguments__

  - __a__
    : The type shall be _real_.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type __integer__(kind) if __kind__ is present and a
default-kind _integer_ otherwise.

The result is undefined if it cannot be represented in the specified
integer type.

## __Examples__

Sample program:

```fortran
program demo_ceiling
implicit none
real :: x = 63.29
real :: y = -63.59
   print *, ceiling(x)
   print *, ceiling(y)
   ! elemental
   print *,ceiling([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])
end program demo_ceiling
```
  Results:
```text
   64
  -63
   -2      -2      -2      -2      -1      -1
    0       0       1       1       2       2
    3       3       3
```
## __Standard__

Fortran 95 and later

## __See Also__

[__floor__(3)](FLOOR),
[__nint__(3)](NINT)

[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__int__(3)](INT),
[__selected_int_kind__(3)](SELECTED_INT_KIND)

###### fortran-lang intrinsic descriptions
# C\_F\_POINTER

## __Name__

__c\_f\_pointer__(3) - \[ISO\_C\_BINDING\] Convert C into Fortran pointer

## __Syntax__
```fortran
call c_f_pointer(cptr, fptr, shape)
```
## __Description__

__c\_f\_pointer(cptr, fptr\[, shape\])__ Assign the target, the C
pointer, __cptr__ to the Fortran pointer __fptr__ and specify its shape.

## __Arguments__

  - __cptr__
    : scalar of the type c\_ptr. It is __intent(in)__.

  - __fptr__
    : pointer interoperable with __cptr__. it is __intent(out)__.

  - __shape__
    : (Optional) Rank-one array of type _integer_ with __intent(in)__ .
    It shall be present if and only if __fptr__ is an array. The size
    must be equal to the rank of __fptr__.

## __Examples__

Sample program:

```fortran
program demo_c_f_pointer
use iso_c_binding
implicit none
interface
   subroutine my_routine(p) bind(c,name='myC_func')
      import :: c_ptr
      type(c_ptr), intent(out) :: p
   end subroutine
end interface
type(c_ptr) :: cptr
real,pointer :: a(:)
   call my_routine(cptr)
   call c_f_pointer(cptr, a, [12])
end program demo_c_f_pointer
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_loc__(3)](C_LOC),
[__c\_f\_procpointer__(3)](C_F_PROCPOINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
# C\_F\_PROCPOINTER

## __Name__

__c\_f\_procpointer__(3) - \[ISO\_C\_BINDING\] Convert C into Fortran procedure pointer

## __Syntax__
```fortran
call c_f_procpointer(cptr, fptr)
```
## __Description__

__c\_f\_procpointer(cptr, fptr)__ assigns the target of the C function
pointer __cptr__ to the Fortran procedure pointer __fptr__.

## __Arguments__

  - __cptr__
    : scalar of the type c\_funptr. It is __intent(in)__.

  - __fptr__
    : procedure pointer interoperable with __cptr__. It is __intent(out)__.

## __Examples__

Sample program:

```fortran
program demo_c_f_procpointer
use iso_c_binding
implicit none
abstract interface
   function func(a)
   import :: c_float
   real(c_float), intent(in) :: a
   real(c_float) :: func
   end function
end interface
interface
   function getIterFunc() bind(c,name="getIterFunc")
   import :: c_funptr
   type(c_funptr) :: getIterFunc
   end function
end interface
type(c_funptr) :: cfunptr
procedure(func), pointer :: myFunc
   cfunptr = getIterFunc()
   call c_f_procpointer(cfunptr, myFunc)
end program demo_c_f_procpointer
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_loc__(3)](C_LOC),
[__c\_f\_pointer__(3)](C_F_POINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
# C\_FUNLOC

## __Name__

__c\_funloc__(3) - \[ISO\_C\_BINDING\] Obtain the C address of a procedure

## __Syntax__
```fortran
result = c_funloc(x)
```
## __Description__

__c\_funloc(x)__ determines the C address of the argument.

## __Arguments__

  - __x__
    : Interoperable function or pointer to such function.

## __Returns__

The return value is of type c\_funptr and contains the C address of the
argument.

## __Examples__

Sample program:

```fortran
! program demo_c_funloc and module
module x
use iso_c_binding
implicit none
contains
subroutine sub(a) bind(c)
real(c_float) :: a
   a = sqrt(a)+5.0
end subroutine sub
end module x
!
program demo_c_funloc
use iso_c_binding
use x
implicit none
interface
   subroutine my_routine(p) bind(c,name='myC_func')
     import :: c_funptr
     type(c_funptr), intent(in) :: p
   end subroutine
end interface
   call my_routine(c_funloc(sub))
!
end program demo_c_funloc
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_associated__(3)](C_ASSOCIATED),
[__c\_loc__(3)](C_LOC),
[__c\_f\_pointer__(3)](C_F_POINTER),

[__c\_f\_procpointer__(3)](C_F_PROCPOINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
# CHAR

## __Name__

__char__(3) - \[CHARACTER\] Character conversion function

## __Syntax__
```fortran
result = char(i, kind)
   elemental integer function char(i,kind)

    integer(kind=KIND),intent(in) :: c
    integer,intent(in),optional :: KIND
```
## __Description__

__char(i, kind)__ returns the character represented by the integer __i__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _character_

## __Examples__

Sample program:

```fortran
program demo_char
implicit none
integer :: i = 74
character(1) :: c
    c = char(i)
    print *, i, c ! returns 'J'
end program demo_char
```
  Results:
```text
             74 J
```

## __Note__

See [__ichar__(3)](CHAR) for a discussion of converting between numerical
values and formatted string representations.

## __Standard__

FORTRAN 77 and later

## __See Also__

[__achar__(3)](ACHAR),
[__iachar__(3)](IACHAR),
[__ichar__(3)](ICHAR)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX),
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# C\_LOC

## __Name__

__c\_loc__(3) - \[ISO\_C\_BINDING\] Obtain the C address of an object

## __Syntax__
```fortran
result = c_loc(x)
```
## __Description__

__c\_loc(x)__ determines the C address of the argument.

## __Arguments__

  - __x__
    : Shall have either the _pointer_ or _target_ attribute. It shall not be a
    coindexed object. It shall either be a variable with interoperable
    type and kind type parameters, or be a scalar, nonpolymorphic
    variable with no length type parameters.

## __Returns__

The return value is of type c\_ptr and contains the C address of the
argument.

## __Examples__

Sample program:

```fortran
   subroutine association_test(a,b)
   use iso_c_binding, only: c_associated, c_loc, c_ptr
   implicit none
   real, pointer :: a
   type(c_ptr) :: b
     if(c_associated(b, c_loc(a))) &
        stop 'b and a do not point to same target'
   end subroutine association_test
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_associated__(3)](C_ASSOCIATED),
[__c\_funloc__(3)](C_FUNLOC),
[__c\_f\_pointer__(3)](C_F_POINTER),

[__c\_f\_procpointer__(3)](C_F_PROCPOINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
# CMPLX

## __Name__

__cmplx__(3) - \[TYPE:NUMERIC\] Complex conversion function

## __Syntax__
```fortran
result = cmplx(x, y, kind)

   complex elemental function :: cmplx
   TYPE(kind=KIND),intent(in), x
   TYPE(kind=KIND),intent(in),optional, y
   integer,intent(in),optional :: kind
```
## __Description__

To convert numeric variables to complex, use the __cmplx__(3) function.
Constants can be used to define a complex variable using the syntax

```
      z8 = (1.2345678901234567d0, 1.2345678901234567d0)
```

but this will not work for variables. You must use the __cmplx__(3) function.

__cmplx(x \[, y \[, kind\]\])__ returns a complex number where __x__ is
converted to the _real_ component. If __x__ is _complex_ then __y__ must not be
present. If __y__ is present it is converted to the imaginary component. If
__y__ is not present then the imaginary component is set to __0.0__.

## __cmplx(3) and double precision__

The Fortran 90 language defines __cmplx__(3) as always returning a result
that is type __complex(kind=KIND(0.0))__.

This means \`__cmplx(d1,d2)__', where __\`d1'__ and __\`d2'__ are
_doubleprecision_, is treated as:
fortran
```
      cmplx(sngl(d1), sngl(d2))
```

_doubleprecision complex_ numbers require specifying a precision.

It was necessary for Fortran 90 to specify this behavior for
_doubleprecision_ arguments, since that is the behavior mandated by
FORTRAN 77.

So Fortran 90 extends the __cmplx__(3) intrinsic by adding an extra
argument used to specify the desired kind of complex result.

```fortran
      integer,parameter :: dp=kind(0.0d0)
      complex(kind=dp) :: z8
      !
      ! NO: result is just the precision of default _real_ values
      !     because KIND parameter is not specified
      !
      ! note this was stored with default real precision
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0)
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      z8 = cmplx(1.2345678901234567e0_dp, 1.2345678901234567e0_dp)
      ! again, note components are just _real_
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      !
      ! YES
      !
      ! kind= makes it work
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
      print *, 'YES, Z8=',z8,real(z8),aimag(z8)
```

F2018 COMPONENT SYNTAX The real and imaginary parts of a complex entity
can be accessed independently with a component-like syntax in f2018:

A complex-part-designator is

``fortran
      designator % RE
      or
      designator % IM.
```

Where the designator is of complex type.

So designator%RE designates the real part of a complex value,
designator%IM designates the imaginary part of complex value. The type
of a complex-part-designator is _real_, and its kind and shape are those
of the designator.

The following are examples of complex part designators:

```fortran
       impedance%re           !-- Same value as _real_(impedance)
       fft%im                 !-- Same value as AIMAG(fft)
       x%im = 0.0             !-- Sets the imaginary part of x to zero
```

## __Arguments__

  - __x__
    The type may be _integer_, _real_, or _complex_.

  - __y__
    (Optional; only allowed if __x__ is not _complex_.). May be _integer_ or
    _real_.

  - __kind__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of _complex_ type, with a kind equal to __kind__ if it is
specified. If __kind__ is not specified, the result is of the default
_complex_ kind, regardless of the kinds of __x__ and __y__.

## __Examples__

Sample program:

```fortran
program demo_aimag
implicit none
integer,parameter :: dp=kind(0.0d0)
complex          :: z4
complex(kind=dp) :: z8
   z4 = cmplx(1.23456789, 1.23456789)
   print *, 'Z4=',z4
   ! using kind=dp makes it keep DOUBLEPRECISION precision
   z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
   print *, 'Z8=',z8
   ! NOTE:
   ! The following is intuitive and works without calling cmplx(3)
   ! but does not work for variables just constants
   z8 = (1.2345678901234567d0 , 1.2345678901234567d0 )
   print *, 'Z8 defined with constants=',z8
end program demo_aimag
```

Typical Results:

```
    Z4= (1.23456788,1.23456788)
    Z8= (1.2345678901234567,1.2345678901234567)
    Z8 defined with constants= (1.2345678901234567,1.2345678901234567)
```

## __See Also__

  - [__aimag__(3)](AIMAG) - Imaginary part of complex number

  - [__cmplx__(3)](CMPLX) - Complex conversion function

  - [__conjg__(3)](CONJG) - Complex conjugate function

  - [__real__(3)](REAL) - Convert to real type

## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# CO\_BROADCAST

## __Name__

__co\_broadcast__(3) - \[COLLECTIVE\] Copy a value to all images the current set of images

## __Syntax__
```fortran
call co_broadcast(a, source_image, stat, errmsg)
```
## __Description__

__co\_broadcast(3)__ copies the value of argument __a__ on the image with image
index source\_image to all images in the current team. __a__ becomes defined
as if by intrinsic assignment. If the execution was successful and __stat__
is present, it is assigned the value zero. If the execution failed, __stat__
gets assigned a nonzero value and, if present, __errmsg__ gets assigned a
value describing the occurred error.

## __Arguments__

  - __a__
    : __intent(inout)__ argument; shall have the same dynamic type and
    type parameters on all images of the current team. If it is an
    array, it shall have the same shape on all images.

  - __source\_image__
    : a scalar integer expression. It shall have the same the same value
    on all images and refer to an image of the current team.

  - __stat__
    : (optional) a scalar integer variable

  - __errmsg__
    : (optional) a scalar character variable

## __Examples__

Sample program:

```fortran
program demo_co_broadcast
implicit none
integer :: val(3)
   if (this_image() == 1) then
      val = [1, 5, 3]
   endif
   call co_broadcast (val, source_image=1)
   print *, this_image(), ":", val
end program demo_co_broadcast
```

## __See Also__

[__co\_max__(3)](CO_MAX),
[__co\_min__(3)](CO_MIN),
[__co\_sum__(3)](CO_SUM),
[__co\_reduce__(3)](CO_REDUCE)

###### fortran-lang intrinsic descriptions
# CO\_LBOUND

## __Name__

__co\_lbound__(3) - \[COLLECTIVE\] Lower codimension bounds of an array

## __Syntax__
```fortran
result = co_lbound(coarray, dim, kind)
```
## __Description__

Returns the lower bounds of a coarray, or a single lower cobound along
the __dim__ codimension.

## __Arguments__

  - __array__
    : Shall be an coarray, of any type.

  - __dim__
    : (Optional) Shall be a scalar _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind. If __dim__ is absent, the
result is an array of the lower cobounds of __coarray__. If __dim__ is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

## __Standard__

Fortran 2008 and later

## __See Also__

[__co\_ubound__(3)](CO_UBOUND),
[__lbound__(3)](LBOUND)

###### fortran-lang intrinsic descriptions
# CO\_MAX

## __Name__

__co\_max__(3) - \[COLLECTIVE\] Maximal value on the current set of images

## __Syntax__
```fortran
call co_max(a, result_image, stat, errmsg)
```
## __Description__

co\_max determines element-wise the maximal value of __a__ on all images of
the current team. If result\_image is present, the maximum values are
returned in __a__ on the specified image only and the value of __a__ on the
other images become undefined. If result\_image is not present, the
value is returned on all images. If the execution was successful and
__stat__ is present, it is assigned the value zero. If the execution failed,
__stat__ gets assigned a nonzero value and, if present, __errmsg__ gets assigned
a value describing the occurred error.

## __Arguments__

  - __a__
    : shall be an integer, real or character variable, which has the same
    type and type parameters on all images of the team.

  - __result\_image__
    : (optional) a scalar integer expression; if present, it shall have
    the same the same value on all images and refer to an image of the
    current team.

  - __stat__
    : (optional) a scalar integer variable

  - __errmsg__
    : (optional) a scalar character variable

## __Examples__

Sample program:

```fortran
program demo_co_max
implicit none
integer :: val
   val = this_image()
   call co_max(val, result_image=1)
   if (this_image() == 1) then
     write(*,*) "Maximal value", val  ! prints num_images()
   endif
end program demo_co_max
```
  Results:
```text
    Maximal value           2
```

## __Standard__

TS 18508 or later

## __See Also__

[__co\_min__(3)](CO_MIN),
[__co\_sum__(3)](CO_SUM),
[__co\_reduce__(3)](CO_REDUCE),
[__co\_broadcast__(3)](CO_BROADCAST)

###### fortran-lang intrinsic descriptions
# CO\_MIN

## __Name__

__co\_min__(3) - \[COLLECTIVE\] Minimal value on the current set of images

## __Syntax__
```fortran
call co_min(a, result_image, stat, errmsg)
```
## __Description__

co\_min determines element-wise the minimal value of __a__ on all images of
the current team. If result\_image is present, the minimal values are
returned in __a__ on the specified image only and the value of __a__ on the
other images become undefined. If result\_image is not present, the
value is returned on all images. If the execution was successful and
__stat__ is present, it is assigned the value zero. If the execution failed,
__stat__ gets assigned a nonzero value and, if present, __errmsg__ gets assigned
a value describing the occurred error.

## __Arguments__

  - __a__
    : shall be an integer, real or character variable, which has the same
    type and type parameters on all images of the team.

  - __result\_image__
    : (optional) a scalar integer expression; if present, it shall have
    the same the same value on all images and refer to an image of the
    current team.

  - __stat__
    : (optional) a scalar integer variable

  - __errmsg__
    : (optional) a scalar character variable

## __Examples__

Sample program:

```fortran
program demo_co_min
implicit none
integer :: val
   val = this_image()
   call co_min(val, result_image=1)
   if (this_image() == 1) then
     write(*,*) "Minimal value", val  ! prints 1
   endif
end program demo_co_min
```

## __Standard__

TS 18508 or later

## __See Also__

[__co\_max__(3)](CO_MAX),
[__co\_sum__(3)](CO_SUM),
[__co\_reduce__(3)](CO_REDUCE),
[__co\_broadcast__(3)](CO_BROADCAST)

###### fortran-lang intrinsic descriptions
# COMMAND\_ARGUMENT\_COUNT

## __Name__

__command\_argument\_count__(3) - \[SYSTEM:COMMAND LINE\] Get number of command line arguments

## __Syntax__
```fortran
    result = command_argument_count()

     integer function command_argument_count() result(count)
     integer :: count
```
## __Description__

   __command\_argument\_count()__ returns the number of arguments passed
   on the command line when the containing program was invoked.

## __Arguments__

None

## __Returns__

  - __count__
    : The return value is of type default _integer_. It is the number of
    arguments passed on the command line when the program was invoked.

## __Examples__

Sample program:

```fortran
program demo_command_argument_count
implicit none
integer :: count
   count = command_argument_count()
   print *, count
end program demo_command_argument_count
```

Sample output:

```bash
   # the command verb does not count
   ./test_command_argument_count
       0
   # quoted strings may count as one argument
   ./test_command_argument_count count arguments
       2
   ./test_command_argument_count 'count arguments'
       1
```
## __Standard__

Fortran 2003 and later

## __See Also__

[__get\_command__(3)](GET_COMMAND),
[__get\_command\_argument__(3)](GET_COMMAND_ARGUMENT)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# COMPILER\_OPTIONS

## __Name__

__compiler\_options__(3) - \[COMPILER INQUIRY\] Options passed to the compiler

## __Syntax__
```fortran
str = compiler_options()
```
## __Description__

compiler\_options returns a string with the options used for compiling.

## __Arguments__

None.

## __Returns__

The return value is a default-kind string with system-dependent length.
It contains the compiler flags used to compile the file, which called
the compiler\_options intrinsic.

## __Examples__

Sample program:

```fortran
program demo_compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
implicit none
   print '(4a)', &
      'This file was compiled by ', &
      compiler_version(),           &
      ' using the options ',        &
      compiler_options()
end program demo_compiler_version
```
Results:
```
   This file was compiled by GCC version 5.4.0 using the options
   -I /usr/include/w32api -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
   -mtune=generic -march=x86-64 -g -Wunused -Wuninitialized -Wall
   -std=f2008 -fbounds-check -fbacktrace -finit-real=nan
   -fno-range-check -frecord-marker=4
   -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
```
## __Standard__

Fortran 2008

## __See Also__

[__compiler\_version__(3)](COMPILER_VERSION),
__iso\_fortran\_env__(7)

###### fortran-lang intrinsic descriptions
# COMPILER\_VERSION

## __Name__

__compiler\_version__(3) - \[COMPILER INQUIRY\] Compiler version string

## __Syntax__

```fortran
str = compiler_version()
```

## __Description__

__compiler\_version__(3) returns a string containing the name and
version of the compiler.

## __Arguments__

None.

## __Returns__

The return value is a default-kind string with system-dependent length.
It contains the name of the compiler and its version number.

## __Examples__

Sample program:

```fortran
program demo_compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
implicit none
   print '(4a)', &
      'This file was compiled by ', &
      compiler_version(),           &
      ' using the options ',        &
      compiler_options()
end program demo_compiler_version
```
Results:
```
   This file was compiled by GCC version 5.4.0 using the options
   -I /usr/include/w32api -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
   -mtune=generic -march=x86-64 -g -Wunused -Wuninitialized -Wall
   -std=f2008 -fbounds-check -fbacktrace -finit-real=nan
   -fno-range-check -frecord-marker=4
   -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
```

## __Standard__

Fortran 2008

## __See Also__

[__compiler\_options__(3)](COMPILER_OPTIONS),
__iso\_fortran\_env__(7)

###### fortran-lang intrinsic descriptions
# CONJG

## __Name__

__conjg__(3) - \[NUMERIC\] Complex conjugate of a complex value

## __Syntax__
```fortran
z = conjg(z)

   complex(kind=K) elemental function conjg(z)
   complex(kind=K),intent(in) :: z
```
where __K__ is the kind of the parameter __z__

## __Description__

__conjg(z)__ returns the complex conjugate of the _complex_ value __z__.

In mathematics, the complex conjugate of a complex_ number is the number
with an equal real part and an imaginary part equal in magnitude but
opposite in sign.

That is, If __z__ is __(x, y)__ then the result is __(x, -y)__.

For matrices of complex numbers, __conjg(array)__ represents the
element-by-element conjugation of __array__; not the conjugate transpose
of __array__ .

## __Arguments__

  - __z__
    : The type shall be _complex_.

## __Returns__

The return value is of type _complex_.

## __Examples__

Sample program:

```fortran
program demo_conjg
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
complex :: z = (2.0, 3.0)
complex(kind=real64) :: dz = (   &
   &  1.2345678901234567_real64, &
   & -1.2345678901234567_real64)
complex :: arr(3,3)
integer :: i

    print *, z
    z= conjg(z)
    print *, z
    print *

    print *, dz
    dz = conjg(dz)
    print *, dz
    print *

    ! the function is elemental so it can take arrays
    arr(1,:)=[(-1.0, 2.0),( 3.0, 4.0),( 5.0,-6.0)]
    arr(2,:)=[( 7.0,-8.0),( 8.0, 9.0),( 9.0, 9.0)]
    arr(3,:)=[( 1.0, 9.0),( 2.0, 0.0),(-3.0,-7.0)]

    write(*,*)'original'
    write(*,'(3("(",g8.2,",",g8.2,")",1x))')(arr(i,:),i=1,3)
    arr = conjg(arr)
    write(*,*)'conjugate'
    write(*,'(3("(",g8.2,",",g8.2,")",1x))')(arr(i,:),i=1,3)

end program demo_conjg
```
  Results:
```fortran
 (2.000000,3.000000)
 (2.000000,-3.000000)

 (1.23456789012346,-1.23456789012346)
 (1.23456789012346,1.23456789012346)

 original
(-1.0    , 2.0    ) ( 3.0    , 4.0    ) ( 5.0    ,-6.0    )
( 7.0    ,-8.0    ) ( 8.0    , 9.0    ) ( 9.0    , 9.0    )
( 1.0    , 9.0    ) ( 2.0    , 0.0    ) (-3.0    ,-7.0    )

 conjugate
(-1.0    ,-2.0    ) ( 3.0    ,-4.0    ) ( 5.0    , 6.0    )
( 7.0    , 8.0    ) ( 8.0    ,-9.0    ) ( 9.0    ,-9.0    )
( 1.0    ,-9.0    ) ( 2.0    , 0.0    ) (-3.0    , 7.0    )
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# CO\_REDUCE

## __Name__

__co\_reduce__(3) - \[COLLECTIVE\] Reduction of values on the current set of images

## __Syntax__
```fortran
call co_reduce(a, operation, result_image, stat, errmsg)
```
## __Description__

co\_reduce determines element-wise the reduction of the value of __a__ on
all images of the current team. The pure function passed as __operation__ is
used to pairwise reduce the values of __a__ by passing either the value of __a__
of different images or the result values of such a reduction as
argument. If __a__ is an array, the reduction is done element wise. If
result\_image is present, the result values are returned in __a__ on the
specified image only and the value of __a__ on the other images become
undefined. If result\_image is not present, the value is returned on all
images. If the execution was successful and __stat__ is present, it is
assigned the value zero. If the execution failed, __stat__ gets assigned a
nonzero value and, if present, __errmsg__ gets assigned a value describing
the occurred error.

## __Arguments__

  - __a__
    : is an __intent(inout)__ argument and shall be nonpolymorphic. If it
    is allocatable, it shall be allocated; if it is a pointer, it shall
    be associated. __a__ shall have the same type and type parameters on all
    images of the team; if it is an array, it shall have the same shape
    on all images.

  - __operation__
    : pure function with two scalar nonallocatable arguments, which shall
    be nonpolymorphic and have the same type and type parameters as __a__.
    The function shall return a nonallocatable scalar of the same type
    and type parameters as __a__. The function shall be the same on all
    images and with regards to the arguments mathematically commutative
    and associative. Note that OPERATION may not be an elemental

      - __function, unless it is an intrinsic function.__
        result\_image

      - (optional) a scalar integer expression; if present, it shall
        have the same the same value on all images and refer to an image
        of the current team.

  - __stat__
    : (optional) a scalar integer variable

  - __errmsg__
    : (optional) a scalar character variable

## __Examples__

Sample program:

```fortran
program demo_co_reduce
implicit none
integer :: val

   val = this_image()
   call co_reduce(val, myprod, 1)
   if (this_image() == 1) then
      write(*,*) "Product value", val  ! prints num_images() factorial
   endif

contains

pure function myprod(a, b)
   integer, value :: a, b
   integer :: myprod
   myprod = a * b
end function myprod

end program demo_co_reduce
```

## __Note__

While the rules permit in principle an intrinsic function, none of the
intrinsics in the standard fulfill the criteria of having a specific
function, which takes two arguments of the same type and returning that
type as a result.

## __Standard__

TS 18508 or later

## __See Also__

[__co\_min__(3)](CO_MIN),
[__co\_max__(3)](CO_MAX),
[__co\_sum__(3)](CO_SUM),
[__co\_broadcast__(3)](CO_BROADCAST)

###### fortran-lang intrinsic descriptions
# COSH

## __Name__

__cosh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic cosine function

## __Syntax__
```fortran
    result = cosh(x)

     TYPE(kind=KIND) elemental function cosh(x)
     TYPE(kind=KIND),intent(in) :: x
```
where TYPE may be _real_ or _complex_ and KIND may be any
supported kind for the associated type. The returned __value__
will be the same type and kind as the input value __x__.

## __Description__

__cosh(x)__ computes the hyperbolic cosine of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__. If __x__ is _complex_, the
imaginary part of the result is in radians.

If __x__ is _real_, the return value has a lower bound of one,
__cosh(x) \>= 1__.

## __Examples__

Sample program:

```fortran
program demo_cosh
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
    x = cosh(x)
end program demo_cosh
```

## __Standard__

FORTRAN 77 and later, for a complex argument - Fortran 2008 or later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [__acosh__(3)](ACOSH)

###### fortran-lang intrinsic descriptions
# COS

## __Name__

__cos__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Cosine function

## __Syntax__
```fortran
result = cos(x)

   TYPE(kind=KIND),elemental :: cos
   TYPE(kind=KIND,intent(in) :: x
```
where TYPE may be _real_ or _complex_ and KIND may be any KIND supported
by the associated type.

## __Description__

__cos(x)__ computes the cosine of an angle __x__ given the size of the
angle in radians.

The cosine of a _real_ value is the ratio of the adjacent side to the
hypotenuse of a right-angled triangle.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.
    __x__ is assumed to be in radians.

## __Returns__

The return value is of the same type and kind as __x__.

If __x__ is of the type _real_, the return value lies in
the range __-1 \<= cos(x) \<= 1__ .

## __Examples__

Sample program:

```fortran
program demo_cos
implicit none
doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0
   write(*,*)'COS(0.0)=',cos(0.0)
   write(*,*)'COS(PI)=',cos(PI)
   write(*,*)'COS(PI/2.0d0)=',cos(PI/2.0d0),' EPSILON=',epsilon(PI)
   write(*,*)'COS(2*PI)=',cos(2*PI)
   write(*,*)'COS(-2*PI)=',cos(-2*PI)
   write(*,*)'COS(-2000*PI)=',cos(-2000*PI)
   write(*,*)'COS(3000*PI)=',cos(3000*PI)
end program demo_cos
```
Results:
```
   COS(0.0)=        1.00000000
   COS(PI)=        -1.0000000000000000
   COS(PI/2.0d0)=   6.1232339957367660E-017
   EPSILON=         2.2204460492503131E-016
   COS(2*PI)=       1.0000000000000000
   COS(-2*PI)=      1.0000000000000000
   COS(-2000*PI)=   1.0000000000000000
```
## __Standard__

FORTRAN 77 and later

## __See Also__
- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

[__acos__(3)](ACOS),
[__sin__(3)](SIN),
[__tan__(3)](TAN)

###### fortran-lang intrinsic descriptions
# CO\_SUM

## __Name__

__co\_sum__(3) - \[COLLECTIVE\] Sum of values on the current set of images

## __Syntax__
```fortran
call co_sum(a, result_image, stat, errmsg)
```
## __Description__

co\_sum sums up the values of each element of __a__ on all images of the
current team. If result\_image is present, the summed-up values are
returned in __a__ on the specified image only and the value of __a__ on the
other images become undefined. If result\_image is not present, the
value is returned on all images. If the execution was successful and
__stat__ is present, it is assigned the value zero. If the execution failed,
__stat__ gets assigned a nonzero value and, if present, __errmsg__ gets assigned
a value describing the occurred error.

## __Arguments__

  - __a__
    : shall be an integer, real or complex variable, which has the same
    type and type parameters on all images of the team.

  - __result\_image__
    : (optional) a scalar integer expression; if present, it shall have
    the same the same value on all images and refer to an image of the
    current team.

  - __stat__
    : (optional) a scalar integer variable

  - __errmsg__
    : (optional) a scalar character variable

## __Examples__

Sample program:

```fortran
program demo_co_sum
implicit none
integer :: val
   val = this_image()
   call co_sum(val, result_image=1)
   if (this_image() == 1) then
      ! prints (n**2 + n)/2, with n = num_images()
      write(*,*) "The sum is ", val
   endif
end program demo_co_sum
```
  Results:
```text
    The sum is            1
```

## __Standard__

TS 18508 or later

## __See Also__

[__co\_max__(3)](CO_MAX),
[__co\_min__(3)](CO_MIN),
[__co\_reduce__(3)](CO_REDUCE),
[__co\_broadcast__(3)](CO_BROADCAST)

###### fortran-lang intrinsic descriptions
# CO\_UBOUND

## __Name__

__co\_ubound__(3) - \[COLLECTIVE\] Upper codimension bounds of an array

## __Syntax__
```fortran
result = co_ubound(coarray, dim, kind)
```
## __Description__

Returns the upper cobounds of a coarray, or a single upper cobound along
the __dim__ codimension.

## __Arguments__

  - __array__
    : Shall be an coarray, of any type.

  - __dim__
    : (Optional) Shall be a scalar _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind. If __dim__ is absent, the
result is an array of the lower cobounds of __coarray__. If __dim__ is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

## __Standard__

Fortran 2008 and later

## __See Also__

[__co\_lbound__(3)](CO_LBOUND),
[__lbound__(3)](LBOUND),
[__ubound__(3)](UBOUND)

###### fortran-lang intrinsic descriptions
# COUNT

## __Name__

__count__(3) - \[ARRAY REDUCTION\] Count function

## __Syntax__
```fortran
result = count(mask, dim, kind)
```
## __Description__

Counts the number of __.true.__ elements in a logical __mask__, or, if the __dim__
argument is supplied, counts the number of elements along each row of
the array in the __dim__ direction. If the array has zero size, or all of
the elements of __mask__ are false, then the result is __0__.

## __Arguments__

  - __mask__
    : The type shall be _logical_.

  - __dim__
    : (Optional) The type shall be _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind. If __dim__ is present, the
result is an array with a rank one less than the rank of __array__, and a
size corresponding to the shape of __array__ with the __dim__ dimension removed.

## __Examples__

Sample program:

```fortran
program demo_count
implicit none
integer, dimension(2,3) :: a, b
logical, dimension(2,3) :: mymask
      a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])
      b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])
      print '(3i3)', a(1,:)
      print '(3i3)', a(2,:)
      print *
      print '(3i3)', b(1,:)
      print '(3i3)', b(2,:)
      print *
      mymask = a.ne.b
      print '(3l3)', mymask(1,:)
      print '(3l3)', mymask(2,:)
      print *
      print '(3i3)', count(mymask)
      print *
      print '(3i3)', count(mymask, 1)
      print *
      print '(3i3)', count(mymask, 2)
end program demo_count
```
   Expected Results:
```text
  1  3  5
  2  4  6

  0  3  5
  7  4  8

  T  F  F
  T  F  T

  3

  2  0  1

  1  2
```
## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003
and later

###### fortran-lang intrinsic descriptions
# CPU\_TIME

## __Name__

__cpu\_time__(3) - \[SYSTEM:TIME\] return CPU processor time in seconds

## __Syntax__
```fortran
     call cpu_time(time)
     real,intent(out) :: time
```
## __Description__

Returns a _real_ value representing the elapsed CPU time in seconds. This
is useful for testing segments of code to determine execution time.

The exact definition of time is left imprecise because of the
variability in what different processors are able to provide.

If no time source is available, TIME is set to a negative value.

Note that TIME may contain a system dependent, arbitrary offset and may
not start with 0.0. For cpu\_time the absolute value is meaningless.
Only differences between subsequent calls, as shown in the example
below, should be used.

A processor for which a single result is inadequate (for example, a
parallel processor) might choose to provide an additional version for
which time is an array.

## __Returns__

  - __TIME__
    : The type shall be _real_ with __intent(out)__. It is assigned a
    processor-dependent approximation to the processor time in seconds.
    If the processor cannot return a meaningful time, a
    processor-dependent negative value

      - __is returned.__
        The start time is left imprecise because the purpose is to time
        sections of code, as in the example. This might or might not
        include system overhead time.

## __Examples__

Sample program:

```fortran
program demo_cpu_time
implicit none
real :: start, finish
   !
   call cpu_time(start)
   ! put code to test here
   call cpu_time(finish)
   !
   ! writes processor time taken by the piece of code.
   print '("Processor Time = ",f6.3," seconds.")',finish-start
end program demo_cpu_time
```
  Results:
```text
   Processor Time =  0.000 seconds.
```
## __Standard__

Fortran 95 and later

## __See Also__

[__system\_clock__(3)](SYSTEM_CLOCK),
[__date\_and\_time__(3)](DATE_AND_TIME)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# CSHIFT

## __Name__

__cshift__(3) - \[TRANSFORMATIONAL\] Circular shift elements of an array

## __Syntax__
```fortran
result = cshift(array, shift, dim)
```
## __Description__

__cshift(array, shift \[, dim\])__ performs a circular shift on elements
of __array__ along the dimension of __dim__. If __dim__ is omitted it is taken to be
__1__. __dim__ is a scalar of type _integer_ in the range of __1 \<= dim \<= n__,
where "n" is the rank of __array__. If the rank of __array__ is one, then all
elements of __array__ are shifted by __shift__ places. If rank is greater than
one, then all complete rank one sections of __array__ along the given
dimension are shifted. Elements shifted out one end of each rank one
section are shifted back in the other end.

## __Arguments__

  - __array__
    : Shall be an array of any type.

  - __shift__
    : The type shall be _integer_.

  - __dim__
    : The type shall be _integer_.

## __Returns__

Returns an array of same type and rank as the __array__ argument.

## __Examples__

Sample program:

```fortran
program demo_cshift
implicit none
integer, dimension(3,3) :: a
    a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
    a = cshift(a, SHIFT=[1, 2, -1], DIM=2)
    print *
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
end program demo_cshift
```
  Results:
```text
     1  4  7
     2  5  8
     3  6  9

     4  7  1
     8  2  5
     9  3  6
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# C\_SIZEOF

## __Name__

__c\_sizeof__(3) - \[ISO\_C\_BINDING\] Size in bytes of an expression

## __Syntax__
```fortran
n = c_sizeof(x)
```
## __Description__

__c\_sizeof(x)__ calculates the number of bytes of storage the
expression __x__ occupies.

## __Arguments__

  - __x__
    : The argument shall be an interoperable data entity.

## __Returns__

The return value is of type integer and of the system-dependent kind
c\_size\_t (from the *iso\_c\_binding* module). Its value is the
number of bytes occupied by the argument. If the argument has the
_pointer_ attribute, the number of bytes of the storage area pointed to is
returned. If the argument is of a derived type with _pointer_ or
_allocatable_ components, the return value does not account for the sizes
of the data pointed to by these components.

## __Examples__

Sample program:

```fortran
program demo_c_sizeof
use iso_c_binding
implicit none
real(c_float) :: r, s(5)
   print *, (c_sizeof(s)/c_sizeof(r) == 5)
end program demo_c_sizeof
```
  Results:
```text
    T
```
The example will print .true. unless you are using a platform where
default _real_ variables are unusually padded.

## __Standard__

Fortran 2008

## __See Also__

[__storage\_size__(3)](STORAGE_SIZE)

###### fortran-lang intrinsic descriptions
# DATE\_AND\_TIME

## __Name__

__date\_and\_time__(3) - \[SYSTEM:TIME\] gets current time

## __Syntax__
```fortran
    subroutine date_and_time(date, time, zone, values)

     character(len=8),intent(out),optional :: date
     character(len=10),intent(out),optional :: time
     character(len=5),intent(out),optional :: zone
     integer,intent(out),optional :: values(8)
```
## __Description__

__date\_and\_time(date, time, zone, values)__ gets the corresponding
date and time information from the real-time system clock.

Unavailable time and date _character_ parameters return blanks.

## __Arguments__

  - __date__
    : A character string of default kind of the form CCYYMMDD, of length 8 or larger.

  - __time__
    : A character string of default kind of the form HHMMSS.SSS, of length 10 or larger.

  - __zone__
    : A character string of default kind of the form (+-)HHMM, of length 5 or larger,
    representing the difference with respect to Coordinated Universal Time (UTC).

  - __values__
    : An _integer_ array of eight elements that contains:

      - __values__(1)
      : The year
      - __values__(2)
      : The month
      - __values__(3)
      : The day of the month
      - __values__(4)
      : Time difference with UTC in minutes
      - __values__(5)
      : The hour of the day
      - __values__(6)
      : The minutes of the hour
      - __values__(7)
      : The seconds of the minute
      - __values__(8)
      : The milliseconds of the second

## __Examples__

Sample program:

```fortran
program demo_date_and_time
implicit none
character(len=8)     :: date
character(len=10)    :: time
character(len=5)     :: zone
integer,dimension(8) :: values

    call date_and_time(date,time,zone,values)

    ! using keyword arguments
    call date_and_time(DATE=date,TIME=time,ZONE=zone)
    print '(*(g0))','DATE="',date,'" TIME="',time,'" ZONE="',zone,'"'

    call date_and_time(VALUES=values)
    write(*,'(i5,a)') &
     & values(1),' - The year', &
     & values(2),' - The month', &
     & values(3),' - The day of the month', &
     & values(4),' - Time difference with UTC in minutes', &
     & values(5),' - The hour of the day', &
     & values(6),' - The minutes of the hour', &
     & values(7),' - The seconds of the minute', &
     & values(8),' - The milliseconds of the second'
end program demo_date_and_time
```
Results:
```
   DATE="20201222" TIME="165738.779" ZONE="-0500"
    2020 - The year
      12 - The month
      22 - The day of the month
    -300 - Time difference with UTC in minutes
      16 - The hour of the day
      57 - The minutes of the hour
      38 - The seconds of the minute
     779 - The milliseconds of the second
```
## __Standard__

Fortran 95 and later

## __See Also__

[__cpu\_time__(3)](CPU_TIME),
[__system\_clock__(3)](SYSTEM_CLOCK)

## __Resources__
 date and time conversion, formatting and computation

 - [M_time](https://github.com/urbanjost/M_time)
 - [datetime](https://github.com/wavebitscientific/datetime-fortran)
 - [datetime-fortran](https://github.com/wavebitscientific/datetime-fortran)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# DBLE

## __Name__

__dble__(3) - \[TYPE:NUMERIC\] Double conversion function

## __Syntax__
```fortran
result = dble(a)

    elemental function dble(a)
    type(real(kind=kind(0.0d0)))     :: dble
    type(TYPE(kind=KIND)),intent(in) :: a
```
where TYPE may be _integer_, _real_, or _complex_ and KIND any kind
supported by the TYPE.
## __Description__

__dble(a)__ Converts __a__ to double precision _real_ type.

## __Arguments__

  - __a__
    : The type shall be _integer_, _real_, or _complex_.

## __Returns__

The return value is of type _doubleprecision_. For _complex_ input,
the returned value has the magnitude and sign of the real component
of the input value.

## __Examples__

Sample program:

```fortran
program demo_dble
implicit none
real:: x = 2.18
integer :: i = 5
complex :: z = (2.3,1.14)
   print *, dble(x), dble(i), dble(z)
end program demo_dble
```
  Results:
```text
  2.1800000667572021  5.0000000000000000   2.2999999523162842
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__float__(3)](FLOAT),
[__real__(3)](REAL)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# DIGITS

## __Name__

__digits__(3) - \[NUMERIC MODEL\] Significant digits function

## __Syntax__
```fortran
result = digits(x)
    function digits(x)
    type(integer(kind=kind(0)))      :: digits
    type(TYPE(kind=KIND)),intent(in) :: x(..)
```
where TYPE may be _integer_ or _real_ and KIND is any kind supported by
TYPE.

## __Description__

__digits(x)__ returns the number of significant digits of the internal
model representation of __x__. For example, on a system using a 32-bit
floating point representation, a default real number would likely return
24.

## __Arguments__

  - __x__
    : The type may be a scalar or array of type _integer_ or _real_.

## __Returns__

The return value is of type _integer_ of default kind.

## __Examples__

Sample program:

```fortran
program demo_digits
implicit none
integer :: i = 12345
real :: x = 3.143
doubleprecision :: y = 2.33d0
   print *,'default integer:', digits(i)
   print *,'default real:   ', digits(x)
   print *,'default doubleprecision:', digits(y)
end program demo_digits
```

Typical Results:

```
    default integer:                  31
    default real:                     24
    default doubleprecision:          53
```

## __Standard__

Fortran 95 and later

## __See Also__

[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# DIM

## __Name__

__dim__(3) - \[NUMERIC\] Positive difference

## __Syntax__
```fortran
result = dim(x, y)

    elemental function dim(x, y)
    type(TYPE(kind=KIND))            :: dim
    type(TYPE(kind=KIND)),intent(in) :: x, y
```
where TYPE may be _real_ or _integer_ and KIND is any supported kind for the type.
## __Description__

__dim(x,y)__ returns the difference __x - y__ if the result is positive;
otherwise it returns zero.

## __Arguments__

  - __x__
    : The type shall be _integer_ or _real_

  - __y__
    : The type shall be the same type and kind as __x__.

## __Returns__

The return value is the same type and kind as the input arguments __x__ and __y__.

## __Examples__

Sample program:

```fortran
program demo_dim
use, intrinsic :: iso_fortran_env, only : real64
implicit none
integer           :: i
real(kind=real64) :: x
    i = dim(4, 15)
    x = dim(4.321_real64, 1.111_real64)
    print *, i
    print *, x
    ! elemental
    print *, dim([1,2,3],2)
    print *, dim([1,2,3],[3,2,1])
    print *, dim(-10,[0,-10,-20])
end program demo_dim
```
Results:
```text
              0
      3.21000000000000
              0           0           1
              0           0           2
              0           0          10
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT)
# DOT\_PRODUCT

## __Name__

__dot\_product__(3) - \[TRANSFORMATIONAL\] Dot product function

## __Syntax__
```fortran
result = dot_product(vector_a, vector_b)
```
## __Description__

__dot\_product(vector\_a, vector\_b)__ computes the dot product
multiplication of two vectors vector\_a and vector\_b. The two vectors
may be either numeric or logical and must be arrays of rank one and of
equal size. If the vectors are _integer_ or _real_, the result is
__sum(vector\_a\*vector\_b)__. If the vectors are _complex_, the result is
__sum(conjg(vector\_a)\*vector\_b)__. If the vectors are _logical_, the
result is __any(vector\_a .and. vector\_b)__.

## __Arguments__

  - __vector\_a__
    : The type shall be numeric or _logical_, rank 1.

  - __vector\_b__
    : The type shall be numeric if vector\_a is of numeric type or _logical_
    if vector\_a is of type _logical_. vector\_b shall be a rank-one
    array.

## __Returns__

If the arguments are numeric, the return value is a scalar of numeric
type, _integer_, _real_, or _complex_. If the arguments are _logical_, the
return value is .true. or .false..

## __Examples__

Sample program:

```fortran
program demo_dot_prod
implicit none
    integer, dimension(3) :: a, b
    a = [ 1, 2, 3 ]
    b = [ 4, 5, 6 ]
    print '(3i3)', a
    print *
    print '(3i3)', b
    print *
    print *, dot_product(a,b)
end program demo_dot_prod
```
  Results:
```text
     1  2  3

     4  5  6

             32
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# DPROD

## __Name__

__dprod__(3) - \[NUMERIC\] Double product function

## __Syntax__
```fortran
result = dprod(x, y)
```
## __Description__

__dprod(x,y)__ produces a higher _doubleprecision_ product of default _real_
numbers __x__ and __y__.

The result has a value equal to a processor-dependent approximation to
the product of __x__ and __y__. It is recommended that the processor compute the
product in double precision, rather than in single precision and then
converted to double precision.

  - __x__
    : shall be default real.

  - __y__
    : shall be default real.

The setting of compiler options specifying _real_ size can affect this
function.

## __Arguments__

  - __x__
    : Must be of default _real(kind=kind(0.0))_ type

  - __y__
    : Must have the same type and kind parameters as __x__

## __Returns__

The return value is of type _real(kind=kind(0.0d0))_.

## __Examples__

Sample program:

```fortran
program demo_dprod
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
integer,parameter :: dp=kind(0.0d0)
real :: x = 5.2
real :: y = 2.3
real(kind=dp) :: dd
   dd = dprod(x,y)
   print *, dd, x*y, kind(x), kind(dd), kind(dprod(x,y))
   ! interesting comparisons
   print *, 52*23
   print *, 52*23/100.0
   print *, 52*23/100.0d0

   !! common extension is to take doubleprecision arguments
   !! and return higher precision
   bigger: block
   doubleprecision :: xx = 5.2d0
   doubleprecision :: yy = 2.3d0
   real(kind=real128) :: ddd
   !ddd = dprod(xx,yy)
   !print *, ddd, xx*yy, kind(xx), kind(ddd), kind(dprod(xx,yy))
   endblock bigger

end program demo_dprod
```
  Results:
```text
   11.959999313354501 11.9599991 4 8 8
        1196
   11.9600000
   11.960000000000001
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# DSHIFTL

## __Name__

__dshiftl__(3) - \[BIT:COPY\] combines bits of arguments __i__ and __j__

## __Syntax__
```fortran
result = dshiftl(i, j, shift)
```
## __Description__

__dshiftl(i, j, shift)__ combines bits of __i__ and __j__. The rightmost __shift__
bits of the result are the leftmost __shift__ bits of __j__, and the remaining
bits are the rightmost bits of __i__.

## __Arguments__

  - __i__
    : Shall be of type _integer_.

  - __j__
    : Shall be of type _integer_, and of the same kind as __i__.

  - __shift__
    : Shall be of type _integer_.

## __Returns__

The return value has same type and kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__dshiftr__(3)](DSHIFTR)

###### fortran-lang intrinsic descriptions
# DSHIFTR

## __Name__

__dshiftr__(3) - \[BIT:COPY\] combines bits of arguments __i__ and __j__

## __Syntax__
```fortran
result = dshiftr(i, j, shift)
```
## __Description__

__dshiftr(i, j, shift)__ combines bits of __i__ and __j__. The leftmost __shift__
bits of the result are the rightmost __shift__ bits of __i__, and the remaining
bits are the leftmost bits of __j__.

## __Arguments__

  - __i__
    : Shall be of type _integer_.

  - __j__
    : Shall be of type _integer_, and of the same kind as __i__.

  - __shift__
    : Shall be of type _integer_.

## __Returns__

The return value has same type and kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__dshiftl__(3)](DSHIFTL)

###### fortran-lang intrinsic descriptions
# EOSHIFT

## __Name__

__eoshift__(3) - \[TRANSFORMATIONAL\] End-off shift elements of an array

## __Syntax__
```fortran
result = eoshift(array, shift, boundary, dim)
```
## __Description__

__eoshift(array, shift\[, boundary, dim\])__ performs an end-off shift
on elements of __array__ along the dimension of __dim__. If __dim__ is omitted it is
taken to be __1__. __dim__ is a scalar of type _integer_ in the range of __1 \<= DIM
\<= n__ where __"n"__ is the rank of __array__. If the rank of __array__ is one, then
all elements of __array__ are shifted by __shift__ places. If rank is greater
than one, then all complete rank one sections of __array__ along the given
dimension are shifted. Elements shifted out one end of each rank one
section are dropped. If __boundary__ is present then the corresponding value
of from __boundary__ is copied back in the other end. If __boundary__ is not
present then the following are copied in depending on the type of __array__.

\*Array Type\* - \*Boundary Value\*

   - Numeric 0 of the type and kind of __array__

   - Logical .false.

   - __Character(len)__ LEN blanks

## __Arguments__

  - __array__
    : May be any type, not scalar.

  - __shift__
    : The type shall be _integer_.

  - __boundary__
    : Same type as ARRAY.

  - __dim__
    : The type shall be _integer_.

## __Returns__

Returns an array of same type and rank as the __array__ argument.

## __Examples__

Sample program:

```fortran
program demo_eoshift
implicit none
    integer, dimension(3,3) :: a
    a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
    a = eoshift(a, SHIFT=[1, 2, 1], BOUNDARY=-5, DIM=2)
    print *
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
end program demo_eoshift
```
  Results:
```text
     1  4  7
     2  5  8
     3  6  9

     4  7 -5
     8 -5 -5
     6  9 -5
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# EPSILON

## __Name__

__epsilon__(3) - \[NUMERIC MODEL\] Epsilon function

## __Syntax__
```fortran
result = epsilon(x)
```
## __Description__

__epsilon(x)__ returns the floating point relative accuracy.
It is the nearly negligible number relative to __1__
such that __1+ little_number__ is not equal to __1__; or more
precisely
```fortran
   real( 1.0, kind(x)) + epsilon(x) /=  real( 1.0, kind(x))
```
It may be thought of as the distance from 1.0 to the next largest
floating point number.

One use of __epsilon__(3) is to select a _delta_ value for algorithms that
search until the calculation is within _delta_ of an estimate.

If _delta_ is too small the algorithm might never halt, as a computation
summing values smaller than the decimal resolution of the data type does
not change.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of the same type as the argument.

## __Examples__

Sample program:

```fortran
program demo_epsilon
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x = 3.143
real(kind=dp) :: y = 2.33d0

   ! so if x is of type real32, epsilon(x) has the value 2**-23
   print *, epsilon(x)
   ! note just the type and kind of x matter, not the value
   print *, epsilon(huge(x))
   print *, epsilon(tiny(x))

   ! the value changes with the kind of the real value though
   print *, epsilon(y)

   ! adding and subtracting epsilon(x) changes x
   write(*,*)x == x + epsilon(x)
   write(*,*)x == x - epsilon(x)

   ! these next two comparisons will be .true. !
   write(*,*)x == x + epsilon(x) * 0.999999
   write(*,*)x == x - epsilon(x) * 0.999999

   ! you can calculate epsilon(1.0d0)
   write(*,*)my_dp_eps()

contains

function my_dp_eps()
! calculate the epsilon value of a machine the hard way
real(kind=dp) :: t
real(kind=dp) :: my_dp_eps

   ! starting with a value of 1, keep dividing the value
   ! by 2 until no change is detected. Note that with
   ! infinite precision this would be an infinite loop,
   ! but floating point values in Fortran have a defined
   ! and limited precision.
   my_dp_eps = 1.0d0
   SET_ST: do
      my_dp_eps = my_dp_eps/2.0d0
      t = 1.0d0 + my_dp_eps
      if (t <= 1.0d0) exit
   enddo SET_ST
   my_dp_eps = 2.0d0*my_dp_eps

end function my_dp_eps

end program demo_epsilon
```
  Results:
```text
  1.1920929E-07
  1.1920929E-07
  1.1920929E-07
  2.220446049250313E-016
 F
 F
 T
 T
  2.220446049250313E-016
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ERFC

## __Name__

__erfc__(3) - \[MATHEMATICS\] Complementary error function

## __Syntax__
```fortran
result = erfc(x)

   elemental function erfc(x)
   real(kind=KIND) :: erfc
   real(kind=KIND),intent(in) :: x
```
## __Description__

__erfc__(x) computes the complementary error function of __x__.  Simpy put
this is equivalent to __1 - erf(x)__, but __erfc__ is provided because
of the extreme loss of relative accuracy if __erf(x)__ is called for
large __x__ and the result is subtracted from __1__.

__erfc(x)__ is defined as

<!--
$$
\text{erfc}(x) = 1 - \text{erf}(x) = 1 - \frac{2}{\sqrt{\pi}} \int_0^x e^{-t^2} dt.
$$
-->

$$
\text{erfc}(x) = 1 - \text{erf}(x) = 1 - \frac{2}{\sqrt{\pi}} \int_x^{\infty} e^{-t^2} dt.
$$

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_ and of the same kind as __x__. It lies in
the range

> 0 \<= __erfc__(x) \<= 2.

## __Examples__

Sample program:

```fortran
program demo_erfc
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 0.17_real64
    write(*,*)x, erfc(x)
end program demo_erfc
```
  Results:
```text
     0.17000000000000001       0.81000753879819121
```
## __Standard__

Fortran 2008 and later

## See also
[__erf__(3)](ERF)

- [Wikipedia:error function](https://en.wikipedia.org/wiki/Error_function)

###### fortran-lang intrinsic descriptions license: MIT)
# ERFC\_SCALED

## __Name__

__erfc\_scaled__(3) - \[MATHEMATICS\] Error function

## __Syntax__
```fortran
result = erfc_scaled(x)
```
## __Description__

__erfc\_scaled__(x) computes the exponentially-scaled complementary
error function of __x__:

$$
e^{x^2} \frac{2}{\sqrt{\pi}} \int_{x}^{\infty}
e^{-t^2} dt.
$$

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_ and of the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_erfc_scaled
implicit none
real(kind(0.0d0)) :: x = 0.17d0
   x = erfc_scaled(x)
   print *, x
end program demo_erfc_scaled
```
  Results:
```text
     0.83375830214998126
```

## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions
# ERF

## __Name__

__erf__(3) - \[MATHEMATICS\] Error function

## __Syntax__
```fortran
result = erf(x)
```
## __Description__

__erf__(x) computes the error function of __x__, defined as
$$
\text{erf}(x) = \frac{2}{\sqrt{\pi}} \int_0^x e^{__-t__^2} dt.
$$

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_, of the same kind as __x__ and lies in the
range __-1__ \<= __erf__(x) \<= 1 .

## __Examples__

Sample program:

```fortran
program demo_erf
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 0.17_real64
    write(*,*)x, erf(x)
end program demo_erf
```
  Results:
```text
     0.17000000000000001       0.18999246120180879
```
## __Standard__

Fortran 2008 and later

## See also

[__erfc__(3)](ERFC)

- [Wikipedia:error function](https://en.wikipedia.org/wiki/Error_function)

###### fortran-lang intrinsic descriptions
# EVENT\_QUERY

## __Name__

__event\_query__(3) - \[COLLECTIVE\] Query whether a coarray event has occurred

## __Syntax__
```fortran
call event_query(event, count, stat)
```
## __Description__

__event\_query__ assigns the number of events to __count__ which have been
posted to the __event__ variable and not yet been removed by calling
__event\_wait__. When __stat__ is present and the invocation was successful, it
is assigned the value __0__. If it is present and the invocation has failed,
it is assigned a positive value and __count__ is assigned the value __-1__.

## __Arguments__

  - __event__
    : (intent(in)) Scalar of type event\_type, defined in
    iso\_fortran\_env; shall not be coindexed.

  - __count__
    : (intent(out))Scalar integer with at least the precision of default
    _integer_.

  - __stat__
    : (OPTIONAL) Scalar default-kind _integer_ variable.

## __Examples__

Sample program:

```fortran
program demo_event_query
use iso_fortran_env
implicit none
type(event_type) :: event_value_has_been_set[*]
integer :: cnt
   if (this_image() == 1) then
      call event_query(event_value_has_been_set, cnt)
      if (cnt > 0) write(*,*) "Value has been set"
   elseif (this_image() == 2) then
      event post(event_value_has_been_set[1])
   endif
end program demo_event_query
```

## __Standard__

TS 18508 or later

###### fortran-lang intrinsic descriptions
# EXECUTE\_COMMAND\_LINE

## __Name__

__execute\_command\_line__(3) - \[SYSTEM:PROCESSES\] Execute a shell command

## __Syntax__
```fortran
   subroutine execute_command_line(command, wait, exitstat, cmdstat, cmdmsg)

    character(len=*),intent(in)  :: command
    logical,intent(in),optional  :: wait
    integer,intent(out),optional :: exitstat
    integer,intent(out),optional :: cmdstat
    character(len=*),intent(out),optional :: cmdmsg
```
## __Description__

The __command__ argument is passed to the shell and executed. (The shell is
generally __sh__(1) on Unix systems, and cmd.exe on Windows.) If __wait__ is
present and has the value __.false.__, the execution of the command is
asynchronous if the system supports it; otherwise, the command is
executed synchronously.

The three last arguments allow the user to get status information. After
synchronous execution, __exitstat__ contains the integer exit code of the
command, as returned by __system__. __cmdstat__ is set to zero if the command
line was executed (whatever its exit status was). __cmdmsg__ is assigned an
error message if an error has occurred.

Note that the system call need not be thread-safe. It is the
responsibility of the user to ensure that the system is not called
concurrently if required.

When the command is executed synchronously, __execute\_command\_line__
returns after the command line has completed execution. Otherwise,
__execute\_command\_line__ returns without waiting.

## __Arguments__

  - __command__
    : a default _character_ scalar containing the command line to be
    executed. The interpretation is programming-environment dependent.

  - __wait__
    : (Optional) a default _logical_ scalar. If __wait__ is present with the
    value .false., and the processor supports asynchronous execution of
    the command, the command is executed asynchronously; otherwise it is
    executed synchronously.

  - __exitstat__
    : (Optional) an _integer_ of the default kind with __intent(inout)__. If
    the command is executed synchronously, it is assigned the value of
    the processor-dependent exit status. Otherwise, the value of
    __exitstat__ is unchanged.

  - __cmdstat__
    : (Optional) an _integer_ of default kind with __intent(inout)__. If an
    error condition occurs and __cmdstat__ is not present, error termination
    of execution of the image is initiated.

    It is assigned the value __-1__ if the processor does not support
    command line execution, a processor-dependent positive value if an
    error condition occurs, or the value __-2__ if no error condition
    occurs but __wait__ is present with the value false and the processor
    does not support asynchronous execution. Otherwise it is assigned
    the value 0.

  - __cmdmsg__
    : (Optional) a _character_ scalar of the default kind. It is an __intent
    (inout)__ argument.If an error condition occurs, it is assigned a
    processor-dependent explanatory message.Otherwise, it is unchanged.

## __Examples__

Sample program:

```fortran
program demo_exec
implicit none
   integer :: i

   call execute_command_line("external_prog.exe", exitstat=i)
   print *, "Exit status of external_prog.exe was ", i

   call execute_command_line("reindex_files.exe", wait=.false.)
   print *, "Now reindexing files in the background"
end program demo_exec
```

## __Note__

Because this intrinsic is making a system call, it is very system
dependent. Its behavior with respect to signaling is processor
dependent. In particular, on POSIX-compliant systems, the SIGINT and
SIGQUIT signals will be ignored, and the SIGCHLD will be blocked. As
such, if the parent process is terminated, the child process might not
be terminated alongside.

## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions
# EXP

## __Name__

__exp__(3) - \[MATHEMATICS\] Exponential function

## __Syntax__
```fortran
result = exp(x)
```
## __Description__

__exp__(x) computes the base "_e_" exponential of __x__ where "_e_" is
_Euler's constant_.

If __x__ is of type _complex_, its imaginary part is regarded as a value
in radians such that (see _Euler's formula_):

if
     __cx=(re,im)__
then
     __exp(cx)=exp(re)*cmplx(cos(im),sin(im))__

Since __exp__(3) is the inverse function of __log__(3) the maximum valid magnitude
of the _real_ component of __x__ is __log(huge(x))__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The value of the result is __e\*\*x__ where __e__ is Euler's constant.

The return value has the same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_exp
implicit none
real :: x , re, im
complex :: cx

   x = 1.0
   write(*,*)"Euler's constant is approximately",exp(x)

   !! complex values
   ! given
   re=3.0
   im=4.0
   cx=cmplx(re,im)

   ! complex results from complex arguments are Related to Euler's formula
   write(*,*)'given the complex value ',cx
   write(*,*)'exp(x) is',exp(cx)
   write(*,*)'is the same as',exp(re)*cmplx(cos(im),sin(im),kind=kind(cx))

   ! exp(3) is the inverse function of log(3) so
   ! the real component of the input must be less than or equal to
   write(*,*)'maximum real component',log(huge(0.0))
   ! or for double precision
   write(*,*)'maximum doubleprecision component',log(huge(0.0d0))

   ! but since the imaginary component is passed to the cos(3) and sin(3)
   ! functions the imaginary component can be any real value

end program demo_exp
```
Results:
```text
 Euler's constant is approximately   2.718282
 given the complex value  (3.000000,4.000000)
 exp(x) is (-13.12878,-15.20078)
 is the same as (-13.12878,-15.20078)
 maximum real component   88.72284
 maximum doubleprecision component   709.782712893384
```
## __Standard__

FORTRAN 77 and later

## __See Also__

* [__log__(3)](LOG)

* Wikipedia:[Exponential function](https://en.wikipedia.org/wiki/Exponential_function)

* Wikipedia:[Euler's formula](https://en.wikipedia.org/wiki/Euler%27s_formula)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# EXPONENT

## __Name__

__exponent__(3) - \[MODEL\_COMPONENTS\] Exponent function

## __Syntax__
```fortran
result = exponent(x)
```
## __Description__

__exponent__(x) returns the value of the exponent part of __x__. If __x__ is
zero the value returned is zero.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type default _integer_.

## __Examples__

Sample program:

```fortran
program demo_exponent
implicit none
real :: x = 1.0
integer :: i
   i = exponent(x)
   print *, i
   print *, exponent(0.0)
end program demo_exponent
```
  Results:
```text
              1
              0
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# EXTENDS\_TYPE\_OF

## __Name__

__extends\_type\_of__(3) - \[STATE\] determine if the dynamic type of __a__ is an extension of the dynamic type of __mold__.

## __Syntax__
```fortran
result=extends_type_of(a, mold)
```
## __Description__

__extends\_type\_of__(3) is __.true.__ if and only if the dynamic type of __a__
is an extension of the dynamic type of __mold__.

## __Options__

  - __a__
    : shall be an object of extensible type. If it is a pointer, it
    shall not have an undefined association status.

  - __mold__
    : shall be an object of extensible type. If it is a pointer, it
    shall not have an undefined association status.

## __Returns__

  - __result__
    : Default logical scalar.

  - __value__
    : If __mold__ is unlimited polymorphic and is either a disassociated
    pointer or unallocated allocatable variable, the result is
    true; otherwise if __a__ is unlimited polymorphic and is either a
    disassociated pointer or unallocated allocatable variable, the result
    is false; otherwise the result is true if and only if the dynamic
    type of __a__ is an extension type of the dynamic type of __mold__.

    The dynamic type of a disassociated pointer or unallocated
    allocatable variable is its declared type.

## __Examples__

###### fortran-lang intrinsic descriptions
# FINDLOC

## __Name__

__findloc__(3) - \[ARRAY:LOCATION\] Location of first element of ARRAY identified by MASK along dimension DIM having a value

## __Syntax__
```fortran
findloc (array, value, dim, mask, kind, back)

or

findloc(array, value, mask, kind, back)
```
## __Description__

Location of the first element of __array__ identified by __mask__ along
dimension __dim__ having a value equal to __value__.

If both __array__ and __value__ are of type logical, the comparison is
performed with the __.eqv.__ operator; otherwise, the comparison is
performed with the == operator. If the value of the comparison is
true, that element of __array__ matches __value__.

If only one element matches __value__, that element's subscripts are
returned. Otherwise, if more than one element matches __value__ and
__back__ is absent or present with the value false, the element whose
subscripts are returned is the first such element, taken in array
element order. If __back__ is present with the value true, the element
whose subscripts are returned is the last such element, taken in array
element order.

## __Options__

  - __array__
    : shall be an array of intrinsic type.

  - __value__
    : shall be scalar and in type conformance with __array__, as specified
    in Table 7.3 for relational intrinsic operations 7.1.5.5.2).

  - __dim__
    : shall be an integer scalar with a value in the range 1 __DIM__ n, where
    n is the rank of __array__. The corresponding actual argument shall
    not be an optional dummy argument.

  - __mask__
    : (optional) shall be of type logical and shall be conformable with
    __array__.

  - __kind__
    : (optional) shall be a scalar integer initialization expression.

  - __back__
    : (optional) shall be a logical scalar.

## __Returns__

Result Characteristics. Integer. If __kind__ is present, the kind type
parameter is that specified by the value of __kind__; otherwise the kind
type parameter is that of default integer type. If __dim__ does not appear,
the result is an array of rank one and of size equal to the rank of
__array__; otherwise, the result is of rank n - 1 and shape

```
   [d1 , d2 , . . . , dDIM-1 , dDIM+1 , . . . , dn ]
```

where

```
   [d1 , d2 , . . . , dn ]
```

is the shape of __array__.

## __Returns__

  - __Case (i):__
    The result of __findloc (array, value)__ is a rank-one array whose
    element values are the values of the subscripts of an element of
    __array__ whose value matches __value__. If there is such a value, the
    ith subscript returned lies in the range 1 to ei , where ei is the
    extent of the ith dimension of __array__. If no elements match __value__
    or __array__ has size zero, all elements of the result are zero.

  - __Case (ii):__
    the result of __findloc (array, value, mask = mask)__ is a
    rank-one array whose element values are the values of the subscripts
    of an element of __array__, corresponding to a true element of __mask__,
    whose value matches __value__. If there is such a value, the ith
    subscript returned lies in the range 1 to ei , where ei is the
    extent of the ith dimension of __array__. If no elements match
    __value__, __array__ has size zero, or every element of __mask__ has the
    value false, all elements of the result are zero.

  - __Case (iii):__
    If __array__ has rank one, the result of

```
      findloc (array, value, dim=dim [, mask = mask])
```

is a scalar whose value is equal to that of the first element of

```
      findloc (array, value [, mask = mask])
```

Otherwise, the value of element

```
      (s1 , s2 , . . . , sDIM-1 , sDIM+1 , . . . , sn )
```

of the result is equal to

```
      findloc (array (s1, s2, ..., sdim-1, :, sdim+1, ..., sn ), &
      value, dim=1 [, mask = mask (s1, s2, ..., sdim-1, :,
                      sdim+1 , ... , sn )]).
```
## __Examples__

  - __Case (i):__
    The value of

```
        findloc ([2, 6, 4, 6,], value = 6)
```

is \[2\], and the value of

```
        findloc ([2, 6, 4, 6], value = 6, back = .true.)
```

is \[4\].

  - __Case (ii):__
    If __a__ has the value

```text
      0 -5  7 7
      3  4 -1 2
      1  5  6 7
```

and __m__ has the value

```text
       T T F T
       T T F T
       T T F T

      findloc (a, 7, mask = m)
```

has the value \[1, 4\] and

```
      findloc (a, 7, mask = m, back = .true.)
```

has the value \[3, 4\]. This is independent of the declared lower
bounds for __a__ .

  - __Case (iii):__
    The value of

```
      findloc ([2, 6, 4], value = 6, dim = 1)
```

is 2. If __b__ has the value

```
       1 2 -9
       2 2  6
```

> findloc (b, __value__ = 2, dim = 1)

has the value \[2, 1, 0\] and

```
      findloc (b, value = 2, dim = 2)
```

has the value \[2, 1\]. This is independent of the declared lower
bounds for __b__.

###### fortran-lang intrinsic descriptions
# FLOOR

## __Name__

__floor__(3) - \[NUMERIC\] function to return largest integral value not greater than argument

## __Syntax__
```fortran
result = floor(a, KIND)

    elemental function floor(a,KIND)
    integer(kind=KIND) :: floor
    real(kind=kind(a)),intent(in) :: a
    integer(kind=IKIND),intent(in),optional :: KIND
```
    where __KIND__ is any valid value for type _integer_.
## __Description__

__floor(a)__ returns the greatest integer less than or equal to __a__.
That is, it picks the whole number at or to the left of the value on
the scale __-huge(int(a,kind=KIND))-1__ to __huge(int(a),kind=KIND)__.

## __Arguments__

  - __a__
    : The type shall be _real_.

  - __kind__
    : (Optional) A scalar _integer_ constant initialization expression
    indicating the kind parameter of the result.

## __Returns__

The return value is of type _integer(kind)_ if __kind__ is present and of
default-kind _integer_ otherwise.

The result is undefined if it cannot be represented in the specified
integer type.

## __Examples__

Sample program:

```fortran
program demo_floor
implicit none
real :: x = 63.29
real :: y = -63.59
    print *, x, floor(x)
    print *, y, floor(y)
   ! elemental
   print *,floor([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

   ! note even a small deviation from the whole number changes the result
   print *,      [2.0,2.0-epsilon(0.0),2.0-2*epsilon(0.0)]
   print *,floor([2.0,2.0-epsilon(0.0),2.0-2*epsilon(0.0)])

   ! A=Nan, Infinity or  <huge(0_KIND)-1 < A > huge(0_KIND) is undefined
end program demo_floor
```
Results:
```text
      63.29000              63
     -63.59000             -64
             -3          -3          -3          -2          -2          -1
             -1           0           0           1           1           2
              2           2           2
      2.000000       2.000000       2.000000
              2           1           1
```

## __Standard__

Fortran 95 and later

## __See Also__

[__ceiling__(3)](CEILING),
[__nint__(3)](NINT)

[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__int__(3)](INT),
[__selected_int_kind__(3)](SELECTED_INT_KIND)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# FRACTION

## __Name__

__fraction__(3) - \[MODEL\_COMPONENTS\] Fractional part of the model representation

## __Syntax__
```fortran
y = fraction(x)
```
## __Description__

__fraction(x)__ returns the fractional part of the model representation
of __x__.

## __Arguments__

  - __x__
    : The type of the argument shall be a _real_.

## __Returns__

The return value is of the same type and kind as the argument. The
fractional part of the model representation of __x__ is returned; it is
__x \* radix(x)\*\*(-exponent(x))__.

## __Examples__

Sample program:

```fortran
program demo_fraction
implicit none
real :: x
   x = 178.1387e-4
   print *, fraction(x), x * radix(x)**(-exponent(x))
end program demo_fraction
```
  Results:
```text
     0.570043862      0.570043862
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# GAMMA

## __Name__

__gamma__(3) - \[MATHEMATICS\] Gamma function, which yields factorials for positive whole numbers

## __Syntax__
```fortran
x = gamma(x)
```
## __Description__

__gamma(x)__ computes Gamma of __x__. For positive whole number values of __n__ the
Gamma function can be used to calculate factorials, as  __(n-1)!  == gamma(real(n))__.
That is
```text
n! == gamma(real(n+1))
```

$$ \\__Gamma__(x) = \\int\_0\*\*\\infty
t\*\*{x-1}{\\mathrm{e}}\*\*{__-t__}\\,{\\mathrm{d}}t $$

## __Arguments__

  - __x__
    : Shall be of type _real_ and neither zero nor a negative integer.

## __Returns__

The return value is of type _real_ of the same kind as _x_.

## __Examples__

Sample program:

```fortran
program demo_gamma
use, intrinsic :: iso_fortran_env, only : wp=>real64
implicit none
real :: x, xa(4)
integer :: i

   x = gamma(1.0)
   write(*,*)'gamma(1.0)=',x

   ! elemental
   xa=gamma([1.0,2.0,3.0,4.0])
   write(*,*)xa
   write(*,*)

   ! gamma(3) is related to the factorial function
   do i=1,20
      ! check value is not too big for default integer type
      if(factorial(i).gt.huge(0))then
         write(*,*)i,factorial(i)
      else
         write(*,*)i,factorial(i),int(factorial(i))
      endif
   enddo
   ! more factorials
   FAC: block
   integer,parameter :: n(*)=[0,1,5,11,170]
   integer :: j
      do j=1,size(n)
         write(*,'(*(g0,1x))')'factorial of', n(j),' is ', &
          & product([(real(i,kind=wp),i=1,n(j))]),  &
          & gamma(real(n(j)+1,kind=wp))
      enddo
   endblock FAC

contains

function factorial(i) result(f)
integer,parameter :: dp=kind(0d0)
integer,intent(in) :: i
real :: f
   if(i.le.0)then
      write(*,'(*(g0))')'<ERROR> gamma(3) function value ',i,' <= 0'
      stop      '<STOP> bad value in gamma function'
   endif
   f=gamma(real(i+1))
end function factorial

end program demo_gamma
```
  Results:
```text
    gamma(1.0)=   1.000000
      1.000000       1.000000       2.000000       6.000000

              1   1.000000               1
              2   2.000000               2
              3   6.000000               6
              4   24.00000              24
              5   120.0000             120
              6   720.0000             720
              7   5040.000            5040
              8   40320.00           40320
              9   362880.0          362880
             10   3628800.         3628800
             11  3.9916800E+07    39916800
             12  4.7900160E+08   479001600
             13  6.2270208E+09
             14  8.7178289E+10
             15  1.3076744E+12
             16  2.0922791E+13
             17  3.5568741E+14
             18  6.4023735E+15
             19  1.2164510E+17
             20  2.4329020E+18
   factorial of 0  is  1.000000000000000 1.000000000000000
   factorial of 1  is  1.000000000000000 1.000000000000000
   factorial of 5  is  120.0000000000000 120.0000000000000
   factorial of 11  is  39916800.00000000 39916800.00000000
   factorial of 170  is  .7257415615307994E+307 .7257415615307999E+307
```

## __Standard__

Fortran 2008 and later

## __See Also__

Logarithm of the Gamma function: [__log\_gamma__(3)](LOG_GAMMA)

[Wikipedia: Gamma_function](https://en.wikipedia.org/wiki/Gamma_function)

###### fortran-lang intrinsic descriptions
# GET\_COMMAND\_ARGUMENT

## __Name__

__get\_command\_argument__(3) - \[SYSTEM:COMMAND LINE\] Get command line arguments

## __Syntax__
```fortran
     call get_command_argument(number, value, length, status)

     subroutine get_command_argument(number,value,length.status)
     integer,intent(in)                    :: number
     character(len=*),intent(out),optional :: value
     integer,intent(out),optional          :: length
     integer,intent(out),optional          :: status
```
## __Description__

Retrieve the __number__-th argument that was passed on the command line
when the containing program was invoked.

There is not anything specifically stated about what an argument is but
in practice the arguments are split on whitespace unless the arguments
are quoted and IFS values (Internal Field Separators) used by common
shells are ignored.

## __Options__

  - __number__
    : Shall be a scalar of type __integer__, __number \>= 0__. If __number =
    0__, __value__ is set to the name of the program (on systems that support
    this feature).

## __Returns__

  - __value__
    :Shall be a scalar of type _character_ and of default kind. After
    get\_command\_argument returns, the __value__ argument holds the
    __number__-th command line argument. If __value__ can not hold the argument,
    it is truncated to fit the length of __value__. If there are less than
    __number__ arguments specified at the command line, __value__ will be filled
    with blanks.

  - __length__
    :(Optional) Shall be a scalar of type _integer_. The __length__
    argument contains the length of the __number__-th command line argument.

  - __status__
    :(Optional) Shall be a scalar of type _integer_. If the argument
    retrieval fails, __status__ is a positive number; if __value__ contains a
    truncated command line argument, __status__ is __-1__; and otherwise the
    __status__ is zero.

## __Examples__

Sample program:

```fortran
program demo_get_command_argument
implicit none
character(len=255)           :: progname
integer                      :: stat
integer                      :: count,i, longest, argument_length
integer,allocatable          :: istat(:), ilen(:)
character(len=:),allocatable :: args(:)
  !
  ! get number of arguments
  count = command_argument_count()
  write(*,*)'The number of arguments is ',count
  !
  ! simple usage
  !
  call get_command_argument (0, progname, status=stat)
  if (stat == 0) then
     print *, "The program's name is " // trim (progname)
  endif
  !
  ! showing how to make an array to hold any argument list
  !
  ! find longest argument
  !
  longest=0
  do i=0,count
     call get_command_argument(number=i,length=argument_length)
     longest=max(longest,argument_length)
  enddo
  !
  ! allocate string array big enough to hold command line
  ! argument strings and related information
  !
  allocate(character(len=longest) :: args(0:count))
  allocate(istat(0:count))
  allocate(ilen(0:count))
  !
  ! read the arguments into the array
  !
  do i=0,count
    call get_command_argument(i, args(i),status=istat(i),length=ilen(i))
  enddo
  !
  ! show the results
  !
  write (*,'(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")') &
  & (i,istat(i),ilen(i),args(i)(:ilen(i)),i=0,count)
end program demo_get_command_argument
```
Results:
```text
/demo_get_command_argument a    test  'of getting   arguments  ' "  leading"

 The number of arguments is            5
 The program's name is xxx
000 00000 00003 [./test_get_command_argument]
001 00000 00001 [a]
003 00000 00004 [test]
004 00000 00024 [of getting   arguments  ]
005 00000 00018 [  leading]
```
## __Standard__

Fortran 2003 and later

## __See Also__

[__get\_command__(3)](GET_COMMAND),
[__command\_argument\_count__(3)](COMMAND_ARGUMENT_COUNT)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# GET\_COMMAND

## __Name__

__get\_command__(3) - \[SYSTEM:COMMAND LINE\] Get the entire command line

## __Syntax__
```fortran
   call get_command(command, length, status)

    subroutine get_command(command,length,status)
    character(len=*),intent(out),optional :: command
    integer,intent(out),optional :: length
    integer,intent(out),optional :: status
```
## __Description__

Retrieve the entire command line that was used to invoke the program.

Note that what is typed on the command line is often processed by
a shell. The shell typically processes special characters and white
space before passing it to the program. The processing can typically be
turned off by turning off globbing or quoting the command line arguments
and/or changing the default field separators, but this should rarely
be necessary.

## __Returns__

  - __command__
    : Shall be of type _character_ and of default kind. If
    __command__ is present, stores the entire command line that was used to
    invoke the program in __command__.

  - __length__
    : Shall be of type _integer_ and of default kind. If __length__
    is present, it is assigned the length of the command line.

  - __status__
    : Shall be of type _integer_ and of default kind. If __status__
    is present, it is assigned 0 upon success of the command, __-1__ if
    __command__ is too short to store the command line, or a positive value
    in case of an error.

## __Examples__

Sample program:

```fortran
program demo_get_command
implicit none
integer                      :: COMMAND_LINE_LENGTH
character(len=:),allocatable :: COMMAND_LINE
   ! get command line length
   call get_command(length=COMMAND_LINE_LENGTH)
   ! allocate string big enough to hold command line
   allocate(character(len=COMMAND_LINE_LENGTH) :: COMMAND_LINE)
   ! get command line as a string
   call get_command(command=COMMAND_LINE)
   ! trim leading spaces just in case
   COMMAND_LINE=adjustl(COMMAND_LINE)
   write(*,'("OUTPUT:",a)')COMMAND_LINE
end program demo_get_command
```
Results:
```bash
     # note that shell expansion removes some of the whitespace
     # without quotes
     ./test_get_command  arguments    on command   line to   echo

     OUTPUT:./test_get_command arguments on command line to echo

     # using the bash shell with single quotes
     ./test_get_command  'arguments  *><`~[]!{}?"\'| '

     OUTPUT:./test_get_command arguments  *><`~[]!{}?"'|
```
## __Standard__

Fortran 2003 and later

## __See Also__

[__get\_command\_argument__(3)](GET_COMMAND_ARGUMENT),
[__command\_argument\_count__(3)](COMMAND_ARGUMENT_COUNT)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# GET\_ENVIRONMENT\_VARIABLE

## __Name__

__get\_environment\_variable__(3) - \[SYSTEM:ENVIRONMENT\] Get an environmental variable

## __Syntax__
```fortran
  call get_environment_variable(name, value, length, status, trim_name)

   character(len=*),intent(in) :: name
   character(len=*),intent(out),optional :: value
   integer,intent(out),optional :: length
   integer,intent(out),optional :: status
   logical,intent(out),optional :: trim_name
```
## __Description__

Get the __value__ of the environmental variable __name__.

Note that __get\_environment\_variable__(3) need not be thread-safe. It
is the responsibility of the user to ensure that the environment is not
being updated concurrently.

## __Options__

  - __name__
    : The name of the environment variable to query.

    Shall be a scalar of type _character_ and of default kind.

## __Returns__

  - __value__
    : The value of the environment variable being queried.

    Shall be a scalar of type _character_ and of default kind.
    The value of __name__ is stored in __value__. If __value__ is not large enough
    to hold the data, it is truncated. If __name__ is not set, __value__ will be
    filled with blanks.

  - __length__
    : Argument __length__ contains the length needed for storing the
    environment variable __name__ or zero if it is not present.

    Shall be a scalar of type _integer_ and of default kind.

  - __status__
    : __status__ is __-1__ if __value__ is present but too short for the
    environment variable; it is __1__ if the environment variable does not
    exist and __2__ if the processor does not support environment variables;
    in all other cases __status__ is zero.

    Shall be a scalar of type _integer_ and of default kind.

  - __trim\_name__
    : If __trim\_name__ is present with the value __.false.__, the trailing blanks in
    __name__ are significant; otherwise they are not part of the environment
    variable name.

    Shall be a scalar of type _logical_ and of default kind.

## __Examples__

Sample program:

```fortran
program demo_getenv
implicit none
character(len=:),allocatable :: homedir
character(len=:),allocatable :: var
     var='HOME'
     homedir=get_env(var)
     write (*,'(a,"=""",a,"""")')var,homedir

contains

function get_env(NAME,DEFAULT) result(VALUE)
! a function that makes calling get_environment_variable(3) simple
implicit none
character(len=*),intent(in)          :: NAME
character(len=*),intent(in),optional :: DEFAULT
character(len=:),allocatable         :: VALUE
integer                              :: howbig
integer                              :: stat
integer                              :: length
   ! get length required to hold value
   length=0
   VALUE=''
   if(NAME.ne.'')then
      call get_environment_variable( &
      & NAME, length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
       !*!print *, NAME, " is not defined in the environment. Strange..."
       VALUE=''
      case (2)
       !*!print *, &
       !*!"This processor does not support environment variables. Boooh!"
       VALUE=''
      case default
       ! make string to hold value of sufficient size
       if(allocated(VALUE))deallocate(VALUE)
       allocate(character(len=max(howbig,1)) :: VALUE)
       ! get value
       call get_environment_variable( &
       & NAME,VALUE,status=stat,trim_name=.true.)
       if(stat.ne.0)VALUE=''
      end select
   endif
   if(VALUE.eq.''.and.present(DEFAULT))VALUE=DEFAULT
end function get_env

end program demo_getenv
```

Typical Results:

```text
   HOME="/home/urbanjs"
```

## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# HUGE

## __Name__

__huge__(3) - \[NUMERIC MODEL\] Largest number of a type and kind

## __Syntax__
```fortran
result = huge(x)

   function huge(x) result(answer)
   TYPE(kind=KIND),intent(in) :: x
   TYPE(kind=KIND) :: answer
```
   where __TYPE__ may be _real_ or _integer_ and __KIND__ is any supported
   associated _kind_.

## __Description__

__huge(x)__ returns the largest number that is not an infinity for the
kind and type of __x__.

## __Arguments__

  - __x__
    : Shall be an arbitrary value of type _real_ or _integer_.
    The value is used merely to determine what _kind_ and _type_ of
    scalar is being queried.

## __Returns__

The return value is of the same type and kind as _x_ and is the
largest value supported by the specified model.

## __Examples__

Sample program:

```fortran
program demo_huge
implicit none
character(len=*),parameter :: f='(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)'
integer :: i,j,k,biggest
real :: v, w
   ! basic
   print *, huge(0), huge(0.0), huge(0.0d0)
   print *, tiny(0.0), tiny(0.0d0)

   ! advanced
   biggest=huge(0)
   ! be careful of overflow when using integers in computation
   do i=1,14
      j=6**i   ! Danger, Danger
      w=6**i   ! Danger, Danger
      v=6.0**i
      k=v      ! Danger, Danger
      if(v.gt.biggest)then
         write(*,f) i, j, k, v, v.eq.w, 'wrong j and k and w'
      else
         write(*,f) i, j, k, v, v.eq.w
      endif
   enddo
end program demo_huge
```
Results:
```
  2147483647  3.4028235E+38  1.797693134862316E+308
  1.1754944E-38  2.225073858507201E-308

    1      6           6             6. T
    2      36          36            36. T
    3      216         216           216. T
    4      1296        1296          1296. T
    5      7776        7776          7776. T
    6      46656       46656         46656. T
    7      279936      279936        279936. T
    8      1679616     1679616       1679616. T
    9      10077696    10077696      10077696. T
    10     60466176    60466176      60466176. T
    11     362797056   362797056     362797056. T
    12    -2118184960 -2147483648    2176782336. F wrong for j and k and w
    13     175792128  -2147483648   13060694016. F wrong for j and k and w
    14     1054752768 -2147483648   78364164096. F wrong for j and k and w
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost

# HYPOT

## __Name__

__hypot__(3) - \[MATHEMATICS\] returns the distance between the point and the origin.

## __Syntax__
```fortran
result = hypot(x, y)

   real(kind=KIND) elemental function hypot(x,y) result(value)
   real(kind=KIND),intent(in) :: x, y
```
   where __x,y,value__ shall all be of the same __kind__.

## __Description__

__hypot(x,y)__ is referred to as the Euclidean distance function. It is equal to
__sqrt(x**2 + y**2)__, without undue underflow or overflow.

In mathematics, the _Euclidean distance_ between two points in Euclidean
space is the length of a line segment between two points.

__hypot(x,y)__ returns the distance between the point __<x,y>__ and the origin.

## __Arguments__

  - __x__
    : The type shall be _real_.

  - __y__
    : The type and kind type parameter shall be the same as __x__.

## __Returns__

The return value has the same type and kind type parameter as __x__.

The result is the positive magnitude of the distance of the point __<x,y>__ from the
origin __<0.0,0.0>__ .

## __Examples__

Sample program:

```fortran
program demo_hypot
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real32) :: x, y
real(kind=real32),allocatable :: xs(:), ys(:)
integer :: i
character(len=*),parameter :: f='(a,/,SP,*(3x,g0,1x,g0:,/))'

   x = 1.e0_real32
   y = 0.5e0_real32

   write(*,*)
   write(*,'(*(g0))')'point <',x,',',y,'> is ',hypot(x,y)
   write(*,'(*(g0))')'units away from the origin'
   write(*,*)

   ! elemental
   xs=[  x,  x**2,  x*10.0,  x*15.0, -x**2  ]
   ys=[  y,  y**2, -y*20.0,  y**2,   -y**2  ]

   write(*,f)"the points",(xs(i),ys(i),i=1,size(xs))
   write(*,f)"have distances from the origin of ",hypot(xs,ys)
   write(*,f)"the closest is",minval(hypot(xs,ys))

end program demo_hypot
```
Results:
```text
   point <1.00000000,0.500000000> is 1.11803401
   units away from the origin

   the points
      +1.00000000 +0.500000000
      +1.00000000 +0.250000000
      +10.0000000 -10.0000000
      +15.0000000 +0.250000000
      -1.00000000 -0.250000000
   have distances from the origin of
      +1.11803401 +1.03077638
      +14.1421356 +15.0020828
      +1.03077638
   the closest is
      +1.03077638
```
## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# IACHAR

## __Name__

__iachar__(3) - \[CHARACTER:CONVERSION\] Code in ASCII collating sequence
```fortran
   elemental function iachar(c,KIND) result(i)
   integer(kind=KIND) :: i
   character(len=1),intent(in) :: c
   integer,intent(in),optional :: KIND
```
The return value is of type _integer_ and of kind __KIND__. If __KIND__ is absent,
the return value is of default integer kind.

## __Syntax__
```fortran
result = iachar(c, kind)
```
## __Description__

__iachar__(c) returns the code for the ASCII character in the first
character position of __c__.

   If __c__ is an ASCII character (ie. in the collating sequence defined
   by ISO 646:1983) the result is the position of __c__ in that sequence
   and is in the range 0 to 127.

   An undefined value is returned if __c__ is not in the ASCII collating
   sequence.

## __Arguments__

  - __c__
    : The character determines the value of the ADE
    (ASCII Decimal Equivalent) that will be returned.

  - __kind__
    :  An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__
   The _integer_ code for the first character in __c__. It represents
   the position of the character in the ASCII collating sequence.

## __Examples__

Sample program:

```fortran
program demo_iachar
implicit none

write(*,*)iachar(['a','z','A','Z'])

! create function to convert uppercase letters to lowercase
   write(*,'(a)')lower('abcdefg ABCDEFG')
contains
!
elemental pure function lower(str) result (string)
! Changes a string to lowercase
character(*), intent(In)     :: str
character(len(str))          :: string
integer                      :: i
   string = str
   ! step thru each letter in the string in specified range
   do i = 1, len(str)
      select case (str(i:i))
      case ('A':'Z') ! change letter to miniscule
         string(i:i) = char(iachar(str(i:i))+32)
      case default
      end select
   end do
end function lower
!
end program demo_iachar
```
  Results:
```text
             97         122          65          90
   abcdefg abcdefg
```
## __Note__

See [__ichar__(3)](ICHAR) for a discussion of converting between numerical
values and formatted string representations.

## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003 and later

## __See Also__

[__achar__(3)](ACHAR),
[__char__(3)](CHAR),
[__ichar__(3)](ICHAR)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX),
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions (License: MIT) @urbanjost
# IALL

## __Name__

__iall__(3) - \[BIT:LOGICAL\] Bitwise and of array elements

## __Syntax__
```fortran
  result = iall(array, mask)

    or

  result = iall(array, dim, mask)
```
## __Description__

Reduces with bitwise _and_ the elements of __array__ along dimension __dim__ if
the corresponding element in __mask__ is __.true.__.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __array__.

  - __mask__
    : (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as __array__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the bitwise _all_ of all elements in __array__
is returned. Otherwise, an array of rank __n-1__, where __n__ equals the
rank of __array__, and a shape similar to that of __array__ with dimension __dim__
dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_iall
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
 & int8, int16, int32, int64
implicit none
integer(kind=int8) :: a(2)

   a(1) = int(b'00100100')
   a(2) = int(b'01101010')

   print '(b8.8)', iall(a)

end program demo_iall
```
  Results:
```text
   00100000
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__iany__(3)](IANY),
[__iparity__(3)](IPARITY),
[__iand__(3)](IAND)

###### fortran-lang intrinsic descriptions
# IAND

## __Name__

__iand__(3) - \[BIT:LOGICAL\] Bitwise logical and

## __Syntax__
```fortran
result = iand(i, j)
```
## __Description__

Bitwise logical __and__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __j__
    : The type shall be _integer_, of the same kind as __i__.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Examples__

Sample program:

```fortran
program demo_iand
implicit none
integer :: a, b
      data a / z'f' /, b / z'3' /
      write (*,*) iand(a, b)
end program demo_iand
```
  Results:
```text
              3
```
## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR),
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions
# IANY

## __Name__

__iany__(3) - \[BIT:LOGICAL\] Bitwise or of array elements

## __Syntax__
```fortran
  result = iany(array, mask)

    or

  result = iany(array, dim, mask)
```
## __Description__

Reduces with bitwise or (inclusive or) the elements of __array__ along
dimension __dim__ if the corresponding element in __mask__ is __.true.__.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __array__.

  - __mask__
    : (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as __array__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the bitwise _or_ of all elements in __array__
is returned. Otherwise, an array of rank __n-1__, where __n__ equals the
rank of __array__, and a shape similar to that of __array__ with dimension __dim__
dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_iany
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
 & int8, int16, int32, int64
implicit none
integer(kind=int8) :: a(2)
     a(1) = int(b'00100100')
     a(2) = int(b'01101010')
     print '(b8.8)', iany(a)
end program demo_iany
```
Results:

```
   01101110
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__iparity__(3)](IPARITY),
[__iall__(3)](IALL),
[__ior__(3)](IOR)

###### fortran-lang intrinsic descriptions
# IBCLR

## __Name__

__ibclr__(3) - \[BIT:SET\] Clear bit

## __Syntax__
```fortran
result = ibclr(i, pos)
```
## __Description__

__ibclr__ returns the value of __i__ with the bit at position __pos__ set to zero.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __pos__
    : The type shall be _integer_. A value of zero refers to the least
    significant bit. __pos__ is an __intent(in)__ scalar or array of type
    _integer_. The value of __pos__ must be within the range zero to
    __(bit\_size(i)-1__).

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions
# IBITS

## __Name__

__ibits__(3) - \[BIT:COPY\] Bit extraction

## __Syntax__
```fortran
result = ibits(i, pos, len)
```
## __Description__

__ibits__ extracts a field of length __len__ from __i__, starting from
bit position __pos__ and extending left for __len__ bits. The result is
right-justified and the remaining bits are zeroed. The value of pos+len
must be less than or equal to the value __bit\_size(i)__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __pos__
    : The type shall be _integer_. A value of zero refers to the least
    significant bit.

  - __len__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR),
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions
# IBSET

## __Name__

__ibset__(3) - \[BIT:SET\] Set bit

## __Syntax__
```fortran
result = ibset(i, pos)
```
## __Description__

__ibset__ returns the value of __i__ with the bit at position __pos__ set to one.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __pos__
    : The type shall be _integer_. A value of zero refers to the least
    significant bit. pos is an __intent(in)__ scalar or array of type
    _integer_. The value of pos must be within the range zero to
    __(bit\_size(i)-1__).

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR),
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions
# ICHAR

## __Name__

__ichar__(3) - \[CHARACTER:CONVERSION\] Character-to-integer conversion function

## __Syntax__
```fortran
   elemental function ichar(c,kind)

    character(len=1),intent(in) :: c
    integer,intent(in),optional :: kind
```
## __Description__

__ichar(c)__ returns the code for the character in the system's native
character set. The correspondence between characters and their codes is
not necessarily the same across different Fortran implementations. For
example, a platform using EBCDIC would return different values than an
ASCII platform.

See __iachar__(3) for specifically working with the ASCII character
set.

## __Arguments__

  - __c__
    : Shall be a scalar _character_, with __intent(in)__

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default _integer_ kind.

## __Examples__

Sample program:

```fortran
program demo_ichar
implicit none
integer i

   write(*,*)ichar(['a','z','A','Z'])
   do i=0,127
      call printme()
   enddo

contains

subroutine printme()
character(len=1) :: letter

   letter=char(i)
   select case(i)
   case (:31,127:)
      write(*,'(1x,i0.3,1x,"HEX=",z2.2,1x,i0)')i,letter,ichar(letter)
   case default
      write(*,'(1x,i0.3,1x,a,1x,i0)')i,letter,ichar(letter)
   end select

end subroutine printme

end program demo_ichar
```

## __Note__

No intrinsic exists to convert between a numeric value and a formatted
character string representation -- for instance, given the _character_
value '154', obtaining an _integer_ or _real_ value with the value 154, or
vice versa. Instead, this functionality is provided by internal-file
I/O, as in the following example:

```
program read_val
integer value
character(len=10) string, string2
   string = '154'

   ! Convert a string to a numeric value
   read (string,'(I10)') value
   print *, value

   ! Convert a value to a formatted string
   write (string2,'(I10)') value
   print *, string2
end program read_val
```
  Results:
```text
            154
           154
```

## __Standard__

Fortran 95 and later, with KIND argument -Fortran 2003 and later

## __See Also__

[__achar__(3)](ACHAR),
[__char__(3)](CHAR),
[__iachar__(3)](IACHAR)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
 [__adjustl__(3)](ADJUSTL),
 [__adjustr__(3)](ADJUSTR),
 [__index__(3)](INDEX),

 [__scan__(3)](SCAN),
 [__verify__(3)](VERIFY)

  - __Nonelemental:__
 [__len\_trim__(3)](LEN_TRIM),
 [__len__(3)](LEN),
 [__repeat__(3)](REPEAT),
 [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# IEOR

## __Name__

__ieor__(3) - \[BIT:LOGICAL\] Bitwise logical exclusive or

## __Syntax__
```fortran
result = ieor(i, j)
```
## __Description__

__ieor__ returns the bitwise Boolean exclusive-__or__ of __i__ and __j__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __j__
    : The type shall be _integer_, of the same kind as __i__.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR),
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions
# IMAGE\_INDEX

## __Name__

__image\_index__(3) - \[COLLECTIVE\] Cosubscript to image index conversion

## __Syntax__
```fortran
result = image_index(coarray, sub)
```
## __Description__

Returns the image index belonging to a cosubscript.

## __Arguments__

  - __coarray__
    : Coarray of any type.

  - __sub__
    : default integer rank-1 array of a size equal to the corank of
    __coarray__.

## __Returns__

Scalar default integer with the value of the image index which
corresponds to the cosubscripts. For invalid cosubscripts the result is
zero.

## __Examples__

Sample program:

```fortran
program demo image_index
implicit none
integer :: array[2,-1:4,8,*]
   ! Writes  28 (or 0 if there are fewer than 28 images)
   write (*,*) image_index(array, [2,0,3,1])
end demo image_index
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__this\_image__(3)](THIS_IMAGE),
[__num\_images__(3)](NUM_IMAGES)

###### fortran-lang intrinsic descriptions
# INDEX

## __Name__

__index__(3) - \[CHARACTER:SEARCH\] Position of a substring within a string

## __Syntax__
```fortran
   index(string, substring, back, kind) result(start)

     character(len=*),intent(in) :: string
     character(len=*),intent(in) :: substring
     logical,intent(in),optional :: back
     integer,intent(in),optional :: kind
     integer(kind=KIND)          :: start
```
## __Description__

Returns the position of the start of the leftmost or rightmost
occurrence of string __substring__ in __string__, counting from one. If
__substring__ is not present in __string__, zero is returned.

## __Arguments__

  - __string__
    : string to be searched

  - __substring__
    : string to attempt to locate in __string__

  - __back__
    : If the __back__ argument is present and true, the return value is the
    start of the rightmost occurrence rather than the leftmost.

  - __kind__
    : An _integer_ initialization expression indicating the kind parameter
    of the result.

## __Returns__

  - __START__
    : The return value is of type _integer_ and of kind __kind__. If __kind__ is
    absent, the return value is of default integer kind.

## __Examples__

Example program

```fortran
program demo_index
implicit none
character(len=*),parameter :: str=&
   'Search this string for this expression'
   !1234567890123456789012345678901234567890
   write(*,*)&
      index(str,'this').eq.8,              &
      ! return value is counted from the left end even if BACK=.TRUE.
      index(str,'this',back=.true.).eq.24, &
      ! INDEX is case-sensitive
      index(str,'This').eq.0
end program demo_index
```
Expected Results:
```text
   T T T
```
## __Standard__

FORTRAN 77 and later, with KIND argument Fortran 2003
and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX),
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# INT

## __Name__
__int__(3) - \[TYPE:NUMERIC\] Convert to integer type by truncating towards zero

## __Syntax__
```fortran
result = int(a, kind)

 integer(kind=KIND) elemental function int(a,kind)
 TYPE(kind=KIND),intent(in),optional :: a
 integer,optional :: kind
```
## __Description__

Convert to integer type by truncating towards zero.

## __Arguments__

  - __a__
    : Shall be of type _integer_, _real_, or _complex_ or a BOZ-literal-constant.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

    If not present the returned type is that of default integer type.

## __Returns__

returns an _integer_ variable or array applying the following rules:

 __Case__:

 1.  If __a__ is of type _integer_, __int__(a) = a

 2.  If __a__ is of type _real_ and __|a| \< 1, int(a)__ equals __0__. If __|a| \>=
     1__, then __int(a)__ equals the integer whose magnitude does not exceed
     __a__ and whose sign is the same as the sign of __a__.

 3.  If __a__ is of type _complex_, rule 2 is applied to the _real_ part of __a__.

 4.  If _a_ is a boz-literal constant, it is treated as an _integer_
     with the _kind_ specified.

     The interpretation of a bit sequence whose most significant bit is
     __1__ is processor dependent.

The result is undefined if it cannot be represented in the specified integer type.

## __Examples__

Sample program:

```fortran
program demo_int
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer :: i = 42
complex :: z = (-3.7, 1.0)
real :: x=-10.5, y=10.5

   print *, int(x), int(y)

   print *, int(i)

   print *, int(z), int(z,8)
   ! elemental
   print *, int([-10.9,-10.5,-10.3,10.3,10.5,10.9])
   ! note int(3) truncates towards zero

   ! CAUTION:
   ! a number bigger than a default integer can represent
   ! produces an incorrect result and is not required to
   ! be detected by the program.
   x=real(huge(0))+1000.0
   print *, int(x),x
   ! using a larger kind
   print *, int(x,kind=int64),x

   print *, int(&
   & B"111111111111111111111111111111111111111111111111111111111111111",&
   & kind=int64)
   print *, int(O"777777777777777777777",kind=int64)
   print *, int(Z"7FFFFFFFFFFFFFFF",kind=int64)

   ! elemental
   print *
   print *,int([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_int
```
  Results:
```text
            -10   10
             42
             -3  -3
            -10  -10  -10   10   10  10
    -2147483648   2.14748467E+09
     2147484672   2.14748467E+09
     9223372036854775807
     9223372036854775807
     9223372036854775807

    -2          -2          -2          -2          -1
    -1           0           0           0           1
     1           2           2           2           2
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__nint__(3)](NINT),
[__selected_int_kind__(3)](SELECTED_INT_KIND),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# IOR

## __Name__

__ior__(3) - \[BIT:LOGICAL\] Bitwise logical inclusive or

## __Syntax__
```fortran
   result = ior(i, j)
    integer,intent(in) :: i
    integer,intent(in) :: j
```
## __Description__

__ior__ returns the bit-wise Boolean inclusive-__or__ of __i__ and __j__.

## __Arguments__

  - __i__
    : an _integer_ scalar or array.

  - __j__
    : _integer_ scalar or array, of the same kind as __i__.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Examples__

Sample program:

```fortran
program demo_ior
implicit none
integer :: i, j, k
   i=53       ! i=00110101 binary (lowest order byte)
   j=45       ! j=00101101 binary (lowest order byte)
   k=ior(i,j) ! k=00111101 binary (lowest order byte) , k=61 decimal
   write(*,'(i8,1x,b8.8)')i,i,j,j,k,k
end program demo_ior
```

Results:

```
         53 00110101
         45 00101101
         61 00111101
```

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR),
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions
# IPARITY

## __Name__

__iparity__(3) - \[BIT:LOGICAL\] Bitwise exclusive or of array elements

## __Syntax__
```fortran
  result = iparity(array, mask)

   or

  result = iparity(array, dim, mask)
```
## __Description__

Reduces with bitwise _xor_ (exclusive _or_) the elements of __array__ along
dimension __dim__ if the corresponding element in __mask__ is __.true.__.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __"1" to "n"__, where __"n"__ equals the rank of __array__.

  - __mask__
    : (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as __array__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the bitwise _xor_ of all elements in __array__
is returned. Otherwise, an array of rank __n-1__, where __n__ equals the
rank of __array__, and a shape similar to that of __array__ with dimension __dim__
dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_iparity
implicit none
integer, dimension(2) :: a
  a(1) = int(b'00100100')
  a(2) = int(b'01101010')
  print '(b8.8)', iparity(a)
end program demo_iparity
```

Results:

```
   01001110
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__iany__(3)](IANY),
[__iall__(3)](IALL),
[__ieor__(3)](IEOR),
[__parity__(3)](PARITY)

###### fortran-lang intrinsic descriptions
# IS\_CONTIGUOUS

## __Name__

__is\_contiguous__(3) - \[ARRAY INQUIRY\] test if object is contiguous

## __Syntax__
```fortran
result = is_contiguous(a)
```
## __Description__

True if and only if an object is contiguous.

An object is contiguous if it is

   - __(1)__
     an object with the CONTIGUOUS attribute,

   - __(2)__
     a nonpointer whole array that is not assumed-shape,

   - __(3)__
     an assumed-shape array that is argument associated with an array
     that is contiguous,

   - __(4)__
     an array allocated by an ALLOCATE statement,

   - __(5)__
     a pointer associated with a contiguous target, or

   - __(6)__
     a nonzero-sized array section provided that

       - __(a)__
         its base object is contiguous,

       - __(b)__
         it does not have a vector subscript,

       - __(c)__
         the elements of the section, in array element order, are a
         subset of the base object elements that are consecutive in
         array element order,

       - __(d)__
         if the array is of type character and a substring-range
         appears, the substring-range specifies all of the characters
         of the parent-string,

       - __(e)__
         only its final part-ref has nonzero rank, and

       - __(f)__
         it is not the real or imaginary part of an array of type
         complex.

An object is not contiguous if it is an array subobject, and

   - the object has two or more elements,

   - the elements of the object in array element order are not
     consecutive in the elements of the base object,

   - the object is not of type character with length zero, and

   - the object is not of a derived type that has no ultimate
     components other than zero-sized arrays and

   - characters with length zero.

It is processor-dependent whether any other object is contiguous.

## __Arguments__

  - __a__
    : may be of any type. It shall be an array. If it is a pointer it
    shall be associated.

## __Returns__

  - __Result__
    : of type Default logical scalar. The result has the value true if __a__
    is contiguous, and false otherwise.

## __Examples__

Sample program:

```fortran
program demo_is_contiguous
implicit none
intrinsic is_contiguous
real, DIMENSION (1000, 1000), TARGET :: A
real, DIMENSION (:, :), POINTER       :: IN, OUT
   IN => A              ! Associate IN with target A
   OUT => A(1:1000:2,:) ! Associate OUT with subset of target A
   !
   write(*,*)'IN is ',IS_CONTIGUOUS(IN)
   write(*,*)'OUT is ',IS_CONTIGUOUS(OUT)
   !
end program demo_is_contiguous
```
  Results:
```text
    IN is  T
    OUT is  F
```
## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions
# ISHFTC

## __Name__

__ishftc__(3) - \[BIT:SHIFT\] Shift bits circularly

## __Syntax__
```fortran
result = ishftc(i, shift, size)
```
## __Description__

__ishftc__(3) returns a value corresponding to __i__ with the rightmost __size__ bits
shifted circularly __shift__ places; that is, bits shifted out one end are
shifted into the opposite end. A value of __shift__ greater than zero
corresponds to a left shift, a value of zero corresponds to no shift,
and a value less than zero corresponds to a right shift. The absolute
value of __shift__ must be less than __size__. If the __size__ argument is omitted,
it is taken to be equivalent to __bit\_size(i)__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

  - __size__
    : (Optional) The type shall be _integer_; the value must be greater than
    zero and less than or equal to __bit\_size__(i).

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ishft__(3)](ISHFT)

###### fortran-lang intrinsic descriptions
# ISHFT

## __Name__

__ishft__(3) - \[BIT:SHIFT\] Shift bits

## __Syntax__
```fortran
result = ishft(i, shift)
```
## __Description__

__ishft__(3) returns a value corresponding to __i__ with all of the bits shifted
__shift__ places. A value of __shift__ greater than zero corresponds to a left
shift, a value of zero corresponds to no shift, and a value less than
zero corresponds to a right shift. If the absolute value of __shift__ is
greater than __bit\_size(i)__, the value is undefined. Bits shifted out
from the left end or right end are lost; zeros are shifted in from the
opposite end.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ishftc__(3)](ISHFTC)

###### fortran-lang intrinsic descriptions
# IS\_IOSTAT\_END

## __Name__

__is\_iostat\_end__(3) - \[STATE\] Test for end-of-file value

## __Syntax__
```fortran
function is_iostat_end(i)

    logical function   :: is_iostat_end (i) result(yesno)
    integer,intent(in) :: i
```
## __Description__

is\_iostat\_end(3) tests whether a variable (assumed returned as a status
from an I/O statement) has the "end of file" I/O status value.

The function is equivalent to comparing the variable with the
__iostat\_end__ parameter of the intrinsic module __iso\_fortran\_env__.

## __Arguments__

  - __i__
    : An _integer_ status value to test if indicating end of file.

## __Returns__

Returns a _logical_ of the default kind, __.true.__ if __i__ has the value
which indicates an end of file condition for __iostat=__ specifiers, and is
__.false.__ otherwise.

## __Examples__

Sample program:

```fortran
program demo_iostat
implicit none
real               :: value
integer            :: ios
character(len=256) :: message
   write(*,*)'Begin entering numeric values, one per line'
   do
      read(*,*,iostat=ios,iomsg=message)value
      if(ios.eq.0)then
         write(*,*)'VALUE=',value
      elseif( is_iostat_end(ios) ) then
         stop 'end of file. Goodbye!'
      else
         write(*,*)'ERROR:',ios,trim(message)
      endif
      !
   enddo
end program demo_iostat
```

## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# IS\_IOSTAT\_EOR

## __Name__

__is\_iostat\_eor__(3) - \[STATE\] Test for end-of-record value

## __Syntax__
```fortran
result = is_iostat_eor(i)
```
## __Description__

is\_iostat\_eor tests whether an variable has the value of the I/O
status "end of record". The function is equivalent to comparing the
variable with the iostat\_eor parameter of the intrinsic module
__iso\_fortran\_env__.

## __Arguments__

  - __i__
    : Shall be of the type _integer_.

## __Returns__

Returns a _logical_ of the default kind, which .true. if __i__ has the value
which indicates an end of file condition for iostat= specifiers, and is
.false. otherwise.

## __Examples__

Sample program:

```fortran
program demo_is_iostat_eor
implicit none
integer :: stat, i(50)

  open(88, file='test.dat', form='unformatted')
  read(88, iostat=stat) i

  if(is_iostat_eor(stat)) stop 'end of record'

end program demo_is_iostat_eor
```

## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions
# KIND

## __Name__

__kind__(3) - \[KIND INQUIRY\] Kind of an entity

## __Syntax__
```fortran
k = kind(x)
```
## __Description__

__kind(x)__ returns the kind value of the entity __x__.

## __Arguments__

  - __x__
    : Shall be of type _logical_, _integer_, _real_, _complex_ or _character_.

## __Returns__

The return value is a scalar of type _integer_ and of the default integer
kind.

## __Examples__

Sample program:

```fortran
program demo_kind
implicit none
integer,parameter :: kc = kind(' ')
integer,parameter :: kl = kind(.true.)

   print *, "The default character kind is ", kc
   print *, "The default logical kind is ", kl

end program demo_kind
```
  Results:
```text
    The default character kind is            1
    The default logical kind is            4
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# LBOUND

## __Name__

__lbound__(3) - \[ARRAY INQUIRY\] Lower dimension bounds of an array

## __Syntax__
```fortran
result = lbound(array, dim, kind)

   TYPE(kind=KIND) elemental function lbound(array,dim,kind)
   TYPE(kind=KIND),intent(in)  :: array
   integer,optional,intent(in) :: dim
   integer,optional,intent(in) :: kind
```
## __Description__

Returns the lower bounds of an array, or a single lower bound along the
__dim__ dimension.

## __Arguments__

  - __array__
    : Shall be an array, of any type.

  - __dim__
    : Shall be a scalar _integer_.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind. If __dim__ is absent, the
result is an array of the lower bounds of __array__. If __dim__ is present, the
result is a scalar corresponding to the lower bound of the array along
that dimension. If __array__ is an expression rather than a whole array or
array structure component, or if it has a zero extent along the relevant
dimension, the lower bound is taken to be 1.

## __Examples__

Note that in my opinion this function should not be used on assumed-size
arrays or in any function without an explicit interface. Errors can
occur if there is no interface defined.

Sample program

```fortran
! program demo_lbound
module m_bounds
implicit none
 contains
    subroutine msub(arr)
       !!integer,intent(in) :: arr(*)  ! cannot be assumed-size array
       integer,intent(in) :: arr(:)
       write(*,*)'MSUB: LOWER=',lbound(arr), &
       & 'UPPER=',ubound(arr), &
       & 'SIZE=',size(arr)
    end subroutine msub
 end module m_bounds

 use m_bounds, only : msub
 implicit none
 interface
    subroutine esub(arr)
    integer,intent(in) :: arr(:)
    end subroutine esub
 end interface
 integer :: arr(-10:10)
    write(*,*)'MAIN: LOWER=',lbound(arr), &
    & 'UPPER=',ubound(arr), &
    & 'SIZE=',size(arr)
    call csub()
    call msub(arr)
    call esub(arr)
 contains
subroutine csub
   write(*,*)'CSUB: LOWER=',lbound(arr), &
   & 'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine csub
end

 subroutine esub(arr)
 implicit none
 integer,intent(in) :: arr(:)
    ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE
    ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)
    write(*,*)'ESUB: LOWER=',lbound(arr), &
    & 'UPPER=',ubound(arr), &
    & 'SIZE=',size(arr)
 end subroutine esub

!end program demo_lbound
```
Results:

```
   MAIN: LOWER=         -10 UPPER=          10 SIZE=          21
   CSUB: LOWER=         -10 UPPER=          10 SIZE=          21
   MSUB: LOWER=           1 UPPER=          21 SIZE=          21
   ESUB: LOWER=           1 UPPER=          21 SIZE=          21
```

## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003 and later

## __See Also__

[__ubound__(3)](UBOUND),
[__co\_lbound__(3)](CO_LBOUND)

###### fortran-lang intrinsic descriptions
# LEADZ

## __Name__

__leadz__(3) - \[BIT:COUNT\] Number of leading zero bits of an integer

## __Syntax__
```fortran
result = leadz(i)
```
## __Description__

__leadz__ returns the number of leading zero bits of an integer.

## __Arguments__

  - __i__
    : Shall be of type _integer_.

## __Returns__

The type of the return value is the same as a default _integer_. If all
the bits of __i__ are zero, the result value is __bit\_size(i)__.

## __Examples__

Sample program:

```fortran
program demo_leadz
implicit none
integer :: value, i
character(len=80) :: f
  write(*,'(*(g0))')'BIT_SIZE=',bit_size(value)
  ! make a format statement for writing a value as a bit string
  write(f,'("(b",i0,".",i0,")")')bit_size(value),bit_size(value)
  ! show output for various integer values
  value=0
  do i=0,bit_size(value)-1
     write (*,'("LEADING ZERO BITS=",i3,1x)') leadz(value)
     write (*,'(" FOR VALUE ")',advance='no')
     write(*,f,advance='no') value
     write(*,'(*(1x,g0))') "OR",value
     value=value+2**(i)
  enddo
end program demo_leadz
```

Results:

```
   BIT_SIZE=32
   LEADING ZERO BITS= 32
    FOR VALUE 00000000000000000000000000000000 OR 0
   LEADING ZERO BITS= 31
    FOR VALUE 00000000000000000000000000000001 OR 1
   LEADING ZERO BITS= 30
    FOR VALUE 00000000000000000000000000000011 OR 3
   LEADING ZERO BITS= 29
    FOR VALUE 00000000000000000000000000000111 OR 7
   LEADING ZERO BITS= 28
    FOR VALUE 00000000000000000000000000001111 OR 15
   LEADING ZERO BITS= 27
    FOR VALUE 00000000000000000000000000011111 OR 31
   LEADING ZERO BITS= 26
    FOR VALUE 00000000000000000000000000111111 OR 63
   LEADING ZERO BITS= 25
    FOR VALUE 00000000000000000000000001111111 OR 127
   LEADING ZERO BITS= 24
    FOR VALUE 00000000000000000000000011111111 OR 255
   LEADING ZERO BITS= 23
    FOR VALUE 00000000000000000000000111111111 OR 511
   LEADING ZERO BITS= 22
    FOR VALUE 00000000000000000000001111111111 OR 1023
   LEADING ZERO BITS= 21
    FOR VALUE 00000000000000000000011111111111 OR 2047
   LEADING ZERO BITS= 20
    FOR VALUE 00000000000000000000111111111111 OR 4095
   LEADING ZERO BITS= 19
    FOR VALUE 00000000000000000001111111111111 OR 8191
   LEADING ZERO BITS= 18
    FOR VALUE 00000000000000000011111111111111 OR 16383
   LEADING ZERO BITS= 17
    FOR VALUE 00000000000000000111111111111111 OR 32767
   LEADING ZERO BITS= 16
    FOR VALUE 00000000000000001111111111111111 OR 65535
   LEADING ZERO BITS= 15
    FOR VALUE 00000000000000011111111111111111 OR 131071
   LEADING ZERO BITS= 14
    FOR VALUE 00000000000000111111111111111111 OR 262143
   LEADING ZERO BITS= 13
    FOR VALUE 00000000000001111111111111111111 OR 524287
   LEADING ZERO BITS= 12
    FOR VALUE 00000000000011111111111111111111 OR 1048575
   LEADING ZERO BITS= 11
    FOR VALUE 00000000000111111111111111111111 OR 2097151
   LEADING ZERO BITS= 10
    FOR VALUE 00000000001111111111111111111111 OR 4194303
   LEADING ZERO BITS=  9
    FOR VALUE 00000000011111111111111111111111 OR 8388607
   LEADING ZERO BITS=  8
    FOR VALUE 00000000111111111111111111111111 OR 16777215
   LEADING ZERO BITS=  7
    FOR VALUE 00000001111111111111111111111111 OR 33554431
   LEADING ZERO BITS=  6
    FOR VALUE 00000011111111111111111111111111 OR 67108863
   LEADING ZERO BITS=  5
    FOR VALUE 00000111111111111111111111111111 OR 134217727
   LEADING ZERO BITS=  4
    FOR VALUE 00001111111111111111111111111111 OR 268435455
   LEADING ZERO BITS=  3
    FOR VALUE 00011111111111111111111111111111 OR 536870911
   LEADING ZERO BITS=  2
    FOR VALUE 00111111111111111111111111111111 OR 1073741823
   LEADING ZERO BITS=  1
    FOR VALUE 01111111111111111111111111111111 OR 2147483647
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bit\_size__(3)](BIT_SIZE),
[__popcnt__(3)](POPCNT),
[__poppar__(3)](POPPAR),
[__trailz__(3)](TRAILZ)

###### fortran-lang intrinsic descriptions
# LEN

## __Name__

__len__(3) - \[CHARACTER\] Length of a character entity

## __Syntax__
```fortran
   l = len(string, kind)

    integer(kind=KIND) function len(string,kind) result(value)
    character(len=*),intent(in) :: string
    integer,optional,intent(in) :: KIND
    integer(kind=KIND) :: value
```
where the returned value is the same kind as the __KIND__, or of
the default kind if __KIND__ is not specified.

## __Description__

__len(3)__ Returns the length of a _character_ string.

If __string__ is an array, the length of an element of __string__
is returned.

Note that __string__ need not be defined when this intrinsic is invoked,
as only the length (not the content) of __string__ is needed.

## __Arguments__

  - __string__
    : Shall be a scalar or array of type _character_.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind.

## __Standard__

FORTRAN 77 and later; with __kind__ argument - Fortran 2003 and later

## __Examples__

Sample program

```fortran
program demo_len
implicit none
character(len=40) :: string
character(len=:),allocatable :: astring
character(len=:),allocatable :: many_strings(:)
integer :: ii

   ii=len(string)
  write(*,*)'length =',ii

  ! the string length will be constant for the fixed-length variable
  string=' How long is this string? '
  write(*,'(a)')' ',string,repeat('=',len(string))

  ! the allocatable string length will be the length of LHS expression
  astring=' How long is this string? '
  write(*,'(a)')' ',astring,repeat('=',len(astring))

   ! you can also query the length (and other attributes) of a string
   ! using a "type parameter inquiry:" (available since fortran 2018)
   write(*,*)'length from type parameter inquiry=',string%len

   ! a scalar is returned for an array, as all values in a Fortran
   ! character array must be of the same length:

   ! define an allocatable array with a constructor ...
     many_strings = [ character(len=7) :: 'Takata', 'Tanaka', 'Hayashi' ]
   write(*,*)
   write(*,*)'length of ALL elements of array=',len(many_strings)

   call proc_star(' how long? ')

contains

   subroutine proc_star(str)
   character(len=*),intent(in)  :: str
   character(len=:),allocatable :: str2
   ! the length of str can be used in the definitions of variables
   character(len=len(str))      :: str3

      if(allocated(str2))deallocate(str2)
      ! syntax for allocating a scalar string
      allocate(character(len=len(str)) :: str2)

      write(*,*)len(str),len(str2),len(str3)
      ! these are other allowable ways to define str2
      str2=str
      str2=repeat(' ',len(str))

   end subroutine proc_star

end program demo_len
```
Results:
```text
```
## __See Also__

len_trim(3), adjustr(3), trim(3), and adjustl(3) are related routines that
allow you to deal with leading and trailing blanks.

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL),
    [__adjustr__(3)](ADJUSTR),
    [__index__(3)](INDEX),
    [__scan__(3)](SCAN),
    [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT),
    [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# LEN\_TRIM

## __Name__

__len\_trim__(3) - \[CHARACTER:WHITESPACE\] Length of a character entity without trailing blank characters

## __Syntax__
```fortran
   result = len_trim(string, kind)

    integer(kind=KIND) function len_trim(string,KIND) result (value)
    character(len=*),intent(in) :: string
    integer,optional,intent(in) :: KIND
    integer(kind=KIND) :: value
```
## __Description__

Returns the length of a character string, ignoring any trailing blanks.

## __Arguments__

  - __string__
    : The input string whose length is to be measured.
    Shall be a scalar of type _character_

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default _integer_ kind.

## __Examples__

Sample program

```fortran
program demo_len_trim
implicit none
character(len=:),allocatable :: string
   string=' how long is this string?     '
   write(*,*)'LENGTH=',len(string)
   write(*,*)'TRIMMED LENGTH=',len_trim(string)
   !
   ELE:block ! elemental example
   character(len=:),allocatable :: tablet(:)
   tablet=[character(len=256) :: &
   & ' how long is this string?     ',&
   & 'and this one?']
      write(*,*)'LENGTH=            ',len(tablet)
      write(*,*)'TRIMMED LENGTH=    ',len_trim(tablet)
      write(*,*)'SUM TRIMMED LENGTH=',sum(len_trim(tablet))
   endblock ELE
   !
end program demo_len_trim
```
Results:
```
    LENGTH=          30
    TRIMMED LENGTH=          25
    LENGTH=                     256
    TRIMMED LENGTH=              25          13
    SUM TRIMMED LENGTH=          38
```
## __Standard__

Fortran 95 and later, with __kind__ argument - Fortran 2003
and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL),
    [__adjustr__(3)](ADJUSTR),
    [__index__(3)](INDEX),
    [__scan__(3)](SCAN),
    [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__repeat__(3)](REPEAT),
    [__len__(3)](LEN),
    [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# LGE

## __Name__

__lge__(3) - \[CHARACTER:COMPARE\] Lexical greater than or equal

## __Syntax__
```fortran
result = lge(string_a, string_b)
```
## __Description__

Determines whether one string is lexically greater than or equal to
another string, where the two strings are interpreted as containing
ASCII character codes. If the String __a__ and String __b__ are not the same
length, the shorter is compared as if spaces were appended to it to form
a value that has the same length as the longer.

In general, the lexical comparison intrinsics __lge__(3), __lgt__(3), __lle__(3), and __llt__(3)
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

## __Arguments__

  - __string\_a__
    : Shall be of default _character_ type.

  - __string\_b__
    : Shall be of default _character_ type.

## __Returns__

Returns .true. if string\_a \>= string\_b, and .false. otherwise, based
on the ASCII ordering.

## __Standard__

FORTRAN 77 and later

## __See Also__

__\[\[lgt__(3), __\[\[lle__(3), __\[\[llt__(3)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
 [__adjustl__(3)](ADJUSTL),
 [__adjustr__(3)](ADJUSTR),
 [__index__(3)](INDEX),

 [__scan__(3)](SCAN),
 [__verify__(3)](VERIFY)

  - __Nonelemental:__
 [__len\_trim__(3)](LEN_TRIM),
 [__len__(3)](LEN),
 [__repeat__(3)](REPEAT),
 [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# LGT

## __Name__

__lgt__(3) - \[CHARACTER:COMPARE\] Lexical greater than

## __Syntax__
```fortran
result = lgt(string_a, string_b)
```
## __Description__

Determines whether one string is lexically greater than another string,
where the two strings are interpreted as containing ASCII character
codes. If the String __a__ and String __b__ are not the same length, the shorter
is compared as if spaces were appended to it to form a value that has
the same length as the longer.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

## __Arguments__

  - __string\_a__
    : Shall be of default _character_ type.

  - __string\_b__
    : Shall be of default _character_ type.

## __Returns__

Returns .true. if string\_a \> string\_b, and .false. otherwise, based
on the ASCII ordering.

## __Standard__

FORTRAN 77 and later

## __See Also__

[__lge__(3)](LGE),
[__lle__(3)](LLE),
[__llt__(3)](LLT)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- __Elemental:__
[__adjustl__(3)](ADJUSTL),
[__adjustr__(3)](ADJUSTR),
[__index__(3)](INDEX),

[__scan__(3)](SCAN),
[__verify__(3)](VERIFY)

- __Nonelemental:__
[__len\_trim__(3)](LEN_TRIM),
[__len__(3)](LEN),
[__repeat__(3)](REPEAT),
[__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# LLE

## __Name__

__lle__(3) - \[CHARACTER:COMPARE\] Lexical less than or equal

## __Syntax__
```fortran
result = lle(str_a, str_b)

   character(len=*),intent(in) :: str_a, str_b

      or

   character(len=*),intent(in) :: str_a, str_b(*) logical :: result
```
## __Description__

Determines whether one string is lexically less than or equal to another
string, where the two strings are interpreted as containing ASCII
character codes. if the __string\_a__ and __string\_b__ are not the same length,
the shorter is compared as if spaces were appended to it to form a value
that has the same length as the longer. Leading spaces are significant.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

## __Arguments__

  - __str\_a__
    : variable or array of default _character_ type.

  - __str\_b__
    : variable or array of default _character_ type.

    if __str_a__ and __str_b__ are both arrays they must be of the
    same shape.

## __Returns__

  - __result__
    Returns __.true.__ if __STR\_A \<= STR\_B__, and __.false.__ otherwise, based on
    the ASCII ordering.

## __Examples__

Sample program:

```fortran
program demo_lle
implicit none
integer             :: i
   write(*,'(*(a))')(char(i),i=32,126)
     write(*,*) lle('abc','ABC')              ! F lowercase is > uppercase
     write(*,*) lle('abc','abc  ')            ! T trailing spaces
     ! If both strings are of zero length the result is true.
     write(*,*) lle('','')                    ! T
     write(*,*) lle('','a')                   ! T the null string is padded
     write(*,*) lle('a','')                   ! F
     write(*,*) lle('abc',['abc','123'])      ! [T,F] scalar and array
     write(*,*) lle(['cba', '123'],'abc')     ! [F,T]
     write(*,*) lle(['abc','123'],['cba','123']) ! [T,T] both arrays
end program demo_lle
```

Results:

```text
  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
  [\]^_`abcdefghijklmnopqrstuvwxyz{|}~
  F
  T
  T
  T
  F
  T F
  F T
  T T
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__lge__(3)](LGE),
[__lgt__(3),](LGT),
[__llt__(3)](LLT)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

 - __Elemental:__
 [__adjustl__(3)](ADJUSTL),
 [__adjustr__(3)](ADJUSTR),
 [__index__(3)](INDEX),

 [__scan__(3)](SCAN),
 [__verify__(3)](VERIFY)

 - __Nonelemental:__
 [__len\_trim__(3)](LEN_TRIM),
 [__len__(3)](LEN),
 [__repeat__(3)](REPEAT),
 [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# LLT

## __Name__

__llt__(3) - \[CHARACTER:COMPARE\] Lexical less than

## __Syntax__
```fortran
result = llt(string_a, string_b)
```
## __Description__

Determines whether one string is lexically less than another string,
where the two strings are interpreted as containing ASCII character
codes. If the __string\_a__ and __string\_b__ are not the same length, the shorter
is compared as if spaces were appended to it to form a value that has
the same length as the longer.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

## __Arguments__

  - __string\_a__
    : Shall be of default _character_ type.

  - __string\_b__
    : Shall be of default _character_ type.

## __Returns__

Returns .true. if string\_a \<= string\_b, and .false. otherwise, based
on the ASCII ordering.

## __Standard__

FORTRAN 77 and later

## __See Also__

[__lge__(3)](LGE),
[__lgt__(3)](LGT),
[__lle__(3](LLE))

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX),
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# LOG10

## __Name__

__log10__(3) - \[MATHEMATICS\] Base 10 logarithm function

## __Syntax__
```fortran
result = log10(x)

   real(kind=KIND) elemental function log10(x)
   real(kind=KIND),intent(in) :: x
```
## __Description__

__log10(x)__ computes the base 10 logarithm of __x__. This
is generally called the "common logarithm".

## __Arguments__

  - __x__
    : A _real_ value > 0 to take the log of.

## __Returns__

The return value is of type _real_ . The kind type parameter is
the same as __x__.

## __Examples__

Sample program:

```fortran
program demo_log10
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 10.0_real64

   x = log10(x)
   write(*,'(*(g0))')'log10(',x,') is ',log10(x)

   ! elemental
   write(*, *)log10([1.0, 10.0, 100.0, 1000.0, 10000.0, &
                     & 100000.0, 1000000.0, 10000000.0])

end program demo_log10
```
  Results:
```text
   log10(1.0000000000000000) is 0.0000000000000000
      0.00000000       1.00000000       2.00000000       3.00000000
      4.00000000       5.00000000       6.00000000       7.00000000
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# LOG\_GAMMA

## __Name__

__log\_gamma__(3) - \[MATHEMATICS\] Logarithm of the Gamma function

## __Syntax__
```fortran
x = log_gamma(x)
```
## __Description__

__log\_gamma(x)__ computes the natural logarithm of the absolute value of the Gamma function.

## __Arguments__

  - __x__
    : Shall be of type _real_ and neither zero nor a negative integer.

## __Returns__

The return value is of type _real_ of the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_log_gamma
implicit none
real :: x = 1.0
   write(*,*)x,log_gamma(x) ! returns 0.0
end program demo_log_gamma
```
  Results:
```text
      1.00000000       0.00000000
```
## __Standard__

Fortran 2008 and later

## __See Also__

Gamma function: [__gamma__(3)](GAMMA)

###### fortran-lang intrinsic descriptions
# LOGICAL

## __Name__

__logical__(3) - \[TYPE:LOGICAL\] Converts one kind of _logical_ variable to another

## __Syntax__
```fortran
result = logical(l, kind)

 logical(kind=KIND) function logical(L,KIND)
  logical(kind=INK),intent(in) :: L
  integer,intent(in),optional :: KIND
```
## __Description__

Converts one kind of _logical_ variable to another.

## __Arguments__

  - __l__
    : The type shall be _logical_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is a _logical_ value equal to __l__, with a kind
corresponding to __kind__, or of the default logical kind if __kind__ is not
given.

## __Examples__
```fortran
program demo_logical
! Access array containing the kind type parameter values supported by this
! compiler for entities of logical type
use iso_fortran_env, only : logical_kinds

   ! list kind values supported on this platform, which generally vary
   ! in storage size
   do i =1, size(logical_kinds)
      write(*,*)logical_kinds(i)
   enddo

end program demo_logical
```
  Results:
```text
              1
              2
              4
              8
             16
```
## __Standard__

Fortran 95 and later, related ISO_FORTRAN_ENV module - fortran 2009

## __See Also__

[__int__(3)](INT),
[__real__(3)](REAL),
[__cmplx__(3)](CMPLX)

###### fortran-lang intrinsic descriptions
# LOG

## __Name__

__log__(3) - \[MATHEMATICS\] Logarithm function

## __Syntax__
```fortran
result = log(x)
```
## __Description__

__log(x)__ computes the natural logarithm of __x__, i.e. the logarithm to
the base "e".

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value is of type _real_ or _complex_. The kind type parameter is
the same as __x__. If __x__ is _complex_, the imaginary part OMEGA is in the range

__-PI__ \< OMEGA \<= PI.

## __Examples__

Sample program:

```fortran
program demo_log
implicit none
  real(kind(0.0d0)) :: x = 2.71828182845904518d0
  complex :: z = (1.0, 2.0)
  write(*,*)x, log(x)    ! will yield (approximately) 1
  write(*,*)z, log(z)
end program demo_log
```
  Results:
```text
      2.7182818284590451        1.0000000000000000
   (1.00000000,2.00000000) (0.804718971,1.10714877)
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# MASKL

## __Name__

__maskl__(3) - \[BIT:SET\] Generates a left justified mask

## __Syntax__
```fortran
result = maskl(i, kind)

  integer elemental function maskl(i,kind)
  integer,intent(in),optional :: kind
```
## __Description__

__maskl(i\[, *kind*\])__ has its leftmost __i__ bits set to __1__, and the
remaining bits set to __0__.

## __Arguments__

  - __i__
    : Shall be of type _integer_.
      Its value must be non-negative, and less than or equal to the
      number of bits for the kind of the result.

  - __kind__
    : Shall be a scalar constant expression of type _integer_.

## __Returns__

The return value is of type _integer_. If __kind__ is present, it specifies
the kind value of the return type; otherwise, it is of the default
integer kind.

The leftmost __i__ bits are set to 1 and the other bits are set to 0.

## __Examples__
Sample program:
```fortran
program demo_maskl
implicit none
integer :: i
   i=maskl(1)
   write(*,'(i0,1x,b0,/)') i,i
   ! elemental
   write(*,'(*(i11,1x,b0,1x,/))') maskl([(i,i,i=1,bit_size(0))])
end program demo_maskl
```
  Results:
```text
-2147483648 10000000000000000000000000000000

          0 0
-2147483648 10000000000000000000000000000000
-1073741824 11000000000000000000000000000000
 -536870912 11100000000000000000000000000000
 -268435456 11110000000000000000000000000000
 -134217728 11111000000000000000000000000000
  -67108864 11111100000000000000000000000000
  -33554432 11111110000000000000000000000000
  -16777216 11111111000000000000000000000000
   -8388608 11111111100000000000000000000000
   -4194304 11111111110000000000000000000000
   -2097152 11111111111000000000000000000000
   -1048576 11111111111100000000000000000000
    -524288 11111111111110000000000000000000
    -262144 11111111111111000000000000000000
    -131072 11111111111111100000000000000000
     -65536 11111111111111110000000000000000
     -32768 11111111111111111000000000000000
     -16384 11111111111111111100000000000000
      -8192 11111111111111111110000000000000
      -4096 11111111111111111111000000000000
      -2048 11111111111111111111100000000000
      -1024 11111111111111111111110000000000
       -512 11111111111111111111111000000000
       -256 11111111111111111111111100000000
       -128 11111111111111111111111110000000
        -64 11111111111111111111111111000000
        -32 11111111111111111111111111100000
        -16 11111111111111111111111111110000
         -8 11111111111111111111111111111000
         -4 11111111111111111111111111111100
         -2 11111111111111111111111111111110
         -1 11111111111111111111111111111111
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__maskr__(3)](MASKR)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# MASKR

## __Name__

__maskr__(3) - \[BIT:SET\] Generates a right-justified mask

## __Syntax__
```fortran
result = maskr(i, kind)

  integer elemental function maskr(i,kind)
  integer,intent(in),optional :: kind
```
## __Description__

__maskr(i\[, kind\])__ has its rightmost __i__ bits set to 1, and the
remaining bits set to 0.

## __Arguments__

  - __i__
    : Shall be of type _integer_.
      Its value must be non-negative, and less than or equal to the
      number of bits for the kind of the result.

  - __kind__
    : Shall be a scalar constant expression of type _integer_.

## __Returns__

The return value is of type _integer_. If __kind__ is present, it
specifies the kind value of the return type; otherwise, it is of the
default integer kind.

It has its rightmost __i__ bits set to 1, and the remaining bits set to 0.

##  __Example__

Sample program:
```fortrqn
program demo_maskr
implicit none
integer :: i
   i=maskr(1)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-1)
   i=maskr(5)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-5)
   i=maskr(11)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-11)
   ! elemental
   write(*,'(*(i11,1x,b0,1x,/))') maskr([(i,i,i=0,bit_size(0))])
end program demo_maskr
```
Results:
```text
1 1 10000000000000000000000000000000

31 11111 111000000000000000000000000000

2047 11111111111 111000000000000000000000

          0 0
          1 1
          3 11
          7 111
         15 1111
         31 11111
         63 111111
        127 1111111
        255 11111111
        511 111111111
       1023 1111111111
       2047 11111111111
       4095 111111111111
       8191 1111111111111
      16383 11111111111111
      32767 111111111111111
      65535 1111111111111111
     131071 11111111111111111
     262143 111111111111111111
     524287 1111111111111111111
    1048575 11111111111111111111
    2097151 111111111111111111111
    4194303 1111111111111111111111
    8388607 11111111111111111111111
   16777215 111111111111111111111111
   33554431 1111111111111111111111111
   67108863 11111111111111111111111111
  134217727 111111111111111111111111111
  268435455 1111111111111111111111111111
  536870911 11111111111111111111111111111
 1073741823 111111111111111111111111111111
 2147483647 1111111111111111111111111111111
         -1 11111111111111111111111111111111
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__maskl__(3)](MASKL)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# MATMUL

## __Name__

__matmul__(3) - \[TRANSFORMATIONAL\] matrix multiplication

## __Syntax__
```fortran
result = matmul(matrix_a, matrix_b)
```
## __Description__

Performs a matrix multiplication on numeric or logical arguments.

## __Arguments__

  - __matrix\_a__
    : An array of _integer_, _real_, _complex_, or _logical_ type, with a rank of
    one or two.

  - __matrix\_b__
    : An array of _integer_, _real_, or _complex_ type if __matrix\_a__ is of a
    numeric type; otherwise, an array of _logical_ type. The rank shall be
    one or two, and the first (or only) dimension of __matrix\_b__ shall be
    equal to the last (or only) dimension of __matrix\_a__.

## __Returns__

The matrix product of __matrix\_a__ and __matrix\_b__. The type and kind of the
result follow the usual type and kind promotion rules, as for the \* or
.and. operators.

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# MAXEXPONENT

## __Name__

__maxexponent__(3) - \[NUMERIC MODEL\] Maximum exponent of a real kind

## __Syntax__
```fortran
result = maxexponent(x)
```
## __Description__

__maxexponent(x)__ returns the maximum exponent in the model of the type
of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_maxexponent
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x
real(kind=dp) :: y

   print *, minexponent(x), maxexponent(x)
   print *, minexponent(y), maxexponent(y)
end program demo_maxexponent
```
  Results:
```text
           -125         128
          -1021        1024
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# MAXLOC

## __Name__

__maxloc__(3) - \[ARRAY:LOCATION\] Location of the maximum value within an array

## __Syntax__
```fortran
result = maxloc(array, dim, mask) result = maxloc(array, mask)
```
## __Description__

Determines the location of the element in the array with the maximum
value, or, if the __dim__ argument is supplied, determines the locations of
the maximum element along each row of the array in the __dim__ direction. If
__mask__ is present, only the elements for which __mask__ is __.true.__ are
considered. If more than one element in the array has the maximum value,
the location returned is that of the first such element in array element
order. If the array has zero size, or all of the elements of __mask__ are
.false., then the result is an array of zeroes. Similarly, if __dim__ is
supplied and all of the elements of __mask__ along a given row are zero, the
result value for that row is zero.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_, or _character_.

  - __dim__
    : (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of __array__, inclusive. It may not be an optional
    dummy argument.

  - __mask__
    : Shall be an array of type _logical_, and conformable with __array__.

## __Returns__

If __dim__ is absent, the result is a rank-one array with a length equal to
the rank of __array__. If __dim__ is present, the result is an array with a rank
one less than the rank of __array__, and a size corresponding to the size of
__array__ with the __dim__ dimension removed. If __dim__ is present and __array__ has a
rank of one, the result is a scalar. In all cases, the result is of
default _integer_ type.

The value returned is reference to the offset from the beginning of the
array, not necessarily the subscript value if the array subscripts do
not start with one.

## __Examples__

sample program

```fortran
program demo_maxloc
implicit none
integer      :: ii
integer,save :: i(-3:3)=[(abs(abs(ii)-50),ii=-3,3)]
integer,save :: ints(3,5)= reshape([&
   1,  2,  3,  4,  5, &
   10, 20, 30, 40, 50, &
   11, 22, 33, 44, 55  &
],shape(ints),order=[2,1])

    write(*,*) maxloc(ints)
    write(*,*) maxloc(ints,dim=1)
    write(*,*) maxloc(ints,dim=2)
    ! when array bounds do not start with one remember MAXLOC(3) returns the
    ! offset relative to the lower bound-1 of the location of the maximum
    ! value, not the subscript of the maximum value. When the lower bound of
    ! the array is one, these values are the same. In other words, MAXLOC(3)
    ! returns the subscript of the value assuming the first subscript of the
    ! array is one no matter what the lower bound of the subscript actually
    ! is.
    write(*,'(g0,1x,g0)') (ii,i(ii),ii=lbound(i,dim=1),ubound(i,dim=1))
    write(*,*)maxloc(i)

end program demo_maxloc
```
Results:
```text
      3       5
      3       3       3       3       3
      5       5       5
   -3 47
   -2 48
   -1 49
   0 50
   1 49
   2 48
   3 47
```
## __Standard__

Fortran 95 and later

## __See Also__

[__max__(3)](MAX),
[__maxval__(3)](MAXVAL)

###### fortran-lang intrinsic descriptions
# MAX

## __Name__

__max__(3) - \[NUMERIC\] Maximum value of an argument list

## __Syntax__
```fortran
result = max(a1, a2, a3, ...)
```
## __Description__

Returns the argument with the largest (most positive) value.

## __Arguments__

  - __a1__
    : The type shall be _integer_ or _real_.

  - __a2,a3,...__
    : An expression of the same type and kind as __a1__.

## __Returns__

The return value corresponds to the maximum value among the arguments,
and has the same type and kind as the first argument.

The function is both elemental and allows for an arbitrary number of
arguments. This means if some elements are scalar and some are arrays
that all the arrays must be of the same size, and the returned value
will be an array that is the result as if multiple calls were made with
all scalar values with a single element of each array used in each call.
If called with all arrays the returned array is the same as if multiple
calls were made with __max(arr1(1),arr2(1), ...)__ to
__max(arr1(N),arr2(N))__.

## __Examples__

Sample program

```fortran
program demo_max
implicit none
real :: arr1(4)= [10.0,11.0,30.0,-100.0]
real :: arr2(5)= [20.0,21.0,32.0,-200.0,2200.0]

  !! this is simple enough because it is not being called elementally
  !! because all arguments are scalar
  !!

  write(*,*)'scalars:',max(10.0,11.0,30.0,-100.0)

  !!
  !! this is all max(3) could do before it became an elemental
  !! function and is the most intuitive
  !! except that it can take an arbitrary number of options,
  !! which is not common in Fortran without
  !! declaring a lot of optional parameters.
  !!
  !! That is it unless you want to use the elemental features of max(3)!

  !! Error: Intrinsic    max    at (1) must have at least two arguments
  !!write(*,*)max(arr1)
  !! This does not work because it is like trying to return
  !! [(max(arr1(i)),i=1,size(arr1))]
  !! so it is trying to take the max of a single value.
  !! To find the largest element of an array
  !! call maxloc(3) or maxval(3).

  !! Error: Different shape for arguments 'a1' and 'a2' for intrinsic
  !! 'max' at (1) on dimension 1 (4 and 5)
  !!write(*,*)max(arr1,arr2)
  !! but this will return an array of
  !! [(max(arr1(N),arr2(N),N=1,size(arr1))]

  write(*,*)max(arr1,arr2(1:4))

  !! so this works only if all the arrays are the same size and
  !! you want an array of the largest Nth elements
  !! from the input arrays.
  !! maybe you wanted to do maxval([arr1,arr2]) or
  !! equivalently max(maxval(arr1),maxval(arr2))
  !! to find the single largest element in both arrays?

  !! compares all scalars to each member of array and
  !! returns array of size arr2

  write(*,*)'scalars and array:',max(10.0,11.0,30.0,-100.0,arr2)

  !! Error: Different shape for arguments 'a5' and 'a6'
  !! for intrinsic 'max' at (1) on dimension 1 (5 and 4)
  !! write(*,*)'scalars and array:',max(10.0,11.0,30.0,-100.0,arr2,arr1)
  !! as the same reason above when arrays are used
  !! (without scalar values) all the arrays must be the same size

  write(*,*)'scalars and array:',&
  & max(40.0,11.0,30.0,-100.0,arr2(:4),arr1)
end program demo_max
```
Results:

```text
    scalars:   30.000000
      20.0000000  21.000000  32.000000 -100.00000
    scalars and array: 30.000000 30.000000 32.000000 30.000000 2200.0000
    scalars and array: 40.000000 40.000000 40.000000 40.000000
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__maxloc__(3)](MAXLOC),
[__maxval__(3)](MAXVAL),
[__min__(3)](MIN)

###### fortran-lang intrinsic descriptions
# MAXVAL

## __Name__

__maxval__(3) - \[ARRAY REDUCTION\] determines the maximum value in an array or row

## __Syntax__
```fortran
result = maxval(array, dim, mask)
```
   or
```fortran
result = maxval(array, mask)
```
## __Description__

Determines the maximum value of the elements in an array value, or, if
the __dim__ argument is supplied, determines the maximum value along each
row of the array in the __dim__ direction. If __mask__ is present, only the
elements for which __mask__ is __.true.__ are considered. If the array has zero
size, or all of the elements of __mask__ are .false., then the result is the
most negative number of the type and kind of __array__ if __array__ is numeric,
or a string of nulls if __array__ is of character type.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_, or _character_.

  - __dim__
    : (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of __array__, inclusive. It may not be an optional
    dummy argument.

  - __mask__
    : (Optional) Shall be an array of type _logical_, and conformable with
    __array__.

## __Returns__

If __dim__ is absent, or if __array__ has a rank of one, the result is a scalar.
If __dim__ is present, the result is an array with a rank one less than the
rank of __array__, and a size corresponding to the size of __array__ with the
__dim__ dimension removed. In all cases, the result is of the same type and
kind as __array__.

## __Examples__

sample program:

```fortran
program demo_maxval
implicit none
integer,save :: ints(3,5)= reshape([&
   1,  2,  3,  4,  5, &
  10, 20, 30, 40, 50, &
  11, 22, 33, 44, 55  &
],shape(ints),order=[2,1])

   write(*,*) maxval(ints)
   write(*,*) maxval(ints,dim=1)
   write(*,*) maxval(ints,dim=2)
   ! find biggest number less than 30 with mask
   write(*,*) maxval(ints,mask=ints.lt.30)
end program demo_maxval
```

Results:

```
   55
   11     22     33     44     55
    5     50     55
   22
```

## __Standard__

Fortran 95 and later

## __See Also__

[__max__(3)](MAX),
[__maxloc__(3)](MAXLOC)

###### fortran-lang intrinsic descriptions
# MERGE\_BITS

## __Name__

__merge\_bits__(3) - \[BIT:COPY\] Merge bits using a mask

## __Syntax__
```fortran
result = merge_bits(i, j, mask)

    elemental function merge_bits(i,j,mask) result(r)
    integer(kind=KIND) ,intent(in) :: i, j, mask
    integer(kind=KIND) :: r
```
where the result and all input values have the same _integer_ type and
KIND with the exception that the mask and either __i__ or __j__ may be
a BOZ constant.

## __Description__

A common graphics operation in Ternary Raster Operations is to combine
bits from two different sources, generally referred to as bit-blending.
__merge\_bits__ performs a masked bit-blend of __i__ and __j__ using
the bits of the __mask__ value to determine which of the input values
to copy bits from.

Specifically, The k-th bit of the result is equal to the k-th bit of
__i__ if the k-th bit of __mask__ is __1__; it is equal to the k-th bit
of __j__ otherwise (so all three input values must have the same number
of bits).

The resulting value is the same as would result from

    __ior (iand (i, mask),iand (j, not (mask)))__

An exception to all values being of the same _integer_ type is that __i__
or __j__ and/or the mask may be a BOZ constant (A BOZ constant means it is
either a Binary, Octal, or Hexadecimal literal constant).  The BOZ values
are converted to the _integer_ type of the non-BOZ value(s) as if called
by the intrinsic function __int()__ with the kind of the non-BOZ value(s),
so the BOZ values must be in the range of the type of the result.

## __Arguments__

  - __i__
  : value to select bits from when the associated bit in the mask is __1__.

  - __j__
  : value to select bits from when the associated bit in the mask is __0__.

  - __mask__
  : a value whose bits are used as a mask to select bits from __i__ and __j__

## __Returns__

The bits blended from __i__ and __j__ using the mask __mask__. It is the
same type as __i__ if __i__ is of type _integer_, otherwise the same type
as __j__.

## __Example__
```fortran
program demo_merge_bits
use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
implicit none
integer(kind=int16) :: if_one,if_zero,msk
character(len=*),parameter :: fmt='(*(g0, 1X))'

   ! basic usage
   print *,'MERGE_BITS( 5,10,41) should be 3.=>',merge_bits(5,10,41)
   print *,'MERGE_BITS(13,18,22) should be 4.=>',merge_bits(13,18,22)

   ! use some values in base2 illustratively:
   if_one =int(b'1010101010101010',kind=int16)
   if_zero=int(b'0101010101010101',kind=int16)

   msk=int(b'0101010101010101',kind=int16)
   print '("should get all zero bits =>",b16.16)', &
   & merge_bits(if_one,if_zero,msk)

   msk=int(b'1010101010101010',kind=int16)
   print '("should get all ones bits =>",b16.16)', &
   & merge_bits(if_one,if_zero,msk)

   ! using BOZ values
   print fmt, &
   & merge_bits(32767_int16,    o'12345',         32767_int16), &
   & merge_bits(o'12345'   , 32767_int16, b'0000000000010101'), &
   & merge_bits(32767_int16,    o'12345',             z'1234')

   ! a do-it-yourself equivalent for comparison and validation
   print fmt, &
   & ior(iand(32767_int16, 32767_int16),                   &
   &   iand(o'12345', not(32767_int16))),                  &

   & ior(iand(o'12345', int(o'12345', kind=int16)),        &
   &   iand(32767_int16, not(int(o'12345', kind=int16)))), &

   & ior(iand(32767_int16, z'1234'),                       &
   &   iand(o'12345', not(int( z'1234', kind=int16))))

end program demo_merge_bits
```
  Results:
```text
    MERGE_BITS( 5,10,41) should be 3.=>           3
    MERGE_BITS(13,18,22) should be 4.=>           4
   should get all zero bits =>0000000000000000
   should get all ones bits =>1111111111111111
   32767 32751 5877
   32767 32767 5877
```

## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions (License: MIT) @urbanjost
# MERGE

## __Name__

__merge__(3) - \[ARRAY CONSTRUCTION\] Merge variables

## __Syntax__
```fortran
result = merge(tsource, fsource, mask)
```
## __Description__

The elemental function __merge__(3) selects values from two arrays or
scalars according to a logical mask. The result is equal to an element
of __tsource__ where the corresponding element of __mask__ is __.true.__, or an
element of __fsource__ when it is .false. .

Multi-dimensional arrays are supported.

Note that argument expressions to __merge__(3) are not required to be
short-circuited so (as an example) if the array __x__ contains zero values
in the statement below the standard does not prevent floating point
divide by zero being generated; as __1.0/x__ may be evaluated for all values
of __x__ before the mask is used to select which value to retain:

```fortran
      y = merge( 1.0/x, 0.0, x /= 0.0 )
```

Note the compiler is also free to short-circuit or to generate an
infinity so this may work in many programming environments but is not
recommended.

For cases like this one may instead use masked assignment via the __where__
construct:

```fortran
      where(x .ne. 0.0)
         y = 1.0/x
      elsewhere
         y = 0.0
      endwhere
```

instead of the more obscure

```fortran
      merge(1.0/merge(x,1.0,x /= 0.0), 0.0, x /= 0.0)
```

## __Arguments__

  - __tsource__
    : May be of any type, including user-defined.

  - __fsource__
    : Shall be of the same type and type parameters as __tsource__.

  - __mask__
    : Shall be of type _logical_.

Note that (currently) _character_ values must be of the same length.

## __Returns__

The result is of the same type and type parameters as __tsource__. For any
element the result is __tsource__ if __mask__ is true and __fsource__ otherwise.

## __Examples__

The value of

```fortran
     merge (1.0, 0.0, k > 0)
```

is 1.0 for K=5 and 0.0 for K=__-2__.

```fortran
program demo_merge
implicit none
integer :: tvals(2,3), fvals(2,3), answer(2,3)
logical :: mask(2,3)
integer :: i
logical :: chooseleft

   tvals(1,:)=[  10, -60,  50 ]
   tvals(2,:)=[ -20,  40, -60 ]

   fvals(1,:)=[ 0, 3, 2 ]
   fvals(2,:)=[ 7, 4, 8 ]

   mask(1,:)=[ .true.,  .false., .true. ]
   mask(2,:)=[ .false., .false., .true. ]

   write(*,*)'mask of logicals'
   answer=merge( tvals, fvals, mask )
   call printme()

   write(*, *)'highest values'
   answer=merge( tvals, fvals, tvals > fvals )
   call printme()

   write(*, *)'lowest values'
   answer=merge( tvals, fvals, tvals < fvals )
   call printme()

   write(*, *)'zero out negative values'
   answer=merge( tvals, 0, tvals < 0)
   call printme()

   write(*, *)'binary choice'
   chooseleft=.false.
   write(*, '(3i4)')merge([1,2,3],[10,20,30],chooseleft)
   chooseleft=.true.
   write(*, '(3i4)')merge([1,2,3],[10,20,30],chooseleft)

contains

subroutine printme()
      write(*, '(3i4)')(answer(i, :), i=1, size(answer, dim=1))
end subroutine printme

end program demo_merge
```
Expected Results:
```
    mask of logicals
     10   3  50
      7   4 -60
    highest values
     10   3  50
      7  40   8
    lowest values
      0 -60   2
    -20   4 -60
    zero out negative values
      0 -60   0
    -20   0 -60
    binary choice
     10  20  30
      1   2   3
```

## __Standard__

Fortran 95 and later

## __See Also__

[__pack__(3)](PACK),
[__unpack__(3)](UNPACK),
[__pack__(3)](PACK),
[__spread__(3)](SPREAD),
[__unpack__(3)](UNPACK)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# MINEXPONENT

## __Name__

__minexponent__(3) - \[NUMERIC MODEL\] Minimum exponent of a real kind

## __Syntax__
```fortran
result = minexponent(x)
```
## __Description__

__minexponent(x)__ returns the minimum exponent in the model of the type
of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_minexponent
use, intrinsic :: iso_fortran_env, only : &
 &real_kinds, real32, real64, real128
implicit none
real(kind=real32) :: x
real(kind=real64) :: y
    print *, minexponent(x), maxexponent(x)
    print *, minexponent(y), maxexponent(y)
end program demo_minexponent
```
Expected Results:
```
        -125         128
       -1021        1024
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# MINLOC

## __Name__

__minloc__(3) - \[ARRAY:LOCATION\] Location of the minimum value within an array

## __Syntax__
```fortran
    result = minloc(array, dim, mask)
```
   or
```fortran
    result = minloc(array, mask)
```
## __Description__

Determines the location of the element in the array with the minimum
value, or, if the __dim__ argument is supplied, determines the locations of
the minimum element along each row of the array in the __dim__ direction. If
__mask__ is present, only the elements for which __mask__ is __.true.__ are
considered. If more than one element in the array has the minimum value,
the location returned is that of the first such element in array element
order. If the array has zero size, or all of the elements of __mask__ are
.false., then the result is an array of zeroes. Similarly, if __dim__ is
supplied and all of the elements of __mask__ along a given row are zero, the
result value for that row is zero.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_, or _character_.

  - __dim__
    : (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of __array__, inclusive. It may not be an optional
    dummy argument.

  - __mask__
    : Shall be an array of type _logical_, and conformable with __array__.

## __Returns__

If __dim__ is absent, the result is a rank-one array with a length equal to
the rank of __array__. If __dim__ is present, the result is an array with a rank
one less than the rank of __array__, and a size corresponding to the size of
__array__ with the __dim__ dimension removed. If __dim__ is present and __array__ has a
rank of one, the result is a scalar. In all cases, the result is of
default _integer_ type.

## __Examples__

sample program:

```fortran
program demo_minloc
implicit none
integer,save :: ints(3,5)= reshape([&
   4, 10,  1,  7, 13, &
   9, 15,  6, 12,  3, &
  14,  5, 11,  2,  8  &
],shape(ints),order=[2,1])
    write(*,*) minloc(ints)
    write(*,*) minloc(ints,dim=1)
    write(*,*) minloc(ints,dim=2)
    ! where in each column is the smallest number .gt. 10 ?
    write(*,*) minloc(ints,dim=2,mask=ints.gt.10)
    ! a one-dimensional array with dim=1 explicitly listed returns a scalar
    write(*,*) minloc(pack(ints,.true.),dim=1) ! scalar
end program demo_minloc
```
Results:

```text
         1       3
         1       3       1       3       2
         3       5       4
         5       4       3
         7
```
## __Standard__

Fortran 95 and later

## __See Also__

[__min__(3)](MIN),
[__minval__(3)](MINVAL)

###### fortran-lang intrinsic descriptions
# MIN

## __Name__

__min__(3) - \[NUMERIC\] Minimum value of an argument list

## __Syntax__
```fortran
result = min(a1, a2, a3, ... )
```
## __Description__

Returns the argument with the smallest (most negative) value.

## __Arguments__

  - __a1__
    : The type shall be _integer_ or _real_.

  - __a2, a3, \`\`\`__
    : An expression of the same type and kind as __A1__.

## __Returns__

The return value corresponds to the minimum value among the arguments,
and has the same type and kind as the first argument.

## __Examples__

Sample program

```fortran
program demo_min
implicit none
    write(*,*)min(10.0,11.0,30.0,-100.0)
end program demo_min
```

Results:

```
      -100.0000000
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__max__(3)](MAX),
[__minloc__(3)](MINLOC),
[__minval__(3)](MINVAL)

###### fortran-lang intrinsic descriptions
# MINVAL

## __Name__

__minval__(3) - \[ARRAY REDUCTION\] Minimum value of an array

## __Syntax__
```fortran
result = minval(array, dim, mask) result = minval(array, mask)
```
## __Description__

Determines the minimum value of the elements in an array value, or, if
the __dim__ argument is supplied, determines the minimum value along each
row of the array in the __dim__ direction.

If __mask__ is present, only the
elements for which __mask__ is __.true.__ are considered.

If the array has zero size, or all of the elements of __mask__ are
.false., then the result is __huge(array)__ if __array__ is numeric, or a
string of __char(len=255)__ characters if __array__ is of character type.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_, or _character_.

  - __dim__
    : (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of ARRAY, inclusive. It may not be an optional
    dummy argument.

  - __mask__
    : Shall be an array of type _logical_, and conformable with __array__.

## __Returns__

If __dim__ is absent, or if __array__ has a rank of one, the result is a scalar.

If __dim__ is present, the result is an array with a rank one less than the
rank of __array__, and a size corresponding to the size of __array__ with the
__dim__ dimension removed. In all cases, the result is of the same type and
kind as __array__.

## __Examples__

sample program:

```fortran
program demo_minval
implicit none
integer :: i
character(len=*),parameter :: g='(3x,*(g0,1x))'

integer,save :: ints(3,5)= reshape([&
       1,  -2,   3,   4,   5,  &
      10,  20, -30,  40,  50,  &
      11,  22,  33, -44,  55  &
],shape(ints),order=[2,1])

integer,save :: box(3,5,2)

   box(:,:,1)=ints
   box(:,:,2)=-ints

   write(*,*)'Given the array'
   write(*,'(1x,*(g4.4,1x))') &
   & (ints(i,:),new_line('a'),i=1,size(ints,dim=1))

   write(*,*)'What is the smallest element in the array?'
   write(*,g) minval(ints),'at <',minloc(ints),'>'

   write(*,*)'What is the smallest element in each column?'
   write(*,g) minval(ints,dim=1)

   write(*,*)'What is the smallest element in each row?'
   write(*,g) minval(ints,dim=2)

   ! notice the shape of the output has less columns
   ! than the input in this case
   write(*,*)'What is the smallest element in each column,'
   write(*,*)'considering only those elements that are'
   write(*,*)'greater than zero?'
   write(*,g) minval(ints, dim=1, mask = ints > 0)

   write(*,*)&
   & 'if everything is false a zero-sized array is NOT returned'
   write(*,*) minval(ints, dim=1, mask = .false.)
   write(*,*)'even for a zero-sized input'
   write(*,g) minval([integer ::], dim=1, mask = .false.)

   write(*,*)'a scalar answer for everything false is huge()'
   write(*,g) minval(ints, mask = .false.)
   write(*,g) minval([integer ::], mask = .false.)

   write(*,*)'some calls with three dimensions'
   write(*,g) minval(box, mask = .true. )
   write(*,g) minval(box, dim=1, mask = .true. )

   write(*,g) minval(box, dim=2, mask = .true. )
   write(*,g) 'shape of answer is ', &
   & shape(minval(box, dim=2, mask = .true. ))

end program demo_minval
```
Results:
```text
 Given the array
    1   -2    3    4    5
   10   20  -30   40   50
   11   22   33  -44   55

 What is the smallest element in the array?
   -44 at < 3 4 >
 What is the smallest element in each column?
   1 -2 -30 -44 5
 What is the smallest element in each row?
   -2 -30 -44
 What is the smallest element in each column,
 considering only those elements that are
 greater than zero?
   1 20 3 4 5
 if everything is false a zero-sized array is NOT returned
  2147483647  2147483647  2147483647  2147483647  2147483647
 even for a zero-sized input
   2147483647
 a scalar answer for everything false is huge()
   2147483647
   2147483647
 some calls with three dimensions
   -55
   1 -2 -30 -44 5 -11 -22 -33 -40 -55
   -2 -30 -44 -5 -50 -55
   shape of answer is  3 2
```

## __Standard__

Fortran 95 and later

## __See Also__

[__min__(3)](MIN),
[__minloc__(3)](MINLOC)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# MOD

## __Name__

__mod__(3) - \[NUMERIC\] Remainder function

## __Syntax__
```fortran
result = mod(a, p)
```
## __Description__

__mod__(a,p) computes the remainder of the division of __a__ by __p__.

## __Arguments__

  - __a__
    : Shall be a scalar of type _integer_ or _real_.

  - __p__
    : Shall be a scalar of the same type and kind as __a__ and not equal to
    zero.

## __Returns__

The return value is the result of __a - (int(a/p) \* p)__. The type and kind
of the return value is the same as that of the arguments. The returned
value has the same sign as __a__ and a magnitude less than the magnitude of
__p__.

## __Examples__

Sample program:

```fortran
program demo_mod
implicit none
     print *, mod(17,3)           ! yields 2
     print *, mod(17.5,5.5)       ! yields 1.0
     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0
     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0

     print *, mod(-17,3)          ! yields -2
     print *, mod(-17.5,5.5)      ! yields -1.0
     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0
     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0

     print *, mod(17,-3)          ! yields 2
     print *, mod(17.5,-5.5)      ! yields 1.0
     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
end program demo_mod
```
  Results:
```text
              2
      1.00000000
      1.0000000000000000
      1.0000000000000000
             -2
     -1.00000000
     -1.0000000000000000
     -1.0000000000000000
              2
      1.00000000
      1.0000000000000000
      1.0000000000000000
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__modulo__(3)](MODULO)

###### fortran-lang intrinsic descriptions
# MODULO

## __Name__

__modulo__(3) - \[NUMERIC\] Modulo function

## __Syntax__
```fortran
result = modulo(a, p)
```
## __Description__

__modulo(a,p)__ computes the __a__ modulo __p__.

## __Arguments__

  - __a__
    : Shall be a scalar of type _integer_ or _real_.

  - __p__
    : Shall be a scalar of the same type and kind as __a__. It shall not be
      zero.

## __Returns__

The type and kind of the result are those of the arguments.

  - If __a__ and __p__ are of type _integer_: __modulo(a,p)__ has the value of
    __a - floor (real(a) / real(p)) \* p__.

  - If __a__ and __p__ are of type _real_: __modulo(a,p)__ has the value of
    __a - floor (a / p) \* p__.

The returned value has the same sign as __p__ and a magnitude less than the
magnitude of __p__.

## __Examples__

Sample program:

```fortran
program demo_modulo
implicit none
     print *, modulo(17,3)        ! yields 2
     print *, modulo(17.5,5.5)    ! yields 1.0

     print *, modulo(-17,3)       ! yields 1
     print *, modulo(-17.5,5.5)   ! yields 4.5

     print *, modulo(17,-3)       ! yields -1
     print *, modulo(17.5,-5.5)   ! yields -4.5
end program demo_modulo
```
  Results:
```text
              2
      1.00000000
              1
      4.50000000
             -1
     -4.50000000
```
## __Standard__

Fortran 95 and later

## __See Also__

[__mod__(3)](MOD)

###### fortran-lang intrinsic descriptions
# MOVE\_ALLOC

## __Name__

__move\_alloc__(3) - \[\] Move allocation from one object to another

## __Syntax__
```fortran
call move_alloc(src, dest)
```
## __Description__

__move\_alloc(src, dest)__ moves the allocation from SRC to DEST. SRC
will become deallocated in the process.

## __Arguments__

  - __src__
    : allocatable, __intent(inout)__, may be of any type and kind.

  - __dest__
    : allocatable, __intent(out)__, shall be of the same type, kind and
    rank as SRC.

## __Examples__

Basic Sample program to allocate a bigger grid

```fortran
program demo_move_alloc
implicit none
! Example to allocate a bigger GRID
real, allocatable :: grid(:), tempgrid(:)
integer :: n, i

   ! initialize small GRID
   n = 3
   allocate (grid(1:n))
   grid = [ (real (i), i=1,n) ]

   ! initialize TEMPGRID which will be used to replace GRID
   allocate (tempgrid(1:2*n))    ! Allocate bigger grid
   tempgrid(::2)  = grid         ! Distribute values to new locations
   tempgrid(2::2) = grid + 0.5   ! initialize other values

   ! move TEMPGRID to GRID
   call MOVE_ALLOC (from=tempgrid, to=grid)

   ! TEMPGRID should no longer be allocated
   ! and GRID should be the size TEMPGRID was
   if (size (grid) /= 2*n .or. allocated (tempgrid)) then
      print *, "Failure in move_alloc!"
   endif
   print *, allocated(grid), allocated(tempgrid)
   print '(99f8.3)', grid
end program demo_move_alloc
```
  Results:
```text
    T F
      1.000   1.500   2.000   2.500   3.000   3.500
```
## __Standard__

Fortran 2003 and later

## __See Also__

[__allocated__(3)](ALLOCATED)

###### fortran-lang intrinsic descriptions
# MVBITS

## __Name__

__mvbits__(3) - \[BIT:COPY\] reproduce bit patterns found in one integer in another

## __Syntax__
```fortran
call mvbits(from, frompos, len, to, topos)
```
## __Description__

__mvbits(3f)__ copies a bit pattern found in a range of adjacent bits in
the _integer_ __from__ to a specified position in another integer __to__
(which is of the same kind as __from__).  It otherwise leaves the bits
in __to__ as-is.

The bit positions copied must exist within the value of __from__.
That is, the values of __frompos+len-1__ and __topos+len-1__ must be
nonnegative and less than __bit\_size__(from).

The bits are numbered __0__ to __bit_size(i)-1__, from right to left.

## __Arguments__

  - __from__
    : An _integer_ to read bits from.
  - __frompos__
    : __frompos__ is the position of the first bit to copy. It is a
    nonnegative _integer_ value < __bit_size(from)__.
  - __len__
    : A nonnegative _integer_ value that indicates how many bits to
    copy from __from__. It must not specify copying bits past the end
    of __from__. That is, __frompos + len__ must be less than or equal
    to __bit_size(from)__.
  - __to__
    : The _integer_ variable to place the copied bits into. It must
    be of the same kind as __from__ and may even be the same variable
    as __from__.

    __to__
    : is set by copying the sequence of bits of length __len__,
    starting at position __frompos__ of __from__ to position __topos__ of
    __to__. No other bits of __to__ are altered. On return, the __len__
    bits of __to__ starting at __topos__ are equal to the value that
    the __len__ bits of __from__ starting at __frompos__ had on entry.

  - __topos__
    : A nonnegative _integer_ value indicating the starting location in
    __to__ to place the specified copy of bits from __from__.
    __topos + len__ must be less than or equal to __bit_size(to)__.

## __Example__
  Sample program that populates a new 32-bit integer with its bytes in
  reverse order (ie. changes the Endian of the integer).

      program demo_mvbits
      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
      implicit none
      integer(kind=int32) :: intfrom, intto, abcd_int
      character(len=*),parameter :: bits= '(g0,t30,b32.32)'
      character(len=*),parameter :: fmt= '(g0,t30,a,t40,b32.32)'

         intfrom=huge(0)  ! all bits are 1 accept the sign bit
         intto=0          ! all bits are 0

         !! CHANGE BIT 0
         ! show the value and bit pattern
         write(*,bits)intfrom,intfrom
         write(*,bits)intto,intto

         ! copy bit 0 from intfrom to intto to show the rightmost bit changes
         !          (from,    frompos, len,    to, topos)
         call mvbits(intfrom,       0,   1, intto,     0) ! change bit 0
         write(*,bits)intto,intto

         !! COPY PART OF A VALUE TO ITSELF
         call mvbits(intfrom,0,1,intfrom,31) ! can copy bit from a value to itself
         write(*,bits)intfrom,intfrom

         !! MOVING BYTES AT A TIME
         ! make native integer value with bit patterns
         ! that happen to be the same as the beginning of the alphabet
         ! to make it easy to see the bytes are reversed
         abcd_int=transfer('abcd',0)
         ! show the value and bit pattern
         write(*,*)'native'
         write(*,fmt)abcd_int,abcd_int,abcd_int

         ! change endian of the value
         abcd_int=int_swap32(abcd_int)
         ! show the values and their bit pattern
         write(*,*)'non-native'
         write(*,fmt)abcd_int,abcd_int,abcd_int

      contains

      pure elemental function int_swap32(intin) result(intout)
      ! Convert a 32 bit integer from big Endian to little Endian,
      ! or conversely from little Endian to big Endian.
      !
      integer(kind=int32), intent(in)  :: intin
      integer(kind=int32) :: intout
         ! copy bytes from input value to new position in output value
         !          (from,  frompos, len,     to, topos)
         call mvbits(intin,       0,   8, intout,    24) ! byte1 to byte4
         call mvbits(intin,       8,   8, intout,    16) ! byte2 to byte3
         call mvbits(intin,      16,   8, intout,     8) ! byte3 to byte2
         call mvbits(intin,      24,   8, intout,     0) ! byte4 to byte1
      end function int_swap32

      end program demo_mvbits
```
  Results:
```text

   2147483647                   01111111111111111111111111111111
   0                            00000000000000000000000000000000
   1                            00000000000000000000000000000001
   -1                           11111111111111111111111111111111
    native
   1684234849                   abcd      01100100011000110110001001100001
    non-native
   1633837924                   dcba      01100001011000100110001101100100
================================================================================
```
## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR),
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR)

###### fortran-lang intrinsic descriptions (License: MIT) @urbanjost

# NEAREST

## __Name__

__nearest__(3) - \[MODEL\_COMPONENTS\] Nearest representable number

## __Syntax__
```fortran
result = nearest(x, s)
```
## __Description__

__nearest(x, s)__ returns the processor-representable number nearest to
__x__ in the direction indicated by the sign of __s__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

  - __s__
    : Shall be of type _real_ and not equal to zero.

## __Returns__

The return value is of the same type as __x__. If __s__ is positive, __nearest__
returns the processor-representable number greater than __x__ and nearest to
it. If __s__ is negative, __nearest__ returns the processor-representable number
smaller than __x__ and nearest to it.

## __Examples__

Sample program:

```fortran
program demo_nearest
implicit none

   real :: x, y
   x = nearest(42.0, 1.0)
   y = nearest(42.0, -1.0)
   write (*,"(3(g20.15))") x, y, x - y

!  write (*,"(3(g20.15))") &
!   nearest(tiny(0.0),1.0), &
!   nearest(tiny(0.0),-1.0), &
!   nearest(tiny(0.0),1.0) -nearest(tiny(0.0),-1.0)

!  write (*,"(3(g20.15))") &
!   nearest(huge(0.0),1.0), &
!   nearest(huge(0.0),-1.0), &
!   nearest(huge(0.0),1.0)- nearest(huge(0.0),-1.0)

end program demo_nearest
```
  Results:
```text
   42.0000038146973    41.9999961853027    .762939453125000E-05
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# NEW\_LINE

## __Name__

__new\_line__(3) - \[CHARACTER\] new-line character

## __Syntax__
```fortran
result = new_line(c)

   character(len=1,kind=kind(c)) :: new_line(c)
   character(len=1),intent(in) :: c(..)
```
## __Description__

__new\_line(c)__ returns the new-line character.

   Case (i)
   : If __a__ is default _character_ and the character in position __10__ of the
   ASCII collating sequence is representable in the default character set,
   then the result is __achar(10)__.

   Case (ii)
   : If __a__ is an ASCII character or an ISO 10646 character, then the
   result is __char(10, kind (a))__.

   Case (iii)
   : Otherwise, the result is a processor-dependent character that
   represents a newline in output to files connected for formatted
   stream output if there is such a character.

   Case (iv)
   : Otherwise, the result is the blank character.

## __Arguments__

  - __C__
    : The argument shall be a scalar or array of the type _character_.

## __Returns__

Returns a _character_ scalar of length one with the new-line character of
the same kind as parameter __c__.

## __Examples__

Sample program:

```fortran
program demo_new_line
implicit none
character,parameter :: nl=new_line('a')
character(len=:),allocatable :: string

   string='This is record 1.'//nl//'This is record 2.'
   write(*,'(a)') string

   write(*,'(*(a))',advance='no') &
      nl,'This is record 1.',nl,'This is record 2.',nl

end program demo_new_line
```
  Results:
```text
   This is record 1.
   This is record 2.

   This is record 1.
   This is record 2.
```
## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# NINT

## __Name__

__nint__(3) - \[TYPE:NUMERIC\] Nearest whole number

## __Syntax__
```fortran
    elemental function nint(x [, kind=NN]) result(ANSWER)
     real(kind=??),intent(in) :: x
     integer(kind=NN) :: ANSWER
```
## __Description__

__nint(x)__ rounds its argument to the nearest whole number with its
sign preserved.

The user must ensure the value is a valid value for the range of the
__kind__ returned. If the processor cannot represent the result in the kind
specified, the result is undefined.

If __x__ is greater than zero, __nint(x)__ has the value __int(x+0.5)__.

If __x__ is less than or equal to zero, __nint(x)__ has the value
__int(a-0.5)__.

## __Arguments__

  - __x__
    : The type of the argument shall be _real_.

  - __kind__
    : (Optional) A constant _integer_ expression indicating the kind
    parameter of the result. Otherwise, the kind type parameter is that
    of default _integer_ type.

## __Returns__

  - __answer__
    : The result is the integer nearest __x__, or if there are two integers
    equally near __x__, the result is whichever such _integer_ has the greater
    magnitude.

    The result is undefined if it cannot be represented in the specified
    integer type.

## __Examples__

Sample program:

```fortran
program demo_nint
implicit none
integer,parameter :: dp=kind(0.0d0)
real              :: x4 = 1.234E0
real(kind=dp)     :: x8 = 4.721_dp

! basic use
   print *, nint(x4), nint(x8),nint(-x8)
   ! elemental
   print *,nint([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

! issues
ISSUES: block
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
integer :: icheck
   ! make sure input is in range for the type returned
   write(*,*)'Range limits for typical KINDS:'
   write(*,'(1x,g0,1x,g0)')  &
   & int8,huge(0_int8),   &
   & int16,huge(0_int16), &
   & int32,huge(0_int32), &
   & int64,huge(0_int64)

   ! the standard does not require this to be an error ...
   x8=12345.67e15 ! too big of a number
   icheck=selected_int_kind(ceiling(log10(x8)))
   write(*,*)'Any KIND big enough? ICHECK=',icheck
   print *, 'These are all wrong answers for ',x8
   print *, nint(x8,kind=int8)
   print *, nint(x8,kind=int16)
   print *, nint(x8,kind=int32)
   print *, nint(x8,kind=int64)
endblock ISSUES

end program demo_nint
```
  Results:
```text
     1    5   -5
    -3   -3   -2   -2   -2
    -1   -1    0    1    1
     2    2    2    3    3
    Range limits for typical KINDS:
    1 127
    2 32767
    4 2147483647
    8 9223372036854775807
    Any KIND big enough? ICHECK=          16
    These are all wrong answers for    1.2345669499901444E+019
       0
         0
              0
    -9223372036854775808
```

## __Standard__

FORTRAN 77 and later, with KIND argument - Fortran 90 and later

## __See Also__

[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__int__(3)](INT),
[__selected_int_kind__(3)](SELECTED_INT_KIND),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# NORM2

## __Name__

__norm2__(3) - \[MATHEMATICS\] Euclidean vector norm

## __Syntax__
```fortran
result = norm2(array, dim)

real function result norm2(array, dim)

   real,intent(in) :: array(..)
   integer,intent(in),optional :: dim
```
## __Description__

Calculates the Euclidean vector norm (L\_2 norm) of __array__ along
dimension __dim__.

## __Arguments__

  - __array__
    : Shall be an array of type _real_.

  - __dim__
    : shall be a scalar of type _integer_ with a value in the
    range from __1__ to  __rank(array)__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the square root of the sum of squares of
the elements of __array__ is returned.

Otherwise, an array of rank __n-1__,
where __n__ equals the rank of __array__, and a shape similar to that of __array__
with dimension DIM dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_norm2
implicit none
integer :: i

real :: x(3,3) = reshape([ &
   1, 2, 3, &
   4, 5 ,6, &
   7, 8, 9  &
],shape(x),order=[2,1])

write(*,*) 'x='
write(*,'(4x,3f4.0)')transpose(x)

write(*,*) 'norm2(x)=',norm2(x)

write(*,*) 'x**2='
write(*,'(4x,3f4.0)')transpose(x**2)
write(*,*)'sqrt(sum(x**2))=',sqrt(sum(x**2))

end program demo_norm2
```
Results:
```text
 x=
      1.  2.  3.
      4.  5.  6.
      7.  8.  9.
 norm2(x)=   16.88194
 x**2=
      1.  4.  9.
     16. 25. 36.
     49. 64. 81.
 sqrt(sum(x**2))=   16.88194
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__product__(3)](PRODUCT),
[__sum__(3)](SUM),
[__hypot__(3)](HYPOT)

###### fortran-lang intrinsic descriptions
# NOT

## __Name__
__not__(3) - \[BIT:LOGICAL\] Logical negation

## __Syntax__
```fortran
result = not(i)
```
## __Description__

NOT returns the bitwise Boolean inverse of I.

## __Arguments__

  - __i__
    : The type shall be _integer_.

## __Returns__

The return type is _integer_, of the same kind as the argument.

## __Examples__

Sample program

```fortran
program demo_not
implicit none
integer :: i

   i=13741
   write(*,'(b32.32,1x,i0)')i,i
   write(*,'(b32.32,1x,i0)')not(i),not(i)

end program demo_not
```

Results:

```
   00000000000000000011010110101101 13741
   11111111111111111100101001010010 -13742
```

## __Standard__

Fortran 95 and later

## __See Also__

[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),

[__ibclr__(3)](IBCLR)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# NULL

## __Name__

__null__(3) - \[TRANSFORMATIONAL\] Function that returns a disassociated pointer

## __Syntax__
```fortran
ptr => null(mold)

```
## __Description__

Returns a disassociated pointer.

If __mold__ is present, a disassociated pointer of the same type is
returned, otherwise the type is determined by context.

In _Fortran 95_, __mold__ is optional. Please note that _Fortran 2003_ includes cases where it is required.

## __Arguments__

  - __mold__
    : (Optional) shall be a pointer of any association status and of any
    type.

## __Returns__

A disassociated pointer or an unallocated allocatable entity.

## __Examples__

Sample program:

```fortran
!program demo_null
module showit
implicit none
private
character(len=*),parameter :: g='(*(g0,1x))'
public gen
! a generic interface that only differs in the
! type of the pointer the second argument is
interface gen
 module procedure s1
 module procedure s2
end interface

contains

subroutine s1 (j, pi)
 integer j
 integer, pointer :: pi
   if(associated(pi))then
      write(*,g)'Two integers in S1:,',j,'and',pi
   else
      write(*,g)'One integer in S1:,',j
   endif
end subroutine s1

subroutine s2 (k, pr)
 integer k
 real, pointer :: pr
   if(associated(pr))then
      write(*,g)'integer and real in S2:,',k,'and',pr
   else
      write(*,g)'One integer in S2:,',k
   endif
end subroutine s2

end module showit

use showit, only : gen

real,target :: x = 200.0
integer,target :: i = 100

real, pointer :: real_ptr
integer, pointer :: integer_ptr

! so how do we call S1() or S2() with a disassociated pointer?

! the answer is the null() function with a mold value

! since s1() and s2() both have a first integer
! argument the NULL() pointer must be associated
! to a real or integer type via the mold option
! so the following can distinguish whether s1(1)
! or s2() is called, even though the pointers are
! not associated or defined

call gen (1, null (real_ptr) )    ! invokes s2
call gen (2, null (integer_ptr) ) ! invokes s1
real_ptr => x
integer_ptr => i
call gen (3, real_ptr ) ! invokes s2
call gen (4, integer_ptr ) ! invokes s1

end
!end program demo_null
```
  Results:
```text
   One integer in S2:, 1
   One integer in S1:, 2
   integer and real in S2:, 3 and 200.000000
   Two integers in S1:, 4 and 100
```
## __Standard__

Fortran 95 and later

## __See Also__

[__associated__(3)](ASSOCIATED)

###### fortran-lang intrinsic descriptions
# NUM\_IMAGES

## __Name__

__num\_images__(3) - \[COLLECTIVE\] Number of images

## __Syntax__
```fortran
result = num_images(distance, failed)
```
## __Description__

Returns the number of images.

## __Arguments__

  - __distance__
    : (optional, __intent(in)__) Nonnegative scalar integer

  - __failed__
    : (optional, __intent(in)__) Scalar logical expression

## __Returns__

Scalar default-kind _integer_. If __distance__ is not present or has value 0,
the number of images in the current team is returned. For values smaller
or equal distance to the initial team, it returns the number of images
index on the ancestor team which has a distance of __distance__ from the
invoking team. If __distance__ is larger than the distance to the initial
team, the number of images of the initial team is returned. If __failed__ is
not present the total number of images is returned; if it has the value
.true., the number of failed images is returned, otherwise, the number
of images which do have not the failed status.

## __Examples__

Sample program:

```fortran
program demo_num_images
implicit none
integer :: value[*]
integer :: i

   value = this_image()
   sync all
   if (this_image() == 1) then
     do i = 1, num_images()
       write(*,'(2(a,i0))') 'value[', i, '] is ', value[i]
     end do
   endif

end program demo_num_images
```

## __Standard__

Fortran 2008 and later. With DISTANCE or FAILED argument, TS 18508 or later

## __See Also__

[__this\_image__(3)](THIS_IMAGE),
[__image\_index__(3)](THIS_INDEX)

###### fortran-lang intrinsic descriptions
# PACK

## __Name__

__pack__(3) - \[ARRAY CONSTRUCTION\] Pack an array into an array of rank one

## __Syntax__
```fortran
result = pack(array, mask,vector)

   TYPE(kind=KIND) function pack(array,mask,vector)
   TYPE(kind=KIND),option(in) :: array(*)
   logical  :: mask(*)
   TYPE(kind=KIND),option(in),optional :: vector(*)
```
   where TYPE(kind=KIND) may be any type, where __array__ and __vector__
   and the returned value must by of the same type. __mask__ may be a
   scalar as well an an array.

## __Description__

Stores the elements of ARRAY in an array of rank one.

The beginning of the resulting array is made up of elements whose __mask__
equals __.true.__. Afterwards, positions are filled with elements taken from
__vector__.

## __Arguments__

  - __array__
    : Shall be an array of any type.

  - __mask__
    : Shall be an array of type _logical_ and of the same size as __array__.
    Alternatively, it may be a _logical_ scalar.

  - __vector__
    : (Optional) shall be an array of the same type as __array__ and of rank
    one. If present, the number of elements in __vector__ shall be equal to
    or greater than the number of true elements in __mask__. If __mask__ is
    scalar, the number of elements in __vector__ shall be equal to or
    greater than the number of elements in __array__.

## __Returns__

The result is an array of rank one and the same type as that of __array__.
If __vector__ is present, the result size is that of __vector__, the number of
__.true.__ values in __mask__ otherwise.

## __Examples__

Sample program:

```fortran
program demo_pack
implicit none
   call test1()
   call test2()
   call test3()
contains
!
subroutine test1()
! gathering nonzero elements from an array:
integer :: m(6)

   m = [ 1, 0, 0, 0, 5, 0 ]
   write(*, fmt="(*(i0, ' '))") pack(m, m /= 0)  ! "1 5"

end subroutine test1
!
subroutine test2()
! Gathering nonzero elements from an array and appending elements
! from VECTOR till the size of the mask array (or array size if the
! mask is scalar):
integer :: m(4)

   m = [ 1, 0, 0, 2 ]
   write(*, fmt="(*(i0, ' '))") pack(m, m /= 0, [ 0, 0, 3, 4 ])

end subroutine test2
!
subroutine test3()
! select strings whose second character is "a"
character(len=10) :: m(4)

m = [ character(len=10) :: 'ape', 'bat', 'cat', 'dog']
   write(*, fmt="(*(g0, ' '))") pack(m, m(:)(2:2) == 'a' )

end subroutine test3
!
end program demo_pack
```
  Results:
```text
   1 5
   1 2 3 4
   bat        cat
```

## __Standard__

Fortran 95 and later

## __See Also__

[__unpack__(3)](UNPACK),
[__merge__(3)](MERGE),
[__pack__(3)](PACK),
[__spread__(3)](SPREAD),
[__unpack__(3)](UNPACK)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# PARITY

## __Name__

__parity__(3) - \[TRANSFORMATIONAL\] Reduction with exclusive __OR__()

## __Syntax__
```fortran
result = parity(mask, dim)

    function parity(mask, dim)
    type(logical(kind=LKIND))                    :: dim
    type(logical(kind=LKIND)),intent(in)         :: mask(..)
    type(integer(kind=KIND)),intent(in),optional :: dim
```
where KIND and LKIND are any supported kind for the type.
```
## __Description__

Calculates the parity (i.e. the reduction using .xor.) of __mask__ along
dimension __dim__.

## __Arguments__

  - __mask__
    : Shall be an array of type _logical_.

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __mask__.

## __Returns__

The result is of the same type as __mask__.

If __dim__ is absent, a scalar with the parity of all elements in __mask__ is
returned: __.true.__ if an odd number of elements are __.true.__ and __.false.__
otherwise.

When __dim__ is specified the returned shape is similar to that of __mask__
with dimension __dim__ dropped.

## __Examples__

Sample program:

```fortran
program demo_parity
implicit none
logical :: x(2) = [ .true., .false. ]
   print *, parity(x)
end program demo_parity
```
  Results:
```text
    T
```
## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions
# POPCNT

## __Name__

__popcnt__(3) - \[BIT:COUNT\] Number of bits set

## __Syntax__
```fortran
result = popcnt(i)
```
## __Description__

Returns the number of bits set in the binary representation of an
_integer_.

## __Arguments__

  - __i__
    : Shall be of type _integer_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_popcnt
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
implicit none
     print *, popcnt(127),       poppar(127)
     print *, popcnt(huge(0)), poppar(huge(0))
     print *, popcnt(huge(0_int8)), poppar(huge(0_int8))
     print *, popcnt(huge(0_int16)), poppar(huge(0_int16))
     print *, popcnt(huge(0_int32)), poppar(huge(0_int32))
     print *, popcnt(huge(0_int64)), poppar(huge(0_int64))
end program demo_popcnt
```
Results:
```text
        7           1
       31           1
        7           1
       15           1
       31           1
       63           1
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__poppar__(3)](POPPAR),
[__leadz__(3)](LEADZ),
[__trailz__(3)](TRAILZ)

###### fortran-lang intrinsic descriptions
# POPPAR

## __Name__

__poppar__(3) - \[BIT:COUNT\] Parity of the number of bits set

## __Syntax__
```fortran
result = poppar(i)
```
## __Description__

Returns the parity of an integer's binary representation (i.e., the
parity of the number of bits set).

## __Arguments__

  - __i__
    : Shall be of type _integer_.

## __Returns__

The return value is equal to __0__ if __i__ has an even number of bits set and 1 if an odd
number of bits are set.

It is of type _integer_ and of the default _integer_ kind.

## __Examples__

Sample program:

```fortran
program demo_popcnt
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
implicit none
   print  *,  popcnt(127),            poppar(127)
   print  *,  popcnt(huge(0_int8)),   poppar(huge(0_int8))
   print  *,  popcnt(huge(0_int16)),  poppar(huge(0_int16))
   print  *,  popcnt(huge(0_int32)),  poppar(huge(0_int32))
   print  *,  popcnt(huge(0_int64)),  poppar(huge(0_int64))
end program demo_popcnt
```
  Results:
```text
              7           1
              7           1
             15           1
             31           1
             63           1
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__popcnt__(3)](POPCNT),
[__leadz__(3)](LEADZ),
[__trailz__(3)](TRAILZ)

###### fortran-lang intrinsic descriptions
# PRECISION

## __Name__

__precision__(3) - \[NUMERIC MODEL\] Decimal precision of a real kind

## __Syntax__
```fortran
result = precision(x)
```
## __Description__

__precision(x)__ returns the decimal precision in the model of the type
of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_ or _complex_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_precision
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x(2)
complex(kind=dp) :: y

   print *, precision(x), range(x)
   print *, precision(y), range(y)
end program demo_precision
```
  Results:
```text
              6          37
             15         307
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# PRESENT

## __Name__

__present__(3) - [STATE\] Determine whether an optional dummy argument
                 is specified

## __Syntax__
```fortran
result = present(a)

   function present (a)
   logical :: present
```
## __Description__

Determines whether an optional dummy argument is present.

## __Arguments__

  - __a__
    : May be of any type and may be a pointer, scalar or array value,
    or a dummy procedure. It shall be the name of an optional dummy
    argument accessible within the current subroutine or function.

## __Returns__

Returns either __.true.__ if the optional argument __a__ is present,
or __.false.__ otherwise.

## __Examples__

Sample program:

```fortran
program demo_present
implicit none
   write(*,*) func(), func(42)
contains

integer function func(x)
integer, intent(in), optional :: x
   if(present(x))then
     func=x**2
   else
     func=0
   endif
end function

end program demo_present
```
  Results:
```text
     0        1764
```

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# PRODUCT

## __Name__

__product__(3) - \[ARRAY REDUCTION\] Product of array elements

## __Syntax__
```fortran
  result = product(array, dim, mask)

    NUMERIC,intent(in) :: array(..)
    integer,intent(in),optional :: dim
    logical,intent(in),optional :: mask(..)
```
where __NUMERIC__ is any numeric type

## __Description__

Multiplies together all the selected elements of __array__, or along
dimension __dim__ if the corresponding element in __mask__ is __.true.__.

If __dim__ is absent, a scalar with the product of all elements in __array__ is
returned. (Note a zero-sized __array__ returns __1__).

When __dim__ is present, If the masked array has a dimension of one
(ie. is a vector) the result is a scalar.  Otherwise, an array of rank
__n-1__, where __n__ equals the rank of __array__, and a shape similar
to that of __array__ with dimension __dim__ dropped is returned.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_ or _complex_.

  - __dim__
    : shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __array__.

  - __mask__
    : shall be of type _logical_ and either be a scalar or an
    array of the same shape as __array__.

## __Returns__

The result is of the same type as __array__.

## __Examples__

Sample program:

```fortran
program demo_product
implicit none
character(len=*),parameter :: all='(*(g0,1x))' ! a handy format
character(len=1),parameter :: nl=new_line('a')

NO_DIM: block
!    If DIM is not specified, the result is the product of all the
!    selected array elements.
integer :: i,n, p1, p2
integer,allocatable :: array(:)
   ! all elements are selected by default
   do n=1,10
      print all, 'factorial of ',n,' is ', product([(real(i),i=1,n)])
   enddo

   ! using a mask
   array=[10,12,13,15,20,25,30]
   p1=product(array, mask=mod(array, 2)==1) ! only odd elements
   p2=product(array, mask=mod(array, 2)/=1) ! only even elements
   print all, nl,'product of all elements',product(array) ! all elements
   print all, ' odd * even =',nl,p1,'*',p2,'=',p1*p2

   ! NOTE: If ARRAY is a zero-sized array, the result is equal to one
   print all
   print all, 'zero-sized array=>',product([integer :: ])
   ! NOTE: If nothing in the mask is true, this also results in a null
   !       array
   print all, 'all elements have a false mask=>',product(array,mask=.false.)

endblock NO_DIM

WITH_DIM: block
integer :: rect(2,3)
integer :: box(2,3,4)

!  lets fill a few arrays
   rect = reshape([ &
     1, 2, 3,       &
     4, 5, 6        &
   ],shape(rect),order=[2,1])
   call print_matrix_int('rect',rect)

!  Find the product of each column in RECT.
   print all, 'product of columns=',product(rect, dim = 1)

! Find the product of each row in RECT.
   print all, 'product of rows=',product(rect, dim = 2)

! now lets try a box
   box(:,:,1)=rect
   box(:,:,2)=rect*(+10)
   box(:,:,3)=rect*(-10)
   box(:,:,4)=rect*2
   ! lets look at the values
   call print_matrix_int('box 1',box(:,:,1))
   call print_matrix_int('box 2',box(:,:,2))
   call print_matrix_int('box 3',box(:,:,3))
   call print_matrix_int('box 4',box(:,:,4))

   ! remember without dim= even a box produces a scalar
   print all, 'no dim gives a scalar',product(real(box))

   ! only one plane has negative values, so note all the "1" values
   ! for vectors with no elements
   call print_matrix_int('negative values',product(box,mask=box < 0,dim=1))

!   If DIM is specified and ARRAY has rank greater than one, the
!   result is a new array in which dimension DIM has been eliminated.

   ! pick a dimension to multiply though
   call print_matrix_int('dim=1',product(box,dim=1))

   call print_matrix_int('dim=2',product(box,dim=2))

   call print_matrix_int('dim=3',product(box,dim=3))

endblock WITH_DIM

contains

subroutine print_matrix_int(title,arr)
implicit none

!@(#) print small 2d integer arrays in row-column format

character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:,:)
integer                      :: i
character(len=:),allocatable :: biggest

   print all
   print all, trim(title),':(',shape(arr),')'  ! print title
   biggest='           '  ! make buffer to write integer into
   ! find how many characters to use for integers
   write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
   ! use this format to write a row
   biggest='(" > [",*(i'//trim(biggest)//':,","))'
   ! print one row of array at a time
   do i=1,size(arr,dim=1)
      write(*,fmt=biggest,advance='no')arr(i,:)
      write(*,'(" ]")')
   enddo

end subroutine print_matrix_int

end program demo_product
```
Results:
```text
factorial of  1  is  1.000000
factorial of  2  is  2.000000
factorial of  3  is  6.000000
factorial of  4  is  24.00000
factorial of  5  is  120.0000
factorial of  6  is  720.0000
factorial of  7  is  5040.000
factorial of  8  is  40320.00
factorial of  9  is  362880.0
factorial of  10  is  3628800.

 product of all elements 351000000
 odd * even =
 4875 * 72000 = 351000000

zero-sized array=> 1
all elements have a false mask=> 1

rect :( 2 3 )
 > [  1,  2,  3 ]
 > [  4,  5,  6 ]
product of columns= 4 10 18
product of rows= 6 120

box 1 :( 2 3 )
 > [  1,  2,  3 ]
 > [  4,  5,  6 ]

box 2 :( 2 3 )
 > [  10,  20,  30 ]
 > [  40,  50,  60 ]

box 3 :( 2 3 )
 > [ -10, -20, -30 ]
 > [ -40, -50, -60 ]

box 4 :( 2 3 )
 > [   2,   4,   6 ]
 > [   8,  10,  12 ]
no dim gives a scalar .1719927E+26

negative values :( 3 4 )
 > [     1,     1,   400,     1 ]
 > [     1,     1,  1000,     1 ]
 > [     1,     1,  1800,     1 ]

dim=1 :( 3 4 )
 > [     4,   400,   400,    16 ]
 > [    10,  1000,  1000,    40 ]
 > [    18,  1800,  1800,    72 ]

dim=2 :( 2 4 )
 > [       6,    6000,   -6000,      48 ]
 > [     120,  120000, -120000,     960 ]

dim=3 :( 2 3 )
 > [    -200,   -3200,  -16200 ]
 > [  -51200, -125000, -259200 ]
```
## __Standard__

Fortran 95 and later

## __See Also__

[__sum__(3)](SUM), note that an element by element multiplication is done
directly using the star character.

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# RADIX

## __Name__

__radix__(3) - \[NUMERIC MODEL\] Base of a model number

## __Syntax__
```fortran
result = radix(x)
```
## __Description__

__radix(x)__ returns the base of the model representing the entity __x__.

## __Arguments__

  - __x__
    : Shall be of type _integer_ or _real_

## __Returns__

The return value is a scalar of type _integer_ and of the default integer
kind.

## __Examples__

Sample program:

```fortran
program demo_radix
implicit none
   print *, "The radix for the default integer kind is", radix(0)
   print *, "The radix for the default real kind is", radix(0.0)
   print *, "The radix for the doubleprecsion real kind is", radix(0.0d0)
end program demo_radix
```
  Results:
```text
    The radix for the default integer kind is           2
    The radix for the default real kind is           2
    The radix for the doubleprecsion real kind is           2
```

## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# RANDOM\_NUMBER

## __Name__

__random\_number__(3) - \[MATHEMATICS:RANDOM\] Pseudo-random number

## __Syntax__
```fortran
   random_number(harvest)
```
## __Description__

Returns a single pseudorandom number or an array of pseudorandom numbers
from the uniform distribution over the range 0 \<= x \< 1.

## __Arguments__

  - __harvest__
    : Shall be a scalar or an array of type _real_.

## __Examples__

Sample program:

```fortran
program demo_random_number
use, intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
integer, allocatable :: seed(:)
integer              :: n
integer              :: first,last
integer              :: i
integer              :: rand_int
integer,allocatable  :: count(:)
real(kind=dp)        :: rand_val
   call random_seed(size = n)
   allocate(seed(n))
   call random_seed(get=seed)
   first=1
   last=10
   allocate(count(last-first+1))
   ! To have a discrete uniform distribution on the integers
   ! [first, first+1, ..., last-1, last] carve the continuous
   ! distribution up into last+1-first equal sized chunks,
   ! mapping each chunk to an integer.
   !
   ! One way is:
   !   call random_number(rand_val)
   ! choose one from last-first+1 integers
   !   rand_int = first + FLOOR((last+1-first)*rand_val)
      count=0
      ! generate a lot of random integers from 1 to 10 and count them.
      ! with a large number of values you should get about the same
      ! number of each value
      do i=1,100000000
         call random_number(rand_val)
         rand_int=first+floor((last+1-first)*rand_val)
         if(rand_int.ge.first.and.rand_int.le.last)then
            count(rand_int)=count(rand_int)+1
         else
            write(*,*)rand_int,' is out of range'
         endif
      enddo
      write(*,'(i0,1x,i0)')(i,count(i),i=1,size(count))
end program demo_random_number
```
Results:
```
   1 10003588
   2 10000104
   3 10000169
   4 9997996
   5 9995349
   6 10001304
   7 10001909
   8 9999133
   9 10000252
   10 10000196
```
## __Standard__

Fortran 95 and later

## __See Also__

[__random\_seed__(3)](RANDOM_SEED)

###### fortran-lang intrinsic descriptions
# RANDOM\_SEED

## __Name__

__random\_seed__(3) - \[MATHEMATICS:RANDOM\] Initialize a pseudo-random number sequence

## __Syntax__
```fortran
call random_seed(size, put, get)
```
## __Description__

Restarts or queries the state of the pseudorandom number generator used
by random\_number.

If random\_seed is called without arguments, it is seeded with random
data retrieved from the operating system.

## __Arguments__

  - __size__
    : (Optional) Shall be a scalar and of type default _integer_, with
    __intent(out)__. It specifies the minimum size of the arrays used
    with the __put__ and __get__ arguments.

  - __put__
    : (Optional) Shall be an array of type default _integer_ and rank one.
    It is __intent(in)__ and the size of the array must be larger than
    or equal to the number returned by the __size__ argument.

  - __get__
    : (Optional) Shall be an array of type default _integer_ and rank one.
    It is __intent(out)__ and the size of the array must be larger than
    or equal to the number returned by the __size__ argument.

## __Examples__

Sample program:

```fortran
program demo_random_seed
implicit none
integer, allocatable :: seed(:)
integer :: n

   call random_seed(size = n)
   allocate(seed(n))
   call random_seed(get=seed)
   write (*, *) seed

end program demo_random_seed
```
  Results:
```text
     -674862499 -1750483360  -183136071  -317862567   682500039
     349459   344020729 -1725483289
```

## __Standard__

Fortran 95 and later

## __See Also__

[__random\_number__(3)](RANDOM_NUMBER)

###### fortran-lang intrinsic descriptions
# RANGE

## __Name__

__range__(3) - \[NUMERIC MODEL\] Decimal exponent range of a real kind

## __Syntax__
```fortran
result = range(x)

      function range (x)
      integer :: range
      type(TYPE,kind=KIND),intent(in) :: x
```
   where TYPE is _real_ or _complex_ and KIND is any kind supported by
   TYPE.
## __Description__

__range(x)__ returns the decimal exponent range in the model of the type
of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_ or _complex_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_range
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp)    :: x(2)
complex(kind=dp) :: y
   print *, precision(x), range(x)
   print *, precision(y), range(y)
end program demo_range
```
  Results:
```text
              6          37
             15         307
```

## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# RANK

## __Name__

__rank__(3) - \[ARRAY INQUIRY\] Rank of a data object

## __Syntax__
```fortran
result = rank(a)
```
## __Description__

__rank(a)__ returns the rank of a scalar or array data object.

## __Arguments__

  - __a__
    : can be of any type

## __Returns__

The return value is of type _integer_ and of the default integer kind. For
arrays, their rank is returned; for scalars zero is returned.

## __Examples__

Sample program:

```fortran
program demo_rank
implicit none
integer :: a
real, allocatable :: b(:,:)
real  :: c(10,20,30)
   print *, rank(a), rank(b), rank(c)
end program demo_rank
```
Results:
```text
   0           2           3
```
## __Standard__

TS 29113

###### fortran-lang intrinsic descriptions
# REAL

## __Name__
__real__(3) - \[TYPE:NUMERIC\] Convert to real type

## __Syntax__
```fortran
result = real(x, kind)
```
## __Description__

__real(x, kind)__ converts its argument __x__ to a real type.

## __Arguments__

  - __x__
    : Shall be _integer_, _real_, or _complex_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

These functions return a _real_ variable or array under the following
rules:

1.  __real__(x) is converted to a default _real_ type if __x__ is an _integer_
    or _real_ variable.

2.  __real__(x) is converted to a real type with the kind type parameter
    of __x__ if __x__ is a _complex_ variable.

3.  __real(x, kind)__ is converted to a _real_ type with kind type
    parameter __kind__ if __x__ is a _complex_, _integer_, or _real_ variable.

## __Examples__

Sample program:

```fortran
program demo_real
use,intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
complex              :: zr = (1.0, 2.0)
doubleprecision      :: xd=huge(3.0d0)
complex(kind=dp) :: zd=cmplx(4.0e0_dp,5.0e0_dp,kind=dp)

   print *, real(zr), aimag(zr)
   print *, dble(zd), aimag(zd)

   write(*,*)xd,real(xd,kind=kind(0.0d0)),dble(xd)
end program demo_real
```

Results:

```
 1.00000000       2.00000000
 4.0000000000000000       5.0000000000000000
 1.7976931348623157E+308  1.7976931348623157E+308  1.7976931348623157E+308
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__dble__(3)](DBLE),
[__float__(3)](FLOAT)

###### fortran-lang intrinsic descriptions
# REPEAT

## __Name__

__repeat__(3) - \[CHARACTER\] Repeated string concatenation

## __Syntax__
```fortran
result = repeat(string, ncopies)

   character(len=len(string)*ncopies) :: repeat
   character(len=*),intent(in)        :: string
   integer,intent(in)                 :: ncopies
```
## __Description__

Concatenates __ncopies__ copies of a string.

## __Arguments__

  - __string__
    : The input string to repeatedly generate.
    Shall be scalar and of type _character_.

  - __ncopies__
    : Number of copies to make of _string_, greater than or equal to zero (0).
    Shall be scalar and of type _integer_.

## __Returns__

A new scalar of type _character_ built up from __ncopies__ copies of __string__.

## __Examples__

Sample program:

```fortran
program demo_repeat
implicit none
integer :: i, j
    write(*,'(a)') repeat("^v", 36)         ! line break
    write(*,'(a)') repeat("_", 72)          ! line break
    write(*,'(a)') repeat("1234567890", 7)  ! number line
    do i=80,0,-1 ! a simple progress bar
        write(*,'(a)',advance='no') &
        & repeat("#", i)//repeat(' ',80-i)//char(13)
        !do something slow
    enddo
end program demo_repeat
```
  Results:
```
   ^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v
   ________________________________________________________________________
   1234567890123456789012345678901234567890123456789012345678901234567890
```
## __Standard__

Fortran 95 and later

## __See Also__

Functions that perform operations on character strings:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL),
    [__adjustr__(3)](ADJUSTR),
    [__index__(3)](INDEX),
    [__scan__(3)](SCAN),
    [__verify__(3)](VERIFY)

  - __Non-elemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT),
    [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# RESHAPE

## __Name__

__reshape__(3) - \[ARRAY RESHAPE\] Function to reshape an array

## __Syntax__
```fortran
result = reshape(source, shape, pad, order)
```
## __Description__

Reshapes array __source__ to correspond to __shape__. If necessary, the new
array may be padded with elements from __pad__ or permuted as defined by
__order__.

## __Arguments__

  - __source__
    : an array of any type.

  - __shape__
    : an array of rank one and type _integer_. Its values must be positive
    or zero.

  - __pad__
    : (Optional) an array of the same type as __source__.

  - __order__
    : (Optional) an array of type _integer_ and the same shape as __shape__. Its
    values shall be a permutation of the numbers from 1 to n, where n is
    the size of __shape__. If __order__ is absent, the natural ordering shall be
    assumed.

## __Returns__

The result is an array of shape __shape__ with the same type as __source__.

## __Examples__

Sample program:

```fortran
program demo_reshape
implicit none
integer :: i
integer, dimension(4) :: x=[(i,i=10,40,10)]
real :: xx(3,4)
real,allocatable :: v(:)
    ! x is originally a vector with four elements
    write(*,*) shape(x) ! what is the current shape of the array?
    write(*,*) shape(reshape(x, [2, 2]))    ! prints "2 2"

    ! pack any array into a vector
    xx=1.0
    v=reshape(xx,[size(xx)])
    write(*,*)shape(v),ubound(v)
end program demo_reshape
```
  Results:
```text
              4
              2           2
             12          12
```
## __Standard__

Fortran 95 and later

## __See Also__

[__shape__(3)](SHAPE)

###### fortran-lang intrinsic descriptions
# RRSPACING

## __Name__

__rrspacing__(3) - \[MODEL\_COMPONENTS\] Reciprocal of the relative spacing

## __Syntax__
```fortran
result = rrspacing(x)
```
## __Description__

__rrspacing(x)__ returns the reciprocal of the relative spacing of model
numbers near __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The return value is of the same type and kind as __x__. The value returned
is equal to __abs(fraction(x)) \* float(radix(x))\*\*digits(x)__.

## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# SAME\_TYPE\_AS

## __Name__

__same\_type\_as__(3) - \[STATE\] Query dynamic types for equality

## __Syntax__
```fortran
result = same_type_as(a, b)
```
## __Description__

Query dynamic types for equality.

## __Arguments__

  - __a__
    : Shall be an object of extensible declared type or unlimited
    polymorphic.

  - __b__
    : Shall be an object of extensible declared type or unlimited
    polymorphic.

## __Returns__

The return value is a scalar of type default logical. It is true if and
only if the dynamic type of __a__ is the same as the dynamic type of __b__.

## __Standard__

Fortran 2003 and later

## __See Also__

[__extends\_type\_of__(3)](EXTENDS_TYPE_OF)

###### fortran-lang intrinsic descriptions
# SCALE

## __Name__

__scale__(3) - \[MODEL\_COMPONENTS\] Scale a real value by a whole power of the radix

## __Syntax__
```fortran
result = scale(x, i)

   real(kind=KIND),intent(in) :: x
   integer,intent(in)         :: i
```
## __Description__

__scale(x,i)__ returns x \* __radix(x)\*\*i__.

## __Arguments__

  - __x__
    : The type of the argument shall be a _real_.

  - __i__
    : The type of the argument shall be a _integer_.

## __Returns__

The return value is of the same type and kind as __x__. Its value is
__x \* radix(x)\*\*i__.

## __Examples__

Sample program:

```fortran
program demo_scale
implicit none
real :: x = 178.1387e-4
integer :: i = 5
   print *, scale(x,i), x*radix(x)**i
end program demo_scale
```

Results:

```
    0.570043862      0.570043862
```

## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# SCAN

## __Name__

__scan__(3) - \[CHARACTER:SEARCH\] Scan a string for the presence of a set of characters

## __Syntax__
```fortran
result = scan(string, set[, back [, kind]])
```
## __Description__

Scans a __string__ for any of the characters in a __set__ of characters.

If __back__ is either absent or equals __.false.__, this function returns the
position of the leftmost character of __STRING__ that is in __set__. If __back__
equals __.true.__, the rightmost position is returned. If no character of __set__
is found in __string__, the result is zero.

## __Arguments__

  - __string__
    : Shall be of type _character_.

  - __set__
    : Shall be of type _character_.

  - __back__
    : (Optional) shall be of type _logical_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind.

## __Examples__

Sample program:

```fortran
program demo_scan
implicit none
   write(*,*) scan("fortran", "ao")          ! 2, found 'o'
   write(*,*) scan("fortran", "ao", .true.)  ! 6, found 'a'
   write(*,*) scan("fortran", "c++")         ! 0, found none
end program demo_scan
```
  Results:
```text
              2
              6
              0
```
## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003 and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX),
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# SELECTED\_CHAR\_KIND

## __Name__

__selected\_char\_kind__(3) - \[KIND\] Choose character kind such as "Unicode"

## __Syntax__
```fortran
result = selected_char_kind(name)
```
## __Description__

__selected\_char\_kind(name)__ returns the kind value for the character
set named NAME, if a character set with such a name is supported, or
__-1__ otherwise. Currently, supported character sets include "ASCII"
and "DEFAULT" (iwhich are equivalent), and "ISO\_10646" (Universal
Character Set, UCS-4) which is commonly known as "Unicode".

## __Arguments__

  - __name__
    : Shall be a scalar and of the default character type.

## __Examples__

Sample program:

```fortran
program demo_selected_char_kind
use iso_fortran_env
implicit none
integer, parameter :: ascii = selected_char_kind ("ascii")
integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')

character(kind=ascii, len=26) :: alphabet
character(kind=ucs4,  len=30) :: hello_world

   alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"
   hello_world = ucs4_'Hello World and Ni Hao -- ' &
                 // char (int (z'4F60'), ucs4)     &
                 // char (int (z'597D'), ucs4)

   write (*,*) alphabet

   open (output_unit, encoding='UTF-8')
   write (*,*) trim (hello_world)
end program demo_selected_char_kind
```
  Results:
```text
    abcdefghijklmnopqrstuvwxyz
    Hello World and Ni Hao -- 你好
```
## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions
# SELECTED\_INT\_KIND

## __Name__

__selected\_int\_kind__(3) - \[KIND\] Choose integer kind

## __Syntax__
```fortran
result = selected_int_kind(r)
```
## __Description__

__selected\_int\_kind(r)__ return the kind value of the smallest integer
type that can represent all values ranging from __-10\*\*r__ (exclusive)
to __10\*\*r__ (exclusive). If there is no integer kind that accommodates
this range, selected\_int\_kind returns __-1__.

## __Arguments__

  - __r__
    : Shall be a scalar and of type _integer_.

## __Examples__

Sample program:

```fortran
program demo_selected_int_kind
implicit none
integer,parameter :: k5 = selected_int_kind(5)
integer,parameter :: k15 = selected_int_kind(15)
integer(kind=k5) :: i5
integer(kind=k15) :: i15

    print *, huge(i5), huge(i15)

    ! the following inequalities are always true
    print *, huge(i5) >= 10_k5**5-1
    print *, huge(i15) >= 10_k15**15-1
end program demo_selected_int_kind
```
  Results:
```text
     2147483647  9223372036854775807
    T
    T
```
## __Standard__

Fortran 95 and later

## __See Also__

[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__int__(3)](INT),
[__nint__(3)](NINT),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions
# SELECTED\_REAL\_KIND

## __Name__

__selected\_real\_kind__(3) - \[KIND\] Choose real kind

## __Syntax__
```fortran
result = selected_real_kind(p, r, radix)
```
## __Description__

__selected\_real\_kind(p, r, radix)__ return the kind value of a real
data type with decimal precision of at least __p__ digits, exponent range of
at least __r__, and with a radix of __radix__.

## __Arguments__

  - __p__
    : (Optional) shall be a scalar and of type _integer_.

  - __r__
    : (Optional) shall be a scalar and of type _integer_.

  - __radix__
    : (Optional) shall be a scalar and of type _integer_.

Before __Fortran 2008__, at least one of the arguments __r__ or __p__ shall
be present; since __Fortran 2008__, they are assumed to be zero if
absent.

## __Returns__

selected\_real\_kind returns the value of the kind type parameter of a
real data type with decimal precision of at least __p__ digits, a decimal
exponent range of at least R, and with the requested __radix__. If the __radix__
parameter is absent, real kinds with any radix can be returned. If more
than one real data type meet the criteria, the kind of the data type
with the smallest decimal precision is returned. If no real data type
matches the criteria, the result is

  - __-1__ if the processor does not support a real data type with a
    precision greater than or equal to __p__, but the __r__ and __radix__
    requirements can be fulfilled

      - __-2__ if the processor does not support a real type with an
        exponent range greater than or equal to __r__, but __p__ and __radix__ are
        fulfillable

      - __-3__ if __radix__ but not __p__ and __r__ requirements are fulfillable

      - __-4__ if __radix__ and either __p__ or __r__ requirements are fulfillable

      - __-5__ if there is no real type with the given __radix__

## __Examples__

Sample program:

```fortran
program demo_selected_real_kind
implicit none
integer,parameter :: p6 = selected_real_kind(6)
integer,parameter :: p10r100 = selected_real_kind(10,100)
integer,parameter :: r400 = selected_real_kind(r=400)
real(kind=p6) :: x
real(kind=p10r100) :: y
real(kind=r400) :: z

   print *, precision(x), range(x)
   print *, precision(y), range(y)
   print *, precision(z), range(z)
end program demo_selected_real_kind
```
  Results:
```text
              6          37
             15         307
             18        4931
```
## __Standard__

Fortran 95 and later; with RADIX - Fortran 2008 and later

## __See Also__

[__precision__(3)](PRECISION),
[__range__(3)](RANGE),
[__radix__(3)](RADIX)

###### fortran-lang intrinsic descriptions
# SET\_EXPONENT

## __Name__

__set\_exponent__(3) - \[MODEL\_COMPONENTS\] Set the exponent of the model

## __Syntax__
```fortran
result = set_exponent(x, i)
```
## __Description__

__set\_exponent(x, i)__ returns the real number whose fractional part is
that of __x__ and whose exponent part is __i__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

  - __i__
    : Shall be of type _integer_.

## __Returns__

The return value is of the same type and kind as __x__. The real number
whose fractional part is that that of __x__ and whose exponent part if __i__ is
returned; it is __fraction(x) \* radix(x)\*\*i__.

## __Examples__

Sample program:

```fortran
program demo_setexp
implicit none
real :: x = 178.1387e-4
integer :: i = 17
   print *, set_exponent(x, i), fraction(x) * radix(x)**i
end program demo_setexp
```
  Results:
```text
      74716.7891       74716.7891
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# SHAPE

## __Name__

__shape__(3) - \[ARRAY INQUIRY\] Determine the shape of an array

## __Syntax__
```fortran
result = shape(source, kind)
```
## __Description__

Determines the shape of an array.

## __Arguments__

  - __source__
    : Shall be an array or scalar of any type. If __source__ is a pointer it
    must be associated and allocatable arrays must be allocated.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

An _integer_ array of rank one with as many elements as __source__ has
dimensions. The elements of the resulting array correspond to the extend
of __source__ along the respective dimensions. If __source__ is a scalar, the
result is the rank one array of size zero. If __kind__ is absent, the return
value has the default integer kind otherwise the specified kind.

## __Examples__

Sample program:

```fortran
program demo_shape
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
integer, dimension(-1:1, -1:2) :: a
   print all, 'shape of array=',shape(a)
   print all, 'shape of constant=',shape(42)
   print all, 'size of shape of constant=',size(shape(42))
   print all, 'ubound of array=',ubound(a)
   print all, 'lbound of array=',lbound(a)
end program demo_shape
```
  Results:
```text
   shape of array= 3 4
   shape of constant=
   size of shape of constant= 0
   ubound of array= 1 2
   lbound of array= -1 -1
```
## __Standard__

Fortran 95 and later; with KIND argument Fortran 2003 and later

## __See Also__

[__reshape__(3)](RESHAPE),
[__size__(3)](SIZE)

###### fortran-lang intrinsic descriptions
# SHIFTA

## __Name__

__shifta__(3) - \[BIT:SHIFT\] shift bits right with fill

## __Syntax__
```fortran
result = shifta(i, shift)
```
## __Description__

Returns a value corresponding to __i__ with all of the bits shifted right by
__shift__ places. If the absolute value of __shift__ is greater than
__bit\_size(i)__, the value is undefined. Bits shifted out from the
right end are lost. The fill is arithmetic: the bits shifted in from the
left end are equal to the leftmost bit, which in two's complement
representation is the sign bit.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__shiftl__(3)](SHIFTL),
[__shiftr__(3)](SHIFTR)

###### fortran-lang intrinsic descriptions
# SHIFTL

## __Name__

__shiftl__(3) - \[BIT:SHIFT\] shift bits left

## __Syntax__
```fortran
result = shiftl(i, shift)
```
## __Description__

Returns a value corresponding to __i__ with all of the bits shifted left by
__shift__ places. If the absolute value of __shift__ is greater than
__bit\_size(i)__, the value is undefined. Bits shifted out from the left
end are lost, and bits shifted in from the right end are set to __0__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__shifta__(3)](SHIFTA),
[__shiftr__(3)](SHIFTR)

###### fortran-lang intrinsic descriptions
# SHIFTR

## __Name__

__shiftr__(3) - \[BIT:SHIFT\] shift bits right

## __Syntax__
```fortran
result = shiftr(i, shift)
```
## __Description__

Returns a value corresponding to __i__ with all of the bits shifted right by
__shift__ places. If the absolute value of __shift__ is greater than
__bit\_size(i)__, the value is undefined. Bits shifted out from the
right end are lost, and bits shifted in from the left end are set to 0.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__shifta__(3)](SHIFTA),
[__shiftl__(3)](SHIFTL)

###### fortran-lang intrinsic descriptions
# SIGN

## __Name__

__sign__(3) - \[NUMERIC\] Sign copying function

## __Syntax__
```fortran
result = sign(a, b)

    elemental function sign(a, b)
    type(TYPE(kind=KIND))            :: sign
    type(TYPE(kind=KIND)),intent(in) :: a, b
```
where TYPE may be _real_ or _integer_ and KIND is any supported kind
for the type.
```
## __Description__

__sign__(a,b) returns the value of __a__ with the sign of __b__.

For processors that distinguish between positive and negative zeros
__sign()__ may be used to distinguish between __real__ values 0.0 and
−0.0. SIGN (1.0, -0.0) will return −1.0 when a negative zero is
distinguishable.

    29  1 Description. Magnitude of A with the sign of B.

## __Arguments__

  - __a__
    : Shall be of type _integer_ or _real_

  - __b__
    : Shall be of the same type and kind as __a__

## __Returns__

The kind of the return value is the magnitude of __a__ with the sign of
__b__. That is,

   - If __b \>= 0__ then the result is __abs(a)__
   - else if __b < 0__ it is -__abs(a)__.
   - if __b__ is _real_ and the processor distinguishes between __-0.0__
     and __0.0__ then the result is __-abs(a)__

## __Examples__

Sample program:

```fortran
program demo_sign
implicit none
   print *, sign( -12,  1 )
   print *, sign( -12,  0 )
   print *, sign( -12, -1 )

   print *, sign( -12.0, [1.0, 0.0, -1.0] )

   print *,'can I distinguish 0 from -0? ',sign(1.0,-0.0).ne.sign(1.0, 0.0)
end program demo_sign
```
Results:
```text
             12
             12
            -12
      12.00000       12.00000      -12.00000
    can I distinguise 0 from -0?  F
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT)
# SINH

## __Name__

__sinh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic sine function

## __Syntax__
```fortran
result = sinh(x)

    elemental TYPE(kind=KIND) function sinh(x)
    TYPE(kind=KIND) :: x
```
Where the returned value has the kind of the input value
and TYPE may be _real_ or _complex_

## __Description__

__sinh(x)__ computes the hyperbolic sine of __x__.

The hyperbolic sine of x is defined mathematically as:

   __sinh(x) = (exp(x) - exp(-x)) / 2.0__

If __x__ is of type _complex_ its imaginary part is regarded as a value
in radians.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_sinh
use, intrinsic :: iso_fortran_env, only : &
& real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = - 1.0_real64
real(kind=real64) :: nan, inf
character(len=20) :: line

   print *, sinh(x)
   print *, (exp(x)-exp(-x))/2.0

   ! sinh(3) is elemental and can handle an array
   print *, sinh([x,2.0*x,x/3.0])

   ! a NaN input returns NaN
   line='NAN'
   read(line,*) nan
   print *, sinh(nan)

   ! a Inf input returns Inf
   line='Infinity'
   read(line,*) inf
   print *, sinh(inf)

   ! an overflow returns Inf
   x=huge(0.0d0)
   print *, sinh(x)

end program demo_sinh
```
Results:
```text
  -1.1752011936438014
  -1.1752011936438014
  -1.1752011936438014       -3.6268604078470190      -0.33954055725615012
                       NaN
                  Infinity
                  Infinity
```
## __Standard__

Fortran 95 and later, for a complex argument Fortran 2008 or later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

[__asinh__(3)](ASINH)

###### fortran-lang intrinsic descriptions
# SIN

## __Name__

__sin__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Sine function

## __Syntax__
```fortran
result = sin(x)

    elemental TYPE(kind=KIND) function sin(x)
    TYPE(kind=KIND) :: x
```
Where the returned value has the kind of the input value
and TYPE may be _real_ or _complex_

## __Description__

__sin(x)__ computes the sine of an angle given the size of the angle in
radians.

The sine of an angle in a right-angled triangle is the ratio of the
length of the side opposite the given angle divided by the length of the
hypotenuse.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_ in radians.

## __Returns__

  - __result__
    : The return value has the same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program sample_sin
implicit none
real :: x = 0.0
   x = sin(x)
end program sample_sin
```

## __Haversine Formula__

From the article on "Haversine formula" in Wikipedia:

```text
The haversine formula is an equation important in navigation,
giving great-circle distances between two points on a sphere from
their longitudes and latitudes.
```

So to show the great-circle distance between the Nashville International
Airport (BNA) in TN, USA, and the Los Angeles International Airport
(LAX) in CA, USA you would start with their latitude and longitude,
commonly given as

```text
BNA: N 36 degrees 7.2',   W 86 degrees 40.2'
LAX: N 33 degrees 56.4',  W 118 degrees 24.0'
```

which converted to floating-point values in degrees is:

```text
     Latitude Longitude

   - BNA
     36.12, -86.67

   - LAX
     33.94, -118.40
```

And then use the haversine formula to roughly calculate the distance
along the surface of the Earth between the locations:

 Sample program:

```fortran
program demo_sin
implicit none
real :: d
    d = haversine(36.12,-86.67, 33.94,-118.40) ! BNA to LAX
    print '(A,F9.4,A)', 'distance: ',d,' km'
contains
function haversine(latA,lonA,latB,lonB) result (dist)
!
! calculate great circle distance in kilometers
! given latitude and longitude in degrees
!
real,intent(in) :: latA,lonA,latB,lonB
real :: a,c,dist,delta_lat,delta_lon,lat1,lat2
real,parameter :: radius = 6371 ! mean earth radius in kilometers,
! recommended by the International Union of Geodesy and Geophysics

! generate constant pi/180
real, parameter :: deg_to_rad = atan(1.0)/45.0
   delta_lat = deg_to_rad*(latB-latA)
   delta_lon = deg_to_rad*(lonB-lonA)
   lat1 = deg_to_rad*(latA)
   lat2 = deg_to_rad*(latB)
   a = (sin(delta_lat/2))**2 + &
          & cos(lat1)*cos(lat2)*(sin(delta_lon/2))**2
   c = 2*asin(sqrt(a))
   dist = radius*c
end function haversine
end program demo_sin
```
Results:

```text
    distance: 2886.4446 km
```

## __Standard__

FORTRAN 77 and later

## __See Also__
- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

[__asin__(3)](ASIN),
[__cos__(3)](COS),
[__tan__(3)](TAN)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# SIZE

## __Name__

__size__(3) - \[ARRAY INQUIRY\] Determine the size of an array

## __Syntax__
```fortran
result = size(array, dim, kind)
```
## __Description__

Determine the extent of __array__ along a specified dimension __dim__,
or the total number of elements in __array__ if __dim__ is absent.

## __Arguments__

  - __array__
    : be an array of any type. If __array__ is a pointer it must be
    associated and allocatable arrays must be allocated.

  - __dim__
    : shall be a scalar of type _integer_ and its value shall be
    in the range from 1 to n, where n equals the rank of __array__.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__
is absent, the return value is of default _integer_ kind.

## __Examples__

Sample program:

```fortran
program demo_size
implicit none
integer :: i, j
integer :: arr(0:2,-5:5)=reshape([(((i-1)*11+j,i=1,3),j=1,11)],[3,11])
   write(*,*) 'SIZE of simple one-dimensional array=', &
   & size([ 11, 22, 33 ])    ! 3

   write(*,*)'body'
   write(*,*)'SHAPE(arr)       :',shape(arr)
   write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
   write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is not "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
   write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)

   call interfaced(arr,arr)
   call nointerface(arr)
contains

subroutine interfaced(arr,arr2)
integer,intent(in)  :: arr(:,:)
integer,intent(in)  :: arr2(2,*)
   !
   write(*,*)'interfaced assumed-shape arr2ay'
   !
   ! source argument of shape intrinsic at (1) must not be
   ! an assumed size array
   !!write(*,*)'SHAPE(arr2)       :',shape(arr2)
   ! The upper bound in the last dimension must appear in the reference
   ! to the assumed size array    arr2    at (1)
   !!write(*,*)'SIZE(arr2)        :',size(arr2)
   write(*,*)'SIZE(arr2,DIM=1)  :',size(arr2,dim=1)
   !    dim    argument of    size    intrinsic at (1) is not
   !a valid dimension index
   !!write(*,*)'SIZE(arr2,DIM=2)  :',size(arr2,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr2)      :',lbound(arr2)
   write(*,*)'LBOUND(arr2)      :',lbound(arr2)
   ! The upper bound in the last dimension must appear in the
   ! reference to the assumed size array    arr2    at (1)
   !!write(*,*)'UBOUND(arr2)      :',ubound(arr2)
   write(*,*)'LBOUND(arr2,DIM=1):',lbound(arr2,dim=1)
   write(*,*)'UBOUND(arr2,DIM=1):',ubound(arr2,dim=1)
   write(*,*)'LBOUND(arr2,DIM=2):',lbound(arr2,dim=2)
   !    dim    argument of    ubound    intrinsic at (1) is not
   ! a valid dimension index
   !!write(*,*)'UBOUND(arr2,DIM=2):',ubound(arr2,dim=2)
   !
   write(*,*)'interfaced'
   !
   write(*,*)'SHAPE(arr)       :',shape(arr)
   write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
   write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
   write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
   !
end subroutine interfaced
!!
! NOTE: If NOINTERFACE(3) had an assumed-shape argument with :
!       for dimensions it could only be properly called with
!       an explicit interface
!!
subroutine nointerface(arr)
integer,intent(in) :: arr(3,*)
   write(*,*)'nointerface'
 ! SHAPE(3) CANNOT BE USED ON AN ASSUMED SIZE ARRAY
 !!write(*,*)'SHAPE(arr)       :',shape(arr)
 !!write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
 ! CANNOT DETERMINE SIZE OF ASSUMED SIZE ARRAY LAST DIMENSION
 !!write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
 !!write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
 !!write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
end subroutine nointerface
!!
end program demo_size
```
Results:
```text
    SIZE of simple one-dimensional array=           3
    body
    SHAPE(arr)       :           3          11
    SIZE(arr)        :          33
    SIZE(arr,DIM=1)  :           3
    SIZE(arr,DIM=2)  :          11
    note lower bound is not "1"
    LBOUND(arr)      :           0          -5
    UBOUND(arr)      :           2           5
    LBOUND(arr,DIM=1):           0
    UBOUND(arr,DIM=1):           2
    LBOUND(arr,DIM=2):          -5
    UBOUND(arr,DIM=2):           5
    interfaced assumed-shape arr2ay
    SIZE(arr2,DIM=1)  :           2
    note lower bound is "1"
    LBOUND(arr2)      :           1           1
    LBOUND(arr2)      :           1           1
    LBOUND(arr2,DIM=1):           1
    UBOUND(arr2,DIM=1):           2
    LBOUND(arr2,DIM=2):           1
    interfaced
    SHAPE(arr)       :           3          11
    SIZE(arr)        :          33
    SIZE(arr,DIM=1)  :           3
    SIZE(arr,DIM=2)  :          11
    note lower bound is "1"
    LBOUND(arr)      :           1           1
    LBOUND(arr)      :           1           1
    UBOUND(arr)      :           3          11
    LBOUND(arr,DIM=1):           1
    UBOUND(arr,DIM=1):           3
    LBOUND(arr,DIM=2):           1
    UBOUND(arr,DIM=2):          11
    nointerface
    SIZE(arr,DIM=1)  :           3
    note lower bound is "1"
    LBOUND(arr)      :           1           1
    LBOUND(arr,DIM=1):           1
    UBOUND(arr,DIM=1):           3
    LBOUND(arr,DIM=2):           1
```
## __Standard__

Fortran 95 and later, with __kind__ argument - Fortran 2003 and later

## __See Also__

[__shape__(3)](SHAPE),
[__reshape__(3)])(RESHAPE)

###### fortran-lang intrinsic descriptions
# SPACING

## __Name__

__spacing__(3) - \[MODEL\_COMPONENTS\] Smallest distance between two numbers of a given type

## __Syntax__
```fortran
result = spacing(x)
```
## __Description__

Determines the distance between the argument __x__ and the nearest adjacent
number of the same type.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The result is of the same type as the input argument __x__.

## __Examples__

Sample program:

```fortran
program demo_spacing
implicit none
integer, parameter :: sgl = selected_real_kind(p=6, r=37)
integer, parameter :: dbl = selected_real_kind(p=13, r=200)

   write(*,*) spacing(1.0_sgl)      ! "1.1920929e-07"          on i686
   write(*,*) spacing(1.0_dbl)      ! "2.220446049250313e-016" on i686
end program demo_spacing
```
  Results:
```text
      1.19209290E-07
      2.2204460492503131E-016
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# SPREAD

## __Name__

__spread__(3) - \[ARRAY CONSTRUCTION\] Add a dimension to an array

## __Syntax__
```fortran
result = spread(source, dim, ncopies)

  TYPE(kind=KIND) function spread(source, dim, ncopies)

   TYPE(kind=KIND)    :: source(..)
   integer,intent(in) :: dim
   integer,intent(in) :: ncopies
```
## __Description__

Replicates a __source__ array __ncopies__ times along a specified
dimension __dim__.

If SOURCE is scalar, the shape of the result is (MAX (NCOPIES, 0)).
and each element of the result has a value equal to SOURCE.

## __Arguments__

  - __source__
    : Shall be a scalar or an array of any type and a rank less than
    fifteen.

  - __dim__
    : Shall be a scalar of type _integer_ with a value in the range from
    __1__ to __n+1__, where __n__ equals the rank of __source__.

  - __ncopies__
    : Shall be a scalar of type _integer_.

## __Returns__

The result is an array of the same type as __source__ and has rank __n+1__
where __n__ equals the rank of __source__.

## __Examples__

Sample program:

```fortran
program demo_spread
implicit none
integer :: a = 1, b(2) = [ 1, 2 ]

   write(*,*) spread(a, 1, 2)            ! "1 1"
   write(*,*) spread(b, 1, 2)            ! "1 1 2 2"

end program demo_spread

program example_spread
!  Author:
!    John Burkardt, 03 July 2006
implicit none
     !
integer ( kind = 4 ) a1(4,3)
integer ( kind = 4 ) a2(3,4)
integer i
integer ( kind = 4 ) s
integer ( kind = 4 ) v(4)
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) 'TEST_SPREAD'
     write ( *, '(a)' ) '  SPREAD is a FORTRAN90 function which replicates'
     write ( *, '(a)' ) '  an array by adding a dimension.'
     write ( *, '(a)' ) ' '
     !
     s = 99
     !
     write ( *, '(a,i6)' ) '  Suppose we have a scalar S = ', s
     write ( *, '(a)' ) ' '
     !
     v = spread ( s, 1, 4 )
     !
     write ( *, '(a)' ) '  V = spread ( s, 1, 4 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (1) of extent 4'
     write ( *, '(a)' ) ' '
     write ( *, '(4i6)' ) v(1:4)
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  Now first reset V to (1,2,3,4)'
     v = [ 1, 2, 3, 4 ]
     !
     a1 = spread ( v, 2, 3 )
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  A1 = spread ( v, 2, 3 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (2) of extent 3'
     write ( *, '(a)' ) ' '
     do i = 1, 4
       write ( *, '(3i6)' ) a1(i,1:3)
     end do
     !
     a2 = spread ( v, 1, 3 )
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  A2 = spread ( v, 1, 3 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (1) of extent 3'
     write ( *, '(a)' ) ' '
     do i = 1, 3
       write ( *, '(4i6)' ) a2(i,1:4)
     end do
end program example_spread
```

## __Standard__

Fortran 95 and later

## __See Also__

[__pack__(3)](PACK),
[__unpack__(3)](UNPACK),
[__merge__(3)](MERGE),
[__pack__(3)](PACK),
[__unpack__(3)](UNPACK)

###### fortran-lang intrinsic descriptions
# SQRT

## __Name__

__sqrt__(3) - \[MATHEMATICS\] Square-root function

## __Syntax__
```fortran
result = sqrt(x)

   TYPE(kind=KIND) elemental function sqrt(x) result(value)
   TYPE(kind=KIND),intent(in) :: x
   TYPE(kind=KIND) :: value
```
Where TYPE may be _real_ or _complex_ and __KIND__ may be any
kind valid for the declared type.

## __Description__

__sqrt(x)__ computes the principal square root of __x__.

In mathematics, a square root of a number __x__ is a number __y__ such
that __y*y = x__.

The number whose square root is being considered is known as the
_radicand_.

Every nonnegative  number _x_ has two square roots of the same unique
magnitude, one positive and one negative. The nonnegative square root
is called the principal square root.

The principal square root of 9 is 3, for example, even though (-3)*(-3)
is also 9.

A _real_, _radicand_ must be positive.

Square roots of negative numbers are a special case of complex numbers,
where the components of the _radicand_ need not be positive in order to
have a valid square root.

## __Arguments__

  - __x__
    : If __x__ is real its value must be greater than or equal to zero.
    The type shall be _real_ or _complex_.

## __Returns__

The return value is of type _real_ or _complex_. The kind type parameter is
the same as __x__.

## __Examples__

Sample program:

```fortran
program demo_sqrt
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x, x2
complex :: z, z2

   x = 2.0_real64
   z = (1.0, 2.0)
   write(*,*)x,z

   x2 = sqrt(x)
   z2 = sqrt(z)
   write(*,*)x2,z2

   x2 = x**0.5
   z2 = z**0.5
   write(*,*)x2,z2

end program demo_sqrt
```
  Results:
```text
  2.0000000000000000    (1.00000000,2.00000000)
  1.4142135623730951    (1.27201962,0.786151350)
  1.4142135623730951    (1.27201962,0.786151350)
```

## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# STORAGE\_SIZE

## __Name__

__storage\_size__(3) - \[BIT:INQUIRY\] Storage size in bits

## __Syntax__
```fortran
result = storage_size(a, kind)
```
## __Description__

Returns the storage size of argument __a__ in bits.

## __Arguments__

  - __a__
    : Shall be a scalar or array of any type.

  - __kind__
    : (Optional) shall be a scalar integer constant expression.

## __Returns__

The result is a scalar integer with the kind type parameter specified by
__kind__ (or default integer type if __kind__ is missing). The result value is
the size expressed in bits for an element of an array that has the
dynamic type and type parameters of __a__.

## __Examples__

Sample program

```fortran
program demo_storage_size
implicit none
   write(*,*)'size of integer       ',storage_size(0)
   write(*,*)'size of real          ',storage_size(0.0)
   write(*,*)'size of logical       ',storage_size(.true.)
   write(*,*)'size of complex       ',storage_size((0.0,0.0))
   write(*,*)'size of integer array ',storage_size([0,1,2,3,4,5,6,7,8,9])
end program demo_storage_size
```
  Results:
```text
    size of integer                 32
    size of real                    32
    size of logical                 32
    size of complex                 64
    size of integer array           32
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__c\_sizeof__(3)](C_SIZEOF)

###### fortran-lang intrinsic descriptions
# SUM

## __Name__

__sum__(3) - \[ARRAY REDUCTION\] sum the elements of an array

## __Syntax__
```fortran
   result = sum(array[, mask])
   result = sum(array, dim[, mask])
```
## __Description__

Adds the elements of ARRAY along dimension DIM if the corresponding
element in MASK is TRUE.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_ or _complex_.

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from 1 to n, where n equals the rank of ARRAY.

  - __mask__
    : (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as ARRAY.

## __Returns__

The result is of the same type as ARRAY.

If __dim__(3) is absent, a scalar with the sum of all elements in ARRAY
is returned. Otherwise, an array of rank n-1, where n equals the rank of
ARRAY, and a shape similar to that of ARRAY with dimension DIM dropped
is returned.

## __Examples__

Sample program:

```fortran
program simple_sum
implicit none
integer :: x(5) = [ 1, 2, 3, 4 ,5 ]
   print *, sum(x)                        ! all elements, sum = 15
   print *, sum(x, mask=mod(x, 2)==1)     ! odd elements, sum = 9
end program simple_sum
```
Demonstrate Fortran 90 SUM function with MASK option

```fortran
program demo_sum
! John Mahaffy  2/16/96
implicit none
integer nd, ndh, nduh, j
parameter (nd=10,ndh=nd/2, nduh=nd-ndh)
real csum, cpsum, cbpsum
real, dimension(nd):: c=[(j, j=-1,nd-2)], b
data b/ndh*-1.0, nduh*2.0/
   csum= sum(c(1:nd))
   cpsum= sum (c(1:nd), mask=c.gt.0)
   cbpsum= sum(c(1:nd), mask=b.gt.0.0)
   print *, 'Sum of all elements in c = ' , csum
   print *, 'Sum of Positive elements in c = ', cpsum
   print *, 'Sum of elements in c when corresponding elements in b>0', &
   & ' =', cbpsum
end program demo_sum
```

Results:

```text
 Sum of all elements in c =    35.0000000
 Sum of Positive elements in c =    36.0000000
 Sum of elements in c when corresponding elements in b>0 =   30.0000000
```

## __Standard__

Fortran 95 and later

## __See Also__

intrinsics

###### fortran-lang intrinsic descriptions
# SYSTEM\_CLOCK

## __Name__

__system\_clock__(3) - \[SYSTEM:TIME\] Return numeric data from a real-time clock.

## __Syntax__
```fortran
subroutine system_clock(count, count_rate, count_max)

   integer,intent(out),optional  :: count
   integer,intent(out),optional  :: count_rate
    ! or !
   real,intent(out),optional     :: count_rate
   integer,intent(out),optional  :: count_max
```
## __Description__

__system\_clock__ lets you measure durations of time with the precision of
the smallest time increment generally available on a system by returning
processor-dependent values based on the current value of the processor
clock. The __clock__ value is incremented by one for each clock count until
the value __count\_max__ is reached and is then reset to zero at the next
count. __clock__ therefore is a modulo value that lies in the range __0 to
count\_max__. __count\_rate__ and __count\_max__ are assumed constant (even though
CPU rates can vary on a single platform).

__count\_rate__ is system dependent and can vary depending on the kind of
the arguments.

If there is no clock, or querying the clock fails, __count__ is set to
__-huge(count)__, and __count\_rate__ and __count\_max__ are set to zero.

__system\_clock__ is typically used to measure short time intervals (system
clocks may be 24-hour clocks or measure processor clock ticks since
boot, for example). It is most often used for measuring or tracking the
time spent in code blocks in lieu of using profiling tools.

## __Arguments__

  - __count__
    : (optional) shall be an _integer_ scalar. It is assigned a
    processor-dependent value based on the current value of the
    processor clock, or __-huge(count)__ if there is no clock. The
    processor-dependent value is incremented by one for each clock count
    until the value __count\_max__ is reached and is reset to zero at the
    next count. It lies in the range __0__ to __count\_max__ if there is a
    clock.

  - __count\_rate__
    : (optional) shall be an _integer_ or _real_ scalar. It is assigned a
    processor-dependent approximation to the number of processor clock
    counts per second, or zero if there is no clock.

  - __count\_max__
    : (optional) shall be an _integer_ scalar. It is assigned the maximum
    value that __COUNT__ can have, or zero if there is no clock.

## __Examples__

Sample program:
```fortran
program demo_system_clock
implicit none
integer, parameter :: wp = kind(1.0d0)
integer :: count, count_rate, count_max
integer :: start, finish
real    :: time_read

   call system_clock(count, count_rate, count_max)
   write(*,*) count, count_rate, count_max

   call system_clock(start, count_rate)
   ! <<<< code to time
   call system_clock(finish)
   time_read=(finish-start)/real(count_rate,wp)
   write(*,'(a30,1x,f7.4,1x,a)') 'time * : ', time_read, ' seconds'

end program demo_system_clock
```
If the processor clock is a 24-hour clock that registers time at
approximately 18.20648193 ticks per second, at 11:30 A.M. the reference
```fortran
      call system_clock (count = c, count_rate = r, count_max = m)
```
defines

```text
      C = (11*3600+30*60)*18.20648193 = 753748,
      R = 18.20648193, and
      M = 24*3600*18.20648193-1 = 1573039.
```
## __Standard__

Fortran 95 and later

## __See Also__

[__date\_and\_time__(3)](DATE_AND_TIME),
[__cpu\_time__(3)](CPU_TIME)

###### fortran-lang intrinsic descriptions
# TANH

## __Name__

__tanh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic tangent function

## __Syntax__
```fortran
x = tanh(x)
```
## __Description__

__tanh(x)__ computes the hyperbolic tangent of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__. If __x__ is complex, the
imaginary part of the result is in radians. If __x__ is _real_, the return
value lies in the range

```
      -1 <= tanh(x) <= 1.
```
## __Examples__

Sample program:

```fortran
program demo_tanh
use, intrinsic :: iso_fortran_env, only : &
& real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 2.1_real64
   write(*,*)x, tanh(x)
end program demo_tanh
```
  Results:
```text
      2.1000000000000001       0.97045193661345386
```
## __Standard__

FORTRAN 77 and later, for a complex argument Fortran 2008 or later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

[__atanh__(3)](ATANH)

###### fortran-lang intrinsic descriptions
# TAN

## __Name__

__tan__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Tangent function

## __Syntax__
```fortran
result = tan(x)
```
## __Description__

__tan(x)__ computes the tangent of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has the same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_tan
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real64) :: x = 0.165_real64
     write(*,*)x, tan(x)
end program demo_tan
```
  Results:
```text
     0.16500000000000001       0.16651386310913616
```
## __Standard__

FORTRAN 77 and later. For a complex argument, Fortran 2008 or later.

## __See Also__

[__atan__(3)](ATAN),
[__cos__(3)](COS),
[__sin__(3)](SIN)

###### fortran-lang intrinsic descriptions
# THIS\_IMAGE

## __Name__

__this\_image__(3) - \[COLLECTIVE\] Cosubscript index of this image

## __Syntax__
```fortran
result = this_image() result = this_image(distance) &
         & result = this_image(coarray, dim)
```
## __Description__

Returns the cosubscript for this image.

## __Arguments__

  - __distance__
    : (optional, __intent(in)__) Nonnegative scalar integer (not permitted
    together with __coarray__).

  - __coarray__
    : Coarray of any type (optional; if __dim__ present, required).

  - __dim__
    : default integer scalar (optional). If present, __dim__ shall be between
    one and the corank of __coarray__.

## __Returns__

Default integer. If __coarray__ is not present, it is scalar; if __distance__ is
not present or has value __0__, its value is the image index on the invoking
image for the current team, for values smaller or equal distance to the
initial team, it returns the image index on the ancestor team which has
a distance of __distance__ from the invoking team. If __distance__ is larger
than the distance to the initial team, the image index of the initial
team is returned. Otherwise when the __coarray__ is present, if __dim__ is not
present, a rank-1 array with corank elements is returned, containing the
cosubscripts for __coarray__ specifying the invoking image. If __dim__ is
present, a scalar is returned, with the value of the __dim__ element of
__this\_image(coarray)__.

## __Examples__

Sample program:

```fortran
program demo_this_image
implicit none
integer :: value[*]
integer :: i
   value = this_image()
   sync all
   if (this_image() == 1) then
      do i = 1, num_images()
         write(*,'(2(a,i0))') 'value[', i, '] is ', value[i]
      end do
   endif
end program demo_this_image
```
  Results:
```text
   value[1] is 1
```
!
! Check whether the current image is the initial image
if (this_image(huge(1)) /= this_image())
error stop "something is rotten here"
```

## __Standard__

Fortran 2008 and later. With DISTANCE argument, TS 18508
or later

## __See Also__

[__num\_images__(3)](NUM_IMAGES),
[__image\_index__(3)](IMAGE_INDEX)

###### fortran-lang intrinsic descriptions
# TINY

## __Name__

__tiny__(3) - \[NUMERIC MODEL\] Smallest positive number of a real kind

## __Syntax__
```fortran
result = tiny(x)
   real(kind=KIND) function(x)
   real(kind=KIND) :: x
```
  where KIND may be any kind supported by type _real_

## __Description__

__tiny(x)__ returns the smallest positive (non zero) number of the type
and kind of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The smallest positive value for the _real_ type of the specified kind.

The return value is of the same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_tiny
implicit none
   print *, 'default real is from',tiny(0.0) ,'to',huge(0.0)
   print *, 'doubleprecision is from ',tiny(0.0d0),'to',huge(0.0d0)
end program demo_tiny
```
Results:
```text
 default real is from 1.17549435E-38 to 3.40282347E+38
 doubleprecision is from 2.2250738585072014E-308 to 1.7976931348623157E+308
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING)

###### fortran-lang intrinsic descriptions
# TRAILZ

## __Name__

__trailz__(3) - \[BIT:COUNT\] Number of trailing zero bits of an integer

## __Syntax__
```fortran
   result = trailz(i) integer :: result
   integer(kind=NNN),intent(in) :: i
```
## __Description__

__trailz(3)__ returns the number of trailing zero bits of an _integer_ value

## __Arguments__

  - __i__
    : Shall be of type _integer_.

## __Returns__

The type of the return value is the default _integer_. If all the bits of
I are zero, the result value is __bit\_size(i)__.

## __Examples__

Sample program:

```fortran
program demo_trailz
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
& int8, int16, int32, int64
implicit none
integer(kind=int64) :: i, value
   write(*,*)'Default integer:'
   write(*,*)'bit_size=',bit_size(0)
   write(*,'(1x,i3,1x,i3,1x,b0)')-1,trailz(1),-1
   write(*,'(1x,i3,1x,i3,1x,b0)')0,trailz(0),0
   write(*,'(1x,i3,1x,i3,1x,b0)')1,trailz(1),1
   write(*,'(" huge(0)=",i0,1x,i0,1x,b0)') &
   & huge(0),trailz(huge(0)),huge(0)
   write(*,*)
   write(*,*)'integer(kind=int64):'

   do i=-1,62,5
      value=2**i
      write(*,'(1x,i19,1x,i3)')value,trailz(value)
   enddo
   value=huge(i)
   write(*,'(1x,i19,1x,i3,"(huge(0_int64))")')value,trailz(value)

   do i=-1,62,5
      value=2**i
      write(*,'(1x,i3,2x,b64.64)')i,value
   enddo
   value=huge(i)
   write(*,'(1x,a,1x,b64.64)') "huge",value

end program demo_trailz
```

Results:

```
 Default integer:
 bit_size=          32
  -1   0 11111111111111111111111111111111
   0  32 0
   1   0 1
 huge(0)=2147483647 0 1111111111111111111111111111111

 integer(kind=int64):
                   0  64
                  16   4
                 512   9
               16384  14
              524288  19
            16777216  24
           536870912  29
         17179869184  34
        549755813888  39
      17592186044416  44
     562949953421312  49
   18014398509481984  54
  576460752303423488  59
 9223372036854775807   0(huge(0_int64))
  -1  0000000000000000000000000000000000000000000000000000000000000000
   4  0000000000000000000000000000000000000000000000000000000000010000
   9  0000000000000000000000000000000000000000000000000000001000000000
  14  0000000000000000000000000000000000000000000000000100000000000000
  19  0000000000000000000000000000000000000000000010000000000000000000
  24  0000000000000000000000000000000000000001000000000000000000000000
  29  0000000000000000000000000000000000100000000000000000000000000000
  34  0000000000000000000000000000010000000000000000000000000000000000
  39  0000000000000000000000001000000000000000000000000000000000000000
  44  0000000000000000000100000000000000000000000000000000000000000000
  49  0000000000000010000000000000000000000000000000000000000000000000
  54  0000000001000000000000000000000000000000000000000000000000000000
  59  0000100000000000000000000000000000000000000000000000000000000000
 huge 0111111111111111111111111111111111111111111111111111111111111111
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bit\_size__(3)](BIT_SIZE),
[__popcnt__(3)](POPCNT),
[__poppar__(3)](POPPAR),
[__leadz__(3)](LEADZ)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# TRANSFER

## __Name__

__transfer__(3) - \[TYPE:MOLD\] Transfer bit patterns

## __Syntax__
```fortran
result = transfer(source, mold, size)
```
## __Description__

Interprets the bitwise representation of __source__ in memory as if it
is the representation of a variable or array of the same type and type
parameters as __mold__.

This is approximately equivalent to the C concept of \*casting\* one
type to another.

## __Arguments__

  - __source__
    : Shall be a scalar or an array of any type.

  - __mold__
    : Shall be a scalar or an array of any type.

  - __size__
    : (Optional) shall be a scalar of type _integer_.

## __Returns__

The result has the same type as __mold__, with the bit level representation
of __source__. If __size__ is present, the result is a one-dimensional array of
length __size__. If __size__ is absent but __mold__ is an array (of any size or
shape), the result is a one-dimensional array of the minimum length
needed to contain the entirety of the bitwise representation of __source__.
If __size__ is absent and __mold__ is a scalar, the result is a scalar.

If the bitwise representation of the result is longer than that of
__source__, then the leading bits of the result correspond to those of
__source__ and any trailing bits are filled arbitrarily.

When the resulting bit representation does not correspond to a valid
representation of a variable of the same type as __mold__, the results are
undefined, and subsequent operations on the result cannot be guaranteed
to produce sensible behavior. For example, it is possible to create
_logical_ variables for which __var__ and .not. var both appear to be true.

## __Examples__

Sample program:
```fortran
program demo_transfer
use,intrinsic :: iso_fortran_env, only : int32, real32
integer(kind=int32) :: i = 2143289344
real(kind=real32)   :: x
character(len=10)   :: string
character(len=1)    :: chars(10)
   x=transfer(i, 1.0)    ! prints "nan" on i686
   ! the bit patterns are the same
   write(*,'(b0,1x,g0)')x,x ! create a NaN
   write(*,'(b0,1x,g0)')i,i

   ! a string to an array of characters
   string='abcdefghij'
   chars=transfer(string,chars)
   write(*,'(*("[",a,"]":,1x))')string
   write(*,'(*("[",a,"]":,1x))')chars
end program demo_transfer
```
Results:
```text
   1111111110000000000000000000000 NaN
   1111111110000000000000000000000 2143289344
   [abcdefghij]
   [a] [b] [c] [d] [e] [f] [g] [h] [i] [j]
```
## __Comments__

_Joe Krahn_: Fortran uses __molding__ rather than __casting__.

Casting, as in C, is an in-place reinterpretation. A cast is a device
that is built around an object to change its shape.

Fortran TRANSFER reinterprets data out-of-place. It can be considered
__molding__ rather than casting. A __mold__ is a device that
confers a shape onto an object placed into it.

The advantage of molding is that data is always valid in the context
of the variable that holds it. For many cases, a decent compiler should
optimize TRANSFER into a simple assignment.

There are disadvantages of this approach. It is problematic to define a
union of data types because you must know the largest data object, which
can vary by compiler or compile options. In many cases, an EQUIVALENCE
would be far more effective, but Fortran Standards committees seem
oblivious to the benefits of EQUIVALENCEs when used sparingly.

## __Standard__

Fortran 90 and later

###### fortran-lang intrinsic descriptions
# TRANSPOSE

## __Name__

__transpose__(3) - \[ARRAY MANIPULATION\] Transpose an array of rank two

## __Syntax__
```fortran
result = transpose(matrix)
```
## __Description__

Transpose an array of rank two. Element (i, j) of the result has the
value __matrix(j, i)__, for all i, j.

## __Arguments__

  - __matrix__
    : Shall be an array of any type and have a rank of two.

## __Returns__

The result has the same type as __matrix__, and has shape \[ m, n \] if
__matrix__ has shape \[ n, m \].

## __Examples__

Sample program:

```fortran
program demo_transpose
implicit none
integer,save :: xx(3,5)= reshape([&
    1,  2,  3,  4,  5,    &
   10, 20, 30, 40, 50,    &
   11, 22, 33, 44, -1055  &
 ],shape(xx),order=[2,1])

call print_matrix_int('xx array:',xx)
call print_matrix_int('xx array transposed:',transpose(xx))

contains

subroutine print_matrix_int(title,arr)
! print small 2d integer arrays in row-column format
implicit none
character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:,:)
integer                      :: i
character(len=:),allocatable :: biggest
   write(*,*)trim(title)  ! print title
   biggest='           '  ! make buffer to write integer into
   ! find how many characters to use for integers
   write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
   ! use this format to write a row
   biggest='(" > [",*(i'//trim(biggest)//':,","))'
   ! print one row of array at a time
   do i=1,size(arr,dim=1)
      write(*,fmt=biggest,advance='no')arr(i,:)
      write(*,'(" ]")')
   enddo
end subroutine print_matrix_int

end program demo_transpose
```

Results:

```
    xx array:
    > [     1,     2,     3,     4,     5 ]
    > [    10,    20,    30,    40,    50 ]
    > [    11,    22,    33,    44, -1055 ]
    xx array transposed:
    > [     1,    10,    11 ]
    > [     2,    20,    22 ]
    > [     3,    30,    33 ]
    > [     4,    40,    44 ]
    > [     5,    50, -1055 ]
```

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# TRIM

## __Name__

__trim__(3) - \[CHARACTER:WHITESPACE\] Remove trailing blank characters of a string

## __Syntax__
```fortran
result = trim(string)
```
## __Description__

Removes trailing blank characters of a string.

## __Arguments__

  - __string__
    : Shall be a scalar of type _character_.

## __Returns__

A scalar of type _character_ which length is that of __string__ less the
number of trailing blanks.

## __Examples__

Sample program:

```fortran
program demo_trim
implicit none
character(len=10), parameter :: s = "gfortran  "
   write(*,*) len(s), len(trim(s))  ! "10 8", with/without trailing blanks

   ! with/without trailing blanks
   write(*,*) len(s), len(trim('   leading'))
   write(*,*) len(s), len(trim('   trailing    '))
   write(*,*) len(s), len(trim('               '))

end program demo_trim
```
Results:
```text
      10           8
      10          10
      10          11
      10           0
```
## __Standard__

Fortran 95 and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
 [__adjustl__(3)](ADJUSTL),
 [__adjustr__(3)](ADJUSTR),
 [__index__(3)](INDEX),

 [__scan__(3)](SCAN),
 [__verify__(3)](VERIFY)

  - __Nonelemental:__
 [__len\_trim__(3)](LEN_TRIM),
 [__len__(3)](LEN),
 [__repeat__(3)](REPEAT),
 [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# UBOUND

## __Name__

__ubound__(3) - \[ARRAY INQUIRY\] Upper dimension bounds of an array

## __Syntax__
```fortran
result = ubound(array, dim, kind)
```
## __Description__

Returns the upper bounds of an array, or a single upper bound along the
__dim__ dimension.

## __Arguments__

  - __array__
    : Shall be an array, of any type.

  - __dim__
    : (Optional) Shall be a scalar _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__
is absent, the return value is of default integer kind.

If __dim__ is absent, the result is an array of the upper bounds of
__array__.

If __dim__ is present, the result is a scalar corresponding to the upper
bound of the array along that dimension.

If __array__ is an expression rather than a whole array or array
structure component, or if it has a zero extent along the relevant
dimension, the upper bound is taken to be the number of elements along
the relevant dimension.

## __Examples__

Note this function should not be used on assumed-size arrays or in any
function without an explicit interface. Errors can occur if there is no
interface defined.

Sample program

```fortran
! program demo_ubound
module m2_bounds
implicit none

contains

subroutine msub(arr)
!!integer,intent(in) :: arr(*)  ! cannot be assumed-size array
integer,intent(in) :: arr(:)
   write(*,*)'MSUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine msub

end module m2_bounds

use m2_bounds, only : msub
implicit none
interface
   subroutine esub(arr)
   integer,intent(in) :: arr(:)
   end subroutine esub
end interface
integer :: arr(-10:10)
   write(*,*)'MAIN: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
   call csub()
   call msub(arr)
   call esub(arr)
contains
subroutine csub
   write(*,*)'CSUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine csub

end

subroutine esub(arr)
implicit none
integer,intent(in) :: arr(:)
   ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE
   ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)
   write(*,*)'ESUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine esub
!end program demo_ubound
```
Results:
```text
  MAIN: LOWER=         -10 UPPER=          10 SIZE=          21
  CSUB: LOWER=         -10 UPPER=          10 SIZE=          21
  MSUB: LOWER=           1 UPPER=          21 SIZE=          21
  ESUB: LOWER=           1 UPPER=          21 SIZE=          21
```
## __Standard__

Fortran 95 and later, with KIND argument Fortran 2003
and later

## __See Also__

[__lbound__(3)](LBOUND),
[__co\_ubound__(3)](CO_UBOUND),
[__co\_lbound__(3)(CO_LBOUND)]

###### fortran-lang intrinsic descriptions
# UNPACK

## __Name__

__unpack__(3) - \[ARRAY CONSTRUCTION\] Store the elements of a vector in an array of higher rank

## __Syntax__
```fortran
result = unpack(vector, mask, field)
```
## __Description__

Store the elements of __vector__ in an array of higher rank.

## __Arguments__

  - __vector__
    : Shall be an array of any type and rank one. It shall have at least
    as many elements as __mask__ has __.true.__ values.

  - __mask__
    : Shall be an array of type _logical_.

  - __field__
    : Shall be of the same type as __vector__ and have the same shape as __mask__.

## __Returns__

The resulting array corresponds to __field__ with __.true.__ elements of __mask__
replaced by values from __vector__ in array element order.

## __Examples__

Sample program:

```fortran
program demo_unpack
implicit none
integer :: vector(2)  = [1,1]
logical :: mask(4)  = [ .true., .false., .false., .true. ]
integer :: field(2,2) = 0, unity(2,2)

   ! result: unity matrix
   unity = unpack(vector, reshape(mask, [2,2]), field)
   write(*,*)unity,size(unity),shape(unity)

end program demo_unpack
```
  Results:
```text
              1           0           0           1           4
              2           2
```
## __Standard__

Fortran 95 and later

## __See Also__

[__pack__(3)](PACK),
[__merge__(3)](MERGE),
[__pack__(3)](PACK),
[__spread__(3)](SPREAD),
[__unpack__(3)](UNPACK)

###### fortran-lang intrinsic descriptions
# VERIFY

## __Name__

__verify__(3) - \[CHARACTER:SEARCH\] Scan a string for the absence of a set of characters

## __Syntax__
```fortran
result = verify(string, set, back, kind)

  integer(kind=KIND) elemental function verify(string,set,back,kind)

   character(len=*),intent(in) :: string
   character(len=*),intent(in) :: set
   logical,intent(in),optional :: back
   integer,intent(in),optional :: KIND
```
## __Description__

Verifies that all the characters in __string__ belong to the set of
characters in __set__ by identifying the first character in the string(s)
that is not in the set(s).

If __back__ is either absent or equals __.false.__, this function
returns the position of the leftmost character of __string__ that is
not in __set__.

If __back__ equals __.true.__, the rightmost position is returned.

If all characters of __string__ are found in __set__, the result is zero.

This makes it easy to verify strings are all uppercase or lowercase,
follow a basic syntax, only contain printable characters, and many of the
conditions tested for with the C routines
__isalnum__(3c), __isalpha__(3c), __isascii__(3c), __isblank__(3c),
__iscntrl__(3c), __isdigit__(3c), __isgraph__(3c), __islower__(3c),
__isprint__(3c), __ispunct__(3c), __isspace__(3c), __isupper__(3c),
and __isxdigit__(3c); but for a string as well an an array of characters.

## __Arguments__

  - __string__
    : String to test for characters not in __set__

  - __set__
    : set of characters that are acceptable to find in __string__.

  - __back__
    : If .true. the rightmost position of any character not in __set__
    is returned, otherwise the leftmost position is returned if any
    non-matching character is found.

  - __kind__
    : An _integer_ initialization expression indicating the kind parameter
    of the result.  If __kind__ is absent, the return value is of default
    integer kind.

## __Returns__

The location of a character in __string__ not found in __set__ or zero
if all characters in __string__ are in the test.

## __Examples__

Sample program I:

```fortran
program demo_verify
implicit none
! some useful character sets
character(len=*),parameter :: int='0123456789'
character(len=*),parameter :: hex='abcdef0123456789'
character(len=*),parameter :: low='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter :: upp='ABCDEFGHIJKLMNOPQRSTUVWXYZ'

character(len=20):: string='   Howdy There!'
character(len=6) :: strings(2)=["Howdy ","there!"]
character(len=2) :: sets(2)=["de","gh"]

   ! first non-lowercase non-blank character
   if(verify(string,low).eq.0)then
       write(*,*) '['//string//'] is all lowercase letters'
   else
       write(*,*) '['//string//'] is not all lowercase letters'
   endif

   write(*,'(*(g0))')'first non-blank character in ['&
   & //string//'] is at column ',verify(string, ' ')

   ! NOTE: same as len_trim(3)
   write(*,*)'last non-blank character',verify(string, ' ',back=.true.)

   !! elemental -- using arrays for both strings and for sets

   ! first character in "Howdy" not in "de", and first letter in "there!"
   ! not in "gh"
   write(*,*) verify(strings,sets)

   ! check each string from right to left for non-letter
   write(*,*) 'last non-letter',verify(strings,upp//low,back=.true.)

   ! note character variables in an array have to be of same length
   ! find last non-uppercase character in "Howdy"
   ! and first non-lowercase in "There!"
   write(*,*) verify(strings,[upp,low],back=[.true.,.false.])

   write(*,*) verify("fortran", "", .true.)  ! 7, found 'n'
   ! 0' found none unmatched
   write(*,*) verify("fortran", "nartrof")

    !! CHECK IF STRING IS OF FORM NN-HHHHH
    CHECK : block
       logical                    :: lout
       character(len=80)          :: chars

       chars='32-af43d'
       lout=.true.

       ! are the first two characters integer characters?
       lout = lout.and.(verify(chars(1:2), int) == 0)

       ! is the third character a dash?
       lout = lout.and.(verify(chars(3:3), '-') == 0)

       ! is remaining string a valid representation of a hex value?
       lout = lout.and.(verify(chars(4:8), hex) == 0)

       if(lout)then
          write(*,*)trim(chars),' passed'
       endif

    endblock CHECK
end program demo_verify
```
  Results:
```text
    [   Howdy There!     ] is not all lowercase letters
   first non-blank character in [   Howdy There!     ] is at column 4
    last non-blank character          15
              1           1
    last non-letter           6           6
              6           6
              7
              0
    32-af43d passed
```
Sample program II:

Determine if strings are valid integer representations
```fortran
program fortran_ints
implicit none
integer :: i
character(len=*),parameter :: ints(*)=[character(len=10) :: &
 '+1 ', &
 '3044848 ', &
 '30.40 ', &
 'September ', &
 '1 2 3', &
 '  -3000 ', &
 ' ']

   write(*,'("|",*(g0,"|"))') ints
   write(*,'("|",*(1x,l1,8x,"|"))') isint(ints)

contains

elemental function isint(line) result (lout)
!
! determine if string is a valid integer representation
! ignoring trailing spaces and leading spaces
!
character(len=*),parameter   :: digits='0123456789'
character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   lout=.false.
   ! make sure at least two characters long to simplify tests
   name=adjustl(line)//'  '
   ! blank string
   if( name .eq. '' )return
   ! allow one leading sign
   if( verify(name(1:1),'+-') == 0 ) name=name(2:)
   ! was just a sign
   if( name .eq. '' )return
   lout=verify(trim(name), digits)  == 0
end function isint

end program fortran_ints
```
Results:
```text
|+1       |3044848  |30.40    |September|1 2 3    |  -3000  |         |
| T       | T       | F       | F       | F       | T       | F       |
```

Sample program III:

Determine if strings represent valid Fortran symbol names
```fortran
program fortran_symbol_name
implicit none
integer :: i
character(len=*),parameter :: symbols(*)=[character(len=10) :: &
 'A_ ', &
 '10 ', &
 'September ', &
 'A B', &
 '_A ', &
 ' ']

   write(*,'("|",*(g0,"|"))') symbols
   write(*,'("|",*(1x,l1,8x,"|"))') fortran_name(symbols)

contains

elemental function fortran_name(line) result (lout)
!
! determine if a string is a valid Fortran name
! ignoring trailing spaces (but not leading spaces)
!
character(len=*),parameter   :: int='0123456789'
character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=*),parameter   :: allowed=upper//lower//int//'_'

character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   name=trim(line)
   if(len(name).ne.0)then
      ! first character is alphameric
      lout = verify(name(1:1), lower//upper) == 0  &
       ! other characters are allowed in a symbol name
       & .and. verify(name,allowed) == 0           &
       ! allowable length
       & .and. len(name) <= 63
   else
      lout = .false.
   endif
end function fortran_name

end program fortran_symbol_name
```

Results:

```text
|A_        |10        |September |A B       |_A        |          |
| T        | F        | T        | F        | F        | F        |
```

## __Standard__

Fortran 95 and later, with __kind__ argument - Fortran 2003 and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL),
    [__adjustr__(3)](ADJUSTR),
    [__index__(3)](INDEX),
    [__scan__(3)](SCAN),
    [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT),
    [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
