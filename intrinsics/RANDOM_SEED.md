---
layout: book
title: random_seed
permalink: /learn/intrinsics/RANDOM_SEED
---
## __Name__
__random\_seed__(3) - \[MATHEMATICS:RANDOM\] Pseudorandom number generator control.

## __Syntax__
```fortran
call random_seed(size, put, get)

   subroutine random_seed(size, put, get)
   integer,intent(out),optional :: size
   integer,intent(in),optional  :: put(*)
   integer,intent(out),optional :: get(*)
```
where the size of put() and get() must be >= SIZE, and the call must
either have no parameters or one.
## __Description__

__random\_seed(3f)__ initializes and/or queries the seed used by the
pseudo-random number generator procedure __random\_number(3f)__.  It can
be used to start a repeatable sequence of pseudorandom values.

To reproduce the same sequence of pseudo\_random values one needs to
provide the same starting point, defined by an array of whole numbers
called the seed. Fortran does not define what random number generator
algorithm should be used by the random\_number(3f) procedure so different
size seeds may be required with different compilers. The __size__
parameter is provided so that this size may be queried generically.

Do not depend on whether random\_number(3f) generates the same or a random
sequence by default the first time it is called. If __random\_seed()__
is called without arguments, it is seeded with random data retrieved
from the operating system. If you want the same sequence each time 
provide the seed array.

Restarts or queries the state of the pseudorandom number generator used
by __random\_number()__.

## __Arguments__

  - __size__
    : specifies the minimum size of the arrays used
      as the seed arrays __put__ and __get__.

  - __put__
    : An array of values used in a processor-dependent manner to define
    a specific sequence of random numbers.

    If no arguments at all  are present, the processor assigns a
    processor-dependent value to the seed.

  - __get__
    : It is assigned the value of the seed when queried.  The values
    may then be used as the __put__ values on a subsequent call to
    __random\_seed() to reset the pseudorandom sequence returned by
    random\_number(3f) to the same sequence.

## __Examples__

Sample program:

```fortran
program demo_random_seed
implicit none
real :: vals(4)
integer, allocatable :: seed(:)
integer :: n

   CALL RANDOM_SEED() ! Processor-dependent initialization

   ! set size of seed array
   call random_seed(size = n)

   ! get the current seed array
   allocate(seed(n))
   call random_seed(get=seed)

   ! generate some random_numbers
   call random_number(vals)
   write (*, *) vals

   ! reset to the same starting point
   call random_seed(put=seed)
   ! to prove vals is really reset to same sequence
   vals=0.0 
   call random_number(vals)
   write (*, *) vals

end program demo_random_seed
```
  Typical Results:
```text
     3.9208680E-07  2.5480442E-02  0.3525161      0.6669145    
     3.9208680E-07  2.5480442E-02  0.3525161      0.6669145    
## __Standard__

Fortran 95 and later

## __See Also__

[__random\_number__(3)](RANDOM_NUMBER),
[__random\_init(3)](RANDOM_INIT)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
