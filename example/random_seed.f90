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
