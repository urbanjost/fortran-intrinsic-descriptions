    program demo_allocated
    use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
    implicit none
    integer :: i = 4
    real(kind=sp), allocatable :: x(:)

       if (allocated(x) .eqv. .false.) allocate(x(i))

    end program demo_allocated
