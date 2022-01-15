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
