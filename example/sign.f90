    program demo_sign
    implicit none
       print *, sign( -12,  1 )
       print *, sign( -12,  0 )
       print *, sign( -12, -1 )

       print *, sign( -12.0, [1.0, 0.0, -1.0] )

    <<<<<<< HEAD
       print *,'can I distinguish 0 from -0? ',sign(1.0,-0.0).ne.sign(1.0, 0.0)
    =======
       print *,  'can I distinguish 0 from -0? ', sign( 1.0, -0.0 ) .ne. sign( 1.0, 0.0 )
    >>>>>>> 9b8cd0c5596d00eea5cc52b465f7b321b7e6c2d5
    end program demo_sign
