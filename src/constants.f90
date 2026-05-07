module constants
!
!This file contains constants that are used for the FORTRAN 
!implementation of plot-variable-functions.R in the fvsUtil package.
!
    implicit none
    save

    !Reineke's slope and foresters constant
    real, parameter :: r_slope = 1.605, f_con = 0.005454154
    real, parameter :: pi = 3.141593

end module