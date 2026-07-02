module constants
!
!This file contains constants that are used for the FORTRAN 
!implementation of plot-variable-functions.R in the fvstools package.
!
    implicit none
    save

    double precision, parameter :: r_slope = 1.605, f_con = 0.005454154
    double precision, parameter :: pi = 3.141593

end module