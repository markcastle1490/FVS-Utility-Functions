module constants
!
!This file contains constants that are used for the FORTRAN 
!implementation of plot-variable-functions.R in the fvstools package.
!
    use iso_fortran_env, only: real64
    implicit none
    save

    real(real64), parameter :: r_slope = 1.605_real64
    real(real64), parameter :: f_con = 0.005454154_real64
    real(real64), parameter :: pi = 3.141593_real64

end module