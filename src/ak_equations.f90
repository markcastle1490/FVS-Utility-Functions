subroutine ak_bratio(ntree, species, dbh, bratio)

    use ak_coefficients, only: bratio_coeffs
    implicit none
    
    integer, intent(in) :: ntree
    integer, intent(in) :: species(ntree)
    double precision, intent(in) :: dbh(ntree)
    double precision, intent(out) :: bratio(ntree)
    
    integer :: i, sp, calc_type
    double precision :: b1, b2
    
    ! Loop through every individual tree record sequentially
    do i = 1, ntree
        sp = species(i)
        
        ! Handle invalid species values safely (default to group 23)
        if (sp < 1 .or. sp > 23) then
            sp = 23
        end if
        
        !Get coefficients
        calc_type = int(bratio_coeffs(sp, 1))
        b1 = bratio_coeffs(sp, 2)
        b2 = bratio_coeffs(sp, 3)
        
        if (calc_type == 1) then
            bratio(i) = (dbh(i) - (b1 * dbh(i)**b2)) / dbh(i)
            
        else if (calc_type == 2) then
            bratio(i) = (b1 + b2 * dbh(i)) / dbh(i)
            
        else
            bratio(i) = (b1 * dbh(i)**b2) / dbh(i)
        end if
        
        !Cap bark ratio
        if (bratio(i) < 0.80d0) then
            bratio(i) = 0.80d0
        else if (bratio(i) > 0.99d0) then
            bratio(i) = 0.99d0
        end if
    end do

end subroutine ak_bratio

subroutine ak_htd_cr(ntree, species, dbh, ht, type_param, result) 
    use ak_coefficients, only: htd_cr_coeffs 
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    implicit none 
    
    integer, intent(in) :: ntree, type_param 
    integer, intent(in) :: species(ntree) 
    double precision, intent(in) :: dbh(ntree), ht(ntree) 
    double precision, intent(out) :: result(ntree) 
    
    integer :: i, sp 
    double precision :: b1, b2, b3, ht_base, mult 

    do i = 1, ntree 
        sp = species(i) 
        if (sp < 1 .or. sp > 23) sp = 23 
        
        b1 = htd_cr_coeffs(sp, 1) 
        b2 = htd_cr_coeffs(sp, 2) 
        b3 = htd_cr_coeffs(sp, 3) 
        
        select case (type_param)
        ! Height from DBH
        case (1)  
            ht_base = 4.5d0 + b1 * (1.0d0 - exp(b2 * dbh(i)))**b3 
            
            if (sp == 14 .or. sp == 21 .or. sp == 23) then 
                mult = 0.45d0 
            else if (sp == 22) then 
                mult = 0.65d0 
            else 
                mult = 1.0d0 
            end if 
            
            result(i) = ht_base * mult 
        
        ! DBH from Height
        case default  
            if ((ht(i) - 4.5d0) <= 0.0d0 .or. b1 <= 0.0d0) then 
                result(i) = ieee_value(0.0d0, ieee_quiet_nan)
            else 
                result(i) = (1.0d0 / b2) * log(1.0d0 - ((ht(i) - 4.5d0) / b1)**(1.0d0 / b3)) 
            end if 
        end select
    end do 

end subroutine ak_htd_cr

