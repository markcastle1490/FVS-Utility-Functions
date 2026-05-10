program test
implicit none

!Initialize values to send to ba subroutine
real :: crwidth(5) = [15.0, 17.3, 10.0, 12.2, 14.5]
real :: dbh(5) = [5.0, 7.3, 10.0, 12.2, 4.5]
integer :: sorted_idx(5) = [4, 3, 2, 1, 5]
real :: expf(5) = 10.0
real :: ht(5) = [80, 90, 60, 65, 59]
integer :: species(5) = [1, 2, 3, 1, 2]
real :: dbhmin = 0.0
real :: dbhmax = 999.0
real :: htmin = 0.0  
real :: htmax = 999.0
integer :: select_species(1) = 3
real :: ba_ = 0.0, tpa_ = 0.0, qmd_ = 0.0, gmd_ = 0.0
real :: lorey_dia_ = 0.0, zsdi_ = 0.0, cc_ = 0.0, rsdi_ = 0.0, lorey_ht_ = 0.0
real:: top_ht_ = 0.0, top_tpa = 40.0, top_per = 0.0
integer:: ntree = 5
integer:: nsp = 1
integer:: all_species = 0

call ba(dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
    all_species, select_species, ntree, nsp, ba_)

call tpa(dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
    all_species, select_species, ntree, nsp, tpa_)

call qmd(dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
    all_species, select_species, ntree, nsp, qmd_)

call gmd(dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
    all_species, select_species, ntree, nsp, gmd_)

call lorey_dia(dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
    all_species, select_species, ntree, nsp, lorey_dia_)

call zsdi(dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
    all_species, select_species, ntree, nsp, zsdi_)

call cc(crwidth, dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
    all_species, select_species, ntree, nsp, cc_)

call rsdi_stage(dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
    all_species, select_species, ntree, nsp, rsdi_)

call lorey_ht(dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
    all_species, select_species, ntree, nsp, lorey_ht_)

call top_ht(dbh, sorted_idx, expf, ht, top_tpa, top_per, ntree, top_ht_)

write(*, *) 'BA =', ba_
write(*, *) 'TPA =', tpa_
write(*, *) 'QMD =', qmd_
write(*, *) 'GMD =', gmd_
write(*, *) 'BAWTDBH =', lorey_dia_
write(*, *) 'ZSDI =', zsdi_
write(*, *) 'CC =', cc_
write(*, *) 'RSDI =', rsdi_
write(*, *) 'Lorey Ht =', lorey_ht_
write(*, *) 'Top Ht =', top_ht_

read(*, *)

end program test