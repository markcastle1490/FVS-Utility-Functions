subroutine ba (dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
all_species, select_species, ntree, nsp, ba_)
use constants
implicit none

!###############################################################################
!This subroutine calculates a basal area per acre given input vectors containing
!diameter at breast height and expansion factor values. This attribute can be 
!calculated for user defined size ranges and for select species.
!###############################################################################

!Arguments
integer, intent(in) :: ntree, nsp, all_species
real, intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
integer, intent(in) :: species(ntree), select_species(nsp)
real, intent(in) :: dbhmin, dbhmax, htmin, htmax
real, intent(out) :: ba_
real :: dbh_, expf_, ht_, species_
integer :: i

!intialize ba_ to 0
ba_ = 0.0

!Do the basal area calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ > dbhmax) cycle
    if(ht_ < htmin .or. ht_ > htmax) cycle
    if(all_species /= 0 .and. .not. any(species_ == select_species)) cycle

    ba_ = ba_ + (dbh_**2 * expf_ * f_con)

end do
end subroutine ba

!###############################################################################
!This subroutine calculates trees per acre given input vectors containing 
!diameter at breast height and expansion factor values. This attribute can be 
!calculated for user defined size ranges and for select species.
!###############################################################################

subroutine tpa (dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
all_species, select_species, ntree, nsp, tpa_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, nsp, all_species
real, intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
integer, intent(in) :: species(ntree), select_species(nsp)
real, intent(in) :: dbhmin, dbhmax, htmin, htmax
real, intent(out) :: tpa_
real :: dbh_, expf_, ht_, species_
integer :: i

!intialize tpa_ to 0
tpa_ = 0.0

!Do the basal area calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ > dbhmax) cycle
    if(ht_ < htmin .or. ht_ > htmax) cycle
    if(all_species /= 0 .and. .not. any(species_ == select_species)) cycle

    tpa_ = tpa_ + expf_

end do
end subroutine tpa

!###############################################################################
!This subroutine calculates quadratic mean diameter given input vectors 
!containing diameter at breast height and expansion factor values. This 
!attribute can be calculated for user defined size ranges and for select 
!species.
!###############################################################################

subroutine qmd (dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
all_species, select_species, ntree, nsp, qmd_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, nsp, all_species
real, intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
integer, intent(in) :: species(ntree), select_species(nsp)
real, intent(in) :: dbhmin, dbhmax, htmin, htmax
real, intent(out) :: qmd_
real :: dbh_, expf_, ht_, species_, tpa_, dbhsq
integer :: i

!intialize tvariables
tpa_ = 0.0
dbhsq = 0.0
qmd_ = 0.0

!Determine trees to include in QMD calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ > dbhmax) cycle
    if(ht_ < htmin .or. ht_ > htmax) cycle
    if(all_species /= 0 .and. .not. any(species_ == select_species)) cycle

    dbhsq = dbhsq + dbh_**2 * expf_
    tpa_ = tpa_ + expf_

end do

! Calculate QMD if TPA is greater than 0
if (tpa_ > 0) qmd_ = sqrt(dbhsq / tpa_)

end subroutine qmd

!###############################################################################
!This subroutine calculates generalized mean diameter (Reineke diameter) given 
!input vectors containing diameter at breast height and expansion factor values.
!This attribute can be calculated for user defined size ranges and for select 
!species.
!###############################################################################

subroutine gmd (dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
all_species, select_species, ntree, nsp, gmd_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, nsp, all_species
real, intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
integer, intent(in) :: species(ntree), select_species(nsp)
real, intent(in) :: dbhmin, dbhmax, htmin, htmax
real, intent(out) :: gmd_
real :: dbh_, expf_, ht_, species_, tpa_, gmd_sum
integer :: i

!intialize tvariables
tpa_ = 0.0
gmd_sum = 0.0
gmd_ = 0.0

!Determine trees to include in QMD calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ > dbhmax) cycle
    if(ht_ < htmin .or. ht_ > htmax) cycle
    if(all_species /= 0 .and. .not. any(species_ == select_species)) cycle

    gmd_sum = gmd_sum + dbh_**r_slope * expf_
    tpa_ = tpa_ + expf_

end do

! Calculate GMD if TPA is greater than 0
if (tpa_ > 0) gmd_ = (gmd_sum / tpa_)**(1 / r_slope)

end subroutine gmd

!###############################################################################
!This subroutine calculates Lorey diameter (basal area weighted diameter) given
!input vectors containing diameter at breast height and expansion factor values.
!This attribute can be calculated for user defined size ranges and for select 
!species.
!###############################################################################

subroutine lorey_dia (dbh, expf, ht, species, dbhmin, dbhmax, htmin, &
htmax, all_species, select_species, ntree, nsp, lorey_dia_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, nsp, all_species
real, intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
integer, intent(in) :: species(ntree), select_species(nsp)
real, intent(in) :: dbhmin, dbhmax, htmin, htmax
real, intent(out) :: lorey_dia_
real :: dbh_, expf_, ht_, species_, tpa_, dbh_sum, ba_, treeba(ntree)
real :: ba_tree
integer :: i

!intialize tvariables
tpa_ = 0.0
dbh_sum = 0.0
lorey_dia_ = 0.0
treeba = 0.0

!Calculate treeba
do i = 1, ntree, 1
    treeba(i) = dbh(i)**2 * expf(i) * f_con
end do

!Determine trees to include in Lorey dia calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)
    ba_tree = treeba(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ > dbhmax) cycle
    if(ht_ < htmin .or. ht_ > htmax) cycle
    if(all_species /= 0 .and. .not. any(species_ == select_species)) cycle

    dbh_sum = dbh_sum + dbh_* ba_tree
    ba_ = ba_ + ba_tree

end do

! Calculate Lorey dia if A is greater than 0
if (ba_ > 0) lorey_dia_ = (dbh_sum / ba_)

end subroutine lorey_dia

!###############################################################################
!This subroutine calculates stand density index using Zeide's method given input
!vectors containing diameter at breast height and expansion factor values. This 
!attribute can be calculated for user defined size ranges and for select 
!species.
!###############################################################################

subroutine zsdi (dbh, expf, ht, species, dbhmin, dbhmax, htmin, &
htmax, all_species, select_species, ntree, nsp, zsdi_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, nsp, all_species
real, intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
integer, intent(in) :: species(ntree), select_species(nsp)
real, intent(in) :: dbhmin, dbhmax, htmin, htmax
real, intent(out) :: zsdi_
real :: dbh_, expf_, ht_, species_
integer :: i

!intialize variables
zsdi_ = 0.0

!Determine trees to include in ZSDI calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ > dbhmax) cycle
    if(ht_ < htmin .or. ht_ > htmax) cycle
    if(all_species /= 0 .and. .not. any(species_ == select_species)) cycle

    zsdi_ = zsdi_ + ((dbh_ / 10)**r_slope * expf_)

end do

end subroutine zsdi

!###############################################################################
!This subroutine calculates percent canopy cover corrected for overlap given 
!input vectors containing crown width and expansion factor values. This 
!attribute can be calculated for user defined size ranges and for select species.
!###############################################################################

subroutine cc (crwidth, dbh, expf, ht, species, dbhmin, dbhmax, & 
htmin, htmax, all_species, select_species, ntree, nsp, cc_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, nsp, all_species
real, intent(in) :: crwidth(ntree), dbh(ntree), expf(ntree), ht(ntree)
integer, intent(in) :: species(ntree), select_species(nsp)
real, intent(in) :: dbhmin, dbhmax, htmin, htmax
real, intent(out) :: cc_
real :: crwidth_, dbh_, expf_, ht_, species_, correct_cc
integer :: i

!intialize variables
cc_ = 0.0

!Determine trees to include in CC calculation
do i = 1, ntree, 1

    crwidth_ = crwidth(i)
    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ > dbhmax) cycle
    if(ht_ < htmin .or. ht_ > htmax) cycle
    if(all_species /= 0 .and. .not. any(species_ == select_species)) cycle

    cc_ = cc_ + ((crwidth_/2)**2 * (expf_/43560) * pi * 100)

end do

!Correct CC for overlap
cc_ = correct_cc(cc_)

end subroutine cc

!###############################################################################
!This function takes in an uncorrected percent canopy cover value and returns a 
!corrected value using the relationship described on page 2  of Crookston, 
!Nicholas L.; Stage, Albert R. 1999. Percent canopy cover and stand structure 
!statistics from the Forest Vegetation Simulator. Gen. Tech. Rep. RMRS-GTR-24.
!Ogden, UT: U. S. Department of Agriculture, Forest Service, Rocky Mountain 
!Research Station. 11 p.
!###############################################################################

real function correct_cc (cc)
implicit none

!Arugments
real, intent(in) :: cc

!Correct CC
correct_cc = 100 * (1 - exp(-0.01 * cc))

end function correct_cc

!###############################################################################
!This subroutine calculates Reinekes stand density index using the methodology 
!proposed by Stage 1968 from Section 7.3.2.1 of EFVS using input vectors 
!containing DBH and expansion factors. This attribute can be calculated for user
!defined size ranges and for select species.
!###############################################################################

subroutine rsdi_stage (dbh, expf, ht, species, dbhmin, dbhmax, htmin, &
htmax, all_species, select_species, ntree, nsp, rsdi_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, nsp, all_species
real, intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
integer, intent(in) :: species(ntree), select_species(nsp)
real, intent(in) :: dbhmin, dbhmax, htmin, htmax
real, intent(out) :: rsdi_
real :: dbh_, expf_, ht_, species_, tpa, dbhsq
integer :: i

!intialize variables
rsdi_ = 0.0
tpa = 0.0
dbhsq = 0.0

!Calculate tpa, dbhsq, and qmd for all trees
do i = 1, ntree, 1
    tpa = tpa + expf(i)
    dbhsq = dbhsq + dbh(i)**2 * expf(i)
end do

!Do RSDI calculation if tpa > 0
if(tpa > 0 ) then

    !Determine trees to include in RSDI calculation
    do i = 1, ntree, 1

        dbh_ = dbh(i)
        expf_ = expf(i)
        ht_ = ht(i)
        species_ = species(i)

        !Determine if tree should be skipped in calculation
        if(dbh_ < dbhmin .or. dbh_ > dbhmax) cycle
        if(ht_ < htmin .or. ht_ > htmax) cycle
        if(all_species /= 0 .and. .not. any(species_ == select_species)) cycle

        rsdi_ = zsdi_ + ((dbh_ / 10)**r_slope * expf_)
    end do
end if

end subroutine rsdi