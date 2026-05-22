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
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
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
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
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
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
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
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
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
real :: dbh_, expf_, ht_, species_, tpa_, dbh_sum, ba_, ba_tree
integer :: i

!intialize tvariables
tpa_ = 0.0
dbh_sum = 0.0
lorey_dia_ = 0.0

!Determine trees to include in Lorey dia calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)
    ba_tree = dbh_**2 * expf_ * f_con

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
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
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
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
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
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
real :: dbh_, expf_, ht_, species_, tpa_, dbhsq, a, b
integer :: i

!intialize variables
rsdi_ = 0.0
tpa_ = 0.0
dbhsq = 0.0

!Calculate tpa, dbhsq, and qmd for all trees
do i = 1, ntree, 1
    tpa_ = tpa_ + expf(i)
    dbhsq = dbhsq + dbh(i)**2 * expf(i)
end do

!Do RSDI calculation if tpa_ > 0
if(tpa_ > 0 ) then

    !Initialize a and b parameters
    a = 10**(-r_slope) * (1 - (r_slope/2)) * (dbhsq/tpa_)**(r_slope/2)
    b = 10**(-r_slope) * (r_slope/2) * (dbhsq/tpa_)**(r_slope/2 - 1)

    !Determine trees to include in RSDI calculation
    do i = 1, ntree, 1

        dbh_ = dbh(i)
        expf_ = expf(i)
        ht_ = ht(i)
        species_ = species(i)

        !Determine if tree should be skipped in calculation
        if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
        if(ht_ < htmin .or. ht_ >= htmax) cycle
        if(all_species /= 0 .and. .not. any(species_ == select_species)) cycle

        rsdi_ = rsdi_ + (a * expf_ + b * dbh_**2 * expf_)
    end do
end if

end subroutine rsdi_stage

!###############################################################################
!This subroutine calculates Lorey height using input vectors containing DBH,
!total tree height and expansion factors. This attribute can be calculated for
!user defined size ranges and for select species.
!###############################################################################

subroutine lorey_ht(dbh, expf, ht, species, dbhmin, dbhmax, htmin, &
htmax, all_species, select_species, ntree, nsp, lorey_ht_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, nsp, all_species
real, intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
integer, intent(in) :: species(ntree), select_species(nsp)
real, intent(in) :: dbhmin, dbhmax, htmin, htmax
real, intent(out) :: lorey_ht_
real :: dbh_, expf_, ht_, species_, ba_, ba_tree, ba_sum
integer :: i

!Initialize variables
lorey_ht_ = 0.0
ba_ = 0.0
ba_sum = 0.0

!Determine trees to include in Lorey Height calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)
    ba_tree = dbh_**2 * expf_ * f_con

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(all_species /= 0 .and. .not. any(species_ == select_species)) cycle

    ba_sum = ba_sum + ba_tree * ht_
    ba_ = ba_ + ba_tree

end do

!Calculate lorey height if ba_ > 0
if(ba_ > 0) lorey_ht_ = ba_sum / ba_

end subroutine lorey_ht

!###############################################################################
!This subroutine calculates top height using input vectors containing DBH,
!total tree height and expansion factors. This attribute can be calculated for
!user defined size ranges and for select species.
!###############################################################################

subroutine top_ht(dbh, sorted_idx, expf, ht, top_tpa, top_per, ntree, top_ht_)
implicit none

!Arguments
integer, intent(in) :: ntree, sorted_idx(ntree)
real, intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
real, intent(inout) :: top_tpa, top_per
real, intent(out) :: top_ht_
real :: dbh_, expf_, ht_, tpa_, top, tpa_sum, ht_sum, tpa_dif
integer :: i, idx
logical :: top_exceed

!Initialize variables
top_ht_ = 0.0
tpa_ = 0.0
tpa_sum = 0.0
ht_sum = 0.0
tpa_dif = 0.0
top_exceed = .false.

!Do checks on top_tpa and top_per
if(top_tpa < 0.0) top_tpa = 40.0
if(top_per < 0.0 .or. top_per > 100.0) top_per = 20.0

!Calculate tpa of stand
do i = 1, ntree, 1
    tpa_ = tpa_ + expf(i)
end do

!Do top height calculation if tpa > 0
if(tpa_ > 0) then

    !Determine value of top
    top = top_tpa
    if(top >= tpa_) top = top_tpa
    if(top_per > 0) top = tpa_ * (top_per/100)

    !Determine trees to include in Top Height calculation
    do i = 1, ntree, 1

        idx = sorted_idx(i)
        dbh_ = dbh(idx)
        expf_ = expf(idx)
        ht_ = ht(idx)

        tpa_sum = tpa_sum + expf_

        if(tpa_sum >= top) then 
            tpa_dif = tpa_sum - top
            tpa_sum = tpa_sum - tpa_dif
            top_exceed = .true.
        endif

        !Update ht_sum
        ht_sum = ht_sum + ht_ * (expf_ - tpa_dif)

        !Exit if top has been exceeded
        if(top_exceed) exit

    end do

    !Calculate top height if tpa_sum > 0
    if(tpa_sum > 0) top_ht_ = ht_sum / tpa_sum
end if

end subroutine top_ht

!###############################################################################
!This function is used to calculate QMD, GMD (reinekes diameter) or average 
!diameter weighted by TPA for the largest trees by DBH within a specified 
!percentage of TPA or an explicit TPA value. This value is calculated from a set
!of input vectors containing DBH values and expansion factors.
!###############################################################################

subroutine top_dia(dbh, sorted_idx, expf, top_tpa, top_per, ntree, dia_type, &
top_dia_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, sorted_idx(ntree), dia_type
real, intent(in) :: dbh(ntree), expf(ntree)
real, intent(inout) :: top_tpa, top_per
real, intent(out) :: top_dia_
real :: dbh_, expf_, ht_, tpa_, top, tpa_sum, dbh_sum, tpa_dif
integer :: i, idx
logical :: top_exceed

!Initialize variables
top_dia_ = 0.0
tpa_ = 0.0
tpa_sum = 0.0
dbh_sum = 0.0
tpa_dif = 0.0
top_exceed = .false.

!Do checks on top_tpa and top_per
if(top_tpa < 0.0) top_tpa = 40.0
if(top_per < 0.0 .or. top_per > 100.0) top_per = 20.0

!Calculate TPA for stand
do i = 1, ntree, 1
    tpa_ = tpa_ + expf(i)
end do

!Do top diameter calculation if tpa_ > 0
if(tpa_ > 0.0) then

    !Determine value of top
    top = top_tpa
    if(top >= tpa_) top = top_tpa
    if(top_per > 0) top = tpa_ * (top_per/100)

    !Determine trees to included in top diameter
    do i = 1, ntree, 1

        idx = sorted_idx(i)
        dbh_ = dbh(i)
        expf_ = expf(i)
        
        !Update tpa_sum
        tpa_sum = tpa_sum + expf_

        !Determine if top is exceeded
        if(tpa_sum >= top) then
            tpa_dif = tpa_sum - top
            tpa_sum = tpa_sum - tpa_dif
            top_exceed = .true. 
        end if

        !Update dbh_sum based on dia_type
        select case (dia_type)
        case(1)
            dbh_sum = dbh_sum + dbh_**2 * (expf_ - tpa_dif)
        case(2)
            dbh_sum = dbh_sum + dbh_ * (expf_ - tpa_dif)
        case default 
            dbh_sum = dbh_sum + dbh_**r_slope * (expf_ - tpa_dif)
        end select

        !Break if top value has been exceeded
        if(top_exceed) exit

    end do
end if

!Calculate top diameter if tpa_sum > 0
if(tpa_sum > 0.0) then
    select case(dia_type)
    case(1)
        top_dia_ = sqrt(dbh_sum / tpa_sum)
    case(2)
        top_dia_ = dbh_sum / tpa_sum
    case(3)
        top_dia_ = (dbh_sum / tpa_sum)**(1 / r_slope)
    end select
end if

end subroutine top_dia