################################################################################
#' Alaska variant bark ratio function
#'
#' @description
#' Calculates the bark ratio for individual trees to convert between outside-bark
#' and inside-bark dimensions using species-specific coefficients from the
#' FVS-AK variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing
#' (`NA`), or out-of-bounds codes automatically default to group 23
#' (Other Hardwood).
#'
#' @param dbh:
#' A numeric vector specifying the tree Diameter at Breast Height (DBH) in
#' inches.
#'
#' @return A numeric vector representing the predicted bark ratio for each tree
#' record.

#' @export
################################################################################

ak_bratio <- function(species, dbh) {
  
  # Handle invalid species values safely (default to 23)
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23
  
  #Get matrix of coefficients
  v <- bratio_coeffs[species, , drop = FALSE]
  
  # Pre-allocate the result vector to match input size
  bratio <- numeric(length(dbh))
  
  # Targeted calculation based on math type (v[, 1] is type_vec)
  idx1 <- (v[, 1] == 1)
  idx2 <- (v[, 1] == 2)
  idx3 <- (v[, 1] == 3)
  
  # Type 1
  if (any(idx1)) {
    bratio[idx1] <- (dbh[idx1] - (v[idx1, 2] * dbh[idx1] ^ v[idx1, 3])) / dbh[idx1]
  }
  
  # Type 2
  if (any(idx2)) {
    bratio[idx2] <- (v[idx2, 2] + v[idx2, 3] * dbh[idx2]) / dbh[idx2]
  }
  
  # Type 3
  if (any(idx3)) {
    bratio[idx3] <- (v[idx3, 2] * dbh[idx3] ^ v[idx3, 3]) / dbh[idx3]
  }
  
  #Bratio boundary setting
  return(pmin(pmax(bratio, 0.80), 0.99))
}

################################################################################
#' Alaska variant Chapman Richards height-diameter function
#'
#' @description
#' Calculates total tree height from diameter, or estimates diameter from total
#' tree height, using the Curtis-Arney functional form from the FVS-AK
#' variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing
#' (`NA`), or out-of-bounds codes automatically default to group 23
#' (Other Hardwood).
#'
#' @param dbh:
#' A numeric vector specifying the tree Diameter at Breast Height (DBH) in
#' inches. Used as an input when type is 1.
#'
#' @param ht:
#' A numeric vector specifying the total tree height in feet. Used as an input
#' when type is not 1.
#'
#' @param type:
#' An integer or numeric vector specifying the calculation branch. Use 1 to
#' calculate height from DBH, or any other value to calculate DBH from height.
#'
#' @return A numeric vector representing either the predicted tree heights (in
#' feet) or the predicted diameters (in inches) for each tree record.
#' @export
################################################################################

ak_htd_cr <- function(species, dbh, ht, type) { 
  
  #Handle invalid species values (default to 23) Safely 
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23 
  
  #Get species and coefficients 
  v <- htd_cr_coeffs[species, , drop = FALSE] 
  
  #Height calculation 
  if (type == 1) { 
    ht_base <- 4.5 + v[, 1] * (1 - exp(v[, 2] * dbh))^v[, 3] 
    
    #Determine multipliers for each species (most have 1) 
    mult <- rep(1.0, length(ht_base)) 
    mult[species %in% c(14, 21, 23)] <- 0.45 
    mult[species == 22] <- 0.65 
    result <- ht_base * mult 
  } 
  
  #Calculate DBH from Ht 
  else { 
    suppressWarnings({ 
      result <- (1 / v[, 2] * log(1 - ((ht - 4.5) / v[, 1])^(1 / v[, 3]))) 
    }) 
  } 
  return(result) 
}

###############################################################################
#' Alaska variant Wykoff height-diameter function
#'
#' @description
#' Calculates total tree height from diameter, or estimates diameter from total
#' tree height, using the Wykoff exponential functional form from the FVS-AK
#' variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing
#' (`NA`), or out-of-bounds codes automatically default to group 23
#' (Other Hardwood).
#'
#' @param dbh:
#' A numeric vector specifying the tree Diameter at Breast Height (DBH) in
#' inches. Used as an input when type is 1.
#'
#' @param ht:
#' A numeric vector specifying the total tree height in feet. Used as an input
#' when type is not 1.
#'
#' @param type:
#' An integer or numeric vector specifying the calculation branch. Use 1 to
#' calculate height from DBH, or any other value to calculate DBH from height.
#'
#' @return A numeric vector representing either the predicted tree heights (in
#' feet) or the predicted diameters (in inches) for each tree record.
#' @export
################################################################################

ak_htd_wy <- function(species, dbh, ht, type) {
  
  #Handle invalid species values safely (default to 23) 
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23 
  
  #Get species and coefficients 
  v <- htd_wy_coeffs[species, , drop = FALSE] 
  
  #Calculate Ht from DBH 
  if (type == 1) { 
    ht_base <- 4.5 + exp(v[, 1] + v[, 2] / (dbh + 1)) 
    
    #Set multipliers 
    mult <- rep(1.0, length(ht_base)) 
    mult[species %in% c(14, 21, 23)] <- 0.45 
    mult[species == 22] <- 0.65 
    return(ht_base * mult) 
  } 
  
  #Calculate DBH from Ht 
  else { 
    suppressWarnings({ 
      return((v[, 2] / (log(ht - 4.5) - v[, 1])) - 1.0) 
    }) 
  } 
  
  return(result) 
}
################################################################################
#' Alaska variant crown width function
#'
#' @description
#' Calculates the projected crown width for individual trees based on dimensions,
#' local stand basal area, and site elevation using functional form from FVS-AK
#' variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing
#' (`NA`), or out-of-bounds codes automatically default to group 23
#' (Other Hardwood).
#'
#' @param dbh:
#' A numeric vector specifying the tree Diameter at Breast Height (DBH) in
#' inches.
#'
#' @param ht:
#' A numeric vector specifying the total tree height in feet.
#'
#' @param cl:
#' A numeric vector specifying the crown length in feet.
#'
#' @param ba:
#' A numeric vector specifying the stand basal area in square feet per acre.
#'
#' @param elev:
#' A numeric vector specifying the plot elevation in hundreds of feet.
#'
#' @return A numeric vector representing the predicted crown width (in feet) for
#' each tree record.
#' @export
################################################################################

ak_cwcalc <- function(species, dbh, ht, cl, ba, elev) { 
  
  #Handle invalid species values safely (default to 23) 
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23 
  
  #Get species and coefficients 
  v <- cwcalc_coeffs[species, , drop = FALSE] 
  
  #Constrain elevation vector element-by-element 
  no_constraint <- (v[, 8] <= 0.0 & v[, 9] <= 0.0) 
  v_elev <- ifelse(no_constraint, elev, pmin(pmax(elev, v[, 8]), v[, 9])) 
  
  #Pre-compute crown width options based on dbh comparison 
  cw_standard <- v[, 2] * dbh^v[, 3] * ht^v[, 4] * cl^v[, 5] * (ba + 1)^v[, 6] *
    exp(v_elev)^v[, 7] 
  
  cw_adjusted <- (v[, 2] * v[, 1]^v[, 3] * ht^v[, 4] * cl^v[, 5] * 
                    (ba + 1)^v[, 6] * exp(v_elev)^v[, 7]) * (dbh / v[, 1]) 
  
  #Select calculation vector branch 
  result <- ifelse(dbh >= v[, 1], cw_standard, cw_adjusted) 
  
  return(result) 
}

################################################################################
#' Alaska variant crown competition factor function
#'
#' @description
#' Calculates the crown competition factor (CCF) contributions for individual
#' trees, accounting for maximum crown width variations across species using
#' functional form from FVS-AK variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing
#' (`NA`), or out-of-bounds codes automatically default to group 23
#' (Other Hardwood).
#'
#' @param dbh:
#' A numeric vector specifying the tree Diameter at Breast Height (DBH) in
#' inches.
#'
#' @param expf:
#' A numeric vector specifying the tree expansion factor or plot weight (Trees
#' Per Acre, TPA) used to scale individual values to a per-acre baseline.
#'
#' @return A numeric vector representing the crown competition factor contribution
#' for each tree record.
#' @export
################################################################################

ak_ccf <- function(species, dbh, expf) { 
  
  #Handle invalid species values safely (default to 23) 
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23 
  
  #Get species and coefficients 
  v <- ccf_coeffs[species, , drop = FALSE] 
  
  #Pre-compute both equation branches for Max Crown Width (MCW) 
  mcw_form1 <- v[, 1] + v[, 2] * dbh 
  mcw_form2 <- (v[, 1] * dbh^v[, 2]) * 3.28 
  
  # Select the proper MCW for each individual element 
  v_mcw <- ifelse(v[, 3] == 1, mcw_form1, mcw_form2) 
  
  # Run remaining calculations across the vectors 
  v_mca <- pi * (v_mcw / 2)^2 
  result <- (v_mca / 43560) * 100 * expf 
  
  return(result) 
}

################################################################################
#' Alaska variant crown ratio function
#'
#' @description
#' Calculates the projected crown ratio for individual trees using a logistic
#' model structure based on the functional form from the FVS-AK variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing
#' (`NA`), or out-of-bounds codes automatically default to group 23
#' (Other Hardwood).
#'
#' @param dbh:
#' A numeric vector specifying the tree Diameter at Breast Height (DBH) in
#' inches.
#'
#' @param ht:
#' A numeric vector specifying the total tree height in feet.
#'
#' @param hdbh:
#' A numeric vector specifying a pre-calculated height-to-diameter ratio.
#' If provided, it overrides the internal `(ht * 12) / dbh` calculation.
#' Defaults to NA.
#'
#' @param rd:
#' A numeric vector specifying the stand Relative Density (RD).
#'
#' @param qmd:
#' A numeric vector specifying the Quadratic Mean Diameter (QMD) of the stand
#' in inches.
#'
#' @param fcr:
#' A numeric vector or scalar specifying a user-defined crown ratio change
#' modifier. This can be used to emulate random effect from dubscr.f. Defaults 
#' to 0.
#'
#' @return A numeric vector representing the predicted crown ratio (expressed as
#' a proportion between 0 and 1) for each tree record.
#' @export
################################################################################

ak_cratio <- function(species, dbh, ht, hdbh = NA, rd, qmd, fcr = 0) { 
  
  #Handle invalid species values safely (default to 23) 
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23 
  
  #Get species and coefficients 
  v <- cratio_coeffs[species, , drop = FALSE] 
  
  #Determine HDR 
  hdr_calc <- (ht * 12) / dbh 
  v_hdr <- ifelse(is.na(hdbh), hdr_calc, hdbh) 
  
  #Calculate crown ratio 
  v_x <- v[, 1] + v[, 2] * log(v_hdr) + v[, 3] * rd + v[, 4] * dbh / qmd 
  result <- 1 / (1 + exp(v_x + fcr)) 
  
  return(result) 
}

################################################################################
#' Alaska variant diameter growth function
#'
#' @description
#' Calculates the projected diameter increment for individual trees over a
#' specified growth period, adjusting for permafrost constraints and accounting
#' for bark thickness changes using functional form from FVS-AK variant.
#'
#' @param type:
#' An integer or numeric vector specifying the model calculation branch.
#' Defaults to 1.
#' 
#' 1 = return outside bark diameter growth value
#' 
#' 2 = return inside bark DDS value
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing
#' (`NA`), or out-of-bounds codes automatically default to group 23
#' (Other Hardwood).
#'
#' @param dbh:
#' A numeric vector specifying the tree Diameter at Breast Height (DBH) in
#' inches.
#'
#' @param bal:
#' A numeric vector specifying the Basal Area in Large trees (BAL) in square feet
#' per acre, representing localized overtopping competition.
#'
#' @param rd:
#' A numeric vector specifying the stand Relative Density (RD).
#'
#' @param cr:
#' A numeric vector specifying individual tree Crown Ratio.
#'
#' @param elev:
#' A numeric vector specifying the plot elevation in hundreds of feet.
#'
#' @param slope:
#' A numeric vector specifying the plot slope percentage.
#'
#' @param aspect:
#' A numeric vector specifying the plot aspect.
#'
#' @param perm:
#' A numeric vector specifying permafrost presence indicator or depth indexes
#' (where values `>= 1.0` indicate permafrost presence). Defaults to 0.
#'
#' @param si:
#' A numeric vector specifying the Site Index of the stand. Defaults to 70.
#'
#' @param yr:
#' A numeric vector or scalar specifying the length of the growth projection
#' period in years. Defaults to 10.
#'
#' @param perm_off:
#' A logical scalar or vector. If `TRUE`, the permafrost modifier calculation
#' is entirely bypassed and reset to 1.0. Defaults to TRUE.
#'
#' @return A numeric vector representing the projected diameter growth increment
#' (in inches) over the specified `yr` period for each tree record.
#' @export
################################################################################

ak_dg <- function(type = 1,
                  species, 
                  dbh, 
                  bal,
                  rd, 
                  cr, 
                  elev,
                  slope,
                  aspect,
                  perm = 0,
                  si = 70, 
                  yr = 10, 
                  perm_off = TRUE) { 
  
  #Handle invalid species values safely (default to 23) 
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23 
  
  #Get species and coefficients 
  v_base <- dg_base_coeffs[species, , drop = FALSE] 
  
  #Calculate vectorized annual base diameter increment 
  basedg <- exp(v_base[, 1] + v_base[, 2]*dbh^2 + v_base[, 3]*log(dbh) + 
                  v_base[, 4]*bal + v_base[, 5]*rd + v_base[, 6]*log(cr) + 
                  v_base[, 7]*elev + v_base[, 8]*slope + 
                  v_base[, 9]*slope*cos(aspect) + v_base[, 10]*log(si)) 
  
  #Scalar values for permafrost equations 
  pb2 <- -0.002711 
  pb6 <- 0.736013 
  pb7 <- -0.000137 
  pb8 <- 0.001702 
  pb9 <- -0.001819 
  pb01 <- -0.349215 
  
  #Get permaforst coefficients 
  v_perm <- dg_perm_coeffs[species, , drop = FALSE] 
  
  # Evaluate Permafrost Modifier Logic across vectors 
  pf_present <- exp(v_perm[, 1] + pb01 + pb2*dbh^2 + v_perm[, 2]*log(dbh) + 
                      v_perm[, 3]*bal + v_perm[, 4]*rd + pb6*log(cr) + 
                      pb7*elev + pb8*slope + pb9*slope*cos(aspect)) / basedg 
  
  pf_absent <- exp(v_perm[, 1] + pb2*dbh^2 + v_perm[, 2]*log(dbh) + 
                     v_perm[, 3]*bal + v_perm[, 4]*rd + pb6*log(cr) + 
                     pb7*elev + pb8*slope + pb9*slope*cos(aspect)) / basedg 
  
  pf_present <- pmin(pf_present, 1.0) 
  pf_absent <- pmax(pf_absent, 1.0) 
  
  affected_species <- species %in% c(4:7, 13, 16:23) 
  pfmod <- ifelse(affected_species, ifelse(perm >= 1.0, pf_present, pf_absent), 1.0) 
  pfmod <- ifelse(perm_off, 1.0, pfmod) 
  
  # Apply final scaling and species multiplication adjustments 
  di_base <- yr * basedg * pfmod 
  di_base <- ifelse(species %in% c(14, 21, 23), 
                    di_base * 0.45, 
                    ifelse(species == 22, di_base * 0.65, di_base)) 
  
  #Bark ratio and DDS calculation 
  br <- ak_bratio(species, dbh) 
  d1 <- dbh * br 
  d2 <- (dbh + di_base) * br 
  dds <- log(d2^2 - d1^2) 
  
  # Type parameter scalar conditional return 
  if (type == 1) { 
    return(di_base) 
  } else { 
    return(dds) 
  } 
}

###z############################################################################
#' Alaska variant height growth function
#'
#' @description
#' Calculates the projected height increment for individual trees over a 
#' specified growth period using functional form from FVS-AK variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing 
#' (`NA`), or out-of-bounds codes automatically default to group 23 
#' (Other Hardwood).
#'  
#' @param dbh:
#' A numeric vector specifying the tree Diameter at Breast Height (DBH) in 
#' inches.
#' 
#' @param ht:
#' A numeric vector specifying the initial total tree height in feet. 
#' Defaults to 4.5'.
#'   
#' @param elev:
#'  A numeric vector specifying the plot elevation in hundreds of feet.
#'  
#' @param si:
#' A numeric vector specifying the Site Index (base age 100 or 50) of the stand. 
#' Defaults to 70.
#' 
#' @param dg:
#' A numeric vector specifying the projected or observed annual diameter growth 
#' increment in inches. Defaults to 0.1.
#'  
#' @param yr: 
#' A numeric vector or scalar specifying the length of the growth projection 
#' period in years. Defaults to 10.
#' 
#' @param perm: 
#' A numeric vector specifying permafrost presence indicator or depth indexes
#'  (where values `>= 1.0` indicate permafrost presence).
#'
#' @return A numeric vector representing the projected height growth increment
#'  (in feet) over the specified `yr` period for each tree record.
#' @export
################################################################################

ak_hg <- function(species,
                  dbh, 
                  ht = 4.5,
                  elev,
                  si = 70,
                  dg = 0.1,
                  yr = 10,
                  perm) { 
  
  #Handle invalid species values safely (default to 23) 
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23 
  
  #Get species, coefficients, and height bounding values 
  v_base <- hg_base_coeffs[species, , drop = FALSE] 
  
  # Calculate annual base height increment 
  basehg <- exp(v_base[, 3] + v_base[, 4]*dbh^2 + v_base[, 5]*log(dbh) + 
                  v_base[, 6]*elev + v_base[, 7]*log(si) + 
                  v_base[, 8]*log(dg)) * v_base[, 9] 
  
  #Get permaforst coefficients 
  v_perm <- hg_perm_coeffs[species, , drop = FALSE] 
  
  # Evaluate Permafrost Logic across vectors 
  pf_present <- exp(v_perm[, 1] + v_perm[, 2] + v_perm[, 3]*dbh^2 + 
                      v_perm[, 4]*log(dbh) + v_perm[, 5]*elev +
                      v_perm[, 6]*log(dg)) / basehg 
  
  pf_absent <- exp(v_perm[, 1] + v_perm[, 3]*dbh^2 + v_perm[, 4]*log(dbh) + 
                     v_perm[, 5]*elev + v_perm[, 6]*log(dg)) / basehg 
  
  pf_present <- pmin(pf_present, 1.0) 
  pf_absent <- pmax(pf_absent, 1.0) 
  
  affected_species <- species %in% c(4:7, 13, 16:23) 
  pfhmod <- ifelse(affected_species, ifelse(perm >= 1.0, pf_present, pf_absent), 1.0) 
  
  # Apply final scaling and species multiplication adjustments 
  hi_base <- basehg * yr * pfhmod 
  hi <- ifelse(species %in% c(14, 21, 23), hi_base * 0.45, 
               ifelse(species == 22, hi_base * 0.65, hi_base)) 
  
  # Apply Height Bounding Logic 
  bnd_scale <- 1.0 - ((ht - v_base[, 1]) / (v_base[, 2] - v_base[, 1])) 
  bnd_scale <- pmax(bnd_scale, 0.1) # Capping if less than 0.1 
  
  # Select the correct bounding factor based on ht tiers 
  hgbnd <- ifelse(ht >= v_base[, 1] & ht < v_base[, 2], bnd_scale, 
                  ifelse(ht < v_base[, 1], 1.0, 0.1)) 
  
  # Apply bounded reduction factor 
  hi <- hi * hgbnd 
  
  # Apply Final Constraints Checks 
  hi <- ifelse(hi <= 0.1, 0.1, hi) 
  hi <- ifelse(dg <= 0.04, 0.1, hi) 
  result <- hi 
  
  return(result) 
}

################################################################################
#' Alaska variant survival function
#' @name ak_surv
#' @description
#' Estimates individual tree survival probability and calculates using
#' functional form from FVS-AK variant.
#' 
#' @param type:
#' Integer value corresponding to type of value to return.
#' 
#' 1 = survival probability
#' 
#' 2 = mortality probability
#' 
#' 3 = mortality in terms of TPA representation
#'
#' @param species:
#' An integer or numeric vector containing the species group codes (1 to 23). 
#' Invalid, missing (`NA`), or non-numeric species identifiers default 
#' automatically to group 23 (Other Hardwood).
#' 
#' @param dbh:
#' A numeric vector specifying the individual tree Diameter at Breast Height 
#' (DBH) in inches.
#' 
#' @param expf:
#' Optional numeric vector containing expansion factors. If this value is not
#' NULL, then bal will be calculated with dbh and expf values.
#' 
#' @param bal:
#' A numeric vector specifying the Basal Area in Large trees (BAL) in square 
#' feet per acre.
#' 
#' @param p:
#' A numeric vector or scalar specifying the tree expansion factor or plot 
#' weight (Trees Per Acre, TPA). Defaults to `1`.
#' 
#' @param fint:
#' An integer or numeric vector specifying the length of the growth projection 
#' period in years. Defaults to `10`.
#'
#' @return A numeric vector representing the compounded probability of survival
#'  over the specified `fint` interval for each tree record. 
#'  @export
################################################################################

ak_surv <- function(type = 1, 
                    species, 
                    dbh, 
                    bal, 
                    p = 1,
                    fint = 10) { 
  
  # Handle invalid species values safely (default to 23) 
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23 
  
  #Get coefficients 
  v <- surv_coeffs[species, , drop = FALSE] 
  
  #Determine diameter to use in calculation 
  dtemp <- pmax(dbh, v[, 6]) 
  
  #Calculate annual survival 
  v_x <- v[, 1] + v[, 2] * dtemp + v[, 3] * dtemp^2 + v[, 4] * bal + 
    v[, 5] * bal / dtemp 
  prob_surv <- exp(v_x) / (1 + exp(v_x)) 
  
  #Scale survival to fint length 
  surv <- prob_surv^fint 
  
  #Return survival 
  if (type == 1) { 
    return(surv) 
  } 
  
  #Return mortality 
  mort <- (1 - surv) 
  if (type == 2) { 
    return(mort) 
  } 
  
  #Return mortality as amount of TPA killed 
  return(mort * p) 
}

################################################################################
#' Alaska variant Hegyi site index function
#'
#' @description
#' Calculates site index from tree height and age, total tree height from site 
#' index and age, or tree age from site index and height using the Hegyi 
#' functional form from the FVS-AK variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing
#' (`NA`), or out-of-bounds codes automatically default to group 23
#' (Other Hardwood).
#'
#' @param ht:
#' A numeric vector specifying the total tree height in feet. Used as an input
#' when type is 1 or when type is 0.
#'
#' @param age:
#' A numeric vector specifying the tree breast-height age in years. Used as an
#' input when type is 1 or 2.
#'
#' @param baseage:
#' A numeric vector or scalar specifying the index base age curve selection. Use
#' values less than 100 to automatically load the base age 50 curves, or any other
#' value to load the base age 100 curves. Defaults to 100.
#'
#' @param si:
#' A numeric vector specifying the site index baseline height in feet. Used as
#' an input when type is 2 or when type is 0.
#'
#' @param type:
#' An integer or numeric value specifying the calculation branch. 
#' 
#' 1 = Calculate site index from height
#' 
#' 2 = Calculate height from site index
#' 
#' 3 = Calculate age from site index and height
#'
#' @return A numeric vector representing either the calculated site index (in 
#' feet), tree height (in feet), or tree age (in years) depending on the selected
#' calculation type.
#' @export
################################################################################

ak_hegyi <- function(species, ht, age, baseage = 100, si, type = 0) { 
  
  #Handle invalid species values safely (default to 23) 
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23 
  
  #Get species 
  v <- hegyi_coeffs[species, , drop = FALSE] 
  
  #Get coefficients 
  b1 <- if (baseage < 100) v[, 4] else v[, 1] 
  
  #Site Index from Height & Age 
  if (type == 1) { 
    return(ht / (b1 * (1 - exp(v[, 2] * age))^v[, 3])) 
    
    #Height from Site Index & Age 
  } else if (type == 2) { 
    return(b1 * si * (1 - exp(v[, 2] * age))^v[, 3]) 
    
    #Age from Height & Site Index 
  } else { 
    suppressWarnings({ 
      return(1 / v[, 2] * log(1 - (ht / b1 / si)^(1 / v[, 3]))) 
    }) 
  } 
}

################################################################################
#' Alaska variant Paya site index function
#'
#' @description
#' Calculates site index from tree height and age, total tree height from site
#' index and age, or tree age from site index and height using the Paya
#' functional form from the FVS-AK variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (primarily 8, 11, or 12).
#' Invalid, missing (`NA`), or unrecognized codes automatically default to group
#' 11.
#'
#' @param ht:
#' A numeric vector specifying the total tree height in feet. Used as an input
#' when type is 1 or when type is 0.
#'
#' @param age:
#' A numeric vector specifying the tree breast-height age in years. Used as an
#' input when type is 1 or 2.
#'
#' @param si:
#' A numeric vector specifying the site index baseline height in feet. Used as
#' an input when type is 2 or when type is 0.
#'
#' @param type:
#' An integer or numeric value specifying the calculation branch. 
#' 
#' 1 = Calculate site index from height
#' 
#' 2 = Calculate height from site index
#' 
#' 3 = Calculate age from site index and height
#'
#' @return A numeric vector representing either the calculated site index (in
#' feet), tree height (in feet), or tree age (in years) depending on the selected
#' calculation type.
#' @export
################################################################################

ak_paya <- function(species,
                    ht, 
                    age,
                    si, 
                    type = 0, 
                    debug = FALSE) { 
  
  #Handle invalid species values safely (default to 11) 
  species[!species %in% c(8, 11, 12) | is.na(species)] <- 11 
  
  #Calculate Site Index 
  if (type == 1) { 
    return(0.520027 * ht^0.999937 * (1 - exp(-0.00625 * age))^(-0.899461 * ht^-0.011825)) 
  } 
  
  #Calculate Height 
  else if (type == 2) { 
    return(1.5469 * si^1.0018 * (1 - exp(-0.0114 * age))^(1.0883 * si^0.0072)) 
  } 
  
  #Calculate Age 
  else { 
    suppressWarnings({ 
      return(1 / -0.0114 * (log(1 - (ht / 1.5469 / si^1.0018)^(1 / (1.0883 * si^0.0072))))) 
    }) 
  } 
}

################################################################################
#' Alaska variant site index wrapper function
#'
#' @description
#' Evaluates tree attributes and routes calculations automatically between the
#' Payandeh or Hegyi model structures based on species codes and age constraints
#' using functional form from the FVS-AK variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing
#' (`NA`), or out-of-bounds codes automatically default to group 23
#' (Other Hardwood).
#'
#' @param ht:
#' A numeric vector specifying the total tree height in feet. Used as an input
#' when type is 1 or when type is 0.
#'
#' @param age:
#' A numeric vector specifying the tree breast-height age in years.
#'
#' @param baseage:
#' A numeric vector or scalar specifying the index base age curve selection 
#' passed down to the underlying Hegyi function. Defaults to 100.
#'
#' @param si:
#' A numeric vector specifying the site index baseline height in feet. Used as
#' an input when type is 2 or when type is 0.
#'
#' @param type:
#' An integer or numeric vector specifying the calculation branch. Use 1 to
#' calculate site index from height and age, 2 to calculate height from site index
#' and age, or any other value to calculate age from site index and height.
#'
#' @return A numeric vector representing either the calculated site index (in
#' feet), tree height (in feet), or tree age (in years) depending on the selected
#' calculation type.
#' @export
################################################################################

ak_si <- function(species,
                  ht, 
                  age,
                  baseage = 100,
                  si, 
                  type = 0) {
  
  #Handle invalid species values safely (default to 23)
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23
  
  #Calculate with payendah and hegyi
  paya_results  <- ak_paya(species = species, 
                           ht = ht, 
                           age = age,
                           si = si, 
                           type = type)
  
  hegyi_results <- ak_hegyi(species = species,
                            ht = ht, 
                            age = age,
                            baseage = baseage,
                            si = si, 
                            type = type)
  
  #Detemine final results
  result <- ifelse((species %in% c(8, 11, 12)) & (age < 200), 
                   paya_results, hegyi_results)
  
  return(result)
}

################################################################################
#' Alaska variant small tree height growth scaling function
#'
#' @description
#' Calculates the dynamically weighted height increment for smaller trees by
#' blending small-tree growth predictions and large-tree predictions using a
#' diameter-based transition scale from the FVS-AK variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing
#' (`NA`), or out-of-bounds codes automatically default to group 23
#' (Other Hardwood).
#'
#' @param dbh:
#' A numeric vector specifying the tree Diameter at Breast Height (DBH) in
#' inches.
#'
#' @param htgr:
#' A numeric vector specifying the unweighted height growth prediction derived
#' from the small tree base sub-model components.
#'
#' @param lthg:
#' A numeric vector specifying the alternate height growth prediction derived
#' from the large tree potential model components.
#'
#' @return A numeric vector representing the blended and scaled height growth
#' increment (in feet) for each tree record.
#' @export
################################################################################

ak_smhg <- function(species,
                    dbh,
                    htgr,
                    lthg) { 
  
  #Get minimum and maximum diameters 
  v <- smhg_coeffs[species, , drop = FALSE] 
  
  #Calculate weighting for small and large tree growth 
  d_clamped <- pmin(pmax(dbh, v[, 1]), v[, 2]) 
  xwt <- (d_clamped - v[, 1]) / (v[, 2] - v[, 1]) 
  
  #Calculate height growth 
  htgr_avg <- (htgr + lthg) / 2 
  result <- htgr_avg * (1.0 - xwt) + xwt * lthg 
  
  return(result) 
}

################################################################################
#' Alaska variant small tree diameter growth scaling function
#'
#' @description
#' Calculates the dynamically weighted diameter increment for smaller trees by
#' blending back-transformed small-tree growth predictions and large-tree
#' predictions using a diameter-based transition scale from the FVS-AK variant.
#'
#' @param species:
#' An integer or numeric vector of species codes (1 to 23). Invalid, missing
#' (`NA`), or out-of-bounds codes automatically default to group 23
#' (Other Hardwood).
#'
#' @param dbh:
#' A numeric vector specifying the tree Diameter at Breast Height (DBH) in
#' inches.
#'
#' @param d1:
#' A numeric vector specifying the initial tree diameter metric inside bark or
#' baseline dimension component used in the small tree routine.
#'
#' @param d2:
#' A numeric vector specifying the projected tree diameter metric inside bark or
#' future dimension component used in the small tree routine.
#'
#' @param ltdg:
#' A numeric vector specifying the alternate large tree model diameter growth
#' increment prediction.
#'
#' @param bark:
#' A numeric vector specifying the bark ratio factor for the individual tree
#' records.
#'
#' @param scale:
#' A numeric vector or scalar multiplier used to scale change in squared diameter
#' measurements to a specific cycle length. Defaults to 1.
#'
#' @return A numeric vector representing the blended and scaled diameter growth
#' increment (in inches) for each tree record.
#' @export
################################################################################

ak_sm_dg <- function(species,
                     dbh,
                     d1, 
                     d2, 
                     ltdg,
                     bark, 
                     scale = 1) { 
  
  #Handle invalid species values safely (default to 23) 
  species[!is.numeric(species) | species < 1 | species > 23 | is.na(species)] <- 23 
  
  #Get coefficients 
  v <- sm_dg_coeffs[species, , drop = FALSE] 
  
  #Calculate weighting for small and large tree growth 
  d_clamped <- pmin(pmax(dbh, v[, 1]), v[, 2]) 
  xwt <- (d_clamped - v[, 1]) / (v[, 2] - v[, 1]) 
  
  #Core small tree diameter change calculations 
  smdg_raw <- (d2 - d1) * bark 
  v_dds <- smdg_raw * (2.0 * bark * dbh + smdg_raw) * scale 
  smdg <- sqrt((dbh * bark)^2.0 + v_dds) - bark * dbh 
  
  #Weight the growth results 
  result <- smdg * (1.0 - xwt) + xwt * ltdg 
  
  return(result) 
}
