################################################################################
#This script is used to build all the lists that read into commons.R
################################################################################

in_path = "C:/FVS_Utility/FVS-Utility-Functions/inst/extdata"

################################################################################
#FVS species codes lists
################################################################################

fvs_species = read.csv(file = file.path(in_path, 'fvs_species.csv'),
                       colClasses = c("character", 
                                      "integer", 
                                      "character",
                                      "character",
                                      "character"))

#fvs_seq_list: List of species sequence numbers for each FVS variant.
fvs_seq_list = split(x = fvs_species$SEQ, f = fvs_species$VARIANT)

#fvs_char_list: List of FVS species codes for each FVS variant.
fvs_char_list = split(x = fvs_species$FVS, f = fvs_species$VARIANT)

#fvs_fia_list: List of FIA species codes for each FVS variant.
fvs_fia_list = split(x = fvs_species$FIA, f = fvs_species$VARIANT)

#fvs_plant_list: List of USDA plant symbols for each FVS variant
fvs_plant_list = split(x = fvs_species$PLANT, f = fvs_species$VARIANT)

#Save lists
saveRDS(object = fvs_seq_list,
        file = file.path(in_path, 'fvs_seq_list.rds'))

saveRDS(object = fvs_char_list,
        file = file.path(in_path, 'fvs_char_list.rds'))

saveRDS(object = fvs_fia_list,
        file = file.path(in_path, 'fvs_fia_list.rds'))

saveRDS(object = fvs_plant_list,
        file = file.path(in_path, 'fvs_plant_list.rds'))

################################################################################
#Location codes by variant list
################################################################################

fvs_locs = read.csv(file = file.path(in_path, 'fvs_locs.csv'))

#fvs_loc_list: List of location codes for each FVS variant
fvs_loc_list = split(x = fvs_locs$LOCATION, f = fvs_locs$VARIANT)

#Save lists
saveRDS(object = fvs_loc_list,
        file = file.path(in_path, 'fvs_loc_list.rds'))

################################################################################
#PV codes by variant list
################################################################################

pv_codes = read.csv(file = file.path(in_path, 'pv_codes.csv'))

#fvs_plant_list: List of USDA plant symbols for each FVS variant
pvcode_list = split(x = pv_codes$PV_CODE, f = pv_codes$VARIANT)

#Save lists
saveRDS(object = pvcode_list,
        file = file.path(in_path, 'pvcode_list.rds'))

################################################################################
#PV codes by variant and region lists
################################################################################

pv_codes_regions = read.csv(file = file.path(in_path, 'pv_codes_regions.csv'))

#habvr_list: list of HABVR values
habpvr_list = split(x = pv_codes_regions$HABPVR,
                   f = paste0(pv_codes_regions$VARIANT, 
                              pv_codes_regions$REGION))

#pvcode_reg_list: List of PVCODE values
pvcode_reg_list = split(x = pv_codes_regions$PVCODE,
                        f = paste0(pv_codes_regions$VARIANT, 
                                   pv_codes_regions$REGION))

#pvref_list: list of PV_REF_CODE values
pvref_list = split(x = pv_codes_regions$PVREF,
                   f = paste0(pv_codes_regions$VARIANT, 
                              pv_codes_regions$REGION))

#Save lists
saveRDS(object = habpvr_list,
        file = file.path(in_path, 'habpvr_list.rds'))

saveRDS(object = pvcode_reg_list,
        file = file.path(in_path, 'pvcode_reg_list.rds'))

saveRDS(object = pvref_list,
        file = file.path(in_path, 'pvref_list.rds'))

#Clean up
rm(list = ls())
