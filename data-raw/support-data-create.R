################################################################################
#support-data-create.R
#
#This script is used to create the following .csv files that support the fvsUtil
#R package:
#
#support_sp.csv: Contains information from REF_SPECIES.csv. Only SPCD less than
#1000 are contained in this file.
#
#state_codes.csv
#Contains state names, FIPS codes, and state abbreviations.
#
#fortyp_codes.csv
#Contains information from REF_FOREST_TYPE.csv.
################################################################################

#Define directories to read from and write to
read_dir = "C:/FVS_Utility/FVS-Utility-Functions/external"
write_dir = "C:/FVS_Utility/FVS-Utility-Functions/inst/extdata/"

#===============================================================================
# Create support_sp.csv
#===============================================================================

#Read in FIA REF_SPECIES.csv
support_sp <- read.csv(paste0(read_dir, "/", "REF_SPECIES.csv"))[
  c("SPCD", "COMMON_NAME", 
    "GENUS", "SPECIES", 
    "VARIETY", "SUBSPECIES",
    "SCIENTIFIC_NAME", "SPECIES_SYMBOL", 
    "STOCKING_SPGRPCD", "FOREST_TYPE_SPGRPCD", 
    "JENKINS_SPGRPCD", "JENKINS_SAPLING_ADJUSTMENT", 
    "SITETREE", "SFTWD_HRDWD", 
    "WOODLAND", "WOOD_SPGR_GREENVOL_DRYWT",
    "WOOD_SPGR_GREENVOL_DRYWT_CIT", "BARK_SPGR_GREENVOL_DRYWT",  
    "BARK_SPGR_GREENVOL_DRYWT_CIT", "CARBON_RATIO_LIVE", 
    "DRYWT_TO_GREENWT_CONVERSION", "CREATED_DATE", 
    "MODIFIED_DATE")]

#Set character values to uppercase for select columns
cols = c("COMMON_NAME", "GENUS", "SCIENTIFIC_NAME", "SPECIES_SYMBOL", 
         "SFTWD_HRDWD", "WOODLAND")

for(col in cols)
{
  support_sp[col] = toupper(support_sp[[col]])
}

#Order and select species less than 1000
support_sp <- support_sp[order(support_sp$SPCD),]
support_sp <- support_sp[support_sp$SPCD < 1000, ]

#Paste quotes and commas around values in all columns. This logic used to be 
#used when species attributes were hard coded as vectors.

# support_sp$SPCD <- paste0('\"',support_sp$SPCD,'\"',",")
# support_sp$COMMON_NAME <- paste0('\"',support_sp$COMMON_NAME,'\"',",")
# support_sp$GENUS <- paste0('\"',support_sp$GENUS,'\"',",")
# support_sp$SCIENTIFIC_NAME <- paste0('\"',support_sp$SCIENTIFIC_NAME,'\"',",")
# support_sp$SPECIES_SYMBOL <- paste0('\"',support_sp$SPECIES_SYMBOL,'\"',",")
# support_sp$SFTWD_HRDWD <- paste0('\"',support_sp$SFTWD_HRDWD,'\"',",")
# support_sp$WOODLAND <- paste0('\"',support_sp$WOODLAND,'\"',",")

write.csv(support_sp,
          file= paste0(write_dir, "/", "support_sp.csv"),
          row.names = F)

#===============================================================================
# Create state_codes.csv
#===============================================================================

state_codes <- data.frame(STATE_NAME = c("ALABAMA",                        "ALASKA",
                                          "ARIZONA",                        "ARKANSAS",
                                          "CALIFORNIA",                     "COLORADO",
                                          "CONNECTICUT",                    "DELAWARE",
                                          "DISTRICT OF COLUMBIA",           "FLORIDA",
                                          "GEORGIA",                        "HAWAII",
                                          "IDAHO",                          "ILLINOIS",
                                          "INDIANA",                        "IOWA",
                                          "KANSAS",                         "KENTUCKY",
                                          "LOUISIANA",                      "MAINE",
                                          "MARYLAND",                       "MASSACHUSETTS",
                                          "MICHIGAN",                       "MINNESOTA",
                                          "MISSISSIPPI",                    "MISSOURI",
                                          "MONTANA",                        "NEBRASKA",
                                          "NEVADA",                         "NEW HAMPSHIRE",
                                          "NEW JERSEY",                     "NEW MEXICO",
                                          "NEW YORK",                       "NORTH CAROLINA",
                                          "NORTH DAKOTA",                   "OHIO",
                                          "OKLAHOMA",                       "OREGON",
                                          "PENNSYLVANIA",                   "RHODE ISLAND",
                                          "SOUTH CAROLINA",                 "SOUTH DAKOTA",
                                          "TENNESSEE",                      "TEXAS",
                                          "UTAH",                           "VERMONT",
                                          "VIRGINIA",                       "WASHINGTON",
                                          "WEST VIRGINIA",                  "WISCONSIN",
                                          "WYOMING",                        "AMERICAN SAMOA",
                                          "FEDERATED STATES OF MICRONESIA", "GUAM",
                                          "MARSHALL ISLANDS",               "NORTHERN MARIANA ISLANDS",
                                          "PALAU",                          "PUERTO RICO",
                                          "US VIRGIN ISLANDS"),
                          FIPS_CODE = c(1, 2,
                                         4, 5,
                                         6, 8,
                                         9, 10, 
                                         11, 12,
                                         13, 15, 
                                         16, 17, 
                                         18, 19, 
                                         20, 21,
                                         22, 23, 
                                         24, 25,
                                         26, 27, 
                                         28, 29, 
                                         30, 31, 
                                         32, 33, 
                                         34, 35, 
                                         36, 37, 
                                         38, 39, 
                                         40, 41, 
                                         42, 44, 
                                         45, 46, 
                                         47, 48, 
                                         49, 50, 
                                         51, 53, 
                                         54, 55, 
                                         56, 60, 
                                         64, 66, 
                                         68, 69, 
                                         70, 72,
                                         78),
                          STATE_ABBRV = c("AL", "AK",
                                          "AZ", "AR",
                                          "CA", "CO",
                                          "CT", "DE",
                                          "DC", "FL",
                                          "GA", "HI",
                                          "ID", "IL",
                                          "IN", "IA",
                                          "KS", "KY",
                                          "LA", "ME",
                                          "MD", "MA",
                                          "MI", "MN",
                                          "MS", "MO",
                                          "MT", "NE",
                                          "NV", "NH",
                                          "NJ", "NM",
                                          "NY", "NC",
                                          "ND", "OH",
                                          "OK", "OR",
                                          "PA", "RI",
                                          "SC", "SD",
                                          "TN", "TX",
                                          "UT", "VT",
                                          "VA", "WA",
                                          "WV", "WI",
                                          "WY", "AS",
                                          "FM", "GU",
                                          "MH", "MP",
                                          "PW", "PR",
                                          "VI"))

write.csv(state_codes,
          file= paste0(write_dir, "/", "state_codes.csv"),
          row.names = F)

#===============================================================================
# Create fortyp_codes.csv
#===============================================================================

#Read in FIA REF_FOREST_TYPE.csv
fortyp_codes <- read.csv(paste0(read_dir, "/", "REF_FOREST_TYPE.csv"))

#Order and fortyp_codes
fortyp_codes <- fortyp_codes[order(fortyp_codes$VALUE),]

write.csv(fortyp_codes,
          file = paste0(write_dir, "/", "fortyp_codes.csv"),
          row.names = F)

#Clean up
rm(list=ls())
