################################################################################
#Commons.R
#
#This file contains common data that is used by various functions in the fvsUtil
#package. The data structures in this file can also be accessed in R sessions 
#by reference the name of the data structure prepended by fvsUtil:::. As an
#example the variants vector could be accessed using the following instruction: 
#fvsUtil:::variants.
################################################################################

################################################################################
#Vector of FVS variants
################################################################################

variants = c("AK", "BM", "CA", "CI", "CR", "CS", "EC", "EM", "IE", "KT",
             "LS", "NC", "NE", "OC", "OP", "PN", "SN", "SO", "TT", "UT",
             "WC", "WS")

################################################################################
#fvs_species dataframe
################################################################################

fvs_species = read.csv(file = system.file("extdata", 
                                          "fvs_species.csv", 
                                          package="fvsUtil"),
                       colClasses = c("character", 
                                      "integer", 
                                      "character",
                                      "character",
                                      "character"))

################################################################################
#fvs_locs dataframe
################################################################################

fvs_locs = read.csv(file = system.file("extdata",
                                       "fvs_locs.csv",
                                       package="fvsUtil"))

################################################################################
#pv_codes dataframe
################################################################################

pv_codes = read.csv(file = system.file("extdata",
                                       "pv_codes.csv",
                                       package="fvsUtil"))

################################################################################
#pv_codes_regions dataframe
################################################################################

pv_codes_regions = read.csv(file = system.file("extdata",
                                               "pv_codes_regions.csv",
                                               package="fvsUtil"))

################################################################################
#support_sp data frame
################################################################################

support_sp = read.csv(system.file("extdata",
                                  "support_sp.csv",
                                  package="fvsUtil"))

################################################################################
#state_codes data frame
################################################################################

state_codes = read.csv(system.file("extdata", 
                                   "state_codes.csv", 
                                   package="fvsUtil"))

################################################################################
#fortyp_codes data frame
################################################################################

fortyp_codes = read.csv(system.file("extdata",
                                    "fortyp_codes.csv",
                                    package="fvsUtil"))

################################################################################
#fvs_seq_list: List of species sequence numbers for each FVS variant.
################################################################################

fvs_seq_list = readRDS(file = system.file("extdata",
                                          "fvs_seq_list.rds",
                                          package="fvsUtil"))

################################################################################
#fvs_char_list: List of FVS species codes for each FVS variant.
################################################################################

fvs_char_list = readRDS(file = system.file("extdata",
                                           "fvs_char_list.rds",
                                           package="fvsUtil"))

################################################################################
#fvs_fia_list: List of FIA species codes for each FVS variant.
################################################################################

fvs_fia_list = readRDS(file = system.file("extdata",
                                          "fvs_fia_list.rds",
                                          package="fvsUtil"))

################################################################################
#fvs_plant_list: List of USDA plant symbols for each FVS variant.
################################################################################

fvs_plant_list = readRDS(file = system.file("extdata",
                                            "fvs_plant_list.rds",
                                            package="fvsUtil"))

################################################################################
#fvs_loc_list: List of location codes by FVS variant. Does not include tribal 
#codes.
################################################################################

fvs_loc_list = readRDS(file = system.file("extdata",
                                          "fvs_loc_list.rds",
                                          package="fvsUtil"))
  
################################################################################
#pvcode_list: list of PV codes by FVS variant
################################################################################

pvcode_list = readRDS(file = system.file("extdata",
                                         "pvcode_list.rds",
                                         package="fvsUtil"))

################################################################################
#habpvr_list: List of PV codes recognized by FVS variant and region
################################################################################

habpvr_list = readRDS(file = system.file("extdata",
                                         "habpvr_list.rds",
                                         package="fvsUtil"))

################################################################################
#pvcode_reg_list: List of PV codes by FVS variant and region
################################################################################

pvcode_reg_list = readRDS(file = system.file("extdata",
                                             "pvcode_reg_list.rds",
                                             package="fvsUtil"))

################################################################################
#pvref_list: List of PV reference codes by FVS variant and region
################################################################################

pvref_list = readRDS(file = system.file("extdata",
                                        "pvref_list.rds",
                                         package="fvsUtil"))
