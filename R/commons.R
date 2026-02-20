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
#'build_var_sp_list
#'@name build_var_sp_list
#'@description
#'
#'This function is used to build a list of species codes for each variant. A 
#'list can be built for the following species codes:
#'
#'Species sequence numbers
#'
#'FVS species character codes
#'
#'FIA species codes
#'
#'USDA plant symbols
#'
#'@param sp_data:
#'Data frame containing species codes for each variant. The columns in this 
#'data frame should include: VARIANT SEQ	FVS	FIA	PLANT (all uppercase).
#'
#'@param sp_type:   
#'Character string corresponding to what list of species to build. Acceptable
#'values are 'SEQ', 'FVS', 'FIA', 'PLANT'.
#'
#'@return 
#'List of species codes for each variant
################################################################################

build_var_sp_list <- function(sp_data = NULL,
                              sp_type = "SEQ")
{
  #Return empty list if any of below are true
  if(is.null(sp_data) || nrow(sp_data) <= 0 || ncol(sp_data) < 5)
    return(list())
  
  #Return empty list if any column names are missing
  names(sp_data) = toupper(names(sp_data))
  if(any(names(sp_data) %in%
         c("VARIANT", "SEQ", "FVS", "FIA", "PLANT")) == FALSE)
    return(list())
    
  #Uppercase sp_type
  sp_type = toupper(sp_type)
  
  #Do checks on sp_type
  if(!sp_type %in% c("SEQ", "FVS", "FIA", "PLANT")) sp_type = "SEQ"
  
  #Get unique variants from sp_data
  vars = unique(sp_data$VARIANT)
  
  #Initialize species list
  sp_list = vector(mode = "list",
                   length = length(vars))
  
  #Populate sp_list
  for(i in 1:length(vars))
  {
    #Get variant
    var = vars[i]
    
    #Add species to list
    sp_list[[i]] = sp_data[sp_data$VARIANT == var, ][[sp_type]]
    
    #Add name to list
    names(sp_list)[i] = var
  }
  
  return(sp_list)
}

################################################################################
#fvs_seq_list: List of species sequence numbers for each FVS variant.
################################################################################

fvs_seq_list = build_var_sp_list(fvs_species, "SEQ")

################################################################################
#fvs_char_list: List of FVS species codes for each FVS variant.
################################################################################

fvs_char_list = build_var_sp_list(fvs_species,"FVS")

################################################################################
#fvs_fia_list: List of FIA species codes for each FVS variant.
################################################################################

fvs_fia_list = build_var_sp_list(fvs_species, "FIA")

################################################################################
#fvs_plant_list: List of USDA plant symbols for each FVS variant.
################################################################################

fvs_plant_list = build_var_sp_list(fvs_species, "PLANT")

################################################################################
#List of location codes by FVS variant. Does not include tribal codes.
################################################################################

fvs_loc_list <- list(
  "AK" = c(1004, 1005, 703, 713, 720, 7400, 7401, 7402, 7403, 7404, 7405,
           7406, 7407, 7408, 8134, 8135, 8112),
  "BM" = c(604, 607, 614, 616, 619),
  "CA" = c(505, 506, 508, 511, 514, 610, 611, 710, 711, 712, 518),
  "CI" = c(117, 402, 406, 412, 413, 414),
  "CR" = c(202, 203, 204, 206, 207, 209, 210, 211, 212, 213, 214, 215, 301, 302,
           303, 304, 305, 306, 307, 308, 309, 310, 312, 201, 205, 208, 224, 311),
  "CS" = c(905, 908, 912, 911),
  "EC" = c(606, 608, 617, 699, 603, 613, 621),
  "EM" = c(102, 108, 109, 111, 112, 115),
  "IE" = c(103, 104, 105, 106, 621, 110, 113, 114, 116, 117, 118, 613, 102, 109,
           112),
  "KT" = c(103, 104, 105, 106, 621, 110, 113, 114, 116, 117, 118, 613),
  "LS" = c(902, 903, 904, 906, 907, 909, 910, 913, 924),
  "NC" = c(505, 510, 514, 611, 705, 800, 712, 518, 507, 508, 715),
  "NE" = c(914, 922, 919, 920, 921, 911, 930),
  "OC" = c(505, 506, 508, 511, 514, 610, 611, 710, 711, 712, 518),
  "OP" = c(609, 612, 800, 708, 709, 712),
  "PN" = c(609, 612, 800, 708, 709, 712),
  "SN" = c(701, 824, 80101, 80103, 80104, 80105, 80106, 80107, 80211, 80212, 
           80213, 80214, 80215, 80216, 80217, 80301, 80302, 80304, 80305, 80306,
           80307, 80308, 80401, 80402, 80403, 80404, 80405, 80406, 80501, 80502,
           80504, 80505, 80506, 80601, 80602, 80603, 80604, 80605, 80701, 80702,
           80704, 80705, 80706, 80707, 80717, 80802, 80803, 80804, 80805, 80806,
           80811, 80812, 80813, 80814, 80901, 80902, 80903, 80904, 80905, 80906,
           80907, 80908, 80909, 80910, 80911, 80912, 81001, 81002, 81003, 81004,
           81005, 81006, 81007, 81102, 81103, 81105, 81107, 81108, 81109, 81110,
           81111, 81201, 81202, 81203, 81205, 81301, 81303, 81304, 81307, 81308),
  "SO" = c(601, 602, 620, 505, 506, 509, 511, 701, 514, 799, 702),
  "TT" = c(403, 405, 415, 416),
  "UT" = c(401, 407, 408, 410, 418, 419, 404, 409, 417),
  "WC" = c(603, 605, 606, 610, 615, 618, 708, 709, 710, 711, 613),
  "WS" = c(503, 511, 513, 515, 516, 517, 501, 502, 504, 507, 512, 519, 417))

################################################################################
#pv_codes dataframe
################################################################################

pv_codes = read.csv(file = system.file("extdata",
                                       "pv_codes.csv",
                                       package="fvsUtil"))

################################################################################
#'build_pvcode_list
#'@name build_pvcode_list
#'@description
#'
#'This function is used to build a list of PV codes for each variant that uses 
#'them (15 variants).
#'
#'@param pvcode_data:
#'Data frame containing variant codes and PV_CODES for each variant.
#'
#'@return 
#'List of pv codes for each variant
################################################################################

build_pvcode_list <- function(pvcode_data = NULL)
{
  #Return empty list if any of below are true
  if(is.null(pvcode_data) || nrow(pvcode_data) <= 0 || ncol(pvcode_data) < 2)
    return(list())
  
  #Return empty list if any column names are missing
  names(pvcode_data) = toupper(names(pvcode_data))
  if(any(names(pvcode_data) %in%
         c("VARIANT", "PV_CODE")) == FALSE)
    return(list())
  
  #Get unique variants from pvcode_data
  vars = unique(pvcode_data$VARIANT)
  
  #Initialize species list
  pv_list = vector(mode = "list",
                   length = length(vars))
  
  #Populate pv_list
  for(i in 1:length(vars))
  {
    #Get variant
    var = vars[i]
    
    #Add species to list
    pv_list[[i]] = pvcode_data[pvcode_data$VARIANT == var, ][['PV_CODE']]
    
    #Add name to list
    names(pv_list)[i] = var
  }
  
  return(pv_list)
}

################################################################################
#PVCODE list
################################################################################

pvcode_list = build_pvcode_list(pvcode_data = pv_codes)

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