################################################################################
#Commons.R
#
#This file contains saves common data to sysdata.rda. This data used by various 
#functions in the fvstools package.
################################################################################

root <- "/home/mark/FVS_Tools/fvstools"
data_raw <- file.path(root, "data-raw")
sysdata  <- file.path(root, "R")

variants <- c(
  "AK","BM","CA","CI","CR","CS","EC","EM","IE","KT",
  "LS","NC","NE","OC","OP","PN","SN","SO","TT","UT",
  "WC","WS"
)

fvs_species <- read.csv(
  file.path(data_raw, "fvs_species.csv"),
  colClasses = c(
    "character",
    "integer",
    "character",
    "character",
    "character"
  )
)

fvs_locs <- read.csv(file.path(data_raw, "fvs_locs.csv"))
pv_codes <- read.csv(file.path(data_raw, "pv_codes.csv"))
pv_codes_regions <- read.csv(file.path(data_raw, "pv_codes_regions.csv"))
support_sp <- read.csv(file.path(data_raw, "support_sp.csv"))
state_codes <- read.csv(file.path(data_raw, "state_codes.csv"))
fortyp_codes <- read.csv(file.path(data_raw, "fortyp_codes.csv"))

fvs_seq_list <- readRDS(file.path(data_raw, "fvs_seq_list.rds"))
fvs_char_list <- readRDS(file.path(data_raw, "fvs_char_list.rds"))
fvs_fia_list <- readRDS(file.path(data_raw, "fvs_fia_list.rds"))
fvs_plant_list <- readRDS(file.path(data_raw, "fvs_plant_list.rds"))
fvs_loc_list <- readRDS(file.path(data_raw, "fvs_loc_list.rds"))
pvcode_list <- readRDS(file.path(data_raw, "pvcode_list.rds"))
habpvr_list <- readRDS(file.path(data_raw, "habpvr_list.rds"))
pvcode_reg_list <- readRDS(file.path(data_raw, "pvcode_reg_list.rds"))
pvref_list <- readRDS(file.path(data_raw, "pvref_list.rds"))

save(
  variants,
  fvs_species,
  fvs_locs,
  pv_codes,
  pv_codes_regions,
  support_sp,
  state_codes,
  fortyp_codes,
  fvs_seq_list,
  fvs_char_list,
  fvs_fia_list,
  fvs_plant_list,
  fvs_loc_list,
  pvcode_list,
  habpvr_list,
  pvcode_reg_list,
  pvref_list,
  file = file.path(sysdata , "sysdata.rda"),
  compress = "xz"
)
