################################################################################
#SupportSP_Create.R
#
#This script is used to create the supportSP .csv file that is read into the 
#Commons.R file of the fvsUtil R package.
################################################################################

#===============================================================================
# Derive read in REF_SPECIES.csv table
#===============================================================================

#Read in FIA REF_SPECIES.csv
supportSP <- read.csv("C:/FVS_Utility/fvsUtil/external/REF_SPECIES.csv")

#Set character values to uppercase for select columns
cols = c("COMMON_NAME", "GENUS", "SCIENTIFIC_NAME", "SPECIES_SYMBOL", 
         "SFTWD_HRDWD", "WOODLAND")

for(col in cols)
{
  supportSP[col] = toupper(supportSP[[col]])
}

#===============================================================================
#Order dataframe by SPCD (FIA species code) and get SPCD less than 1000
#===============================================================================

supportSP <- supportSP[order(supportSP$SPCD),]
supportSP <- supportSP[supportSP$SPCD < 1000, ]

#===============================================================================
#Paste quotes and commas around values in all columns. This logic used to be 
#used when species attributes were hard coded as vectors.
#===============================================================================

# supportSP$SPCD <- paste0('\"',supportSP$SPCD,'\"',",")
# supportSP$COMMON_NAME <- paste0('\"',supportSP$COMMON_NAME,'\"',",")
# supportSP$GENUS <- paste0('\"',supportSP$GENUS,'\"',",")
# supportSP$SCIENTIFIC_NAME <- paste0('\"',supportSP$SCIENTIFIC_NAME,'\"',",")
# supportSP$SPECIES_SYMBOL <- paste0('\"',supportSP$SPECIES_SYMBOL,'\"',",")
# supportSP$SFTWD_HRDWD <- paste0('\"',supportSP$SFTWD_HRDWD,'\"',",")
# supportSP$WOODLAND <- paste0('\"',supportSP$WOODLAND,'\"',",")

#===============================================================================
#Save supportSP as .csv.
#===============================================================================

write.csv(supportSP,
          file= "C:/FVS_Utility/fvsUtil/data-raw/support_sp.csv",
          row.names = F)
