################################################################################
#This script is used to copy all the PV_Rerference and habitat type and PV code
#routines from a cloned repository to data-raw folder of fvsUtil package.
################################################################################

#Repository path
repo_path = "C:/FVS_GitHub"

#data-raw path
data_raw = "C:/FVS_Utility/FVS-Utility-Functions/data-raw"

#Get pvreffiles
pvref_files = list.files(path = repo_path,
                         recursive = TRUE,
                         pattern = "pvref",
                         full.names = TRUE)

#Copy files
for(file in pvref_files)
{
  file_ = gsub("C:/FVS_GitHub/ForestVegetationSimulator/", "", file)
  file_ = gsub("/", "_", file_)
  
  file.copy(from = file,
            to = file.path(data_raw, file_))
  rm(file_)
}


#Get habtyp files
habtyp_files = list.files(path = repo_path,
                          recursive = TRUE,
                          pattern = "habtyp",
                          full.names = TRUE)

#Copy files
for(file in habtyp_files)
{
  file_ = gsub("C:/FVS_GitHub/ForestVegetationSimulator/", "", file)
  file_ = gsub("/", "_", file_)
  
  file.copy(from = file,
            to = file.path(data_raw, file_))
  rm(file_)
}

