################################################################################
#plot-variable-testing.R
#
#This script is used to test the functions from plot-variables.R. Output from
#the functions in plot-variables.R are compared against event monitor output
#from plot-variables-testing.kcp.
################################################################################

library(RSQLite)
library(dplyr)
library(data.table)
library(fvstools)

#Get test data
con = dbConnect(SQLite(),
                 "C:/FVS/Plot Variable Testing/FVSOut.db")

tree = dbGetQuery(con,
                  paste("SELECT",
                  "TL.StandID, TL.CaseID, Year, ActPt, PtIndex, TreeID, DBH, SpeciesFVS, Ht, 
                  TPA, CrWidth, PtBAL, TCuFt, MCuFt, SCuFt, BdFt",
                  "FROM FVS_TreeList as TL",
                  "INNER JOIN FVS_Cases",
                  "ON TL.CaseID = FVS_Cases.CaseID",
                  "WHERE RunTitle = 'CI Run' AND Year < 2076"))

comp = dbGetQuery(con,
                  paste("SELECT",
                        "cmp.*",
                        "FROM FVS_Compute as cmp",
                        "INNER JOIN FVS_Cases",
                        "ON cmp.CaseID = FVS_Cases.CaseID",
                        "WHERE RunTitle = 'CI Run'",
                        "ORDER BY CaseID, Year"))

#Disconnect
dbDisconnect(con)

#Define test species group
sp_group = c('DF', 'GF', 'AF')

#===============================================================================
#Testing sequence for plot variables using dplyr
#===============================================================================

fvs_sum = tree %>%
  group_by(CaseID, StandID, Year) %>%
  #mutate(TREEBA = DBH^2 * TPA * fvstools:::for_constant) %>%
  summarize(BA_ = ba(dbh = DBH, expf = TPA),
            TPA_ = tpa(dbh = DBH, expf = TPA),
            QMD_ = qmd(dbh = DBH, expf = TPA),
            RSDI_ = rsdi_stage(dbh = DBH, expf = TPA),
            ZSDI_ = zsdi(dbh = DBH, expf = TPA),
            TCUFT_ = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA),
            MCUFT_ = expand_attr(dbh = DBH, attr = MCuFt, expf = TPA),
            SCUFT_ = expand_attr(dbh = DBH, attr = SCuFt, expf = TPA),
            BDFT_ = expand_attr(dbh = DBH, attr = BdFt, expf = TPA),
            CC_ = cc(dbh = DBH, crwidth = CrWidth, expf = TPA),
            TOPHT_ = top_ht(dbh = DBH, expf = TPA, ht = Ht),
            AVGHT_ = mean_attr(dbh = DBH, attr = Ht, weight = TPA),
            BAWTD_ = lorey_dia(dbh = DBH, expf = TPA),
            BAWTH_ = lorey_ht(dbh = DBH, ht = Ht, expf = TPA),
            BAG5 = ba(dbh = DBH, expf = TPA, dbhmin = 5),
            TPAG5 = tpa(dbh = DBH, expf = TPA, dbhmin = 5),
            QMDG5 = qmd(dbh = DBH, expf = TPA, dbhmin = 5),
            RSDIG5 = rsdi_stage(dbh = DBH, expf = TPA, dbhmin = 5),
            ZSDIG5 = zsdi(dbh = DBH, expf = TPA, dbhmin = 5),
            TCUFTG5 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, dbhmin = 5),
            CCG5 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, dbhmin = 5),
            BAL5 = ba(dbh = DBH, expf = TPA, dbhmax = 5),
            TPAL5 = tpa(dbh = DBH, expf = TPA, dbhmax = 5),
            QMDL5 = qmd(dbh = DBH, expf = TPA, dbhmax = 5),
            RSDIL5 = rsdi_stage(dbh = DBH, expf = TPA, dbhmax = 5),
            ZSDIL5 = zsdi(dbh = DBH, expf = TPA, dbhmax = 5),
            TCUFTL5 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, dbhmax = 5),
            CCL5 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, dbhmax = 5),
            BAG50 = ba(dbh = DBH, expf = TPA, ht = Ht, htmin = 50),
            TPAG50 = tpa(dbh = DBH, expf = TPA, ht = Ht, htmin = 50),
            QMDG50 = qmd(dbh = DBH, expf = TPA, ht = Ht, htmin = 50),
            RSDIG50 = rsdi_stage(dbh = DBH, expf = TPA, ht = Ht, htmin = 50),
            ZSDIG50 = zsdi(dbh = DBH, expf = TPA, ht = Ht, htmin = 50),
            TCUFTG50 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, ht = Ht, htmin = 50),
            CCG50 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, ht = Ht, htmin = 50),
            BAL50 = ba(dbh = DBH, expf = TPA, ht = Ht, htmax = 50),
            TPAL50 = tpa(dbh = DBH, expf = TPA, ht = Ht, htmax = 50),
            QMDL50 = qmd(dbh = DBH, expf = TPA, ht = Ht, htmax = 50),
            RSDIL50 = rsdi_stage(dbh = DBH, expf = TPA, ht = Ht, htmax = 50),
            ZSDIL50 = zsdi(dbh = DBH, expf = TPA, ht = Ht, htmax = 50),
            TCUFTL50 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, ht = Ht, htmax = 50),
            CCL50 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, ht = Ht, htmax = 50),
            BA5T10 = ba(dbh = DBH, expf = TPA, dbhmin = 5, dbhmax = 10),
            TPA5T10 = tpa(dbh = DBH, expf = TPA, dbhmin = 5, dbhmax = 10),
            QMD5T10 = qmd(dbh = DBH, expf = TPA, dbhmin = 5, dbhmax = 10),
            RSDI5T10 = rsdi_stage(dbh = DBH, expf = TPA, dbhmin = 5, dbhmax = 10),
            ZSDI5T10 = zsdi(dbh = DBH, expf = TPA, dbhmin = 5, dbhmax = 10),
            TCUF5T10 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, dbhmin = 5, dbhmax = 10),
            CCG5T10 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, dbhmin = 5, dbhmax = 10),
            BA50100 = ba(dbh = DBH, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
            TPA50100 = tpa(dbh = DBH, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
            QMD50100 = qmd(dbh = DBH, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
            RSD50100 = rsdi_stage(dbh = DBH, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
            ZSD50100 = zsdi(dbh = DBH, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
            TCU50100 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
            CC50100 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
            BASP = ba(dbh = DBH, expf = TPA, species = SpeciesFVS, select_species = sp_group),
            TPASP = tpa(dbh = DBH, expf = TPA, species = SpeciesFVS, select_species = sp_group),
            QMDSP = qmd(dbh = DBH, expf = TPA, species = SpeciesFVS, select_species = sp_group),
            RSDISP = rsdi_stage(dbh = DBH, expf = TPA, species = SpeciesFVS, select_species = sp_group),
            ZSDISP = zsdi(dbh = DBH, expf = TPA, species = SpeciesFVS, select_species = sp_group),
            TCUFTSP = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, species = SpeciesFVS, select_species = sp_group),
            MCUFTSP = expand_attr(dbh = DBH, attr = MCuFt, expf = TPA, species = SpeciesFVS, select_species = sp_group),
            SCUFTSP = expand_attr(dbh = DBH, attr = SCuFt, expf = TPA, species = SpeciesFVS, select_species = sp_group),
            BDFTSP = expand_attr(dbh = DBH, attr = BdFt, expf = TPA, species = SpeciesFVS, select_species = sp_group),
            CCSP = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, species = SpeciesFVS, select_species = sp_group),
            AVGHTSP = mean_attr(dbh = DBH, attr = Ht, weight = TPA, species = SpeciesFVS, select_species = sp_group),
            RDIA_ = gmd(dbh = DBH, expf = TPA)) %>%
  arrange(CaseID, Year)
fvs_sum = as.data.frame(fvs_sum)

#Test if fvs_sum and comp are equivalent
all.equal(comp, fvs_sum)

#Minor differences in top height seem to be related to sort handling within FVS. 
#Different trees can be included in calculation when there is a tie in diameter
#values.

#Test if top heights and top diameter values are equivalent to individual height
#and diameter calculations when all trees in stand are included in calculation.

size_test = tree %>%
  group_by(CaseID, StandID, Year) %>%
  summarize(QMD1 = qmd(dbh = DBH, expf = TPA),
            QMD2 = top_dia(dbh = DBH, expf = TPA, top_per = 100, dia_type = 1),
            AVGD1 = mean_attr(attr = DBH, weight = TPA),
            AVGD2 = top_dia(dbh = DBH, expf = TPA, top_per = 100, dia_type = 2),
            RDIA1 = gmd(dbh = DBH, expf = TPA),
            RDIA2 = top_dia(dbh = DBH, expf = TPA, top_per = 100, dia_type = 3),
            TOPHT1 = mean_attr(attr = Ht, weight = TPA),
            TOPHT2 = top_ht(dbh = DBH, expf = TPA, ht = Ht, top_per = 100))
size_test= as.data.frame(size_test)

all.equal(size_test$QMD1,
          size_test$QMD2)

all.equal(size_test$AVGD1,
          size_test$AVGD2)

all.equal(size_test$RDIA1,
          size_test$RDIA2)

all.equal(size_test$TOPHT1,
          size_test$TOPHT2)

#Test BAL calculation
tree <- tree |>
  mutate(N = max(PtIndex),
         .by = c(CaseID, StandID, Year)) |>
  mutate(PtBAL2 = round(bal(dbh = DBH, expf = TPA * N), 0), 
         .by = c(CaseID, StandID, ActPt, Year))

all.equal(tree$PtBAL, tree$PtBAL2)

#Mistmatches are related to differences in tree ordering between Fortran and R.
#This occurs primarily for inventory year and those where equal diameters can
#occur.
nrow(tree[tree$PtBAL != tree$PtBAL2, ])

#===============================================================================
#Testing sequence for plot variables using data.table
#===============================================================================

#Create datatable
tree2 = setDT(tree)

fvs_sum2 = tree2[, TREEBA := DBH^2 * TPA * fvstools:::for_constant][, .(
  BA_ = ba(dbh = DBH, expf = TPA),
  TPA_ = tpa(dbh = DBH, expf = TPA),
  QMD_ = qmd(dbh = DBH, expf = TPA),
  RSDI_ = rsdi_stage(dbh = DBH, expf = TPA),
  ZSDI_ = zsdi(dbh = DBH, expf = TPA),
  TCUFT_ = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA),
  MCUFT_ = expand_attr(dbh = DBH, attr = MCuFt, expf = TPA),
  SCUFT_ = expand_attr(dbh = DBH, attr = SCuFt, expf = TPA),
  BDFT_ = expand_attr(dbh = DBH, attr = BdFt, expf = TPA),
  CC_ = cc(dbh = DBH, crwidth = CrWidth, expf = TPA),
  TOPHT_ = top_ht(dbh = DBH, expf = TPA, ht = Ht),
  AVGHT_ = mean_attr(dbh = DBH, attr = Ht, weight = TPA),
  BAWTD_ = mean_attr(dbh = DBH, attr = DBH, weight = TREEBA),
  BAWTH_ = mean_attr(dbh = DBH, attr = Ht, weight = TREEBA),
  BAG5 = ba(dbh = DBH, expf = TPA, dbhmin = 5),
  TPAG5 = tpa(dbh = DBH, expf = TPA, dbhmin = 5),
  QMDG5 = qmd(dbh = DBH, expf = TPA, dbhmin = 5),
  RSDIG5 = rsdi_stage(dbh = DBH, expf = TPA, dbhmin = 5),
  ZSDIG5 = zsdi(dbh = DBH, expf = TPA, dbhmin = 5),
  TCUFTG5 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, dbhmin = 5),
  CCG5 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, dbhmin = 5),
  BAL5 = ba(dbh = DBH, expf = TPA, dbhmax = 5),
  TPAL5 = tpa(dbh = DBH, expf = TPA, dbhmax = 5),
  QMDL5 = qmd(dbh = DBH, expf = TPA, dbhmax = 5),
  RSDIL5 = rsdi_stage(dbh = DBH, expf = TPA, dbhmax = 5),
  ZSDIL5 = zsdi(dbh = DBH, expf = TPA, dbhmax = 5),
  TCUFTL5 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, dbhmax = 5),
  CCL5 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, dbhmax = 5),
  BAG50 = ba(dbh = DBH, expf = TPA, ht = Ht, htmin = 50),
  TPAG50 = tpa(dbh = DBH, expf = TPA, ht = Ht, htmin = 50),
  QMDG50 = qmd(dbh = DBH, expf = TPA, ht = Ht, htmin = 50),
  RSDIG50 = rsdi_stage(dbh = DBH, expf = TPA, ht = Ht, htmin = 50),
  ZSDIG50 = zsdi(dbh = DBH, expf = TPA, ht = Ht, htmin = 50),
  TCUFTG50 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, ht = Ht, htmin = 50),
  CCG50 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, ht = Ht, htmin = 50),
  BAL50 = ba(dbh = DBH, expf = TPA, ht = Ht, htmax = 50),
  TPAL50 = tpa(dbh = DBH, expf = TPA, ht = Ht, htmax = 50),
  QMDL50 = qmd(dbh = DBH, expf = TPA, ht = Ht, htmax = 50),
  RSDIL50 = rsdi_stage(dbh = DBH, expf = TPA, ht = Ht, htmax = 50),
  ZSDIL50 = zsdi(dbh = DBH, expf = TPA, ht = Ht, htmax = 50),
  TCUFTL50 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, ht = Ht, htmax = 50),
  CCL50 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, ht = Ht, htmax = 50),
  BA5T10 = ba(dbh = DBH, expf = TPA, dbhmin = 5, dbhmax = 10),
  TPA5T10 = tpa(dbh = DBH, expf = TPA, dbhmin = 5, dbhmax = 10),
  QMD5T10 = qmd(dbh = DBH, expf = TPA, dbhmin = 5, dbhmax = 10),
  RSDI5T10 = rsdi_stage(dbh = DBH, expf = TPA, dbhmin = 5, dbhmax = 10),
  ZSDI5T10 = zsdi(dbh = DBH, expf = TPA, dbhmin = 5, dbhmax = 10),
  TCUF5T10 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, dbhmin = 5, dbhmax = 10),
  CCG5T10 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, dbhmin = 5, dbhmax = 10),
  BA50100 = ba(dbh = DBH, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
  TPA50100 = tpa(dbh = DBH, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
  QMD50100 = qmd(dbh = DBH, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
  RSD50100 = rsdi_stage(dbh = DBH, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
  ZSD50100 = zsdi(dbh = DBH, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
  TCU50100 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
  CC50100 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, ht = Ht, htmin = 50, htmax = 100),
  BASP = ba(dbh = DBH, expf = TPA, species = SpeciesFVS, select_species = sp_group),
  TPASP = tpa(dbh = DBH, expf = TPA, species = SpeciesFVS, select_species = sp_group),
  QMDSP = qmd(dbh = DBH, expf = TPA, species = SpeciesFVS, select_species = sp_group),
  RSDISP = rsdi_stage(dbh = DBH, expf = TPA, species = SpeciesFVS, select_species = sp_group),
  ZSDISP = zsdi(dbh = DBH, expf = TPA, species = SpeciesFVS, select_species = sp_group),
  TCUFTSP = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, species = SpeciesFVS, select_species = sp_group),
  MCUFTSP = expand_attr(dbh = DBH, attr = MCuFt, expf = TPA, species = SpeciesFVS, select_species = sp_group),
  SCUFTSP = expand_attr(dbh = DBH, attr = SCuFt, expf = TPA, species = SpeciesFVS, select_species = sp_group),
  BDFTSP = expand_attr(dbh = DBH, attr = BdFt, expf = TPA, species = SpeciesFVS, select_species = sp_group),
  CCSP = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, species = SpeciesFVS, select_species = sp_group),
  AVGHTSP = mean_attr(dbh = DBH, attr = Ht, weight = TPA, species = SpeciesFVS, select_species = sp_group)), 
by = .(CaseID, StandID, Year)][order(CaseID, Year)]

#Test if fvs_sum2 and comp are equivalent
all.equal(as.data.frame(fvs_sum2), comp)

#Test BAL calculation
tree2[, PtBAL3 := round(bal(dbh = DBH, expf = TPA * N), 0), 
     by = .(CaseID, StandID, ActPt, Year)]

#Same number of mistmatches as dplyr test
nrow(tree2[tree$PtBAL != tree$PtBAL2,])

#Clean up
rm(list=ls()); gc()

library(fvstools)
getLoadedDLLs()[["fvstools"]]$path
system2("dumpbin", c("/symbols", dll))
        