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
library(fvsUtil)

#Get some test data
con = dbConnect(SQLite(),
                 "C:/FVS/Plot Variable Testing/FVSOut.db")

tree = dbGetQuery(con,
                  paste("SELECT",
                  "TL.StandID, TL.CaseID, Year, TreeID, DBH, SpeciesFVS, Ht, TPA, CrWidth, TCuFt, MCuFt, SCuFt, BdFt",
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

#Test species group
sp_group = c('DF', 'GF', 'AF')

#Testing sequence for plot variables using dplyr
fvs_sum = tree %>%
  group_by(CaseID, StandID, Year) %>%
  mutate(TREEBA = DBH^2 * TPA * fvsUtil:::for_constant) %>%
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
            AVGHT_ = avg_attr(dbh = DBH, attr = Ht, weight = TPA, avgtype = 2),
            BAWTD_ = avg_attr(dbh = DBH, attr = DBH, weight = TREEBA, avgtype = 2),
            BAWTH_ = avg_attr(dbh = DBH, attr = Ht, weight = TREEBA, avgtype = 2),
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
            AVGHTSP = avg_attr(dbh = DBH, attr = Ht, weight = TPA, avgtype = 2, species = SpeciesFVS, select_species = sp_group)) %>%
  arrange(CaseID, Year)
fvs_sum = as.data.frame(fvs_sum)

#Test if fvs_sum and comp are equivalent
all.equal(comp, fvs_sum)

#Now look at data.table
tree <- as.data.table(tree)

#Testing sequence for plot variables using data.table
#Note that chaining is done with multiple [] in sequence.
fvs_sum2 = tree[, TREEBA := DBH^2 * TPA * fvsUtil:::for_constant][, .(
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
  AVGHT_ = avg_attr(dbh = DBH, attr = Ht, weight = TPA, avgtype = 2),
  BAWTD_ = avg_attr(dbh = DBH, attr = DBH, weight = TREEBA, avgtype = 2),
  BAWTH_ = avg_attr(dbh = DBH, attr = Ht, weight = TREEBA, avgtype = 2),
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
  AVGHTSP = avg_attr(dbh = DBH, attr = Ht, weight = TPA, avgtype = 2, species = SpeciesFVS, select_species = sp_group)), 
by = .(CaseID, StandID, Year)][order(CaseID, Year)]

#Test if fvs_sum2 and comp are equivalent
all.equal(as.data.frame(fvs_sum2), comp)

#Clean up
rm(list=ls()); gc()

#Example of how to use dplyr with parse and eval. You could theoretically write
#dplyr queries as a character string in R (or externally in a text file) and
#then evaluate the expression with eval.
# fvs_sum = eval(parse(text = "tree %>%
#   group_by(StandID, Year) %>%
#   mutate(TREEBA = sum(DBH * TPA * for_constant)) %>%
#   summarize(BA_ = ba(dbh = DBH, expf = TPA),
#             TPA_ = tpa(dbh = DBH, expf = TPA),
#             QMD_ = qmd(dbh = DBH, expf = TPA),
#             RSDI_ = rsdi_stage(dbh = DBH, expf = TPA),
#             ZSDI_ = zsdi(dbh = DBH, expf = TPA),
#             TCUFT_ = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA),
#             MCUFT_ = expand_attr(dbh = DBH, attr = MCuFt, expf = TPA),
#             SCUFT_ = expand_attr(dbh = DBH, attr = SCuFt, expf = TPA),
#             BDFT_ = expand_attr(dbh = DBH, attr = BdFt, expf = TPA),
#             CC_ = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, corrected = TRUE),
#             top_ht_ = top_ht(dbh = DBH, expf = TPA, ht = Ht),
#             TOPQMD_ = topQMD(dbh = DBH, expf = TPA),
#             XBAWTD = avg_attr(dbh = DBH, attr = DBH, weight = TREEBA, avgtype = 2),
#             XBAWTH = avg_attr(dbh = DBH, attr = Ht, weight = TREEBA, avgtype = 2),
#             XAVGHT = avg_attr(dbh = DBH, attr = Ht, weight = TPA, avgtype = 2),
#             XBAG5 = ba(dbh = DBH, expf = TPA, dbhmin = 5),
#             XTPAG5 = tpa(dbh = DBH, expf = TPA, dbhmin = 5),
#             XQMDG5 = qmd(dbh = DBH, expf = TPA, dbhmin = 5),
#             XRSDIG5 = rsdi_stage(dbh = DBH, expf = TPA, dbhmin = 5),
#             XZSDIG5 = zsdi(dbh = DBH, expf = TPA, dbhmin = 5),
#             XTCUFTG5 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, dbhmin = 5),
#             XCCG5 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, dbhmin = 5, corrected = TRUE),
#             XBAL5 = ba(dbh = DBH, expf = TPA, dbhmax = 5),
#             XTPAL5 = tpa(dbh = DBH, expf = TPA, dbhmax = 5),
#             XQMDL5 = qmd(dbh = DBH, expf = TPA, dbhmax = 5),
#             XRSDIL5 = rsdi_stage(dbh = DBH, expf = TPA, dbhmax = 5),
#             XZSDIL5 = zsdi(dbh = DBH, expf = TPA, dbhmax = 5),
#             XTCUFTL5 = expand_attr(dbh = DBH, attr = TCuFt, expf = TPA, dbhmax = 5),
#             XCCL5 = cc(dbh = DBH, crwidth = CrWidth, expf = TPA, dbhmax = 5, corrected = TRUE))"))
#

# fvs_sum = as.data.frame(fvs_sum)
# fvs_sum
