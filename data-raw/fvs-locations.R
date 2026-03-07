################################################################################
#This script creates a list of all location codes by variants (non-tribal codes)
################################################################################

#Vectors of location codes (pulled from forkod.f routines)
ak = c(1004, 1005, 703, 713, 720, 7400, 7401, 7402, 7403, 7404, 7405,
         7406, 7407, 7408, 8134, 8135, 8112)
bm = c(604, 607, 614, 616, 619)
ca = c(505, 506, 508, 511, 514, 610, 611, 710, 711, 712, 518)
ci = c(117, 402, 406, 412, 413, 414)
cr = c(202, 203, 204, 206, 207, 209, 210, 211, 212, 213, 214, 215, 301, 302,
         303, 304, 305, 306, 307, 308, 309, 310, 312, 201, 205, 208, 224, 311)
cs = c(905, 908, 912, 911)
ec = c(606, 608, 617, 699, 603, 613, 621)
em = c(102, 108, 109, 111, 112, 115)
ie = c(103, 104, 105, 106, 621, 110, 113, 114, 116, 117, 118, 613, 102, 109,
         112)
kt = c(103, 104, 105, 106, 621, 110, 113, 114, 116, 117, 118, 613)
ls = c(902, 903, 904, 906, 907, 909, 910, 913, 924)
nc = c(505, 510, 514, 611, 705, 800, 712, 518, 507, 508, 715)
ne = c(914, 922, 919, 920, 921, 911, 930)
oc = c(505, 506, 508, 511, 514, 610, 611, 710, 711, 712, 518)
op = c(609, 612, 800, 708, 709, 712)
pn = c(609, 612, 800, 708, 709, 712)
sn = c(701, 824, 80101, 80103, 80104, 80105, 80106, 80107, 80211, 80212, 
         80213, 80214, 80215, 80216, 80217, 80301, 80302, 80304, 80305, 80306,
         80307, 80308, 80401, 80402, 80403, 80404, 80405, 80406, 80501, 80502,
         80504, 80505, 80506, 80601, 80602, 80603, 80604, 80605, 80701, 80702,
         80704, 80705, 80706, 80707, 80717, 80802, 80803, 80804, 80805, 80806,
         80811, 80812, 80813, 80814, 80901, 80902, 80903, 80904, 80905, 80906,
         80907, 80908, 80909, 80910, 80911, 80912, 81001, 81002, 81003, 81004,
         81005, 81006, 81007, 81102, 81103, 81105, 81107, 81108, 81109, 81110,
         81111, 81201, 81202, 81203, 81205, 81301, 81303, 81304, 81307, 81308)
so = c(601, 602, 620, 505, 506, 509, 511, 701, 514, 799, 702)
tt = c(403, 405, 415, 416)
ut = c(401, 407, 408, 410, 418, 419, 404, 409, 417)
wc = c(603, 605, 606, 610, 615, 618, 708, 709, 710, 711, 613)
ws = c(503, 511, 513, 515, 516, 517, 501, 502, 504, 507, 512, 519, 417)

#Combine PV_CODES for all variants
fvs_locs = do.call("rbind",
                   list(data.frame(VARIANT = "AK",
                                   LOCATION = ak),
                        data.frame(VARIANT = "BM",
                                   LOCATION = bm),
                        data.frame(VARIANT = "CA",
                                   LOCATION = ca),
                        data.frame(VARIANT = "CI",
                                   LOCATION = ci),
                        data.frame(VARIANT = "CR",
                                   LOCATION = cr),
                        data.frame(VARIANT = "CS",
                                   LOCATION = cs),
                        data.frame(VARIANT = "EC",
                                   LOCATION = ec),
                        data.frame(VARIANT = "EM",
                                   LOCATION = em),
                        data.frame(VARIANT = "IE",
                                   LOCATION = ie),
                        data.frame(VARIANT = "KT",
                                   LOCATION = kt),
                        data.frame(VARIANT = "LS",
                                   LOCATION = ls),
                        data.frame(VARIANT = "NC",
                                   LOCATION = nc),
                        data.frame(VARIANT = "NE",
                                   LOCATION = ne),
                        data.frame(VARIANT = "OC",
                                   LOCATION = oc),
                        data.frame(VARIANT = "OP",
                                   LOCATION = op),
                        data.frame(VARIANT = "PN",
                                   LOCATION = pn),
                        data.frame(VARIANT = "SN",
                                   LOCATION = sn),
                        data.frame(VARIANT = "SO",
                                   LOCATION = so),
                        data.frame(VARIANT = "TT",
                                   LOCATION = tt),
                        data.frame(VARIANT = "UT",
                                   LOCATION = ut),
                        data.frame(VARIANT = "WC",
                                   LOCATION = wc),
                        data.frame(VARIANT = "WS",
                                   LOCATION = ws)))

#Write csv with PV_CODES
write.csv(file = "C:/FVS_Utility/FVS-Utility-Functions/inst/extdata/fvs_locs.csv",
          x = fvs_locs,
          row.names = FALSE)

#Clean up
rm(list = ls())
