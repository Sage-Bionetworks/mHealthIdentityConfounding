
library("install.load")
install_load("pROC", "randomForest", "MatchIt", "doMC", "plyr", "dplyr")
registerDoMC(detectCores() - 2)
source("utility_functions_disease_recognition_and_identity_confounding.R")

nFeatures <- 10 
nRecordsRange <- c(10, 20) 
nCases <- 13
nControls <- 7
nperm1 <- 1e+4
nperm2 <- 1e+3
nperm3 <- 300


source("simulated_example_1.R")
save(dat1, aucR.1, aucS.1, dr.rws.1, dr.sws.1, ic.rws.1, ic.sws.1, file = "output_synthetic_data_example_1.RData", compress = TRUE)

source("simulated_example_2.R")
save(dat2, aucR.2, aucS.2, dr.rws.2, dr.sws.2, ic.rws.2, ic.sws.2, file = "output_synthetic_data_example_2.RData", compress = TRUE)

source("simulated_example_3.R")
save(dat3, aucR.3, aucS.3, dr.rws.3, dr.sws.3, ic.rws.3, ic.sws.3, file = "output_synthetic_data_example_3.RData", compress = TRUE)

source("simulated_example_4.R")
save(dat4, aucR.4, aucS.4, dr.rws.4, dr.sws.4, ic.rws.4, ic.sws.4, file = "output_synthetic_data_example_4.RData", compress = TRUE)

source("simulated_example_5.R")
save(dat5, aucR.5, aucS.5, dr.rws.5, dr.sws.5, ic.rws.5, ic.sws.5, file = "output_synthetic_data_example_5.RData", compress = TRUE)

source("simulated_example_6.R")
save(dat6, aucR.6, aucS.6, dr.rws.6, dr.sws.6, ic.rws.6, ic.sws.6, file = "output_synthetic_data_example_6.RData", compress = TRUE)



