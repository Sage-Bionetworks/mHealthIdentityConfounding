
library("install.load")
install_load("pROC", "randomForest", "MatchIt", "doMC", "plyr", "dplyr", "lhs")
registerDoMC(detectCores() - 2)
source("utility_functions_disease_recognition_and_identity_confounding.R")


nSim <- 500
set.seed(12345)
UD <- maximinLHS(nSim, 5)

par.ranges <- list(cRange = c(0.1, 2), 
                   dRange = c(0.1, 2),
                   nRecordsRange = c(15, 15), 
                   nCasesRange = c(5, 10), 
                   nControlsRange = c(5, 10))


is.discrete <- c(FALSE, FALSE, TRUE, TRUE, TRUE)
D <- TransformDesign(x = UD, par.ranges, is.discrete)
colnames(D) <- c("c", "d", "nRecords", "nCases", "nControls")

set.seed(12345001)
myseeds <- sample(10000:100000, nSim, replace = FALSE)

nullSim.1 <- RunNullSimulations(D[1:100,],
                                rhoRecord = 0.95,
                                rhoFeature = 0.5,
                                nperm = 100,
                                nperml = 100,
                                verbose = FALSE,
                                parallel = TRUE,
                                myseeds = myseeds[1:100])
save(nullSim.1, file = "output_null_simulations.RData", compress = TRUE)




nullSim.2 <- RunNullSimulations(D[101:200,],
                                rhoRecord = 0.95,
                                rhoFeature = 0.5,
                                nperm = 100,
                                nperml = 100,
                                verbose = FALSE,
                                parallel = TRUE,
                                myseeds = myseeds[101:200])
save(nullSim.1, nullSim.2, file = "output_null_simulations.RData", compress = TRUE)




nullSim.3 <- RunNullSimulations(D[201:300,],
                                rhoRecord = 0.95,
                                rhoFeature = 0.5,
                                nperm = 100,
                                nperml = 100,
                                verbose = FALSE,
                                parallel = TRUE,
                                myseeds = myseeds[201:300])
save(nullSim.1, nullSim.2, nullSim.3, file = "output_null_simulations.RData", compress = TRUE)




nullSim.4 <- RunNullSimulations(D[301:400,],
                                rhoRecord = 0.95,
                                rhoFeature = 0.5,
                                nperm = 100,
                                nperml = 100,
                                verbose = FALSE,
                                parallel = TRUE,
                                myseeds = myseeds[301:400])
save(nullSim.1, nullSim.2, nullSim.3, nullSim.4, file = "output_null_simulations.RData", compress = TRUE)




nullSim.5 <- RunNullSimulations(D[401:500,],
                                rhoRecord = 0.95,
                                rhoFeature = 0.5,
                                nperm = 100,
                                nperml = 100,
                                verbose = FALSE,
                                parallel = TRUE,
                                myseeds = myseeds[401:500])
save(nullSim.1, nullSim.2, nullSim.3, nullSim.4, nullSim.5, file = "output_null_simulations.RData", compress = TRUE)


