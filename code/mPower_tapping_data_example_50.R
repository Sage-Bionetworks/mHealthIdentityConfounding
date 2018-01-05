
require(synapseClient)
synapseLogin()

source("utility_functions_disease_recognition_and_identity_confounding.R")


## load the data 
load(getFileLocation(synGet("syn10933730")))

## load matched participants 
load(getFileLocation(synGet("syn10933736")))

## load feature names
load(getFileLocation(synGet("syn10903865")))

dat <- bdatTap
featNames <- tapFeatNames


nperm <- 1000
nRuns <- 30

set.seed(123)
myseeds <- sample(10000:100000, nRuns, replace = TRUE)

outRecordWiseSplit <- vector(mode = "list", length = nRuns)
outSubjectWiseSplit <- vector(mode = "list", length = nRuns)

statsRWS <- matrix(NA, nRuns, 4)
colnames(statsRWS) <- c("auc", "medianDRNull", "permPvalDR", "approxVar")
statsSWS <- statsRWS

drRWS <- matrix(NA, nperm, nRuns)
colnames(drRWS) <- paste("run", seq(nRuns), sep = "")
drSWS <- drRWS

for (i in seq(nRuns)) {
  cat("tap", i, "\n")
  set.seed(myseeds[i])
  recordSplit <- GetIdxTrainTestSplitByRecord(dat, nSplits = 2)
  set.seed(myseeds[i])
  subjectSplit <- GetIdxTrainTestSplitBySubjectBalanced(dat = dat, 
                                                        matchedCaseControlParticipants = matchedCaseControlParticipants, 
                                                        subjectIdName = "healthCode",
                                                        labelName = "professional.diagnosis",
                                                        negClassName = "FALSE", 
                                                        posClassName = "TRUE")
  ####
  aucRWS <- GetAUC(dat = dat, 
                   idxTrain = recordSplit$idxTrain, 
                   idxTest = recordSplit$idxTest, 
                   subjectIdName = "healthCode", 
                   labelName = "professional.diagnosis", 
                   featNames = featNames,
                   negClassName = "FALSE", 
                   posClassName = "TRUE")
  statsRWS[i, "auc"] <- aucRWS$aucObs
  statsRWS[i, "approxVar"] <- aucRWS$approxVar["v"]
  ####
  aucSWS <- GetAUC(dat = dat, 
                   idxTrain = subjectSplit$idxTrain, 
                   idxTest = subjectSplit$idxTest, 
                   subjectIdName = "healthCode", 
                   labelName = "professional.diagnosis", 
                   featNames = featNames,
                   negClassName = "FALSE", 
                   posClassName = "TRUE")
  statsSWS[i, "auc"] <- aucSWS$aucObs
  statsSWS[i, "approxVar"] <- aucSWS$approxVar["v"]   
  ####
  drRWS[, i] <- DRPermDistrAUC(dat = dat, 
                               idxTrain = recordSplit$idxTrain, 
                               idxTest = recordSplit$idxTest, 
                               nperm = nperm, 
                               subjectIdName = "healthCode", 
                               labelName = "professional.diagnosis", 
                               featNames = featNames,
                               negClassName = "FALSE", 
                               posClassName = "TRUE",
                               verbose = FALSE)
  statsRWS[i, "medianDRNull"] <- median(drRWS[, i], na.rm = TRUE)
  statsRWS[i, "permPvalDR"] <- sum(drRWS[, i] >= statsRWS[i, "auc"])/nperm
  ####
  drSWS[, i] <- DRPermDistrAUC(dat = dat, 
                               idxTrain = subjectSplit$idxTrain, 
                               idxTest = subjectSplit$idxTest, 
                               nperm = nperm, 
                               subjectIdName = "healthCode", 
                               labelName = "professional.diagnosis", 
                               featNames = featNames,
                               negClassName = "FALSE", 
                               posClassName = "TRUE",
                               verbose = FALSE) 
  statsSWS[i, "medianDRNull"] <- median(drSWS[, i], na.rm = TRUE)
  statsSWS[i, "permPvalDR"] <- sum(drSWS[, i] >= statsSWS[i, "auc"])/nperm  
  
  save(statsRWS, statsSWS, drRWS, drSWS,
       file = "output_mPower_tap_data_50.RData", compress = TRUE)
}

