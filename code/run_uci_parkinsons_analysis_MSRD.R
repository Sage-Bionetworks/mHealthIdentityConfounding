
codePath <- ""
dataPath <- ""
outputPath <- ""

source(paste(codePath, "utility_functions_disease_recognition_and_identity_confounding.R", sep = ""))

load(paste(dataPath, "uci_parkinsons_MSRD.RData", sep = ""))
dim(dat)
head(dat)
str(dat)

dat$SubjectId <- as.character(dat$SubjectId)
dat$class_information <- as.character(dat$class_information)

featNames <- names(dat)[c(2:27)]

myVerbose <- FALSE
myParallel <- FALSE


nperm <- 1000
nRuns <- 30

set.seed(123)
myseeds <- sample(10000:100000, nRuns, replace = TRUE)

statsRWS <- matrix(NA, nRuns, 5)
colnames(statsRWS) <- c("auc", "medianLRNull", "permPvalLR", "approxVar", "pseudoPval")
statsSWS <- statsRWS

drRWS <- matrix(NA, nperm, nRuns)
colnames(drRWS) <- paste("split", seq(nRuns), sep = "")
drSWS <- drRWS


for (i in seq(nRuns)) {
  cat("split", i, "\n")
  set.seed(myseeds[i])
  recordSplit <- GetIdxTrainTestSplitByRecord(dat, nSplits = 2)
  set.seed(myseeds[i])
  subjectSplit <- GetIdxTrainTestSplitBySubject(dat = dat, 
                                                nSplits = 2,
                                                subjectIdName = "SubjectId",
                                                labelName = "class_information",
                                                negClassName = "0", 
                                                posClassName = "1")
  ####
  aucRWS <- GetAUC(dat = dat, 
                   idxTrain = recordSplit$idxTrain, 
                   idxTest = recordSplit$idxTest, 
                   subjectIdName = "SubjectId", 
                   labelName = "class_information", 
                   featNames = featNames,
                   negClassName = "0", 
                   posClassName = "1")
  statsRWS[i, "auc"] <- aucRWS$aucObs
  statsRWS[i, "approxVar"] <- aucRWS$approxVar["v"]
  ####
  ####
  aucSWS <- GetAUC(dat = dat, 
                   idxTrain = subjectSplit$idxTrain, 
                   idxTest = subjectSplit$idxTest, 
                   subjectIdName = "SubjectId", 
                   labelName = "class_information", 
                   featNames = featNames,
                   negClassName = "0", 
                   posClassName = "1")
  statsSWS[i, "auc"] <- aucSWS$aucObs
  statsSWS[i, "approxVar"] <- aucSWS$approxVar["v"]
  ####
  drRWS[, i] <- DRPermDistrAUC(dat = dat, 
                               idxTrain = recordSplit$idxTrain, 
                               idxTest = recordSplit$idxTest, 
                               nperm = nperm, 
                               subjectIdName = "SubjectId", 
                               labelName = "class_information", 
                               featNames = featNames,
                               negClassName = "0", 
                               posClassName = "1",
                               verbose = myVerbose,
                               parallel = myParallel)
  statsRWS[i, "medianLRNull"] <- median(drRWS[, i], na.rm = TRUE)
  statsRWS[i, "permPvalLR"] <- sum(drRWS[, i] >= statsRWS[i, "auc"])/nperm
  statsRWS[i, "pseudoPval"] <- pnorm(statsRWS[i, "medianLRNull"], 0.5, sqrt(aucRWS$approxVar["v"]), lower.tail = FALSE)
  ####
  drSWS[, i] <- DRPermDistrAUC(dat = dat, 
                               idxTrain = subjectSplit$idxTrain, 
                               idxTest = subjectSplit$idxTest, 
                               nperm = nperm, 
                               subjectIdName = "SubjectId", 
                               labelName = "class_information", 
                               featNames = featNames,
                               negClassName = "0", 
                               posClassName = "1",
                               verbose = myVerbose,
                               parallel = myParallel)
  statsSWS[i, "medianLRNull"] <- median(drSWS[, i], na.rm = TRUE)
  statsSWS[i, "permPvalLR"] <- sum(drSWS[, i] >= statsSWS[i, "auc"])/nperm
  statsSWS[i, "pseudoPval"] <- pnorm(statsSWS[i, "medianLRNull"], 0.5, sqrt(aucSWS$approxVar["v"]), lower.tail = FALSE)
  
  save(statsRWS, drRWS, statsSWS, drSWS,
       file = paste(outputPath, "outputs_uci_parkinsons_MSRD.RData", sep = ""), compress = TRUE)
}






