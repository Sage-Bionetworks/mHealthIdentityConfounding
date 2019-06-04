
codePath <- ""
outputPath <- ""
dataPath <- ""

source(paste(codePath, "utility_functions_disease_recognition_and_identity_confounding.R", sep = ""))

## download the file "parkinsons.data" from 
## https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/
## and save it as a .txt file (uci_parkinsons.txt)

dat <- read.table(paste(dataPath, "uci_parkinsons.txt", sep = ""), header = TRUE, sep = ",")
dim(dat)
head(dat)

subjectId <- strsplit(x = as.character(dat$name), split = "_")
subjectId <- unlist(lapply(subjectId, function(x) x[3]))
dat$subjectId <- subjectId
dat$status <- as.character(dat$status)

featNames <- names(dat)[c(2:17, 19:24)]


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
                                                subjectIdName = "subjectId",
                                                labelName = "status",
                                                negClassName = "0", 
                                                posClassName = "1")
  ####
  aucRWS <- GetAUC(dat = dat, 
                   idxTrain = recordSplit$idxTrain, 
                   idxTest = recordSplit$idxTest, 
                   subjectIdName = "subjectId", 
                   labelName = "status", 
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
                   subjectIdName = "subjectId", 
                   labelName = "status", 
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
                               subjectIdName = "subjectId", 
                               labelName = "status", 
                               featNames = featNames,
                               negClassName = "0", 
                               posClassName = "1",
                               verbose = FALSE,
                               parallel = FALSE)
  statsRWS[i, "medianLRNull"] <- median(drRWS[, i], na.rm = TRUE)
  statsRWS[i, "permPvalLR"] <- sum(drRWS[, i] >= statsRWS[i, "auc"])/nperm
  statsRWS[i, "pseudoPval"] <- pnorm(statsRWS[i, "medianLRNull"], 0.5, sqrt(aucRWS$approxVar["v"]), lower.tail = FALSE)
  ####
  drSWS[, i] <- DRPermDistrAUC(dat = dat, 
                               idxTrain = subjectSplit$idxTrain, 
                               idxTest = subjectSplit$idxTest, 
                               nperm = nperm, 
                               subjectIdName = "subjectId", 
                               labelName = "status", 
                               featNames = featNames,
                               negClassName = "0", 
                               posClassName = "1",
                               verbose = FALSE,
                               parallel = FALSE)
  statsSWS[i, "medianLRNull"] <- median(drSWS[, i], na.rm = TRUE)
  statsSWS[i, "permPvalLR"] <- sum(drSWS[, i] >= statsSWS[i, "auc"])/nperm
  statsSWS[i, "pseudoPval"] <- pnorm(statsSWS[i, "medianLRNull"], 0.5, sqrt(aucSWS$approxVar["v"]), lower.tail = FALSE)

  save(statsRWS, drRWS, statsSWS, drSWS,
       file = paste(outputPath, "outputs_uci_parkinsons.RData", sep = ""), compress = TRUE)
}



