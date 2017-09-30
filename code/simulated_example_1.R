library("install.load")
install_load("pROC", "randomForest", "MatchIt", "doMC")
registerDoMC(detectCores() - 2)
source("code/utility_functions_for_disease_recognition_and_individual_confounding_tests.R")

nFeatures <- 10 
nRecordsRange <- c(10, 20) 
nCases <- 13
nControls <- 7
nperm <- 1e+4

#####################################
## example 1  
#####################################

set.seed(12345001)
mu1 <- rnorm(nCases + nControls, 0, 0)
dat1 <- SimulateData(nFeatures, 
                     nRecordsRange, 
                     nCases,
                     nControls,
                     mu = mu1,
                     a = 0, 
                     b = 2, 
                     c = 1, 
                     d = 0.5,
                     rhoRecord = 0.95,
                     rhoFeature = 0.5)
subjectSplit1 <- GetIdxTrainTestSplitBySubject(dat = dat1, 
                                               nSplits = 2, 
                                               subjectIdName = "subjectId",
                                               labelName = "diseaseLabel",
                                               negClassName = "-1", 
                                               posClassName = "1")
recordSplit1 <- GetIdxTrainTestSplitByRecord(dat1, nSplits = 2)


aucR1 <- GetAUC(dat = dat1, 
                idxTrain = recordSplit1$idxTrain, 
                idxTest = recordSplit1$idxTest, 
                subjectIdName = "subjectId", 
                labelName = "diseaseLabel", 
                featNames = paste("feature", seq(10), sep = ""),
                negClassName = "-1", 
                posClassName = "1")
aucR1$aucObs

aucS1 <- GetAUC(dat = dat1, 
                idxTrain = subjectSplit1$idxTrain, 
                idxTest = subjectSplit1$idxTest, 
                subjectIdName = "subjectId", 
                labelName = "diseaseLabel", 
                featNames = paste("feature", seq(10), sep = ""),
                negClassName = "-1", 
                posClassName = "1")
aucS1$aucObs


plot(aucR1$rocObj)
plot(aucS1$rocObj, add = TRUE, lty = 2)


#########################################
## record-wise data split
#########################################

set.seed(12345001)
system.time(dr.rws.1 <- DRPermDistrAUC(dat = dat1, 
                           idxTrain = recordSplit1$idxTrain, 
                           idxTest = recordSplit1$idxTest, 
                           nperm = nperm, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = TRUE,
                           parallel = T))


## nperm hardcoded
set.seed(12345001)
system.time(ic.rws.1 <- ICPermDistrAUC(dat = dat1, 
                           idxTrain = recordSplit1$idxTrain, 
                           idxTest = recordSplit1$idxTest, 
                           npermf = 1000,
                           nperml = 30,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = TRUE))







#########################################
## subject-wise data split
#########################################

set.seed(12345001)
cat("record-wise split 1", "\n")
dr.sws.1 <- DRPermDistrAUC(dat = dat1, 
                           idxTrain = subjectSplit1$idxTrain, 
                           idxTest = subjectSplit1$idxTest, 
                           nperm = nperm, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = TRUE)

set.seed(12345001)
cat("record-wise split 1", "\n")
ic.sws.1 <- ICPermDistrAUC(dat = dat1, 
                           idxTrain = subjectSplit1$idxTrain, 
                           idxTest = subjectSplit1$idxTest, 
                           npermf = 1000,
                           nperml = 30,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = TRUE)


######################################################
######################################################
######################################################

nc <- 10
hist(dr.rws.1, probability = TRUE, nclass = nc)
abline(v = aucR1$aucObs)
sum(dr.rws.1 >= aucR1$aucObs)/length(dr.rws.1)

#hist(ic.rws.1, probability = TRUE, nclass = nc)

hist(dr.sws.1, probability = TRUE, nclass = nc)
abline(v = aucS1$aucObs)

sum(dr.sws.1 >= aucS1$aucObs)/length(dr.sws.1)
