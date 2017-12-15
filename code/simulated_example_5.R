
#####################################
## example 5
#####################################

set.seed(12345005)
mu5 <- rnorm(nCases + nControls, 0, 0)
sig5 <- runif(nCases + nControls, 1, 10)
dat5 <- SimulateData(nFeatures, 
                     nRecordsRange, 
                     nCases,
                     nControls,
                     mu = mu5,
                     a = 0, 
                     b = 0, 
                     c = 1, 
                     d = 0,
                     rhoRecord = 0.95,
                     rhoFeature = 0.5,
                     sig = sig5)
subjectSplit5 <- GetIdxTrainTestSplitBySubject(dat = dat5, 
                                               nSplits = 2, 
                                               subjectIdName = "subjectId",
                                               labelName = "diseaseLabel",
                                               negClassName = "-1", 
                                               posClassName = "1")
recordSplit5 <- GetIdxTrainTestSplitByRecord(dat5, nSplits = 2)


aucR.5 <- GetAUC(dat = dat5, 
                 idxTrain = recordSplit5$idxTrain, 
                 idxTest = recordSplit5$idxTest, 
                 subjectIdName = "subjectId", 
                 labelName = "diseaseLabel", 
                 featNames = paste("feature", seq(10), sep = ""),
                 negClassName = "-1", 
                 posClassName = "1")

aucS.5 <- GetAUC(dat = dat5, 
                 idxTrain = subjectSplit5$idxTrain, 
                 idxTest = subjectSplit5$idxTest, 
                 subjectIdName = "subjectId", 
                 labelName = "diseaseLabel", 
                 featNames = paste("feature", seq(10), sep = ""),
                 negClassName = "-1", 
                 posClassName = "1")



#########################################
## record-wise data split
#########################################

cat("DR - RWS - example 5", "\n")
set.seed(12345005)
dr.rws.5 <- DRPermDistrAUC(dat = dat5, 
                           idxTrain = recordSplit5$idxTrain, 
                           idxTest = recordSplit5$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)

cat("IC - RWS - example 5", "\n")
set.seed(12345005)
ic.rws.5 <- ICPermDistrAUC(dat = dat5, 
                           idxTrain = recordSplit5$idxTrain, 
                           idxTest = recordSplit5$idxTest, 
                           npermf = nperm2,
                           nperml = nperm3,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)


#########################################
## subject-wise data split
#########################################

cat("DR - SWS - example 5", "\n")
set.seed(12345005)
dr.sws.5 <- DRPermDistrAUC(dat = dat5, 
                           idxTrain = subjectSplit5$idxTrain, 
                           idxTest = subjectSplit5$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)

cat("IC - SWS - example 5", "\n")
set.seed(12345005)
ic.sws.5 <- ICPermDistrAUC(dat = dat5, 
                           idxTrain = subjectSplit5$idxTrain, 
                           idxTest = subjectSplit5$idxTest, 
                           npermf = nperm2,
                           nperml = nperm3,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)


