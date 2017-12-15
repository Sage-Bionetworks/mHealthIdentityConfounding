
#####################################
## example 3  
#####################################

set.seed(12345003)
mu3 <- rnorm(nCases + nControls, 0, 10)
dat3 <- SimulateData(nFeatures, 
                     nRecordsRange, 
                     nCases,
                     nControls,
                     mu = mu3,
                     a = 0, 
                     b = 0, 
                     c = 1, 
                     d = 0,
                     rhoRecord = 0.95,
                     rhoFeature = 0.5)
subjectSplit3 <- GetIdxTrainTestSplitBySubject(dat = dat3, 
                                               nSplits = 2, 
                                               subjectIdName = "subjectId",
                                               labelName = "diseaseLabel",
                                               negClassName = "-1", 
                                               posClassName = "1")
recordSplit3 <- GetIdxTrainTestSplitByRecord(dat3, nSplits = 2)


aucR.3 <- GetAUC(dat = dat3, 
                 idxTrain = recordSplit3$idxTrain, 
                 idxTest = recordSplit3$idxTest, 
                 subjectIdName = "subjectId", 
                 labelName = "diseaseLabel", 
                 featNames = paste("feature", seq(10), sep = ""),
                 negClassName = "-1", 
                 posClassName = "1")

aucS.3 <- GetAUC(dat = dat3, 
                 idxTrain = subjectSplit3$idxTrain, 
                 idxTest = subjectSplit3$idxTest, 
                 subjectIdName = "subjectId", 
                 labelName = "diseaseLabel", 
                 featNames = paste("feature", seq(10), sep = ""),
                 negClassName = "-1", 
                 posClassName = "1")



#########################################
## record-wise data split
#########################################

cat("DR - RWS - example 3", "\n")
set.seed(12345003)
dr.rws.3 <- DRPermDistrAUC(dat = dat3, 
                           idxTrain = recordSplit3$idxTrain, 
                           idxTest = recordSplit3$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)

cat("IC - RWS - example 3", "\n")
set.seed(12345003)
ic.rws.3 <- ICPermDistrAUC(dat = dat3, 
                           idxTrain = recordSplit3$idxTrain, 
                           idxTest = recordSplit3$idxTest, 
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

cat("DR - SWS - example 3", "\n")
set.seed(12345003)
dr.sws.3 <- DRPermDistrAUC(dat = dat3, 
                           idxTrain = subjectSplit3$idxTrain, 
                           idxTest = subjectSplit3$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)

cat("IC - SWS - example 3", "\n")
set.seed(12345003)
ic.sws.3 <- ICPermDistrAUC(dat = dat3, 
                           idxTrain = subjectSplit3$idxTrain, 
                           idxTest = subjectSplit3$idxTest, 
                           npermf = nperm2,
                           nperml = nperm3,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)

