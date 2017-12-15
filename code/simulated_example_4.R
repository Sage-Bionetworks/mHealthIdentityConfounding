
#####################################
## example 4  
#####################################

set.seed(12345004)
mu4 <- rnorm(nCases + nControls, 0, 0)
dat4 <- SimulateData(nFeatures, 
                     nRecordsRange, 
                     nCases,
                     nControls,
                     mu = mu4,
                     a = 1, 
                     b = 0, 
                     c = 2, 
                     d = 0,
                     rhoRecord = 0.95,
                     rhoFeature = 0.5)
subjectSplit4 <- GetIdxTrainTestSplitBySubject(dat = dat4, 
                                               nSplits = 2, 
                                               subjectIdName = "subjectId",
                                               labelName = "diseaseLabel",
                                               negClassName = "-1", 
                                               posClassName = "1")
recordSplit4 <- GetIdxTrainTestSplitByRecord(dat4, nSplits = 2)


aucR.4 <- GetAUC(dat = dat4, 
                 idxTrain = recordSplit4$idxTrain, 
                 idxTest = recordSplit4$idxTest, 
                 subjectIdName = "subjectId", 
                 labelName = "diseaseLabel", 
                 featNames = paste("feature", seq(10), sep = ""),
                 negClassName = "-1", 
                 posClassName = "1")

aucS.4 <- GetAUC(dat = dat4, 
                 idxTrain = subjectSplit4$idxTrain, 
                 idxTest = subjectSplit4$idxTest, 
                 subjectIdName = "subjectId", 
                 labelName = "diseaseLabel", 
                 featNames = paste("feature", seq(10), sep = ""),
                 negClassName = "-1", 
                 posClassName = "1")



#########################################
## record-wise data split
#########################################

cat("DR - RWS - example 4", "\n")
set.seed(12345004)
dr.rws.4 <- DRPermDistrAUC(dat = dat4, 
                           idxTrain = recordSplit4$idxTrain, 
                           idxTest = recordSplit4$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)

cat("IC - RWS - example 4", "\n")
set.seed(12345004)
ic.rws.4 <- ICPermDistrAUC(dat = dat4, 
                           idxTrain = recordSplit4$idxTrain, 
                           idxTest = recordSplit4$idxTest, 
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

cat("DR - SWS - example 4", "\n")
set.seed(12345004)
dr.sws.4 <- DRPermDistrAUC(dat = dat4, 
                           idxTrain = subjectSplit4$idxTrain, 
                           idxTest = subjectSplit4$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)

cat("IC - SWS - example 4", "\n")
set.seed(12345004)
ic.sws.4 <- ICPermDistrAUC(dat = dat4, 
                           idxTrain = subjectSplit4$idxTrain, 
                           idxTest = subjectSplit4$idxTest, 
                           npermf = nperm2,
                           nperml = nperm3,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)


