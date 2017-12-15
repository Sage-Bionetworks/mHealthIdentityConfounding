
#####################################
## example 2  
#####################################

set.seed(12345002)
mu2 <- rnorm(nCases + nControls, 0, 0)
dat2 <- SimulateData(nFeatures, 
                     nRecordsRange, 
                     nCases,
                     nControls,
                     mu = mu2,
                     a = 1, 
                     b = 1, 
                     c = 1, 
                     d = 0.5,
                     rhoRecord = 0.95,
                     rhoFeature = 0.5)
subjectSplit2 <- GetIdxTrainTestSplitBySubject(dat = dat2, 
                                               nSplits = 2, 
                                               subjectIdName = "subjectId",
                                               labelName = "diseaseLabel",
                                               negClassName = "-1", 
                                               posClassName = "1")
recordSplit2 <- GetIdxTrainTestSplitByRecord(dat2, nSplits = 2)


aucR.2 <- GetAUC(dat = dat2, 
                 idxTrain = recordSplit2$idxTrain, 
                 idxTest = recordSplit2$idxTest, 
                 subjectIdName = "subjectId", 
                 labelName = "diseaseLabel", 
                 featNames = paste("feature", seq(10), sep = ""),
                 negClassName = "-1", 
                 posClassName = "1")

aucS.2 <- GetAUC(dat = dat2, 
                 idxTrain = subjectSplit2$idxTrain, 
                 idxTest = subjectSplit2$idxTest, 
                 subjectIdName = "subjectId", 
                 labelName = "diseaseLabel", 
                 featNames = paste("feature", seq(10), sep = ""),
                 negClassName = "-1", 
                 posClassName = "1")



#########################################
## record-wise data split
#########################################

cat("DR - RWS - example 2", "\n")
set.seed(12345002)
dr.rws.2 <- DRPermDistrAUC(dat = dat2, 
                           idxTrain = recordSplit2$idxTrain, 
                           idxTest = recordSplit2$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)

cat("IC - RWS - example 2", "\n")
set.seed(12345002)
ic.rws.2 <- ICPermDistrAUC(dat = dat2, 
                           idxTrain = recordSplit2$idxTrain, 
                           idxTest = recordSplit2$idxTest, 
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

cat("DR - SWS - example 2", "\n")
set.seed(12345002)
dr.sws.2 <- DRPermDistrAUC(dat = dat2, 
                           idxTrain = subjectSplit2$idxTrain, 
                           idxTest = subjectSplit2$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)

cat("IC - SWS - example 2", "\n")
set.seed(12345002)
ic.sws.2 <- ICPermDistrAUC(dat = dat2, 
                           idxTrain = subjectSplit2$idxTrain, 
                           idxTest = subjectSplit2$idxTest, 
                           npermf = nperm2,
                           nperml = nperm3,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)





