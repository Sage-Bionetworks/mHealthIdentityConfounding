
#####################################
## example 6  
#####################################

set.seed(12345006)
mu6 <- rnorm(nCases + nControls, 0, 0)
dat6 <- SimulateData(nFeatures, 
                     nRecordsRange, 
                     nCases,
                     nControls,
                     mu = mu6,
                     a = 0, 
                     b = 0, 
                     c = 1, 
                     d = 0,
                     rhoRecord = 0.95,
                     rhoFeature = 0.5)
subjectSplit6 <- GetIdxTrainTestSplitBySubject(dat = dat6, 
                                               nSplits = 2, 
                                               subjectIdName = "subjectId",
                                               labelName = "diseaseLabel",
                                               negClassName = "-1", 
                                               posClassName = "1")
recordSplit6 <- GetIdxTrainTestSplitByRecord(dat6, nSplits = 2)


aucR.6 <- GetAUC(dat = dat6, 
                idxTrain = recordSplit6$idxTrain, 
                idxTest = recordSplit6$idxTest, 
                subjectIdName = "subjectId", 
                labelName = "diseaseLabel", 
                featNames = paste("feature", seq(10), sep = ""),
                negClassName = "-1", 
                posClassName = "1")

aucS.6 <- GetAUC(dat = dat6, 
                idxTrain = subjectSplit6$idxTrain, 
                idxTest = subjectSplit6$idxTest, 
                subjectIdName = "subjectId", 
                labelName = "diseaseLabel", 
                featNames = paste("feature", seq(10), sep = ""),
                negClassName = "-1", 
                posClassName = "1")



#########################################
## record-wise data split
#########################################

cat("DR - RWS - example 6", "\n")
set.seed(12345006)
dr.rws.6 <- DRPermDistrAUC(dat = dat6, 
                           idxTrain = recordSplit6$idxTrain, 
                           idxTest = recordSplit6$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)

cat("IC - RWS - example 6", "\n")
set.seed(12345006)
ic.rws.6 <- ICPermDistrAUC(dat = dat6, 
                           idxTrain = recordSplit6$idxTrain, 
                           idxTest = recordSplit6$idxTest, 
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

cat("DR - SWS - example 6", "\n")
set.seed(12345006)
dr.sws.6 <- DRPermDistrAUC(dat = dat6, 
                           idxTrain = subjectSplit6$idxTrain, 
                           idxTest = subjectSplit6$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)

cat("IC - SWS - example 6", "\n")
set.seed(12345006)
ic.sws.6 <- ICPermDistrAUC(dat = dat6, 
                           idxTrain = subjectSplit6$idxTrain, 
                           idxTest = subjectSplit6$idxTest, 
                           npermf = nperm2,
                           nperml = nperm3,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = FALSE,
                           parallel = TRUE)


