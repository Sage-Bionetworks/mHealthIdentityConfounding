
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


aucR.1 <- GetAUC(dat = dat1, 
                idxTrain = recordSplit1$idxTrain, 
                idxTest = recordSplit1$idxTest, 
                subjectIdName = "subjectId", 
                labelName = "diseaseLabel", 
                featNames = paste("feature", seq(10), sep = ""),
                negClassName = "-1", 
                posClassName = "1")

aucS.1 <- GetAUC(dat = dat1, 
                idxTrain = subjectSplit1$idxTrain, 
                idxTest = subjectSplit1$idxTest, 
                subjectIdName = "subjectId", 
                labelName = "diseaseLabel", 
                featNames = paste("feature", seq(10), sep = ""),
                negClassName = "-1", 
                posClassName = "1")


#########################################
## record-wise data split
#########################################

set.seed(12345001)
cat("record-wise split 1", "\n")
dr.rws.1 <- DRPermDistrAUC(dat = dat1, 
                           idxTrain = recordSplit1$idxTrain, 
                           idxTest = recordSplit1$idxTest, 
                           nperm = nperm, 
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = TRUE)

set.seed(12345001)
cat("record-wise split 1", "\n")
ic.rws.1 <- ICPermDistrAUC(dat = dat1, 
                           idxTrain = recordSplit1$idxTrain, 
                           idxTest = recordSplit1$idxTest, 
                           npermf = nperm,
                           nperml = nperml,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = TRUE)


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
                           npermf = nperm,
                           nperml = nperml,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1",
                           verbose = TRUE)

