
require(synapseClient)
synapseLogin()

source("utility_functions_disease_recognition_and_identity_confounding.R")

## load the data 
load(getFileLocation(synGet("syn10903849")))

## load matched participants 
load(getFileLocation(synGet("syn10903906")))

## load feature names
load(getFileLocation(synGet("syn10903865")))

dat <- bdatTap
featNames <- tapFeatNames

nperm1 <- 10000
nperm2 <- 1000
nperm3 <- 300

set.seed(96115)
recordSplit <- GetIdxTrainTestSplitByRecord(dat, nSplits = 2)
set.seed(96115)
subjectSplit <- GetIdxTrainTestSplitBySubjectBalanced(dat = dat, 
                                                      matchedCaseControlParticipants = matchedCaseControlParticipants, 
                                                      subjectIdName = "healthCode",
                                                      labelName = "professional.diagnosis",
                                                      negClassName = "FALSE", 
                                                      posClassName = "TRUE")

set.seed(12345)

aucRWStap <- GetAUC(dat = dat, 
                    idxTrain = recordSplit$idxTrain, 
                    idxTest = recordSplit$idxTest, 
                    subjectIdName = "healthCode", 
                    labelName = "professional.diagnosis", 
                    featNames = featNames,
                    negClassName = "FALSE", 
                    posClassName = "TRUE")

aucSWStap <- GetAUC(dat = dat, 
                    idxTrain = subjectSplit$idxTrain, 
                    idxTest = subjectSplit$idxTest, 
                    subjectIdName = "healthCode", 
                    labelName = "professional.diagnosis", 
                    featNames = featNames,
                    negClassName = "FALSE", 
                    posClassName = "TRUE")

cat("DR - RWS - tapping", "\n")
drRWStap <- DRPermDistrAUC(dat = dat, 
                           idxTrain = recordSplit$idxTrain, 
                           idxTest = recordSplit$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "healthCode", 
                           labelName = "professional.diagnosis", 
                           featNames = featNames,
                           negClassName = "FALSE", 
                           posClassName = "TRUE",
                           verbose = FALSE,
                           parallel = TRUE)

cat("DR - SWS - tapping", "\n")
drSWStap <- DRPermDistrAUC(dat = dat, 
                           idxTrain = subjectSplit$idxTrain, 
                           idxTest = subjectSplit$idxTest, 
                           nperm = nperm1, 
                           subjectIdName = "healthCode", 
                           labelName = "professional.diagnosis", 
                           featNames = featNames,
                           negClassName = "FALSE", 
                           posClassName = "TRUE",
                           verbose = FALSE,
                           parallel = TRUE) 

cat("IC - RWS - tapping", "\n")
icRWStap <- ICPermDistrAUC(dat = dat, 
                           idxTrain = recordSplit$idxTrain, 
                           idxTest = recordSplit$idxTest, 
                           npermf = nperm2,
                           nperml = nperm3,
                           subjectIdName = "healthCode", 
                           labelName = "professional.diagnosis", 
                           featNames = featNames,
                           negClassName = "FALSE", 
                           posClassName = "TRUE",
                           verbose = FALSE,
                           parallel = TRUE)

cat("IC - SWS - tapping", "\n")
icSWStap <- ICPermDistrAUC(dat = dat, 
                           idxTrain = subjectSplit$idxTrain, 
                           idxTest = subjectSplit$idxTest, 
                           npermf = nperm2,
                           nperml = nperm3,
                           subjectIdName = "healthCode", 
                           labelName = "professional.diagnosis", 
                           featNames = featNames,
                           negClassName = "FALSE", 
                           posClassName = "TRUE",
                           verbose = FALSE,
                           parallel = TRUE) 


save(aucRWStap, aucSWStap, drRWStap, drSWStap, icRWStap, icSWStap, 
     file = "tapping_example_22subj_100recs.RData", compress = TRUE)




