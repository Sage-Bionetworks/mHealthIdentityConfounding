
require(synapseClient)
synapseLogin()

source("utility_functions_disease_recognition_and_identity_confounding.R")

## load the data 
load(getFileLocation(synGet("syn10903828")))

## load matched participants 
load(getFileLocation(synGet("syn10903903")))

## load feature names
load(getFileLocation(synGet("syn10903864")))

dat <- bdatVoi
featNames <- voiFeatNames

nperm1 <- 10000
nperm2 <- 1000
nperm3 <- 300

set.seed(35882)
recordSplit <- GetIdxTrainTestSplitByRecord(dat, nSplits = 2)
set.seed(35882)
subjectSplit <- GetIdxTrainTestSplitBySubjectBalanced(dat = dat, 
                                                      matchedCaseControlParticipants = matchedCaseControlParticipants, 
                                                      subjectIdName = "healthCode",
                                                      labelName = "professional.diagnosis",
                                                      negClassName = "FALSE", 
                                                      posClassName = "TRUE")

set.seed(12345)

aucRWSvoi <- GetAUC(dat = dat, 
                    idxTrain = recordSplit$idxTrain, 
                    idxTest = recordSplit$idxTest, 
                    subjectIdName = "healthCode", 
                    labelName = "professional.diagnosis", 
                    featNames = featNames,
                    negClassName = "FALSE", 
                    posClassName = "TRUE")

aucSWSvoi <- GetAUC(dat = dat, 
                    idxTrain = subjectSplit$idxTrain, 
                    idxTest = subjectSplit$idxTest, 
                    subjectIdName = "healthCode", 
                    labelName = "professional.diagnosis", 
                    featNames = featNames,
                    negClassName = "FALSE", 
                    posClassName = "TRUE")

cat("DR - RWS - voice", "\n")
drRWSvoi <- DRPermDistrAUC(dat = dat, 
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

cat("DR - SWS - voice", "\n")
drSWSvoi <- DRPermDistrAUC(dat = dat, 
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

cat("IC - RWS - voice", "\n")
icRWSvoi <- ICPermDistrAUC(dat = dat, 
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

cat("IC - SWS - voice", "\n")
icSWSvoi <- ICPermDistrAUC(dat = dat, 
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


save(aucRWSvoi, aucSWSvoi, drRWSvoi, drSWSvoi, icRWSvoi, icSWSvoi, 
     file = "voice_example_22subj_100recs.RData", compress = TRUE)


