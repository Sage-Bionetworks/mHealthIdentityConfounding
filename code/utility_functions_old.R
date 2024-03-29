library(install.load)
install_load("pROC", "randomForest", "MatchIt")


####################################
## analysis functions
####################################

SubjectWiseLabelShuffling0 <- function(dat, subjectIdName, labelName) {
  ids <- as.character(unique(dat[, subjectIdName]))
  nids <- length(ids)
  dat[, labelName] <- as.character(dat[, labelName])
  labels <- dat[match(ids, dat[, subjectIdName]), labelName]
  slabels <- labels[sample(nids)]
  for (i in seq(nids)) {
    dat[which(dat[, subjectIdName] == ids[i]), labelName] <- slabels[i]
  }
  dat[, labelName] <- as.factor(dat[, labelName])
  
  dat
}


## This function makes sure that we have both labels in the training and test sets.
## It keeps generating new shuffles of the label data until both labels are present
## in the training and test sets.
## 
SubjectWiseLabelShuffling <- function(dat, subjectIdName, labelName, idxTrain, idxTest) {
  aux <- 0
  while (aux == 0) {
    datS <- SubjectWiseLabelShuffling0(dat, subjectIdName, labelName)
    labelsTrain <- unique(datS[idxTrain, labelName])
    labelsTest <- unique(datS[idxTest, labelName])
    if (length(labelsTrain) == 2 & length(labelsTest) == 2) {
      aux <- 1
    }
  }
  
  datS
}


DRPermDistrAUC <- function(dat, 
                           idxTrain,
                           idxTest,
                           nperm, 
                           subjectIdName, 
                           labelName, 
                           featNames,
                           negClassName, 
                           posClassName,
                           verbose = TRUE,
                           parallel=F) {
  dat <- dat[, c(labelName, featNames, subjectIdName)]
  dat[, labelName] <- factor(as.character(dat[, labelName]), 
                             levels = c(negClassName, posClassName)) 
  myFormula <- as.formula(paste(labelName, " ~ ", paste(featNames, collapse = " + ")))
  res_auc <- plyr::llply(1:nperm, .parallel = parallel,  function(num){
    datPS <- SubjectWiseLabelShuffling(dat, subjectIdName, labelName, idxTrain, idxTest)
    fitPS <- randomForest(myFormula, data = datPS[idxTrain,])
    predProbsPS <- predict(fitPS, datPS[idxTest, -1, drop = FALSE], type = "prob")
    rocObjPS <- roc(datPS[idxTest, 1], predProbsPS[, posClassName], direction = "<", 
                    levels = c(negClassName, posClassName)) 
    pROC::auc(rocObjPS)[1]
  })
  unlist(res_auc)
}



RecordWiseFeatureShuffling <- function(dat, featNames) {
  n <- nrow(dat)
  dat[, featNames] <- dat[sample(n), featNames]
  
  dat
}


ICPermDistrAUC <- function(dat, 
                           idxTrain,
                           idxTest,
                           npermf,
                           nperml,
                           subjectIdName, 
                           labelName, 
                           featNames,
                           negClassName, 
                           posClassName,
                           verbose = TRUE) {
  
  dat <- dat[, c(labelName, featNames, subjectIdName)]
  dat[, labelName] <- factor(as.character(dat[, labelName]), 
                             levels = c(negClassName, posClassName)) 
  
  #myFormula <- as.formula(paste(labelName, " ~ ", paste(featNames, collapse = " + ")))
  res_median_auc <- plyr::llply(1:npermf, .parallel = T,  function(num){
      datS <- RecordWiseFeatureShuffling(dat, featNames)
      aux <- DRPermDistrAUC(datS, 
                            idxTrain,
                            idxTest,
                            nperm = nperml, 
                            subjectIdName, 
                            labelName, 
                            featNames,
                            negClassName, 
                            posClassName,
                            verbose = FALSE,
                            parallel=F) ## In a nested setting we dont want to parallelize the inner loop 
     median(aux)
  })
  unlist(res_median_auc)
}


GetAUC <- function(dat,
                   idxTrain, 
                   idxTest, 
                   subjectIdName, 
                   labelName, 
                   featNames,
                   negClassName, 
                   posClassName) {
  dat <- dat[, c(labelName, featNames, subjectIdName)]
  dat[, labelName] <- factor(as.character(dat[, labelName]), 
                             levels = c(negClassName, posClassName)) 
  myFormula <- as.formula(paste(labelName, " ~ ", paste(featNames, collapse = " + ")))
  fit <- randomForest(myFormula, data = dat[idxTrain,])
  predProbs <- predict(fit, dat[idxTest, -1, drop = FALSE], type = "prob")
  rocObj <- roc(dat[idxTest, 1], predProbs[, posClassName], direction = "<", 
                levels = c(negClassName, posClassName))    
  aucObs <- pROC::auc(rocObj)[1]
  
  list(aucObs = aucObs, rocObj = rocObj)
}


GetIdxTrainTestSplitBySubject <- function(dat, 
                                          nSplits = 2, 
                                          subjectIdName,
                                          labelName,
                                          negClassName = "FALSE", 
                                          posClassName = "TRUE") {
  ids <- as.character(unique(dat[, subjectIdName]))
  labels <- dat[match(ids, dat[, subjectIdName]), labelName]
  caseIds <- ids[which(labels == posClassName)]
  controlIds <- ids[which(labels == negClassName)]
  nCase <- length(caseIds)
  nControl <- length(controlIds)  
  testControlIds <- sample(controlIds, round(nControl/nSplits), replace = FALSE)
  trainControlIds <- setdiff(controlIds, testControlIds)
  testCaseIds <- sample(caseIds, round(nCase/nSplits), replace = FALSE)
  trainCaseIds <- setdiff(caseIds, testCaseIds) 
  trainIds <- c(trainControlIds, trainCaseIds)
  testIds <- c(testControlIds, testCaseIds) 
  idxTrain <- dat[, subjectIdName] %in% trainIds
  idxTest <- dat[, subjectIdName] %in% testIds
  
  list(idxTrain = idxTrain, idxTest = idxTest)
}


GetIdxTrainTestSplitByRecord <- function(dat, nSplits = 2) {
  n <- nrow(dat)
  idxTrain <- sample(n, round(n/2), replace = FALSE)
  idxTest <- setdiff(seq(n), idxTrain)
  
  list(idxTrain = idxTrain, idxTest = idxTest)
}




########################################
## data simulation functions
########################################


SimulateData <- function(nFeatures, 
                         nRecordsRange, 
                         nCases,
                         nControls,
                         mu,
                         a, 
                         b, 
                         c, 
                         d, 
                         rhoRecord, 
                         rhoFeature) {
  # n: nFeatures
  # r: nRecords
  # s: nSubjects
  # y_s = beta_s
  Psi <- CreateCorrelationMatrixForFeatures(rho = rhoFeature, p = nFeatures)
  nRecords <- sample(seq(nRecordsRange[1], nRecordsRange[2], by = 1), 1)
  Sigma <- CreateCorrelationMatrixForRecords(rho = rhoRecord, p = nRecords)
  M <- matrix(0, nRecords, nFeatures)
  I_r <- diag(nRecords)
  I_n <- diag(nFeatures)
  nSubjects <- nCases + nControls
  y_s <- c(rep(-1, nControls), rep(1, nCases))[sample(nSubjects)]
  ## generate dat for subject 1
  U_s <- GenerateMatrixNormalErrorMatrix(M, Sigma, I_n)
  V_s <- GenerateMatrixNormalErrorMatrix(M, I_r, I_n)
  E_s <- GenerateMatrixNormalErrorMatrix(M, I_r, Psi)
  feat <- mu[1] + a * y_s[1] + b * U_s + c * V_s + d * E_s
  diseaseLabel <- rep(y_s[1], nRecords)
  subjectId <- rep(1, nRecords)
  ## generate dat for other subjects
  for (i in 2:nSubjects) {
    nRecords <- sample(seq(nRecordsRange[1], nRecordsRange[2], by = 1), 1)
    Sigma <- CreateCorrelationMatrixForRecords(rho = rhoRecord, p = nRecords)
    M <- matrix(0, nRecords, nFeatures)
    I_r <- diag(nRecords)
    U_s <- GenerateMatrixNormalErrorMatrix(M, Sigma, I_n)
    V_s <- GenerateMatrixNormalErrorMatrix(M, I_r, I_n)
    E_s <- GenerateMatrixNormalErrorMatrix(M, I_r, Psi)    
    aux <- mu[i] + a * y_s[i] + b * U_s + c * V_s + d * E_s
    feat <- rbind(feat, aux)
    diseaseLabel <- c(diseaseLabel, rep(y_s[i], nRecords))
    subjectId <- c(subjectId, rep(i, nRecords))
  }
  subjectId <- paste("subject", subjectId, sep = "")
  colnames(feat) <- paste("feature", seq(nFeatures), sep = "")
  diseaseLabel <- as.factor(diseaseLabel)
  dat <- data.frame(subjectId, diseaseLabel, feat)
  
  dat
}


GenerateMatrixNormalErrorMatrix <- function(M, Sigma, Psi) {
  ## M: mean matrix (R by N)
  ## Sigma: covariance across rows (R by R)
  ## Psi: covariance across columns (N by N)
  r <- nrow(M)
  n <- ncol(M)
  Z <- matrix(rnorm(r * n), r, n)
  cholSigma <- chol(Sigma)
  cholPsi <- chol(Psi)
  E <- M + cholSigma %*% Z %*% cholPsi
  
  E
}


CreateCorrelationMatrixForRecords <- function(rho, p) {
  aux1 <- matrix(rep(1:p, p), p, p)
  aux2 <- matrix(rep(1:p, each = p), p, p) 
  rho^abs(aux1 - aux2)
}


CreateCorrelationMatrixForFeatures <- function(rho, p) {
  Psi <- matrix(rho, p, p)
  diag(Psi) <- 1
  
  Psi
}


