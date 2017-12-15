
library(pROC)
library(randomForest)
library(MatchIt)
library(plyr)
library(dplyr)
library(lhs)

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
                           verbose = FALSE,
                           parallel = TRUE) {
  dat <- dat[, c(labelName, featNames, subjectIdName)]
  dat[, labelName] <- factor(as.character(dat[, labelName]), 
                             levels = c(negClassName, posClassName)) 
  myFormula <- as.formula(paste(labelName, " ~ ", paste(featNames, collapse = " + ")))
  if (verbose) {
    res_auc <- plyr::llply(1:nperm, .parallel = parallel,  function(num){
      datPS <- SubjectWiseLabelShuffling(dat, subjectIdName, labelName, idxTrain, idxTest)
      fitPS <- randomForest(myFormula, data = datPS[idxTrain,])
      predProbsPS <- predict(fitPS, datPS[idxTest, -1, drop = FALSE], type = "prob")
      rocObjPS <- roc(datPS[idxTest, 1], predProbsPS[, posClassName], direction = "<", 
                      levels = c(negClassName, posClassName)) 
      pROC::auc(rocObjPS)[1]
    }, .progress = progress_text(char = "."))
  }
  else {
    res_auc <- plyr::llply(1:nperm, .parallel = parallel,  function(num){
      datPS <- SubjectWiseLabelShuffling(dat, subjectIdName, labelName, idxTrain, idxTest)
      fitPS <- randomForest(myFormula, data = datPS[idxTrain,])
      predProbsPS <- predict(fitPS, datPS[idxTest, -1, drop = FALSE], type = "prob")
      rocObjPS <- roc(datPS[idxTest, 1], predProbsPS[, posClassName], direction = "<", 
                      levels = c(negClassName, posClassName)) 
      pROC::auc(rocObjPS)[1]
    })
  }
  
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
                           verbose = FALSE,
                           parallel = TRUE) {
  
  dat <- dat[, c(labelName, featNames, subjectIdName)]
  dat[, labelName] <- factor(as.character(dat[, labelName]), 
                             levels = c(negClassName, posClassName)) 
  
  if (verbose) {
    res_median_auc <- plyr::llply(1:npermf, .parallel = parallel,  function(num){
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
                            parallel = FALSE) ## In a nested setting we dont want to parallelize the inner loop 
      median(aux)
    }, .progress = progress_text(char = "."))
  }
  else {
    res_median_auc <- plyr::llply(1:npermf, .parallel = parallel,  function(num){
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
                            parallel = FALSE) ## In a nested setting we dont want to parallelize the inner loop 
      median(aux)
    })
  }
  
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
  approxVar <- GetNormApproxVarAUC(dat[idxTest, 1], predProbs, negClassName, posClassName)
  
  list(aucObs = aucObs, rocObj = rocObj, approxVar = approxVar)
}


GetNormApproxVarAUC <- function(ytest, predProbs, negClassName, posClassName) {
  GetTieStats <- function(x) {
    tj <- 0
    u <- unique(x)
    if (length(x) > length(u)) {
      idupli <- which(duplicated(x))
      ud <- unique(x[idupli])
      tau <- length(ud)
      tj <- rep(NA, tau)
      for (i in seq(tau)) {
        tj[i] <- sum(x == ud[i])
      }
    }
    
    list(tj = tj, aux = sum(tj * (tj - 1) * (tj + 1)))
  }
  ytest <- factor(ytest)
  ylevels <- levels(ytest)
  n1 <- sum(ytest == negClassName)
  n2 <- sum(ytest == posClassName)
  ties <- GetTieStats(predProbs)
  n <- n1 + n2
  v <- (n + 1)/(12 * n1 * n2) - ties[[2]]/(12 * n1 * n2 * n * (n - 1))
  
  c(v = v, n = n, nNeg = n1, nPos = n2, statTies = ties[[2]])
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
  idxTrain <- which(dat[, subjectIdName] %in% trainIds)
  idxTest <- which(dat[, subjectIdName] %in% testIds)
  
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
                         rhoFeature,
                         sig = rep(1, length(mu))) {
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
  V_s <- sig[1] * GenerateMatrixNormalErrorMatrix(M, I_r, I_n)
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
    V_s <- sig[i] * GenerateMatrixNormalErrorMatrix(M, I_r, I_n)
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


## D space filling design
RunNullSimulations <- function(D,
                               rhoRecord = 0.95,
                               rhoFeature = 0.5,
                               nperm = 100,
                               nperml = 300,
                               verbose = FALSE,
                               parallel = FALSE,
                               myseeds) {
  nSim <- nrow(D)
  
  aucRWS <- rep(NA, nSim)
  aucSWS <- rep(NA, nSim)
  drPermPvalRWS <- rep(NA, nSim)
  icPermPvalRWS <- rep(NA, nSim)
  drPermPvalSWS <- rep(NA, nSim)
  icPermPvalSWS <- rep(NA, nSim)
  pseudoPvalRWS <- rep(NA, nSim)
  pseudoPvalSWS <- rep(NA, nSim)
  null3PvalRWS <- rep(NA, nSim)
  null3PvalSWS <- rep(NA, nSim)
  medianAucRWS <- rep(NA, nSim)
  medianAucSWS <- rep(NA, nSim)
  
  for (i in seq(nSim)) {
    
    cat(i, "\n")
    c <- D[i, "c"]
    d <- D[i, "d"]
    nRecords <- D[i, "nRecords"]
    nCases <- D[i, "nCases"]
    nControls <- D[i, "nControls"]
    nFeatures <- 10
    a <- 0
    b <- 0
    mus <- rep(0, nCases + nControls)
    sigs <- rep(1, nCases + nControls)
    
    set.seed(myseeds[i])
    dat <- SimulateData(nFeatures, c(nRecords - 5, nRecords + 5), nCases, nControls,
                        mus, a, b, c, d, rhoRecord, rhoFeature, sigs)
    
    subjectSplit <- GetIdxTrainTestSplitBySubject(dat = dat, 
                                                  nSplits = 2, 
                                                  subjectIdName = "subjectId",
                                                  labelName = "diseaseLabel",
                                                  negClassName = "-1", 
                                                  posClassName = "1")
    recordSplit <- GetIdxTrainTestSplitByRecord(dat, nSplits = 2)
    
    aucR <- GetAUC(dat = dat, 
                   idxTrain = recordSplit$idxTrain, 
                   idxTest = recordSplit$idxTest, 
                   subjectIdName = "subjectId", 
                   labelName = "diseaseLabel", 
                   featNames = paste("feature", seq(10), sep = ""),
                   negClassName = "-1", 
                   posClassName = "1")
    aucRWS[i] <- aucR$aucObs
    
    aucS <- GetAUC(dat = dat, 
                   idxTrain = subjectSplit$idxTrain, 
                   idxTest = subjectSplit$idxTest, 
                   subjectIdName = "subjectId", 
                   labelName = "diseaseLabel", 
                   featNames = paste("feature", seq(10), sep = ""),
                   negClassName = "-1", 
                   posClassName = "1")
    aucSWS[i] <- aucS$aucObs
    
    cat("DR - RWS - ", i, "\n")
    drRWS <- DRPermDistrAUC(dat = dat, 
                            idxTrain = recordSplit$idxTrain,
                            idxTest = recordSplit$idxTest,
                            nperm = nperm, 
                            subjectIdName = "subjectId", 
                            labelName = "diseaseLabel", 
                            featNames = paste("feature", seq(nFeatures), sep = ""),
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = verbose,
                            parallel = parallel)
    medianAucRWS[i] <- median(drRWS, na.rm = TRUE)
    drPermPvalRWS[i] <- sum(drRWS >= aucR$aucObs)/nperm
    pseudoPvalRWS[i] <- pnorm(medianAucRWS[i], 0.5, sqrt(aucR$approxVar[1]), lower.tail = FALSE)
    null3PvalRWS[i] <- pnorm(aucR$aucObs, 0.5, sqrt(aucR$approxVar[1]), lower.tail = FALSE)
    
    cat("DR - SWS - ", i, "\n")
    drSWS <- DRPermDistrAUC(dat = dat, 
                            idxTrain = subjectSplit$idxTrain,
                            idxTest = subjectSplit$idxTest,
                            nperm = nperm, 
                            subjectIdName = "subjectId", 
                            labelName = "diseaseLabel", 
                            featNames = paste("feature", seq(nFeatures), sep = ""),
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = verbose,
                            parallel = parallel)
    medianAucSWS[i] <- median(drSWS, na.rm = TRUE)  
    drPermPvalSWS[i] <- sum(drSWS >= aucS$aucObs)/nperm
    pseudoPvalSWS[i] <- pnorm(medianAucSWS[i], 0.5, sqrt(aucS$approxVar[1]), lower.tail = FALSE)
    null3PvalSWS[i] <- pnorm(aucS$aucObs, 0.5, sqrt(aucS$approxVar[1]), lower.tail = FALSE)
    
    cat("IC - RWS - ", i, "\n")
    icRWS <- ICPermDistrAUC(dat = dat, 
                            idxTrain = recordSplit$idxTrain,
                            idxTest = recordSplit$idxTest,
                            npermf = nperm, 
                            nperml = nperml,
                            subjectIdName = "subjectId", 
                            labelName = "diseaseLabel", 
                            featNames = paste("feature", seq(nFeatures), sep = ""),
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = verbose,
                            parallel = parallel)
    icPermPvalRWS[i] <- sum(icRWS >= medianAucRWS[i])/nperm
    
    cat("IC - SWS - ", i, "\n")
    icSWS <- ICPermDistrAUC(dat = dat, 
                            idxTrain = subjectSplit$idxTrain,
                            idxTest = subjectSplit$idxTest,
                            npermf = nperm,
                            nperml = nperml,
                            subjectIdName = "subjectId", 
                            labelName = "diseaseLabel", 
                            featNames = paste("feature", seq(nFeatures), sep = ""),
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = verbose,
                            parallel = parallel)  
    icPermPvalSWS[i] <- sum(icSWS >= medianAucSWS[i])/nperm
    
  }
  
  list(aucRWS = aucRWS,
       aucSWS = aucSWS,
       drPermPvalRWS = drPermPvalRWS,
       icPermPvalRWS = icPermPvalRWS,
       drPermPvalSWS = drPermPvalSWS,
       icPermPvalSWS = icPermPvalSWS,
       pseudoPvalRWS = pseudoPvalRWS,
       pseudoPvalSWS = pseudoPvalSWS,
       null3PvalRWS = null3PvalRWS,
       null3PvalSWS = null3PvalSWS,
       medianAucRWS = medianAucRWS,
       medianAucSWS = medianAucSWS)
}


TransformDesign <- function(x, par.ranges, is.discrete) {
  for (i in 1:ncol(x)) {
    par.range <- par.ranges[[i]]
    par.min <- min(par.range)
    par.max <- max(par.range)
    x[, i] <- x[, i] * (par.max - par.min) + par.min
    if (is.discrete[[i]]) {
      x[, i] <- round(x[, i])
    }
  }
  x
}



######################################
## mPower specific functions
######################################


FilterOutParticipantsWithFewRecords <- function(dat, thr) {
  aux <- table(dat$healthCode)
  aux <- sort(aux, decreasing = TRUE)
  keep <- names(aux)[which(aux >= thr)]
  
  dat[dat$healthCode %in% keep,]
}


## balanced by age and diagnosis
GetIdxTrainTestSplitBySubjectBalanced <- function(dat, 
                                                  matchedCaseControlParticipants,
                                                  subjectIdName = "healthCode",
                                                  labelName = "professional.diagnosis",
                                                  negClassName = "FALSE", 
                                                  posClassName = "TRUE") {
  
  ids <- as.character(unique(dat$healthCode))
  n <- nrow(matchedCaseControlParticipants)
  nTrain <- ceiling(n/2)
  idxTrainCases <- sample(seq(n), nTrain, replace = FALSE)
  idxTrainControls <- setdiff(seq(n), idxTrainCases)
  trainHealthCodes <- c(matchedCaseControlParticipants[idxTrainCases, "case"], 
                        matchedCaseControlParticipants[idxTrainControls, "control"])
  testHealthCodes <- setdiff(ids, trainHealthCodes)
  idxTrain <- which(dat[, subjectIdName] %in% trainHealthCodes)
  idxTest <- which(dat[, subjectIdName] %in% testHealthCodes)
  
  list(idxTrain = idxTrain, idxTest = idxTest)
}




##########################################
## helpers for the manuscript figures
##########################################


FeaturePlot <- function(dat, cols = c("darkgreen", "darkorange"), mycex = 0.5, mycexlab = 1) {
  dat$subjectId <- as.character(dat$subjectId)
  tb <- as.data.frame.matrix(table(dat$subjectId, dat$diseaseLabel))
  tb[tb != 0] <- 1
  dx <- tb[, 2] + 1
  names(dx) <- rownames(tb)
  lb <- min(dat[, -c(1, 2)])
  ub <- max(dat[, -c(1, 2)])
  par(mar = c(4, 3, 1, 1) + 0.1, mgp = c(2, 0.75, 0))
  boxplot(feature1 ~ subjectId, dat, at = seq(1, 400, by = 20), xaxt = "n", 
          ylim = c(lb, ub), col = cols[dx], border = cols[dx], cex = mycex,
          ylab = "feature values")
  axis(side = 1, at = seq(5.5, 404.5, by = 20), labels = FALSE)
  legend("topleft", legend = c("cases", "controls"), text.col = rev(cols), bty = "n")
  mtext(rownames(tb), side = 1, at = seq(5.5, 404.5, by = 20), las = 2, line = 1, cex = mycexlab)
  boxplot(feature2 ~ subjectId, dat, at = seq(2, 401, by = 20), xaxt = "n", 
          add = TRUE, col = cols[dx], border = cols[dx], cex = mycex)
  boxplot(feature3 ~ subjectId, dat, at = seq(3, 402, by = 20), xaxt = "n", 
          add = TRUE, col = cols[dx], border = cols[dx], cex = mycex)
  boxplot(feature4 ~ subjectId, dat, at = seq(4, 403, by = 20), xaxt = "n", 
          add = TRUE, col = cols[dx], border = cols[dx], cex = mycex)
  boxplot(feature5 ~ subjectId, dat, at = seq(5, 404, by = 20), xaxt = "n", 
          add = TRUE, col = cols[dx], border = cols[dx], cex = mycex)
  boxplot(feature6 ~ subjectId, dat, at = seq(6, 405, by = 20), xaxt = "n", 
          add = TRUE, col = cols[dx], border = cols[dx], cex = mycex)
  boxplot(feature7 ~ subjectId, dat, at = seq(7, 406, by = 20), xaxt = "n", 
          add = TRUE, col = cols[dx], border = cols[dx], cex = mycex)
  boxplot(feature8 ~ subjectId, dat, at = seq(8, 407, by = 20), xaxt = "n", 
          add = TRUE, col = cols[dx], border = cols[dx], cex = mycex)
  boxplot(feature9 ~ subjectId, dat, at = seq(9, 408, by = 20), xaxt = "n", 
          add = TRUE, col = cols[dx], border = cols[dx], cex = mycex)
  boxplot(feature10 ~ subjectId, dat, at = seq(10, 409, by = 20), xaxt = "n", 
          add = TRUE, col = cols[dx], border = cols[dx], cex = mycex)
}


MyRocPlot <- function(rocR, rocS, cols = c("brown", "purple"), lwds = c(1, 1), cex.leg = 0.75) {
  xaxis <- seq(0, 1, length.out = 100)
  yaxis <- xaxis
  plot(xaxis, yaxis, type = "n", xlab = "1 - specificity", ylab = "sensitivity")
  abline(a = 0, b = 1, col = "grey")
  lines(1 - rocR$rocObj$specificities, rocR$rocObj$sensitivities, col = cols[1], lwd = lwds[1])
  lines(1 - rocS$rocObj$specificities, rocS$rocObj$sensitivities, col = cols[2], lwd = lwds[2])
  leg1 <- paste("record-wise data split (AUC = ", round(rocR$aucObs, 2), ")", sep = "")
  leg2 <- paste("subject-wise data split (AUC = ", round(rocS$aucObs, 2), ")", sep = "")
  legend("bottomright", legend = c(leg1, leg2), text.col = cols, bty = "n", cex = cex.leg)
}


GetSubSampledStandardDev <- function(x, B = 100, N = 1000) {
  out <- matrix(NA, B, ncol(x))
  colnames(out) <- colnames(x)
  idx <- seq(nrow(x))
  for (i in seq(B)) {
    #cat(i, "\n")
    aux <- sample(idx, N, replace = FALSE)
    xx <- x[aux,]
    out[i,] <- apply(xx, 2, sd)
  }
  
  out
}


SyntheticDataExampleFigure <- function(dat,
                                       dr.rws,
                                       ic.rws,
                                       dr.sws,
                                       ic.sws,
                                       aucR,
                                       aucS,
                                       rgbalpha = 0.75,
                                       yupper1 = 11,
                                       yupper2 = 50,
                                       yupper3 = 100,
                                       nc1 = 20,
                                       nc2 = 5,
                                       mylwd = 1.5,
                                       cm = 1,
                                       cl = 1,
                                       my.cex.lab = 1,
                                       my.cex.axis = 1,
                                       myline = -5,
                                       mycols = c("brown", "black"),
                                       mylwds = c(1, 1)) {
  mat <- matrix(c(1, 1, 2, 2, 2, 2,
                  1, 1, 2, 2, 2, 2,
                  1, 1, 2, 2, 2, 2,
                  3, 3, 3, 3, 3, 3,
                  3, 3, 3, 3, 3, 3,
                  4, 4, 4, 4, 4, 4,
                  4, 4, 4, 4, 4, 4,
                  5, 5, 5, 5, 5, 5,
                  5, 5, 5, 5, 5, 5,
                  6, 6, 6, 6, 6, 6,
                  6, 6, 6, 6, 6, 6), 11, 6, byrow = TRUE)
  or <- aucR$aucObs
  os <- aucS$aucObs
  xaxis <- seq(0, 1, by = 0.001)
  approxDensityR <- dnorm(xaxis, 0.5, sqrt(aucR$approxVar["v"]))
  approxDensityS <- dnorm(xaxis, 0.5, sqrt(aucS$approxVar["v"]))
  layout(mat)
  ##############
  par(mar = c(1.5, 3, 1, 1) + 0.1, mgp = c(2, 0.75, 0))
  ##############
  MyRocPlot(rocR = aucR, rocS = aucS, cols = mycols, lwds = mylwds)
  mtext("(a)", side = 3, at = 0)
  ##############
  FeaturePlot(dat = dat, cols = c("darkgreen", "darkorange"), mycex = 0.5, mycexlab = 0.6)
  mtext("(b)", side = 3, at = 0)
  ##############
  par(mar = c(0, 3, 2, 1) + 0.1, mgp = c(2, 0.75, 0))
  ##############
  hist(dr.rws, probability = TRUE, xlim = c(0, 1), col = rgb(0, 0, 1, rgbalpha), 
       main = "", xlab = "AUC", ylim = c(0, yupper1),
       nclass = nc1, cex.main = cm, cex.lab = my.cex.lab, cex.axis = my.cex.axis, xaxt = "n")
  axis(side = 1, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = FALSE)
  legend("topleft", legend = c("", "disease recognition null"),
         text.col = c(mycols[1], rgb(0, 0, 1, rgbalpha)), bty = "n", cex = cl)
  segments(x0 = or, x1 = or, y0 = -5, y1 = 8, col = mycols[1], lwd = mylwd)
  mtext("(c)", side = 3, at = 0.05, line = myline)
  mtext("record-wise train/test data split", side = 3, at = 0.5, line = -1, cex = 0.8, col = "brown")
  ##############
  par(mar = c(2, 3, 0, 1) + 0.1, mgp = c(2, 0.75, 0))
  ##############
  hist(ic.rws, probability = TRUE, xlim = c(0, 1), col = rgb(1, 0, 0, rgbalpha),
       main = "", xlab = "", ylim = c(0, yupper2), 
       nclass = nc1, cex.main = cm, cex.lab = my.cex.lab, cex.axis = my.cex.axis)
  lines(xaxis, approxDensityR, col = "darkgrey", lwd = mylwd)
  segments(x0 = median(dr.rws), x1 = median(dr.rws), y0 = -5, y1 = yupper2, col = rgb(0, 0, 1, rgbalpha), lwd = mylwd)
  legend("topleft", legend = c("", "subject identification null"),
         text.col = c(mycols[1], rgb(1, 0, 0, rgbalpha)), bty = "n", cex = cl)
  mtext("(d)", side = 3, at = 0.05, line = myline)
  mtext("test statistic", side = 1, at = 0.5, line = 1, cex = 0.75)
  ##############
  par(mar = c(0, 3, 2, 1) + 0.1, mgp = c(2, 0.75, 0))
  ##############
  hist(dr.sws, probability = TRUE, xlim = c(0, 1), col = rgb(0, 0, 1, rgbalpha), 
       main = "", xlab = "AUC", ylim = c(0, yupper1),
       nclass = nc1, cex.main = cm, cex.lab = my.cex.lab, cex.axis = my.cex.axis, xaxt = "n")
  axis(side = 1, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = FALSE)
  legend("topleft", legend = c("", "disease recognition null"),
         text.col = c(mycols[2], rgb(0, 0, 1, rgbalpha)), bty = "n", cex = cl)
  segments(x0 = os, x1 = os, y0 = -5, y1 = 8, col = mycols[2], lwd = mylwd)
  mtext("(e)", side = 3, at = 0.05, line = myline)
  mtext("subject-wise train/test data split", side = 3, at = 0.5, line = -1, cex = 0.8)
  ##############
  par(mar = c(2, 3, 0, 1) + 0.1, mgp = c(2, 0.75, 0))
  ##############
  hist(ic.sws, probability = TRUE, xlim = c(0, 1), col = rgb(1, 0, 0, rgbalpha),
       main = "", xlab = "", 
       nclass = nc2, cex.main = cm, ylim = c(0, yupper3), cex.lab = my.cex.lab, cex.axis = my.cex.axis)
  lines(xaxis, approxDensityS, col = "darkgrey", lwd = mylwd)
  segments(x0 = median(dr.sws), x1 = median(dr.sws), y0 = -5, y1 = yupper3, col = rgb(0, 0, 1, rgbalpha), lwd = mylwd)
  legend("topleft", legend = c("", "subject identification null"), 
         text.col = c(mycols[2], rgb(1, 0, 0, rgbalpha)), bty = "n", cex = cl)
  mtext("(f)", side = 3, at = 0.05, line = myline)
  mtext("test statistic", side = 1, at = 0.5, line = 1, cex = 0.75)
}





