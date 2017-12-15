library("install.load")
install_load("pROC", "randomForest", "MatchIt", "doMC")
registerDoMC(detectCores() - 2)
source("code/utility_functions_old.R")


nFeatures <- 10 
nRecordsRange <- c(10, 20) 
nCases <- 13
nControls <- 7
nperm <- 1e4

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
recordSplit1 <- GetIdxTrainTestSplitByRecord(dat1, nSplits = 2)


aucR1 <- GetAUC(dat = dat1, 
                idxTrain = recordSplit1$idxTrain, 
                idxTest = recordSplit1$idxTest, 
                subjectIdName = "subjectId", 
                labelName = "diseaseLabel", 
                featNames = paste("feature", seq(10), sep = ""),
                negClassName = "-1", 
                posClassName = "1")
out <- matrix(NA, nperm, 10)
colnames(out) <- seq(100, 1000, by = 100)

set.seed(12345001)
system.time(res_1 <- ICPermDistrAUC(dat = dat1, 
                           idxTrain = recordSplit1$idxTrain, 
                           idxTest = recordSplit1$idxTest, 
                           npermf = nperm,
                           nperml = 100,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1"))
#took 3167 seconds
                           

set.seed(12345002)
system.time(res_2 <- ICPermDistrAUC(dat = dat1, 
                           idxTrain = recordSplit1$idxTrain, 
                           idxTest = recordSplit1$idxTest, 
                           npermf = nperm,
                           nperml = 200,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1"))
#took 6756.90 seconds


set.seed(12345003)
system.time(res_3 <- ICPermDistrAUC(dat = dat1, 
                           idxTrain = recordSplit1$idxTrain, 
                           idxTest = recordSplit1$idxTest, 
                           npermf = nperm,
                           nperml = 300,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1"))
# 9067.711


set.seed(12345004)
system.time(res_4 <- ICPermDistrAUC(dat = dat1, 
                           idxTrain = recordSplit1$idxTrain, 
                           idxTest = recordSplit1$idxTest, 
                           npermf = nperm,
                           nperml = 400,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1"))


set.seed(12345005)
system.time(res_5 <- ICPermDistrAUC(dat = dat1, 
                           idxTrain = recordSplit1$idxTrain, 
                           idxTest = recordSplit1$idxTest, 
                           npermf = nperm,
                           nperml = 500,
                           subjectIdName = "subjectId", 
                           labelName = "diseaseLabel", 
                           featNames = paste("feature", seq(10), sep = ""),
                           negClassName = "-1", 
                           posClassName = "1"))






#####

res_10_50 <- lapply(seq(10,50,by=10), function(x){
  ICPermDistrAUC(dat = dat1, 
                 idxTrain = recordSplit1$idxTrain, 
                 idxTest = recordSplit1$idxTest, 
                 npermf = nperm,
                 nperml = x,
                 subjectIdName = "subjectId", 
                 labelName = "diseaseLabel", 
                 featNames = paste("feature", seq(10), sep = ""),
                 negClassName = "-1", 
                 posClassName = "1")
})


res_60_90 <- lapply(seq(60,90,by=10), function(x){
  ICPermDistrAUC(dat = dat1, 
                 idxTrain = recordSplit1$idxTrain, 
                 idxTest = recordSplit1$idxTest, 
                 npermf = nperm,
                 nperml = x,
                 subjectIdName = "subjectId", 
                 labelName = "diseaseLabel", 
                 featNames = paste("feature", seq(10), sep = ""),
                 negClassName = "-1", 
                 posClassName = "1")
})

sd_AUC <- c(unlist(lapply(res_10_50, sd)), 
unlist(lapply(res_60_90, sd)),
sd(unlist(res_1)),
sd(unlist(res_2)),
sd(unlist(res_3)),
sd(unlist(res_4)),
sd(unlist(res_5)))

res <- data.frame(permutation=c(10,20,30,40,50,60,70,80,90,100,200,300,400,500),
           sd_AUC = sd_AUC)
library(ggplot2)
ggplot(data=res, aes(x=permutation, y=sd_AUC)) + geom_line() + geom_point() + theme_bw()
ggsave(file="permutation_plot.png", width=5, height=5, dpi=200)

save.image("20170930_permutation_combinations.Rdata")
synapseClient::synapseLogin()
synapseClient::synStore(synapseClient::File("20170930_permutation_combinations.Rdata",
                                            parentId = "syn10932612"),
                        executed="https://github.com/Sage-Bionetworks/individualConfoundingTests/blob/master/code/check_necessary_number_of_label_permutations.R")

