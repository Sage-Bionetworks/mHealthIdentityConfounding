
require(synapseClient)
synapseLogin()

source("utility_functions_disease_recognition_and_identity_confounding.R")

########################################################
########################################################
########################################################
########################################################
########################################################
## matrix-normal distribution examples figure
## (supplementary figure)
########################################################

nRecords <- 100
nFeatures <- 50

Sigma <- CreateCorrelationMatrixForRecords(rho = 0.95, p = nRecords)
Psi <- CreateCorrelationMatrixForFeatures(rho = 0.5, p = nFeatures)
I_r <- diag(nRecords)
I_f <- diag(nFeatures)

set.seed(1234567)
M <- matrix(0, nRecords, nFeatures)
X0 <- GenerateMatrixNormalErrorMatrix(M, I_r, I_f)
X1 <- GenerateMatrixNormalErrorMatrix(M, Sigma, I_f)
X2 <- GenerateMatrixNormalErrorMatrix(M, I_r, Psi)
X3 <- GenerateMatrixNormalErrorMatrix(M, Sigma, Psi)

cr0 <- cor(t(X0))
cf0 <- cor(X0)

cr1 <- cor(t(X1))
cf1 <- cor(X1)

cr2 <- cor(t(X2))
cf2 <- cor(X2)

cr3 <- cor(t(X3))
cf3 <- cor(X3)

mycol <- heat.colors(n = 100)

par(mfrow = c(2, 4), mar = c(3, 3, 1, 0.1), mgp = c(1.5, 0.75, 0))
image(cr0, xaxt = "n", yaxt = "n", col = mycol, xlab = "rows", ylab = "rows",
      main = expression(MN(0, I, I)), zlim = c(-1, 1))
axis(side = 1, at = seq(nRecords)/nRecords, label = seq(nRecords))
axis(side = 2, at = seq(nRecords)/nRecords, label = seq(nRecords))
mtext("(a)", side = 3, at = 0)
#####
image(cr1, xaxt = "n", yaxt = "n", col = mycol, xlab = "rows", ylab = "rows",
      main = expression(MN(0, Sigma, I)), zlim = c(-1, 1))
axis(side = 1, at = seq(nRecords)/nRecords, label = seq(nRecords))
axis(side = 2, at = seq(nRecords)/nRecords, label = seq(nRecords))
mtext("(b)", side = 3, at = 0)
#####
image(cr2, xaxt = "n", yaxt = "n", col = mycol, xlab = "rows", ylab = "rows",
      main = expression(MN(0, I, Psi)), zlim = c(-1, 1))
axis(side = 1, at = seq(nRecords)/nRecords, label = seq(nRecords))
axis(side = 2, at = seq(nRecords)/nRecords, label = seq(nRecords))
mtext("(c)", side = 3, at = 0)
#####
image(cr3, xaxt = "n", yaxt = "n", col = mycol, xlab = "rows", ylab = "rows",
      main = expression(MN(0, Sigma, Psi)), zlim = c(-1, 1))
axis(side = 1, at = seq(nRecords)/nRecords, label = seq(nRecords))
axis(side = 2, at = seq(nRecords)/nRecords, label = seq(nRecords))
mtext("(d)", side = 3, at = 0)
#####
image(cf0, xaxt = "n", yaxt = "n", col = mycol, xlab = "columns", ylab = "columns",
      main = expression(MN(0, I, I)), zlim = c(-1, 1))
axis(side = 1, at = seq(nFeatures)/nFeatures, label = seq(nFeatures))
axis(side = 2, at = seq(nFeatures)/nFeatures, label = seq(nFeatures))
mtext("(e)", side = 3, at = 0)
#####
image(cf1, xaxt = "n", yaxt = "n", col = mycol, xlab = "columns", ylab = "columns",
      main = expression(MN(0, Sigma, I)), zlim = c(-1, 1))
axis(side = 1, at = seq(nFeatures)/nFeatures, label = seq(nFeatures))
axis(side = 2, at = seq(nFeatures)/nFeatures, label = seq(nFeatures))
mtext("(f)", side = 3, at = 0)
#####
image(cf2, xaxt = "n", yaxt = "n", col = mycol, xlab = "columns", ylab = "columns",
      main = expression(MN(0, I, Psi)), zlim = c(-1, 1))
axis(side = 1, at = seq(nFeatures)/nFeatures, label = seq(nFeatures))
axis(side = 2, at = seq(nFeatures)/nFeatures, label = seq(nFeatures))
mtext("(g)", side = 3, at = 0)
#####
image(cf3, xaxt = "n", yaxt = "n", col = mycol, xlab = "columns", ylab = "columns",
      main = expression(MN(0, Sigma, Psi)), zlim = c(-1, 1))
axis(side = 1, at = seq(nFeatures)/nFeatures, label = seq(nFeatures))
axis(side = 2, at = seq(nFeatures)/nFeatures, label = seq(nFeatures))
mtext("(h)", side = 3, at = 0)




########################################################
########################################################
########################################################
########################################################
## assess a sufficient number of subject-wise
## label permutations for the computation of the 
## identity confounding null distribution
## (supplementary figure)
########################################################

## output generated in AWS using the R script:
## (abhi's script name here)
##
load(getFileLocation(synGet("syn10945245")))

ic <- matrix(NA, 10000, 14)
colnames(ic) <- c(seq(10, 100, by = 10), seq(200, 500, by = 100))

ic[, 1] <- unlist(res_10_50[[1]])
ic[, 2] <- unlist(res_10_50[[2]])
ic[, 3] <- unlist(res_10_50[[3]])
ic[, 4] <- unlist(res_10_50[[4]])
ic[, 5] <- unlist(res_10_50[[5]])
ic[, 6] <- unlist(res_60_90[[1]])
ic[, 7] <- unlist(res_60_90[[2]])
ic[, 8] <- unlist(res_60_90[[3]])
ic[, 9] <- unlist(res_60_90[[4]])
ic[, 10] <- unlist(res_1)
ic[, 11] <- unlist(res_2)
ic[, 12] <- unlist(res_3)
ic[, 13] <- unlist(res_4)
ic[, 14] <- unlist(res_5)

sdObs <- apply(ic, 2, sd)

xaxis <- as.numeric(colnames(ic))

set.seed(123)
sds <- GetSubSampledStandardDev(x = ic, B = 10000, N = 1000)

d10 <- density(ic[, 1])
d20 <- density(ic[, 2])
d30 <- density(ic[, 3])
d40 <- density(ic[, 4])
d50 <- density(ic[, 5])
d60 <- density(ic[, 6])
d70 <- density(ic[, 7])
d80 <- density(ic[, 8])
d90 <- density(ic[, 9])
d100 <- density(ic[, 10])
d200 <- density(ic[, 11])
d300 <- density(ic[, 12])
d400 <- density(ic[, 13])
d500 <- density(ic[, 14])

mat <- matrix(c(1, 1,
                1, 1,
                1, 1,
                2, 3,
                2, 3), 5, 2, byrow = TRUE)


layout(mat)
par(mar = c(4, 4, 1, 1), mgp = c(2, 0.75, 0))
####
boxplot(sds, at = xaxis, cex = 0.5, pars = list(boxwex = 8), 
        ylab = "st. dev. of the identity confounding null distributions",
        xlab = "number of label permutations")
points(xaxis, sdObs, col = "red", pch = 20)
lines(xaxis, sdObs, col = "red", lty = 3)
mtext("(a)", side = 3, at = 500, line = -2, cex = 1.2)
####
plot(d10, ylim = c(0, 40), col = 1, xlab = "test statistic", 
     main = "identity confounding null distributions", cex.main = 1.1)
lines(d20$x, d20$y, col = 2)
lines(d30$x, d30$y, col = 3)
lines(d40$x, d40$y, col = 4)
lines(d50$x, d50$y, col = 5)
lines(d60$x, d60$y, col = 6)
lines(d70$x, d70$y, col = 7)
lines(d80$x, d80$y, col = 8)
lines(d90$x, d90$y, col = 9)
lines(d100$x, d100$y, col = 10)
lines(d200$x, d200$y, col = 11)
lines(d300$x, d300$y, col = 12)
lines(d400$x, d400$y, col = 13)
lines(d500$x, d500$y, col = 14)
legend("topleft", legend = c("# of label perms:", xaxis), text.col = c(1, seq(14)), 
       bty = "n", cex = 0.9)
mtext("(b)", side = 3, at = 0.575, line = -2, cex = 1.2)
####
plot(d10, ylim = c(0, 40), col = 1, xlab = "test statistic", 
     main = "identity confounding null distributions", cex.main = 1.1)
lines(d300$x, d300$y, col = 12)
lines(d400$x, d400$y, col = 13)
lines(d500$x, d500$y, col = 14)
legend("topleft", legend = c("# of label perms:", xaxis[c(1, 12:14)]), text.col = c(1, 1, 12:14), 
       bty = "n", cex = 0.9)
mtext("(c)", side = 3, at = 0.575, line = -2, cex = 1.2)



########################################################
########################################################
########################################################
########################################################
## synthetic data example 1
########################################################

## output generated with the script:
## run_synthetic_data_examples.R 
load(getFileLocation(synGet("syn11564867")))

SyntheticDataExampleFigure(dat = dat1,
                           dr.rws = dr.rws.1,
                           ic.rws = ic.rws.1,
                           dr.sws = dr.sws.1,
                           ic.sws = ic.sws.1,
                           aucR = aucR.1,
                           aucS = aucS.1)



########################################################
########################################################
########################################################
########################################################
## synthetic data example 2
########################################################

## output generated with the script:
## run_synthetic_data_examples.R 
load(getFileLocation(synGet("syn11564868")))

SyntheticDataExampleFigure(dat = dat2,
                           dr.rws = dr.rws.2,
                           ic.rws = ic.rws.2,
                           dr.sws = dr.sws.2,
                           ic.sws = ic.sws.2,
                           aucR = aucR.2,
                           aucS = aucS.2)



########################################################
########################################################
########################################################
########################################################
## synthetic data example 3
########################################################

## output generated with the script:
## run_synthetic_data_examples.R 
load(getFileLocation(synGet("syn11564869")))

SyntheticDataExampleFigure(dat = dat3,
                           dr.rws = dr.rws.3,
                           ic.rws = ic.rws.3,
                           dr.sws = dr.sws.3,
                           ic.sws = ic.sws.3,
                           aucR = aucR.3,
                           aucS = aucS.3)



########################################################
########################################################
########################################################
########################################################
## synthetic data example 4
########################################################

## output generated with the script: 
## run_synthetic_data_examples.R 
load(getFileLocation(synGet("syn11564870")))

SyntheticDataExampleFigure(dat = dat4,
                           dr.rws = dr.rws.4,
                           ic.rws = ic.rws.4,
                           dr.sws = dr.sws.4,
                           ic.sws = ic.sws.4,
                           aucR = aucR.4,
                           aucS = aucS.4)



########################################################
########################################################
########################################################
########################################################
## synthetic data example 5
########################################################

## output generated with the script:
## run_synthetic_data_examples.R
load(getFileLocation(synGet("syn11564871")))

SyntheticDataExampleFigure(dat = dat5,
                           dr.rws = dr.rws.5,
                           ic.rws = ic.rws.5,
                           dr.sws = dr.sws.5,
                           ic.sws = ic.sws.5,
                           aucR = aucR.5,
                           aucS = aucS.5)



########################################################
########################################################
########################################################
########################################################
## synthetic data example 6
########################################################

## output generated with the script:
## run_synthetic_data_examples.R 
load(getFileLocation(synGet("syn11564872")))

SyntheticDataExampleFigure(dat = dat6,
                           dr.rws = dr.rws.6,
                           ic.rws = ic.rws.6,
                           dr.sws = dr.sws.6,
                           ic.sws = ic.sws.6,
                           aucR = aucR.6,
                           aucS = aucS.6)




########################################################
########################################################
########################################################
########################################################
## simulation study (supplementary figure)
########################################################

## output generated with the script:
## run_simulation_study.R 
load(getFileLocation(synGet("syn11565429")))

nullSim <- nullSim.1
nullSim$aucRWS <- c(nullSim.1$aucRWS, nullSim.2$aucRWS, nullSim.3$aucRWS, nullSim.4$aucRWS, nullSim.5$aucRWS)
nullSim$aucSWS <- c(nullSim.1$aucSWS, nullSim.2$aucSWS, nullSim.3$aucSWS, nullSim.4$aucSWS, nullSim.5$aucSWS)

nullSim$drPermPvalRWS <- c(nullSim.1$drPermPvalRWS, nullSim.2$drPermPvalRWS, nullSim.3$drPermPvalRWS, nullSim.4$drPermPvalRWS, nullSim.5$drPermPvalRWS)
nullSim$drPermPvalSWS <- c(nullSim.1$drPermPvalSWS, nullSim.2$drPermPvalSWS, nullSim.3$drPermPvalSWS, nullSim.4$drPermPvalSWS, nullSim.5$drPermPvalSWS)
nullSim$icPermPvalRWS <- c(nullSim.1$icPermPvalRWS, nullSim.2$icPermPvalRWS, nullSim.3$icPermPvalRWS, nullSim.4$icPermPvalRWS, nullSim.5$icPermPvalRWS)
nullSim$icPermPvalSWS <- c(nullSim.1$icPermPvalSWS, nullSim.2$icPermPvalSWS, nullSim.3$icPermPvalSWS, nullSim.4$icPermPvalSWS, nullSim.5$icPermPvalSWS)
nullSim$pseudoPvalRWS <- c(nullSim.1$pseudoPvalRWS, nullSim.2$pseudoPvalRWS, nullSim.3$pseudoPvalRWS, nullSim.4$pseudoPvalRWS, nullSim.5$pseudoPvalRWS)
nullSim$pseudoPvalSWS <- c(nullSim.1$pseudoPvalSWS, nullSim.2$pseudoPvalSWS, nullSim.3$pseudoPvalSWS, nullSim.4$pseudoPvalSWS, nullSim.5$pseudoPvalSWS)
nullSim$null3PvalRWS <- c(nullSim.1$null3PvalRWS, nullSim.2$null3PvalRWS, nullSim.3$null3PvalRWS, nullSim.4$null3PvalRWS, nullSim.5$null3PvalRWS)
nullSim$null3PvalSWS <- c(nullSim.1$null3PvalSWS, nullSim.2$null3PvalSWS, nullSim.3$null3PvalSWS, nullSim.4$null3PvalSWS, nullSim.5$null3PvalSWS)


myylim <- c(0, 7)
cm <- 0.8
myat <- 0.1
cl <- 0.8
ca <- 0.8
cleg <- 0.6


par(mfrow = c(2, 4), mar = c(3, 3, 1, 0.5), mgp = c(1.5, 0.5, 0))
hist(nullSim$drPermPvalRWS, probability = TRUE, ylim = myylim, xlim = c(0, 1), xlab = "p-value", 
     main = "disease recog. perm. test", cex.main = cm, border = "brown", cex.lab = cl, cex.axis = ca)
legend("topright", legend = "record-wise split", text.col = "brown", bty = "n", cex = cleg)
mtext("(a)", side = 3, at = myat, line = -2, cex = cl)
hist(nullSim$icPermPvalRWS, probability = TRUE, ylim = myylim, xlim = c(0, 1), xlab = "p-value", 
     main = "identity conf. perm. test", cex.main = cm, border = "brown", cex.lab = cl, cex.axis = ca)
legend("topright", legend = "record-wise split", text.col = "brown", bty = "n", cex = cleg)
mtext("(b)", side = 3, at = myat, line = -2, cex = cl)
hist(nullSim$pseudoPvalRWS, probability = TRUE, ylim = myylim, xlim = c(0, 1), xlab = "p-value", 
     main = "pseudo p-value", cex.main = cm, border = "brown", cex.lab = cl, cex.axis = ca)
legend("topright", legend = "record-wise split", text.col = "brown", bty = "n", cex = cleg)
mtext("(c)", side = 3, at = myat, line = -2, cex = cl)
hist(nullSim$null3PvalRWS, probability = TRUE, ylim = myylim, xlim = c(0, 1), xlab = "p-value", 
     main = "disease recog. and / or ind. conf.", cex.main = cm, border = "brown", cex.lab = cl, cex.axis = ca)
legend("topright", legend = "record-wise split", text.col = "brown", bty = "n", cex = cleg)
mtext("(d)", side = 3, at = myat, line = -2, cex = cl)
#####
hist(nullSim$drPermPvalSWS, probability = TRUE, ylim = myylim, xlim = c(0, 1), xlab = "p-value", 
     main = "disease recog. perm. test", cex.main = cm, cex.lab = cl, cex.axis = ca)
legend("topright", legend = "subject-wise split", text.col = "black", bty = "n", cex = cleg)
mtext("(e)", side = 3, at = myat, line = -2, cex = cl)
hist(nullSim$icPermPvalSWS, probability = TRUE, ylim = myylim, xlim = c(0, 1), xlab = "p-value", 
     main = "identity conf. perm. test", cex.main = cm, cex.lab = cl, cex.axis = ca)
legend("topright", legend = "subject-wise split", text.col = "black", bty = "n", cex = cleg)
mtext("(f)", side = 3, at = myat, line = -2, cex = cl)
hist(nullSim$pseudoPvalSWS, probability = TRUE, ylim = myylim, xlim = c(0, 1), xlab = "p-value", 
     main = "pseudo p-value", cex.main = cm, cex.lab = cl, cex.axis = ca)
legend("topright", legend = "subject-wise split", text.col = "black", bty = "n", cex = cleg)
mtext("(g)", side = 3, at = myat, line = -2, cex = cl)
hist(nullSim$null3PvalSWS, probability = TRUE, ylim = myylim, xlim = c(0, 1), xlab = "p-value", 
     main = "disease recog. and / or ind. conf.", cex.main = cm, cex.lab = cl, cex.axis = ca)
legend("topright", legend = "subject-wise split", text.col = "black", bty = "n", cex = cleg)
mtext("(h)", side = 3, at = myat, line = -2, cex = cl)



########################################################
########################################################
########################################################
########################################################
## mPower voice example
########################################################

## output generated with the script:
## voice_example.R 
## (used for panels a and b)
load(getFileLocation(synGet("syn11565454")))


## output generated with the script:
## mPower_voice_data_example_100.R 
## (30 distinct train/test data splits 
## used for panels c and d)
load(getFileLocation(synGet("syn11566235")))

nas <- rep(NA, 1000)
pvalR <- c(statsRWS[, "permPvalDR"], rep(NA, 970))
datR <- data.frame(drRWS, nas, nas, pvalR)
colnames(datR) <- c(paste("split", seq(30), sep = " "), "", "", "p-value")
pvalS <- c(statsSWS[, "permPvalDR"], rep(NA, 970))
datS <- data.frame(drSWS, nas, nas, pvalS)
colnames(datS) <- c(paste("split", seq(30), sep = " "), "", "", "p-value")

or <- aucRWSvoi$aucObs
os <- aucSWSvoi$aucObs

xaxis <- seq(0, 1, length.out = 1000)
approxDensityR <- dnorm(xaxis, 0.5, sqrt(aucRWSvoi$approxVar["v"])) ## to be replaced
approxDensityS <- dnorm(xaxis, 0.5, sqrt(aucSWSvoi$approxVar["v"]))

nc1 <- 20
nc2 <- 20
mylwd <- 1
cm <- 1
cl <- 1
my.cex.lab <- 1
my.cex.axis <- 1

mylwd <- 1.5
myline <- 0
mycols <- c("brown", "black")
mylwds <- c(1, 1)

mat <- matrix(c(1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 1, 
                2, 2, 2, 2, 2, 2,
                2, 2, 2, 2, 2, 2,
                3, 3, 3, 4, 4, 4,
                3, 3, 3, 4, 4, 4,
                3, 3, 3, 4, 4, 4), 7, 6, byrow = TRUE)

rgbalpha <- 0.75

yupper1 <- 60
yupper2 <- 65

layout(mat)
##############
par(mar = c(3, 3, 2, 1) + 0.1, mgp = c(2, 0.75, 0))
##############
hist(drRWSvoi, probability = TRUE, xlim = c(0, 1), col = rgb(0, 0, 1, rgbalpha), 
     main = "record-wise data split", xlab = "AUC", ylim = c(0, yupper1),
     nclass = nc1, cex.main = cm, cex.lab = my.cex.lab, cex.axis = my.cex.axis)
lines(xaxis, approxDensityR, col = "darkgrey", lwd = mylwd)
hist(icRWSvoi, probability = TRUE, xlim = c(0, 1), border = rgb(1, 0, 0, rgbalpha), 
     main = "record-wise data split", xlab = "AUC", add = TRUE,
     nclass = nc1, cex.main = cm, cex.lab = my.cex.lab, cex.axis = my.cex.axis)
abline(v = or, col = mycols[1], lwd = mylwd)
legend("topleft", legend = c("disease recognition null", "subject identification null"),
       text.col = c(rgb(0, 0, 1, rgbalpha), rgb(1, 0, 0, rgbalpha)), bty = "n", cex = cl)
mtext("(a)", side = 3, at = 0, line = myline)
##############
hist(drSWSvoi, probability = TRUE, xlim = c(0, 1), col = rgb(0, 0, 1, rgbalpha), 
     main = "subject-wise data split", xlab = "AUC", ylim = c(0, yupper1),
     nclass = nc1, cex.main = cm, cex.lab = my.cex.lab, cex.axis = my.cex.axis)
lines(xaxis, approxDensityS, col = "darkgrey", lwd = mylwd)
hist(icSWSvoi, probability = TRUE, xlim = c(0, 1), border = rgb(1, 0, 0, rgbalpha), 
     main = "subject-wise data split", xlab = "AUC", add = TRUE,
     nclass = nc1, cex.main = cm, cex.lab = my.cex.lab, cex.axis = my.cex.axis)
abline(v = os, col = mycols[2], lwd = mylwd)
legend("topleft", legend = c("disease recognition null", "subject identification null"),
       text.col = c(rgb(0, 0, 1, rgbalpha), rgb(1, 0, 0, rgbalpha)), bty = "n", cex = cl)
mtext("(b)", side = 3, at = 0, line = myline)
##################
par(mar = c(5, 4, 2, 3))
##################
boxplot(datR, ylim = c(0, 1), main = "record-wise data split",
        ylab = "AUC", cex.main = cm, cex = 0.1, xaxt = "n",
        border = c(rep(rgb(0, 0, 1, 0.75), 30), rep("brown", 3)),
        las = 2, cex.lab = cl)
points(statsRWS[, "auc"], pch = 20, col = "brown", cex = 0.75)
axis(side = 4, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
axis(side = 1, at = seq(33), labels = colnames(datS), las = 2, cex.axis = 0.75)
mtext(side = 4, "disease recognition permutation p-value", line = 2, cex = 0.75)
segments(x0 = -1, x1 = 31, y0 = 0.5, y1 = 0.5, col = "red")
segments(x0 = 31, x1 = 40, y0 = 0.05, y1 = 0.05, lty = 2, col = "red")
abline(v = 31, lty = 1)
mtext("(c)", side = 3, at = 0, line = myline)
###################
boxplot(datS, ylim = c(0, 1), main = "subject-wise data split",
        ylab = "AUC", cex.main = cm, cex = 0.1, xaxt = "n",
        border = c(rep(rgb(0, 0, 1, 0.75), 30), rep("black", 3)),
        las = 2, cex.lab = cl)
points(statsSWS[, "auc"], pch = 20, col = "black", cex = 0.75)
axis(side = 4, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
axis(side = 1, at = seq(33), labels = colnames(datS), las = 2, cex.axis = 0.75)
mtext(side = 4, "disease recognition permutation p-value", line = 2, cex = 0.75)
segments(x0 = -1, x1 = 31, y0 = 0.5, y1 = 0.5, col = "red")
segments(x0 = 31, x1 = 40, y0 = 0.05, y1 = 0.05, lty = 2, col = "red")
abline(v = 31, lty = 1)
mtext("(d)", side = 3, at = 0, line = myline)



########################################################
########################################################
########################################################
########################################################
## mPower tapping example figure
########################################################

## output generated with the script:
## tapping_example.R 
## (used for panels a and b)
load(getFileLocation(synGet("syn11566306")))


## output generated with the script:
## mPower_tapping_data_example_100.R 
## (30 distinct train/test data splits 
## used for panels c and d)
load(getFileLocation(synGet("syn11566313")))

nas <- rep(NA, 1000)
pvalR <- c(statsRWS[, "permPvalDR"], rep(NA, 970))
datR <- data.frame(drRWS, nas, nas, pvalR)
colnames(datR) <- c(paste("split", seq(30), sep = " "), "", "", "p-value")
pvalS <- c(statsSWS[, "permPvalDR"], rep(NA, 970))
datS <- data.frame(drSWS, nas, nas, pvalS)
colnames(datS) <- c(paste("split", seq(30), sep = " "), "", "", "p-value")

or <- aucRWStap$aucObs
os <- aucSWStap$aucObs

xaxis <- seq(0, 1, length.out = 1000)
approxDensityR <- dnorm(xaxis, 0.5, sqrt(aucRWStap$approxVar["v"]))
approxDensityS <- dnorm(xaxis, 0.5, sqrt(aucSWStap$approxVar["v"]))

nc1 <- 20
nc2 <- 20
mylwd <- 1
cm <- 1
cl <- 1
my.cex.lab <- 1
my.cex.axis <- 1

mylwd <- 1.5
myline <- 0
mycols <- c("brown", "black")
mylwds <- c(1, 1)


mat <- matrix(c(1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 1, 
                2, 2, 2, 2, 2, 2,
                2, 2, 2, 2, 2, 2,
                3, 3, 3, 4, 4, 4,
                3, 3, 3, 4, 4, 4,
                3, 3, 3, 4, 4, 4), 7, 6, byrow = TRUE)

rgbalpha <- 0.75

yupper1 <- 60
yupper2 <- 65

layout(mat)
##############
par(mar = c(3, 3, 2, 1) + 0.1, mgp = c(2, 0.75, 0))
##############
hist(drRWStap, probability = TRUE, xlim = c(0, 1), col = rgb(0, 0, 1, rgbalpha), 
     main = "record-wise data split", xlab = "AUC", ylim = c(0, yupper1),
     nclass = nc1, cex.main = cm, cex.lab = my.cex.lab, cex.axis = my.cex.axis)
lines(xaxis, approxDensityR, col = "darkgrey", lwd = mylwd)
hist(icRWStap, probability = TRUE, xlim = c(0, 1), border = rgb(1, 0, 0, rgbalpha), 
     main = "record-wise data split", xlab = "AUC", add = TRUE,
     nclass = nc1, cex.main = cm, cex.lab = my.cex.lab, cex.axis = my.cex.axis)
abline(v = or, col = mycols[1], lwd = mylwd)
legend("topleft", legend = c("disease recognition null", "subject identification null"),
       text.col = c(rgb(0, 0, 1, rgbalpha), rgb(1, 0, 0, rgbalpha)), bty = "n", cex = cl)
mtext("(a)", side = 3, at = 0, line = myline)
##############
hist(drSWStap, probability = TRUE, xlim = c(0, 1), col = rgb(0, 0, 1, rgbalpha), 
     main = "subject-wise data split", xlab = "AUC", ylim = c(0, yupper1),
     nclass = nc1, cex.main = cm, cex.lab = my.cex.lab, cex.axis = my.cex.axis)
lines(xaxis, approxDensityS, col = "darkgrey", lwd = mylwd)
hist(icSWStap, probability = TRUE, xlim = c(0, 1), border = rgb(1, 0, 0, rgbalpha), 
     main = "subject-wise data split", xlab = "AUC", add = TRUE,
     nclass = nc1, cex.main = cm, cex.lab = my.cex.lab, cex.axis = my.cex.axis)
abline(v = os, col = mycols[2], lwd = mylwd)
legend("topleft", legend = c("disease recognition null", "subject identification null"),
       text.col = c(rgb(0, 0, 1, rgbalpha), rgb(1, 0, 0, rgbalpha)), bty = "n", cex = cl)
mtext("(b)", side = 3, at = 0, line = myline)
##################
par(mar = c(5, 4, 2, 3))
##################
boxplot(datR, ylim = c(0, 1), main = "record-wise data split",
        ylab = "AUC", cex.main = cm, cex = 0.1, xaxt = "n",
        border = c(rep(rgb(0, 0, 1, 0.75), 30), rep("brown", 3)),
        las = 2, cex.lab = cl)
points(statsRWS[, "auc"], pch = 20, col = "brown", cex = 0.75)
axis(side = 4, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
axis(side = 1, at = seq(33), labels = colnames(datS), las = 2, cex.axis = 0.75)
mtext(side = 4, "disease recognition permutation p-value", line = 2, cex = 0.75)
segments(x0 = -1, x1 = 31, y0 = 0.5, y1 = 0.5, col = "red")
segments(x0 = 31, x1 = 40, y0 = 0.05, y1 = 0.05, lty = 2, col = "red")
abline(v = 31, lty = 1)
mtext("(c)", side = 3, at = 0, line = myline)
###################
boxplot(datS, ylim = c(0, 1), main = "subject-wise data split",
        ylab = "AUC", cex.main = cm, cex = 0.1, xaxt = "n",
        border = c(rep(rgb(0, 0, 1, 0.75), 30), rep("black", 3)),
        las = 2, cex.lab = cl)
points(statsSWS[, "auc"], pch = 20, col = "black", cex = 0.75)
axis(side = 4, at = c(0, 0.2, 0.4, 0.6, 0.8, 1))
axis(side = 1, at = seq(33), labels = colnames(datS), las = 2, cex.axis = 0.75)
mtext(side = 4, "disease recognition permutation p-value", line = 2, cex = 0.75)
segments(x0 = -1, x1 = 31, y0 = 0.5, y1 = 0.5, col = "red")
segments(x0 = 31, x1 = 40, y0 = 0.05, y1 = 0.05, lty = 2, col = "red")
abline(v = 31, lty = 1)
mtext("(d)", side = 3, at = 0, line = myline)



########################################################
########################################################
########################################################
########################################################
## increasing number of participants voice figure
########################################################

## output generated with the script:
## mPower_voice_data_example_100.R 
load(getFileLocation(synGet("syn11566235")))
drRWS.100 <- drRWS
drSWS.100 <- drSWS
statsRWS.100 <- statsRWS
statsSWS.100 <- statsSWS

## output generated with the script:
## mPower_voice_data_example_50.R 
load(getFileLocation(synGet("syn11566321")))
drRWS.50 <- drRWS
drSWS.50 <- drSWS
statsRWS.50 <- statsRWS
statsSWS.50 <- statsSWS

## output generated with the script:
## mPower_voice_data_example_10.R 
load(getFileLocation(synGet("syn11566323")))
drRWS.10 <- drRWS
drSWS.10 <- drSWS
statsRWS.10 <- statsRWS
statsSWS.10 <- statsSWS

Pvals <- data.frame(statsRWS.100[, "permPvalDR"], statsRWS.50[, "permPvalDR"], statsRWS.10[, "permPvalDR"], 
                    statsSWS.100[, "permPvalDR"], statsSWS.50[, "permPvalDR"], statsSWS.10[, "permPvalDR"])
names(Pvals) <- c("22 subjects", "42 subjects", "240 subjects",
                  "22 subjects", "42 subjects", "240 subjects")


colnames(drRWS.100) <- paste("split", 1:30, sep = " ")
colnames(drSWS.100) <- paste("split", 1:30, sep = " ")

colnames(drRWS.50) <- paste("split", 1:30, sep = " ")
colnames(drSWS.50) <- paste("split", 1:30, sep = " ")

colnames(drRWS.10) <- paste("split", 1:30, sep = " ")
colnames(drSWS.10) <- paste("split", 1:30, sep = " ")

mat <- matrix(c(1, 1, 2, 2, 3, 3,
                1, 1, 2, 2, 3, 3,
                4, 4, 5, 5, 6, 6,
                4, 4, 5, 5, 6, 6,
                7, 7, 7, 7, 7, 7,
                7, 7, 7, 7, 7, 7), 6, 6, byrow = TRUE)

mycex <- 0.9
cm <- 1
cs <- 0.6


par(mar = c(3, 3, 1.5, 0.5), mgp = c(2, 0.75, 0))
layout(mat)
boxplot(drRWS.100, ylim = c(0.82, 1), las = 2, main = "record-wise split, 22 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01,
        yaxt = "n")
axis(side = 2, at = c(0.85, 0.90, 0.95, 1), labels = c("0.85", "", "0.95", "1.00"), las = 2, cex.axis = 1)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsRWS.100[, "auc"], pch = 20, col = "brown", cex = mycex)
mtext("(a)", side = 3, at = 2, line = -2)
####
boxplot(drRWS.50, ylim = c(0.82, 1), las = 2, main = "record-wise split, 42 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01,
        yaxt = "n")
axis(side = 2, at = c(0.85, 0.90, 0.95, 1), labels = c("0.85", "", "0.95", "1.00"), las = 2, cex.axis = 1)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsRWS.50[, "auc"], pch = 20, col = "brown", cex = mycex)
mtext("(b)", side = 3, at = 2, line = -2)
####
boxplot(drRWS.10, ylim = c(0.82, 1), las = 2, main = "record-wise split, 240 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01,
        yaxt = "n")
axis(side = 2, at = c(0.85, 0.90, 0.95, 1), labels = c("0.85", "", "0.95", "1.00"), las = 2, cex.axis = 1)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsRWS.10[, "auc"], pch = 20, col = "brown", cex = mycex)
mtext("(c)", side = 3, at = 2, line = -2)
####
boxplot(drSWS.100, ylim = c(0, 1), las = 2, main = "subject-wise split, 22 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsSWS.100[, "auc"], pch = 20, col = "black", cex = mycex)
mtext("(d)", side = 3, at = 2, line = -2)
####
boxplot(drSWS.50, ylim = c(0, 1), las = 2, main = "subject-wise split, 42 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsSWS.50[, "auc"], pch = 20, col = "black", cex = mycex)
mtext("(e)", side = 3, at = 2, line = -2)
####
boxplot(drSWS.10, ylim = c(0, 1), las = 2, main = "subject-wise split, 240 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsSWS.10[, "auc"], pch = 20, col = "black", cex = mycex)
mtext("(f)", side = 3, at = 2, line = -2)
####
boxplot(Pvals, ylim = c(0, 1), las = 1, ylab = "p-value",
        border = c("brown", "brown", "brown", "black", "black", "black"),
        main = "p-value distributions across the 30 random data splits", cex.main = cm)
abline(h = 0.05, col = "red")
legend("topleft", legend = c("record-wise data split", "subject-wise data split"), 
       text.col = c("brown", "black"), bty = "n")
mtext("(g)", side = 3, at = 6.5, line = -2)



#######################################################
#######################################################
#######################################################
#######################################################
## increasing number of participants tapping figure
#######################################################

## output generated with the script:
## mPower_tapping_data_example_100.R 
load(getFileLocation(synGet("syn11566313")))
drRWS.100 <- drRWS
drSWS.100 <- drSWS
statsRWS.100 <- statsRWS
statsSWS.100 <- statsSWS

## output generated with the script:
## mPower_tapping_data_example_50.R 
load(getFileLocation(synGet("syn11566668")))
drRWS.50 <- drRWS
drSWS.50 <- drSWS
statsRWS.50 <- statsRWS
statsSWS.50 <- statsSWS

## output generated with the script:
## mPower_tapping_data_example_10.R 
load(getFileLocation(synGet("syn11566718")))
drRWS.10 <- drRWS
drSWS.10 <- drSWS
statsRWS.10 <- statsRWS
statsSWS.10 <- statsSWS

Pvals <- data.frame(statsRWS.100[, "permPvalDR"], statsRWS.50[, "permPvalDR"], statsRWS.10[, "permPvalDR"], 
                    statsSWS.100[, "permPvalDR"], statsSWS.50[, "permPvalDR"], statsSWS.10[, "permPvalDR"])
names(Pvals) <- c("22 subjects", "48 subjects", "290 subjects",
                  "22 subjects", "48 subjects", "290 subjects")

colnames(drRWS.50) <- paste("split", 1:30, sep = " ")
colnames(drSWS.50) <- paste("split", 1:30, sep = " ")

colnames(drRWS.10) <- paste("split", 1:30, sep = " ")
colnames(drSWS.10) <- paste("split", 1:30, sep = " ")

mat <- matrix(c(1, 1, 2, 2, 3, 3,
                1, 1, 2, 2, 3, 3,
                4, 4, 5, 5, 6, 6,
                4, 4, 5, 5, 6, 6,
                7, 7, 7, 7, 7, 7,
                7, 7, 7, 7, 7, 7), 6, 6, byrow = TRUE)

mycex <- 0.9
cm <- 1
cs <- 0.6


par(mar = c(3, 3, 1.5, 0.5), mgp = c(2, 0.75, 0))
layout(mat)
boxplot(drRWS.100, ylim = c(0.77, 1), las = 2, main = "record-wise split, 22 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01,
        yaxt = "n")
axis(side = 2, at = c(0.8, 0.85, 0.90, 0.95, 1), labels = c("0.80", "0.85", "", "0.95", "1.00"), las = 2, cex.axis = 1)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsRWS.100[, "auc"], pch = 20, col = "brown", cex = mycex)
mtext("(a)", side = 3, at = 2, line = -2)
####
boxplot(drRWS.50, ylim = c(0.77, 1), las = 2, main = "record-wise split, 48 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01,
        yaxt = "n")
axis(side = 2, at = c(0.8, 0.85, 0.90, 0.95, 1), labels = c("0.80", "0.85", "", "0.95", "1.00"), las = 2, cex.axis = 1)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsRWS.50[, "auc"], pch = 20, col = "brown", cex = mycex)
mtext("(b)", side = 3, at = 2, line = -2)
####
boxplot(drRWS.10, ylim = c(0.77, 1), las = 2, main = "record-wise split, 290 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01,
        yaxt = "n")
axis(side = 2, at = c(0.8, 0.85, 0.90, 0.95, 1), labels = c("0.80", "0.85", "", "0.95", "1.00"), las = 2, cex.axis = 1)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsRWS.10[, "auc"], pch = 20, col = "brown", cex = mycex)
mtext("(c)", side = 3, at = 2, line = -2)
####
boxplot(drSWS.100, ylim = c(0, 1), las = 2, main = "subject-wise split, 22 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsSWS.100[, "auc"], pch = 20, col = "black", cex = mycex)
mtext("(d)", side = 3, at = 2, line = -2)
####
boxplot(drSWS.50, ylim = c(0, 1), las = 2, main = "subject-wise split, 48 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsSWS.50[, "auc"], pch = 20, col = "black", cex = mycex)
mtext("(e)", side = 3, at = 2, line = -2)
####
boxplot(drSWS.10, ylim = c(0, 1), las = 2, main = "subject-wise split, 290 subjects",
        border = rep(rgb(0, 0, 1, 0.75), 30), ylab = "AUC", cex.main = cm, xaxt = "n", cex = 0.01)
axis(side = 1, at = seq(30), labels = colnames(drRWS.100), las = 2, cex.axis = cs)
points(statsSWS.10[, "auc"], pch = 20, col = "black", cex = mycex)
mtext("(f)", side = 3, at = 2, line = -2)
####
boxplot(Pvals, ylim = c(0, 0.3), las = 1, ylab = "p-value",
        border = c("brown", "brown", "brown", "black", "black", "black"),
        main = "p-value distributions across the 30 random data splits", cex.main = cm)
abline(h = 0.05, col = "red")
legend("topleft", legend = c("record-wise data split", "subject-wise data split"), 
       text.col = c("brown", "black"), bty = "n")
mtext("(g)", side = 3, at = 6.5, line = -2)



