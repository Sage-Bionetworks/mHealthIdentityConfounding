
require(synapser)
synLogin()


## output generated with the script:
## mPower_voice_data_example_100.R 
load(synGet("syn11566235")$path)
drRWS.voi.100 <- drRWS
drSWS.voi.100 <- drSWS
statsRWS.voi.100 <- statsRWS
statsSWS.voi.100 <- statsSWS

## output generated with the script:
## mPower_voice_data_example_50.R 
load(synGet("syn11566321")$path)
drRWS.voi.50 <- drRWS
drSWS.voi.50 <- drSWS
statsRWS.voi.50 <- statsRWS
statsSWS.voi.50 <- statsSWS

## output generated with the script:
## mPower_voice_data_example_10.R 
load(synGet("syn11566323")$path)
drRWS.voi.10 <- drRWS
drSWS.voi.10 <- drSWS
statsRWS.voi.10 <- statsRWS
statsSWS.voi.10 <- statsSWS

colnames(drRWS.voi.100) <- paste("split", 1:30, sep = " ")
colnames(drSWS.voi.100) <- paste("split", 1:30, sep = " ")

colnames(drRWS.voi.50) <- paste("split", 1:30, sep = " ")
colnames(drSWS.voi.50) <- paste("split", 1:30, sep = " ")

colnames(drRWS.voi.10) <- paste("split", 1:30, sep = " ")
colnames(drSWS.voi.10) <- paste("split", 1:30, sep = " ")

j.voi.100 <- which.min(abs(statsRWS.voi.100[, "auc"] - median(statsRWS.voi.100[, "auc"])))
j.voi.50 <- which.min(abs(statsRWS.voi.50[, "auc"] - median(statsRWS.voi.50[, "auc"])))
j.voi.10 <- which.min(abs(statsRWS.voi.10[, "auc"] - median(statsRWS.voi.10[, "auc"])))




## output generated with the script:
## mPower_tapping_data_example_100.R 
load(synGet("syn11566313")$path)
drRWS.tap.100 <- drRWS
drSWS.tap.100 <- drSWS
statsRWS.tap.100 <- statsRWS
statsSWS.tap.100 <- statsSWS

## output generated with the script:
## mPower_tapping_data_example_50.R 
load(synGet("syn11566668")$path)
drRWS.tap.50 <- drRWS
drSWS.tap.50 <- drSWS
statsRWS.tap.50 <- statsRWS
statsSWS.tap.50 <- statsSWS

## output generated with the script:
## mPower_tapping_data_example_10.R 
load(synGet("syn11566718")$path)
drRWS.tap.10 <- drRWS
drSWS.tap.10 <- drSWS
statsRWS.tap.10 <- statsRWS
statsSWS.tap.10 <- statsSWS

colnames(drRWS.tap.100) <- paste("split", 1:30, sep = " ")
colnames(drSWS.tap.100) <- paste("split", 1:30, sep = " ")

colnames(drRWS.tap.50) <- paste("split", 1:30, sep = " ")
colnames(drSWS.tap.50) <- paste("split", 1:30, sep = " ")

colnames(drRWS.tap.10) <- paste("split", 1:30, sep = " ")
colnames(drSWS.tap.10) <- paste("split", 1:30, sep = " ")

j.tap.100 <- which.min(abs(statsRWS.tap.100[, "auc"] - median(statsRWS.tap.100[, "auc"])))
j.tap.50 <- which.min(abs(statsRWS.tap.50[, "auc"] - median(statsRWS.tap.50[, "auc"])))
j.tap.10 <- which.min(abs(statsRWS.tap.10[, "auc"] - median(statsRWS.tap.10[, "auc"])))


## output generated with the script:
## run_uci_parkinsons_analysis.R
outputPath <- ""
load(paste(outputPath, "outputs_uci_parkinsons.RData", sep = ""))
drRWS.uci1 <- drRWS
statsRWS.uci1 <- statsRWS
j.uci1 <- which.min(abs(statsRWS.uci1[, "auc"] - median(statsRWS.uci1[, "auc"])))

## output generated with the script:
## run_uci_parkinsons_analysis_MSRD.R
load(paste(outputPath, "outputs_uci_parkinsons_MSRD.RData", sep = ""))
drRWS.uci2 <- drRWS
statsRWS.uci2 <- statsRWS
j.uci2 <- which.min(abs(statsRWS.uci2[, "auc"] - median(statsRWS.uci2[, "auc"])))

myylim <- c(0, 50)
mylwd <- 2
histcol <- rgb(0, 0, 1, 0.5)
abcol <- "brown"

nc <- 15
cl <- 1.5

figpath <- ""

#pdf(file = paste(figpath, "results_figure_main_text.pdf", sep = ""), width = 7, height = 5)
par(mfrow = c(4, 2), mar = c(3, 2.5, 1, 1), mgp = c(1.5, 0.75, 0))
hist(drRWS.uci1[, j.uci1], probability = TRUE, xlim = c(0.5, 1), ylim = myylim, xlab = "AUC", 
     col = histcol, main = "UCI Parkinsons", nclass = nc)
abline(v = statsRWS.uci1[j.uci1, "auc"], col = abcol, lwd = mylwd)
legend("topleft", legend = "(a)", cex = cl, bty = "n")
hist(drRWS.uci2[, j.uci2], probability = TRUE, xlim = c(0.5, 1), ylim = myylim, xlab = "AUC", 
     col = histcol, main = "UCI Parkinsons MSRD", nclass = nc)
abline(v = statsRWS.uci2[j.uci2, "auc"], col = abcol, lwd = mylwd)
legend("topleft", legend = "(b)", cex = cl, bty = "n")
####
hist(drRWS.voi.100[, j.voi.100], probability = TRUE, xlim = c(0.5, 1), ylim = myylim, xlab = "AUC", 
     col = histcol, main = "mPower, Voice, 22 subjects", nclass = nc)
abline(v = statsRWS.voi.100[j.voi.100, "auc"], col = abcol, lwd = mylwd)
legend("topleft", legend = "(c)", cex = cl, bty = "n")
hist(drRWS.tap.100[, j.tap.100], probability = TRUE, xlim = c(0.5, 1), ylim = myylim, xlab = "AUC", 
     col = histcol, main = "mPower, Tapping, 22 subjects", nclass = nc)
abline(v = statsRWS.tap.100[j.tap.100, "auc"], col = abcol, lwd = mylwd)
legend("topleft", legend = "(d)", cex = cl, bty = "n")
####
hist(drRWS.voi.50[, j.voi.50], probability = TRUE, xlim = c(0.5, 1), ylim = myylim, xlab = "AUC", 
     col = histcol, main = "mPower, Voice, 42 subjects", nclass = nc)
abline(v = statsRWS.voi.50[j.voi.50, "auc"], col = abcol, lwd = mylwd)
legend("topleft", legend = "(e)", cex = cl, bty = "n")
hist(drRWS.tap.50[, j.voi.50], probability = TRUE, xlim = c(0.5, 1), ylim = myylim, xlab = "AUC", 
     col = histcol, main = "mPower, Tapping, 48 subjects", nclass = nc)
abline(v = statsRWS.tap.50[j.tap.50, "auc"], col = abcol, lwd = mylwd)
legend("topleft", legend = "(f)", cex = cl, bty = "n")
####
hist(drRWS.voi.10[, j.voi.10], probability = TRUE, xlim = c(0.5, 1), ylim = myylim, xlab = "AUC", 
     col = histcol, main = "mPower, Voice, 240 subjects", nclass = nc)
abline(v = statsRWS.voi.10[j.voi.10, "auc"], col = abcol, lwd = mylwd)
legend("topleft", legend = "(g)", cex = cl, bty = "n")
hist(drRWS.tap.10[, j.voi.10], probability = TRUE, xlim = c(0.5, 1), ylim = myylim, xlab = "AUC", 
     col = histcol, main = "mPower, Tapping, 290 subjects", nclass = nc)
abline(v = statsRWS.tap.10[j.tap.10, "auc"], col = abcol, lwd = mylwd)
legend("topleft", legend = "(h)", cex = cl, bty = "n")
#dev.off()

