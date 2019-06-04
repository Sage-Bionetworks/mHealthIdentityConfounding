
## download the .rar file "Parkinson_Multiple_Sound_Recording.rar" from
## https://archive.ics.uci.edu/ml/machine-learning-databases/00301/
## extract and save the "train_data.txt" file


trainDat <- read.table("train_data.txt", sep = ",")
dim(trainDat)

table(trainDat[, 1])
table(trainDat[, 29])

names(trainDat) <- c("SubjectId",
                     "Jitter_local", "Jitter_local_absolute", "Jitter_rap", "Jitter_ppq5", "Jitter_ddp", 
                     "Shimmer_local", "Shimmer_local_dB", "Shimmer_apq3", "Shimmer_apq5", "Shimmer_apq11", "Shimmer_dda", 
                     "AC", "NTH" , "HTN",
                     "Median_pitch", "Mean_pitch", "Standard_deviation_pitch", "Minimum_pitch", "Maximum_pitch",
                     "Number_of_pulses", "Number_of_periods", "Mean_period", "Standard_deviation_of_period",
                     "Fraction_of_locally_unvoiced_frames", "Number_of_voice_breaks", "Degree_of_voice_breaks",
                     "UPDRS",
                     "class_information")

dat <- trainDat[, -28] ## remove UPDRS

dataPath <- ""

save(dat, file = paste(dataPath, "uci_parkinsons_MSRD.RData", sep = ""), compress = TRUE)
