library(synapseClient)
synapseLogin()


#Tapping Provenance

f1 <- synGet("syn11566306", downloadFile=F)
f1 <- synStore(f1,
         used=c('syn10903849', 'syn10903906', 'syn10903865', 'https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D'),
         executed ='https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/tapping_example.R?token=AAI6zxihYwx85RaK9SjEbouV0idempNKks5aQr5swA%3D%3D')
         

