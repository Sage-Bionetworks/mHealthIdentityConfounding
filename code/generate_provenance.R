
library(synapseClient)
synapseLogin()


############################################
## outputs
############################################

## output_null_simulations.RData
f0 <- synGet("syn11565429", downloadFile = FALSE)
f0 <- synStore(f0,
               used = c("https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
               executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/run_simulation_study.R?token=ABoVeaM64I81DdSRlGXola9s18rTi0jxks5aQ-tCwA%3D%3D")


## output_synthetic_data_example_1.RData
f1 <- synGet("syn11564867", downloadFile = FALSE)
f1 <- synStore(f1,
               used = c("https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/simulated_example_1.R?token=ABoVeX5U0BWd53NL_LpWJKPZxifK_SiTks5aQv5HwA%3D%3D", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
               executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/run_synthetic_data_examples.R?token=ABoVeUXUvEi3IM8LyxqgurDMQU_TD1z1ks5aQv6nwA%3D%3D")


## output_synthetic_data_example_2.RData
f2 <- synGet("syn11564868", downloadFile = FALSE)
f2 <- synStore(f2,
               used = c("https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/simulated_example_2.R?token=ABoVebmNrpjOJ3WljO1is9RLiTYdr7Zpks5aQwArwA%3D%3D", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
               executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/run_synthetic_data_examples.R?token=ABoVeUXUvEi3IM8LyxqgurDMQU_TD1z1ks5aQv6nwA%3D%3D")


## output_synthetic_data_example_3.RData
f3 <- synGet("syn11564869", downloadFile = FALSE)
f3 <- synStore(f3,
               used = c("https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/simulated_example_3.R?token=ABoVeYBk4k7Krz9Jvj5_wPsc2S-BUQrJks5aQwFNwA%3D%3D", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
               executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/run_synthetic_data_examples.R?token=ABoVeUXUvEi3IM8LyxqgurDMQU_TD1z1ks5aQv6nwA%3D%3D")


## output_synthetic_data_example_4.RData
f4 <- synGet("syn11564870", downloadFile = FALSE)
f4 <- synStore(f4,
               used = c("https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/simulated_example_4.R?token=ABoVefCHx0E94KG_FFZy4LS_gdgdq7kTks5aQwJ9wA%3D%3D", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
               executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/run_synthetic_data_examples.R?token=ABoVeUXUvEi3IM8LyxqgurDMQU_TD1z1ks5aQv6nwA%3D%3D")


## output_synthetic_data_example_5.RData
f5 <- synGet("syn11564871", downloadFile = FALSE)
f5 <- synStore(f5,
               used = c("https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/simulated_example_5.R?token=ABoVeb2ozqzuo_3ZWbpuZNZWyFv0djyeks5aQwNQwA%3D%3D", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
               executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/run_synthetic_data_examples.R?token=ABoVeUXUvEi3IM8LyxqgurDMQU_TD1z1ks5aQv6nwA%3D%3D")


## output_synthetic_data_example_6.RData
f6 <- synGet("syn11564872", downloadFile = FALSE)
f6 <- synStore(f6,
               used = c("https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/simulated_example_6.R?token=ABoVeYks6wCtE4LwbVldnc7EWz1hFqQ4ks5aQwQ3wA%3D%3D", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
               executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/run_synthetic_data_examples.R?token=ABoVeUXUvEi3IM8LyxqgurDMQU_TD1z1ks5aQv6nwA%3D%3D")


## voice_example_22subj_100recs.RData
f7 <- synGet("syn11565454", downloadFile = FALSE)
f7 <- synStore(f7,
               used = c("syn10903828", "syn10903903", "syn10903864", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
               executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/voice_example.R?token=ABoVecFMy8bxq2IMCBFEWEEjeja0lLqLks5aQwXDwA%3D%3D")


## output_mPower_voice_data_100.RData
f8 <- synGet("syn11566235", downloadFile = FALSE)
f8 <- synStore(f8,
               used = c("syn10903828", "syn10903903", "syn10903864", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
               executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/mPower_voice_data_example_100.R?token=ABoVeXZJg3nftxmzluBmlC8kVRWVLQBZks5aQwn-wA%3D%3D")


## tapping_example_22subj_100recs.RData
f9 <- synGet("syn11566306", downloadFile = FALSE)
f9 <- synStore(f9,
               used = c("syn10903849", "syn10903906", "syn10903865", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
               executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/tapping_example.R?token=ABoVeYVltkofSQLGncxHiTe-b1LIkgL8ks5aQ8sxwA%3D%3D")


## output_mPower_tap_data_100.RData
f10 <- synGet("syn11566313", downloadFile = FALSE)
f10 <- synStore(f10,
                used = c("syn10903849", "syn10903906", "syn10903865", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
                executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/mPower_tapping_data_example_100.R?token=ABoVebtB1joEr84jQ5Ltc2ZH7fCJRrhmks5aQ8wVwA%3D%3D")


## output_mPower_voice_data_50.RData
f11 <- synGet("syn11566321", downloadFile = FALSE)
f11 <- synStore(f11,
                used = c("syn10927961", "syn10927962", "syn10903864", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
                executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/mPower_voice_data_example_50.R?token=ABoVeZ47UDkpNNg6JBj5AL1G10BJkUgsks5aQ87rwA%3D%3D")


## output_mPower_voice_data_10.RData
f12 <- synGet("syn11566323", downloadFile = FALSE)
f12 <- synStore(f12,
                used = c("syn11059980", "syn11059982", "syn10903864", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
                executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/mPower_voice_data_example_10.R?token=ABoVeda8ToExBCzRSC06IU8gnNs57zjWks5aQ8-YwA%3D%3D")


## output_mPower_tap_data_50.RData
f13 <- synGet("syn11566668", downloadFile = FALSE)
f13 <- synStore(f13,
                used = c("syn10933730", "syn10933736", "syn10903865", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
                executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/mPower_tapping_data_example_50.R?token=ABoVeQ_mTRctfCKslcPexzS9erJbegHCks5aQ9QpwA%3D%3D")


## output_mPower_tap_data_10_aws.RData
f14 <- synGet("syn11566718", downloadFile = FALSE)
f14 <- synStore(f14,
                used = c("syn11059974", "syn11059977", "syn10903865", "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/utility_functions_disease_recognition_and_identity_confounding.R?token=AAI6zwVqpa2otvq-BDmrqCqhmuTHvyPMks5aQr6HwA%3D%3D"),
                executed = "https://raw.githubusercontent.com/Sage-Bionetworks/mHealthIdentityConfounding/master/code/mPower_tapping_data_example_10.R?token=ABoVee8grRHErsJJ2QeZ1QoXWghJUYF_ks5aQ9cjwA%3D%3D")



############################################
## data
############################################

## tapping_bal_mPower_data_22subjects_100records.RData
f15 <- synGet("syn10903849", downloadFile = FALSE)
f15 <- synStore(f15,
                used = c("syn5511439", "syn5511429"),
                executed = "https://github.com/Sage-Bionetworks/mpowertools")


## tapping_bal_mPower_data_290subjects_10records.RData
f16 <- synGet("syn11059974", downloadFile = FALSE)
f16 <- synStore(f16,
                used = c("syn5511439", "syn5511429"),
                executed = "https://github.com/Sage-Bionetworks/mpowertools")


## tapping_bal_mPower_data_48subjects_50records.RData
f17 <- synGet("syn10933730", downloadFile = FALSE)
f17 <- synStore(f17,
                used = c("syn5511439", "syn5511429"),
                executed = "https://github.com/Sage-Bionetworks/mpowertools")


## voice_bal_mPower_data_22subjects_100records.RData
f18 <- synGet("syn10903828", downloadFile = FALSE)
f18 <- synStore(f18,
                used = c("syn5511444", "syn5511429"),
                executed = "https://github.com/Sage-Bionetworks/mpowertools")


## voice_bal_mPower_data_240subjects_10records.RData
f19 <- synGet("syn11059980", downloadFile = FALSE)
f19 <- synStore(f19,
                used = c("syn5511444", "syn5511429"),
                executed = "https://github.com/Sage-Bionetworks/mpowertools")


## voice_bal_mPower_data_42subjects_50records.RData
f20 <- synGet("syn10927961", downloadFile = FALSE)
f20 <- synStore(f20,
                used = c("syn5511444", "syn5511429"),
                executed = "https://github.com/Sage-Bionetworks/mpowertools")





