source("util/apply_local_settings.R")
apply_local_settings()
dd<-localsettings$data.dir
rawdata <- read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment.txt"), header=T)
table(rawdata$runid==1,rawdata$Motivation=="reward")

#OK, let's get just one run to keep things simple
rawdata.rewardrun1<-rawdata[rawdata$runid==1 & rawdata$Motivation=="reward",]
#remove these subjects - I found evidence of bad data.
rawdata.rewardrun1<-rawdata.rewardrun1[!rawdata.rewardrun1$subid %in% c(115,216,254,332),]
#key columns are:
colnames(rawdata.rewardrun1)
#subid: nominal factor representing the subject ID for this trial
#image: nominal factor representing the particular image cue shown to subjects in this trail
#presentation_n:  ordinal factor, ranging from 1 to 13, indicating that this is the nth trial this this image has been presented.
#                 The reversal can be anything between the 6th and 9th image because the number of pre-reversal trials varies.
#presentation_n_in_segment: ordinal factor, ranges from 1 to 8, indicating that this is the nth trial 
#                 this image has been presented since either the start or since reversal. So the first pre-reversal trial
#                 is numbered 1 but the first post-reversal trial is also numbered 1
table(rawdata.rewardrun1$presentation_n_in_segment,rawdata.rewardrun1$presentation_n)
#correct: indicates whether subjects responded with a correct reponse or an incorrect response
#         in the model this was re-coded to be 1 or -1 indicating correct or incorrect respectively.
rawdata.rewardrun1$cor_res_aligned<-
  ((rawdata.rewardrun1$subid+1) %% 2)*(3-rawdata.rewardrun1$cor_res)+
  ((rawdata.rewardrun1$subid) %% 2)*(rawdata.rewardrun1$cor_res)
table(rawdata.rewardrun1$cor_res_aligned,rawdata.rewardrun1$response_key,rawdata.rewardrun1$correct)
View(rawdata.rewardrun1[rawdata.rewardrun1$cor_res_aligned==1 & rawdata.rewardrun1$response_key==1 & rawdata.rewardrun1$correct==FALSE,])
#reaction_time: reaction time in seconds. 
hist(rawdata$reaction_time)
#               In non-response trials, the reaction_time column is set to zero.
#               The current model ignores non-response trials altogether so this is not a problem for us, but be careful when using reaction time.
table(rawdata$reaction_time==0,rawdata$correct)





table(rawdata.rewardrun1$Condition)
table(rawdata.rewardrun1$presentation_n)
table(rawdata.rewardrun1$presentation_n)
