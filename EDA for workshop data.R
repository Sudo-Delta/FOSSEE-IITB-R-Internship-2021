library(readxl)
workshop <- read_excel("C:/Users/Siddhant/Desktop/R beginner workshop for data science (IITB-NMEICT).xls", col_names = TRUE)
View(workshop)

colnames(workshop)<-c("Name","Institute","Audience","Age","Background","Background.other",
                      
                      "NotR","NotR.name","NotR.other","Soft.time","AlreadyUseR","PurposeR",
                      "PurposeR.other","WhylearnR","WhylearnR.other",
                      
                      "Pre_Training","Spoken_Tut.wellMade","Spoken_Tut.improvement",
                      "Spoken_Tut.unclear","Spoken_Tut.selfLearn",
                      
                      "HowTo.setwd", "HowTo.ggplot", "HowTo.ggaes","HowTo.dplyr",
                      "HowTo.pipe","HowTo.import","HowTo.rstudio","HowTo.rscript",
                      "HowTo.dataframe","HowTo.matrices","HowTo.manipulate","HowTo.hist_pie",
                      "HowTo.bar_scatter","HowTo.function_dplyr",
                      
                      "PracticeP.stuseful","PracticeP.impR","PracticeP.impRexplain","PracticeP.difficult",
                      
                      "Exp.pedagogy","Exp.basicStat1","Exp.textComp","Exp.basicStat2","Exp.animation",
                      "Exp.markdownfile","Exp.modellingCancer","Exp.cluster_classExposure","Exp.cluster_class_motivation",
                      
                      "Asp.quality","Asp.spokentut","Asp.teamint","Asp.livesession","Asp.overallquality",
                      
                      "Pace","Pre_topic_decleration",
                      
                      "OverAll.exposure", "OverAll.notmuch","OverAll.willrecommend_R","OverAll.willrecommend_friend",
                      
                      "Format.good","Format.crowed","Fromat.lessCrowed","Format.feeLessCrowed",
                      
                      "Knowledge.before","knowledge.after",
                      
                      "Asp.like","Asp.dislike",
                      
                      "Spoken.joinforum","Spoken.postque","Spoken.ansque","Spoken.preans","Stforum.gooddiscussion",
                      "Spforum.ansforall","Spforum.useful","Spforum.perrecognition","Spforum.recommend","Stforum.like",
                      
                      "HelpReq.fossee","HelpReq.spoken","HelpReq.nohelp",
                      
                      "Interesting_Activity","Other_Suggestion")


for(i in 1:length(workshop$Age)){
  workshop[i,4]<-substr(workshop[i,4],1,2)
}
workshop[4]<-type.convert(workshop[4])
workshop[3]<-type.convert(workshop[3])

library(ggplot2)
ggplot(workshop[c(3,4)])+
  geom_bar(mapping = aes(x=Age, fill = Audience))

workshop[,21:34]<-lapply(workshop[,21:34], function(x) substring(x,1,1))
workshop[,21:34]<-lapply(workshop[,21:34], function(x) type.convert(x))


##subsetting

cleaned<-workshop[,c(17:34,38:52,55:64,71:75,77:79)]

## librarys

library(REdaS)
library(psych)

## Analysis

bart_spher(cleaned) #this should be significant (bartletts shphere test shows correlation)
KMO(cleaned) #rule of thumb 0.8 to 1 adequate. less than 0.6 not adequate

#The Kaiser-Meyer-Olkin Measure of Sampling Adequacy is a statistic that indicates the proportion 
#of variance in your variables that might be caused by underlying factors. High values (close to 1.0) generally 
#indicate that a factor analysis may be useful with your data. If the value is less than 0.50, 
#the results of the factor analysis probably won't be very useful.

#Bartlett's test of sphericity tests the hypothesis that your correlation matrix is an identity matrix, 
#which would indicate that your variables are unrelated and therefore unsuitable for structure detection. 
#Small values (less than 0.05) of the significance level indicate that a factor analysis may be useful with your data.

