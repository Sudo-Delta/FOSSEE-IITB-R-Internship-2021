library(readxl)
workshop <- read_excel("C:/Users/Siddhant/Desktop/R beginner workshop for data science (IITB-NMEICT).xls", col_names = TRUE)
View(workshop)

colnames(workshop)<-c("Name","Institute","Audience","Age","Background","Background.other",
                      "NotR","NotR.name","NotR.other","Soft.time","WillUseR","PurposeR",
                      "PurposeR.other","PurposeR.other","WhyR",
                      "Pre_Training","Spoken_Tut.wellMade","Spoken_Tut.improvement",
                      "Spoken_Tut.unclear","Spoken_Tut.selfLearn")


# # bad ideas

# workshop$Age<-str_remove(workshop$Age,"years")
# workshop$Age<-str_remove(workshop$Age, "yrs")
# workshop["Age"]<-type.convert(workshop["Age"])
# age<-type.convert(workshop["Age"])
# audience<-as.factor(unlist(workshop[-1,3]))
#library(tidyverse)
# workshop %>%
#   select(as.numeric(Age))

for(i in 1:length(workshop$Age)){
  workshop[i,4]<-substr(workshop[i,4],1,2)
}
workshop[4]<-type.convert(workshop[4])
workshop[3]<-type.convert(workshop[3])

ggplot(workshop[c(3,4)])+
  geom_bar(mapping = aes(x=Age, fill = Audience))

