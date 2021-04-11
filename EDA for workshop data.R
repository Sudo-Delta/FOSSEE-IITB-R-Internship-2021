library(readxl)
workshop <- read_excel("C:/Users/Siddhant/Desktop/R beginner workshop for data science (IITB-NMEICT).xls", col_names = TRUE)
View(workshop)

colnames(workshop)<-c("Name","Institute","Audience","Age","Background","Other Background",
                      
                      "Software other than R","Other software from list","other software not in list","Use time of stat software","Using R in institute","Purpose for R in institute",
                      "Other purpose for R in institute","Reasons to learn R","Other reasons to learn R",
                      
                      "Pre training duration","Spoken tutorial well made","Spoken tutorial need improvement",
                      "Spoken tutorial are unclear","Learned a lot via spoken tutorial",
                      
                      "How to use setwd", "How to use ggplot", "How to use ggplot aes","How to use dplyr",
                      "How to use pipe","How to use import","How to use rstudio","How to use rscript",
                      "How to use dataframe","How to use matrices","How to use manipulate","How to use hist and pie",
                      "How to use bar and scatter","How to use function of dplyr",
                      
                      "Spoken tutorial useful for practice problem","practice problem improved r programming","Practice problem improved R explaination","Practice Problem difficult",
                      
                      "Experience with Pedagogy of workshop","Experience of learning basic stat1","Experience improved by textbook companion","Experience of learning basic stat2","Experience with animation in R",
                      "Experience with creating R markdown","Experience with modelling cancer","Experience with cluster and classification learning","Motivation to use classification and clustering",
                      
                      "Quality of instruction","Quality of spoken tutorial","Interactin with FOSSEE team","Live session learning","Overall quality",
                      
                      "Pace","Helpful to know topic in advance",
                      
                      "Overall exposure", "Did not learn much","Recommend using R","Will recommend workshop for friend",
                      
                      "Happy with format","Workshop was crowded","Selective admission for less crowd","Apply fee so the crowd decrease",
                      
                      "Knowledge before workshop","knowledge after workshop",
                      
                      "Most liked aspect","Most disliked aspect",
                      
                      "Joined spoken tutorial forum","Posted Question on spoken tutorial forum","Answered question on spoken tutorial forum","Doubts already answered on spoken tutorial forum","Spoken tutorial forum usefull after workshop",
                      "Spoken tutorial forum benifit all","Spoken tutorual forum helpful in conducting workshop","Spoken tutorial provide personal recognation","Will recommend spoken tutorial forum","like most about spoken tutorial forum",
                      
                      "Self workshop requir help from FOSSEE","Sopken tutorial suppot is enough","No help required",
                      
                      "Interesting activity","Other Suggestion")


workshop[4]<-lapply(workshop[4], function(x){substr(x,1,2)})
workshop$Age<-as.numeric(workshop$Age)

library(ggplot2)
ggplot(workshop[c(3,4)])+
  geom_bar(mapping = aes(x=Age, fill = Audience))

workshop$`Helpful to know topic in advance`<-lapply(workshop$`Helpful to know topic in advance`, function(x){
  switch(
    x,
    "Extremely useful"= x<-"5",
    "Very useful"= x<-"4",
    "Useful"= x<-"3",
    "Somewhat useful"= x<-"2",
    x<-"1"
  )
})

workshop$Pace<-lapply(workshop$Pace, function(x){
  switch(
    x,
    "Acceptable"= x<-"5",
    "	Fast"= x<-"3",
    "Slow"= x<-"3",
    x<-"1"
  )
})

workshop$Pace<-as.numeric(workshop$Pace)
workshop$`Helpful to know topic in advance`<-as.numeric(workshop$`Helpful to know topic in advance`)
workshop[,21:34]<-lapply(workshop[,21:34], function(x) substring(x,1,1))
workshop[,21:34]<-lapply(workshop[,21:34], function(x) type.convert(x))

cleaned<-workshop[,c(17:34,38:52,53:64,71:75,77:79)]

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

library(tidyverse)

spoken<-cleaned %>%
  select(contains("spoken"))

KMO(spoken)
bart_spher(spoken)

howTo<-cleaned %>%
  select(contains("how to use"))

bart_spher(howTo)
KMO(howTo)

spoken.pca<-principal(spoken)
summary(spoken.pca)
plot(spoken.pca)


m1<-fa(spoken, nfactors = 5, rotate = "oblimin")
fa.diagram(m1)
plot(m1)

library(nFactors)
ev <- eigen(cor(cleaned))
ap <- parallel(subject=nrow(cleaned),var=ncol(cleaned),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


spoken.fa1<-factanal(spoken,factors = 3,rotation = "varimax")
spoken.fa1
##------------------------------------------------------------------------
#no of factor
library(nFactors)
ev <- eigen(cor(howTo))
ap <- parallel(subject=nrow(howTo),var=ncol(howTo),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# 3 factor model
m1<-fa(howTo, nfactors = 3, rotate = "oblimin")
fa.diagram(m1)
plot(m1)

#PCA
howTo.pca<-principal(howTo)
summary(howTo.pca)
plot(howTo.pca)

workshop.fa1<-factanal(cleaned,factors = 3,rotation = "varimax")
