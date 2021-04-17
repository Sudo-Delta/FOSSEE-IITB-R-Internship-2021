# 1) Data Cleaning
library(readxl)

workshop <- read_excel("R beginner workshop for data science (IITB-NMEICT).xls", col_names = TRUE)
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
                      "Experience with creating R markdown","Experience with modelling cancer","Experience with cluster and classification learning","Experience and Motivation to use classification and clustering",
                      
                      "Quality of instruction","Quality of spoken tutorial","Interactin with FOSSEE team","Live session learning","Overall quality",
                      
                      "Pace","Helpful to know topic in advance",
                      
                      "Overall exposure", "Did not learn much","Recommend using R","Will recommend workshop for friend",
                      
                      "Happy with format of workshop","Workshop was crowded","Selective admission for less crowd in workshop","Apply fee so that crowd decreases in workshop",
                      
                      "Knowledge before workshop","knowledge after workshop",
                      
                      "Most liked aspect","Most dislike aspect",
                      
                      "Joined spoken tutorial forum","Posted Question on spoken tutorial forum","Answered question on spoken tutorial forum","Doubts already answered on spoken tutorial forum","Spoken tutorial forum usefull after workshop",
                      "Spoken tutorial forum benifit all","Spoken tutorual forum helpful in conducting workshop","Spoken tutorial provide personal recognation","Will recommend spoken tutorial forum","like most about spoken tutorial forum",
                      
                      "Self workshop requir help from FOSSEE","Sopken tutorial suppot is enough","No help required",
                      
                      "Interesting activity","Other Suggestion")

##------------------------------------------------------------------------

## 1.1) Encoding data to numbers 

#converting Q16 of survey to numeric encoding
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

#converting Q15 of survey to numeric encoding
workshop$Pace<-lapply(workshop$Pace, function(x){
  switch(
    x,
    "Acceptable"= x<-"5",
    "Fast"= x<-"3",
    "Slow"= x<-"3",
    x<-"1"
  )
})

##------------------------------------------------------------------------

## 1.2)  Type conversion from char to numeric

#removing text present with numeric data
workshop[4]<-lapply(workshop[4], function(x){substr(x,1,2)})
workshop[,21:34]<-lapply(workshop[,21:34], function(x) substring(x,1,1))

#converting data to numeric
workshop$Age<-as.numeric(workshop$Age) 
workshop$Pace<-as.numeric(workshop$Pace)
workshop$`Helpful to know topic in advance`<-as.numeric(workshop$`Helpful to know topic in advance`)
workshop[,21:34]<-lapply(workshop[,21:34], function(x) type.convert(x))

##------------------------------------------------------------------------

## 1.3) Age distribution of audience
#Age distribution of audience 
library(ggplot2)
ggplot(workshop[c(3,4)])+
  geom_bar(mapping = aes(x=Age, fill = Audience))

##------------------------------------------------------------------------

## 1.4) Subsetting Numeric Data
#subsetting only numeric data to new table (without age)
cleaned<-workshop[,c(17:34,38:52,53:64,71:75,77:79)]


##------------------------------------------------------------------------

# 2) Exploratory factor analysis


## 2.1) loading libraries 
## librarys for factor analysis
library(REdaS) # for Bartletts shpericity test
library(psych) # for factor analysis and KMO test
library(tidyverse) # to select data and create subset of questions
library(GPArotation) # for factor rotation
library(nFactors) # for parallel processing and scree plot

##---------------------><-------------------------##

## 2.2) Creating subset of data according to questions
#Spoken Tutorial questions
spoken<-cleaned %>%
  select(contains("spoken"))

#How to questions from workshop
howTo<-cleaned %>%
  select(contains("how to use"))

##---------------------><-------------------------##

## 2.3) EFA on Spoken Tutorial Related Questions

### 2.3.1) KMO and bartlett's test on spoken tutorial subset

# KMO and bartlett's test on data
KMO(spoken) #this should be significant (bartletts shphere test shows correlation)
bart_spher(spoken) #rule of thumb 0.8 to 1 adequate. less than 0.6 not adequate


### 2.3.2) PCA on spoken tutorial data
spoken.pca<-principal(spoken)
summary(spoken.pca)
plot(spoken.pca)


### 2.3.3) Scree plot to determine the number of factor
ev <- eigen(cor(spoken))
ap <- parallel(subject=nrow(spoken),var=ncol(spoken),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


### 2.3.4) Factor Analysis on spoken tutorial subset
m1<-fa(spoken, nfactors = 3, rotate = "oblimin")
fa.diagram(m1)
plot(m1)


### 2.3.5) text summary of the model 
spoken.fa1<-factanal(spoken,factors = 3,rotation = "varimax")
spoken.fa1

##------------------------------------------------------------------------

## 2.4) Factor analysis on howTo questions

### 2.4.1) Bartlett's test and KMO test
bart_spher(howTo)
KMO(howTo)

### 2.4.2) Number of factor with scree plot
ev <- eigen(cor(howTo))
ap <- parallel(subject=nrow(howTo),var=ncol(howTo),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

### 2.4.2) Factor model and diagram
m1<-fa(howTo, nfactors = 3, rotate = "oblimin")
fa.diagram(m1)
plot(m1)

### 2.4.4) PCA
howTo.pca<-principal(howTo)
summary(howTo.pca)
plot(howTo.pca)

##-----------------------------------------------------------------------

## 2.5) Factor analysis on experience of workshop question

### 2.5.1) Bartlett's test and KMO test 
bart_spher(Experience_of_Workshop2)
KMO(Experience_of_Workshop2)

### 2.5.2) Number of factor with scree plot
ev <- eigen(cor(Experience_of_Workshop2))
ap <- parallel(subject=nrow(Experience_of_Workshop2),var=ncol(Experience_of_Workshop2),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

### 2.5.3) Factor diagram
m1<-fa(Experience_of_Workshop2, nfactors = 4, rotate = "oblimin")
fa.diagram(m1)
plot(m1)

##--------------------------------------------------------------------------

## 2.6) Factor analysis on experience of workshop question second set
Experience_of_Workshop<-cleaned[,c(20:33)]

### 2.6.1) Bartlett's test and KMO test
bart_spher(Experience_of_Workshop)
KMO(Experience_of_Workshop)

### 2.6.2) Number of factor with scree plot
ev <- eigen(cor(Experience_of_Workshop))
ap <- parallel(subject=nrow(Experience_of_Workshop),var=ncol(Experience_of_Workshop),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

### 2.6.3) Factor diagram
m1<-fa(Experience_of_Workshop, nfactors = 2, rotate = "oblimin")
fa.diagram(m1)
plot(m1)

##-------------------------------------------------------

## 2.7) Factor analysis on experience of workshop question third set
Experience<-cleaned[,c(29:45)]

### 2.7.1) Bartlett's test and KMO test
bart_spher(Experience)
kmotest<-KMO(Experience)

### 2.7.2) Number of factor with scree plot
ev <- eigen(cor(Experience))
ap <- parallel(subject=nrow(Experience),var=ncol(Experience),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

### 2.7.3) Factor diagram
m1<-fa(Experience, nfactors = 4, rotate = "oblimin")
fa.diagram(m1)

m1<-fa(Experience, nfactors = 3, rotate = "varimax")
fa.diagram(m1)
