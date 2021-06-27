#setwd("D:/FOSSEE R Internship")

##-----------------------------
# Libraries

library(readr)
library(forecast)

##----------------------------

# Data loading
wpi_monthly_data <- read_csv("wpi_monthly_data.csv")
wpi_monthly_data<-as.data.frame(wpi_monthly_data)

# Note: Removing commodity id , weight and NA values beyond Dec 2020
wpi_monthly_data<-wpi_monthly_data[,-c(2,3,121:147)]

# Making a date vector
date<-seq(as.Date("2011/4/1"), by = "month", to = as.Date("2020/12/1"))


##--------------------------

# Plotting all the Data 
x<-t(wpi_monthly_data[138,-1]) # this is petroleum 

plot(date,x, type = "l", col = "red")

  # Same plot as above but different scale
  plot(date,x, type = "l", col = "red", ylim = c(50,730))

  for(i in 1:length(wpi_monthly_data)){
    lines(date, wpi_monthly_data[i,-1], type = "l")
  }

##-------------------------------
# Mineral oils ->[151:160]

minerals.ma5<-ma(t(wpi_monthly_data[151:160,-1]),5)
minerals.ma5[1:20,]

##-------------------------------
# Processed Meat ->[165:169]

processedMeat.ma5<-ma(t(wpi_monthly_data[165:169,-1]),5)
processedMeat.ma5[1:20,]

##--------------------------------
# Vegetable oils ->[179:189]


veg_oil.ma10<-ma(t(wpi_monthly_data[179:189,-1]),10)
veg_oil.ma10[1:20,]
tail(veg_oil.ma10)

# This is Average for all the oil indexes 
plot(date,t(wpi_monthly_data[178,-1]),type = "l", col ="red", main = "VEG OIL")

  # Same plot as above but different ylim
  plot(date,t(wpi_monthly_data[178,-1]),ylim = c(90,180) ,type = "l", col ="red", main = "VEG OIL")

# veg_avg is average of all the oils
veg_avg.ma10<-ma(t(wpi_monthly_data[178,-1]),10)
plot(date[-c(1,2,116,117)],veg_avg.ma10[-c(1,2,116,117),],ylim = c(90,180), type ="l", main="Moving average Oil AVG", col = "red")

v<-veg_oil.ma10[-c(1,2,116,117),] # removing rows with NA

for(i in 1:ncol(v)){
  lines(date[-c(1,2,116,117)],v[,i])
}

# Viewing each line individually 
plot(date[-c(1,2,116,117)],veg_avg.ma10[-c(1,2,116,117),],ylim = c(90,180), type ="l", main="Moving average Oil AVG", col = "red")

# Previous plot is recorded as p
p<-recordPlot()

lines(date[-c(1,2,116,117)],v[,1])# similar to veg_avg
lines(date[-c(1,2,116,117)],v[,2])# dissimilar 1
lines(date[-c(1,2,116,117)],v[,3])# similar to veg_avg
lines(date[-c(1,2,116,117)],v[,4])# similar to veg_avg

## Note: becomes too clustered to see
p
lines(date[-c(1,2,116,117)],v[,5])# second dissimilar
lines(date[-c(1,2,116,117)],v[,6])# third dissimilar

## Note: becomes too clustered to see
p
lines(date[-c(1,2,116,117)],v[,7])# similar to veg_avg
lines(date[-c(1,2,116,117)],v[,8])# similar to veg_avg
lines(date[-c(1,2,116,117)],v[,9])# similar to dissimilar 1
lines(date[-c(1,2,116,117)],v[,10])# very dissimilar

## Note: becomes too clustered to see
p
lines(date[-c(1,2,116,117)],v[,11])# similar to veg_avg


# # Done without stationary test
# acf(t(wpi_monthly_data[178,-1]))
# pacf(t(wpi_monthly_data[178,-1])) # PACF of veg oil

##-----------------------------


# Moving average complete time series

all.ma10<-ma(t(wpi_monthly_data[,-1]),10)
all.ma10[1:20,1:4]

# (X-Y)^2
x<-all.ma10[,1]

residual<-NULL
for (i in 1:ncol(all.ma10)) {
  y<-all.ma10[,i]
  residual<-rbind(residual,sum((x-y)^2, na.rm = T))
}

# Norm of the clusters
all<-as.data.frame(t(all.ma10[-c(1:5,113:117),]))

all.norm<-NULL
for(i in 1:nrow(all)){
  all.norm<-rbind(all.norm,norm(all[i,], type = "2"))
}

## Summary of norm
head(all.norm)
summary(all.norm)


# 
# all.normzlized<-list()
# for (i in 1:ncol(all.ma10)) {
#   all.normzlized[[i]]<-all.ma10[,i]/all.norm[1]
# }


# Dividing by the norm
all.ma10.normalized<-all.ma10
for (i in 1:ncol(all.ma10)) {
  all.ma10.normalized[,i]<-all.ma10[,i]/all.norm[1]
}

all.ma10.normalized[,1:6]

# K-means with n clusters
number.of.clusters<-20
set.seed(121)
ts.group<-kmeans(residual, number.of.clusters, iter.max = 20)
ts.group$cluster
ts.group$centers


# grouping the clusters
cluster<-list()
for(i in 1:number.of.clusters){
  cluster[[i]]<-which(ts.group$cluster==i)
}

# Row which got selected for cluster 1
# Similarly we can change the number to see other clusters
cluster[[1]]


#----------------------------

# Normalized (divided by norm)
for(k in 1:number.of.clusters){
  
  na.range<-c(1:5,113:117)
  plot(date[-na.range],all.ma10.normalized[-na.range,1], main = paste("Cluster", k),
       type ="l", col ="red",xlab = "Date",ylab ="Index",ylim=c(0,0.3))
  
  t<-cluster[[k]]
  for(i in 1:length(t)){
    lines(date[-na.range],all.ma10.normalized[-na.range,t[i]] )
  }
}


# Non Normalized visualization

# for(k in 1:c){
#   plot(date[-c(1:5,113:117)],all.ma10[-c(1:5,113:117),1], type ="l", col ="red", ylim=c(30,450),xlab = "Date",ylab ="Index")
#   t<-cluster[[k]]
#   for(i in 1:length(t)){
#     lines(date[-c(1:5,113:117)],all.ma10[-c(1:5,113:117),t[i]] )
#   }
# }
#-------------------------

# Items in the cluster
for(i in 1:number.of.clusters){
  cat("Cluster :",i,"\n")
  print(wpi_monthly_data[cluster[[i]],1])
}
##---------------------------




