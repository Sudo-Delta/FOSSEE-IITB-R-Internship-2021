rm(list = ls())

set.seed(11)
# Matrix of input vector X
r1<-cbind(runif(500),runif(500, 4,6))
r2<-cbind(runif(500),runif(500, 0,2))
r3<-cbind(runif(500, 3,4), runif(500,4,6))
r4<-cbind(runif(500, 3,4), runif(500,0,2))

data<-matrix(rbind(r1,r2,r3,r4), ncol = 2)

weight<-matrix(cbind(runif(100,0,4),runif(100,0,6)), ncol = 2)
#weight<-scale(weight)

plot(data, xlim=c(0,4),col ="red")
points(weight,col ="blue", pch = 3)
##--------------------------------------------
#Neuron Grid
neuron.grid<-NULL
##The grid is of 10X10
dimension<-c(10,10) # c(rows, column)
##Grid making matrix inside list
for(i in 1:dimension[1]){
  v<-NULL

  for(j in 1:dimension[2]){
    t<-weight[(i*dimension[1]+j-dimension[1]),]
    t<-matrix(c(t,c(i,j)), ncol=2, byrow = T)
    
    if(j==1)
      v<-list(t)
    else
      v<-append(v,list(t))
  }
  
  if(i==1)
    neuron.grid<-list(v)
  else
    neuron.grid<-append(neuron.grid,list(v))
}
##-------------------------------------------
euclidean<-function(x,y){
  sum<-0
  for(i in 1:length(x)){
    sum<- sum + (x[i]-y[i])^2
  }
  return(sqrt(sum))
}
##---------------------------------------------
BMU<-function(s){
  min<-Inf
  for(i in 1:dimension[1]){
    for(j in 1:dimension[2]){
      distance = euclidean(data[s,],neuron.grid[[i]][[j]][1,])
      
      if(distance<min){
        min<-distance 
        winning_neuron<-c(i,j) #(row,column)
      }
    }
  }
  return(winning_neuron)
}
#-----------------------------------------------
initial_eta = 2
initial_sig = 1
total_itr = 5000
tconst = 1000
#-----------------------------------------------

for( time in 1:total_itr){
  
  eta = initial_eta*exp(-time/tconst)
  sig = initial_sig*exp(-time/tconst)
  
  if(eta<0.01)
    eta=0.01
  
  # Random input from continuous input space
  s<-floor(runif(1,1,dim(data)[1]))
  winning_neuron=BMU(s)
  
  for (i in 1:dimension[1]) {
    for(j in 1:dimension[2]){
      lateral_distance <- euclidean(neuron.grid[[winning_neuron[1]]][[winning_neuron[2]]][2,],neuron.grid[[i]][[j]][2,])
      
      if(lateral_distance<=sig){
        influence = exp(-(lateral_distance^2)/2*(sig^2))
        
        for(k in 1:length(weight[1,]))
          neuron.grid[[i]][[j]][1,k]<-neuron.grid[[i]][[j]][1,k]+eta * influence *(data[s,k]-neuron.grid[[i]][[j]][1,k])
      }
    }
  }
}

##--------------------------------------------------------
#result plot
plot(data, col="red", pch = 1)

for(i in 1:dimension[1]){
  for (j in 1:dimension[2]) {
    points(neuron.grid[[i]][[j]][1,], col="blue", pch=2)
  }
}


##-------------------------------------------------------
#Neuron map
# plot(0,0, xlim = c(0,12), ylim = c(0,12))
# 
# plotgrid<-function(){
#   for(i in 1:dimension[1]){
#     for (j in 1:dimension[2]) {
#       x<-neuron.grid[[i]][[j]][2,1]
#       y<-neuron.grid[[i]][[j]][2,2]
#       #points(x,y,col="blue", pch=21)
#     }
#   }
#   plot(x,y, pch = 21)
# }
# plotgrid()



