
rm(list=ls(all=T))

library(fpc)
library(cluster)
library(ellipse)

source("Gmeans.R")
# read the file
df = read.csv("hw5-3d-data.csv",head = T, sep = ",")

# run the G-Mean model on the data
# my function is Gmeans <- function(X,alpha = 0.0001,k=1)
# you can set the alpha and k as you want. k is the initial number of clusters.
result = Gmeans(df)

plot(df[c(1,2)])

clusters = result[[1]]
centers = result[[2]]

labels = unique(clusters)

for(i in labels){
  x<-as.vector(df[clusters==i,][,1])
  y<-as.vector(df[clusters==i,][,2])
  dataEllipse(x,y,levels=0.9,col=i,add = TRUE)
}
