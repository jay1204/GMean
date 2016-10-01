####################################################################
# Implement G-means algorithm                                      #
# Hw3 of CSC 591(Foundation of Data Science)                       #
# Author: Zexi chen(Unity ID: zchen22)                             #
####################################################################

library(cluster)
library(nortest)  # use for the normal distribution test

#data <- data.frame(col1 = sample(1:10,100,replace=TRUE))
#data$col2 = sample(10:20,100, replace = TRUE)

# 
Gmeans <- function(X,alpha = 0.0001,k=1){
  
  # initial the clusters
  kmn <- kmeans(X,k)
  
  # get the clusters from kmeans model and the centroids
  clusters <- kmn$cluster
  totalCenters <- kmn$centers
  initialK = k
  
  # set the flag of whether spliting the model to be true
  split = TRUE
  
  while(split){
    
    # the total number of clusters is k
    k = length(unique(clusters))
    if(k!=initialK){
      labels = unique(clusters)
      centers <-matrix(0,k,ncol(X))
      # find the centroids for all the clusters
      for(i in 1:k){
        centers[i,]<-apply(X[clusters==labels[i],],2,mean)
      }
      
      # clustering use kmeans algorithm
      kmn = kmeans(X,centers)
      clusters = kmn$cluster
      totalCenters = kmn$centers
    }

    labels = unique(clusters)
    
    split = FALSE
    for(i in labels){
      
      sub_size = nrow(X[clusters==i,])
      sub_index = which(clusters==i)
      
      #put the constraint on the sample size for normal distribution
      if(sub_size>=30){
        # create two clusters for this subset
        km = kmeans(X[clusters==i,],2)
        two_clusters = km$cluster
        two_centers = km$centers
        # calculate the d-dimensional vector
        v = two_centers[1,]-two_centers[2,]
        dinominator = sqrt(sum(v^2))
        oneDim <- numeric(sub_size)
        # project the sub cluster onto this vector
        counter =1
        for(j in sub_index){
          oneDim[counter] <- sum(X[j,]*v)/dinominator
          counter = counter+1
        }
        miu = mean(oneDim)
        sd = sqrt(var(oneDim))
        
        # transform so it has mean 0 and variance 1
        oneDim = (oneDim-miu)/sd
        
        # run the AD-test
        test_result = ad.test(oneDim)
        
        # if it is in critical region, reject the null hypothesis and keep the two clusters
        p_value = test_result$p.value
        if(p_value<alpha){
          new_label = available_label(clusters)
          clusters[clusters==i][km$cluster==2] = new_label
          split = TRUE  
        }
      }
    }
  }
  
  result = list(clusters,totalCenters)
  
  return (result)

}

# this function is for users to find the available labels to use
available_label <- function(clusters){
  i=1
  select_pool = unique(clusters)
  while(1){
    if(any(select_pool==i)){
      i = i+1
    }else{
      return (i)
    }
  }
}


