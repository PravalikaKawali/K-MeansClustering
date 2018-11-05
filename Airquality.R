# 1.The details about this dataset can be found at https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/airquality.html

rm(list=ls(all=TRUE))   # remove all the objects in the list
library(ggplot2)        # importing packages
data("airquality")    # importing the inbuiltdata 
str(airquality)        #structure of a data frame(data types, values, variables)


# Takes only first 6 observations out of 153.
head(airquality)

# 2. Preprocess the dataset

col1<- mapply(anyNA,airquality) # apply function anyNA() on all columns of airquality dataset
col1

# The output shows that only Ozone and Solar.R attributes have NA i.e. some missing value.

for (i in 1:nrow(airquality)){
  if(is.na(airquality[i,"Ozone"])){
    airquality[i,"Ozone"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Ozone"],na.rm = TRUE)
  }
  # Impute monthly mean in Solar.R
  if(is.na(airquality[i,"Solar.R"])){
    airquality[i,"Solar.R"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Solar.R"],na.rm = TRUE)
  }
  
}
#Normalize the dataset so that no particular attribute has more impact on clustering algorithm than others. To get into similar ranges.
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# calculating normalized value for ozone=8
(8-min(airquality))/(max(airquality)-min(airquality)) 
airquality2 <- normalize(airquality)  # assigning the normalized data set to airquality2 variable

# 3. Apply k-means clustering algorithm

set.seed(4) # to fix the random centroids(we can put any number in seed)

# apply k-means algorithm using first 4 attributes and with k=3(no. of required clusters)
result<- kmeans(airquality2[,c(1,2,3,4)],3) 
result$size # gives no. of records in each cluster

# final centroids
result$centers
# cluster assign to each row
result$cluster

# 4. Visualize clustering results

# Creating a new attribute 'cluster' for visualization

airquality$cluster = as.factor(result$cluster)  # for legend 

# Scatter plot between ozone and solar by each cluster 
# Plot to see how ozone and solar data points have been distributed in clusters
ggplot(airquality, aes(x=Ozone, y=Solar.R, color=cluster))+geom_point()+theme_bw()


# Plot to see how Wind and Temp data points have been distributed in clusters
ggplot(airquality, aes(x=Temp, y=Solar.R, color=cluster))+geom_point()+theme_bw()

# Export data files to local system
write.csv(airquality, "C:/Users/s530936/Downloads/DV/Airquality_Data.csv")
write.csv(airquality2, "C:/Users/s530936/Downloads/DV/kmeansppt/Airquality_NormalizeData.csv")
getwd()




