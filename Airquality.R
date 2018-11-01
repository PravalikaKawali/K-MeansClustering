# 1.The details about this dataset can be found at https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/airquality.html

data("airquality")
str(airquality)

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
#Normalize the dataset so that no particular attribute has more impact on clustering algorithm than others.
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
airquality<- normalize(airquality) # replace contents of dataset with normalized values

# 3. Apply k-means clustering algorithm

result<- kmeans(airquality[c(1,2,3,4)],3) # apply k-means algorithmusing first 4 attributes and with k=3(no. of required clusters)
result$size # gives no. of records in each cluster

result$centers
result$cluster

# 4. Visualize clustering results
par(mfrow=c(1,2), mar=c(5,4,2,2))
plot(airquality[,1:2], col=result$cluster) # Plot to see how Ozone and Solar.R data points have been distributed in clusters

plot(airquality[,3:4], col=result$cluster) # Plot to see how Wind and Temp data points have been distributed in clusters