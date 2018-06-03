#Social Network Clustering Analysis



teens <- read.csv("//svrin000egl01.asia.corp.anz.com/swamyy1$/Desktop/ML/CASE_STUDY/CASE_STUDY/CLUSTERING/SocialNetworking/snsdata.csv")
head(teens,3)
dim(teens)
summary(teems)
str(teens)
colnames(teens)

#As we had expected, the data include 30,000 teenagers with four variables indicating personal 
#characteristics and 36 words indicating interests. 
#Note that there are some NA’s in the variable gender.

summary(teens$age)

#We will skip all the data with missing values:

teens = na.omit(teens)
dim(teens)

#We’ll start our cluster analysis by considering only the 36 features that represent the 
#number of times various interests appeared on the SNS profiles of teens. For convenience, 
#let’s make a data frame containing only these features:

interests <- teens[5:40]

#To apply z-score standardization to the interests data frame, we can use the scale() 
#function with lapply(), as follows:

interests_z <- as.data.frame(lapply(interests, scale))

#To divide teens into five clusters, we can use the following command:

teen_clusters <- kmeans(interests_z, 5)

teen_clusters$size

o=order(teen_clusters$cluster)
data.frame(teens$age[o],teen_clusters$cluster[o])

teen_clusters$centers

#The cluster characterization can be obtained with pie charts:

par(mfrow=c(2,2))
pie(colSums(interests[teen_clusters$cluster==1,]),cex=0.5)
pie(colSums(interests[teen_clusters$cluster==2,]),cex=0.5)
pie(colSums(interests[teen_clusters$cluster==3,]),cex=0.5)
pie(colSums(interests[teen_clusters$cluster==4,]),cex=0.5)

