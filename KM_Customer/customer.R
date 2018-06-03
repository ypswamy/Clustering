http://www.learnbymarketing.com/tutorials/k-means-clustering-in-r-example/

data <-read.csv("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/KM_Customer/Wholesale customers data.csv",header=T)
summary(data)

#There’s obviously a big difference for the top customers in each category (e.g. Fresh #goes from a min of 3 to a max of 112,151).  Normalizing / scaling the data won’t #necessarily remove those outliers.  Doing a log transformation might help.   We could #also remove those customers completely.  From a business perspective, you don’t really #need a clustering algorithm to identify what your top customers are buying.  You usually #need clustering and segmentation for your middle 50%.

#With that being said, let’s try removing the top 5 customers from each category.  We’ll #use a custom function and create a new data set called data.rm.top

top.n.custs <- function (data,cols,n=5) 
{ #Requires some data frame and the top N to remove
  #Initialize a vector to hold customers being removed
  ## For every column in the data we passed to this function
  idx.to.remove <-integer(0) 
  for (c in cols)
    { 
  #Sort column "c" in descending order (bigger on top)
  col.order <-order(data[,c],decreasing=T) 
  #Order returns the sorted index (e.g. row 15, 3, 7, 1, ...) rather than the actual #values sorted.
  #Take the first n of the sorted column C to
  idx <-head(col.order, n) 
  #Combine and de-duplicate the row ids that need to be removed
  idx.to.remove <-union(idx.to.remove,idx) 
    }
##Return the indexes of customers to be removed
return(idx.to.remove) 
}

top.custs <-top.n.custs(data,cols=3:8,n=5)
#How Many Customers to be Removed?
length(top.custs)
#Examine the customers 
data[top.custs,] 
#Remove the Customers
data.rm.top<-data[-c(top.custs),] 

#Set the seed for reproducibility
set.seed(76964057) 
#Create 5 clusters, Remove columns 1 and 2
k <-kmeans(data.rm.top[,-c(1,2)], centers=5) 
#Display&nbsp;cluster centers
k$centers 
#Give a count of data points in each cluster
table(k$cluster) 


#A measurement that is more relative would be the withinss and betweenss.
#k$withinss would tell you the sum of the square of the distance from each data point to #the cluster center.  Lower is better.  Seeing a high withinss would indicate either #outliers are in your data or you need to create more clusters.
#k$betweenss tells you the sum of the squared distance between cluster centers.  Ideally #you want cluster centers far apart from each other.
#It’s important to try other values for K.  You can then compare withinss and #betweenss.  This will help you select the best K.   For example, with this data set, #what if you ran K from 2 through 20 and plotted the total within sum of squares?  You #should find an “elbow” point.  Wherever the graph bends and stops making gains in #withinss you call that your K.

#K from 2 to 20
rng<-2:20 
#Run the K Means algorithm 100 times
tries <-100 

avg.totw.ss <-integer(length(rng)) 
# For each value of the range variable
for(v in rng){ 
#Set up an empty vector to hold the 100 tries
 v.totw.ss <-integer(tries) 
 for(i in 1:tries){
 #Run kmeans
 k.temp <-kmeans(data.rm.top,centers=v) 
 #Store the total withinss
 v.totw.ss[i] <-k.temp$tot.withinss
 }
 #Average the 100 total withinss
 avg.totw.ss[v-1] <-mean(v.totw.ss) 
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
 ylab="Average Total Within Sum of Squares",
 xlab="Value of K")