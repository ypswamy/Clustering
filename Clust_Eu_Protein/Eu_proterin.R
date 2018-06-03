#Study case I: EUROPEAN PROTEIN CONSUMPTION

url = 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv'
food <- read.csv(url)
head(food)
summary(food)
str(food)
dim(food)
nrow(food)

#We start first, clustering on just Red and White meat (p=2) and k=3 clusters.

## to fix the random starting clusters
set.seed(123456789) 
grpMeat <- kmeans(food[,c("WhiteMeat","RedMeat")], centers=3, nstart=10)
grpMeat

## list of cluster assignments
o=order(grpMeat$cluster)
data.frame(food$Country[o],grpMeat$cluster[o])

#To see a graphical representation of the clustering solution we plot cluster assignments on Red and White meat on a scatter plot:
plot(food$Red, food$White, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat")
text(x=food$Red, y=food$White, labels=food$Country,col=grpMeat$cluster+1)


## same analysis, but now with clustering on all
## protein groups change the number of clusters to 7
set.seed(123456789)
grpProtein <- kmeans(food[,-1], centers=7, nstart=10)
o=order(grpProtein$cluster)
data.frame(food$Country[o],grpProtein$cluster[o])

library(cluster)
clusplot(food[,-1], grpProtein$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

#Alternatively we can implement a Hierarchical approach. We use the agnes function in the package cluster. 
#Argument diss=FALSE indicates that we use the dissimilarity matrix that is being calculated from raw data. 
#Argument metric=“euclidian” indicates that we use Euclidean distance. No standardization is used and the link 
#function is the “average” linkage.

foodagg=agnes(food,diss=FALSE,metric="euclidian")

## dendrogram
plot(foodagg, main='Dendrogram') 

# cut tree into 3 clusters
groups <- cutree(foodagg, k=4) 
rect.hclust(foodagg, k=4, border="red") 