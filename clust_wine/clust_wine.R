install.packages("tidyverse")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("GGally")
install.packages("magrittr")
install.packages("ggplot2")
library(magrittr)
library(tidyverse)
library(corrplot)
library(gridExtra)
library(GGally)
library(ggplot2)

wines <- read.csv("//svrin000egl01.asia.corp.anz.com/swamyy1$/Desktop/ML/CASE_STUDY/CASE_STUDY/CLUSTERING/wine/Wine.csv")
wines
names(wines)
dim(wines)
nrow(wines)
str(wines)
summary(wines)
head(wines)
pairs(wines)
cor(wines)
corrplot(wines)

#We don’t need the Customer_Segment column. As we have said before, k-means is an unsupervised machine learning algorithm and works with unlabeled data.
# Remove the Type column
wines <- wines[,-14]

#We can use the corrplot() function to create a graphical display of a correlation matrix.
# Correlation matrix 
corrplot(cor(wines), type="upper", method="ellipse", tl.cex=0.9)

#There is a strong linear correlation between Total_Phenols and Flavanoids. We can model the relationship between these two variables by fitting a linear equation.
# Relationship between Phenols and Flavanoids
ggplot(wines, aes(x=Total_Phenols, y=Flavanoids)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)


#We have to normalize the variables to express them in the same range of values. In other words, normalization means adjusting values measured on different scales to a common scale.
#Normalization
winesNorm <- as.data.frame(scale(wines))

# Original data
p1 <- ggplot(wines, aes(x=Alcohol, y=Malic_Acid)) +
  geom_point() +
  labs(title="Original data")

plot(p1)
  
# Normalized data 
p2 <- ggplot(winesNorm, aes(x=Alcohol, y=Malic_Acid)) +
  geom_point() +
  labs(title="Normalized data")
plot(p2)
  
# Subplot
grid.arrange(p1, p2, ncol=2)
#The points in the normalized data are the same as the original one. The only thing that changes is the scale of the axis.

# Execution of k-means with k=2
set.seed(1234)
wines_k2 <- kmeans(winesNorm, centers=2)

# Cluster to which each point is allocated
wines_k2$cluster
# Cluster centers
wines_k2$centers
# Cluster size
wines_k2$size
# Between-cluster sum of squares
wines_k2$betweenss
# Within-cluster sum of squares
wines_k2$withinss
# Total within-cluster sum of squares 
wines_k2$tot.withinss
# Total sum of squares
wines_k2$totss

#6 How many clusters?

bss <- numeric()
wss <- numeric()

# Run the algorithm for different values of k 
set.seed(1234)
for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(winesNorm, centers=i)$betweenss
  wss[i] <- kmeans(winesNorm, centers=i)$tot.withinss
}

# Between-cluster sum of squares vs Choice of k
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Total within-cluster sum of squares vs Choice of k
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Subplot
grid.arrange(p3, p4, ncol=2)
plot(p3)
plot(p4)

#it is clear that 3 is the appropriate value for k.

# Execution of k-means with k=3
set.seed(1234)
wines_k2 <- kmeans(winesNorm, centers=3)

# Mean values of each cluster
aggregate(wines, by=list(wines_k2$cluster), mean)

