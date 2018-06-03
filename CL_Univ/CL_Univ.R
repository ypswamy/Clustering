C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/CL_Univ

# load package 'readxl' to load data from xlsx file
library(readxl)
input <- read_excel("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/CL_Univ/University_Clustering.xlsx")
summary(input)
head(input)
str(input)
dim(input)
length(input)
colnames(input)

mydata<-input[1:25,c(1,3:8)]
colnames(mydata)
# Excluding the university name column before normalizing the data
normalized_data<-scale(mydata[,2:7])
# Distance matrix
d<-dist(normalized_data,method="euclidean") 
fit<-hclust(d,method="complete")
?hclust
plot(fit) # Display Dendrogram
plot(fit,hang=-1)
groups<-cutree(fit,k=5)
?cutree
rect.hclust(fit,k=5,border="red")
?rect.hclust

membership<-as.matrix(groups)

final<-data.frame(mydata,membership)
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)

# Load the package for writing the data into xlsx file format
library(xlsx)
write.xlsx(final1,file="C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/CL_Univ/final1.xlsx")

