library(tidyverse)
library(readxl)
library(dplyr)
library(cluster)

#importing the dataset
online_retail_II = read_excel("online_retail_II.xlsx")
retail = online_retail_II

#exploring the data
View(retail)
str(retail)

#checking for missing values
sapply(retail,function(x) sum(is.na(x)))

#dropping irrelevant column
retail = retail[-3]
View(retail)
str(retail)

#data pre-processing
retail$Invoice = as.factor(retail$Invoice)
retail$StockCode = as.factor(retail$StockCode)
retail$Country = as.factor(retail$Country)
retail$InvoiceDate = as.Date(retail$InvoiceDate, '%m/%d/%Y %H:%M')

retail = retail %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         Price = replace(Price, Price<=0, NA))

retail = retail %>% 
  drop_na()

retail = retail %>% 
  mutate(Total_revenue = Quantity*Price)

sapply(retail,function(x) sum(is.na(x)))

glimpse(retail)
View(retail)

#rfm analysis 
retail_RFM = retail %>% 
  group_by(`Customer ID`) %>% 
  summarise(recency = as.Date("2010-12-31") -  max(InvoiceDate),
            frequency = n_distinct(Invoice), monetary = sum(Total_revenue)/n_distinct(Invoice)) 

View(retail_RFM)  

#dropping irrelevant column
retail_RFM = retail_RFM[-1]

View(retail_RFM)
summary(retail_RFM)

#convert recency to numerirc
retail_RFM$recency = as.numeric(retail_RFM$recency)

sapply(retail_RFM,function(x) sum(is.na(x)))

#transforming the data
hist(retail_RFM$recency)
hist(retail_RFM$frequency)
hist(retail_RFM$monetary)

#transforming the data because it is skewed
retail_RFM$recency = log(retail_RFM$recency)
retail_RFM$frequency = log(retail_RFM$frequency)
retail_RFM$monetary = log(retail_RFM$monetary)

View(retail_RFM)

#normalizing the data to apply k-means
#retail_RFM <- as.data.frame(lapply(retail_RFM, scale))

sapply(retail_RFM,function(x) sum(is.na(x)))

View(retail_RFM)
summary(retail_RFM)

#Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(retail_RFM, i)$withinss)
plot(1:10, wcss,type = "b", main = paste('Clusters of clients'), xlab = "Number of clusters", ylab = "WCSS")


#Applying k-means to the data
set.seed(19)
retail_kmeans = kmeans(retail_RFM, 4, iter.max = 300, nstart = 10)

str(retail_kmeans)

# Visualize k-means clusters
fviz_cluster(retail_kmeans, data = retail_RFM, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())


#recency and frequency are the most important factors contributing to the model
#k-means with only recency and frequency values
test = retail_RFM[-3]
set.seed(19)
test_kmeans = kmeans(test, 3, iter.max = 300, nstart = 10)
View(test)

fviz_cluster(test_kmeans, data = test, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

#kmeans with pca
pca = prcomp(retail_RFM, center = F)
res.4 = kmeans(pca$x, 3)

str(pca)
str(res.4)
summary(pca)

#visualizing with kmeans with pca
clusplot(retail_RFM,
         res.4$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Clusters of clients"),
         xlab = "Money",
         ylab = "Recency",)

#Applying Hierachical clustering
#Using dendogram to find the optimal number of clusters
dendrogram = hclust(dist(retail_RFM, method = "euclidean"), method = "ward.D")
plot(dendrogram,
     main = paste("Dendrogram"),
     xlab = "Customers",
     ylab = "Euclidean distances")

#fitting hierachical clustering to the data
hc.cut = hcut(retail_RFM, k = 3, hc_method = "complete")
#hc.cut = hcut(test, k = 3, hc_method = "complete")

# Visualize k-means clusters
fviz_cluster(hc.cut, frame.type= "convex")


#Evaluating the cluster statistics
install.packages(c("factoextra", "fpc", "NbClust"))
library(factoextra)
library(fpc)
library(NbClust)

#Kmeans
# Silhouette coefficient of observations
library("cluster")
sil = silhouette(retail_kmeans$cluster, dist(retail_RFM))
head(sil[, 1:3], 10)

# Summary of silhouette analysis
si.sum = summary(sil)
# Average silhouette width of each cluster
si.sum$clus.avg.widths
# The total average (mean of all individual silhouette widths)
si.sum$avg.width

#fitting hierachical clustering
hc = hclust(dist(retail_RFM, method = "euclidean"), method = "ward.D")
y_hc = cutree(hc, 3)
y_hc

# Silhouette information for Hierachical clustering
hil = silhouette(y_hc, dist(retail_RFM))
head(sil[, 1:3], 10)
# Summary of silhouette analysis
hi.sum = summary(hil)
# Average silhouette width of each cluster
hi.sum$clus.avg.widths
# The total average (mean of all individual silhouette widths)
hi.sum$avg.width

#Evaluating usng Dunn index

# Compute pairwise-distance matrices
dd = dist(retail_RFM, method ="euclidean")
# Statistics for k-means clustering
km_stats = cluster.stats(dd,  retail_kmeans$cluster)
# (k-means) within clusters sum of squares
km_stats$within.cluster.ss
# (k-means) cluster average silhouette widths
km_stats$clus.avg.silwidths
km_stats

# Statistics for hierarchical clustering
hc_stats <- cluster.stats(dd,  hc.cut$cluster)
# (HCLUST) within clusters sum of squares
hc_stats$within.cluster.ss
# (HCLUST) cluster average silhouette widths
hc_stats$clus.avg.silwidths
hc_stats





