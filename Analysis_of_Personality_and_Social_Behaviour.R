library(dplyr)
library(factoextra)
library(ggplot2)
library(ggpubr)
library(cluster)
dataset1=read.csv("E:\\17MDC46 - Predictive Analytics Lab\\Personality and Social Behavior Dataset For Analysis.csv")
print(dataset1)
summary(dataset1)

datasetm=read.csv("E:\\17MDC46 - Predictive Analytics Lab\\male.csv")
print(datasetm)
summary(datasetm)
datasetf=read.csv("E:\\17MDC46 - Predictive Analytics Lab\\female.csv")
print(datasetf)
summary(datasetf)


fviz_nbclust(dataset, kmeans, method="wss")
labs(subtitle="Elbow Method")

results11<-kmeans(dataset1,5)
results11
results11$size
results11$cluster

dataset1[,1:7] <-scale(dataset1[,1:7])

plot(dataset1[c("AGE","MIND")],col=results11$cluster)
points(results11$centers,pch=2,col="red")

plot(dataset1[c("AGE","ENERGY")],col=results11$cluster)
points(results11$centers,pch=2,col="red")

plot(dataset1[c("AGE","NATURE")],col=results11$cluster)
points(results11$centers,pch=2,col="red")

plot(dataset1[c("AGE","TACTICS")],col=results11$cluster)
points(results11$centers,pch=2,col="red")

plot(dataset1[c("AGE","IDENTITY")],col=results11$cluster)
points(results11$centers,pch=2,col="red")

clusplot(dataset1,results11$cluster)

sil <- silhouette(results11$cluster, dist(dataset1))
fviz_silhouette(sil)

#EUCLIDEAN
data.exc1<-dist(dataset1,method="euclidean")
round(as.matrix(data.exc1)[1:7,1:7],1)

# Use hcut() which compute hclust and cut the tree
hc.cut <- hcut(dataset1, k = 5, hc_method = "complete")
# Visualize dendrogram
fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE)

res.pca <- prcomp(dataset1, scale = TRUE)
fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Dimension reduction using PCA
res.pca <- prcomp(dataset1, scale = TRUE)
res.pca
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(results11$cluster)
# Add Species groups from the original data sett
ind.coord$Species <- df$Species
# Data inspection
head(ind.coord)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "circle", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)


