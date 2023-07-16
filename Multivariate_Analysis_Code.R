topsongs.df <- read.csv("C:/Users/Anita Dash/Downloads/RMV_Project/Top_100_moststreamed_data.csv")
str(topsongs.df)
# including all the libraries needed for the analysis
library(psych)
library(tidyverse)
library(cluster)
library(factoextra)
suppressPackageStartupMessages(library(dendextend))

#Parallel Analysis to determine the number of factors and components

fa.parallel(topsongs.df[-c(1,1:4)], fa="both", n.iter=150,
            main="Scree plots with parallel analysis")

#Parallel Analysis suggests that the number of factors = 2 and the number of components = 1

#Factor Analysis
fa_topsongs <- data.frame(topsongs.df[-c(1,1:4)])
fa_topsongs <- fa(fa_topsongs, nfactors=2, rotate="varimax", fm="pa")
#loadings of the observed variables represent the correlation between the variable and the factor.
fa_topsongs_loading <- (fa_topsongs$loadings)
fa_topsongs_loading
#Energy and Valance have higher correlation between PA1 and PA2 respectively
#adding the values of PA1 and PA2 to our main dataframe
fa_topsongs <- data.frame(fa_topsongs$scores)
topsongs.df <- cbind.data.frame(topsongs.df,fa_topsongs)

#Principal Component Analysis
pca_topsongs <- data.frame(topsongs.df[-c(1,1:4)])
pca_topsongs <- principal(pca_topsongs[-c(11,12)], nfactors=1, score=TRUE)
pca_topsongs_loading <- (pca_topsongs$loadings)
pca_topsongs_loading
#adding the values of PC1 to our main dataframe
pca_topsongs <- data.frame(pca_topsongs$scores)
topsongs.df <- cbind.data.frame(topsongs.df,pca_topsongs)

#k-means clustering 
df <- data.frame(topsongs.df[c("title","PA1", "PA2")])
df.norm <- sapply(df[,-1], scale)
row.names(df.norm) <- row.names(df)

#elbow method to determine number of clusters
wss <- sapply(1:10, function(k) {
  kmeans(df.norm, k, nstart = 50)$tot.withinss
})
plot(1:10, wss, type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total Within-Clusters Sum of Squares")
#The number of clusters = 4

km <- kmeans(df.norm, 4)
# Adding the clusters to the original dataframe
km_list <- data.frame(km$cluster)
topsongs.df <- cbind.data.frame(topsongs.df,km_list)
#plotting cluster:
fviz_cluster(km, data = df.norm,
             palette = c("red", "blue", "green", "orange"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
) 

#hierarchical clustering
df <- data.frame(topsongs.df[c("title","PA1", "PA2")])
df.norm <- sapply(df[,-1], scale)
row.names(df.norm) <- row.names(df)
d.norm <- dist(df.norm, method = "euclidean")
hc <- hclust(d.norm)

#plotting the clusters
avg_dend_obj <- as.dendrogram(hc)
avg_col_dend <- color_branches(avg_dend_obj, h = 4)
plot(avg_col_dend)
n_clusters=4
heir.cluster <- cutree(hc, k = n_clusters)# 4 is the number of clusters
# Adding the clusters to the original dataframe
heir.cluster_list <- data.frame(heir.cluster)
topsongs.df <- cbind.data.frame(topsongs.df,heir.cluster_list)

#plotting scatter plot energy vs valance (grouped by clusters formed by k-means)
#Code to select random points from the dataframe from each cluster to label them in the graph
random_points <- topsongs.df %>%
  mutate(cluster = km$cluster) %>%
  group_by(cluster) %>%
  sample_n(1)
random_points
topsongs.df %>% select(c("valance","energy","km.cluster"))%>%
  group_by(km.cluster) %>% 
  ggplot(aes(x = valance, y = energy, color = factor(km.cluster))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  labs(color = "km.cluster") + geom_text(data = random_points, aes(x = valance, y = energy, label = random_points$title))

#plotting scatter plot energy vs valance (grouped by clusters formed by hierarchical clustering)
topsongs.df %>% select(c("valance","energy","heir.cluster"))%>%
  group_by(heir.cluster) %>% 
  ggplot(aes(x = valance, y = energy, color = factor(heir.cluster))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  labs(color = "heir.cluster")


write.csv(topsongs.df, "C:/Users/Anita Dash/Downloads/RMV_Project/Top_songs_Analysis.csv", row.names=FALSE)