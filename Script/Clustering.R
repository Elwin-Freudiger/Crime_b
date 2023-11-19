library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(cluster)
library(plotly)

#Load the dataset
Dep_data <- read_csv(here::here("data_end/Everything_by_dep.csv"))

Data_to_cluster <- Dep_data[c(3, 6, 7, 8, 10, 11)]
#distance matrix
Dep_dist <- dist(Data_to_cluster, method= "euclidean", diag = FALSE, upper = FALSE)
#build dendrogram
Dep_completelink <- hclust(Dep_dist, method = 'average')


#look who belong to what
Cluster_numbers <- cutree(Dep_completelink, k= 3)

data_add_cluster <- cbind(Dep_data, Cluster_numbers)

#plot the data with unemployment and crime, add colors for clusters
plot_cluster <- ggplot(data_add_cluster, aes(x = Crime_rate_1k, y = Density_2019, label= Dep_name, color = factor(Cluster_numbers))) + 
  geom_point() +
  geom_text(check_overlap = TRUE)

#put it on a map of France
border <- st_read(here::here("data_end/Departement_geoson_carte.geojson"))
colnames(border) <- c("Dep_number", "Dep_name", "geometry")
data_cluster_geom <- left_join(border, data_add_cluster, join_by("Dep_number"))

cluster_map <- ggplot() +
  geom_sf(data = data_cluster_geom, aes(fill = as.factor(Cluster_numbers))) + 
  theme(axis.text.x  = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Map of hierarchical clustering")


#Run K-means

withins <- vector()

#scree plot for k-means
for (i in 1:15) {
  kclust <- kmeans(scale(Data_to_cluster), centers = i, nstart = 25)
  withins[i] <- kclust$tot.withinss
}

#plot the scree plot
scree_plot <- ggplot(mapping = aes(x = 1:15, y = withins)) + 
  geom_line(color = "blue") + 
  geom_point(color = "blue") +
  geom_vline(xintercept = 6, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Optimal number of clusters", x= "Number of clusters", y = "Total Within Sum Squares")

#run the kmeans with 5 clusters
kcluster <- kmeans(Data_to_cluster, centers = 6, nstart = 25)
clust_number <- kcluster$cluster

#add what each observation belong to what cluster 
data_with_clust <- cbind(Dep_data, clust_number)

cluster_plot <- ggplot(data_with_clust, aes(x = Lepen_score, y = Unemp_2019, color = factor(clust_number))) + 
  geom_point() + 
  geom_text(label= data_with_clust$Dep_name, nudge_x = -0.5, nudge_y = 0.25, check_overlap = TRUE) +
  labs(title = "Visualization of cluster by mapping Unemployement and Crime rate", x = "Score Lepen", y = "Unemployment")


#map our new results

data_cluster_geom_means <- left_join(border, data_with_clust, join_by("Dep_number"))

cluster_map_means <- ggplot() +
  geom_sf(data = data_cluster_geom, aes(fill = as.factor(clust_number))) + 
  theme(axis.text.x  = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Map of K-means clustering")