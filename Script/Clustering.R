###########################
###########################
#This script is our clustering file
###########################
###########################

#load required packages
source(here::here("Script/setup.R"))

#Load the dataset
Dep_data <- read_csv(here::here("data_end/Everything_by_dep.csv"))
#select onyl columns to cluster
Data_to_cluster <- Dep_data[c(3, 6, 7, 8, 10, 11)]

dMatrix <- data.frame(cor(Data_to_cluster))#correlation matrix

Dep_dist <- dist(Data_to_cluster, method= "euclidean", diag = FALSE, upper = FALSE) #distance matrix

Dep_completelink <- hclust(Dep_dist, method = 'average') #build dendrogram

#give a cluster number to each observation
Cluster_numbers <- cutree(Dep_completelink, k= 3) #
data_add_cluster <- cbind(Dep_data, Cluster_numbers)

#plot the data with unemployment and crime, add colors for clusters
plot_cluster <- ggplot(data_add_cluster, aes(x = Crime_rate_1k, y = Density_2019, label= Dep_name, color = factor(Cluster_numbers))) + 
  geom_point() +
  geom_text(check_overlap = TRUE) +
  labs(color = "Cluster numbers", x = "Crime rate per thousand inhabitants", y = "Population density")

#Visualize clusters using a map
border <- st_read(here::here("data_end/Departement_geoson_carte.geojson")) #load our geojson file that gives department borders
colnames(border) <- c("Dep_number", "Dep_name", "geometry")
data_cluster_geom <- left_join(border, data_add_cluster, join_by("Dep_number")) #dataframe with our data and the borders

cluster_map <- ggplot() +
  geom_sf(data = data_cluster_geom, aes(fill = as.factor(Cluster_numbers))) + #create the map
  theme(axis.text.x  = element_blank(), #remove plot grids, ticks, labels
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Map of hierarchical clustering", fill = "Cluster numbers") #add a title and a legend title

#Run K-means
withins <- vector() #create a vector

for (i in 1:15) { #run a k-means clustering with 1 to 15 clusters
  kclust <- kmeans(scale(Data_to_cluster), centers = i, nstart = 25)
  withins[i] <- kclust$tot.withinss #each time add it to the vector
}

#plot the scree plot
scree_plot <- ggplot(mapping = aes(x = 1:15, y = withins)) + #plot the scree plot to see how many clsuters should be chosen
  geom_line(color = "blue") + 
  geom_point(color = "blue") +
  geom_vline(xintercept = 6, linetype = "dashed") + #we chose 6 clusters, show it on the plot with a vertical line
  theme_minimal() +
  labs(title = "Optimal number of clusters", x= "Number of clusters", y = "Total Within Sum Squares")

#run the k-means with 6 clusters
kcluster <- kmeans(Data_to_cluster, centers = 6, nstart = 25)
clust_number <- kcluster$cluster #extract cluster membership

#add cluster membership to our dataframe
data_with_clust <- cbind(Dep_data, clust_number)

#visualize the results with a scatter plot and a facet wrap.
cluster_plot <- ggplot(data_with_clust, aes(x = Lepen_score, y = Unemp_2019, color = factor(clust_number))) + #plot Lepen score and Unemployment, the most correlated variables
  geom_point() + 
  geom_text(label= data_with_clust$Dep_name, nudge_x = -0.5, nudge_y = 0.25, check_overlap = TRUE) + #add label names for each department
  labs(title = "Visualization of clusters using Unemployement and Marine Le Pen scores", x = "Score of Marine Le pen", y = "Unemployment", color = "Cluster numbers")

cluster_facet <- cluster_plot + facet_wrap(~clust_number, nrow = 2) #add the facet wrap

#map of our K-means clustering
data_cluster_geom_means <- left_join(border, data_with_clust, join_by("Dep_number")) #add border geometry to our dataset

cluster_map_means <- ggplot() +
  geom_sf(data = data_cluster_geom, aes(fill = as.factor(clust_number))) + #plot the map
  theme(axis.text.x  = element_blank(), #remove axis, ticks, background and grid
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Map of K-means clustering", fill = "Cluster numbers")