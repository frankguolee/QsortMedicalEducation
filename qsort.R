# Install necessary packages 
# install.packages("dplyr")
# install.packages("ggplot2") 
# install.packages("reshape2") 

# Load packages
library(dplyr)
library(ggplot2)
library(reshape2) 

# Load dataframe
df <- read.csv("C:/Users/filepath/dummy.csv") # Update with filepath of Qsort result csv file


# Define the new names for the statements 
new_names <- c("Statement label 1", 
               "Statement label 2",
               "Statement label 3",
               "Statement label 4",
               "etc.") # Update "Statement label X" with own names (number of labels must match statements) 

# Rename columns in the dataset 
num_cols <- ncol(df)
colnames(df)[2:num_cols] <- new_names 

#--------- CREATING "the general" BARPLOT ---------# 

# Exclude the first column from summing and calculate sums for the remaining columns 
column_sums <- colSums(df[, -1]) 

# Create a data frame with column names and sums 
data <- data.frame(Column = names(column_sums), Sums = column_sums) 

# Create the bar plot with gradient color 
ggplot(data, aes(x = reorder(Column, Sums), y = Sums, fill = Sums)) + 
  geom_bar(stat = "identity", color = "black", alpha = 0.6) + 
  scale_fill_gradient(low = "blue", high = "red") + # Gradient color scale 
  geom_text(aes(label = Sums), vjust = -0.5, size = 3) + # Add text labels for the sums 
  labs(title = "Statement Scores and Ranking", 
       x = "Statements", 
       y = "Total Score", 
       fill = "Score") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) # Rotate column names for readability 

#--------- HIERARCHICAL CLUSTERING ---------# 

# Extract Q-sort scores
qsort_scores <- df[, -1]  # Exclude the first column 
rownames(qsort_scores) <- df$Participant  # Set participant IDs as row names 

# Compute distance matrix 
distance_matrix <- dist(qsort_scores, method = "euclidean") 

# Perform hierarchical clustering 
hc <- hclust(distance_matrix, method = "complete") 

# Assign clusters 
num_clusters <- 2 # Choose number of clusters (e.g. 1-3 clusters)
hierarchical_clusters <- cutree(hc, k = num_clusters) 

# Add cluster assignments to the original dataset 
df$Hierarchical_Cluster <- hierarchical_clusters 

# Perform PCA 
pca <- prcomp(qsort_scores, center = TRUE, scale. = TRUE) 

# Create a data frame for plotting 
pca_data <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], 
                       Hierarchical_Cluster = factor(df$Hierarchical_Cluster)) 

# Plot Hierarchical Clustering results & DOWNLOAD # 
ggplot(pca_data, aes(x = PC1, y = PC2, color = Hierarchical_Cluster)) + 
  geom_point(size = 2, alpha = 0.8) + 
  labs(title = "Visualization of Hierarchical Clustering", 
       x = "Principal Component 1", 
       y = "Principal Component 2",
       color = "Cluster") + 
  theme_minimal() 

# Heatmap for hierarchical clustering averages 

#Ensure non-numeric columns like 'P' are excluded 
# Select only numeric columns (e.g., statements s1 to s20) and cluster assignments 
numeric_columns <- df[, sapply(df, is.numeric)] 

# Add the hierarchical cluster column for aggregation 
numeric_columns$Hierarchical_Cluster <- df$Hierarchical_Cluster 

# Calculate averages for hierarchical clustering 
cluster_means_hc <- aggregate(. ~ Hierarchical_Cluster, data = numeric_columns, FUN = mean) 

# Melt the cluster means data for plotting 
melted_hc <- melt(cluster_means_hc, id.vars = "Hierarchical_Cluster") 

# Calculate number of participants in each hierarchical cluster 
hierarchical_cluster_sizes <- table(df$Hierarchical_Cluster) 

# Add cluster size to hierarchical cluster labels 
cluster_labels_hc <- paste0("Cluster ", names(hierarchical_cluster_sizes), 
                            " (n = ", as.numeric(hierarchical_cluster_sizes), ")") 

# Map cluster sizes to the y-axis labels 
melted_hc$Cluster_Label <- factor(melted_hc$Hierarchical_Cluster, 
                                  levels = unique(melted_hc$Hierarchical_Cluster), 
                                  labels = cluster_labels_hc) 

# Plot heatmap & DOWNLOAD # 
ggplot(melted_hc, aes(x = variable, y = Cluster_Label, fill = value)) + 
  geom_tile() + 
#  scale_y_discrete(limits = rev(levels(factor(melted_hc$Cluster_Label)))) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) + 
  labs(title = "Heatmap of Statement Averages (Hierarchical Clustering)", 
       x = "Statement", y = "", fill = "Score") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank()) 

# How to see which participant is in which cluster: 

# Get unique cluster labels from the Hierarchical_Cluster column 
clusters <- unique(df$Hierarchical_Cluster) 

# Loop through each cluster and print participant codes 
for (cluster in clusters) { 
  cat("Participants in Hierarchical Cluster", cluster, ":\n") 
  print(df$Participant[df$Hierarchical_Cluster == cluster]) 
  cat("\n")} 

