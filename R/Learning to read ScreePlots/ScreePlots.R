#set dir----
rm(list = ls())
setwd("/Users/nikhilahuja/Documents/MGSC_Courses/MGSC_310/Assignment4/")

#libraries----
library(readr)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)

#preprocessing----
#load data
data <- read_csv("dataset_socialMedia-2.csv")

#missing values
data_cleaned <- na.omit(data)

#cleaning
data_standardized <- scale(data_cleaned)

#pca----
#pca analysis
pca_result <- PCA(data_standardized, graph = FALSE)

#scree----
fviz_eig(pca_result)

#calculate the culm variance/explained variance----
explained_variance <- pca_result$eig/sum(pca_result$eig) * 100
cumulative_variance <- cumsum(explained_variance)

cumulative_variance_df <- data.frame(Component = 1:length(cumulative_variance), 
                                     CumulativeVariance = cumulative_variance)

#variance plot to determin components
ggplot(cumulative_variance_df, aes(x = Component, y = CumulativeVariance)) +
  geom_line() + 
  geom_point() +
  theme_minimal() +
  labs(title = "Cumulative Explained Variance", x = "Number of Components", y = "Cumulative Variance Explained (%)")

#pca with components and biplots----
#pca 2 components
pca_result_2 <- PCA(data_standardized, ncp = 2, graph = FALSE)

#biplot of individuals
fviz_pca_ind(pca_result_2,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#biplot of variables
fviz_pca_var(pca_result_2,
             col.var = "contrib", # Color by contributions to the PCA
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#make plots into a data table----
#extract loadings
loadings <- pca_result_2$var$coord[, 1:2]

#make data frame
loadings_df <- as.data.frame(loadings)

#column names
colnames(loadings_df) <- c("PC1", "PC2")

#print
print(loadings_df)




#comments----
#scree plot
#for the scree plot you look for the elbow in the data and try to find where the point where a variable is the most unsignificant compared to the previous one.
#and then you take the prior two so in this case we took the first two components since the biggest jump was between them.

#PC1 Engagement/Interaction
#Since variables like likes,shares,comments, and engagements have positive loadings, pc1 one can be seen as the Engagment/interaction variable

#PC2 Total Activity
#since variables like "Page total likes", "Post Month", and "Post Hour" have a strong positive loading, pc2 is seen as the total activity variable
