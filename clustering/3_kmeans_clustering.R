# This script runs a k-means analysis with a variable number of cluster centers.
# K-means is a centroid based clustering approach that groups data points around a central point 
# to minimize within group sum of squares and maximize between group sum of squares. To help determine the 
# number of clusters to use, k-means is run with 1 through 4 clusters. 

# Set-up ------------------------------------------------------------------
library(here)
library(purrr)
library(factoextra)

load(here("output", "data_pca3_rt.Rda"))
load(here("output", "data_pca3_bias.Rda"))

# Function to run k-means, varying k ------------------------------------------------

kmeans_vary_k <- function(df, min_k, max_k){
    set.seed(20) # for nstart
    purrr::map(seq(from = min_k, to = max_k),
               ~eclust(df,
                       FUNcluster = 'kmeans',
                       hc_metric = 'euclidean',
                       k = .x, nstart = 25, graph = TRUE))
}

# Run k-means -------------------------------------------------------------

km_data_pca3_rt <- kmeans_vary_k(data_pca3_rt, min_k = 2, max_k = 4)
km_data_pca3_bias <- kmeans_vary_k(data_pca3_bias, min_k = 2, max_k = 4)

save(km_data_pca3_rt, file = here("output", "data_km_pca3_rt.Rda"))
save(km_data_pca3_bias, file = here("output", "data_km_pca3_bias.Rda"))