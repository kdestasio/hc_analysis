# This script runs a k-means analysis with a variable number of cluster centers.
# K-means is a centroid based clustering approach that groups data points around a central point 
# to minimize within group sum of squares and maximize between group sum of squares. To help determine the 
# number of clusters to use, k-means is run with 1 through 4 clusters. 


# Set-up ------------------------------------------------------------------
library(here)
library(purrr)

setwd(here())

load("output/data_pca3_rt.Rda")
load("output/data_pca3_bias.Rda")


# Function to run k-means, varying k ------------------------------------------------
kmeans_vary_k <- function(df, min_k, max_k){
    set.seed(20) # for nstart
    km_out <- purrr::map(seq(from = min_k, to = max_k), 
                             ~kmeans(df, centers = .x, nstart = 20))
    assign(paste0("km_", deparse(substitute(df))), km_out, envir = globalenv())
}

# Run k-means -------------------------------------------------------------

kmeans_vary_k(data_pca3_rt, min_k = 1, max_k = 4)
kmeans_vary_k(data_pca3_bias, min_k = 1, max_k = 4)

save(km_data_pca3_rt, file = "output/data_km_pca3_rt.Rda")
save(km_data_pca3_bias, file = "output/data_km_pca3_bias.Rda")