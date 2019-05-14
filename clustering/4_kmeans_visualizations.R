# The total within-cluster sum of squares is recorded for each kmeans solution.
# The results are displayed as scatterplots and as a scree plot. The elbow in the scree plot will 
# guide the decision of how many clusters to use.  

# Set-up ------------------------------------------------------------------
library(here)
library(purrr)

setwd(here())

# PCA data
load("output/data_pca3_rt.Rda")
load("output/data_pca3_bias.Rda")

# K-means clustering solutions
load("output/data_km_pca3_rt.Rda")
load("output/data_km_pca3_bias.Rda")

# Scatterplot colored by cluster membership -------------------------------------
for (i in seq(1, length(km_data_pca3_bias))) {
    num_clusts <- length(unique(km_data_pca3_bias[[i]]$cluster))
    jpeg(paste0("plots/scatplot_km", num_clusts, "clusters.jpg"))
    plot(data_pca3_bias, 
         col = km_data_pca3_bias[[i]]$cluster, 
         main = paste(num_clusts, "clusters:", deparse(substitute(km_data_pca3_bias))))
    dev.off()
}

# Scree plot of within cluster sums of squares for each solution -------------------------------------
