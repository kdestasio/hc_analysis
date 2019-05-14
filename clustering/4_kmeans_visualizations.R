# The total within-cluster sum of squares is recorded for each kmeans solution.
# The results are displayed as scatterplots and as a scree plot. The elbow in the scree plot will 
# guide the decision of how many clusters to use.  

# Set-up ------------------------------------------------------------------
library(here)
library(purrr)

setwd(here())

load("output/data_km_pca3_rt.Rda")
load("output/data_km_pca3_bias.Rda")

# Scatterplot colored by cluster membership -------------------------------------


# Scree plot of within cluster sums of squares for each solution -------------------------------------
