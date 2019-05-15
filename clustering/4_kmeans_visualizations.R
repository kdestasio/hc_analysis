# The total within-cluster sum of squares is recorded for each kmeans solution.
# The results are displayed as scatterplots and as a scree plot. The elbow in the scree plot will 
# guide the decision of how many clusters to use.  

# Set-up ------------------------------------------------------------------
library(here)
library(purrr)
library(factoextra)

setwd(here())

# PCA data
load("output/data_pca3_rt.Rda")
load("output/data_pca3_bias.Rda")

# K-means clustering solutions
load("output/data_km_pca3_rt.Rda")
load("output/data_km_pca3_bias.Rda")

# Scatterplot colored by cluster membership -------------------------------------
myscat <- function(df){
    for (i in seq_along(df)) {
        nclusts <- max(unique(df[[i]]$cluster))
        jpeg(paste0("plots/scatplot", deparse(substitute(df)), "_", nclusts, "clusters.jpg"))
        plot(data_pca3_bias, 
             col = df[[i]]$cluster, 
             main = paste(nclusts, "clusters:", deparse(substitute(df)))
        )
    }
}

myscat(km_data_pca3_rt)
dev.off()
myscat(km_data_pca3_bias)
dev.off()

# Function for scree plot of total sums of squares for each solution -------------------------------------

myscreeplot <- function(df) {
    wss <- 0
    wss[1] <- NA
    for (i in seq_along(df)) {
        wss[i + 1] <- df[[i]]$tot.withinss
    }
    jpeg(paste0("plots/screeplot_totalss_", deparse(substitute(df)), ".jpg"))
    plot(1:4, wss, type = "b", 
         xlab = "Number of Clusters", 
         ylab = "Within groups sum of squares", 
         main = paste("Total Sum of Squares by clustering solution"),
         ylim = c(0, 7000))
}

## Make the scree plots
myscreeplot(km_data_pca3_rt)
dev.off()
myscreeplot(km_data_pca3_bias)
dev.off()

# Silhoutte plots ---------------------------------------------------------
silplots_bias <- map(km_data_pca3_bias, ~fviz_silhouette(.x, palette = "jco", print.summary = FALSE))
silplots_rt <- map(km_data_pca3_rt, ~fviz_silhouette(.x, palette = "jco", print.summary = FALSE))

nclusts <- c(max(km_data_pca3_bias[[1]]$cluster):max(km_data_pca3_bias[[3]]$cluster))

file_names <- paste0("silplot_pca3_rt_", nclusts, "clusters", ".png")
paths <- here::here("plots", file_names)
walk2(paths, silplots_rt, ggsave,
      width = 9.5, 
      height = 6.5,
      dpi = 500)

file_names <- paste0("silplot_pca3_bias_", nclusts, "clusters", ".png")
paths <- here::here("plots", file_names)
walk2(paths, silplots_bias, ggsave,
      width = 9.5, 
      height = 6.5,
      dpi = 500)

# Biplots -----------------------------------------------------------------
file_names <- paste0("biplot_pca3_rt_", nclusts, "clusters", ".png")
paths <- here::here("plots", file_names)
walk2(km_data_pca3_rt, paths, ~ggsave(.y, .x$clust_plot,
      width = 9.5, 
      height = 6.5,
      dpi = 500))

file_names <- paste0("biplot_pca3_bias_", nclusts, "clusters", ".png")
paths <- here::here("plots", file_names)
walk2(km_data_pca3_bias, paths, ~ggsave(.y, .x$clust_plot,
                                      width = 9.5, 
                                      height = 6.5,
                                      dpi = 500))
