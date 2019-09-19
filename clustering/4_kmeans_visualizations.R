# The total within-cluster sum of squares is recorded for each kmeans solution.
# The results are displayed as scatterplots and as a scree plot. The elbow in the scree plot will 
# guide the decision of how many clusters to use.  

# Set-up ------------------------------------------------------------------
library(here)
library(purrr)
library(factoextra)

# Raw bias score
load(here("output", "data_bias.Rda"))

# PCA data
load(here("output", "data_pca3_rt.Rda"))
load(here("output", "data_pca3_bias.Rda"))

# K-means clustering solutions
load(here("output", "data_km_pca3_rt.Rda"))
load(here("output", "data_km_pca3_bias.Rda"))

# Scatterplot colored by cluster membership -------------------------------------
myscat <- function(df, data_tomaponto){
    for (i in seq_along(df)) {
        nclusts <- max(unique(df[[i]]$cluster))
        jpeg(here("plots", paste0("scatplot", deparse(substitute(df)), "_", nclusts, "clusters.jpg")))
        plot(data_tomaponto, 
             col = df[[i]]$cluster, 
             main = paste(nclusts, "clusters:", deparse(substitute(df)))
        )
    }
}

myscat(km_data_pca3_rt, data_pca3_rt)
dev.off()
myscat(km_data_pca3_bias, data_pca3_bias)
dev.off()

# Function for scree plot of total sums of squares for each solution -------------------------------------
myscreeplot <- function(df) {
    wss <- 0
    wss[1] <- NA
    for (i in seq_along(df)) {
        wss[i + 1] <- df[[i]]$tot.withinss
    }
    jpeg(here("plots", paste0("screeplot_totalss_", deparse(substitute(df)), ".jpg")))
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
silplots_bias <- map(km_data_pca3_bias, ~fviz_silhouette(.x, palette = "jco", 
                                                         print.summary = FALSE, 
                                                         ggtheme = theme_minimal()))
silplots_rt <- map(km_data_pca3_rt, ~fviz_silhouette(.x, palette = "jco", 
                                                     print.summary = FALSE, 
                                                     ggtheme = theme_minimal()))

## Save the plots
nclusts_rt <- c(max(km_data_pca3_rt[[1]]$cluster):max(km_data_pca3_rt[[length(km_data_pca3_rt)]]$cluster))
nclusts_bias <- c(max(km_data_pca3_bias[[1]]$cluster):max(km_data_pca3_bias[[length(km_data_pca3_bias)]]$cluster))

paste0("silplot_pca3_rt_", nclusts_rt, "clusters", ".png") %>% 
    here::here("plots", .) %>% 
    walk2(., silplots_rt, ggsave,
          width = 9.5, 
          height = 6.5,
          dpi = 500)

paste0("silplot_pca3_bias_", nclusts_bias, "clusters", ".png") %>% 
    here::here("plots", .) %>% 
    walk2(., silplots_bias, ggsave,
          width = 9.5, 
          height = 6.5,
          dpi = 500)

# Biplots -----------------------------------------------------------------
paste0("biplot_pca3_rt_", nclusts_rt, "clusters", ".png") %>% 
    here::here("plots", .) %>% 
    walk2(km_data_pca3_rt, ., ~ggsave(.y, .x$clust_plot,
          width = 9.5, 
          height = 6.5,
          dpi = 500))

paste0("biplot_pca3_bias_", nclusts_bias, "clusters", ".png") %>% 
    here::here("plots", .) %>% 
    walk2(km_data_pca3_bias, ., ~ggsave(.y, .x$clust_plot,
                                      width = 9.5, 
                                      height = 6.5,
                                      dpi = 500))


# Boxplots ----------------------------------------------------------------

myplot <- function(x, title_as_string){
    jpeg(here("plots", paste0("boxplot_bias_", length(unique(x$cluster)), "clusters.jpg")))
    cbind(data_bias, cluster = x$cluster) %>% 
        gather(value = value, key = variable, -cluster) %>% 
        ggplot(., aes(x = variable, y = value, fill = factor(cluster))) + 
        geom_point(alpha = 0.2,  pch = 21, position = position_jitterdodge()) +
        geom_boxplot(alpha = 0.4) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        ggtitle(title_as_string) 
}

myplot(km_data_pca3_bias[[1]], "Observations by variable and cluster membership: 2 cluster solution") 
dev.off()
myplot(km_data_pca3_bias[[2]], "Observations by variable and cluster membership: 3 cluster solution")
dev.off()
myplot(km_data_pca3_bias[[3]], "Observations by variable and cluster membership: 4 cluster solution")
dev.off()