# This script runs a Principal Components Analysis (PCA) on the data. PCA is a dimensionality reduction 
# technique that can be used as a precursor to k-means clustering (Zha, et al., 2002; Ding & He, 2004). 
# PCA projects high dimensional data onto a lower dimensional sub-space and rotates data to mazimize variance 
# in the new axes. The components are then listed in order of  decreasing variance (e.g. component 1 
# has the most variation in the data, component 2 is orthogonal to component 1 and has the most remaining 
# variation, and so on). PCA is an unsuprevised technique that can be used as in this case when we do not 
# have hypotheses about the distribution of varaince across features.


# Set-up ------------------------------------------------------------------
library(here)
dir.create(here("plots"), showWarnings = FALSE)

# Load the scaled data ----------------------------------------------------
load(here("output", "scaled_data_rt.Rda"))
load(here("output", "scaled_data_bias.Rda"))

# Run PCA on the scaled data ----------------------------------------------
pca_scaled_rt <- prcomp(x = scaled_data_rt)
pca_scaled_bias <- prcomp(x = scaled_data_bias)

# Examine the PCA Results ---------------------------------------------------------
jpeg("plots/pca_rt.jpg")
biplot(pca_scaled_rt)
dev.off()
jpeg("plots/pca_bias.jpg")
biplot(pca_scaled_bias)
dev.off()

# Get the proportion of variance ---------------------------------------------------------
pca_var_rt <- pca_scaled_rt$sdev^2
prop_var_each_rt <- pca_var_rt / sum(pca_var_rt)

pca_var_bias <- pca_scaled_bias$sdev^2
prop_var_each_bias <- pca_var_bias / sum(pca_var_bias)

# PCA variance plots ---------------------------------------------------------
## Plot the variance explained for each principal component
jpeg("plots/pca_variance_rt.jpg")
par(mfrow = c(1,2)) 
plot(prop_var_each_rt, 
     main = "Response Time Data Set PCA",
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

## Plot cumulative proportion of variance explained
plot(cumsum(prop_var_each_rt), 
     main = "Response Time Data Set PCA",
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")
dev.off()

## Plot the variance explained for each principal component
jpeg("plots/pca_variance_bias.jpg")
par(mfrow = c(1,2)) 
plot(prop_var_each_bias, 
     main = "Threat Bias Data Set PCA",
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

## Plot cumulative proportion of variance explained
plot(cumsum(prop_var_each_rt), 
     main = "Threat Bias Data Set PCA",
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")
par(mar = c(0, 0, 0, 0)) 
dev.off()

# Create tables to look at proportion and cumulative variance explained ---------------------------------------------------------
pca_summary_rt <- summary(pca_scaled_rt)
kable(pca_summary_rt$importance, digits = 2, 
      caption = 'PCA Summary Table, RT Dataset') %>% 
    arsenal::write2html(here("output", "kable_pca_variance_rt.html"), quiet = TRUE)

pca_summary_bias <- summary(pca_scaled_bias)
kable(pca_summary_bias$importance, digits = 2, 
      caption = 'PCA Summary Table, Threat Bias Dataset') %>% 
    arsenal::write2html(here("output", "kable_pca_variance_bias.html"), quiet = TRUE)

# Save data ---------------------------------------------------------
data_pca3_rt <- pca_scaled_rt$x[,1:3]
data_pca3_bias <- pca_scaled_bias$x[,1:3]
save(data_pca3_rt, file = here("output", "data_pca3_rt.Rda"))
save(data_pca3_bias, file = here("output", "data_pca3_bias.Rda"))

save(pca_summary_rt, file = here("output", "data_pcasummary_rt.Rda"))
save(pca_summary_bias, file = here("output", "data_pcasummary_bias.Rda"))