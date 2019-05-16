# This script checks the data for missing observations, outliers, near-zero variance, and multicolinearity.
# Means and standard deviations are checked and the data scaled to M = 0, SD = 1. "disengage_erlab" is removed
# from data_bias as it is a linear combination of other variables. Both scaled datasets are saved in /output.


# Set-up ---------------------------------------------------------
library(here)
library(caret)
library(knitr)
library(kableExtra)
library(arsenal)

# load the data frame you want to work with ---------------------------------------------------------
load("output/data_rt.Rda")
load("output/data_bias.Rda")

# Are any observations missing? (Want all FALSE) ---------------------------------------------------------
table(is.na(data_rt))
table(is.na(data_bias))

# Make sure there are no outliers (Want 0) ---------------------------------------------------------
OutVals <- boxplot(data_rt, plot = FALSE)$out
which(data_rt %in% OutVals) 

OutVals <- boxplot(data_bias, plot = FALSE)$out
which(data_bias %in% OutVals) 

# Frequency ratio & % of unique values (Want all FALSE) ---------------------------------------------------------
# http://topepo.github.io/caret/pre-processing.html
caret::nearZeroVar(data_rt, saveMetrics = TRUE) 
caret::nearZeroVar(data_bias, saveMetrics = TRUE) 

# Identify linear combinations (Remove if there are any) ---------------------------------------------------------
caret::findLinearCombos(data_rt) 
caret::findLinearCombos(data_bias) 

# Remove "disengage_erlab" ---------------------------------------------------------
## "disengage_erlab" is a linear combination of "threatbias_erlab" and "vigilance_erlab"
## disengage = threat_bias - vigilance
data_bias <- dplyr::select(data_bias, -disengage_erlab)

# Check means and standard deviations ---------------------------------------------------------
raw_means_rt <- colMeans(data_rt) 
raw_sds_rt <- apply(data_rt, 2, sd) 

raw_means_bias <- colMeans(data_bias) 
raw_sds_bias <- apply(data_bias, 2, sd) 

# Scale the data ---------------------------------------------------------
scaled_data_rt <- scale(data_rt)
scaled_data_bias <- scale(data_bias)

# Check that the data are scaled ---------------------------------------------------------
scaled_means_rt <- round(colMeans(scaled_data_rt), 2)
scaled_sds_rt <- apply(scaled_data_rt, 2, sd)
kable(rbind(raw_means_rt, raw_sds_rt, scaled_means_rt, scaled_sds_rt), digits = 2) %>% 
    arsenal::write2html(paste0(here("output/"), "kable_rt_compare_means.html"), quiet = TRUE)

scaled_means_bias <- round(colMeans(scaled_data_bias), 2)
scaled_sds_bias <- apply(scaled_data_bias, 2, sd)
kable(rbind(raw_means_bias, raw_sds_bias, scaled_means_bias, scaled_sds_bias), digits = 2) %>% 
    arsenal::write2html(paste0(here("output/"), "kable_bias_compare_means.html"), quiet = TRUE)

# Save data ---------------------------------------------------------
save(scaled_data_rt, file = "output/scaled_data_rt.Rda")
save(scaled_data_bias, file = "output/scaled_data_bias.Rda")
