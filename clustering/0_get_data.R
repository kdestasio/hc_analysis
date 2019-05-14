# This script reads in the dot probe data and splits it into the two data frames that are of interest. 
# One with the response time measures and one with the threat bias measures plus on response time measure.
# The data frames are then saved in the /output folder, which is created by this script if it does not
# exist already.

# Set-up ---------------------------------------------------------
library(here)
library(janitor)

setwd(here())

# Read-in the data ---------------------------------------------------------
path_datafile <- here('data/N=844_FINAL_Traditional metrics_Trial Level metrics_questionnaires (n=837)_6.16.18.xlsx')

data_frame <- clean_names(readxl::read_excel(path_datafile))

# Subset dot probe measures ---------------------------------------------------------
## For the response-time based analysis
data_rt <- data_frame %>% 
    subset(., select = c(rt_neutral_nt, rt_threat_nt, rt_baseline, 
                         mean_pos, mean_neg, peak_pos, peak_neg, variability))
## For the threat-bias based analysis
data_bias <- data_frame %>% 
    subset(., select = c(threatbias_erlab, vigilance_erlab, disengage_erlab, 
                         rt_threat_nt, variability))

# Save the two data frames as .Rda files ---------------------------------------------------------
dir.create(file.path(here(), "output"), showWarnings = FALSE)
save(data_rt, file = "output/data_rt.Rda")
save(data_bias, file = "output/data_bias.Rda")