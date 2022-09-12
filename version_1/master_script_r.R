
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script master_script_r.R         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Helsinki         ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Implement the R-scripts required to replicate the analysis.

# R version 4.0.5, RStudio version 1.2.5033.
# Running time approximately 130 minutes.
rm(list=ls())

# First, make sure that the SAS scripts (listed in master_script_sas.sas) 
# have run successfully.


# To install packages from a CRAN mirror repository in FIONA:
# 1) Create .Rprofile file to the root where the project is:
#     local({
#       r <- getOption("repos")
#       r["CRAN"] <- "https://cran.isaacus.local/"
#       options(repos = r)
#     })
# 2) Restart RStudio. Now you can load CRAN packages by install.packages():
# 3) Use the library() function to load packages.

# The packages loaded are listed below.
library(openxlsx)         # Save as excel file. 
library(stargazer)        # Save as tex file.
library(data.table)       # Mutating data. 
library(ggplot2)          # Plotting data. 
library(patchwork)        # Print multiple plots into same figure. 
library(sf)               # simple features.
library(fixest)           # linear fixed effects estimation.
library(broom)            # Statistical objects into tidy tibbles.
library(fwildclusterboot) # Wild cluster bootstrap.
library(Rglpk)            # R/Gnu linear programming.
library(mvtnorm)          # Multivariate normal probabilities.
library(dplyr)            # data wrangling.
library(HonestDiD)        # Robust inference for DD designs.
library(Synth)            # Synthetic controls estimation.

writeLines(capture.output(sessionInfo()), 'sessionInfo.txt')


###
###

Sys.time()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Create analysis data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Functions that will be used in many R scripts.
#source(file="W:/ASMA2/data/1_shared_functions.R")
#     Functions: save.table() + create.table()


# Clean the raw copayment data, and define the sample.
source(file="W:/ASMA2/data/1_sample.R")
#     Inputs: municipalities_2013.csv + copay_raw.csv
#     Outputs: sample.rds
# Running time <0.5 minutes.


# Assess the data quality, and illustrate the sample.
source(file="W:/ASMA2/data/1_quality_checks.R")
#     Inputs: sample.rds + postal_codes.csv + postal_codes_2015.csv +
#           visits_201X.csv (X in 1:4) + folk_data_201X.csv (X in 1:4) +
#           specialist_visits_201X.csv (X in 1:4) + 
#           dentist_visits_201X.csv (X in 1:4) +
#           social_assistance_201X.csv (X in 2:4) + 1_shared_functions.R
#     Outputs: postal_codes_cleaned.rds + trend_plot_gp.pdf + 
#           trend_plot_ed.pdf + trend_plot_spes.pdf + trend_plot_dentist.pdf +
#           trend_plot_assist.pdf + sample_plot_map.pdf + 
#           summary_table(tex + rds + xlsx) + sample_cleaned.rds
# Running time 14 minutes.


# Link datasets to create analysis data.
source(file="W:/ASMA2/data/1_link_datasets.R")
#     Inputs: sample_cleaned.rds + postal_codes_cleaned.rds +
#           folk_data_201X.csv (X in 1:4) + visits_201X.csv (X in 1:4) +
#           specialist_visits_201X.csv (X in 1:4) + 
#           dentist_visits_201X.csv (X in 1:4) +
#           social_assistance_201X.csv (X in 1:4) + municipalities_2013.csv
#     Outputs: analysis_data.rds + analysis_data_aggr.rds + 
#           analysis_data_synth.rds
# Running time 27 minutes.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Analyses. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Plot the evolution of outcomes to assess pre-trends and effects:
source(file="W:/ASMA2/analysis/2_trend_plots.R")
#     Inputs: analysis_data.rds
#     Outputs: plot_all_gp.pdf + plot_all_rest.pdf + plot_inc_gp.pdf +
#             plot_inc_dent.pdf + plot_detrend_gp.pdf + 
#             plot_detrend_rest.pdf + plot_detrend_dent.pdf 
# Running time 11 minutes.


# Estimate the DD results:
source(file="W:/ASMA2/analysis/2_results_dd.R")
#     Inputs: analysis_data.rds + 1_shared_functions.R
#     Outputs: table_dd_gp(xlsx + tex + rds) + 
#             table_dd_rest(xlsx + tex + rds) + 
#             table_dd_dent(xlsx + tex + rds) + 
#             table_dd_gp_indicator(xlsx + tex + rds) +
#             table_dd_gp_assumptions(xlsx + tex + rds) + dd_income.pdf
# Running time 38 minutes.


# Estimate the DDD results:
source(file="W:/ASMA2/analysis/2_results_ddd.R")
#     Inputs: analysis_data.rds + 1_shared_functions.R
#     Outputs: table_ddd_gp(xlsx + tex + rds) + table_ddd_dent(xlsx + tex + rds)
# Running time 23 minutes.


# Robustness of the DD results:
source(file="W:/ASMA2/analysis/2_results_dd_robust.R")
#     Inputs: analysis_data_aggr.rds + 1_shared_functions.R
#     Outputs: table_dd_hagemann(xlsx + tex + rds) + 
#             table_se_aggr(xlsx + tex + rds) + plot_honestdid.pdf + 
#             plot_leave_x_out.pdf + plot_pta_sensitivity.pdf
# Running time 11 minutes.


# Estimate the synthetic controls results:
source(file="W:/ASMA2/analysis/2_results_synth.R")
#     Inputs: analysis_data_synth.rds + 1_shared_functions.R
#     Outputs: table_synth_weights(xlsx + tex + rds) + plots_synth_gp_main.pdf +
#                plots_synth_gp_rob.pdf
# Running time 20 minutes.

Sys.time()

# End.

