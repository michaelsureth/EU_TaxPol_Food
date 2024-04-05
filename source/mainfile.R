# File info --------------------------------------------------------------------

# File:    Main analysis file
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption

# _____________________________________-----------------------------------------
# Preparations and setup -------------------------------------------------------
  
  # Set working directory
  if(commandArgs()[1] == "RStudio"){
    # set working directory to path of current script if opened in RStudio
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    # enable unstable forking in RStudio
    options(parallelly.fork.enable = TRUE)
  } else {
    message(paste("Make sure to set working directory manually to folder where",
                  "mainfile.R is located before sourcing files."))
    }
  
  # prepare workspace
  source("99_functions/preparations.R")


# Data preparation --------------------------------------------------------

# _ prepare household data ------------------------------------------------
  source("01_data_management/easi_data-preparation.R")

# _ prepare MRIO data -----------------------------------------------------
  source("01_data_management/mrio_data-preparation.R")


# Main analysis -----------------------------------------------------------

# _ Demand system estimation ----------------------------------------------

# (1) Demand system analysis - selected parameters
  source("02_analysis/1_la-easi_estimation.R")

# _ MRIO analysis ---------------------------------------------------------

# (3) Footprint computation
  source("02_analysis/3_footprint_computation.R")

# (4) Intensity computation
  source("02_analysis/4_intensity_computation.R")

# _ Policy simulation -----------------------------------------------------

# (5) Policy analysis: VAT reform
  source("02_analysis/5_policy-analysis_VAT.R")

# (6) Policy analysis: Externality pricing
  source("02_analysis/6_policy-analysis_taxes.R")

# (7) Welfare analysis
  source("02_analysis/7_welfare-analysis.R")

# _ Bootstrapping ---------------------------------------------------------

  source("02_analysis/8_bootstrapping.R")

# _ Create plots and tables -----------------------------------------------

# Source functions for plotting
  source("99_functions/plot_functions.R")

# Create elasticity plots
  source("03_plots/plots_elasticities.R")

# Create footprint plots
  source("03_plots/plots_footprints.R")

# Create intensity plots
  source("03_plots/plots_intensities.R")

# Create policies plots
  source("03_plots/plots_policies.R")

# Create welfare plots
  source("03_plots/plots_welfare.R")

# Create elasticity tables
  source("04_tables/tables_elasticities.R")

# Create VAT rates table
  source("04_tables/table_VAT_rates.R")

# Create household descriptive tables
  source("04_tables/tables_hh-data_descriptives.R")

# Create MRIO info tables
  source("04_tables/tables_MRIO_info.R")


# Robustness analyses -----------------------------------------------------

# _ Demand system analysis ------------------------------------------------

# with vs without pz-interaction terms
  config <- adjust_config(parameter = "pzint", value = "pzintyes")
  source("02_analysis/1_la-easi_estimation.R")
  source("02_analysis/5_policy-analysis_VAT.R")
  source("02_analysis/6_policy-analysis_taxes.R")
  source("02_analysis/7_welfare-analysis.R")
  source("02_analysis/8_bootstrapping.R")
  source("03_plots/plots_elasticities.R")
  source("03_plots/plots_policies.R")
  source("03_plots/plots_welfare.R")
  source("04_tables/tables_elasticities.R")
  config <- adjust_config(parameter = "pzint", value = "pzintno")

# censored alln vs uncensored nmin1
  config <- adjust_config(parameter = "cens_yesno", value = "uncensored")
  config <- adjust_config(parameter = "equations", value = "nmin1")
  source("02_analysis/1_la-easi_estimation.R")
  source("02_analysis/5_policy-analysis_VAT.R")
  source("02_analysis/6_policy-analysis_taxes.R")
  source("02_analysis/7_welfare-analysis.R")
  source("02_analysis/8_bootstrapping.R")
  source("03_plots/plots_elasticities.R")
  source("03_plots/plots_policies.R")
  source("03_plots/plots_welfare.R")
  source("04_tables/tables_elasticities.R")
  config <- adjust_config(parameter = "cens_yesno", value = "censored")
  config <- adjust_config(parameter = "equations", value = "alln")

# ycentered vs yuncentered
  config <- adjust_config(parameter = "ycentered_yesno", value = "ycentered")
  source("02_analysis/1_la-easi_estimation.R")
  source("02_analysis/5_policy-analysis_VAT.R")
  source("02_analysis/6_policy-analysis_taxes.R")
  source("02_analysis/7_welfare-analysis.R")
  source("02_analysis/8_bootstrapping.R")
  source("03_plots/plots_elasticities.R")
  source("03_plots/plots_policies.R")
  source("03_plots/plots_welfare.R")
  source("04_tables/tables_elasticities.R")
  config <- adjust_config(parameter = "ycentered_yesno", value = "yuncentered")

# ystone vs ytilda 
  config <- adjust_config(parameter = "y", value = "ytilda")
  source("02_analysis/1_la-easi_estimation.R")
  source("02_analysis/5_policy-analysis_VAT.R")
  source("02_analysis/6_policy-analysis_taxes.R")
  source("02_analysis/7_welfare-analysis.R")
  source("02_analysis/8_bootstrapping.R")
  source("03_plots/plots_elasticities.R")
  source("03_plots/plots_policies.R")
  source("03_plots/plots_welfare.R")
  source("04_tables/tables_elasticities.R")
  config <- adjust_config(parameter = "y", value = "ystone")

# adj. vs. non-adj. unit values
  config <- adjust_config(parameter = "prices", value = "uvnonadj")
  source("02_analysis/1_la-easi_estimation.R")
  source("02_analysis/5_policy-analysis_VAT.R")
  source("02_analysis/6_policy-analysis_taxes.R")
  source("02_analysis/7_welfare-analysis.R")
  source("02_analysis/8_bootstrapping.R")
  source("03_plots/plots_elasticities.R")
  source("03_plots/plots_policies.R")
  source("03_plots/plots_welfare.R")
  source("04_tables/tables_elasticities.R")
  config <- adjust_config(parameter = "prices", value = "uvadj")

# weighted vs. unweighted SUR
  config <- adjust_config(parameter = "weight_yesno", value = "unweighted")
  source("02_analysis/1_la-easi_estimation.R")
  source("02_analysis/5_policy-analysis_VAT.R")
  source("02_analysis/6_policy-analysis_taxes.R")
  source("02_analysis/7_welfare-analysis.R")
  source("02_analysis/8_bootstrapping.R")
  source("03_plots/plots_elasticities.R")
  source("03_plots/plots_policies.R")
  source("03_plots/plots_welfare.R")
  source("04_tables/tables_elasticities.R")
  config <- adjust_config(parameter = "weight_yesno", value = "weighted")

# incomplete vs partialnoDE demand system
  config <- adjust_config(parameter = "dse", value = "incomplete")
  countries <- countries[!countries %in% c("DE")]
  source("02_analysis/1_la-easi_estimation.R")
  source("02_analysis/5_policy-analysis_VAT.R")
  source("02_analysis/6_policy-analysis_taxes.R")
  source("02_analysis/7_welfare-analysis.R")
  source("02_analysis/8_bootstrapping.R")
  source("03_plots/plots_elasticities.R")
  source("03_plots/plots_policies.R")
  source("03_plots/plots_welfare.R")
  source("04_tables/tables_elasticities.R")
  config <- adjust_config(parameter = "dse", value = "partialnoDE")
  countries <- countries[!countries %in% c("DE")]
  source("02_analysis/1_la-easi_estimation.R")
  source("02_analysis/5_policy-analysis_VAT.R")
  source("02_analysis/6_policy-analysis_taxes.R")
  source("02_analysis/7_welfare-analysis.R")
  source("02_analysis/8_bootstrapping.R")
  source("03_plots/plots_elasticities.R")
  source("03_plots/plots_policies.R")
  source("03_plots/plots_welfare.R")
  source("04_tables/tables_elasticities.R")
  config <- adjust_config(parameter = "dse", value = "partial")
  countries <- EU27$geo


# _ MRIO ------------------------------------------------------------------

# year_io: 2019 vs 2011 (non-extrapolated data only)
  config <- adjust_config(parameter = "year_io", value = "2011")
  source("01_data_management/mrio_data-preparation.R") 
  source("02_analysis/3_footprint_computation.R")
  source("02_analysis/4_intensity_computation.R")
  source("02_analysis/5_policy-analysis_VAT.R")
  source("02_analysis/6_policy-analysis_taxes.R")
  source("02_analysis/7_welfare-analysis.R")
  source("02_analysis/8_bootstrapping.R")
  source("03_plots/plots_footprints.R")
  source("03_plots/plots_intensities.R")
  source("03_plots/plots_policies.R")
  source("03_plots/plots_welfare.R")
  config <- adjust_config(parameter = "year_io", value = "2019")


# _ Plot robustness results -----------------------------------------------

  source("03_plots/plots_robustness.R")


# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------