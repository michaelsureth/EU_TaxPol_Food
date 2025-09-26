# File info --------------------------------------------------------------------

# File:    Settings and preparations
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# _____________________________________-----------------------------------------
# _ General --------------------------------------------------------------------

# Print in fixed notation
options(scipen = 999)

# Increase maximum allowed size of global objects
size_in_mb <- 5000
options(future.globals.maxSize = size_in_mb*1024^2)

# Supress dplyr messages on grouping in join-operations
options(dplyr.summarise.inform = FALSE)

# Load packages
source("05_functions/load_packages.R")

# Clean workspace
rm(list = ls())

# Load default config parameters from config_default.do
config <- read.table("config_default.do") %>%
  dplyr::select(V2, V3) %>%
  column_to_rownames(var = "V2") %>%
  t() %>% as.data.frame() %>% as.list()

# Save config_default as config defining correct procdatapath
source("05_functions/helper_functions.R")
config <- adjust_config(parameter = "procdatapath", 
                        value = gsub("source", "build/data/", getwd()))

# set number of cores for parallel processing (forking)
plan(multicore, workers = as.numeric(availableCores(omit = 1)))

# Source user-defined functions
functions <- c("05_functions/dse_functions.R",
               "05_functions/policy_functions.R",
               "05_functions/mrio_functions.R")
invisible(lapply(functions, source))

# EU country names and subregions by UN geoscheme
# (https://unstats.un.org/unsd/methodology/m49/ + Cyprus to South)
EU27 <- eu_countries %>%
  dplyr::select(geo = code, name) %>% 
  mutate(code_iso3 = countrycode(geo,
                                 origin      = "iso2c",
                                 destination = "iso3c")) %>%
  mutate(code_iso3 = ifelse(is.na(code_iso3) & geo == "EL", "GRC", code_iso3)) %>% 
  mutate(region = with(., case_when((geo %in% c("DK", "EE", "FI", "IE", "LT",
                                                "LV", "SE", "UK")) ~ "North", 
                                    (geo %in% c("AL", "CY", "EL", "ES", "HR",
                                                "IT", "ME", "MK", "MT", "PT",
                                                "SI")) ~ "South", 
                                    (geo %in% c("BG", "CZ", "HU", "PL", "RO",
                                                "SK")) ~ "East", 
                                    (geo %in% c("AT", "BE", "DE", "FR", "LU",
                                                "NL")) ~ "West", 
                                    TRUE ~ "NA")))

# Define country vector
countries <- EU27$geo %>% sort()


# Select indicators (in correct order) ------------------------------------
selectedindicators <- c("Biodiversity loss", 
                        "Land use",
                        "N","NH3","NOx",
                        "Phosphorus",
                        "CO2", "CH4", "N2O", "HFC", "PFC", "SF6",
                        "Water")

# Assign plot groups (relevant for Nitrogen and GHG emissions)
# Note: Within plotgroup, Stressors need to be measured in the same unit (eg kg)
selectedindicators_df <- data.frame(shortname = selectedindicators) %>%
  mutate(plotgroup = ifelse(shortname %in% c("N", "NH3", "NOx"), "Nitrogen",
                            ifelse(shortname %in% c("CO2", "CH4", "N2O", "HFC", "PFC", "SF6"), "GHG emissions", 
                                   shortname))) %>% 
  # define order of plotted indicators
  dplyr::mutate(n = with(.,case_when((plotgroup == "Biodiversity loss") ~ 1, 
                                     (plotgroup == "Land use") ~ 2,
                                     (plotgroup == "Nitrogen") ~ 3,
                                     (plotgroup == "Phosphorus") ~ 4,
                                     (plotgroup == "GHG emissions") ~ 5,
                                     (plotgroup == "Water") ~ 6,
                                     TRUE ~ 0)))

# For plots we only show aggregated indicators (in correct order)
# Ensure same order in in plots_policies.R
selectedindicators_plot <- c("Biodiversity loss", 
                             "Land use",
                             "Nitrogen",
                             "Phosphorus",
                             "GHG emissions",
                             "Water")

# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------