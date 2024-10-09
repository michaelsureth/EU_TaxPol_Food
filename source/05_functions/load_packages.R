# File info ---------------------------------------------------------------

# File:    Install and load packages required for all R files of this project 
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# Install packages --------------------------------------------------------
install_packages <- function(pkg){
  
  #'@name install_packages
  #'@title Install required packages and load them
  #'@param pkg Package to be loaded 
  #'@return none
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Determine the package list, install and load them 
package_list <- c(
  'tidyverse',
  'eurostat',
  'countrycode',
  'readxl',
  'insight',
  'openxlsx',
  'data.table',
  'spatstat.geom',
  'dineq',
  'sampleSelection',
  'magrittr',
  'RStata',
  'kableExtra',
  'scales',
  'FNN',
  'parallel',
  'future.apply',
  'haven',
  'ggpubr',
  'sf',
  'RColorBrewer',
  'stringr',
  'ggridges',
  'patchwork',
  'rnaturalearth',
  'vecsets',
  'extrafont',
  'cowplot',
  'ggtext',
  'DescTools'
)

# Function call
install_packages(package_list)


# Load fonts --------------------------------------------------------------

# Fonts are required to save plots as pdf
extrafont::font_import()

# END OF FILE -------------------------------------------------------------