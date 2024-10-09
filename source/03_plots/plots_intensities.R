# File info ---------------------------------------------------------------

# File:    Plot environmental intensities 
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# _____________________________________------------------------------------
# Preparations ------------------------------------------------------------

# Load selected stressors and impacts
stressors_impacts_selected   <- read.csv("../build/data/intermediate_output/"%&%
                                         "MRIO_stressors_impacts_final.csv") %>%
  select(-c("Landusetype", "Landusetype_coeff"))

# Load EXIOBASE regions
EXIO_region_df <- read.xlsx("00_data/manual_input/Bruckner2023.xlsx",
                            sheet = "EXIOBASE_381_regions")

# Load categorization of food items
catexplain <- fread("../build/data/intermediate_output/"%&%
                      "HBS_catexplain_"%&%config$categorization%&%".csv",
                    data.table = FALSE)

# _____________________________________------------------------------------
# Plot intensities --------------------------------------------------------

plot_intensities_point(categorization     = "by_foodcat",
                       selectedindicators = c("GHG emissions"))

ggsave("../build/figures/SI_figure_intensities_point_by_foodcat_"%&%
         config$year_io%&%".pdf",
       width = 8, height = 4)


# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------