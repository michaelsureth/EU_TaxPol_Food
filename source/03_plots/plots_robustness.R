# File info ---------------------------------------------------------------

# File:    Plot results of robustness analyses
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# _____________________________________-----------------------------------------
# Preparations -----------------------------------------------------------------

# Set (default) configpath
configpath <- config$p_to_norm%&%"_"%&%
  config$prices%&%"_"%&%
  config$y%&%"_"%&%
  config$cens_yesno%&%"_"%&%
  config$weight_yesno%&%"_"%&%
  config$equations%&%"_"%&%
  config$ycentered_yesno%&%"_"%&%
  config$shares%&%"_"%&%
  config$dse%&%"_"%&%
  config$pzint%&%"_"%&%
  config$year_io

# Load category names
catexplain <- fread("../build/data/intermediate_output/"%&%
                      "HBS_catexplain_"%&%config$categorization%&%".csv",
                    data.table = FALSE)

# Load selected stressors and impacts
stressors_impacts_selected   <- read.csv("../build/data/intermediate_output/"%&%
                                           "MRIO_stressors_impacts_final.csv") %>%
  select(-c("Landusetype", "Landusetype_coeff"))

# Load unit conversion
units_conversion <- read.xlsx("00_data/manual_input/unit_conversion.xlsx")

# Retrieve all robustness configurations
rob_all <- sub(config$procdatapath%&%"policies/VAT_increase/", "", 
               list.dirs(config$procdatapath%&%"policies/VAT_increase", recursive=FALSE))

# Create mapping between impact number and impact names
mapping_impact_no_name <- stressors_impacts_selected %>%
  select(shortname, code_s_i) %>%
  mutate(code_s_i = as.character(code_s_i)) %>%
  filter(shortname %in% selectedindicators_plot) %>%
  deframe()

# _____________________________________-----------------------------------------
# Robustness: Demand system estimation -----------------------------------------

# _ Carbon price ----------------------------------------------------------
plot_policies_compare(countries          = countries,
                      rob_options        = rob_all,
                      graph              = "carbonprice",
                      selectedindicators = selectedindicators_plot,
                      pol                = "tax_GHG emissions") +
  ylim(30, 60)

ggsave("../build/figures/figure_carbonprice_rob.pdf",
       width = 10, height = 7)

# _ Footprint reductions --------------------------------------------------
fp_rob_vat <- plot_policies_compare(countries          = countries,
                                    rob_options        = rob_all,
                                    graph              = "reduction",
                                    selectedindicators = selectedindicators_plot,
                                    pol                = "VAT_increase") +
  labs(caption = "") +
  ggtitle("VAT reform")

fp_rob_ghg <- plot_policies_compare(countries          = countries,
                                    rob_options        = rob_all,
                                    graph              = "reduction",
                                    selectedindicators = selectedindicators_plot,
                                    pol                = "tax_GHG emissions") + 
  ggtitle("GHG emission price")

ggarrange(fp_rob_vat,
          fp_rob_ghg,
          ncol = 1)

ggsave("../build/figures/figure_footprints_rob.pdf",
       width = 18, height = 9)


# END OF FILE ------------------------------------------------------------------