# File info --------------------------------------------------------------------

# File:    Policy analysis: Removal of VAT reduction on meat products
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption

# _____________________________________-----------------------------------------
# Preparations -----------------------------------------------------------------

# Load names for categories
catexplain <- fread("../build/data/intermediate_output/"%&%
                      "HBS_catexplain_"%&%config$categorization%&%".csv",
                    data.table = FALSE)

# Load selected stressors and impacts
stressors_impacts_selected <- fread("../build/data/intermediate_output/"%&%
                                      "MRIO_stressors_impacts_final.csv",
                                    data.table = FALSE) %>%
  select(-c("Landusetype", "Landusetype_coeff"))

# Set configpath according to config.do
configpath <- config$p_to_norm%&%"_"%&%
  config$prices%&%"_"%&%
  config$y%&%"_"%&%
  config$cens_yesno%&%"_"%&%
  config$weight_yesno%&%"_"%&%
  config$equations%&%"_"%&%
  config$ycentered_yesno%&%"_"%&%
  config$shares%&%"_"%&%
  config$dse%&%"_"%&%
  config$pzint

# _ Load footprints ------------------------------------------------------------

footprints <- 
  future_lapply(countries, function(c){
    if( c == "EL"){ c <- "GR" }
    files <- "CBF_final_"%&%c%&%"_cat_"%&%1:nrow(catexplain)%&%".csv"
    
    df_return <- lapply(files,
                        function(x){
                          fread("../build/data/aggr_footprints/"%&%
                                  config$year_io%&%"/by_foodcat/"%&%x,
                                data.table = FALSE) %>%
                            rename(footprint = value) %>%
                            group_by(str_imp) %>%
                            summarize(footprint = sum(footprint))}) %>%
      do.call(rbind.data.frame, .) %>%
      mutate(group = rep(1:nrow(catexplain), each = length(unique(str_imp))))
    
    return(df_return)
  }) %>%
  setNames(countries)


# _ Load VAT increases by country ----------------------------------------------

df_VAT <- read_excel("00_data/manual_input/VAT_rates.xlsx",
                     col_types = c("text", "numeric", "numeric", "text", "text",
                                   "text", "text","text", "text","text", "text",
                                   "text", "text","text", "text"),
                     sheet     = "Sheet1")

# _ Specify food categories affected by VAT increase ---------------------------

meat <- c("Beef", "Pork", "Poultry", "Other meat/animal products")
# safety stop:
if(!identical(intersect(meat, catexplain$category_name_new), meat)){
  stop("Meat categories are not named correctly!")
}


# _____________________________________-----------------------------------------
# Policy analysis: VAT increase on meat products -------------------------------

# _ Load and prepare elasticity estimates --------------------------------------

data_list <- 
  future_lapply(countries, function(c){
  
  # Pull country name
  countryname <- EU27 %>%
    filter(geo == c) %>%
    dplyr::select(name) %>% pull(); countryname
  
  # Retrieve VAT increase
  VAT <- df_VAT %>% filter(country == c)
  
  # Load uncompensated price elasticities
  cpe_uncomp <- load_elasticity_matrix(type      = "cpe_uncomp", 
                                       country   = c, 
                                       averaging = "mean") 

  
  # _ Compute footprint changes due to VAT increase ----------------------------
  
  df_return <- compute_VAT_fp_reductions(cpe_uncomp,
                                         VAT,
                                         meat,
                                         catexplain,
                                         footprints = footprints[[c]],
                                         stressors_impacts_selected)

  if(!dir.exists("../build/data/policies/VAT_increase/"%&%configpath%&%"_"%&%
                 config$year_io%&%"/")){
    dir.create("../build/data/policies/VAT_increase/"%&%configpath%&%"_"%&%
                 config$year_io%&%"/",
               recursive = TRUE)
  }

  write.csv(df_return,
            "../build/data/policies/VAT_increase/"%&%configpath%&%"_"%&%
              config$year_io%&%"/reduction_"%&%c%&%".csv",
            row.names = FALSE)
  
  return(df_return)
}, future.seed = TRUE)

# _ Save aggregate results------------------------------------------------------

# Aggregate countries to EU27 total
EU27_total <- data_list %>%
  bind_rows(.id = "country") %>%
  group_by(impact_no, impact_name, cat_no, category, unit) %>%
  summarize(across(c("footprint_nopolicy", "footprint_reduction_abs"), sum)) %>%
  mutate(footprint_reduction_rel = footprint_reduction_abs/footprint_nopolicy) %>%
  relocate(category, cat_no, impact_name, impact_no, footprint_nopolicy,
           footprint_reduction_abs, unit, footprint_reduction_rel)

write.csv(EU27_total,
          "../build/data/policies/VAT_increase/"%&%configpath%&%"_"%&%
            config$year_io%&%"/reduction_EU27.csv",
          row.names = FALSE)

# Compute benchmark by aggregating across all food categories
benchmark <- EU27_total %>%
  group_by(impact_no, impact_name, unit) %>%
  summarize(footprint_nopolicy = sum(footprint_nopolicy),
            footprint_reduction_abs = sum(footprint_reduction_abs)) %>%
  mutate(country = "Total", .before = 1) %>%
  relocate(unit, .after = last_col())

write.csv(benchmark,
          "../build/data/policies/VAT_increase/"%&%configpath%&%"_"%&%
            config$year_io%&%"/benchmark_reductions_VAT.csv",
          row.names = FALSE)

# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------