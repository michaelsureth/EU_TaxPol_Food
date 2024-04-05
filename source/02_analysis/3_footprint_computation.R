# File info --------------------------------------------------------------------

# File:    Computation of environmental footprints from multi-regional 
#          input-output data
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption

# _________________________________---------------------------------------------
# Aggregation of footprints across products ------------------------------------

# _ Prepare stressor and impact data -------------------------------------------

# Load selected stressors and impacts 
stressors_impacts_selected   <- read.csv("../build/data/intermediate_output/"%&%
                                           "MRIO_selected_stressors_impacts.csv")

# Year
year <- config$year_io

# Number of years
n_year = length(year)

# Read in EXIO region data
EXIO_region_df <- read.xlsx("00_data/manual_input/Bruckner2023.xlsx",
                            sheet = "EXIOBASE_381_regions")

# Number of regions in EXIOBASE
n_reg_EXIO <- nrow(EXIO_region_df)

# Number of sectors in EXIOBASE
if(config$IorP == "pxp"){n_sectors <- 200}else{break()}

# Number of stressor categories
n_stress_old <- stressors_impacts_selected %>% filter(!is.na(s_group)) %>% nrow()
n_stress <- n_stress_old + (stressors_impacts_selected %>% filter(!is.na(s_group)) %>%
                              group_by(s_group) %>% slice(1) %>% nrow())

# Number of stressors including/excluding newly computed ones (excl. biodiversity loss)
n_stress_impact     <- nrow(stressors_impacts_selected)

# Add grouped stressors which will be computed
stressors_impacts_selected <- stressors_impacts_selected %>%
  bind_rows(stressors_impacts_selected %>% group_by(s_group) %>% filter(!is.na(s_group)) %>% 
              slice(1) %>% ungroup() %>%
              transmute(Stressor = s_group, 
                                     Unit, 
                                     S_row_stressor_impact = NA,
                                     s_group = NA,
                                     Landusetype = NA,
                                     shortname = NA,
                                     target.unit,
                                     code_s_i = sequence(n(), 
                                                         from = nrow(stressors_impacts_selected)+1))
  ) 

# Add once again grouped stressors (GHG emissions, nitrogen)
stressors_impacts_selected <- stressors_impacts_selected %>%
  bind_rows(selectedindicators_df %>% filter(shortname!=plotgroup) %>% 
              group_by(plotgroup) %>% slice(1) %>% ungroup() %>%
              left_join(stressors_impacts_selected[, c("Stressor", "Unit", "target.unit")], 
                        by = c("shortname" = "Stressor"))%>%
              mutate(Unit = ifelse(plotgroup=="GHG emissions" & Unit == "kg", "kg CO2 eq.", Unit)) %>%
              dplyr::rename(Stressor = plotgroup) %>%
              dplyr::transmute(Stressor, 
                               Unit, 
                               S_row_stressor_impact = NA,
                               s_group = NA,
                               Landusetype = NA,
                               shortname = NA,
                               target.unit,
                               code_s_i = sequence(n(), 
                                                   from = nrow(stressors_impacts_selected)+1))
  ) %>%
  # add shortnames for grouped stressors
  mutate(shortname = ifelse(is.na(shortname) & is.na(S_row_stressor_impact), Stressor, shortname)) 
  
# Final number of stressors including/excluding newly computed ones (excl. biodiversity loss)
n_stress_impact_new     <- nrow(stressors_impacts_selected)

# Read in biodiversity loss coefficients for land use
biodiv_coeff <- read.xlsx("00_data/manual_input/Bruckner2023.xlsx",
                          sheet = "biodiv_coeff_Koslowski")

# Multiply with 10^6 for converting PDF/m2 to PDF/km2
biodiv_coeff[ , 5:53] <- biodiv_coeff[ , 5:53]*10^6
biodiv_coeff$unit     <- "PDF/km2"

# Add biodiversity loss to stressors_impacts_selected
stressors_impacts_selected <- rbind(stressors_impacts_selected,
                                    c("Biodiversity loss",
                                      "PDF",
                                      rep(NA, 5),
                                      "Biodiversity loss",
                                      "PDF",
                                      nrow(stressors_impacts_selected) + 1))

write.csv(stressors_impacts_selected,
          "../build/data/intermediate_output/MRIO_stressors_impacts_final.csv",
          row.names = FALSE)

# _ Aggregate data for plotting depending on grouping --------------------------

# Repeat footprint computation for different product groupings defined in
# exiobase_product_grouping.xlsx
for(grouping in c("by_foodcat", "by_food_nonfood")){
  
  # load grouping
  if(grouping == "by_foodcat"){
    grouping_matrix <- read.xlsx("00_data/manual_input/exiobase_product_grouping.xlsx",
                                 sheet = config$categorization,
                                 cols  = 2:11,
                                 rows  = 2:202)
  }
  if(grouping == "by_food_nonfood"){
    grouping_matrix <- read.xlsx("00_data/manual_input/exiobase_product_grouping.xlsx",
                                 sheet = "by_food_nonfood",
                                 cols  = 2:3,
                                 rows  = 2:202)
  }
  
  future_lapply(countries, function(c){
    
    if(c=="EL"){ c <- "GR"}
    message(c)
    
    #create column names for (CBF_reg) regionally explicit stressors
    colnames_vec_new <- character(n_stress_impact_new)
    colnames_vec     <- character(n_stress_impact)
    
    for (i in 1:n_stress_impact_new) {
      if (i < n_stress_impact + 1) {
        colnames_vec[i]   <- c%&%"_"%&%stressors_impacts_selected$code_s_i[i]
      }
      colnames_vec_new[i] <- c%&%"_"%&%stressors_impacts_selected$code_s_i[i]
    }; rm(i)
    
    # for all consumption categories (loop)
    for (g in 1:ncol(grouping_matrix)) {
      
      message(g)

      # __ Load disaggregated product-country specific footprint data ----------
      
      # (regionally disaggregated: 200 prod*49 countries x 21 stressors/impacts)
      CBF_reg <- fread(config$procdatapath%&%"MRIO_proc/"%&%config$year_io%&%
                            "/"%&%grouping%&%"/CBF_"%&%config$IorP%&%"_"%&%c%&%
                            "_cat_"%&%g%&%".csv",
                          header = TRUE, data.table = FALSE)
      
      # Rename columns of CBF_reg
      colnames(CBF_reg) <- colnames_vec
      
      # __ Aggregate stress/impact by country/region ---------------------------
      
      # Create empty aggregated CBF matrix without direct stressors/impacts
      # (49 countries x X stressors/impacts)
      CBF_reg_aggr <- data.frame(matrix(0,
                                        nrow = n_reg_EXIO,
                                        ncol = n_stress_impact))
      colnames(CBF_reg_aggr) <- colnames_vec
      
      # Run through columns and 49 EXIO regions and aggregate stressors/impacts
      # (i.e. we load the category- and demand country dataframe, and sum over all
      # sectors by impreg)
      for (col in 1:ncol(CBF_reg)) { # nstressimpact cols
        for (r in 1:n_reg_EXIO) { # 49 import regions
          rows <- seq((r-1)*n_sectors + 1,
                      r   *n_sectors)
          CBF_reg_aggr[r , col] <- sum(CBF_reg[rows, col])
        }; rm(col, r, rows)
      }
      
      # Prepare empty result dataframe: X stressors/impacts * 49 countries
      CBF_df <- data.frame(
        year         = rep(year, each = n_stress_impact*n_reg_EXIO),
        str_imp      = rep(rep(1:n_stress_impact, each = n_reg_EXIO), n_year),
        imp_reg      = rep(EXIO_region_df$cntry, n_stress_impact*n_year),
        imp_reg_code = rep(1:n_reg_EXIO, n_stress_impact*n_year),
        in_EU        = rep(c(rep(1, nrow(EU27)), rep(0, n_reg_EXIO-nrow(EU27))), 
                           n_stress_impact*n_year),
        value        = numeric(n_year*n_stress_impact*n_reg_EXIO)
        )
      
      # Store values in CBF_df for plots
      for (j in 1:n_stress_impact){
        rows <- seq((j-1)*n_reg_EXIO + 1,
                    j    *n_reg_EXIO)
        CBF_df$value[rows] <- CBF_reg_aggr[ , j]
      }; rm(j, rows)
      
      # Add grouped output for stressors (i.e. land use, nitrogen, GHG emission)
      CBF_df_grouped <- CBF_df %>% 
        mutate(code_s_i = as.character(str_imp)) %>%
        # add group information for stressors
        left_join(stressors_impacts_selected, by = "code_s_i") %>% 
        # drop impacts
        filter(!is.na(s_group)) %>%
        # compute characterized stressors (=impacts)
        # note emissions are still in kg but now in kg CO2eq (-> adjust below)
        mutate(value_cf = value*as.numeric(characterization_factor)) %>% 
        # summarise characterized stressors
        group_by(s_group, year, imp_reg, imp_reg_code, in_EU) %>%
        dplyr::summarise(value = sum(value_cf)) %>% ungroup() %>%
        arrange(s_group, imp_reg_code) %>% 
        # add new code_s_i for grouped stressors and unit
        left_join(stressors_impacts_selected[c("Stressor", "code_s_i", "Unit", "target.unit")], 
                  by = c("s_group"="Stressor")) %>%
        dplyr::select(year, str_imp = code_s_i, s_group, imp_reg, 
                      imp_reg_code, in_EU, value, Unit, target.unit) %>% 
        mutate(Unit = ifelse(s_group %in% c("CH4", "CO2", "N2O","HFC", "PFC", "SF6"), 
                             "kg CO2 eq.", Unit))

      # Add another grouping layer for nitrogen and GHGs
      CBF_df_grouped_2 <- CBF_df_grouped %>%
        left_join(selectedindicators_df[,c("shortname", "plotgroup")], 
                  by=c("s_group"="shortname"))%>%
        # filter only not yet summarized stressors
        filter(s_group!=plotgroup) %>% 
        # summarize over plotgroups (nitrogen, GHG emissions)
        group_by(year, in_EU, imp_reg, imp_reg_code, Unit, target.unit, plotgroup) %>%
        summarize(value = sum(value)) %>% ungroup() %>%      
        arrange(plotgroup, imp_reg_code) %>% 
        # add new code_s_i for grouped stressors
        left_join(stressors_impacts_selected[c("Stressor", "code_s_i")], 
                  by = c("plotgroup"="Stressor")) %>%
        dplyr::select(year, str_imp = code_s_i, s_group = plotgroup, imp_reg, 
                      imp_reg_code, in_EU, value, Unit, target.unit)
      
      # _ Biodiversity calculations ---------------------------------------------
      
      # Create vector that assigns biodiversity loss coefficients to land use
      # stressors (from Bruckner et al. 2023 / Koslowski (2020))
      # 6 land use types: annual crops, permanent crops, pasture, urban,
      # extensive forestry, intensive forestry (for 20 land use stressors)
      biodiv_coeff_all_vec <- read.csv("../build/data/intermediate_output/"%&%
                                         "MRIO_selected_stressors_impacts.csv") %>%
        filter(!is.na(Landusetype_coeff)) %>%
        select(Landusetype_coeff) %>% pull()
      
      # Make biodiversity loss coefficients numeric and cut unnecessary data
      biodiv_coeff_num <- biodiv_coeff[ , 5:53] %>%
        mutate_all(function(x){as.numeric(x)})
      
      # Vector of str_imp for land use stressors
      str_imp_landuse <- stressors_impacts_selected %>% 
        filter(!is.na(Landusetype)) %>%
        dplyr::select(code_s_i) %>% pull()
      
      # Keep only land use stressor values of CBF_df
      biodiv_df_year <- CBF_df %>%
        filter(str_imp %in% str_imp_landuse & year == year)
      
      # Prepare empty result dataframe: land use stressors * 49 countries 
      biodiv_df <- data.frame(
        year         = rep(year, each = length(str_imp_landuse)*n_reg_EXIO),
        str_imp      = rep(rep(str_imp_landuse, each = n_reg_EXIO), n_year),
        imp_reg      = rep(EXIO_region_df$cntry, length(str_imp_landuse)*n_year),
        imp_reg_code = rep(1:n_reg_EXIO, length(str_imp_landuse)*n_year),
        in_EU        = rep(c(rep(1, nrow(EU27)), rep(0, n_reg_EXIO-nrow(EU27))), 
                           length(str_imp_landuse)*n_year),
        value        = numeric(n_year*length(str_imp_landuse)*n_reg_EXIO))
      
      # Multiply land use with biodiversity loss coefficients
      for (i in 1:length(str_imp_landuse)){
        
        # define rows to be filled
        rows <- seq((i-1)*n_reg_EXIO + 1,
                     i   *n_reg_EXIO)

          # Create vector for biodiversity coefficients for each EXIOBASE region
          # (filtering correct land element from biodiv_coeff_all_vec)
          coeff_49 <- as.numeric(biodiv_coeff_num[biodiv_coeff_all_vec[i], ])
          
          # Multiply land use with biodiversity loss coefficients for 20 land use
          # categories
          biodiv_df$value[rows] <- biodiv_df_year$value[rows] * coeff_49
          
      } 
    
      # For total land use: summarise biodiversity loss over all land use types
      biodiv_total <- biodiv_df %>%
        group_by(year, imp_reg, imp_reg_code, in_EU) %>%
        dplyr::summarise(value = sum(value)) %>% ungroup() %>% 
        arrange(imp_reg_code) %>%
        # add new code_s_i for grouped stressors
        mutate(Stressor = "Biodiversity loss") %>%
        left_join(stressors_impacts_selected[c("Stressor", "code_s_i","Unit", "target.unit")], 
                  by = "Stressor") %>% 
        dplyr::select(year, str_imp = code_s_i, imp_reg, imp_reg_code, in_EU, value,
                      Unit, target.unit)
      
      # Combine all datasets: Stressors, stressors grouped and biodiversity loss
      CBF_final <- CBF_df %>%
        # add units for Stressors
        mutate(str_imp = as.character(str_imp)) %>%
        left_join(stressors_impacts_selected[c("Stressor", "code_s_i", "Unit", "target.unit")], 
                  by = c("str_imp"="code_s_i")) %>%
        bind_rows(CBF_df_grouped) %>%
        bind_rows(CBF_df_grouped_2) %>%
        bind_rows(biodiv_total) %>% 
        # add shortnames
        left_join(stressors_impacts_selected[c("code_s_i","shortname")], 
                              by = c("str_imp" = "code_s_i")) 

      
      # _ Save -----------------------------------------------------------------
      
      # Create results folder if necessary
      ifelse(!dir.exists(file.path(config$procdatapath%&%"aggr_footprints/"%&%
                                     config$year_io%&%"/"%&%grouping)), 
             dir.create(config$procdatapath%&%"aggr_footprints/"%&%
                                    config$year_io%&%"/"%&%grouping,
                        recursive = TRUE), 
             FALSE)
      
      # Save full dataframe containing the EEMRIO results
      write.csv(CBF_final,
                config$procdatapath%&%"aggr_footprints/"%&%config$year_io%&%
                  "/"%&%grouping%&%"/CBF_final_"%&%c%&%"_cat_"%&%g%&%".csv",
                row.names = FALSE)

    } # end category loop
  }) # end country loop
} # end grouping loop

# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------