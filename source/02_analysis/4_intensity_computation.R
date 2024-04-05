# File info --------------------------------------------------------------------

# File:    Computation of intensities based on computed environmental footprints
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption

# _____________________________________-----------------------------------------
# Preparations -----------------------------------------------------------------

# _ Load selected stressors and impacts ----------------------------------------

stressors_impacts_selected <- fread("../build/data/intermediate_output/"%&%
                                      "MRIO_stressors_impacts_final.csv",
                                    data.table = FALSE) %>%
  select(-c("Landusetype", "Landusetype_coeff"))


# _ Load EXIOBASE regions ------------------------------------------------------

EXIO_region_df <- read.xlsx("00_data/manual_input/Bruckner2023.xlsx",
                            sheet = "EXIOBASE_381_regions")


# _ Categorization of food items -----------------------------------------------

# Load names for categories
catexplain <- fread("../build/data/intermediate_output/"%&%
                      "HBS_catexplain_"%&%config$categorization%&%".csv",
                    data.table = FALSE)



# _________________________________---------------------------------------------
# Intensity computations -------------------------------------------------------

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
  
  message(grouping)
  
  # For each country we have the environmental (GHG/landuse/biodiv/...) footprints
  # by import-region and foodgroup (saved in aggr_footprints as
  # final_country_cat_group.csv)
  
  # We want to get the total intensity by foodgroup. Therefore, we need to
  # (1) aggregate footprint by food group over all import-regions to get total
  #     footprint per foodgroup for the demand country
  # (2) aggregate final demand by foodgroup in MEUR (from y)
  
  intensities <- list()
  x <- 0
  
  for(c in countries){
    
    if(c == "EL"){ c <- "GR" }
    message(c)
    
    for (g in 1:ncol(grouping_matrix)) {
      
      x <- x + 1
      
      
      # __ Load and prepare data -------------------------------------------------
      
      # load demand
      Y_c_g <- fread(config$procdatapath%&%"MRIO_proc/"%&%config$year_io%&%
                       "/"%&%grouping%&%"/Y_"%&%config$IorP%&%"_"%&%c%&%
                       "_cat"%&%g%&%".csv",
                     data.table = FALSE)
      
      # load footprints
      intensities[[x]] <- fread(config$procdatapath%&%"aggr_footprints/"%&%
                                  config$year_io%&%"/"%&%grouping%&%"/CBF_final_"%&%
                                  c%&%"_cat_"%&%g%&%".csv",
                                data.table = FALSE) %>%
        # add country codes (call impreg)
        left_join(EXIO_region_df, by = c("imp_reg" = "cntry")) %>% 
        # add final demand (still by import-region)
        left_join(Y_c_g, by = c("iso_2"="impreg")) %>%
        # summarise across all import regions
        group_by(str_imp) %>%
        dplyr::summarise(FD_hh = sum(FD_hh), value = sum(value)) %>%
        # compute intensities
        mutate(intensity = value/FD_hh) %>%
        # add demand country and category
        mutate(demandcountry = c, category = g)
      
      rm(Y_c_g)
    }
  }
  
  
  # __ Compute intensities -------------------------------------------------------
  
  # bind list to dataframe
  intensities_all <- do.call(rbind.data.frame, intensities) %>%
    # add indicator names
    left_join(stressors_impacts_selected, by = c("str_imp" = "code_s_i")) %>%
    dplyr::select(-S_row_stressor_impact) %>%
    # adjust units: FD_hh from MEUR to EUR 
    mutate(FD_hh_eur = FD_hh * 1000000) %>% 
    # adjust units: emissions into tCO2eq
    mutate(value_new = ifelse(Unit == "kg CO2 eq.",
                              value * unit.conv(from = "kg", to = "t"),
                              ifelse(Unit == "Gg CO2-eq",
                                     value * unit.conv(from = "Gg", to ="t"), 
                                     value)),
           unit_new  = ifelse(Unit == "kg CO2 eq." | Unit == "Gg CO2-eq",
                              "tCO2eq",
                              Unit)) %>% 
    # compute intensities in terms of EUR
    mutate(intensity_new        = value_new/FD_hh_eur,
           unit_intensity_final = paste0(unit_new, "/EUR")) %>%
    # select variables
    dplyr::select(demandcountry, category,
                  impact_no        = str_imp,
                  impact_name      = Stressor,
                  impact_shortname = shortname,
                  FD_hh_eur, value_new, unit_new, intensity_new,
                  unit_intensity_final) %>%
    # add food category names if grouping is by food categories
    {if(grouping == "by_foodcat") 
      full_join(., catexplain, by = c("category" = "category_code_new"))
      else . 
    } %>%
    # create header
    mutate(impact_name_unit = impact_shortname%&%" intensity \n ("%&%
             unit_intensity_final%&%")")
  
  
  # __ Save data -----------------------------------------------------------------
  
  ifelse(!dir.exists(file.path(config$procdatapath%&%"intensities/"%&%
                                 config$year_io)), 
         dir.create(config$procdatapath%&%"intensities/"%&%
                                config$year_io,
                    recursive = TRUE), 
         FALSE)
  
  write.csv(intensities_all,
            "../build/data/intensities/"%&%config$year_io%&%
              "/intensities_"%&%grouping%&%".csv",
            row.names = FALSE)
  
} # close grouping loop

# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------