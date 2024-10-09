# File info --------------------------------------------------------------------

# File:    Policy analysis: Compute GHG emission price level with equivalent 
#          GHG emission footprint reductions as VAT policy
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

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


# _ Load (VAT) benchmark footprint reductions ----------------------------------

VAT_benchmarks <- read.csv("../build/data/policies/VAT_increase/"%&%configpath%&%
                             "_"%&%config$year_io%&%"/benchmark_reductions_VAT.csv")


# _ Load footprint intensities -------------------------------------------------

intensities <- fread("../build/data/intensities/"%&%config$year_io%&%
                       "/intensities_by_foodcat.csv",
                     header = TRUE, data.table = FALSE) %>%
  mutate(demandcountry = ifelse(demandcountry == "GR",
                                "EL",
                                demandcountry)) %>%
  arrange(demandcountry, category, impact_no)


# _ Load uncompensated elasticities --------------------------------------------

# load uncompensated elasticities
cpe <-  lapply(countries, load_elasticity_matrix,
               type      = "cpe_uncomp",
               averaging = "mean") 

# name list elements with country codes
names(cpe) <- countries

# _ Load footprints ------------------------------------------------------------

footprints <- future_lapply(countries, function(c){
  
  if( c == "EL"){ c <- "GR" }
  
  files <- "CBF_final_"%&%c%&%"_cat_"%&%1:nrow(catexplain)%&%".csv"
  
  lapply(files,
         function(x){
           fread("../build/data/aggr_footprints/"%&%
                   config$year_io%&%"/by_foodcat/"%&%x,
                 data.table = FALSE) %>%
             rename(footprint = value) %>%
             group_by(str_imp) %>%
             summarize(footprint = sum(footprint))}) %>%
    do.call(rbind.data.frame, .) %>%
    mutate(group = rep(1:nrow(catexplain), each = length(unique(str_imp))))
}) %>%
  setNames(countries)

names(footprints)[names(footprints) == "GR"] <- "EL"


# _____________________________________-----------------------------------------
# Policy analysis: VAT-footprint-reduction-equivalent taxes --------------------

# _ Compute VAT-policy equivalent tax levels -----------------------------------

# loop over relevant impact names
for(impact in c("GHG emissions")){
  
  print_color(impact%&%"\n", "blue")
  
  impact_taxed <- stressors_impacts_selected %>%
    filter(shortname == impact) %>%
    select(code_s_i) %>% pull()
  
  # load relevant prior for price
  p_tax_upper <- as.numeric(config$carbprice_upperlim)
  
  # load total impact reduction in VAT increase policy scenario
  benchmark <- VAT_benchmarks %>%
    filter(impact_name == impact) %>%
    select(footprint_reduction_abs) %>% pull()
  
  # retrieve original and target unit for conversion of final reduction
  unit_from <- stressors_impacts_selected %>%
    filter(shortname == impact) %>%
    mutate(Unit = gsub(" CO2 eq.| CO2-eq", "", Unit)) %>%
    select(Unit) %>% pull()
  
  unit_to   <- stressors_impacts_selected %>%
    filter(shortname == impact) %>%
    select(target.unit) %>% pull()
  
  p_tax <- optimize(function(p_tax){
    reductions <- compute_tax_fp_reductions(countries        = countries,
                                            intensities      = intensities,
                                            cpe              = cpe,
                                            price            = p_tax,
                                            footprints       = footprints,
                                            impact_taxed     = impact_taxed,
                                            impact_footprint = impact_taxed,
                                            convert_from     = unit_from,
                                            convert_to       = unit_to)[["footprint"]] %>%
      select(-group, -unit) %>%
      sum()
    
    deviation <- as.numeric(abs(benchmark - reductions))
    
    return(deviation)
  },
  lower = 30,
  upper = p_tax_upper,
  tol   = 0.0001)$minimum
  
  
  # _ Compute demand and footprint reductions at found tax level ---------------
  
  # vector of relevant impact numbers
  vec_impacts <- footprints[[1]] %>% select(str_imp) %>% pull() %>% unique()
  
  country_list <- list()
  
  for(c in countries){
    
    message(c)
    
    # Compute reductions at found tax level for all indicators
    impact_list_c <-  
      future_lapply(vec_impacts, function(impact_no){
        
        # retrieve original and target unit for conversion of final reduction
        unit_from <- stressors_impacts_selected %>%
          filter(code_s_i == impact_no) %>%
          mutate(Unit = gsub(" CO2 eq.| CO2-eq", "", Unit)) %>%
          select(Unit) %>% pull()
        
        unit_to   <- stressors_impacts_selected %>%
          filter(code_s_i == impact_no) %>%
          select(target.unit) %>% pull()
        
        # name of impact considered
        impact_name <- stressors_impacts_selected %>%
          filter(code_s_i == impact_no) %>%
          select(shortname) %>% pull()
        
        # compute demand and footprint reductions
        reductions <- compute_tax_fp_reductions(countries        = countries,
                                                intensities      = intensities,
                                                cpe              = cpe,
                                                price            = p_tax,
                                                footprints       = footprints,
                                                impact_taxed     = impact_taxed,
                                                impact_footprint = impact_no,
                                                convert_from     = unit_from,
                                                convert_to       = unit_to)
        
        # convert absolute footprint without policy to target unit
        footprint_nopolicy <- footprints[[c]] %>%
          filter(str_imp == impact_no) %>%
          select(footprint) %>% pull() * unit.conv(from = unit_from,
                                                   to   = unit_to)
        
        # create result dataframe
        df_return <- catexplain %>%
          select(category_name_new, category_code_new) %>%
          rename(category  = category_name_new,
                 cat_no    = category_code_new) %>%
          mutate(impact_no               = impact_no,
                 impact_name             = impact_name,
                 env_tax                 = round(p_tax, digits = 2),
                 price_increase_rel      = reductions[["price_increase"]][[c]],
                 demand_reduction_rel    = reductions[["demand"]][[c]],
                 footprint_nopolicy      = footprint_nopolicy,
                 footprint_reduction_abs = reductions[["footprint"]][[c]],
                 unit                    = unit_to) %>%
          mutate(footprint_reduction_rel = footprint_reduction_abs / footprint_nopolicy)
        
        return(df_return)
        
      }, future.seed = TRUE) # close impact loop for one tax level
    
    # list to dataframe: all impacts for country c
    country_list[[c]] <- impact_list_c %>% bind_rows()
    
    # create results folder if required
    if(!dir.exists("../build/data/policies/tax_"%&%impact%&%"/"%&%configpath%&%"_"%&%
                   config$year_io)){
      dir.create("../build/data/policies/tax_"%&%impact%&%"/"%&%configpath%&%"_"%&%
                   config$year_io,
                 recursive = TRUE)
    }
    
    # save results
    write.csv(country_list[[c]],
              "../build/data/policies/tax_"%&%impact%&%"/"%&%configpath%&%"_"%&%
                config$year_io%&%"/reduction_"%&%c%&%".csv")
    
  }; rm(c) # close country loop
  
  
  # __ Save aggregate results---------------------------------------------------
  
  # Aggregate countries to EU27 total
  EU27_total <- country_list %>% bind_rows() %>% 
    group_by(impact_no, impact_name, cat_no, category, unit) %>%
    summarize(across(c("footprint_nopolicy", "footprint_reduction_abs"), sum)) %>%
    mutate(footprint_reduction_rel = footprint_reduction_abs / footprint_nopolicy) %>%
    relocate(category, cat_no, impact_name, impact_no, footprint_nopolicy,
             footprint_reduction_abs, unit, footprint_reduction_rel)
  
  write.csv(EU27_total,
            "../build/data/policies/tax_"%&%impact%&%"/"%&%configpath%&%"_"%&%
              config$year_io%&%"/reduction_EU27.csv",
            row.names = FALSE)
  
} # close impact loop to compute tax levels

# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------