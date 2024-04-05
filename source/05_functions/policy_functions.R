# File info --------------------------------------------------------------------

# File:    Functions for policy analysis
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption


# Compute VAT footprint reductions ----------------------------------------

compute_VAT_fp_reductions <- function(cpe_uncomp,
                                      VAT,
                                      meat,
                                      catexplain,
                                      footprints,
                                      stressors_impacts_selected){
  
  #' @name compute_VAT_fp_reductions
  #' @title Compute VAT footprint reductions
  #' @description Compute footprint reductions due to VAT reform
  #' @param cpe_uncomp List with elasticity matrices for each country
  #' @param VAT table of VAT rates
  #' @param catexplain dataframe with food category codes and names
  #' @param footprints dataframe with footprints for impact-food category
  #'                   combinations for one country
  #' @param stressors_impacts_selected dataframe with selected impacts
  #' @return data_list
  
  # Determine increase in meat prices because of VAT increase
  
  # determine percentage price change
  pct_price_change <- ((1+VAT$VAT_standard_2023/100)/(1+VAT$VAT_meat_2023/100))-1
  
  # determine delta_p vector (percentage increase in prices)
  delta_p_df <- catexplain %>% 
    dplyr::select(category_code_new, category_name_new) %>% 
    mutate(delta_p = ifelse(category_name_new %in% meat, 
                            pct_price_change,
                            0))
  
  # Compute footprint changes due to VAT increase
  
  # (1) Compute quantity change per category
  # each row of cpe_uncomp gives the percentage change in quantity demanded due 
  # to a one percent price increase of the good given in the respective column
  # -> multiply each row with price change in percent and sum over row to get 
  #    aggregate change in quantity demanded (in %); equivalent to a matrix 
  #    multiplication of the 10x10-matrix cpe_uncomp times the 1x10-vector
  #    delta_p_df$delta_p
  demand_reduction <- as.matrix(cpe_uncomp) %*% delta_p_df$delta_p
  
  data_list <- data.frame()
  for (impact_no in unique(footprints$str_imp)){
    
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
    
    # (2) Convert absolute footprint without policy to target unit
    footprint_nopolicy <- footprints %>%
      filter(str_imp == impact_no) %>%
      select(footprint) %>% pull() * unit.conv(from = unit_from,
                                               to   = unit_to)
    
    # (3) Convert price elasticities into (relative) changes in footprints
    # - for 1 percent increase: change in footprint = footprint * CPEc,cc
    # - for x percent increase:
    #          change in footprint = footprint * CPEc,cc * PercentagePriceIncrease
    footprint_reduction <- footprint_nopolicy * demand_reduction
    
    data_list <- rbind(data_list,
                 catexplain %>%
                   select(category_name_new, category_code_new) %>%
                   rename(category  = category_name_new,
                          cat_no    = category_code_new) %>%
                   mutate(impact_name             = impact_name,
                          impact_no               = impact_no,
                          price_increase_rel      = delta_p_df$delta_p,
                          demand_reduction_rel    = demand_reduction,
                          footprint_nopolicy      = footprint_nopolicy,
                          footprint_reduction_abs = footprint_reduction,
                          unit                    = unit_to,
                          footprint_reduction_rel = footprint_reduction_abs / 
                                                     footprint_nopolicy)
    )
  }; rm(impact_no) # close impact-loop
  
  return(data_list)
}



# Compute env tax footprint reductions ------------------------------------

compute_tax_fp_reductions <- function(countries,
                                      intensities,
                                      cpe,
                                      price,
                                      footprints,
                                      impact_taxed,
                                      impact_footprint,
                                      convert_from,
                                      convert_to){
  
  #' @name compute_tax_fp_reductions
  #' @title Compute environmental tax footprint reductions 
  #' @param intensities dataframe with intenisites for each impact per country
  #' @param cpe list with elasticity matrices for each country
  #' @param price vector with level of environmental tax
  #' @footprints dataframe with environmental footprints for all countries
  #' @param impact_taxed impact that is taxed
  #' @param impact_footprint footprint reduction due to tax to be computed
  #' @param convert_from original unit
  #' @param convert_to target unit
  
  # initialize lists for price change and demand reductions
  delta_price_price <- list()
  demand_reduction  <- list()
  
  # initialize data frame for results with nrow = number of food categories
  footprint_reductions <- data.frame(group = 1:nrow(catexplain),
                                     unit  = convert_to)
  
  for(c in countries){
    
    # compute price change in percent by multiplying the footprint intensity per
    # EUR with the environmental tax ("price")
    delta_price_price[[c]] <- intensities %>%                                         
      filter(impact_no == impact_taxed & demandcountry == c) %>%
      {if(is.na(sum(.$intensity_new)))
        
        message("Warning: "%&%c%&%" contains NA intensity for "%&%config$year_io%&%
                  ". NA value is replaced with 0.")
        
        mutate(., intensity_new = ifelse(is.na(intensity_new),
                                         0,
                                         intensity_new))
        } %>%
      mutate(delta_price_price = intensity_new * price) %>%
      select(delta_price_price) %>% pull()
    
    # compute the change in quantity demanded by multiplying the cross-price
    # elasticity matrix with the price change vector (10x10 %*% 10x1)
    demand_reduction[[c]] <- as.matrix(cpe[[c]]) %*% delta_price_price[[c]]
    
    # retrieve the absolute footprint of the relevant impact
    footprints_str_imp <- footprints[[c]] %>%
      filter(str_imp == impact_footprint)
    
    # compute the footprint reduction by multiplying element-wise the vector of
    # quantity changes in percent with the absolute footprints
    footprint_reductions <- footprint_reductions %>%
      mutate(!!sym(c) := demand_reduction[[c]] * footprints_str_imp$footprint * 
               unit.conv(from = convert_from,
                         to   = convert_to))
  }
  colnames(footprint_reductions) <- c("group", "unit", countries)
  
  return <- list("price_increase" = delta_price_price,
                 "demand"         = demand_reduction,
                 "footprint"      = footprint_reductions)
  return(return)
}