# File info -------------------------------------------------------------

# File:    Monetize environmental benefits of policies
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# Preparations ----------------------------------------------------------

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
  config$pzint%&%"_"%&%
  config$year_io

# Load names for categories
catexplain <- fread("../build/data/intermediate_output/"%&%
                      "HBS_catexplain_"%&%config$categorization%&%".csv",
                    data.table = FALSE)

# Load EXIOBASE regions
EXIO_region_df <- read.xlsx("00_data/manual_input/Bruckner2023.xlsx",
                            sheet = "EXIOBASE_381_regions")

# Load selected stressors and impacts
stressors_impacts_selected   <- read.csv("../build/data/intermediate_output/"%&%
                                           "MRIO_stressors_impacts_final.csv") 

# Load unit conversion
units_conversion <- read.xlsx("00_data/manual_input/unit_conversion.xlsx")


# Load number of households for all EU27 countries
EU_hh_year <- read_excel("00_data/lfst_hhnhwhtc__custom_10042738_spreadsheet.xlsx",
                         sheet = "Sheet 1",
                         skip  = 10) %>%
  dplyr::transmute(name = ifelse(TIME == "Germany (until 1990 former territory of the FRG)",
                                 "Germany", TIME), 
                   # convert unit (thousand households) into households
                   nhh = as.numeric(!!sym(config$year_io))*1000) %>% 
  right_join(EU27[,c("geo", "name")], by="name") %>%
  mutate(weight = nhh/sum(nhh))

# Choose policy to compare
policy <- "tax_GHG emissions"


# _____________________________________------------------------------------
# TAX INCOME --------------------------------------------------------------

# Load tax revenue changes (VAT and GHG emission price income) by household
tax_inc <- fread("../build/data/welfare/"%&%configpath%&%"/tax_paid_changes.csv",
                 data.table = FALSE, showProgress = FALSE)

# Aggregate by country and for the EU
tax_inc_final <- tax_inc %>%
  # compute weighted mean per household additional VAT and GHG emission price
  # income per country
  group_by(policy, country) %>%
  dplyr::summarise(tax_VAT_diff_mean = weighted.mean(VAT_diff, hh_wgt),
            tax_GHG_inc_mean  = weighted.mean(GHG_inc, hh_wgt)) %>%
  mutate(tax_inc_mean = tax_VAT_diff_mean+tax_GHG_inc_mean)%>%
  ungroup() %>%
  # multiply mean by number of households for absolute change in tax income
  left_join(EU_hh_year %>% dplyr::select(geo, nhh),
            by = c("country" = "geo")) %>%
  mutate(tax_inc_abs_MEUR = tax_inc_mean*nhh/1e6) %>%
  # summarize over all EU27 countries
  group_by(policy) %>%
  dplyr::summarise(value_MEUR = sum(tax_inc_abs_MEUR),
                   plotgroup = "Tax income",
                   s_group = "Tax income")
  
# _____________________________________------------------------------------
# MONETIZED WELFARE COSTS -------------------------------------------------

# Load mean welfare costs (based on lcol_abs) for households
df_welfare <- list()
for(pol in c("VAT_increase",  policy)){
  # load lcol_abs results 
  df_welfare[[pol]] <- fread("../build/data/welfare/"%&%configpath%&%
                             "/"%&%pol%&%"/"%&%"country-means_lcol.csv",
                           data.table = FALSE) %>%
    mutate(policy  = pol)
}

# Aggregate by country and for the EU
welfare_final <- df_welfare %>% 
  bind_rows() %>%
  # multiply mean by number of households for absolute change welfare
  left_join(EU_hh_year %>% dplyr::select(geo, nhh),
            by = c("country" = "geo")) %>%
  mutate(lcol_abs_MEUR = mean_lcol_abs*nhh/1e6) %>% 
  # summarize: EU-wide
  group_by(policy) %>% 
  dplyr::summarise(value_MEUR = (-1)*sum(lcol_abs_MEUR),
                   plotgroup = "Consumer surplus",
                   s_group = "Consumer surplus")

# _____________________________________------------------------------------
# MONETIZED BENEFITS ------------------------------------------------------

# _ Preparation: Spatial assignment of impact reductions ------------------
# So far, reductions have not been assigned to the location of impact.
# We assume that reductions occur proportional to where impacts are  
# generated in the status quo. 

# (1) For each environmental indicator, determine the share that occurs in any 
# given EXIOBASE region due to consumption of category i in country c
  
  footprints <- list()
  x <- 0
  
  for (c in countries){
    
    if(c == "EL"){ c <- "GR" }
    
    for (g in 1:nrow(catexplain)) {
      
      x <- x + 1
      
      footprints[[x]] <- fread(config$procdatapath%&%"aggr_footprints/"%&%
                                config$year_io%&%"/by_foodcat/CBF_final_"%&%
                                c%&%"_cat_"%&%g%&%".csv",
                              data.table = FALSE) %>% 
        # add country codes (call impreg)
        left_join(EXIO_region_df, by = c("imp_reg" = "cntry")) %>% 
        # compute share of footprints across all import regions
        group_by(str_imp) %>%
        dplyr::mutate(totalvalue = sum(value),
                      share = value/totalvalue) %>% 
        # add demand country and category
        transmute(demandcountry = ifelse(c == "GR", "EL", c),
                  category      = as.character(g),
                  Unit, target.unit,
                  str_imp, shortname, 
                  imp_reg, iso_2, in_EU, 
                  value, totalvalue, share) %>% 
        ungroup()
      
    }
  }
  
  df_footprints <- footprints %>% bind_rows()

# (2) Load reductions of consumption = reductions of footprints of category i 
# in country c 
  pol_c <- list()
  x <- 0
  
  for(pol in c("VAT_increase", policy)){
  
    for (c in countries){
      
      countryname <- EU27[EU27$geo == c,]$name
      
      x <- x+1
      
      pol_c[[x]] <- fread("../build/data/policies/"%&%pol%&%"/"%&%configpath%&%
                            "/reduction_"%&%c%&%".csv",
                          data.table = FALSE) %>%
        transmute(category_name           = category, 
                  category                = as.character(cat_no), 
                  footprint_reduction_rel = round(footprint_reduction_rel, 10),
                  policy                  = pol, 
                  country                 = c, 
                  countryname             = countryname) %>%
        distinct() %>%
        # drop NAs (coming from Stressors with 0 footprint)
        filter(!is.na(footprint_reduction_rel))
      
    }
  }
  
  df_pol <- pol_c %>% bind_rows() %>% distinct() %>% 
    pivot_wider(names_from  = policy, 
                values_from = footprint_reduction_rel, 
                names_glue  = "{policy}_footprintchangepct") 

# (3) Determine absolute impact reductions by EXIOBASE imp_regs across all
# categories and demandcountries  based on (1) and (2)
delta_footprints_imp <- df_footprints %>% 
  left_join(df_pol, by = c("demandcountry" = "country", "category")) %>% 
  mutate(footprint_post_VAT  = value*(1+VAT_increase_footprintchangepct),
         footprint_post_GHG  = value*(1+`tax_GHG emissions_footprintchangepct`),
         delta_footprint_VAT = value - footprint_post_VAT,
         delta_footprint_GHG = value - footprint_post_GHG) %>%
  # group by importing regions
  group_by(imp_reg, iso_2, str_imp, Unit, in_EU, target.unit) %>%
  summarise_at(vars(delta_footprint_VAT, delta_footprint_GHG), ~sum(.)) %>%
  ungroup()

# To disentangle by demandcountry: 
# Determine absolute impact reductions by EXIOBASE imp_regs across all categories
delta_footprints_imp_bycountry <- df_footprints %>% 
  left_join(df_pol, by = c("demandcountry" = "country", "category")) %>% 
  mutate(footprint_post_VAT  = value*(1+VAT_increase_footprintchangepct),
         footprint_post_GHG  = value*(1+`tax_GHG emissions_footprintchangepct`),
         delta_footprint_VAT = value - footprint_post_VAT,
         delta_footprint_GHG = value - footprint_post_GHG) %>%
  # group by importing regions
  group_by(demandcountry, imp_reg, iso_2, str_imp, Unit, in_EU, target.unit) %>%
  summarise_at(vars(delta_footprint_VAT, delta_footprint_GHG), ~sum(.)) %>%
  ungroup()

# _ Social cost of carbon -------------------------------------------------

# (1) Load social cost of carbon

  # Load dollar inflation rate of 2020 (to convert 2020 dollars to 2019 dollars)
  # Source: https://www.macrotrends.net/countries/USA/united-states/inflation-rate-cpi
  dollar_inflation_rate_2020 <- 1.23
  # Load dollar-euro exchange rate of 2019 (convert 2019 dollars to 2019 euro)
  # Source: https://www.macrotrends.net/2548/euro-dollar-exchange-rate-historical-chart
  dollar_euro_2019 <- 1.12
  
  # Load social cost of GHG (CO2, CH4, N2O) 
  # Source: https://www.epa.gov/system/files/documents/2023-12/epa_scghg_2023_report_final.pdf
  sc_ghg <- readxl::read_excel("00_data/manual_input/social_cost_epa.xlsx",
                               sheet = "Sheet1",
                               range = "A3:E6") %>% 
    pivot_longer(cols      = SCCO2:SCN2O, 
                 names_to  = "GHG",
                 values_to = "value_dollar_2020") %>%
    mutate(value_dollar_2019 = value_dollar_2020/(1+dollar_inflation_rate_2020/100),
           value_euro_2019   = value_dollar_2019/dollar_euro_2019) %>% 
    transmute(discount_rate,
              GHG = gsub("SC","",GHG),
              value_euro_2019)
    
  # Global GHG emission reductions
  globalemissionreductions <- delta_footprints_imp %>% 
    left_join(stressors_impacts_selected[,c("code_s_i","Stressor","s_group", "Unit")], 
              by = c("str_imp"="code_s_i", "Unit")) %>% 
    # note: Stressors are in original kg (=not in CO2eq as summarized stressors, 
    # eg CO2, CH4, N2O, ...)
    filter(s_group %in% c("CO2", "CH4", "N2O", "HFC", "PFC", "SF6")) %>% 
    # summarize over all EU27 demandcountries, import regions and categories  
    group_by(s_group, Unit) %>%
    dplyr::summarise(delta_footprint_VAT = sum(delta_footprint_VAT),
                     delta_footprint_GHG = sum(delta_footprint_GHG)) %>% 
    ungroup() %>% 
    # convert to tons 
    dplyr::mutate(delta_footprint_VAT = ifelse(grepl("kg", Unit),
                                               delta_footprint_VAT/1000,
                                               delta_footprint_VAT),
                  delta_footprint_GHG = ifelse(grepl("kg", Unit),
                                               delta_footprint_GHG/1000,
                                               delta_footprint_GHG),
                  Unit = ifelse(Unit == "kg", "t", 
                         ifelse(Unit == "kg CO2-eq", "tCO2eq",NA))) %>% 
    
  # Add social cost estimates to reductions 
    left_join(  sc_ghg[sc_ghg$discount_rate==2,], 
                by = c("s_group" = "GHG"))  %>%
  
  # SF6 is given in kg SF6 but needs to be valued at SC-CO2
  # option 1: convert using GWP (used), option 2: filter for str_imp in CO2eq
      mutate(delta_footprint_VAT = ifelse(s_group != "SF6", 
                                        delta_footprint_VAT,
                                        delta_footprint_VAT*
                                          stressors_impacts_selected[stressors_impacts_selected$s_group == "SF6" & 
                                          !is.na(stressors_impacts_selected$s_group),]$characterization_factor
                                        ),
           delta_footprint_GHG = ifelse(s_group != "SF6", 
                                        delta_footprint_GHG,
                                        delta_footprint_GHG*
                                          stressors_impacts_selected[stressors_impacts_selected$s_group == "SF6" & 
                                          !is.na(stressors_impacts_selected$s_group),]$characterization_factor
                                        ),
           Unit = ifelse(s_group != "SF6", 
                         Unit,
                         paste0(Unit, "CO2eq")))
           
    # CO2, CH4 and N2O are valued at their respective SC
    # HFC, PFC, SF6 are now given in CO2eq and will be valued at SC-CO2
    globalemissionreductions[globalemissionreductions$Unit == "tCO2eq",]$value_euro_2019 <- 
      sc_ghg[sc_ghg$GHG == "CO2" & sc_ghg$discount_rate==2,]$value_euro_2019
    
    # Compute total reduction benefits in MEUR and generate output table
    GHG_final <- globalemissionreductions %>%
      transmute(s_group,
                VAT_increase        = delta_footprint_VAT*value_euro_2019/1e6,
                `tax_GHG emissions` = delta_footprint_GHG*value_euro_2019/1e6) %>%
      pivot_longer(-s_group,
                   names_to  = "policy",
                   values_to = "value_MEUR") %>%
    # add s_group %>%
      left_join(selectedindicators_df[,c("shortname", "plotgroup")], 
                by = c("s_group"="shortname")) 
    
    
    # ---- Global emission reduction by country ---
    globalemissionreductions_bycountry <- delta_footprints_imp_bycountry %>% 
      left_join(stressors_impacts_selected[,c("code_s_i","Stressor","s_group", "Unit")], 
                by = c("str_imp"="code_s_i", "Unit")) %>% 
      # note: Stressors are in original kg (=not in CO2eq as summarized stressors, 
      # eg CO2, CH4, N2O, ...)
      filter(s_group %in% c("CO2", "CH4", "N2O", "HFC", "PFC", "SF6")) %>% 
      # summarize over all EU27 demandcountries, import regions and categories  
      group_by(s_group, Unit, demandcountry) %>% 
      dplyr::summarise(delta_footprint_VAT = sum(delta_footprint_VAT),
                       delta_footprint_GHG = sum(delta_footprint_GHG)) %>% 
      ungroup() %>% 
      # convert to tons 
      dplyr::mutate(delta_footprint_VAT = ifelse(grepl("kg", Unit),
                                                 delta_footprint_VAT/1000,
                                                 delta_footprint_VAT),
                    delta_footprint_GHG = ifelse(grepl("kg", Unit),
                                                 delta_footprint_GHG/1000,
                                                 delta_footprint_GHG),
                    Unit = ifelse(Unit == "kg", "t", 
                                  ifelse(Unit == "kg CO2-eq", "tCO2eq",NA))) %>%
      
      
      # Add social cost estimates to reductions 
      left_join(  sc_ghg[sc_ghg$discount_rate==2,], 
                  by = c("s_group" = "GHG"))  %>%
      
      # SF6 is given in kg SF6 but needs to be valued at SC-CO2
      # option 1: convert using GWP (used), option 2: filter for str_imp in CO2eq
      mutate(delta_footprint_VAT = ifelse(s_group != "SF6", 
                                          delta_footprint_VAT,
                                          delta_footprint_VAT*
                                            stressors_impacts_selected[stressors_impacts_selected$s_group == "SF6" & 
                                                                         !is.na(stressors_impacts_selected$s_group),]$characterization_factor
      ),
      delta_footprint_GHG = ifelse(s_group != "SF6", 
                                   delta_footprint_GHG,
                                   delta_footprint_GHG*
                                     stressors_impacts_selected[stressors_impacts_selected$s_group == "SF6" & 
                                                                  !is.na(stressors_impacts_selected$s_group),]$characterization_factor
      ),
      Unit = ifelse(s_group != "SF6", 
                    Unit,
                    paste0(Unit, "CO2eq")))
    
    # CO2, CH4 and N2O are valued at their respective SC
    # HFC, PFC, SF6 are now given in CO2eq and will be valued at SC-CO2
    globalemissionreductions_bycountry[globalemissionreductions_bycountry$Unit == "tCO2eq",]$value_euro_2019 <- 
      sc_ghg[sc_ghg$GHG == "CO2" & sc_ghg$discount_rate==2,]$value_euro_2019
    
    # Compute total reduction benefits in MEUR and generate output table
    GHG_final_bycountry <- globalemissionreductions_bycountry %>%
      transmute(s_group,
                demandcountry,
                VAT_increase        = delta_footprint_VAT*value_euro_2019/1e6,
                `tax_GHG emissions` = delta_footprint_GHG*value_euro_2019/1e6) %>%
      pivot_longer(-c(s_group,demandcountry),
                   names_to  = "policy",
                   values_to = "value_MEUR") %>%
      # add s_group %>%
      left_join(selectedindicators_df[,c("shortname", "plotgroup")], 
                by = c("s_group"="shortname")) 
    

# _ Social cost of nitrogen ------------------------------------------
# Source: van Grinsven et al 2018
# We only determine the value of benefits accruing within the EU (excl. Croatia) 
# since no robust social cost of nitrogen estimates are available globally.
# Note: We use these values as given (no inflation adjustment).
  
  # Load SCN for EU countries (by N compound)
  scn <- read_excel("00_data/manual_input/vanGrinsven_2018.xlsx",
                    sheet = "Sheet1",
                    range = "A3:K30") %>% 
    filter(!is.na(Emission)) %>%
    mutate(Emission = ifelse(Emission == "Czech. Rep", "Czechia", Emission)) %>%
    left_join(EU27, by = c("Emission" = "name")) %>%
    dplyr::select(country = Emission,geo,
                  NOx,
                  NH3     = NHx,
                  N       = Nleach) %>%
    pivot_longer(cols      = NOx:N,
                 names_to  = "Stressor",
                 values_to = "SCN_EUR_kg")
  
  # Nitrogen stressors
    stressors_nitrogen <- stressors_impacts_selected %>% 
      filter(s_group %in% c("NOx", "NH3", "N")) %>% 
      dplyr::select(Stressor) %>% pull()
  
  # Compute social cost of nitrogen (N, NH3, NOx) footprints for country c and category i
  
  # (1) Load footprints  
  fp_scn <- list()
  x <- 0
  
  for(c in countries){
  
    if(c == "EL"){ c <- "GR"}
    
    for (i in catexplain$category_code_new){
      
      x <- x+1
      
      fp_scn[[x]] <- fread(config$procdatapath%&%"aggr_footprints/"%&%config$year_io%&%
                         "/by_foodcat/CBF_final_"%&%c%&%"_cat_"%&%i%&%".csv",
                             header = TRUE, data.table = FALSE) %>% 
        filter(s_group %in% c("NOx", "NH3", "N")) %>% 
        
        # we only compute costs within the EU as no SCN for other countries available
        filter(in_EU == 1) %>%
        # add social (unit) cost
        left_join(scn, by = c("imp_reg" = "country", "s_group"="Stressor")) %>% 
        # compute total social cost in category
        mutate(SCN = value*SCN_EUR_kg) %>%
        group_by(s_group) %>%
        summarize(SCN = sum(SCN, na.rm=T)) %>% ungroup() %>%
        # add country and category info
        mutate(country = c, category = i)
    
      }# end category loop
  }# end country loop
  
  # List to df
  fp_scn <- fp_scn %>% bind_rows() 
  
  # (2) Load percentage reduction for country c for category i due to policy p
  pol_c <- list()
  x <- 0
  
  for(pol in c("VAT_increase", policy)){

    for (c in countries){
      
      countryname <- EU27[EU27$geo == c,]$name
      
      x <- x+1
      
      pol_c[[x]] <- fread("../build/data/policies/"%&%pol%&%"/"%&%configpath%&%
                            "/reduction_"%&%c%&%".csv",
                          data.table = FALSE) %>%
        transmute(category, cat_no, 
                  footprint_reduction_rel = round(footprint_reduction_rel, 6),
                  policy = pol, 
                  country = c, 
                  countryname = countryname) %>% distinct()
      
    } # end country loop
  }# end policy loop
  
  pol_c <- pol_c %>% bind_rows() %>% distinct()
  
  # (3) Compute reduction in N, NH3, NOx due to footprint reductions
  N_final <- fp_scn %>%
    mutate(country = ifelse(country == "GR", "EL", country)) %>% 
    left_join(pol_c,
              by = c("category" = "cat_no", "country"),
              relationship = "many-to-many") %>% 
    mutate(social_value_of_reduction = SCN*(-footprint_reduction_rel)) %>% 
    filter(!is.na(policy)) %>%
    group_by(policy,s_group) %>%
    summarize(value_MEUR = sum(social_value_of_reduction, na.rm = T)/1e6) %>%
    mutate(plotgroup = "Nitrogen")
  
  
  # (4) Compute reduction in N, NH3, NOx due to footprint reductions by country
  N_final_bycountry <- fp_scn %>%
    mutate(country = ifelse(country == "GR", "EL", country)) %>% 
    left_join(pol_c,
              by = c("category" = "cat_no", "country"),
              relationship = "many-to-many") %>% 
    mutate(social_value_of_reduction = SCN*(-footprint_reduction_rel)) %>% 
    filter(!is.na(policy)) %>%
    group_by(policy,s_group, country) %>%
    summarize(value_MEUR = sum(social_value_of_reduction, na.rm = T)/1e6) %>%
    mutate(plotgroup = "Nitrogen")
  
# _ Social cost of Phosphorus --------------------------------------------
  
  # Load SCP for EU countries (in 2020 EURO)
  # Source: Tabelle 25 in https://www.umweltbundesamt.de/sites/default/files/medien/1410/publikationen/2020-12-21_methodenkonvention_3_1_kostensaetze.pdf
  scp_2020 <- 153.5
  
  # Load euro inflation rate of 2019 in EU27 (to convert 2020 euro to 2019 euro)
  # https://de.statista.com/statistik/daten/studie/156285/umfrage/entwicklung-der-inflationsrate-in-der-eu-und-der-eurozone/
  euro_inflation_rate_2020 <- 0.7
  
  # Convert SCP to euro_2019
  scp = scp_2020/(1+euro_inflation_rate_2020/100)
  
  # Compute social cost of phosphorus footprints for country c and category i
  fp_scp <- list()
  x <- 0
  
  for(c in countries){
    
    if(c == "EL"){ c <- "GR"}
    
    for (i in catexplain$category_code_new){
      
      x <- x+1
      
      fp_scp[[x]] <- fread(config$procdatapath%&%"aggr_footprints/"%&%config$year_io%&%
                             "/by_foodcat/CBF_final_"%&%c%&%"_cat_"%&%i%&%".csv",
                           header = TRUE, data.table = FALSE) %>%
        filter(shortname %in% c("Phosphorus")) %>%
        
        # we only compute costs within the EU 
        filter(in_EU == 1) %>%
        # compute total social cost in category
        mutate(SCP = value*scp) %>%
        group_by(Stressor) %>%
        summarize(SCP = sum(SCP, na.rm = T)) %>% ungroup() %>%
        # add country and category info
        mutate(country = c, category = i)
      
    }# end category loop
  }# end country loop
  
  # List to df
  fp_scp <- fp_scp %>% bind_rows() 
  
  # Load percentage reduction for country c for category i due to policy p
  pol_c <- list()
  x <- 0
  
  for(pol in c("VAT_increase", policy)){
    
    for (c in countries){
      
      countryname <- EU27[EU27$geo == c,]$name
      
      x <- x+1
      
      pol_c[[x]] <- fread("../build/data/policies/"%&%pol%&%"/"%&%configpath%&%
                            "/reduction_"%&%c%&%".csv",
                          data.table = FALSE) %>%
        transmute(category, cat_no, 
                  footprint_reduction_rel = round(footprint_reduction_rel, 6),
                  policy                  = pol, 
                  country                 = c, 
                  countryname             = countryname) %>%
        distinct()
      
    } # end country loop
  }# end policy loop
  
  pol_c <- pol_c %>% bind_rows() %>% distinct()
  
  # Compute reduction in P due to footprint reductions
  P_final <- fp_scp %>% 
    mutate(country = ifelse(country == "GR", "EL", country)) %>%  
    left_join(pol_c, by = c("category" = "cat_no", "country"),
                       relationship = "many-to-many") %>%
    mutate(social_value_of_reduction = SCP*(-footprint_reduction_rel)) %>% 
    filter(!is.na(policy)) %>% 
    group_by(policy) %>%
    summarize(value_MEUR = sum(social_value_of_reduction, na.rm=T)/1000000) %>%
    mutate(plotgroup = "Phosphorus",
           s_group = "Phosphorus")
  
  
  # Compute reduction in P due to footprint reductions by country
  P_final_bycountry <- fp_scp %>% 
    mutate(country = ifelse(country == "GR", "EL", country)) %>%
    left_join(pol_c, by = c("category" = "cat_no", "country"),
              relationship = "many-to-many") %>% 
    mutate(social_value_of_reduction = SCP*(-footprint_reduction_rel)) %>% 
    filter(!is.na(policy)) %>%
    group_by(policy, country) %>%
    summarize(value_MEUR = sum(social_value_of_reduction, na.rm=T)/1000000) %>%
    mutate(plotgroup = "Phosphorus",
           s_group = "Phosphorus")
  

# _ Table: Aggregate benefits ---------------------------------------------

  # Create directory if necessary
  ifelse(!dir.exists(file.path("../build/tables/"%&%
                                 substring(configpath, 1, nchar(configpath)-5)%&%
                                 "/")
  ), 
  dir.create(file.path("../build/tables/"%&%
                         substring(configpath, 1, nchar(configpath)-5)%&%"/"),
             recursive = TRUE),
  FALSE)
  
  # Save table
  write(
    GHG_final %>%
      bind_rows(N_final) %>%
      bind_rows(P_final) %>% 
      mutate(value_MEUR = round(value_MEUR)) %>%
      pivot_wider(names_from = policy, 
                  values_from = value_MEUR) %>%
      dplyr::select(Benefit = plotgroup, Stressor = s_group, 
             `VAT reform` = VAT_increase,
             `GHG emission price` = `tax_GHG emissions`) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~"Total"))) %>%
      kable(format     = "latex",
            escape     = TRUE,
            booktabs   = TRUE,
            linesep    = "",
            row.names  = FALSE,
            align      = "c") %>%
      kable_styling(font_size = 8) %>%
      row_spec(0, bold = TRUE) %>%
      kable_paper(),
    file ="../build/tables/"%&%substring(configpath, 1, nchar(configpath)-5)%&%
      "/benefits_table.tex") 

  

# _ Table: Benefits by country --------------------------------------------

  GHG_final_bycountry_save <- GHG_final_bycountry %>% 
    group_by(policy, demandcountry, plotgroup) %>% 
    dplyr::summarise(value_MEUR=sum(value_MEUR)) %>% rename(country=demandcountry) %>% ungroup()
  N_final_bycountry_save <- N_final_bycountry %>%
    group_by(policy, country, plotgroup) %>% 
    dplyr::summarise(value_MEUR=sum(value_MEUR)) %>% ungroup()
  P_final_bycountry_save <- P_final_bycountry %>%
    group_by(policy, country, plotgroup) %>% 
    dplyr::summarise(value_MEUR=sum(value_MEUR)) %>% ungroup()
  
  envbenefits_by_country <- GHG_final_bycountry_save %>%
    bind_rows(N_final_bycountry_save) %>%
    bind_rows(P_final_bycountry_save)
  
write.csv(envbenefits_by_country, 
          "../build/tables/"%&%substring(configpath, 1, nchar(configpath)-5)%&%
            "/benefits_table_bycountry.csv",
          row.names = FALSE)  

# _ Table: Social cost assumptions ----------------------------------------

# Table of social cost assumptions
table_social_cost <- 
  # Social cost of GHGs
  sc_ghg[sc_ghg$discount_rate==2, 
         c("GHG", "value_euro_2019")] %>%
  dplyr::transmute(Unit = "Global", 
                   Stressor = GHG, 
                   Value = round(value_euro_2019,1)) %>%
  # Social cost of phosphorus
  bind_rows(
    data.frame(Unit = "EU", 
               Stressor = "Phosphorus",
               Value = round(scp,1))
  ) %>%
  # Social cost of nitrogen
  bind_rows(
    scn %>% transmute(Unit = country,
                      Stressor,
                      Value = SCN_EUR_kg)
  )

# Save as .tex
write(table_social_cost %>%
        kbl(format     = "latex",
            escape     = TRUE,
            booktabs   = TRUE,
            longtable = TRUE, 
            linesep    = "",
            caption    = "Social cost estimates",
            label      = "socialcost",
            row.names  = FALSE,
            align      = "c") %>%
        kable_styling(font_size = 8, latex_options = c("hold_position", "repeat_header")) %>%
        row_spec(0, bold = TRUE) %>%
        column_spec(1:3, width = "12em") %>%
        kable_paper(),
      file   = "../build/tables/social_costs.tex",
      append = FALSE)

# _____________________________________------------------------------------
# SUMMARY: Waterfall plots ------------------------------------------------

 # Combine results
 final <- GHG_final %>%
   bind_rows(N_final) %>%
   bind_rows(P_final) %>%
   bind_rows(welfare_final) %>%
   bind_rows(tax_inc_final) %>%
   dplyr::transmute(plotgroup, s_group, policy, value_BEUR=value_MEUR/1e3)

 # Load plotting functions
 source("99_functions/plot_functions.R")
 
# _ Waterfall plot --------------------------------------------------------

 # Waterfall plot: Total values
 waterfall_plot(type="total")
 ggsave("../build/figures/"%&%configpath%&%"/waterfall_benefits_costs.pdf", 
        width = 12, height = 6)
 
 # Waterfall plot: Per capita values
 waterfall_plot(type="percapita")
 ggsave("../build/figures/"%&%configpath%&%"/waterfall_benefits_costs_pc.pdf", 
        width = 12, height = 6)
 
 # Waterfall plot: Per household values
 waterfall_plot(type="perhousehold")
 ggsave("../build/figures/"%&%configpath%&%"/waterfall_benefits_costs_ph.pdf", 
        width = 12, height = 6)

# _ Waterfall plot simplified ---------------------------------------------

 # Simplified waterfall plot: Total values
 waterfall_plot_simple(type="total")
 
 # Simplified waterfall plot: Per capita values
 waterfall_plot_simple(type="percapita")
 ggsave("../build/figures/"%&%configpath%&%"/waterfall_benefits_costs_pc_simple.pdf", 
        width = 7, height = 4)
 
 # Simplified waterfall plot:Per household values
 waterfall_plot_simple(type="perhousehold")
 ggsave("../build/figures/"%&%configpath%&%"/waterfall_benefits_costs_ph_simple.pdf", 
        width = 7, height = 4)
 
# _ Waterfall plot simplified: German -------------------------------------
 waterfall_plot_simple(type="perhousehold", language="german")
 ggsave("../build/figures/"%&%configpath%&%"/waterfall_benefits_costs_ph_simple_DE.pdf", 
        width = 7, height = 7)
 
# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------
 