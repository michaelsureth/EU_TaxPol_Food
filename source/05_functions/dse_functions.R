# File info --------------------------------------------------------------------

# File:    Functions for demand system analysis
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# HBS data editing -------------------------------------------------------------

var_indicator <- function(df){
  
  #' @name var_indicator
  #' @title Variable indicator 
  #' @description This function indicates which variables in the dataset
  #' are filled, measured by whether there is any variance across observations
  #' @param df
  #' @return df_return
  
  df_return <- df %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor,    as.numeric) %>%
    replace(is.na(.), 0) %>%
    dplyr::summarise(across(where(is.numeric),
                            ~ var(.x, na.rm = TRUE))) %>%
    t() %>% as.data.frame() %>%
    mutate(V2 = ifelse(V1 > 0,
                       1,
                       0)) %>%
    dplyr::select(V2) %>%
    t() %>% as.data.frame() %>%
    dplyr::select(order(colnames(.)))
  
  return(df_return)
}



prepare_HBS_data <- function(df_hh, df_hm){
  
  #' @name prepare_HBS_data
  #' @title Prepare HBS data
  #' @param df_hh HBS household dataset (expenditure information)
  #' @param df_hm HBS household dataset (household member information)
  #' @return df_return
  
  # (A) Household data cleaning 
  
  # Available Variables: HA..    Sample information;
  #                      HB-HD.. Household socio-demographics;
  #                      HI..    Income;
  #                      HQ..    Quantities;
  #                      EUR..   Expenditures
  # Household information to add from household data: 
  # - weight, density category
  # - household size (modified OECD scale), number of children
  # - total annual income
  
  df_hh_edit <- df_hh %>%
    rename(HA04 = matches("HA04")) %>%
    select(c("HA04", "COUNTRY" , "HA09", "HA10", "EUR_HH099", "HB062"),
           starts_with("EUR_HE0"),
           starts_with("HQ011"),
           starts_with("HB05")) %>%
    # Note: we drop "+" (top-coding) from child and adult count variables
    mutate_at(vars(HB051, HB052, HB053, HB054, HB056, HB057),
              ~ as.numeric(gsub( "\\D", "", .))) %>% 
    rename(hh_id               = HA04) %>%
    dplyr::mutate(country      = COUNTRY,
                  hh_wgt       = HA10,
                  dens_cat     = HA09,
                  hh_sizeoecdm = HB062,
                  hh_children  = rowSums(cbind(HB051, HB052, HB053),
                                         na.rm = TRUE),
                  # Note: income is missing in IT and set to zero
                  hh_inctot    = ifelse(country == "IT",
                                        0,
                                        EUR_HH099),
                  hh_exptot    = EUR_HE00,
                  hh_norig     = nrow(df_hh))
  
  # (B) Household member data cleaning 
  
  # Household information to add from household member data
  # - household head: sex, age
  
  # _ (B1) Keep one obs. per household: household head (or representative)
  # we count the number of households by country to make sure we have the
  # equivalent number of household heads
  
  n_hh    <- nrow(df_hh)
  n_heads <- df_hm %>% dplyr::filter(MB05 == 1) %>% nrow()
  
  if(identical(n_hh, n_heads)){
    df_hm_edit <- df_hm %>%
      rename(MA04 = matches("MA04")) %>%
      dplyr::filter(MB05 == 1)
  }else{
    # If there is no household head defined, we determine the household head to be
    # the person with the highest income (in line with EUROSTAT Definition: "the  
    # member that contributes most to the household consumption budget.")
    df_hm_edit <- df_hm %>%
      rename(MA04 = matches("MA04")) %>%
      group_by(MA04) %>%
      filter(MB03_Recoded_5Classes != "0_14") %>%
      arrange(MA04, desc(EUR_MF099)) %>% 
      slice(1) %>% ungroup()
  }
  
  # _ (B2) Define variables to keep for each household head
  # MC01 
  if(all(c("MA04", "MB02", "MB03_Recoded_5Classes", "MB03_Recoded_5YearsClasses") %in%
         names(df_hm_edit))){
    df_hm_edit <- df_hm_edit %>%
      dplyr::select(hh_id       = MA04, 
                    hh_sexhead  = MB02,
                    hh_agehead  = MB03_Recoded_5Classes)
  }else{
    stop("Error: Check values in hm dataframe")
  }
  
  # _ (B3) Merge household information from hh and hm 
  df_edit <- full_join(df_hh_edit,
                       df_hm_edit,
                       by = "hh_id") %>%
    unite(hh_id,
          c(country, "hh_id"),
          sep = "_",
          remove = FALSE) %>%
    dplyr::select(hh_id, everything()) %>%
    arrange(hh_id)
  
  # _ (B4) Recode sociodemographics to dummy variables
  df_edit <- df_edit %>%
    # median income
    {if(!is.character(.$hh_inctot))
      mutate(., hh_inctot_medianwtd = spatstat.geom::weighted.median(hh_inctot,
                                                                     hh_wgt))
      else 
        mutate(., hh_inctot_medianwtd = NA)} %>%
    # Household characteristics (factor variables)
    mutate(dens_cat        = as.factor(dens_cat),
           hh_inctot       = as.numeric(hh_inctot),
           hh_exptot       = as.numeric(hh_exptot),
           hh_sexhead      = as.factor(hh_sexhead),
           hh_agehead      = as.factor(hh_agehead),
           # Household characteristics - subset as binary/numeric variables
           z1 = ifelse(dens_cat == 1,
                       1,
                       ifelse(dens_cat == 2 | dens_cat == 3,
                              0,
                              NA)), # urban
           z2 = ifelse(hh_sexhead == 2,
                       1,
                       ifelse(hh_sexhead == 1,
                              0,
                              NA)), # female
           z3 = ifelse(hh_agehead == "45_59" | hh_agehead == "60_Inf",
                       1,
                       ifelse(hh_agehead == "0_14" |
                                hh_agehead == "15_29" |
                                hh_agehead == "30_44",
                              0,
                              NA)), # above 45
           z4 = ifelse(hh_inctot >= hh_inctot_medianwtd,
                       1,
                       ifelse(hh_inctot < hh_inctot_medianwtd,
                              0,
                              NA)), # high income
           z5 = ifelse(hh_children > 0 & hh_children < 20,
                       1,
                       ifelse(hh_children == 0,
                              0,
                              NA))) %>% # children 
    dplyr::select(hh_id, order(colnames(.))) %>% select(-hh_inctot_medianwtd)
  
  # (C) Expenditure and quantity data cleaning 
  
  # Issue: In the original data, 0s may indicate 0 expenditures of households OR
  # the category not having being asked
  # -> we want keep all observations in the data set and distinguish between 
  #    (1) asked but 0 -> 0 and
  #    (2) not asked -> NA
  # -> we use var_indicator function to determine whether item has been asked
  # ASSUMPTION: no variance in expenditures across households = not asked
  
  # Identify expenditure variables without any variance
  item_exp_NA <- var_indicator(df_hh_edit) %>%
    select(starts_with("EUR_HE01")) %>% 
    t() %>% as.data.frame() %>%
    rownames_to_column() %>%
    filter(V2 == 0) %>%
    select(rowname) %>% pull()
  
  # Define zeros as NA if variable has not been asked
  df_exp <- df_edit %>%
    select(c("hh_id", "country", starts_with("EUR_HE01"))) %>%
    pivot_longer(c(-hh_id, -country),
                 names_to  = "item_code",
                 values_to = "expenditures") %>%
    dplyr::mutate(expenditures = ifelse(item_code %in% item_exp_NA,
                                        NA,
                                        expenditures)) %>%
    dplyr::mutate(item_code    = gsub("EUR_HE",
                                      "",
                                      item_code))
  
  
  # (D) Quantities data cleaning 
  
  # Identify quantity variables without any variance
  item_quan_NA <- var_indicator(df_hh_edit) %>%
    select(starts_with("HQ01")) %>% 
    t() %>% as.data.frame() %>%
    rownames_to_column() %>%
    filter(V2 == 0) %>%
    select(rowname) %>% pull()
  
  # Define zeros as NA if variable has not been asked
  df_quan <- df_edit %>%
    select(c("hh_id", "country", starts_with("HQ01"))) %>%
    pivot_longer(c(-hh_id, -country),
                 names_to  = "item_code",
                 values_to = "quantity") %>%
    dplyr::mutate(quantity  = ifelse(item_code %in% item_quan_NA,
                                     NA,
                                     quantity),
                  item_code = gsub("HQ",
                                   "",
                                   item_code),
                  # Convert unit of food item "eggs" from pieces to kg
                  quantity  = ifelse(item_code == "01147",
                                     quantity*0.062,
                                     quantity))
  
  # Merge household and household member data with cleaned expenditure (C) and
  # quantity (D) datasets
  df_return <- df_edit %>%
    select(starts_with("hh_"), country, dens_cat, starts_with("z")) %>%
    full_join(df_exp,
              by = c("hh_id", "country")) %>%
    full_join(df_quan,
              by = c("hh_id", "country", "item_code")) %>%
    # keep only food item level rows (i.e., 5 digits - four-digit does
    # not have quantity data for some countries and categories)
    filter(nchar(item_code) == 5) %>%
    # exclude non-food items (e.g., beverages)
    filter(grepl("^(011)[[:digit:]]+", item_code)) %>%
    relocate(hh_id, hh_wgt, country, dens_cat, hh_children, hh_sizeoecdm,
             hh_inctot, hh_exptot, hh_sexhead, hh_agehead, item_code,
             expenditures, quantity, starts_with("z"), hh_norig) %>%
    arrange(hh_id, item_code)
  
  return(df_return)
}



# DE (EVS) data editing --------------------------------------------------------

prepare_DE_data <- function(df){
  
  #' @name prepare_DE_data
  #' @title Prepare EVS data for Germany
  #' @param df EVS dataset
  #' @return df_return_list
  
  # (A) Household data and household member data cleaning 
  
  df_hh_edit <- df %>% 
    
    # we need the following information on hh composition
    # hh_children: number of individuals aged 15 or less
    # hh_adults: number of individuals aged 14 or more for OECD scale
    # hh_u14: number of individuals aged less than 14 for OECD scale
    
    # hh_adults: number of individuals aged 14 or more for OECD scale
    mutate(adult1 = ifelse(2018-EF8u3  >= 14, 1, ifelse(2018-EF8u3  < 14, 0, NA)),
           adult2 = ifelse(2018-EF9u3  >= 14, 1, ifelse(2018-EF9u3  < 14, 0, NA)),
           adult3 = ifelse(2018-EF10u3 >= 14, 1, ifelse(2018-EF10u3 < 14, 0, NA)),
           adult4 = ifelse(2018-EF11u3 >= 14, 1, ifelse(2018-EF11u3 < 14, 0, NA)),
           adult5 = ifelse(2018-EF12u3 >= 14, 1, ifelse(2018-EF12u3 < 14, 0, NA)),
           adult6 = ifelse(2018-EF13u3 >= 14, 1, ifelse(2018-EF13u3 < 14, 0, NA)),
           adult7 = ifelse(2018-EF14u3 >= 14, 1, ifelse(2018-EF14u3 < 14, 0, NA)),
           adult8 = ifelse(2018-EF15u3 >= 14, 1, ifelse(2018-EF15u3 < 14, 0, NA))
    ) %>% 
    
    # hh_u14: number of individuals aged less than 14 for OECD scale
    mutate(u141 = ifelse(2018-EF8u3  < 14, 1, ifelse(2018-EF8u3  >= 14, 0, NA)),
           u142 = ifelse(2018-EF9u3  < 14, 1, ifelse(2018-EF9u3  >= 14, 0, NA)),
           u143 = ifelse(2018-EF10u3 < 14, 1, ifelse(2018-EF10u3 >= 14, 0, NA)),
           u144 = ifelse(2018-EF11u3 < 14, 1, ifelse(2018-EF11u3 >= 14, 0, NA)),
           u145 = ifelse(2018-EF12u3 < 14, 1, ifelse(2018-EF12u3 >= 14, 0, NA)),
           u146 = ifelse(2018-EF13u3 < 14, 1, ifelse(2018-EF13u3 >= 14, 0, NA)),
           u147 = ifelse(2018-EF14u3 < 14, 1, ifelse(2018-EF14u3 >= 14, 0, NA)),
           u148 = ifelse(2018-EF15u3 < 14, 1, ifelse(2018-EF15u3 >= 14, 0, NA))
    ) %>%
    
    # hh_children: number of individuals aged 15 or less
    mutate(a15orless1 = ifelse(2018-EF8u3  <= 15, 1, ifelse(2018-EF8u3  > 15, 0, NA)),
           a15orless2 = ifelse(2018-EF9u3  <= 15, 1, ifelse(2018-EF9u3  > 15, 0, NA)),
           a15orless3 = ifelse(2018-EF10u3 <= 15, 1, ifelse(2018-EF10u3 > 15, 0, NA)),
           a15orless4 = ifelse(2018-EF11u3 <= 15, 1, ifelse(2018-EF11u3 > 15, 0, NA)),
           a15orless5 = ifelse(2018-EF12u3 <= 15, 1, ifelse(2018-EF12u3 > 15, 0, NA)),
           a15orless6 = ifelse(2018-EF13u3 <= 15, 1, ifelse(2018-EF13u3 > 15, 0, NA)),
           a15orless7 = ifelse(2018-EF14u3 <= 15, 1, ifelse(2018-EF14u3 > 15, 0, NA)),
           a15orless8 = ifelse(2018-EF15u3 <= 15, 1, ifelse(2018-EF15u3 > 15, 0, NA))
    ) %>%
    mutate(hh_children  = rowSums(as.matrix(cbind(.[startsWith(names(.), "a15"  )])), na.rm = TRUE),
           hh_adults    = rowSums(as.matrix(cbind(.[startsWith(names(.), "adult")])), na.rm = TRUE),
           hh_u14       = rowSums(as.matrix(cbind(.[startsWith(names(.), "u14"  )])), na.rm = TRUE)) %>% 
    
    dplyr::transmute(hh_id        = "DE_"%&%EF3,
                     hh_wgt       = EF31,
                     country      = "DE",
                     dens_cat     = EF5,
                     hh_children,
                     hh_sizeoecdm = 1 + 0.5*(hh_adults-1) + 0.3*hh_u14,
                     # income: quartalsabrechnung -> *4 for annual net income
                     hh_inctot    = ifelse(EF30 == -1781, NA, 4*EF30),
                     # Note: total expenditures (EF89) from GF3 cannot be merged 
                     hh_exptot    = NA,
                     # Note: 5 households with missing income -> dropped later
                     hh_sexhead   = EF8u2,
                     # categorize age
                     hh_agehead_help = 2018-EF8u3,
                     hh_agehead = with(., case_when((hh_agehead_help>0  & hh_agehead_help<=14) ~ "0_14",
                                                    (hh_agehead_help>14 & hh_agehead_help<=29) ~ "15_29",
                                                    (hh_agehead_help>29 & hh_agehead_help<=44) ~ "30_44",
                                                    (hh_agehead_help>44 & hh_agehead_help<=59) ~ "45_59",
                                                    (hh_agehead_help>5)                        ~ "60_Inf",
                                                    TRUE ~ "NA")),
                     hh_norig     = nrow(df)
    ) %>%
    arrange(hh_id)
  
  # _ (B) Recode sociodemographics to dummy variables
  df_edit <- df_hh_edit %>%
    # median income
    mutate(hh_inctot_medianwtd = spatstat.geom::weighted.median(hh_inctot,
                                                                hh_wgt)) %>%
    # Household characteristics (factor variables)
    mutate(dens_cat        = as.factor(dens_cat),
           hh_sexhead      = as.factor(hh_sexhead),
           hh_agehead      = as.factor(hh_agehead),
           # Household characteristics - subset as binary/numeric variables
           z1 = ifelse(dens_cat == 1,
                       1,
                       ifelse(dens_cat == 2 | dens_cat == 3,
                              0,
                              NA)), # urban
           z2 = ifelse(hh_sexhead == 2,
                       1,
                       ifelse(hh_sexhead == 1,
                              0,
                              NA)), # female
           z3 = ifelse(hh_agehead == "45_59" | hh_agehead == "60_Inf",
                       1,
                       ifelse(hh_agehead == "0_14" |
                                hh_agehead == "15_29" |
                                hh_agehead == "30_44",
                              0,
                              NA)), # above 45
           z4 = ifelse(hh_inctot >= hh_inctot_medianwtd,
                       1,
                       ifelse(hh_inctot < hh_inctot_medianwtd,
                              0,
                              NA)), # high income
           z5 = ifelse(hh_children > 0 & hh_children < 20,
                       1,
                       ifelse(hh_children == 0,
                              0,
                              NA))) %>% # children 
    dplyr::select(hh_id, order(colnames(.))) %>% select(-hh_inctot_medianwtd,)
  
  # (C) Expenditure data cleaning
  liters <- c("EF139", "EF140", "EF144", "EF224")
  pieces <- tibble(item_code   = c("EF142", "EF69"),
                   gramm_piece = c(62, 250))
  
  df_exp <- df %>% 
    mutate(hh_id = paste0("DE_", EF3)) %>%
    dplyr::select(hh_id, EF58u1:EF249u2) %>% 
    pivot_longer(cols      = -c(hh_id),
                 names_to  = "item",
                 values_to = "value") %>%
    separate_wider_delim(item, delim = "u", names = c("item_code", "typeno")) %>%
    mutate(type = ifelse(typeno == 1,
                         "quantity",
                         ifelse(typeno == 2,
                                "expenditure",
                                NA))) %>%
    # Unit conversion
    mutate(
      # Convert quantities measured in units into grams
      value = ifelse(type == "quantity" & item_code == pieces$item_code[1],
                     value*pieces$gramm_piece[1],
                     ifelse(type == "quantity" & item_code == pieces$item_code[2],
                            value*pieces$gramm_piece[2],
                            value)),
      # Convert quantities in g/month into kg/year
      value = ifelse(type == "quantity" & !(item_code %in% liters),
                     (value/1000)*12,
                     # Convert quantities in l/month and expenditures in 
                     # EUR/month into l/year and EUR/year
                     value*12)) %>%
    pivot_wider(id_cols     = c("hh_id", "item_code"), 
                names_from  = type, 
                values_from = value)
  
  # Merge (C) with household characteristics
  df_return <- df_exp %>%
    full_join(df_edit, by = "hh_id") %>%
    rename(expenditures = expenditure) %>%
    select(-hh_agehead_help) %>%
    # drop five observations with negative household income
    filter(!is.na(hh_inctot)) %>%
    relocate(hh_id, hh_wgt, country, dens_cat, hh_children, hh_sizeoecdm,
             hh_inctot, hh_sexhead, hh_agehead, item_code, expenditures,
             quantity, starts_with("z"), hh_norig)
  
  return(df_return)
}



# AT (K14) data editing --------------------------------------------------------

prepare_AT_data <- function(df_hh, df_hm, df_exp){
  
  #' @name prepare_AT_data
  #' @title Prepare Konsumerhebung data for Austria
  #' @param df_hh Konsumerhebung 2014/15 household data
  #' @param df_hm Konsumerhebung 2014/15 household member data
  #' @param df_exp Konsumerhebung 2014/15 household expenditure data
  #' @return df_return
  
  # (A) Household data cleaning 
  
  # we keep household ID, NUTS1 region, population density level,
  # household weight, net income and original number of households
  df_hh_edit <- df_hh %>% 
    dplyr::transmute(hh_id        = paste0("AT_", OB), 
                     hh_wgt       = HGEW,
                     dens_cat     = DICHT,
                     hh_inctot    = 12*HHEK_INKL,  # monthly to annual
                     hh_norig     = nrow(df_hh))
  
  
  # (B) Household member data cleaning 
  
  # _ (B1) Keep one obs. per household: household head (or representative)
  # we count the number of households to make sure we have the
  # equivalent number of household heads
  
  n_hh    <- nrow(df_hh)
  n_heads <- df_hm %>% dplyr::filter(STELLHV == 1) %>% nrow()
  
  if(identical(n_hh, n_heads)){
    df_hm_edit <- df_hm %>%
      dplyr::filter(STELLHV == 1)
  } else{
    break()
  }
  
  # _ (B2.1) Define variables to keep for each household head
  df_hm_b2.1 <- df_hm_edit %>%
    dplyr::transmute(hh_id           = "AT_"%&%OB,
                     hh_sexhead      = geschl,
                     # categorize age
                     hh_agehead = with(.,case_when((alt > 0   & alt < 15) ~ "0_14", 
                                                   (alt >= 15 & alt < 30) ~ "15_29",
                                                   (alt >= 30 & alt < 45) ~ "30_44",
                                                   (alt >= 45 & alt < 60) ~ "45_59",
                                                   (alt >= 60)            ~ "60_Inf",
                                                   TRUE                   ~ "NA"))) 
  
  # _ (B2.2) Add variables not included in data_AT_hh but data_AT_hm instead
  df_hm_b2.2 <- df_hm %>%
    group_by(OB) %>%
    # compute hh_sizeoecdm
    mutate(hh_sizeoecdm = sum(PAQ1)) %>% 
    # number of children
    mutate(hh_children_help = ifelse(alt <= 15,
                                     1, 
                                     ifelse(alt > 15,
                                            0,
                                            NA))) %>%
    mutate(hh_children = sum(hh_children_help, na.rm = TRUE)) %>%
    ungroup() %>%
    # keep only heads
    dplyr::filter(STELLHV == 1) %>%
    dplyr::transmute(hh_id   = "AT_"%&%OB,
                     hh_sizeoecdm,
                     hh_children)
  
  # _ (B3) Merge household information from hh and hm 
  df_edit <- df_hh_edit %>%
    full_join(df_hm_b2.1, by = "hh_id") %>%
    full_join(df_hm_b2.2, by = "hh_id") %>%
    arrange(hh_id)
  
  # _ (B4) Recode sociodemographics to dummy variables
  df_edit <- df_edit %>%
    # median income
    mutate(hh_inctot_medianwtd = spatstat.geom::weighted.median(hh_inctot,
                                                                hh_wgt)) %>%
    # Household characteristics (factor variables)
    mutate(dens_cat        = as.factor(dens_cat),
           hh_sexhead      = as.factor(hh_sexhead),
           hh_agehead      = as.factor(hh_agehead),
           # Household characteristics - subset as binary/numeric variables
           z1 = ifelse(dens_cat == 1,
                       1,
                       ifelse(dens_cat == 2 | dens_cat == 3,
                              0,
                              NA)), # urban
           z2 = ifelse(hh_sexhead == 2,
                       1,
                       ifelse(hh_sexhead == 1,
                              0,
                              NA)), # female
           z3 = ifelse(hh_agehead == "45_59" | hh_agehead == "60_Inf",
                       1,
                       ifelse(hh_agehead == "0_14" |
                                hh_agehead == "15_29" |
                                hh_agehead == "30_44",
                              0,
                              NA)), # above 45
           z4 = ifelse(hh_inctot >= hh_inctot_medianwtd,
                       1,
                       ifelse(hh_inctot < hh_inctot_medianwtd,
                              0,
                              NA)), # high income
           z5 = ifelse(hh_children > 0 & hh_children < 20,
                       1,
                       ifelse(hh_children == 0,
                              0,
                              NA))) %>% # children 
    dplyr::select(hh_id, order(colnames(.))) %>% select(-hh_inctot_medianwtd,)
  
  # (C) Expenditure data cleaning 
  # Note: there are no NAs as row only exists if household consumed item
  
  # _ Food expenditures in categories (but still disaggregated)
  df_exp_cats_wo_meat <- df_exp %>%
    # create household id
    dplyr::mutate(hh_id = "AT_"%&%OB) %>%
    # Compute total expenditures
    group_by(hh_id) %>%
    dplyr::mutate(hh_exptot = sum(12*ausgHH)) %>%     # monthly to annual
    ungroup() %>%
    # keep aggregate online + offline
    dplyr::filter(online == "00") %>%
    # merge COICOP codes 
    mutate(coicop = gsub("0*$", "", paste0(c1, c2, as.numeric(c3), c4, c5, c6))) %>% 
    mutate(level = nchar(coicop)) %>% 
    # keep only food products
    # note: 7 households do not have food expenditures at all
    #       -> these are not included in df_exp
    filter(grepl("^011", coicop)) %>%
    left_join(map_K14_item_codes, by = c("coicop" = "item_code_orig"))  %>%
    rename(category = item_code_AT) %>%
    # note: food items only coincide with HBS up to level 4
    #       -> we match based on level 4
    filter(level == 4) %>%
    group_by(hh_id, category) %>%
    dplyr::summarize(expenditures = sum(12*ausgHH)) %>%    # monthly to annual
    ungroup() %>%
    complete(category, hh_id) %>%
    mutate_at(vars(expenditures), ~replace(., is.na(.), 0)) %>%
    arrange(hh_id, category)
  
  df_exp_cats <- df_exp_cats_wo_meat %>%
    filter(category == 6789) %>%
    uncount((9-6)+1) %>%
    rename(expenditures_meat = expenditures) %>%
    group_by(hh_id) %>%
    mutate(category = (6-1)+row_number()) %>%
    ungroup() %>%
    full_join(df_exp_cats_wo_meat %>%
                filter(category != 6789))
  
  # add total expenditures
  df_exp_total <- df_exp %>%
    # keep aggregate online + offline of total expenditures
    dplyr::filter(online == "00",
                  COICOP_TEXT == "VERBRAUCHSAUSGABEN insgesamt") %>%
    transmute(hh_id = "AT_"%&%OB,
              hh_exptot = 12*ausgHH) 
  
  # merge all datasets from (A), (B) with (C)
  df_return <- df_exp_cats %>%
    full_join(df_edit, by = "hh_id") %>%
    full_join(df_exp_total, by="hh_id") %>%
    # remove 7 households that are not included in extk14_ausg_det.sas7bdat
    # but in extk14_hhvar.txt and EXTK14_PERSVAR.txt
    filter(!is.na(category)) %>%
    mutate(country  = "AT",
           quantity = NA) %>%
    arrange(hh_id, category) %>%
    relocate(hh_id, hh_wgt, country, dens_cat, hh_children, hh_sizeoecdm, 
             hh_inctot, hh_exptot, hh_sexhead, hh_agehead, category, 
             expenditures, expenditures_meat, starts_with("z"), hh_norig)
  
  return(df_return)
}



# EASI estimation --------------------------------------------------------------

compute_cdf_pdf <- function(i, data, s){
  
  #' @name compute_cdf_pdf
  #' @title Compute cdf and pdf for censoring
  #' @param i Food item code 
  #' @param data Long household data set
  #' @param s Vector of household controls
  #' @return data_return
  
  # create dummy if expenditure > 0 by category 
  data_slct_pre <- data %>%
    mutate(category         = as.numeric(gsub( "\\D", "", category)),
           expenditures_dum = ifelse(expenditures > 0,
                                     1,
                                     ifelse(expenditures == 0,
                                            0,
                                            NA))) %>%
    # create item specific data set 
    filter(category == i) 
  
  # drop NA households
  data_slct <- data_slct_pre %>% filter(!is.na(expenditures)) 
  
  if(!(nrow(data_slct)==nrow(data_slct_pre))){
    print_color(paste0((nrow(data_slct_pre)-nrow(data_slct))," NAs in expenditure in data!\n"), "red")
  }
  
  
  if(sum(data_slct$expenditures_dum, na.rm = TRUE) == nrow(data_slct)){
    
    # compute cdf
    data_slct$cdf <- 1
    
    # compute pdf
    data_slct$pdf <- 0
    
    message("No zero expenditures in category "%&%i%&%". Returns CDF=1 and PDF=0.")
    
  } else if(config$cens_yesno == "uncensored"){
    
    # compute cdf
    data_slct$cdf <- 1
    
    # compute pdf
    data_slct$pdf <- 0
    
    message("Config file setting: No adjustment for censored distribution."%&%
            " Returns CDF=1 and PDF=0.")
  } else if(config$cens_yesno == "censored"){
    
    message(paste0("Category ", i, ": ",
                 nrow(data_slct)-sum(data_slct$expenditures_dum, na.rm = TRUE),
                 " (of ", nrow(data_slct),
                 ") households with zero observations"))
    
    # define model
    eq <- as.formula(paste("expenditures_dum", paste(s, collapse = " + "),
                           sep = " ~ "))
    
    # run probit
    model <- glm(eq, family = binomial(link = "probit"), data = data_slct)
    
    # compute cdf
    data_slct$cdf <- pnorm(model$linear.predictors)
    
    # compute pdf
    data_slct$pdf <- dnorm(model$linear.predictors)
    
  } else{
    stop("No valid value set in config.do file")
  }
  
  return(data_slct)
}



prepare_easi <- function(df, s, dem_vars){
  
  #' @name prepare_easi
  #' @title Generate variables and compute cdf/pdf for EASI estimation
  #' @param df Clean household data set
  #' @param s Vector of first-stage covariates for censoring
  #' @param dem_vars Vector of demographic variable names
  #' @return df_return
  
  df <- df %>%
    
    {
      if(config$dse == "partial" | config$dse == "partialnoDE")
        
        # compute total expenditures (food only)
        group_by(., hh_id) %>%
        mutate(total_exp = sum(expenditures, na.rm = TRUE)) %>%
        ungroup()
      
      else if(config$dse == "incomplete")
        
        mutate(., total_exp = hh_exptot) %>%
        filter(total_exp>=0)
      
      else
        
        stop("dse not defined in config file (must be 'partial', 'partialnoDE' or 'incomplete'")
      
    } %>%
    
    # compute expenditure shares
    mutate(., s = expenditures/total_exp) %>%
    
    # compute mean expenditure shares
    group_by(., category) %>%
    mutate(s_mean = mean(s, na.rm = TRUE)) %>%
    ungroup() %>%
    
    # define price value according to configuration
    rename(uvadj    = uv_adj,
           uvnonadj = uv) %>%
    mutate(p = !!sym(config$prices)) %>% 
    
    # compute log prices
    mutate(., logp = log(p)) %>%
    
    # compute slogp and smeanlogp (product to be used for Stone price index)
    mutate(slogp     = s*logp,
           smeanlogp = s_mean*logp) %>%
    
    # wide format
    pivot_wider(.,
                id_cols     = c("hh_id", "total_exp", 
                                "dens_cat", all_of(dem_vars),
                                starts_with("hh_")),
                names_from  = category,
                values_from = c("s", "s_mean", "logp", "slogp",
                                "smeanlogp", "expenditures"), 
                names_sep   = "") %>%
    
    # compute log price-normalized prices
    mutate(., across(starts_with("logp"),
                     .fns   = list(n = ~. -eval(parse(text = p_to_norm))),
                     .names = "{fn}{col}")) %>%
    
    # compute log expenditure-normalized prices
    mutate(., across(starts_with("logp"),
                     .fns   = list(nfoodexp = ~. -log(total_exp)),
                     .names = "{fn}{col}")) %>%
    
    # compute Stone price index
    mutate(log_spi      = rowSums(across(starts_with("slogp")),
                                  na.rm = TRUE),
           log_spi_wbar = rowSums(across(starts_with("smeanlogp")),
                                  na.rm = TRUE)) %>%
    
    # in LA-EASI, real total expenditures is defined as y = X/p where X
    # is total nominal consumption expenditure and
    # log(P) = \sum_i s_i log(p_i) (Stone price index)
    mutate(y_stone = total_exp/exp(log_spi),
           y_tilda = total_exp/exp(log_spi_wbar)) %>%
    
    # define logy (ystone/ytilda) centered or uncentered
    {
      if(config$ycentered_yesno == "ycentered")
        mutate(., 
               logy_stone = log(y_stone) - weighted.median(log(y_stone),
                                                           w = hh_wgt),
               logy_tilda = log(y_tilda) - weighted.median(log(y_tilda),
                                                           w = hh_wgt))
      else if(config$ycentered_yesno == "yuncentered")
        mutate(., 
               logy_stone = log(y_stone),
               logy_tilda = log(y_tilda))
      else
        stop("ycentered_yesno not defined in config file (must be ycentered or yuncentered)")
    }
  
  # add polynomials
  k <- ncol(df)
  for (t in 1:config$npowers) {
    k <- k + 1
    # y polynomials: ystone or ytilda
    if(config$y == "ystone"){
      df[,k] <- df$logy_stone^t
      message("y is defined such that y=x-p'w")
    } else if(config$y == "ytilda"){
      df[,k] <- df$logy_tilda^t
      message("y is defined such that y=x-p'wbar")
    } else{
      stop("y not defined in config file (must be ystone or ytilda)")
    }
    colnames(df)[k]<-  paste0("log_y", t)
  }
  
  # Compute cumulative distribution, and probability density function
  df <- rbindlist(
    future_lapply(category_vec,
           compute_cdf_pdf,
           data = df %>%
             pivot_longer(cols = -c(starts_with("hh_"),
                                    starts_with("z"),
                                    starts_with("log_y"),
                                    "total_exp", "dens_cat",
                                    "log_spi", "log_spi_wbar", 
                                    "y_stone", "y_tilda",
                                    "logy_stone", "logy_tilda"),
                          names_to = c(".value", "category"),
                          names_sep = "(?<=[a-z])(?=[0-9])"),
           s = s
    )) %>%
    arrange(hh_id, category) %>%
    # Multiply log expenditures, normalized log prices, and sociodemographics
    # with cdf to adjust for censored distribution of sampling process
    mutate(across(starts_with("z"),
                  ~ as.numeric(as.character(.x)),
                  .names = "orig{col}")) %>%  # keeping the original zvar
    mutate(across(starts_with("z"),
                  ~ as.numeric(as.character(.x)))) %>%
    mutate(across(c(starts_with("log_y"),"nlogp", "nfoodexplogp"),
                  ~ (.x),
                  .names = "orig{col}")) %>%  # keeping the original log_y and nlogp/nfoodexplogp
    mutate(across(c(c(starts_with("log_y"), "nlogp", "nfoodexplogp",
                      starts_with("z"))),
                  ~ .x * cdf)) %>% # censor log_y and nlogp/nfoodexplogp
    pivot_wider(id_cols     = c(starts_with("hh_"),
                                starts_with("origz"),
                                starts_with("origlog_y"),
                                "total_exp", 
                                "dens_cat", "log_spi_wbar",
                                "y_stone", "y_tilda",
                                "logy_stone", "logy_tilda"),
                names_from  = category,
                values_from = c(starts_with("log_y"),
                                starts_with("z"),
                                "s", "s_mean", "logp", "slogp",
                                "smeanlogp", "expenditures",
                                "nlogp", "orignlogp", "nfoodexplogp",
                                "orignfoodexplogp",
                                "cdf", "pdf"),
                names_sep   = "")
  
  return(df)
}



elas_weighted_mean_ref <- function(i, df_result, type){
  
  #' @name elas_weighted_mean_ref
  #' @title Compute country-specific mean and reference household elasticities 
  #' @param i Food category (1:n_cat)
  #' @param df_result Household result data set containing household-specific
  #' elasticity estimates
  #' @param type can be one of "CPE_comp", "CPE_uncomp" or "EE"
  #' @return df_i_return
  
  # define expenditure and share columns of category i
  colex <- paste0("expenditures", i)
  colshare <- paste0("s", i)
  
  # generate weight
  df_i1 <- df_result %>% mutate(hhwgt_i = !!sym(colex)*hh_wgt) 
  
  # EXPENDITURE ELASTICITIES
  
  colelas_EE <- paste0("EE_", i)
  colsemielas_y <-  paste0("semielas_y_", i)
  colsemielas_y_ref <- colsemielas_y%&%"_ref"
  
  # compute mean expenditure elasticities
  df_ee_mean <- df_i1 %>%
    dplyr::select(hh_id, hhwgt_i, all_of(colelas_EE)) %>%
    summarise(across(all_of(colelas_EE), 
                     ~weighted.mean(., w = hhwgt_i, na.rm=TRUE), 
                     .names = '{col}_mean'))
  
  # compute reference expenditure elasticities
  df_ee_ref <- df_i1 %>%
    dplyr::select(hh_id, hhwgt_i, all_of(c(colsemielas_y, colshare))) %>%
    summarise(across(all_of(c(colsemielas_y, colshare)), 
                     ~weighted.mean(., w = hhwgt_i, na.rm=TRUE), 
                     .names = '{col}_ref')) %>% 
    summarise("{colsemielas_y_ref}" := !!sym(colsemielas_y_ref)/!!sym(colshare%&%"_ref") + 1) %>%
    rename_with(~ gsub("semielas_y", "EE", .x, fixed = TRUE))
  
  if(str_sub(type, 1,2) == "EE"){
    
    df_i_return <- bind_cols(df_ee_mean, df_ee_ref)
    
  } else if(str_sub(type, 1,2) == "CP"){
    
    # CROSS-PRICE ELASTICITIES (mean and ref)
    
    colsemielas_p <- paste0("semielas_p_i",i,"j")
    colsemielas_p_ref <- "semielas_p_i"%&%i%&%"j"%&%i%&%"_ref"
    colelas_compPE <- paste0("CPE_comp_i",i,"j")
    
    # compute mean CPEs
    df_comp_mean <- df_i1 %>%
      dplyr::select(hh_id, hhwgt_i, starts_with(colelas_compPE)) %>%
      summarise(across(starts_with(colelas_compPE),
                       ~weighted.mean(., w = hhwgt_i, na.rm=TRUE), 
                       .names = '{col}_mean'))
    
    # compute reference CPEs - compensated
    
    # weighted averages over components
    df_i_help <- df_i1 %>%
      dplyr::select(hh_id, hhwgt_i,
                    starts_with(colsemielas_p), matches("^s[[:digit:]]")) %>%
      summarise(across(c(starts_with(colsemielas_p),matches("^s[[:digit:]]")), 
                       ~weighted.mean(., w = hhwgt_i, na.rm=TRUE), 
                       .names = '{col}_ref'))
    
    # compute compensated elasticity
    for (j in 1:n_cat){
      df_i_help <- df_i_help %>%
        mutate("CPE_comp_i{i}j{j}_ref" :=
                 !!sym("semielas_p_i"%&%i%&%"j"%&%j%&%"_ref")/!!sym("s"%&%i%&%"_ref") + 
                 !!sym("s"%&%j%&%"_ref") - as.numeric(i == j)
        )
    }
    
    # choose variables
    df_comp_ref <- df_i_help %>% dplyr::select(starts_with("CPE_comp"))
    
    if(type=="CPE_comp"){
      
      df_i_return <- bind_cols(df_comp_mean, df_comp_ref)
      
    }else if(type == "CPE_uncomp"){
      
      # compute mean CPEs - uncompensated
      df_uncomp_mean <- df_i1 %>%
        dplyr::select(hh_id, hhwgt_i, starts_with("CPE_uncomp_i"%&%i%&%"j")) %>%
        summarise(across(starts_with("CPE_uncomp_i"%&%i%&%"j"), 
                         ~weighted.mean(., w = hhwgt_i, na.rm=TRUE), 
                         .names = '{col}_mean'))
      
      # compute reference CPEs - uncompensated
      
      # weighted averages over components (adding comp CPEs and EEs)
      df_i_help <- df_i1 %>%
        dplyr::select(hh_id, hhwgt_i, matches("^s[[:digit:]]")) %>%
        summarise(across(c(matches("^s[[:digit:]]")), 
                         ~weighted.mean(., w = hhwgt_i, na.rm=TRUE), 
                         .names = '{col}_ref')) %>%
        bind_cols(df_comp_ref) %>%
        bind_cols(df_ee_ref)
      
      # construct uncompensated elasticity
      for (j in 1:n_cat){
        df_i_help <- df_i_help %>%
          mutate("CPE_uncomp_i{i}j{j}_ref" :=
                   !!sym("CPE_comp_i"%&%i%&%"j"%&%j%&%"_ref") -
                   !!sym("EE_"%&%i%&%"_ref") * !!sym("s"%&%j%&%"_ref")
          )
      }
      
      # choose variables
      df_uncomp_ref <- df_i_help %>% dplyr::select(starts_with("CPE_uncomp"))
      
      # combine
      df_i_return <- bind_cols(df_uncomp_mean, df_uncomp_ref)
    }
    
    
  } else {
    
    break("Elasticities not correctly specified.")
    
  }
  
  return(df_i_return)
}



compute_elasticities <- function(df_final,
                                 df_coefs,
                                 c,
                                 ndem,
                                 catexplain,
                                 n_cat){
  
  #' @name compute_elasticities
  #' @title Compute elasticities
  #' @description 
  #' @param df_final final household data set
  #' @param df_coefs estimated coefficients from SUR
  #' @param c country to compute elasticities for
  #' @param ndem number of demographic variables
  #' @param catexplain dataframe with food category codes and names
  #' @param number of categories in catexplain
  #' @return list of hh_data, cpe_comp, cpe_uncomp, and ee
  
  # add coefficients to dataset
  data_est_result <- cbind(
    df_final,
    df_coefs %>%
      slice(rep(1:n(),
                each = nrow(df_final)))
  )
  colnames(data_est_result) <- sub('\\_restr', '', colnames(data_est_result))
  
  # vector of demographic characteristics (no income data for Italy)
  if(c == "IT"){
    ndem_vec <- c(1:ndem)[-4]
  }else{
    ndem_vec <- c(1:ndem)
  }
  
  # set price var depending on alln vs nmin1
  if(config$equations == "alln"){
    pricevar <- "nfoodexplogp"
  }else if(config$equations == "nmin1"){
    pricevar <- "nlogp"
  }
  
  # keep original share vector as sorig
  data_est_result <- data_est_result %>%
    mutate(across(matches("^s[[:digit:]]"),
                  ~ .x,
                  .names = "orig{col}")) 
  
  
    # fitted values (expected shares)
    for(i in 1:nrow(catexplain)){
      
      # logy (r polynomials)
      y_vec <- 0
      for (r in 1:config$npowers) y_vec <- y_vec + data_est_result[["br"%&%r%&%"_"%&%i]]*data_est_result[["log_y"%&%r%&%i]]
      # p 
      p_vec <- 0
      for (j in 1:nrow(catexplain)) p_vec <- p_vec + data_est_result[["a_i"%&%i%&%"j"%&%j]]*data_est_result[[pricevar%&%j]]
      # z
      z_vec <- 0
      for (k in ndem_vec) z_vec <- z_vec + data_est_result[["c_"%&%i%&%"_z"%&%k]]*data_est_result[["z"%&%k%&%i]]
      # yz-interactions
      yz_vec <- 0
      for (k in ndem_vec) yz_vec <- yz_vec + data_est_result[["d_"%&%i%&%"_z"%&%k]]*data_est_result[["log_y1"%&%i]]*data_est_result[["z"%&%k%&%i]]
      # py-interactions
      py_vec <- 0
      for (j in 1:nrow(catexplain)) py_vec <- py_vec + data_est_result[["b_i"%&%i%&%"j"%&%j]]*data_est_result[["log_y1"%&%i]]*data_est_result[[pricevar%&%j]]
      # pz-interactions
      pz_vec <- 0
      if(config$pzint == "pzintyes"){
        for (k in ndem_vec) {
          for (j in 1:nrow(catexplain)){
            pz_vec <- pz_vec + data_est_result[["e_i"%&%i%&%"j"%&%j%&%"_z"%&%k]]*data_est_result[[pricevar%&%j]]*data_est_result[["z"%&%k%&%i]]
          }
      }
      }

      # if shares = actual, save expected shares (under a different name)
      if(config$shares == "actual"){
        
        message("Category "%&%i%&%": Actual shares used in elasticity computation")
        
        # sum up all components listed above + cons + pdf
        data_est_result[["s_exp_"%&%i]] <- y_vec+p_vec+z_vec+yz_vec+py_vec+pz_vec+data_est_result[["cons_"%&%i]]+data_est_result[["f_"%&%i]]*data_est_result[["pdf"%&%i]]
        
      # if shares = expected, redefine s as fitted or orig values
      }else if(config$shares == "expected"){
        
        message("Category "%&%i%&%": Expected shares used in elasticity computation")
        
        # sum up all components listed above + cons + pdf
        data_est_result[["s"%&%i]] <- y_vec+p_vec+z_vec+yz_vec+py_vec+pz_vec+data_est_result[["cons_"%&%i]]+data_est_result[["f_"%&%i]]*data_est_result[["pdf"%&%i]]
        
      }
    }#close i loop
  
  
  # Compute expenditure elasticities (EE)
  
  # Note: only br needs to be multiplied with cdf
  # (z,nlogp/nfoodexplogp and log_y are 'censored')
  for (i in 1:n_cat) {
    
    # generate sum_d_z_i
    data_est_result[["sum_d_"%&%i%&%"_Z"]] <- 0
    for (k in ndem_vec){
      data_est_result[["sum_d_"%&%i%&%"_Z"]] <- data_est_result[["sum_d_"%&%i%&%"_Z"]] +  
        data_est_result[["z"%&%k%&%i]]*data_est_result[["d_"%&%i%&%"_z"%&%k]]
    }
    
    # generate sum_b_p_i
    data_est_result[["sum_b_p_"%&%i]] <- 0
    for (j in 1:n_cat){
      data_est_result[["sum_b_p_"%&%i]] <- data_est_result[["sum_b_p_"%&%i]] +  
        data_est_result[["b_i"%&%i%&%"j"%&%j]]*data_est_result[[pricevar%&%j]]
    }
    
    # compute semielas_y
    data_est_result <- data_est_result %>%
      mutate("semielas_y_{i}" :=
               !!sym("cdf"%&%i) * !!sym("br1_"%&%i) +
               2 * !!sym("br2_"%&%i) * !!sym("log_y1"%&%i) +
               3 * !!sym("br3_"%&%i) * !!sym("log_y2"%&%i) +
               4 * !!sym("br4_"%&%i) * !!sym("log_y3"%&%i) +
               !!sym("sum_b_p_"%&%i) + 
               !!sym("sum_d_"%&%i%&%"_Z"))
    
    # compute expenditure elasticity
    data_est_result <- data_est_result %>%
      mutate("EE_{i}" :=
               !!sym("semielas_y_"%&%i) / !!sym("s"%&%i) + 1
      ) 
    
  }
  
  # (+/-) infinite values to NA
  data_est_result <- data_est_result %>% 
    mutate_at(vars(starts_with("EE")),
              ~ replace(., . == -Inf, NA)) %>% 
    mutate_at(vars(starts_with("EE")),
              ~ replace(., . ==  Inf, NA))
  
  
  # Compute compensated own- and cross-price elasticities (CPE)
  
  for (i in 1:n_cat) {
    for (j in 1:n_cat){
      
      # compute sum_e_z_i_j
      data_est_result[["sum_e_"%&%i%&%j%&%"_Z"]] <- 0
      if(config$pzint == "pzintyes"){
        for (k in ndem_vec){
          data_est_result[["sum_e_"%&%i%&%j%&%"_Z"]] <- data_est_result[["sum_e_"%&%i%&%j%&%"_Z"]] +  
            data_est_result[["z"%&%k%&%i]]*data_est_result[["e_i"%&%i%&%"j"%&%j%&%"_z"%&%k]]
        }
      }
      
      # compute semi-elasticity
      # Note: only a needs to be multiplied with cdf (z and log_y are 'censored')
      data_est_result <- data_est_result %>%
        mutate("semielas_p_i{i}j{j}" :=
                 !!sym("cdf"%&%i) * !!sym("a_i"%&%i%&%"j"%&%j) +
                 !!sym("b_i"%&%i%&%"j"%&%j) * !!sym("log_y1"%&%i) + 
                 !!sym("sum_e_"%&%i%&%j%&%"_Z"))
      
      # compute compensated elasticity
      data_est_result <- data_est_result %>%
        mutate("CPE_comp_i{i}j{j}" :=
                 !!sym("semielas_p_i"%&%i%&%"j"%&%j)/!!sym("s"%&%i) + 
                 !!sym("s"%&%j) - as.numeric(i == j)
        )
    }
  }
  
  # (+/-) infinite values to NA
  data_est_result <- data_est_result %>% 
    mutate_at(vars(starts_with("CPE")),
              ~ replace(., . == -Inf, NA)) %>% 
    mutate_at(vars(starts_with("CPE")),
              ~ replace(., . ==  Inf, NA))
  
  
  # Compute uncompensated own- and cross-price elasticities
  # uncompensated (Marshallian) demand elasticities can be computed using the Slutsky equation:
  # - e_{i}^{uncompensated} be the uncompensated price elasticity of good i
  # - e_{i}^{compensated} be the compensated price elasticity of good i
  # - n_{j} be the expenditure elasticity of good j
  # - w_i be the expenditure share of good i
  # -> own-price Slutsky equation:   e_{i}^{uncompensated} = e_{i}^{compensated} - n_{i} w_i
  # -> cross-price Slutsky equation: e_{ij}^{uncompensated} = e_{ij}^{compensated} - n_{i} w_j
  # see e.g. https://eml.berkeley.edu/~webfac/saez/e131_s04/taxes.pdf, http://willmann.com/~gerald/mikro1-01/lecnotes3.pdf 
  # formula used below from slide 7 in http://willmann.com/~gerald/mikro1-01/lecnotes3.pdf
  
  for (i in 1:n_cat) {
    for (j in 1:n_cat){
      data_est_result <- data_est_result %>%
        mutate("CPE_uncomp_i{i}j{j}" :=
                 !!sym("CPE_comp_i"%&%i%&%"j"%&%j) -
                 !!sym("EE_"%&%i) * !!sym("s"%&%j)
        )
    }
  }
  
  
  # Prepare output
  
  # Mean and reference country expenditure elasticities
  ee <- lapply(1:n_cat, elas_weighted_mean_ref, 
               df_result = data_est_result, 
               type = "EE") %>%
    bind_cols()
  
  # Mean and reference country compensated own- and cross-price elasticities
  cpe_comp <- lapply(1:n_cat, elas_weighted_mean_ref, 
                     df_result = data_est_result, 
                     type = "CPE_comp") %>%
    bind_cols()
  
  # Mean and reference country uncompensated own- and cross-price elasticities
  cpe_uncomp <- lapply(1:n_cat, elas_weighted_mean_ref, 
                       df_result = data_est_result, 
                       type = "CPE_uncomp") %>%
    bind_cols()
  
  return(list(hh_data    = data_est_result,
              ee         = ee,
              cpe_comp   = cpe_comp,
              cpe_uncomp = cpe_uncomp))
}






