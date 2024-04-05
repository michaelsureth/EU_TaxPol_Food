# File info --------------------------------------------------------------------

# File:    Preparation and processing of data (HBS, EVS, Austrian 
#          Konsumerhebung - K14)  for EASI demand system estimation
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption

# _____________________________________-----------------------------------------
# Preparations -----------------------------------------------------------------

# _ Load categorization of food items ------------------------------------------

# Load manually aggregated food categories based on HBS data's COICOP food items
map_HBS_item_codes <- read_xlsx("00_data/manual_input/categorization_fooditems.xlsx",
                                sheet = "HBS_"%&%config$categorization) %>%
  select(item_code_orig, category_name_new,
         category_code_new, item_code_new) %>%
  filter(!is.na(item_code_new)) %>%
  mutate(item_code_orig = gsub("EUR_HE", "", item_code_orig))

map_EVS_item_codes <- read_xlsx("00_data/manual_input/categorization_fooditems.xlsx",
                                sheet = "EVS_"%&%config$categorization) %>%
  select(item_code_orig, category_code_new, category_name_new) %>%
  filter(!is.na(item_code_orig))

map_K14_item_codes <- read_xlsx("00_data/manual_input/categorization_fooditems.xlsx",
                                sheet = "HBS_"%&%config$categorization) %>%   
  transmute(item_code_orig = gsub("EUR_HE", "", item_code_orig),
            item_name_orig_2015,
            item_code_AT)

# Load manually defined names for aggregated food categories and save as .csv
catexplain <- map_HBS_item_codes %>%
  filter(!is.na(category_code_new)) %>%
  dplyr::transmute(category_code_new = as.character(category_code_new),
                   category_name_new) %>%
  distinct() %>%
  arrange(as.numeric(category_code_new))

if(!dir.exists("../build/data/intermediate_output/")){
  dir.create("../build/data/intermediate_output/",
             recursive = TRUE)
}

write.csv(catexplain, "../build/data/intermediate_output/"%&%
            "HBS_catexplain_"%&%config$categorization%&%".csv",
          row.names = FALSE)

# Save item codes
category_vec <- map_HBS_item_codes %>% arrange(category_code_new) %>%
  select(category_code_new) %>% unique() %>% pull()



# _____________________________________-----------------------------------------
# HBS data ---------------------------------------------------------------------

# _ Load and prepare -----------------------------------------------------------

# Lookup available years in HBS data
y <- list.files("00_data/1. HBS microdata - full set/") %>%
  str_subset(pattern = "HBS[[:digit:]]") %>% sub(".*HBS", "", .)

# Load mapping of food item codes to food categories
items <- read_xlsx("00_data/manual_input/categorization_fooditems.xlsx",
                   sheet = "HBS_"%&%config$categorization) %>%
  filter(!is.na(item_code_new)) %>%
  select(item_code      = item_code_orig,
         item_name_2010 = item_name_orig_2010,
         item_name_2015 = item_name_orig_2015,
         category_code  = category_code_new,
         category_name  = category_name_new) %>%
  mutate(item_code = gsub("EUR_HE", "", item_code))

# For each year y
list_HBS <-
  sapply(y,
         function(y){
           message(y)
           
           # Construct vector of available files
           files <- list.files("00_data/1. HBS microdata - full set/HBS"%&%y,
                               pattern = ".xlsx") %>%
             # Note: Remove Germany as EVS is used
             str_subset("DE", negate = TRUE) %>%
             # Note: Remove UK as not part of EU27 anymore
             str_subset("UK", negate = TRUE)
           
           # Construct vector of available countries (country codes)
           countries <- files %>% substr(1, 2) %>% unique()
           
           return <- 
             # For each country c available for year y
             future_lapply(countries,
                      function(c){
                        message(c)
                        
                        # Load household and household member data
                        df_hh <- read_xlsx("00_data/1. HBS microdata - full set/HBS"%&%y%&%
                                             "/"%&%files[grepl("^"%&%c%&%"(.*hh)", files)])
                        df_hm <- read_xlsx("00_data/1. HBS microdata - full set/HBS"%&%y%&%
                                             "/"%&%files[grepl("^"%&%c%&%"(.*hm)", files)])
                        
                        # Check availability of expenditures and quantity per food item 
                        item_avail <- items %>%
                          select(c("item_code",
                                   "item_name_"%&%y,
                                   "category_code",
                                   "category_name")) %>%
                          filter(!!sym("item_name_"%&%y) != "NA") %>%
                          left_join(var_indicator(df_hh %>%
                                                    select(starts_with("EUR_HE01"))) %>%
                                      t() %>% as.data.frame() %>%
                                      rownames_to_column() %>%
                                      mutate(item_code = gsub("EUR_HE", "", rowname)) %>%
                                      select(item_code, exp_avail = V2),
                                    by = "item_code") %>%
                          left_join(var_indicator(df_hh %>%
                                                    select(starts_with("HQ01"))) %>%
                                      t() %>% as.data.frame() %>%
                                      rownames_to_column() %>%
                                      mutate(item_code = gsub("HQ", "", rowname)) %>%
                                      select(item_code, quan_avail = V2),
                                    by = "item_code") %>%
                          replace(is.na(.), 0) %>%
                          mutate(exp_quan_avail = ifelse(exp_avail == 1 & quan_avail == 1,
                                                         1,
                                                         0))
                        
                        return <- list(df_hh = df_hh,
                                       df_hm = df_hm,
                                       items_avail = item_avail,
                                       # Prepare data using the function prepare_HBS_data()
                                       prepared    = prepare_HBS_data(df_hh, df_hm))
                        
                        return(return)
                      })
           names(return) <- countries
           return(return)
         },
         simplify = FALSE)


# _ Treat implausible observations on item level -------------------------------

list_clean_HBS <- 
  sapply(y, function(y){
    print_color(y%&%"\n", color = "cyan")
    
    temp <- lapply(list_HBS[[y]], "[[", "prepared")
    
    future_lapply(temp,
             function(c){
               print_color((c %>% distinct(country) %>% pull)%&%"\n", color = "red")
               
               # Drop observations with negative expenditures
               out1 <- c %>%
                 filter(expenditures < 0) %>%
                 distinct(hh_id) %>%
                 pull
               
               # Drop observations with zero expenditures and positive quantities
               out2 <- c %>%
                 filter(quantity > 0 & expenditures == 0) %>%
                 distinct(hh_id) %>%
                 pull
               
               message(length(out1)%&%" households dropped due to negative "%&%
                       "expenditures in at least one food item.")
               message(length(out2)%&%" households dropped due to zero expenditures "%&%
                       "and positive quantities.")
               
               return(
                 c %>%
                   filter(!(hh_id %in% out1 | hh_id %in% out2))
               )
             }) %>%
      setNames(names(temp))
  }, simplify = FALSE)



# _ Imputation of missing quantity data ----------------------------------------

# There are two cases of missing quantity data in HBS
# - Case (1): Quantity of food item i has not been assessed in country c
# - Case (2): Quantity has been assessed, but data is implausible as quantity of
#             food item i is 0 and expenditures for food item i are >0
# Case (1) and (2) are addressed separately as follows
# - Case (1): We construct four regions based on the UN geo-scheme for Europe.
#             Within each region we impute the quantities of all food items that 
#             have not been assessed in country c by matching each household to
#             its nearest-neighbours among all households of the countries in the 
#             region that do have data on the respective food item available
# - Case (2): We impute all implausible quantity observations in country c by
#             matching the respective households with their nearest neighbors
#             among all other households of country c


# __ Case (1): region-based ----------------------------------------------------

# Construct country vectors for each year y
c_vecs <-
  sapply(y,
         function(y){
           
           # Draw availability data from list constructed above
           items_avail <- sapply(list_HBS[[y]],
                                 "[[", "items_avail",
                                 simplify = FALSE) %>%
             bind_rows(.id = "country") %>%
             select(c("country", "item_code", "item_name_"%&%y,
                      "category_code", "category_name", "exp_quan_avail"))
           
           # Set of all countries available for year y
           all <- items_avail %>% select(country) %>% unique() %>% pull()
           
           # Set of all countries that do not have expenditure and quantity
           # data available for at least one food item in each food category
           out <- items_avail  %>%
             group_by(country, category_code, category_name) %>%
             summarize(available_items = sum(exp_quan_avail, na.rm = TRUE)) %>%
             ungroup() %>%
             filter(available_items == 0) %>% 
             select(country) %>% distinct() %>% pull()
           
           # Set of countries that do not have expenditure and quantity data
           # available for all food items (and thus need imputation)
           matching <-  items_avail %>%
             pivot_wider(id_cols = c("item_code", 
                                     "item_name_"%&%y,
                                     "category_code",
                                     "category_name"),
                         values_from = "exp_quan_avail",
                         names_from  = "country") %>%
             summarize(across(-c(item_code,
                                 "item_name_"%&%y, category_code,
                                 category_name),
                              ~ sum(., na.rm = TRUE))) %>%
             pivot_longer(cols = everything()) %>%
             filter(value < nrow(items_avail %>%
                                   distinct(item_code))) %>% pull(name)
           
           return <- list(items_avail = items_avail,
                          all         = all,
                          out         = out,
                          matching    = matching)
           
           return(return)
         },
         simplify = FALSE)

# Keep country data from HBS 2010 if country c is in the set 2015$out (i.e.
# country c has not at least one food item available per food category) and
# (1) in the set 2010$all (i.e. available in HBS 2010)
# (2) is not in 2010$out (i.e. not excluded from HBS 2010 due to missing data)
keep2010 <- c_vecs[["2015"]]$out[  c_vecs[["2015"]]$out %in% c_vecs[["2010"]]$all &
                                 ! c_vecs[["2015"]]$out %in% c_vecs[["2010"]]$out]

# Keep country data from HBS 2015 if country c is in the set 20105$all (i.e.
# available in HBS 2015) and is not in keep2010 (i.e. taken from HBS 2010)
keep2015 <- c_vecs[["2015"]]$all[! c_vecs[["2015"]]$all %in% keep2010]

# Match country data from HBS 2010 if country c is in the set HBS2010$matching
# (i.e. quantity data is not for all food items available) and
# (1) is in the set keep2010 (i.e. taken from HBS 2010)
# (2) is not in the set keep2015 (i.e. not taken from HBS 2015)
matching2010 <- c_vecs[["2010"]]$matching[  c_vecs[["2010"]]$matching %in% keep2010 & 
                                          ! c_vecs[["2010"]]$matching %in% keep2015]

# Match country data from HBS 2015 if country c is in the set HBS2015$matching
# (i.e. quantity data is not for all food items available) and
# (1) is in the set keep2015 (i.e. taken from HBS 2015)
# (2) is not in the set keep2010 (i.e. not taken from HBS 2010)
matching2015 <- c_vecs[["2015"]]$matching[  c_vecs[["2015"]]$matching %in% keep2015 &
                                          ! c_vecs[["2015"]]$matching %in% keep2010]

# Bind list of all country data into single data frame for each year y
list_df_HBS <- sapply(y,
                      function(y){
                        list_clean_HBS[[y]] %>%
                          bind_rows(.id = "countries")
                      },
                      simplify = FALSE)

# Bind list of all food item availability (expenditures and quantities) for each
# year y
items_avail_all_c <- sapply(y,
                            function(y){
                              sapply(list_HBS[[y]], "[[", "items_avail",
                                     simplify = FALSE) %>%
                                bind_rows(.id = "country")
                            },
                            simplify = FALSE)

list_matched1_HBS <- 
  # For each HBS wave (2010 and 2015)
  sapply(y,
         function(y){
           message(y)
           
           # For each country in the vector matching2010 or matching2015
           future_lapply(eval_obj("matching"%&%y),
                    function(c){
                      message(c)
                      
                      # Retrieve vector of items for which matching should be done
                      # in country c
                      match_items <- list_HBS[[y]][[c]]$items_avail %>%
                        # match only items for which no quantities but expenditures
                        # are available
                        filter(quan_avail == 0 & exp_avail == 1) %>%
                        select(item_code) %>% pull()
                      
                      # Construct a (regional) matching pool
                      match_pool_r <- list_df_HBS[[y]] %>%
                        # keeping only countries that belong to the same region as
                        # country c
                        filter(country %in%
                                 (EU27 %>%
                                    filter(region == EU27 %>%
                                             filter(geo == c) %>%
                                             pull(region)) %>%
                                    pull(geo))) %>%
                        # compute standardized income and expenditures variables
                        # for knn-algorithm
                        mutate(across(c("hh_inctot", "expenditures"),
                                      ~ c(scale(., center = TRUE, scale = TRUE)),
                                      .names = "{.col}_std"))
                      
                      # Construct household dataset containing only observations
                      # with positive expenditures in country c
                      hh_data <- match_pool_r %>%
                        filter(country == c & expenditures > 0)
                      
                      # For each item i contained in the vector of items that have
                      # to be matched for country c
                      lapply(match_items,
                               function(i){
                                 
                                 # Construct filter: keep only countries for which
                                 # item i is available
                                 c_filter <- items_avail_all_c[[y]] %>%
                                   filter(item_code == i & exp_quan_avail == 1) %>%
                                   pull(country)
                                 
                                 # Apply filter to regional matching pool
                                 match_pool_r_i <- match_pool_r %>%
                                   filter(item_code == i &
                                            country != c &
                                            country %in% c_filter &
                                            quantity > 0)
                                 
                                 # Select item to be imputed in hh_data
                                 hhs <- hh_data %>%
                                   filter(item_code == i)
                                 
                                 # Set number of nearest-neighbor matches
                                 k <- min(10, nrow(match_pool_r_i))
                                 
                                 # Select variables to match on
                                 x <- c("hh_inctot_std", "expenditures_std", "z1",
                                        "z2", "z3", "z5")
                                 # Note: for 2010 LU and 2015 IT data, household
                                 # income is missing in HBS. Thus, income and z4 are
                                 # removed from vector x
                                 if((y == "2010" &
                                     "LU" %in% (match_pool_r_i %>%
                                                distinct(country) %>% pull)) |
                                    (y == "2015" & c == "IT")){
                                   x <- x[!(grepl("hh_inctot_std", x) | grepl("z4", x))]
                                 }
                                 
                                 # Find k nearest neighbors of household hh w.r.t.
                                 # income, item expenditures, and socio-demographics z
                                 nn <- get.knnx(match_pool_r_i %>%
                                                  select(all_of(x)),
                                                hhs %>%
                                                  select(all_of(x)),
                                                k = k)
                                 return(
                                   # Select matched households in match_pool_r_i (possibly
                                   # multiple times): for each household to be imputed, k
                                   # households from the matching pool are selected
                                   match_pool_r_i[nn$nn.index %>% t() %>% as.vector, ] %>%
                                     select(quantity, expenditures) %>%
                                     mutate(weight  = 1/(nn$nn.dist %>% t %>% as.vector),
                                            # Add hh_ids of households to be imputed
                                            hh_id =  rep(hhs %>% pull(hh_id), each = k)) %>%
                                     group_by(hh_id) %>%
                                     # Compute mean quantity and expenditures of k nearest
                                     # neighbors of household to be imputed weighted by
                                     # distance
                                     summarize(quantity     = weighted.mean(quantity,
                                                                            weight,
                                                                            na.rm = TRUE),
                                               expenditures = weighted.mean(expenditures,
                                                                            weight,
                                                                            na.rm = TRUE)) %>%
                                     # Compute scaling factor for quantity, i.e., the ratio
                                     # of the expenditures of the household to be imputed and
                                     # the weighted mean expenditures of the k nearest neighbors
                                     mutate(scaling = hhs %>% pull(expenditures) / expenditures,
                                            # Scale the imputation quantity
                                            quantity_imputed_case1 = quantity * scaling) %>%
                                     select(hh_id, quantity_imputed_case1)
                                 )
                               }) %>%
                        setNames(match_items) %>%
                        bind_rows(.id = "item_code")
                    }) %>%
             setNames(eval_obj("matching"%&%y)) %>%
             bind_rows(.id = "country")
         },
         simplify = FALSE)

# Bind matched list to dataframe keeping only those countries from HBS 2010 and
# HBS 2015 specified in keep2010 and keep2015
df_matched1_HBS <- list_matched1_HBS[["2010"]] %>%
  filter(country %in% keep2010) %>%
  bind_rows(list_matched1_HBS[["2015"]] %>%
              filter(country %in% keep2015))

list_imp_case1_HBS <- 
  # Combine list of HBS data sets to single data frame
  lapply(y,
         function(y){
           # Select countries from HBS 2010 or HBS 2015 based on keep2010 and keep2015
           list_clean_HBS[[y]][names(list_clean_HBS[[y]]) %in% eval_obj("keep"%&%y)] %>%
             bind_rows(.id = "country")
         }) %>%
  setNames(y) %>%
  bind_rows() %>%
  # Add imputation values to full data set
  left_join(df_matched1_HBS %>%
              select(hh_id, item_code, quantity_imputed_case1),
            by = join_by(hh_id, item_code)) %>%
  # Impute missing quantities
  mutate(quantity = ifelse(is.na(quantity) & !is.na(quantity_imputed_case1),
                           quantity_imputed_case1,
                           quantity),
         quantity = ifelse(is.na(quantity) & expenditures == 0,
                           0,
                           quantity)) %>%
  # Split single data frame into list
  split(., f = .$country)


# __ Case (2): per country -----------------------------------------------------

list_imp_case2_HBS <-
  # For each country c
  sapply(list_imp_case1_HBS,
         function(c){
           message(c %>% distinct(country) %>% pull())
           
           # Construct set of household-item cases that need imputation
           match_hh_i <- c %>%
             rownames_to_column(.) %>%
             rename(index = rowname) %>%
             # select all observations that need imputation (i.e., that are
             # implausible as expenditures > 0, but quantity = 0)
             filter(!is.na(expenditures) &
                      expenditures >  0 &
                      quantity     == 0) %>%
             select(hh_id, item_code, index)
           
           # Construct matching pool
           match_pool_c <- c %>%
             # compute standardized income and expenditures variables for knn-
             # algorithm
             mutate(across(c("hh_inctot", "expenditures"),
                           ~ c(scale(., center = TRUE, scale = TRUE)),
                           .names = "{.col}_std"))
           
           # Construct dataset containing all households that need imputation
           hh_data <- match_pool_c[match_hh_i$index, ]
           
           return <- 
             future_lapply(match_hh_i %>% distinct(item_code) %>% pull(),
                      function(i){
                        
                        # Restrict matching pool to item code i, and households
                        # with strictly positive expenditures and  quantities
                        match_pool_c_i <- match_pool_c %>%
                          filter(expenditures > 0 & quantity > 0 & item_code == i)
                        
                        # Select item to be imputed in hh_data
                        hhs <- hh_data %>%
                          filter(item_code == i)
                        
                        # Select variables to match on
                        x <- c("hh_inctot_std", "expenditures_std", "z1", "z2",
                               "z3", "z5")
                        
                        k <- min(10, nrow(match_pool_c_i))
                        
                        nn <- get.knnx(match_pool_c_i %>%
                                         select(all_of(x)),
                                       hhs %>%
                                         select(all_of(x)),
                                       k = k)
                        
                        return(
                          # Select matched households in match_pool_r_i (possibly
                          # multiple times): for each household to be imputed, k
                          # households from the matching pool are selected
                          match_pool_c_i[nn$nn.index %>% t() %>% as.vector, ] %>%
                            select(quantity, expenditures) %>%
                            mutate(weight  = 1/(nn$nn.dist %>% t %>% as.vector),
                                   # Add hh_ids of households to be imputed
                                   hh_id =  rep(hhs %>% pull(hh_id), each = k)) %>%
                            group_by(hh_id) %>%
                            # Compute mean quantity and expenditures of k nearest
                            # neighbors of household to be imputed weighted by
                            # distance
                            summarize(quantity     = weighted.mean(quantity,
                                                                   weight,
                                                                   na.rm = TRUE),
                                      expenditures = weighted.mean(expenditures,
                                                                   weight,
                                                                   na.rm = TRUE)) %>%
                            # Compute scaling factor for quantity, i.e., the ratio
                            # of the expenditures of the household to be imputed and
                            # the weighted mean expenditures of the k nearest neighbors
                            mutate(scaling = hhs %>% pull(expenditures) / expenditures,
                                   # Scale the imputation quantity
                                   quantity_imputed_case2 = quantity * scaling) %>%
                            select(hh_id, quantity_imputed_case2)
                        )
                      }) %>%
             setNames(match_hh_i %>% distinct(item_code) %>% pull()) %>%
             bind_rows(.id = "item_code")
           
           if(length(return) == 0){
             c %>%
               # Impute missing quantities
               mutate(quantity = ifelse(is.na(quantity) & expenditures == 0,
                                        0,
                                        quantity))
           } else {
             return %>%
               # Add imputation values to full country data set
               right_join(c,
                          by = join_by("hh_id", "item_code")) %>%
               # Impute missing quantities
               mutate(quantity = ifelse(quantity == 0 & !is.na(quantity_imputed_case2),
                                        quantity_imputed_case2,
                                        quantity),
                      quantity = ifelse(is.na(quantity) & expenditures == 0,
                                        0,
                                        quantity))
           }
         },
         simplify = FALSE)

list_imp_final_HBS <-
  sapply(list_imp_case2_HBS,
         function(c){
           c %>%
             relocate(item_code, .before = expenditures) %>%
             select(-c(matches("quantity_imputed")))
         },
         simplify =  FALSE)



# _____________________________________-----------------------------------------
# EVS data (DE) ----------------------------------------------------------------

# _ Load and prepare -----------------------------------------------------------

EVS_prepared <- 
  fread("00_data/2023_EVS_2018/23D024/"%&%
          "suf_evs_2018_ngt_gf4_slr/daten/evs_ngt2018_slr.csv",
        sep = ";",
        data.table = FALSE) %>%
  prepare_DE_data()


# _ Treat implausible observations on item level -------------------------------

# Drop observations with negative expenditures
out1 <- EVS_prepared %>%
  filter(expenditures < 0) %>%
  distinct(hh_id) %>%
  pull

# Drop observations with zero expenditures and positive quantities
out2 <- EVS_prepared %>%
  filter(quantity > 0 & expenditures == 0) %>%
  distinct(hh_id) %>%
  pull

message("DE: "%&%length(out1)%&%" households dropped due to negative "%&%
        "expenditures in at least one food item.")
message("DE: "%&%length(out2)%&%" households dropped due to zero expenditures "%&%
        "and positive quantities.")

EVS_clean <- 
  EVS_prepared %>%
  filter(!(hh_id %in% out1 | hh_id %in% out2))

# Note: EVS food items do not coincide with COICOP level 5 food items as in HBS.
# Thus, although there are EVS food items for which no quantity data has been 
# collected, Case (1) matching and imputation with HBS data is not possible.

# Diagnostics: Check that Case (2) imputation is not necessary for EVS data
# -> correct
EVS_clean %>%
  filter(expenditures >  0 &
           quantity   == 0)


# _ Combine with HBS data ------------------------------------------------------

list_clean_data <-
  c(list_imp_final_HBS, list(DE = EVS_clean))



# _____________________________________-----------------------------------------
# Unit value computation and categorization ------------------------------------

#  _ Compute (adjusted) unit values --------------------------------------------
# (following Cox & Wohlgenant 1986)

# Select independent variables for quality adjustment regression
x <- c("dens_cat", "hh_children", "hh_sizeoecdm", "hh_inctot", "hh_agehead",
       "hh_sexhead")

# Set number of observations at least needed to run regression (i.e., if less than
# n_min households in country c consumed food item i, no unit value adjustment can
# be conducted -> then uv is imputed vor uv_adj)
n_min <- 30

list_prices_data <- 
  future_lapply(list_clean_data,
           function(c){
             
             i <- c %>% distinct(item_code) %>% pull() %>% sort()
             
             return(
               lapply(i,
                      function(i){
                        
                        # Construct data set to run regression on
                        data <- c %>%
                          # Keep only household observations of item i
                          filter(item_code == i) %>%
                          # Compute unit value (UV) if expenditures and quantities
                          # are available
                          mutate(uv = ifelse(expenditures > 0 & quantity == 0,
                                             NA,
                                             expenditures/quantity)) %>%
                          # Use only observations with UVs and income equal to or
                          # below the respective 99th percentile
                          filter(uv        <= quantile(uv, 0.99, na.rm = TRUE) &
                                 hh_inctot <= quantile(hh_inctot, 0.99, na.rm = TRUE)) %>%
                          # Keep only complete cases
                          filter(complete.cases(select(., -hh_exptot)))
                        
                        # Only run regression if sufficient observations are available
                        if(nrow(data) > n_min) {
                          # Construct regression equation for quality adjustment
                          for(j in x){
                            # Check for variance in independent variables and remove
                            # if constant
                            if(length(unique(data[[j]])) == 1) {x <- x[!grepl(j, x)]}
                          }
                          eq <- as.formula("log(uv) ~ "%&%paste(x, collapse = "+"))
                          
                          # Run linear regression: log(uv) = x-vector + error
                          lm <- lm(eq, data, na.action = na.exclude)
                        }
                        
                        return(
                          data %>%
                            # If regression was computed
                            {
                              if(nrow(.) > n_min)  
                                filter(., complete.cases(select(., -hh_exptot))) %>%
                                mutate(resid  = lm$residuals,
                                       # Compute adjusted UV:
                                       # UV_adj = regression constant + residual
                                       uv_adj = exp(lm$coefficients[1] + resid)) %>%
                                select(-resid)
                              else 
                                .
                            } %>%
                            # Rejoin with full country c data set with observations
                            # for food item i
                            right_join(c %>% filter(item_code == i),
                                       by = join_by(hh_id, hh_wgt, country, dens_cat,
                                                    hh_children, hh_sizeoecdm,
                                                    hh_inctot, hh_exptot, hh_sexhead,
                                                    hh_agehead, item_code, expenditures,
                                                    quantity, z1, z2, z3, z4, z5,
                                                    hh_norig))
                        )
                      }) %>%
                 bind_rows() %>%
                 # Compute national median values of UV and adjusted UV
                 mutate(uv     = ifelse(expenditures > 0 & quantity == 0,
                                        NA,
                                        expenditures/quantity)) %>%
                 group_by(item_code) %>%
                 # Winsorize (adjusted) unit values to Q3 + 3*IQR
                 mutate(uv_q3      = quantile(uv, 0.75, na.rm = TRUE),
                        uv_adj_q3  = quantile(uv_adj, 0.75, na.rm = TRUE),
                        uv_IQR     = IQR(uv    , na.rm = TRUE),
                        uv_adj_IQR = IQR(uv_adj, na.rm = TRUE)) %>%
                 ungroup() %>%
                 mutate(uv     = ifelse(uv > uv_q3 + 3*uv_IQR,
                                        uv_q3 + 3*uv_IQR,
                                        uv),
                        uv_adj = ifelse(uv_adj > uv_adj_q3 + 3*uv_adj_IQR,
                                        uv_adj_q3 + 3*uv_adj_IQR,
                                        uv_adj)) %>%
                 select(-uv_q3, -uv_adj_q3, -uv_IQR, -uv_adj_IQR) %>%
                 # Impute (adjusted) UVs with national median for households with
                 # zero expenditures/quantities (i.e., for which no UV can be
                 # computed as no purchase has been observed)
                 mutate(uv     = ifelse(is.na(uv),
                                        median(uv,     na.rm = TRUE),
                                        uv),
                        uv_adj = ifelse(is.na(uv_adj),
                                        median(uv_adj, na.rm = TRUE),
                                        uv_adj),
                        # Impute unadjusted UV for households with positive expenditures/
                        # quantities but no adjusted UV (as n_min was not met for this
                        # item)
                        uv_adj = ifelse(is.na(uv_adj),
                                        uv,
                                        uv_adj)) %>%
                 ungroup() %>%
                 select(-matches("median_uv"))
             )
           }) %>%
  setNames(names(list_clean_data))


# _ Categorize data ------------------------------------------------------------

list_categorized_data <- 
  future_lapply(list_prices_data,
           function(c){
             c %>%
               {
                 if(c %>% distinct(country) %>% pull() == "DE")
                   left_join(., map_EVS_item_codes %>%
                               select(item_code_orig, category_code_new),
                             by = join_by("item_code" == "item_code_orig"))
                 else
                   left_join(., map_HBS_item_codes %>%
                               select(item_code_orig, category_code_new),
                             by = join_by("item_code" == "item_code_orig"))
               } %>%
               rename(category = category_code_new) %>%
               group_by(across(c("dens_cat", "category", starts_with("hh_")))) %>%
               # Compute per category mean weighted by expenditures. If all expenditures are
               # zero, use unweighted mean.
               mutate(uv           = ifelse(is.na(weighted.mean(uv,
                                                                expenditures,
                                                                na.rm = TRUE)),
                                            mean(uv, na.rm = TRUE),
                                            weighted.mean(uv,
                                                          expenditures,
                                                          na.rm = TRUE)),
                      uv_adj       = ifelse(is.na(weighted.mean(uv_adj,
                                                                expenditures,
                                                                na.rm = TRUE)),
                                            mean(uv_adj, na.rm = TRUE),
                                            weighted.mean(uv_adj,
                                                          expenditures,
                                                          na.rm = TRUE)),
                      expenditures = sum(expenditures, na.rm = TRUE),
                      quantity     = sum(quantity, na.rm = TRUE)) %>%
               ungroup() %>%
               select(-item_code) %>%
               distinct() %>%
               # Classification of food items in EVS does not coincide with HBS
               # classification. Thus, even though for some items, no quantity data
               # is available, Case (1) matching is not possible. Thus, to treat 
               # observations with missing (adjusted) per category UVs, the median
               # (adjusted) UVs for each category are imputed for Germany.
               {
                 if(c %>% distinct(country) %>% pull() == "DE")
                   group_by(., category) %>%
                   mutate(median_uv     = median(uv, na.rm = TRUE),
                          median_uv_adj = median(uv_adj, na.rm = TRUE),
                          uv            = ifelse(is.na(uv),
                                                 median_uv,
                                                 uv),
                          uv_adj        = ifelse(is.na(uv_adj),
                                                 median_uv_adj,
                                                 uv_adj)) %>%
                   ungroup() %>%
                   select(-matches("median_uv"))
                 else
                   .
               } %>%
               arrange(hh_id, as.numeric(category)) %>%
               relocate(category, .before = expenditures)
           }) %>%
  setNames(names(list_prices_data))



# _____________________________________-----------------------------------------
# KE14 data (AT) ---------------------------------------------------------------

# _ Load and prepare KE14 data (AT) --------------------------------------------

# Note: KE14 data is loaded and prepared here as the COICOP food items only 
# coincide with the HBS down to level 4. Thus, the AT level 4 food items are
# matched to the categorization applied to HBS level 5 food items

AT_clean <-
  prepare_AT_data(
    df_hh  = fread("00_data/KE14_File_detailliert/extk14_hhvar.txt",
                   sep = ";", header = TRUE,
                   data.table = FALSE),
    df_hm  = fread("00_data/KE14_File_detailliert/EXTK14_PERSVAR.txt",
                   sep = ";", header = TRUE,
                   data.table = FALSE),
    df_exp = haven::read_sas("00_data/KE14_File_detailliert/extk14_ausg_det.sas7bdat") %>%
      # Note: we use the sas7bdat table because of umlauts
      as.matrix() %>% data.frame() %>%
      mutate(ausgHH = as.numeric(gsub(" ", "", ausgHH)))
    # Note: There are 7 households that appear in the AT_hh and AT_hm datasets but
    # not the AT_exp dataset, those are excluded
  )

# Diagnostics: Check that AT does not contain observations with negative
# expenditures -> correct
AT_clean %>%
  filter(expenditures < 0) %>% nrow()


# _ Imputation of missing quantity values --------------------------------------

# Construct vector of countries in same region as AT
region_AT <- EU27 %>%
  filter(region == EU27 %>%
           filter(geo == "AT") %>%
           select(region) %>% pull()) %>%
  select(geo) %>% pull()

# Construct matching pool for AT from countries in region_AT
match_pool_r <- list_categorized_data[names(list_categorized_data) %in% region_AT] %>%
  bind_rows(.id = "country") %>% 
  filter(category >= 6 & category <= 9) %>%
  group_by(hh_id) %>%
  # compute total expenditures across meat / animal product categories
  summarize(expenditures_meat = sum(expenditures, na.rm = TRUE)) %>%
  uncount((9-6)+1) %>%
  group_by(hh_id) %>%
  mutate(category = (6-1)+row_number()) %>% ungroup() %>%
  # Re-add non-meat categories to reduced data set
  right_join(list_categorized_data[names(list_categorized_data) %in% region_AT] %>%
               bind_rows(.id = "country") %>%
               arrange(country, hh_id, category) %>%
               select(-matches("_meat")),
             by = join_by("hh_id", "category")) %>%
  arrange(country, hh_id, category) %>%
  # keep only observations with positive expenditures and quantities as only
  # AT observations with positive expenditures will be imputed with matching
  filter(expenditures > 0 & quantity > 0) %>%
  # add AT
  bind_rows(AT_clean) %>%
  mutate(across(c("hh_inctot", "expenditures", "expenditures_meat"),
                ~ c(scale(., center = TRUE, scale = TRUE)),
                .names = "{.col}_std"))

df_matched_AT <-
  # For each category
  sapply(AT_clean %>%
           distinct(category) %>% pull(),
         function(cat){
           message(cat)
           
           # Construct a (regional) matching pool
           match_pool_r_cat <- match_pool_r %>%
             filter(country != "AT" & category == cat)
           
           # Construct item-specific dataset with households to be matched
           hh_data <- match_pool_r %>%
             filter(country == "AT" & category == cat)
           
           # Set number of nearest-neighbor matches
           k <- 10
           
           
           # If category is one of four meat categories, ...
           if(cat >= 6 & cat <= 9){
             
             # Select variables to match on
             x <- c("hh_inctot_std", "expenditures_meat_std", "z1", "z2", "z3",
                    "z5")
             
             # Find k nearest neighbors of household hh w.r.t. income,
             # meat expenditures, and socio-demographics z
             nn <- get.knnx(match_pool_r_cat %>%
                              select(all_of(x)),
                            hh_data %>%
                              select(all_of(x)),
                            k = k)
             return(
               # Select matched households in match_pool_r_i (possibly
               # multiple times): for each household to be imputed, k
               # households from the matching pool are selected
               match_pool_r_cat[nn$nn.index %>% t() %>% as.vector, ] %>%
                 select(quantity, expenditures, expenditures_meat) %>%
                 mutate(weight  = 1/(nn$nn.dist %>% t %>% as.vector),
                        # Add hh_ids of households to be imputed
                        hh_id =  rep(hh_data %>% pull(hh_id), each = k)) %>%
                 group_by(hh_id) %>%
                 # Compute mean quantity and expenditures of k nearest
                 # neighbors of household to be imputed weighted by
                 # distance
                 summarize(quantity          = weighted.mean(quantity,
                                                             weight,
                                                             na.rm = TRUE),
                           expenditures      = weighted.mean(expenditures,
                                                             weight,
                                                             na.rm = TRUE),
                           expenditures_meat = weighted.mean(expenditures_meat,
                                                             weight,
                                                             na.rm = TRUE)) %>%
                 # Compute scaling factor for quantity, i.e., the ratio
                 # of the expenditures of the household to be imputed and
                 # the weighted mean expenditures of the k nearest neighbors
                 mutate(scaling = hh_data %>% pull(expenditures_meat) / expenditures_meat,
                        # Scale the imputation quantity and expenditures
                        quantity_imputed = quantity * scaling,
                        expenditures_imputed = expenditures * scaling) %>%
                 select(hh_id, quantity_imputed, expenditures_imputed)
             )
             
             # If category is a non-meat category, ...
           } else {
             
             # Select variables to match on
             x <- c("hh_inctot_std", "expenditures_std", "z1", "z2", "z3", "z5")
             
             # Find k nearest neighbors of household hh w.r.t. income,
             # meat expenditures, and socio-demographics z
             nn <- get.knnx(match_pool_r_cat %>%
                              select(all_of(x)),
                            hh_data %>%
                              filter(expenditures > 0) %>%
                              select(all_of(x)),
                            k = k)
             
             return(
               # Select matched households in match_pool_r_i (possibly
               # multiple times): for each household to be imputed, k
               # households from the matching pool are selected
               match_pool_r_cat[nn$nn.index %>% t() %>% as.vector, ] %>%
                 select(quantity, expenditures) %>%
                 mutate(weight  = 1/(nn$nn.dist %>% t %>% as.vector),
                        # Add hh_ids of households to be imputed
                        hh_id =  rep(hh_data %>%
                                       filter(expenditures > 0) %>%
                                       pull(hh_id),
                                     each = k)) %>%
                 group_by(hh_id) %>%
                 # Compute mean quantity and expenditures of k nearest
                 # neighbors of household to be imputed weighted by
                 # distance
                 summarize(quantity     = weighted.mean(quantity,
                                                        weight,
                                                        na.rm = TRUE),
                           expenditures = weighted.mean(expenditures,
                                                        weight,
                                                        na.rm = TRUE)) %>%
                 # Compute scaling factor for quantity, i.e., the ratio
                 # of the expenditures of the household to be imputed and
                 # the weighted mean expenditures of the k nearest neighbors
                 mutate(scaling = hh_data %>%
                          filter(expenditures > 0) %>%
                          pull(expenditures) / expenditures,
                        # Scale the imputation quantity
                        quantity_imputed = quantity * scaling) %>%
                 select(hh_id, quantity_imputed)
             )
           }
         },
         simplify = FALSE) %>%
  bind_rows(.id = "category") %>%
  mutate(category = as.numeric(category)) %>%
  arrange(hh_id, category)

# Impute matched quantity data
AT_imp <- 
  AT_clean %>%
  left_join(df_matched_AT,
            by = join_by(hh_id, category)) %>%
  mutate(quantity     = ifelse(!is.na(quantity_imputed),
                               quantity_imputed,
                               quantity),
         expenditures = ifelse(!is.na(expenditures_imputed),
                               expenditures_imputed,
                               expenditures),
         # naïve imputation for zero-expenditure observations
         quantity     = ifelse(expenditures == 0,
                               0,
                               quantity)) %>%
  select(-expenditures_imputed, -quantity_imputed, -expenditures_meat) %>%
  relocate(category, .before = expenditures) %>%
  relocate(quantity, .after  = expenditures)


# _ Unit value computation -----------------------------------------------------

# Specify independent variables for UV-adjustment regression
x <- c("dens_cat", "hh_children", "hh_sizeoecdm", "hh_inctot", "hh_agehead",
       "hh_sexhead")

AT_prices <- 
  sapply(AT_imp %>%
           # Compute UV for observations with positive expenditures and
           # quantities
           mutate(uv = ifelse(expenditures == 0 & quantity == 0,
                              NA,
                              expenditures / quantity)) %>%
           # Split into list to apply function to each food category
           split(., f = .$category),
         function(data_cat){
           eq <- as.formula("log(uv) ~ "%&%paste(x, collapse = "+"))
           
           lm <- lm(eq, data_cat, na.action = "na.exclude")
           
           return(
             data_cat %>%
               filter(complete.cases(.)) %>%
               mutate(resid         = lm$residuals,
                      # Compute adjusted UV: regression constant + residual
                      uv_adj        = exp(lm$coefficients[1] + resid)) %>%
               right_join(data_cat,
                          by = join_by(hh_id, hh_wgt, country, dens_cat, hh_children,
                                       hh_sizeoecdm, hh_inctot, hh_exptot, hh_sexhead,
                                       hh_agehead, category, expenditures, quantity,
                                       z1, z2, z3, z4, z5, hh_norig, uv)) %>%
               # Winsorize (adjusted) unit values to Q3 + 3*IQR
               group_by(category) %>%
               mutate(uv_q3      = quantile(uv, 0.75, na.rm = TRUE),
                      uv_adj_q3  = quantile(uv_adj, 0.75, na.rm = TRUE),
                      uv_IQR     = IQR(uv    , na.rm = TRUE),
                      uv_adj_IQR = IQR(uv_adj, na.rm = TRUE)) %>%
               ungroup() %>%
               mutate(uv     = ifelse(uv > uv_IQR,
                                      uv_q3 + 3*uv_IQR,
                                      uv),
                      uv_adj = ifelse(uv_adj > uv_adj_IQR,
                                      uv_adj_q3 + 3*uv_adj_IQR,
                                      uv_adj)) %>%
               select(-uv_q3, -uv_adj_q3, -uv_IQR, -uv_adj_IQR) %>%
               # Impute median (adjusted) UV for missing (adjusted) UVs
               mutate(uv     = ifelse(is.na(uv),
                                      median(uv,     na.rm = TRUE),
                                      uv),
                      uv_adj = ifelse(is.na(uv_adj),
                                      median(uv_adj, na.rm = TRUE),
                                      uv_adj)) %>%
               select(-resid)
           )
         },
         simplify = FALSE) %>%
  bind_rows(.id = "category") %>%
  arrange(hh_id, as.numeric(category))


# _____________________________________-----------------------------------------
# Treat implausible observations in general ------------------------------------

list_final <-
  sapply(c(list_categorized_data, list(AT = AT_prices)),
         function(c){
           
           prev_n <- nrow(c)
           
           # Drop observations with total food expenditures = 0
           out1 <- c %>%
             group_by(hh_id) %>%
             summarize(total_food_exp = sum(expenditures)) %>%
             filter(total_food_exp == 0) %>%
             distinct(hh_id) %>% pull()
           
           message((c[1, ] %>% pull(country))%&%": We remove "%&%length(out1)%&%
                   " out of "%&%prev_n%&%" households because total food "%&%
                   "expenditures are 0")
           
           # Drop observations with food expenditures/total expenditures >0.75
           out2 <- c %>%
             group_by(hh_id) %>%
             mutate(total_food_exp = sum(expenditures)) %>%
             ungroup() %>%
             filter(total_food_exp/hh_exptot > 0.75) %>%
             distinct(hh_id) %>% pull()
           
           message((c[1, ] %>% pull(country))%&%": We remove "%&%length(out2)%&%
                   " out of "%&%prev_n%&%" households because food expenditures "%&%
                   "constitute more than 75% of total expenditures")
           
           print_color((c[1, ] %>% pull(country))%&%": In total we remove "%&%
                   length(union(out1, out2))%&%" out of "%&%
                   prev_n%&%" households \n",
                   color = "cyan")
           
           return(c %>%
                    filter(!hh_id %in% out1 & !hh_id %in% out2))
         },
         simplify = FALSE)  %>% .[order(names(.))]



# _____________________________________-----------------------------------------
# Save data --------------------------------------------------------------------

#_ Data source as well as original and final N
data_info <- 
  lapply(c("keep2010", "keep2015"),
         function(keep){
           tibble(
             country            = eval_obj(keep),
             data_source        = "HBS"%&%(keep %>% sub("keep", "", .)),
             n_orig             = lapply(list_final[eval_obj(keep)],
                                         function(c){
                                           c %>% slice(1) %>% pull(hh_norig)
                                         }) %>% unlist(),
             n_final            = lapply(list_final[eval_obj(keep)],
                                         function(c){
                                           c %>% distinct(hh_id) %>% nrow()
                                         }) %>% unlist(),
             quantities_imputed = ifelse(country %in% c(matching2010, matching2015),
                                         "yes",
                                         "no"),
             items_imputed      = c_vecs[[keep %>% sub("keep", "", .)]]$items_avail %>%
               filter(country %in% eval_obj(keep)) %>%
               group_by(country) %>%
               summarize(n_avail = sum(exp_quan_avail),
                         n_total = n()) %>%
               mutate(items_imputed = round(((n_total-n_avail) / n_total)*100)%&%" %") %>%
               select(items_imputed) %>% pull
           )
         }) %>%
  bind_rows() %>%
  rbind(.,
        c("DE",
          "EVS",
          list_final$DE[1, ] %>% pull(hh_norig),
          list_final$DE %>% distinct(hh_id) %>% nrow(),
          "no",
          "0 %"),
        c("AT",
          "KE14",
          list_final$AT[1, ] %>% pull(hh_norig),
          list_final$AT %>% distinct(hh_id) %>% nrow(),
          "yes",
          "100 %")) %>%
  arrange(country)

write.csv(data_info, "../build/data/intermediate_output/data_info.csv")

#_ Data files
path <- config$procdatapath%&%"DSE_proc/clean_datasets/"

if(!dir.exists(file.path(path))){
  dir.create(file.path(path), recursive = TRUE)
}

future_lapply(list_final,
         function(x){
           
           c <-  x[1, ] %>% pull(country)
           
           write.csv(x, path%&%c%&%".csv", row.names = FALSE)
         })

# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------