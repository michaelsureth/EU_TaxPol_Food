# File info --------------------------------------------------------------------

# File:    Welfare analysis
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption

# _____________________________________-----------------------------------------
# Preparations -----------------------------------------------------------------

# Load functions
source("99_functions/welfare_functions.R")

# Set configpath
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

# Create coef path
coefpath <- config$procdatapath%&%"easi_estimates/coefficients/"%&%
  configpath%&%"/"

# Create resultdata path
resultdatapath <- config$procdatapath%&%"DSE_proc/result_datasets/"%&%
  configpath%&%"/"

# Load categorization of food items
catexplain <- fread("../build/data/intermediate_output/"%&%
                      "HBS_catexplain_"%&%config$categorization%&%".csv",
                    data.table = FALSE)


# Load selected stressors and impacts
stressors_impacts_selected <- fread("../build/data/intermediate_output/"%&%
                                      "MRIO_stressors_impacts_final.csv",
                                    data.table = FALSE) %>%
  select(-c("Landusetype", "Landusetype_coeff"))

# Define policy of interest
policy <- "tax_GHG emissions"

# _____________________________________-----------------------------------------
# Log-cost-of-living index (lcol) ----------------------------------------------

# loop over policies and countries
for(pol in c("VAT_increase", policy)){
  
  message(pol)
  
  # available coefficient estimates
  files <- list.files(coefpath)
  
  # create empty list to store results
  lcol <- list()
  mean_lcol <- list()
  
  for(f in files){
    
    # _ Load data --------------------------------------------------------------
    
    # extract country abbreviation from file name
    c <- substr(f, 1, 2)
    
    # load estimated demand system coefficients of country c
    coefs <- fread(coefpath%&%f,
                   data.table = FALSE, skip = 2) %>%
      column_to_rownames("V1") %>%
      rename(value = V2)
    
    # load final household data of country c
    file_result_c <- list.files(resultdatapath,
                                pattern = "_"%&%c%&%"_")
    data_c <- fread(resultdatapath%&%file_result_c,
                    data.table = FALSE)
    
    # load budget shares (w_i) of households (h) in country c
    w_hi <- data_c %>% select(matches("^s[[:digit:]]"))
    
    # load total real expenses of households (h) in country c
    y_h <- data_c %>% select(starts_with("log_y1"))
    
    # load sociodemographic characteristics (z_l) of households (h) in country c
    z_hl <- data_c %>% select(matches("^origz[[:digit:]]"))
    
    # number of food expenditure categories (number of equations in demand system)
    neq <- ncol(w_hi)
    
    # number of household characteristics considered (number of demographics)
    ndem <- ncol(z_hl)
    
    # vector of demographic characteristics (Italy does not have income-z)
    if(c == "IT"){
      ndem_vec <- c(1:ndem)[-4]
    }else{
      ndem_vec <- c(1:ndem)
    }
    
    # load household weights
    weights <- data_c %>% select(hh_wgt) %>% pull()
    
    # load price change in percent (app. log(p1)-log(p0))
    diff_p1_p0 <- fread("../build/data/policies/"%&%pol%&%"/"%&%configpath%&%
                          "_"%&%config$year_io%&%"/reduction_"%&%c%&%".csv",
                        data.table = FALSE) %>%
      select(price_increase_rel) %>% slice(1:nrow(catexplain)) %>% pull()
    
    
    # _ Computation (following Lewbel & Pendakur (2009) ------------------------
    
    # (1) construction of sum_i w_i (p1_i - p0_i)
    E_wi_p1p0 <- 0
    for(i in 1:neq){
      E_wi_p1p0 <- E_wi_p1p0 + w_hi[ , i] * diff_p1_p0[i]
    }
    
    # (2) construction of 'sum_i sum_j b_ij logy (p1_i-p0_i)*(p1_j-p0_j)'                      
    # zeta parameters (p-y interaction coefficients) - b
    EE_b_y_p1p0_p1p0 <- 0
    for (i in 1:neq) {
      for (j in 1:neq) {
        EE_b_y_p1p0_p1p0 <- EE_b_y_p1p0_p1p0 +
          coefs["b_restr_i"%&%i%&%"j"%&%j, ] * 
          unlist(y_h["log_y1"%&%i],use.names=FALSE) * 
          diff_p1_p0[i] * diff_p1_p0[j]
      }
    }
    
    # (3) construction of 'sum_i sum_j sum_l a_ijl z_l (p1_i-p0_i)*(p1_j-p0_j)
    
    # gamma parameters (p coefficients) - a
    tot_gamma <- 0
    for (i in 1:neq){
      for (j in 1:neq){
        tot_gamma <- tot_gamma +
          coefs["a_restr_i"%&%i%&%"j"%&%j, ] * 
          diff_p1_p0[i] * diff_p1_p0[j]
      }
    }
    
    # theta parameters (p-z interaction coefficients)  - e           
    tot_theta <- 0
    if(config$pzint == "pzintyes"){
      for (i in 1:neq){
        for (j in 1:neq){
          for(l in ndem_vec){
            tot_theta <- tot_theta +
              coefs["e_restr_i"%&%i%&%"j"%&%j%&%"_z"%&%l, ] * 
              z_hl[ , l] * 
              diff_p1_p0[i] * diff_p1_p0[j]
          }
        }
      }
    }
    
    # add gamma and theta for total second order price effect
    EEE_a_z_p1p0_p1p0 <- tot_gamma + tot_theta
    
    # (4) sum for log cost of living index
    lcol[[c]] <- data.frame(hh_id = data_c$hh_id,
                            hh_wgt = data_c$hh_wgt,
                            lcol  = E_wi_p1p0 + 
                              0.5*(EE_b_y_p1p0_p1p0 + EEE_a_z_p1p0_p1p0)
    ) %>%
      # add total food expenditure
      left_join(data_c[,c("total_exp", "hh_id")], by = c("hh_id")) %>%
      # compute absolute cost of living
      mutate(lcol_abs = lcol*total_exp)
    
    # compute weighted average
    mean_lcol[[c]] <- weighted.mean(lcol[[c]]$lcol_abs, weights) %>%
      tibble() %>% rename(mean_lcol_abs = ".")
    
    # _ Save results ------------------------------------------------------
    
    # Create results directory if required
    if(!dir.exists("../build/data/welfare/"%&%configpath%&%"_"%&%config$year_io%&%
                   "/"%&%pol%&%"/")){
      dir.create("../build/data/welfare/"%&%configpath%&%"_"%&%config$year_io%&%
                   "/"%&%pol%&%"/",
                 recursive = TRUE)
    }
    
    # save results
    write.csv(lcol[[c]],
              "../build/data/welfare/"%&%configpath%&%"_"%&%config$year_io%&%
                "/"%&%pol%&%"/"%&%c%&%"_hhs.csv",
              row.names = FALSE)
    
  } # close country loop
  
  write.csv(mean_lcol %>% bind_rows(.id = "country"),
            "../build/data/welfare/"%&%configpath%&%"_"%&%config$year_io%&%
              "/"%&%pol%&%"/"%&%"country-means_lcol.csv",
            row.names = FALSE)
  
} # close policy loop

# _____________________________________-----------------------------------------
# Tax income -------------------------------------------------------------------

# _ Preparations and load data -------------------------------------------------

# specify low or high VAT estimate
sel <- "high"

# specify meat categories
meat <- c(6, 7, 8, 9)

# Load VAT rates by category and by country
df_VAT <- read_excel("00_data/manual_input/VAT_rates.xlsx",
                     col_types = c("text", "numeric", "numeric", "text", "text",
                                   "text", "text","text", "text","text", "text",
                                   "text", "text","text", "text"),
                     sheet     = "Sheet1")[ ,c(1:2, 6:15)] %>%
  {
    if(sel == "low")
      mutate_at(., vars(-country), ~as.numeric(gsub("\\ to.*", "", .)))
    else if(sel == "high")
      mutate_at(., vars(-country), ~as.numeric(gsub("^[^_]*to ", "", .)))
  } %>%
  pivot_longer(cols      = -c(country, VAT_standard_2023), 
               names_to  = "category", 
               values_to = "VAT_reduced") %>%
  mutate(across(starts_with("VAT"), ~./100)) %>%
  mutate(category = as.integer(category))

# Load footprint intensities
intensities <- fread("../build/data/intensities/"%&%config$year_io%&%
                       "/intensities_by_foodcat.csv",
                     header = TRUE, data.table = FALSE) %>%
  mutate(demandcountry = ifelse(demandcountry == "GR",
                                "EL",
                                demandcountry)) %>%
  arrange(demandcountry, category, impact_no) %>%
  filter(impact_no == 67) # impact number 67 is GHG emissions

# Load GHG emission price level 
GHG_price <- fread("../build/data/policies/"%&%policy%&%"/"%&%configpath%&%
                     "_"%&%config$year_io%&%"/"%&%
                     list.files("../build/data/policies/"%&%policy%&%"/"%&%
                                  configpath%&%"_"%&%config$year_io)[1]) %>%
  select(env_tax) %>% slice(1) %>% pull()


# _ Compute additional tax income generated per household ------------------------
tax_inc <-
  # For each country...
  future_lapply(countries, function(country){
    
    c <- country
    
    # Load VAT tax rates (reduced and standard)
    df_VAT <- df_VAT %>%
      filter(country == c) %>%
      select(category,
             r_0 = VAT_reduced,
             r_1 = VAT_standard_2023)
    
    # Load GHG emission intensity
    df_mu <- intensities %>%
      filter(demandcountry == c) %>%
      select(category, intensity_new)
    
    # Load household data containing price semi-elasticities
    file <- list.files(config$procdatapath%&%
                         "DSE_proc/result_datasets/"%&%configpath%&%"/") %>%
      as_tibble() %>%
      filter(grepl(c, .$value)) %>%
      pull()
    
    # Extract semi-elasticities
    df_semielas <- fread(config$procdatapath%&%
                           "DSE_proc/result_datasets/"%&%configpath%&%"/"%&%file,
                         data.table = FALSE) %>%
      select(hh_id, matches("semielas")) %>%
      # for each food category i, extract own- and cross-price semi-elasticities j
      lapply(1:10, function(i, df){
        df %>%
          select(starts_with("semielas_p_i"%&%i%&%"j")) %>%
          rename_at(vars(everything()), ~"semielas_j"%&%as.character(1:10)) %>%
          bind_cols(df %>%
                      select(hh_id), .)
      },
      df = .) %>%
      bind_rows(.id = "category") %>%
      mutate(category = as.numeric(category)) %>%
      select(hh_id, category, everything()) %>%
      arrange(hh_id, category)
    
    # Define policy vector to "loop over"
    pol <- c("VAT_increase", "tax_GHG emissions")
    
    # Load household data set
    fread(config$procdatapath%&%
            "DSE_proc/clean_datasets/"%&%"/"%&%c%&%".csv",
          data.table = FALSE) %>%
      select(hh_id, hh_wgt, category, expenditures) %>%
      # Join with semi-elasticities, VAT rates, and intensities
      left_join(df_semielas,
                by = join_by("hh_id", "category")) %>%
      left_join(df_VAT, by = "category") %>%
      left_join(df_mu,  by = "category") %>%
      group_by(hh_id) %>%
      # Compute pre-policy budget shares for each category i
      mutate(w_0 = expenditures / sum(expenditures)) %>%
      ungroup() %>%
      # Compute relative price change induced by VAT reform and GHG emission price
      mutate(delta_p_rel_VAT = ifelse(category %in% meat,
                                      (1+r_1)/(1+r_0) - 1,
                                      0),
             delta_p_rel_GHG = intensity_new * GHG_price,
             # "rename" pre-policy expenditures per category to x_0
             x_0             = expenditures,
             # compute absolute VAT paid per household pre policy
             VAT_0           = x_0 * r_0/(1+r_0)) %>%
      # For each policy
      lapply(pol, function(pol, df){
        df %>%
          mutate(policy = pol) %>%
          # rename relative price change
          {
            if(pol == "VAT_increase")
              mutate(., delta_p_rel = delta_p_rel_VAT)
            else if(pol == "tax_GHG emissions")
              mutate(., delta_p_rel = delta_p_rel_GHG)
          } %>%
          # Add price changes of all categories j to each category i in wide format
          left_join({.} %>%
                      select(hh_id, category, delta_p_rel) %>%
                      pivot_wider(names_from   = category,
                                  values_from  = delta_p_rel,
                                  names_prefix = "delta_p_rel_"),
                    by = "hh_id") %>%
          select(-delta_p_rel) %>%
          # pivot to long format
          pivot_longer(cols         = starts_with("semielas_"),
                       names_to     = "j",
                       names_prefix = "semielas_j",
                       values_to    = "semielas") %>%
          # Add relative price changes of all categories j to each category i in long format
          left_join({.} %>%
                      select(hh_id, category, starts_with("delta_p_rel_")) %>%
                      distinct() %>%
                      pivot_longer(cols         = starts_with("delta_p_rel_"),
                                   names_to     = "j",
                                   names_prefix = "delta_p_rel_",
                                   values_to    = "delta_p_rel"),
                    by = c("hh_id", "category", "j")) %>%
          select(-starts_with("delta_p_rel_")) %>%
          # For each category i compute new expenditures per category i:
          # x_i1 = \sum_j ((\Delta p_j / p_j) * (\partial w_i / \partial p_j) * x_i0) + x_i0
          group_by(hh_id, category) %>%
          mutate(x_1 = sum(delta_p_rel * semielas * x_0) + x_0) %>%
          ungroup() %>%
          select(-c("j", "semielas", "delta_p_rel")) %>%
          distinct() %>%
          # For each policy compute VAT and GHG emission price paid per household
          {
            if(pol == "VAT_increase")
              mutate(., VAT_1 = ifelse(category %in% meat,
                                       # to compute post-policy VAT paid use standard 
                                       # tax rate, if i is meat category
                                       x_1 * r_1/(1+r_1),
                                       # else use pre-policy VAT
                                       x_1 * r_0/(1+r_0)),
                     GHG_inc = 0)
            else if(pol == "tax_GHG emissions")
              # use pre-policy VAT to compute VAT paid in GHG emission price policy scenario
              mutate(., VAT_1 = x_1 * r_0/(1+r_0),
                     # compute GHG emission price paid using GHG emission intensity per EUR
                     # and GHG emission price
                     GHG_inc  = x_1 * 1/(1+r_0) * intensity_new * GHG_price)
          } %>%
          # Compute difference in VAT paid per household pre and post policy
          mutate(VAT_diff = VAT_1 - VAT_0) %>%
          # sum over categories
          group_by(hh_id, hh_wgt) %>%
          mutate(x_0      = sum(x_0),
                 x_1      = sum(x_1),
                 VAT_0    = sum(VAT_0),
                 VAT_1    = sum(VAT_1),
                 VAT_diff = sum(VAT_diff),
                 GHG_inc  = sum(GHG_inc)) %>%
          ungroup() %>%
          distinct(hh_id, hh_wgt, r_0, r_1, x_0, x_1,
                   VAT_0, VAT_1, VAT_diff, GHG_inc, policy)
      }, df = .) %>%
      bind_rows()
  }) %>%
  setNames(countries) %>%
  bind_rows(.id = "country")

# _ Save results ---------------------------------------------------------------
fwrite(tax_inc,
       "../build/data/welfare/"%&%configpath%&%"_"%&%config$year_io%&%
         "/tax_paid_changes.csv")


# by country
fwrite(tax_inc %>%
         group_by(policy, country) %>%
         summarize(mean_VAT_diff = weighted.mean(VAT_diff, hh_wgt),
                   mean_GHG_inc  = weighted.mean(GHG_inc, hh_wgt),
                   # median_VAT_diff = median(VAT_diff),
                   # median_GHG_inc = median(GHG_inc),
                   # nhh           = sum(hh_wgt),
                   sum_VAT_diff  = sum(VAT_diff * hh_wgt),
                   sum_GHG_inc   = sum(GHG_inc * hh_wgt)),
       "../build/data/welfare/"%&%configpath%&%"_"%&%config$year_io%&%
         "/tax_paid_changes_bycountry.csv")

# EU total
fwrite(tax_inc %>%
         summarize(mean_VAT_diff = weighted.mean(VAT_diff, hh_wgt),
                   mean_GHG_inc  = weighted.mean(GHG_inc, hh_wgt),
                   # median_VAT_diff = median(VAT_diff),
                   # median_GHG_inc = median(GHG_inc),
                   # nhh           = sum(hh_wgt),
                   sum_VAT_diff  = sum(VAT_diff * hh_wgt),
                   sum_GHG_inc   = sum(GHG_inc * hh_wgt)),
       "../build/data/welfare/"%&%configpath%&%"_"%&%config$year_io%&%
         "/tax_paid_changes_EUtotal.csv")

# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------