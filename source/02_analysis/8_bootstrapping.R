# File info --------------------------------------------------------------------

# File:    Bootstrapped estimation of LA-EASI
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption


# _____________________________________-----------------------------------------
# Preparations ---------------------------------------------------------------

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

# create bootstrapping path
bootpath <- config$procdatapath%&%"bootstrap/"%&%configpath%&%"_"%&%
  config$year_io%&%"/"


# _ Categorization of food items -----------------------------------------------

# Load names for categories
catexplain <- fread("../build/data/intermediate_output/"%&%
                      "HBS_catexplain_"%&%config$categorization%&%".csv",
                    data.table = FALSE)

# Save number of categories
n_cat <- length(catexplain$category_code_new) 

# Save all available category codes
category_vec <- catexplain$category_code_new


# Load selected stressors and impacts
stressors_impacts_selected <- fread("../build/data/intermediate_output/"%&%
                                      "MRIO_stressors_impacts_final.csv",
                                    data.table = FALSE) %>%
  select(-c("Landusetype", "Landusetype_coeff"))


# _ Load VAT increases by country ----------------------------------------------

df_VAT <- read_excel("00_data/manual_input/VAT_rates.xlsx",
                     col_types = c("text", "numeric", "numeric", "text", "text",
                                   "text", "text","text", "text","text", "text",
                                   "text", "text","text", "text"),
                     sheet     = "Sheet1")

# Specify food categories affected by VAT increase
meat <- c("Beef", "Pork", "Poultry", "Other meat/animal products")



# _ Load household data --------------------------------------------------------

c_clean <- 
  future_lapply(countries, function(c){
    
    return(fread(config$procdatapath%&%"DSE_proc/clean_datasets/"%&%c%&%".csv",
                 data.table = FALSE, showProgress = FALSE))
  }) %>%
  setNames(countries)



# _ Set EASI parameters --------------------------------------------------------

# set category of which price is used to normalize other prices with
p_to_norm <- paste0("logp", config$p_to_norm)

# Define vector of independent variables for first stage of correction of
# censored distribution of expenditures following Shonkwiler and Yen (1999)
s <- c("hh_sizeoecdm", "hh_inctot", "hh_agehead", "hh_sexhead", "dens_cat")


# _ Load footprint data --------------------------------------------------------

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



# _ Load footprint intensities -------------------------------------------------

intensities <- fread("../build/data/intensities/"%&%config$year_io%&%
                       "/intensities_by_foodcat.csv",
                     header = TRUE, data.table = FALSE) %>%
  mutate(demandcountry = ifelse(demandcountry == "GR",
                                "EL",
                                demandcountry)) %>%
  arrange(demandcountry, category, impact_no)


# Set number of bootstrapping runs according to config-file
nboot_start <- as.numeric(config$nbootstart)
nboot_end <- as.numeric(config$nbootend)


# Create country-bootstrapping sample number dataframe to "loop" over
nboot_country <- data.frame(nboot   = rep(nboot_start:nboot_end,
                                          length(countries)) %>% sort(),
                            country = rep(countries,
                                          nboot_end)) %>%
  split(., 1:nrow(.))

# if bootstrap sample estimates already exist, adjust nboot_country dataframe
if(!is.empty(list.files(bootpath))){
  # Check which bootstrap runs are already available and adjust nboot_country
  # accordingly
  temp_c <- list.files(bootpath)
  
  tmp1 <-   lapply(temp_c, function(c){
    list.files(bootpath%&%c, pattern = "^(fp_reduction_abs)", recursive = TRUE) %>% 
      sub("sample", "", .) %>%
      sub("/fp_reduction_abs.csv", "", .) %>%
      as.numeric()
  }) %>%
    setNames(temp_c %>% sub("temp_", "", .))
  
  tmp2 <- lapply(names(tmp1), function(c){
    data.frame(country = rep(c, length(tmp1[[c]])),
               nboot   = as.vector(tmp1[[c]])) %>%
      arrange(nboot)
  }) %>%
    bind_rows()
  
  nboot_country <- nboot_country %>%
    bind_rows() %>%
    left_join(tmp2, keep = TRUE) %>%
    filter(is.na(country.y) | is.na(nboot.y)) %>%
    select(nboot = nboot.x, country = country.x) %>%
    split(., 1:nrow(.))
}; rm(temp_c, tmp1, tmp2)

# as some Stata instances fail to write output in parallel, repeat bootstrapping
# function until specified number of samples have been estimated and computed
while(nrow(nboot_country[[1]]) > 0){
  
  future_lapply(nboot_country, function(nboot_country){
    
    # extract number of bootstrap sample, country, and country-specific footprints
    nboot        <- nboot_country$nboot
    c            <- nboot_country$country
    
    message("Country "%&%c)
    message("Bootstrap sample "%&%nboot)
    
    # Split household dataset into list to sample in long-format
    df_clean <- c_clean[[c]] %>%
      split(., .$hh_id)
    
    # number of demographic characteristics
    ndem <- df_clean[[1]] %>%
      dplyr::select(starts_with("z")) %>% ncol()
    
    # demographic variable names
    dem_vars <- df_clean[[1]] %>%
      select(starts_with("z")) %>% colnames()
    
    # Create temporary directory for data and .do-file and estimation results
    dir.create(bootpath%&%"temp_"%&%c%&%"/sample"%&%nboot,
               recursive = TRUE)
    
    # Sample N households with replacement
    set.seed(nboot)
    sample <- sample(c(1:length(df_clean)), length(df_clean), replace = TRUE)
    
    # Create final dataset by binding sampled households together
    df_boot <- df_clean[sample] %>%
      setNames(1:length(.)) %>%
      bind_rows(.id = "hh_id_new") %>%
      mutate(hh_id = c%&%"_"%&%hh_id_new) %>%
      select(-hh_id_new)
    
    # remove df_clean and sample
    rm(df_clean, sample)
    
    # Generate EASI variables and compute cdf and pdf for censoring correction
    # according to Shonkwiler and Yen (1999)
    df_final <- prepare_easi(df_boot, s, dem_vars)
    
    # remove df_clean and sample
    rm(df_boot)
    
    # Write data set to read into Stata
    fwrite(df_final,
           file = bootpath%&%"temp_"%&%c%&%"/sample"%&%nboot%&%"/data_temp.csv")
    
    # Copy .do-file to temporary location
    readLines("02_analysis/2_stata_restricted_easi.do") %>%
      # adjust data path in .do-file
      sub("cd path_placeholder", "cd \""%&%bootpath%&%"temp_"%&%c%&%"/sample"%&%nboot%&%"/\"", .) %>%
      writeLines(bootpath%&%"temp_"%&%c%&%"/sample"%&%nboot%&%"/stata_temp.do")
    
    
    # Copy config file to temporary location
    file.copy(from = "config.do",
              to   = bootpath%&%"temp_"%&%c%&%"/sample"%&%nboot%&%"/config.do")
    
    # Execute Stata .do-file
      stata(bootpath%&%"temp_"%&%c%&%"/sample"%&%nboot%&%"/stata_temp.do",
            stata.path    = config$statapath,
            stata.version = config$stataversion,
            stata.echo    = FALSE)
    
    # Remove temporary files
    file.remove(bootpath%&%"temp_"%&%c%&%"/sample"%&%nboot%&%"/data_temp.csv",
                bootpath%&%"temp_"%&%c%&%"/sample"%&%nboot%&%"/stata_temp.do",
                bootpath%&%"temp_"%&%c%&%"/sample"%&%nboot%&%"/config.do")
    
    
    # _ Compute elasticities ---------------------------------------------------
    
    # "Catch error" condition: if Stata instance does not return estimated
    # coefficients, bootstrap sample run will be ignored
    if(file.exists(bootpath%&%"temp_"%&%c%&%"/sample"%&%
                   nboot%&%"/coefs.csv")){
      
      elas_list <- compute_elasticities(df_final,
                                        df_coefs = read.csv(bootpath%&%"temp_"%&%c%&%
                                                              "/sample"%&%nboot%&%
                                                              "/coefs.csv",
                                                            skip = 1) %>%
                                          pivot_wider(names_from  = X,
                                                      values_from = mean),
                                        c, ndem, catexplain, n_cat)
      
      for(i in c("cpe_comp", "cpe_uncomp", "ee")){
        fwrite(elas_list[[i]],
               bootpath%&%"temp_"%&%c%&%"/sample"%&%nboot%&%"/"%&%i%&%".csv")
      }; rm(i) # close elasticity-loop
      
      
      # _ Compute VAT footprint reductions ---------------------------------------
      
      # Retrieve VAT increase
      VAT <- df_VAT %>% filter(country == c)
      
      # Format elasticities vector into matrix
      cpe_uncomp <- elas_list[["cpe_uncomp"]] %>%
        select(matches("mean")) %>% t() %>% as.vector() %>%
        matrix(nrow = n_cat,
               ncol = n_cat,
               byrow = TRUE)
      
      # Compute reductions by impact
      fp_reductions <- compute_VAT_fp_reductions(cpe_uncomp,
                                                 VAT,
                                                 catexplain,
                                                 footprints = footprints[[c]],
                                                 stressors_impacts_selected) %>%
        group_by(impact_no) %>%
        summarize(footprint_reduction_abs = sum(footprint_reduction_abs)) %>%
        pivot_wider(names_from  = impact_no,
                    names_glue  = "impact_{impact_no}",
                    values_from = footprint_reduction_abs)
      
      # Write results
      fwrite(fp_reductions,
             bootpath%&%"temp_"%&%c%&%"/sample"%&%nboot%&%
               "/fp_reduction_abs.csv")
      
      message(c%&%" - Sample "%&%nboot%&%": coefficients, elasticities, and footprint"%&%
                " reductions computed")
      
    } else {
      message("Stata instance did not write coefs.csv. No elasticities and footprints"%&%
                " computed")
    }
    # close "catch-error" if-condition
    
    # remove all objects generated in function
    rm(nboot, c, ndem, dem_vars, df_final, elas_list, VAT,
       cpe_uncomp, fp_reductions)
  },
  future.seed = TRUE)
  
  # Check which bootstrap runs are already available and adjust nboot_country
  # accordingly
  temp_c <- list.files(bootpath)
  
  tmp1 <-   lapply(temp_c, function(c){
    list.files(bootpath%&%c, pattern = "^(fp_reduction_abs)", recursive = TRUE) %>% 
      sub("sample", "", .) %>%
      sub("/fp_reduction_abs.csv", "", .) %>%
      as.numeric()
  }) %>%
    setNames(temp_c %>% sub("temp_", "", .))
  
  tmp2 <- lapply(names(tmp1), function(c){
    data.frame(country = rep(c, length(tmp1[[c]])),
               nboot   = as.vector(tmp1[[c]])) %>%
      arrange(nboot)
  }) %>%
    bind_rows()
  
  nboot_country <- nboot_country %>%
    bind_rows() %>%
    left_join(tmp2, keep = TRUE) %>%
    filter(is.na(country.y) | is.na(nboot.y)) %>%
    select(nboot = nboot.x, country = country.x) %>%
    split(., 1:nrow(.))
  
}; rm(nboot_country) # close while-loop


# Bind bootstrap sample runs into single dataframe per country and save
# output
lapply(countries, function(c){
  lapply(c("cpe_comp", "cpe_uncomp", "ee", "fp_reduction_abs"), function(i){
    out <- list.files(bootpath%&%"temp_"%&%c,
                      pattern   = "^("%&%i%&%".csv)",
                      recursive = TRUE) %>%
      lapply(., function(sample){
        fread(bootpath%&%"temp_"%&%c%&%"/"%&%sample,
              data.table = FALSE)
      }) %>%
      bind_rows()
    
    fwrite(out,
           bootpath%&%"temp_"%&%c%&%"/"%&%"all_samples_"%&%i%&%".csv")
  })
})


# _____________________________________-----------------------------------------
# Save aggregate results -------------------------------------------------------

c_paths <- list.files(bootpath, pattern = "all_samples_fp_reduction_abs.csv",
                      recursive = TRUE)

impacts <- stressors_impacts_selected %>%
  filter(shortname %in% selectedindicators_plot) %>%
  pull(code_s_i)

# Load VAT footprint reductions per country
fp_reductions_VAT <- lapply(c_paths, function(c_path){
  fread(bootpath%&%c_path)
}) %>%
  setNames(sort(countries)) %>%
  bind_rows(.id = "country")

# Aggregate across countries per bootstrap sample runs
fp_reductions_VAT_total <- lapply(impacts, function(impact_no){
  future_lapply(1:config$nbootend, function(nboot){
    fp_reductions_VAT %>%
      slice(nboot, .by = country) %>%
      select("impact_"%&%impact_no) %>%
      summarize(across(starts_with("impact"), sum))
  }) %>%
    bind_rows()
}) %>%
  bind_cols()

# Save output
fwrite(fp_reductions_VAT_total,
       bootpath%&%"reduction_VAT_EU27.csv")


# _ Compute VAT-equivalent GHG emission price level ----------------------------

# For each bootstrap sample...
GHG_price_level <- future_lapply(1:config$nbootend, function(nboot){
  
  set.seed(nboot)
  
  # Load cpe matrix for each country
  cpe <- lapply(countries, function(c){
    fread(bootpath%&%"temp_"%&%c%&%"/all_samples_cpe_uncomp.csv",
          data.table = FALSE) %>%
      slice(nboot) %>%
      select(matches("mean")) %>% t() %>% as.vector() %>%
      matrix(nrow  = n_cat,
             ncol  = n_cat,
             byrow = TRUE)
  }) %>%
    setNames(countries)
  
  # Load total GHG emission reduction as benchmark for GHG emission price policy
  benchmark <- fp_reductions_VAT_total %>%
    select(impact_67) %>%
    slice(nboot)
  
  # Find GHG emission price leading to equivalent reductions in GHG emissions as
  # VAT policy
  price <- optimize(function(p_tax, cpe){
    reductions <- compute_tax_fp_reductions(countries,
                                            intensities,
                                            cpe,
                                            price            = p_tax,
                                            footprints,
                                            impact_taxed     = 67, # GHG emissions
                                            impact_footprint = 67,
                                            convert_from     = "kg",
                                            convert_to       = "Mt")[["footprint"]] %>%
      select(-group, -unit) %>%
      sum()
    
    deviation <- as.numeric(abs(benchmark - reductions))
    
    return(deviation)
  },
  cpe   = cpe,
  lower = 0, 
  upper = config$carbprice_upperlim,
  tol   = 0.01)$minimum
  
  return(price)
},
future.seed = TRUE) %>%
  unlist() %>%
  data.frame(price = .)

# Save GHG emission price level for all bootstrap samples
fwrite(GHG_price_level,
       bootpath%&%"GHG_price_level.csv",
       row.names = FALSE)


# _ Compute footprint reductions at VAT-equivalent GHG emission price ----------

# For each impact...
fp_reductions_tax_total <- lapply(impacts, function(impact_no){
  
  # For each bootstrap sample...
  future_lapply(config$nbootstart:config$nbootend, function(nboot, impact_no){
    set.seed(nboot)
    
    # Retrieve original and target unit for conversion of final reduction
    unit_from <- stressors_impacts_selected %>%
      filter(code_s_i == impact_no) %>%
      mutate(Unit = gsub(" CO2 eq.| CO2-eq", "", Unit)) %>%
      select(Unit) %>% pull()
    
    unit_to   <- stressors_impacts_selected %>%
      filter(code_s_i == impact_no) %>%
      select(target.unit) %>% pull()
    
    # Load elasticity matrices 
    cpe <- lapply(countries, function(c){
      fread(bootpath%&%"temp_"%&%c%&%"/all_samples_cpe_uncomp.csv") %>%
        slice(nboot) %>%
        select(matches("mean")) %>% t() %>% as.vector() %>%
        matrix(nrow  = n_cat,
               ncol  = n_cat,
               byrow = TRUE)
      
    }) %>%
      setNames(countries)
    
    # Compute footprint reduction due to GHG emission price at sample-specific
    # VAT-equivalent GHG emission price level
    fp_reductions_tax_total <-
      compute_tax_fp_reductions(countries,
                                intensities,
                                cpe,
                                price            = GHG_price_level[nboot, ],
                                footprints,
                                impact_taxed     = 67,
                                impact_footprint = impact_no,
                                convert_from     = unit_from,
                                convert_to       = unit_to)[["footprint"]] %>%
      select(-group, -unit) %>%
      sum()
    
    return(fp_reductions_tax_total)
  },
  future.seed = TRUE,
  impact_no = impact_no) %>%
    unlist()
}) %>%
  bind_cols() %>%
  setNames("impact_"%&%impacts)

# Save results
fwrite(fp_reductions_tax_total,
       bootpath%&%"reduction_tax_EU27.csv")


# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------