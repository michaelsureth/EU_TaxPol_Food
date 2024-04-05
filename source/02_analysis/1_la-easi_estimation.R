# File info ---------------------------------------------------------------

# File:    Estimation of LA-EASI
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption

# _____________________________________------------------------------------
# Preparations ------------------------------------------------------------

# Load categorization of food items
catexplain <- fread("../build/data/intermediate_output/"%&%
                      "HBS_catexplain_"%&%config$categorization%&%".csv",
                    data.table = FALSE)

# Save number of categories
n_cat <- length(catexplain$category_code_new) 

# Save all available category codes
category_vec <- catexplain$category_code_new

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


# _____________________________________------------------------------------
# Prepare data ------------------------------------------------------------

# _ Load prepared data ----------------------------------------------------

c_clean <- 
  future_lapply(countries, function(c){
    
    return(fread(config$procdatapath%&%"DSE_proc/clean_datasets/"%&%c%&%".csv",
                 data.table = FALSE, showProgress = FALSE))
  }) %>%
  setNames(countries)

# _ Set up EASI variables and compute CDF and PDF of sampling process -----

# Check compatibility of settings in config-file
if(config$dse == "incomplete" & config$equations != "alln"){
  stop("If dse is specified as 'incomplete' in config.do, equations must be set to 'alln'.")
}

# set category of which price is used to normalize other prices with
p_to_norm <- paste0("logp", config$p_to_norm)

# number of demographic characteristics
ndem <- c_clean[[1]] %>%
  dplyr::select(starts_with("z")) %>% ncol()

# demographic variable names
dem_vars <- c_clean[[1]] %>%
  select(starts_with("z")) %>% colnames()

# Define vector of independent variables for first stage of correction of
# censored distribution of expenditures following Shonkwiler and Yen (1999)
s <- c("hh_sizeoecdm", "hh_inctot", "hh_agehead", "hh_sexhead", "dens_cat")

# prepare data  
c_final <- 
  future_lapply(c_clean, prepare_easi, s = s, dem_vars=dem_vars)


# _____________________________________------------------------------------
# Linear approximated EASI ------------------------------------------------

# _ Estimate linear EASI (SUR) --------------------------------------------

# Create coefpath
coefpath <- config$procdatapath%&%"easi_estimates/coefficients/"%&%
  configpath%&%"/"

# Create results folder (if required)
ifelse(!dir.exists(file.path(coefpath)), 
       dir.create(file.path(coefpath),
                  recursive = TRUE), 
       FALSE)

# As some Stata instances fail to write output, repeat computation for missing
# countries until all countries have been estimated
while(length(setdiff(countries,
                     list.files(coefpath,
                                pattern = "_coefs.csv") %>%
                     sub("_coefs.csv", "", .))) != 0) {
  
  # Estimate coefficients for which no estimates exist
  est_countries <- setdiff(countries,
                           list.files(coefpath,
                                      pattern = "_coefs.csv") %>%
                             sub("_coefs.csv", "", .))
  
  # Run LA-EASI by country 
  future_lapply(est_countries,
         function(c){
                  
                  # Create temporary directory for data and .do-file and estimation
                  # results
                  dir.create(config$procdatapath%&%"easi_estimates/temp_"%&%c,
                             recursive = TRUE)
                  
                  # Write data set to read into Stata
                  write.csv(c_final[[c]],
                            file      = config$procdatapath%&%"easi_estimates/temp_"%&%
                              c%&%"/data_temp.csv",
                            row.names = FALSE)
                  
                  # Copy .do-file to temporary location
                  readLines("02_analysis/2_stata_restricted_easi.do") %>%
                    # adjust data path in .do-file
                    sub("cd path_placeholder", "cd \""%&%config$procdatapath%&%
                          "easi_estimates/temp_"%&%c%&%"/\"", .) %>%
                    writeLines(config$procdatapath%&%"easi_estimates/temp_"%&%
                                 c%&%"/stata_temp.do")
                  
                  # Copy config file to temporary location
                  file.copy(from = "config.do",
                            to = config$procdatapath%&%"easi_estimates/temp_"%&%
                              c%&%"/config.do")
                  
                  # Execute Stata .do-file
                    stata(config$procdatapath%&%"easi_estimates/temp_"%&%c%&%"/stata_temp.do",
                          stata.path    = config$statapath,
                          stata.version = as.numeric(config$stataversion),
                          stata.echo    = FALSE)
                  
                  # Copy estimated coefficients from temporary directory to permanent
                  # directory
                  file.copy(config$procdatapath%&%"easi_estimates/temp_"%&%c%&%"/coefs.csv",
                            coefpath%&%c%&%"_coefs.csv")
                  
                })
}

# Delete temporary directories
unlink(config$procdatapath%&%"easi_estimates/temp_*", recursive = TRUE)


# _ Compute elasticities --------------------------------------------------
# price increase causes changes in demand through
# (1) income effect -> price increase leads to lower real income 
# (2) substitution effect -> price increase affects relative prices
# Hicksian/compensated elasticities (substitution effect only)
# Marshallian/uncompensated elasticities (substitution and income effect)

# _ Prepare estimated coefficients ----------------------------------------

future_lapply(countries, function(c){
  
  # Load coefficients
  coefs <- read.csv(coefpath%&%c%&%"_coefs.csv", skip = 1) %>%
    pivot_wider(names_from  = X,
                values_from = mean)
  message(paste0("Success: coefficients for country ", c, " loaded."))
  
  # Compute elasticities
  list_elas <- compute_elasticities(c_final[[c]],
                                    coefs,
                                    c,
                                    ndem,
                                    catexplain,
                                    n_cat)
  
  message("Success: Computed own- and cross-price elasticities for country "%&%c)
  
  # Load data source
  source <- read.csv("../build/data/intermediate_output/data_info.csv") %>%
    dplyr::filter(country == c) %>%
    dplyr::select(data_source) %>% pull()
  
  
  # ___ full household country data set (for welfare analysis) ------------
  
  # create resultdata path
  resultdatapath <- config$procdatapath%&%"DSE_proc/result_datasets/"%&%
    configpath%&%"/"
  
  ifelse(!dir.exists(file.path(resultdatapath)
  ), 
  dir.create(file.path(resultdatapath),
             recursive = TRUE), 
  FALSE)
  
  write.csv(list_elas[["hh_data"]] %>%
              dplyr::select(starts_with("hh"),
                            total_exp, dens_cat,
                            starts_with("CPE_comp"),
                            starts_with("CPE_uncomp"),
                            starts_with("EE_"),
                            starts_with("expenditures"),
                            starts_with("quantity"),
                            starts_with("logp"),
                            starts_with("nlogp"),
                            starts_with("nfoodexplogp"),
                            starts_with("uv"),
                            starts_with("s"),
                            starts_with("orig"),
                            starts_with("log_y1"),
                            matches("^z[[:digit:]]")) %>% 
              mutate(datasource = source),
            file = resultdatapath%&%
              "data_result_"%&%c%&%"_"%&%source%&%".csv",
            row.names = FALSE)
  
  # ___ CSV - compensated and uncompensated elasticities ------------------
  
  # create elasticity path
  elaspath <- config$procdatapath%&%"easi_estimates/elasticities/"%&%
    configpath%&%"/"
  
  ifelse(!dir.exists(file.path(elaspath)
  ), 
  dir.create(file.path(elaspath),
             recursive = TRUE), 
  FALSE)
  
  for(i in c("cpe_comp", "cpe_uncomp", "ee")){
    write.csv(list_elas[[i]] %>% t(),
              elaspath%&%i%&%"_"%&%c%&%"_"%&%source%&%".csv")
  }
}) # close country loop


# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------