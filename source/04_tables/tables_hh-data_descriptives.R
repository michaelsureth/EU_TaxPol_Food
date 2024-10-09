# File info --------------------------------------------------------------------

# File:    Generate household data descriptive statistics
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# _____________________________________------------------------------------
# Preparations ------------------------------------------------------------

# Naming of categories
catexplain <- fread("../build/data/intermediate_output/"%&%
                      "HBS_catexplain_"%&%config$categorization%&%".csv",
                    data.table = FALSE)

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

# Set result path
resultpath <- "../build/data/DSE_proc/result_datasets/"%&%configpath%&%"/"

# _ Load data -------------------------------------------------------------

# List result files
files <- list.files(resultpath, 
                    pattern = "_result_")

# Load files
tab_soc <- NULL
tab_exp <- NULL

for (f in files){
  
  # generate country abbreviation from loaded file
  print(c <- sub("\\_.*", "", sub(".*data_result_", "", f)))

  # generate country name from loaded file
  countryname <- EU27 %>% filter(geo == c) %>% dplyr::select(name) %>% pull()
  
  # Load country household data
  data <- fread(resultpath%&%f,
                data.table = FALSE, showProgress = FALSE)

  # __ Sociodemographic characteristics -----------------------------------
  tab_soc <- data %>%
    add_tally %>%
    group_by(datasource) %>%
    dplyr::summarise(hh_sizeoecdm   = round(weighted.mean(hh_sizeoecdm, 
                                                          hh_wgt,
                                                          na.rm = TRUE), 
                                            digits = 2),
                     withchildren   = round(100*weighted.mean(as.numeric(as.character(origz5)),
                                                              hh_wgt,
                                                              na.rm = TRUE),
                                            digits = 2),
                     urban          = round(100*weighted.mean(as.numeric(as.character(origz1)),
                                                              hh_wgt,
                                                              na.rm = TRUE),
                                            digits = 2),
                     headfemale     = round(100*weighted.mean(as.numeric(as.character(origz2)),
                                                              hh_wgt,
                                                              na.rm = TRUE),
                                            digits = 2),
                     headageabove45 = round(100*weighted.mean(as.numeric(as.character(origz3)),
                                                              hh_wgt,
                                                              na.rm = TRUE),
                                            digits = 2),
                     medianinc      = round(spatstat.geom::weighted.median(hh_inctot,
                                                                           hh_wgt),
                                            digits = 2),
                     n              = mean(n),
                     norig          = mean(hh_norig)) %>%
    ungroup() %>%
    mutate(Country = countryname) %>%
    dplyr::select(Country, everything()) %>%
    bind_rows(tab_soc)

  # __ Expenditure shares -------------------------------------------------
  tab_exp <- data %>%
    dplyr::select_if(grepl("hh_id|hh_wgt|^s[[:digit:]]|expenditures",
                           names(.))) %>%
    pivot_longer(-c(hh_id,hh_wgt),
                 names_to  = c(".value", "category"),
                 names_sep = "(?<=[a-z])(?=[0-9])") %>%
    mutate(zero = ifelse(expenditures > 0,
                         0,
                         ifelse(expenditures == 0,
                                1,
                                NA))) %>%
    group_by(category) %>%
    dplyr::summarise(expenditure  = round(weighted.mean(expenditures, hh_wgt,
                                                        na.rm = TRUE),
                                          digits = 2),
                     share_infood = round(100*weighted.mean(s, hh_wgt,
                                                            na.rm = TRUE),
                                          digits = 2),
                     zero         = round(weighted.mean(100*zero, hh_wgt,
                                                        na.rm = TRUE),
                                          digits = 2)) %>%
    ungroup() %>% 
    mutate(category = as.integer(category)) %>%
    full_join(catexplain, by = c("category" = "category_code_new")) %>%
    mutate(Country = countryname) %>%
    arrange(category) %>%
    bind_rows(tab_exp)

  rm(data)  
}



# _____________________________________------------------------------------
# Table: Sociodemographic chracteristics ----------------------------------

# create directory if necessary
if(!dir.exists("../build/tables")){
  dir.create("../build/tables",
             recursive = TRUE)
}

# Add formatting and column names
tab_soc_table <- tab_soc %>%
  # drop variables which are not included
  dplyr::select(-norig) %>%
  # add information on case I imputation share
  left_join(EU27[, c("geo", "name")], by = c("Country" = "name")) %>% 
  left_join(fread("../build/data/intermediate_output/data_info.csv")[,c("country", 
                                                                        "items_imputed")],
            by = c("geo" = "country")) %>% 
  dplyr::select(-geo) %>%
  # variable formatting
  mutate_at(vars(n, medianinc), ~round(as.numeric(.))) %>%
  mutate_at(vars(hh_sizeoecdm), ~round(., digits = 2)) %>%
  mutate_at(vars(headfemale,urban,headageabove45, withchildren),
            ~paste0(round(as.numeric(.), digits = 1), "%")) %>%
  # column names
  rename("Household size \n (mod. OECD)"              = hh_sizeoecdm,
         "Household head is \n female (share)"        = headfemale,
         "Median household \n income"                 = medianinc,
         "Urban households \n (share)"                = urban,
         "Household head aged \n 45 or older (share)" = headageabove45,
         "Households with \n children (share)"        = withchildren,
         "Sample size (final)"                        = n,
         "Data source"                                = datasource,
         "Share of imputed items"                     = items_imputed) %>%
  # replace 0s with NA
  mutate_all(~replace(., . == 0, NA)) %>%
  arrange(Country)

# Generate and save LaTeX code
write("\\renewcommand{\\baselinestretch}{1}",
      file = "../build/tables/desc_sociodems_ALLCOUNTRIES_"%&%
        config$categorization%&%".tex")

write(tab_soc_table %>%
        dplyr::mutate_all(linebreak) %>%
        kable(format     = "latex",
              escape     = TRUE,
              booktabs   = TRUE,
              linesep    = "",
              caption    = "Sample characteristics by country",
              label      = "desc_stat",
              row.names  = FALSE,
              col.names  = NA,
              align      = "c") %>%
        kable_styling(font_size = 8) %>%
        column_spec(1,                 bold  = TRUE) %>%
        column_spec(2,                 width = "1.2cm") %>%
        column_spec(3:(ncol(tab_soc_table)), width = "1.2cm") %>%
        footnote(
          general = c("Sample characteristics based on Eurostat Household Budget Survey (HBS) 2010 and 2015, Konsumerhebung (KE) 2014/2015,",
                      "and Einkommens- und Verbrauchsstichprobe (EVS) 2018. All values are rounded means weighted using sampling weights.",
                      "Household size is computed according to the modified OECD scale, which assigns a value of 1 to the household head, of 0.5 ",
                      "to each additional adult member and of 0.3 to each child. Items imputed displays the share of imputed quantities for all food",
                      "items that have not been recorded in a given country (Case I).")) %>%
        kable_paper(),
      file = "../build/tables/desc_sociodems_ALLCOUNTRIES_"%&%
        config$categorization%&%".tex",
      append = TRUE)

write("\\renewcommand{\\baselinestretch}{1.5}",
      file = "../build/tables/desc_sociodems_ALLCOUNTRIES_"%&%
        config$categorization%&%".tex",
      append = TRUE)



# Table: Expenditure shares -----------------------------------------------

# Add formatting
tab_exp_table <- tab_exp %>%
  pivot_wider(id_cols = c("Country"), 
              names_from = category_name_new, 
              values_from = c("share_infood")) %>% #share_infood
  arrange(Country)

# Generate and save LaTeX code
write("\\renewcommand{\\baselinestretch}{1}",
      file = "../build/tables/desc_expshares_ALLCOUNTRIES_"%&%
        config$categorization%&%".tex")

write(tab_exp_table %>%
        dplyr::mutate_all(linebreak) %>%
        kable(format     = "latex",
              escape     = TRUE,
              booktabs   = TRUE,
              linesep    = "",
              caption    = "Budget shares for different food items by country (2015)",
              label      = "exp_shares",
              row.names  = FALSE,
              col.names  = NA,
              align      = "c") %>%
        kable_styling(font_size = 8) %>%
        row_spec(0, bold = TRUE) %>%
        column_spec(1,                 bold  = TRUE) %>%
        column_spec(2,                 width = "1.2cm") %>%
        column_spec(3:(ncol(tab_exp_table)), width = "1.4cm") %>%
        column_spec(ncol(tab_exp_table)+1) %>%
        footnote(#general_title ="",
          general = c("NEC = Not elsewhere classified. Budget shares are given in percent (rows may not sum to 100 due to rounding).",
                      "Weighted averages based on Eurostat Household Budget Survey (HBS).")) %>%
        landscape() %>% kable_paper(),
      file = "../build/tables/desc_expshares_ALLCOUNTRIES_"%&%
        config$categorization%&%".tex",
      append = TRUE)

write("\\renewcommand{\\baselinestretch}{1.5}",
      file = "../build/tables/desc_expshares_ALLCOUNTRIES_"%&%
        config$categorization%&%".tex",
      append = TRUE)

# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------