# File info ---------------------------------------------------------------

# File:    Generate plots comparing elasticity estimates for all countries
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption

# _____________________________________------------------------------------
# Preparations ------------------------------------------------------------

# Load category names
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

# Create figure directory if required
if(!dir.exists("../build/figures/"%&%configpath%&%"_"%&%config$year_io)){
  dir.create("../build/figures/"%&%configpath%&%"_"%&% config$year_io,
             recursive = TRUE)
}

# _____________________________________------------------------------------
# Budget shares -----------------------------------------------------------

# _ Load data -------------------------------------------------------------

# Determine result path
resultpath <- "../build/data/DSE_proc/result_datasets/"%&%
  configpath%&%"/"

# List result files
files <- list.files(resultpath, 
                    pattern = "_result_")

# Load files
tab_exp <- NULL

for (f in files){
  
  # generate country abbreviation from loaded file
  message(c <- sub("\\_.*", "", sub(".*data_result_", "", f)))
  
  # generate country name from loaded file
  countryname <- EU27 %>% filter(geo == c) %>% dplyr::select(name) %>% pull()
  
  # Load country household data
  data <- fread(resultpath%&%f,
                data.table = FALSE, showProgress = FALSE)
  
# Compute shares
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

# _ Plot budget shares ----------------------------------------------------

tab_exp_plot <- tab_exp %>%
  mutate(fill = reorder(category_name_new, as.numeric(category),  
                        decreasing = F)) %>%
  ggplot(aes(fill = fill,
             y    = share_infood,
             x    = Country)) + 
  geom_bar(position = "stack",
           stat     = "identity") +
  theme_c1m3() +
  scale_fill_manual(values = food_colours_10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none") + 
  xlab("")+ylab("Percentage") +ggtitle("")

ggarrange(tab_exp_plot, NULL, cat10_legend, 
          ncol = 1, heights = c(1, 0.01, 0.1))

# save plot
ggsave("../build/figures/SI_figure_budgetshares.pdf",
       width = 12, height = 7)


# _ Generate info for text ------------------------------------------------

# Category with highest budget share per country
tab_exp %>%
  group_by(Country) %>% 
  mutate(maxshare = max(share_infood)) %>%
  filter(share_infood == maxshare) %>% ungroup() %>%
  dplyr::select(category_name_new) %>% table()

# Country with highest budget share for Beef
tab_exp %>%
  filter(category_name_new == "Beef") %>%
  arrange(-share_infood) %>% head()


# Elasticities ------------------------------------------------------------

# _ Plot CPEs -------------------------------------------------------------

type <- "uncomp"
dir  <- "from"
av   <- "mean"

plot_CPEs(type      = type, 
          direction = dir,
          lims      = c(-2, 1),
          averaging = av)

ggsave("../build/figures/"%&%configpath%&%"_"%&%config$year_io %&%
         "/figure_CPEs_"%&%type%&%"_"%&%dir%&%"_"%&%av%&%".pdf",
       width = 20, height = 6)

# _ Generate info for text ------------------------------------------------

# Compensated or uncompensated elasticities?
type <- "uncomp"
averaging <- "mean"

# Load elasticity results for default configuration
files <- list.files(config$procdatapath%&%"easi_estimates/elasticities/"%&%
                      configpath%&%"/", pattern = "cpe_"%&%type)

df <- list()
x  <- 0

for(f in files){
  
  x <- x + 1
  
  # generate country abbreviation from loaded file
  c <- as.vector(str_match(f, "(?<=comp_).{2}"))
  
  # load elasticity results 
  df[[x]] <- fread(config$procdatapath%&%"easi_estimates/elasticities/"%&%
                     configpath%&%"/"%&%f,
                   data.table = FALSE) %>%
    setNames(c("coef","value")) %>%
    mutate(country       = c,
           configuration = configpath)
  
}

# Turn list into df
df_all <- do.call(rbind.data.frame, df) 

# Define OPEs (mean)
pattern <- NULL
for(cat in 1:nrow(catexplain)){
  pattern <- c(pattern, "CPE_"%&%type%&%"_i"%&%cat%&%"j"%&%cat%&%"_"%&%averaging)
}

# (1) Own-price elasticities
  df_ope <- df_all %>%
  dplyr::filter(coef %in% pattern) 

# (2) Cross-price elasticities
  df_cpe <- df_all %>%
    dplyr::filter(!coef %in% pattern) 
  
# share of OPEs between -2 and 0
  df_ope %>%
    transmute(inrange = ifelse(value > -2 & value < 0,
                               1,
                               0)) %>%
    table()
    
# which category has the lowest/highest OPE on average (across countries)
df_ope %>%
  group_by(coef) %>%
  dplyr::summarise(mean = mean(value)) %>%
  arrange(mean) %>%
  mutate(category_code_new = (gsub( "\\D", "", coef))) %>%
  mutate(category_code_new = as.integer(ifelse(nchar(category_code_new) == 2,
                                               substr(category_code_new, 1, 1),
                                               substr(category_code_new, 1, 2)
  ))) %>%
  left_join(catexplain, by = "category_code_new") %>%
  transmute(category_name_new,mean = round(mean, 2), category_code_new) %>%
  arrange(desc(mean))

# how many countries have < -1 OPE for Milk and Dairy
df_ope %>%
  mutate(category_code_new = (gsub( "\\D", "", coef))) %>%
  mutate(category_code_new = as.integer(ifelse(nchar(category_code_new) == 2,
                                               substr(category_code_new, 1, 1),
                                               substr(category_code_new, 1, 2)
  ))) %>%
  left_join(catexplain, by = "category_code_new") %>%
  filter(category_name_new == "Milk and dairy") %>% 
  transmute(category_name_new, country, value) %>% arrange(-value)

# which category has the highest OPE by country
df_ope %>%
  dplyr::select(coef, value, country) %>%
  mutate(category_code_new = (gsub( "\\D", "", coef))) %>%
  mutate(category_code_new = as.integer(ifelse(nchar(category_code_new) == 2,
                                               substr(category_code_new, 1, 1),
                                               substr(category_code_new, 1, 2)
  ))) %>%
  left_join(catexplain, by = "category_code_new") %>%
  group_by(country) %>%
  filter(value == min(value))

# non-negative OPE values
df_ope %>%
  filter(value > 0) %>%
  arrange(-value)

# range for single category: beef
df_ope %>%
  filter(coef %in% c("CPE_uncomp_i6j6_mean", "CPE_uncomp_i6j6_ref")) %>%
  arrange(value) %>% dplyr::select(country, coef, value) 

# range of CPEs
df_cpe %>% dplyr::select(coef, value, country) %>%
  filter(!coef %in% pattern) %>%
  arrange(value) %>% 
  ggplot(aes(x     = value,
             group = factor(country),
             col   = factor(country))) +
  geom_density()

# share of CPEs between lower and upper limit
lo <- -0.4; up <- 0.3

df_cpe %>% dplyr::select(coef, value, country) %>%
  filter(!coef %in% pattern) %>%
  mutate(cases = ifelse(value >= lo & value <= up,
                        "between "%&%lo%&%" and "%&%up, 
                        "outside of interval")) %>%
  dplyr::select(cases) %>% table()


# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------