# File info --------------------------------------------------------------------

# File:    Generate LaTeX code for table on welfare comparison
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption

# _____________________________________------------------------------------
# Preparations ------------------------------------------------------------

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
  config$pzint%&%"_"%&%
  config$year_io

# Create table directory if required
if(!dir.exists("../build/tables/"%&%substring(configpath, 1, nchar(configpath)-5))){
  dir.create("../build/tables/"%&%substring(configpath, 1, nchar(configpath)-5),
             recursive = TRUE)
}

# Choose policy to compare
policy <- "tax_GHG emissions"

# Load environmental tax level and corresponding unit
price <- fread("../build/data/policies/"%&%policy%&%"/"%&%configpath%&%"/"%&%
                 list.files("../build/data/policies/"%&%policy%&%"/"%&%configpath)[1]) %>%
  select(env_tax) %>%
  slice(1) %>%
  pull

unit  <- fread("../build/data/intensities/"%&%
                 config$year_io%&%"/intensities_by_foodcat.csv") %>%
  filter(impact_shortname == gsub("tax_","",policy)) %>%
  select(unit_new) %>%
  unique() %>%
  pull()

# _ Load data -------------------------------------------------------------

# __ Tax revenue changes by household -------------------------------------

tax_inc <- fread("../build/data/welfare/"%&%configpath%&%"/tax_paid_changes.csv",
                 data.table = FALSE, showProgress = FALSE)

# __ number of households for all EU27 countries --------------------------

EU_hh_year <- read_excel("00_data/lfst_hhnhwhtc__custom_10042738_spreadsheet.xlsx",
                         sheet = "Sheet 1",
                         skip  = 10) %>%
  dplyr::transmute(name = ifelse(TIME == "Germany (until 1990 former territory of the FRG)",
                                 "Germany", TIME), 
                   # convert unit (thousand households) into households
                   nhh = as.numeric(!!sym(config$year_io))*1000) %>% 
  right_join(EU27[,c("geo", "name")], by="name") %>%
  mutate(weight = nhh/sum(nhh))


# __ average household sizes for all EU27 ---------------------------------

EU_hhsize_year <- read_excel("00_data/lfst_hhantych_page_spreadsheet.xlsx",
                             sheet = "Sheet 1",
                             skip  = 10) %>%
  dplyr::transmute(name = ifelse(TIME == "Germany (until 1990 former territory of the FRG)",
                                 "Germany", TIME),
                   hh_size_av = as.numeric(!!sym(config$year_io))) %>% 
  right_join(EU27[,c("geo", "name")], by="name") 

# Table: Net costs --------------------------------------------------------

# Net costs (difference between absolute log cost-of-living and additional 
# available tax revenue per household)
Table_welfare <- tax_inc %>%
    # replace country with country name
    left_join(EU27[,c("geo", "name")], by=c("country"="geo")) %>%
    # compute weighted mean per household additional VAT and GHG emission price
    # income per country
    group_by(policy, country, name) %>%
    summarize(tax_VAT_diff_mean = weighted.mean(VAT_diff, hh_wgt),
              tax_GHG_inc_mean  = weighted.mean(GHG_inc, hh_wgt)) %>%
    mutate(tax_inc_mean = tax_VAT_diff_mean+tax_GHG_inc_mean)%>%
    ungroup() %>% 
    # add country-specific weighted mean welfare costs (absolute log cost-of-living)
    left_join(lapply(tax_inc %>%
                       distinct(policy) %>%
                       pull(),
                     function(pol){
                       fread("../build/data/welfare/"%&%configpath%&%
                               "/"%&%pol%&%"/"%&%"country-means_lcol.csv",
                             data.table = FALSE)
                     }) %>% 
                setNames(tax_inc %>%
                           distinct(policy) %>%
                           pull()) %>%
                bind_rows(.id = "policy"),
              by = join_by("policy", "country")) %>% 
    # compute net costs = welfare costs - additional tax revenue (weighted average
    # per country)
    rename(welfare_lcol_abs_mean = mean_lcol_abs) %>%
    mutate(abs_net_costs = welfare_lcol_abs_mean - (tax_VAT_diff_mean + tax_GHG_inc_mean)) %>%
    pivot_wider(id_cols = c("name"), 
                names_from = policy, 
                values_from = c("tax_inc_mean", "welfare_lcol_abs_mean", "abs_net_costs"),
                names_glue = "{policy}_{.value}") %>%
    dplyr::select(country=name, order(colnames(.),decreasing = TRUE)) %>%
  arrange(country) %>%
  
  # add EU-wide weighted (by number of households per country) mean values
  bind_rows({.} %>% left_join(EU_hh_year %>% select(name, weight),
                                        by = c("country" = "name")) %>% 
              summarize(country = "EU27",
                        VAT_increase_welfare_lcol_abs_mean = sum(VAT_increase_welfare_lcol_abs_mean*weight),
                        VAT_increase_tax_inc_mean = sum(VAT_increase_tax_inc_mean*weight),
                        VAT_increase_abs_net_costs = sum(VAT_increase_abs_net_costs*weight),
                        `tax_GHG emissions_welfare_lcol_abs_mean` = sum(`tax_GHG emissions_welfare_lcol_abs_mean`*weight),
                        `tax_GHG emissions_tax_inc_mean` = sum(`tax_GHG emissions_tax_inc_mean`*weight),
                        `tax_GHG emissions_abs_net_costs` = sum(`tax_GHG emissions_abs_net_costs`*weight))
  ) %>%

  # format table
  mutate_at(vars(-country), ~round(.))
  
# Save as .tex
write(Table_welfare %>%
        kable(format     = "latex",
              escape     = TRUE,
              booktabs   = TRUE,
              linesep    = "",
              caption    = "Mean per household welfare impacts of policies in EUR",
              label      = "net_welfare",
              row.names  = FALSE,
              col.names  = c("", "Change in COL","Change in T", "Net cost",
                             "Change in COL","Change in T", "Net cost"),
              align      = "c") %>%
        add_header_above(c(" ",
                           "VAT reform" = 3,
                           "GHG emission price" = 3)) %>%
        kable_styling(font_size = 8) %>%
        row_spec(0, bold = TRUE) %>%
        row_spec(28, bold = TRUE) %>%
        column_spec(2:7, width = "6em") %>%
        footnote(general = c("The table presents welfare effects resulting from the removal of value-added tax reductions for meat products",
                             "(VAT reform) and the implementation of a GHG emission price of "%&%price%&%" EUR/"%&%unit%&%" on all food products. It",
                             "compares the mean change in the absolute cost-of-living (Change in cost) with the mean change in tax income" ,
                             "(Change in tax revenue, comprising the change in VAT income and the additional income from GHG emission",
                             "pricing) by country. Net cost represents the difference between mean change in the absolute cost-of-living and",
                             "the mean additional tax income. All values are rounded annual means per household in EUR, weighted using",
                             "sampling weights to ensure representativeness.")) %>%
        kable_paper(),
      file = "../build/tables/"%&%
        substring(configpath, 1, nchar(configpath)-5)%&%
        "/net_welfare_country.tex") 


# _ text ------------------------------------------------------------------

# majority of countries has higher mean gross costs under GHG emission price
Table_welfare %>%
  transmute(country, VAT_increase_welfare_lcol_abs_mean, `tax_GHG emissions_welfare_lcol_abs_mean`,
            diff = VAT_increase_welfare_lcol_abs_mean - `tax_GHG emissions_welfare_lcol_abs_mean`) %>%
  arrange(-diff)

# majority of countries has higher mean net costs under VAT
Table_welfare %>%
  transmute(country, VAT_increase_abs_net_costs, `tax_GHG emissions_abs_net_costs`,
            diff = VAT_increase_abs_net_costs - `tax_GHG emissions_abs_net_costs`) %>%
  arrange(diff)

# expressed at per capita scale
Table_welfare %>%
  dplyr::select(country,
                VAT_increase_abs_net_costs,
                `tax_GHG emissions_abs_net_costs`) %>%
  left_join(EU_hhsize_year, by=c("country"="name")) %>%
  transmute(country, geo, hh_size_av,
            VAT_increase_abs_net_costs_PC = VAT_increase_abs_net_costs/hh_size_av,
            `tax_GHG emissions_abs_net_costs_PC` = `tax_GHG emissions_abs_net_costs`/hh_size_av) %>% 
  
  # add EU-wide weighted (by number of households per country) mean values
  filter(!is.na(geo)) %>%
  bind_rows({.} %>% left_join(EU_hh_year %>% select(name, weight),
                              by = c("country" = "name")) %>% 
              summarize(country = "EU27",
                        VAT_increase_abs_net_costs_PC = sum(VAT_increase_abs_net_costs_PC*weight),
                        `tax_GHG emissions_abs_net_costs_PC` = sum(`tax_GHG emissions_abs_net_costs_PC`*weight))
  ) %>%
  filter(country == "EU27")

# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------
