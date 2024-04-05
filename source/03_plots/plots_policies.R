# File info ---------------------------------------------------------------

# File:    Plot policy simulation results
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
  config$pzint

# Create figure directory if required
if(!dir.exists("../build/figures/"%&%configpath%&%"_"%&%config$year_io)){
  dir.create("../build/figures/"%&%configpath%&%"_"%&%config$year_io,
             recursive = TRUE)
}

# Load selected stressors and impacts
stressors_impacts_selected   <- read.csv("../build/data/intermediate_output/"%&%
                                           "MRIO_stressors_impacts_final.csv") %>%
  select(-c("Landusetype", "Landusetype_coeff"))

# Load unit conversion
units_conversion <- read.xlsx("00_data/manual_input/unit_conversion.xlsx")

# Check if final bootstrap results exist for this configuration
boot_exist <-
  ifelse(file.exists("../build/data/bootstrap/"%&%configpath%&%
                       "_"%&%config$year_io%&%"/GHG_price_level.csv"), 
         "boot_yes", 
         "boot_no")

# Choose policy to compare
policy <- "tax_GHG emissions"

# _ Load data -------------------------------------------------------------

# Load environmental tax level and corresponding unit
price <- fread("../build/data/policies/"%&%policy%&%"/"%&%configpath%&%
                 "_"%&%config$year_io%&%"/"%&%
                 list.files("../build/data/policies/"%&%policy%&%"/"%&%configpath%&%
                              "_"%&%config$year_io)[1]) %>%
  select(env_tax) %>% slice(1) %>% pull()

if(boot_exist == "boot_yes"){
  
  price_lo <- read.csv("../build/data/bootstrap/"%&%configpath%&%
                         "_"%&%config$year_io%&%"/GHG_price_level.csv") %>%
    as_vector() %>%
    min() %>% # change here whether sd, min/max, or 5/95 percentile is used for error bars
    round(digits = 2)
  
  price_hi <- read.csv("../build/data/bootstrap/"%&%configpath%&%
                         "_"%&%config$year_io%&%"/GHG_price_level.csv") %>%
    as_vector() %>%
    max() %>% # change here whether sd, min/max, or 5/95 percentile is used for error bars
    round(digits = 2)
}else{
  message("No bootstrap estimates available")
}

unit_tax  <- fread("../build/data/intensities/"%&%config$year_io%&%
                     "/intensities_by_foodcat.csv") %>% 
  filter(impact_shortname == gsub("tax_", "", policy)) %>% 
  select(unit_new) %>% unique() %>% pull()

# Load data
pol_all <-
  lapply(c("VAT_increase", policy), function(pol){
    
    lapply(countries, function(c){
      fread("../build/data/policies/"%&%pol%&%"/"%&%configpath%&%
              "_"%&%config$year_io%&%"/reduction_"%&%c%&%".csv",
            data.table = FALSE) %>%
        mutate(countryname = EU27[EU27$geo == c, ]$name)
    }
    ) %>%
      setNames(countries) %>%
      bind_rows(.id = "country")
  }
  ) %>%
  setNames(c("VAT_increase", policy)) %>%
  bind_rows(.id = "policy") %>%
  select(-V1) %>%
  mutate(fill = reorder(category, as.numeric(cat_no),  decreasing = F))

# Load absolute footprints to compute relative reduction
EU_footprint_food <-
  prepare_data_barplot(demandcountries    = countries, 
                       categorization     = "by_food_nonfood",
                       type               = "aggregate",
                       inout              = FALSE,
                       selectedindicators = selectedindicators_plot) %>%
  filter(catname == "Food") %>%
  dplyr::select(catname, impact_name = x, shortname, value.target)


# _____________________________________------------------------------------
# Figure 3: Price and quantity changes ------------------------------------

# add jitter for VAT price change (otherwise points overlap perfectly)
jitterjoin <- data.frame(category = seq(1,10,1), jitter = seq(-0.95,0.95,0.2))

# prepare dataframe to plot
df_plot_pq <- pol_all %>%
  dplyr::select(category, cat_no, price_increase_rel, demand_reduction_rel, fill,
                policy, country, countryname) %>%
  distinct() %>%
  pivot_longer(cols = price_increase_rel:demand_reduction_rel, 
               names_to = "variable", values_to = "relative_change") %>%
  left_join(jitterjoin, by=c("cat_no" = "category")) %>%
  mutate(ymin = ifelse(variable == "demand_reduction_rel",
                       -0.4,       # change this to change second row lower limit
                       -0.01),     # change this to change first row lower limit
         ymax =  ifelse(variable == "price_increase_rel",
                        0.23,      # change this to change second row upper limit
                        0.1))      # change this to change first row upper limit

for(pol in c("VAT_increase", policy)){
  assign("plot_p_"%&%pol,
         df_plot_pq %>%
           filter(policy == pol & variable == "price_increase_rel") %>%
           ggplot() +
           geom_hline(yintercept = 0) +
           {if(pol == "VAT_increase")
             geom_point(aes(y     = relative_change,
                            x     = as.numeric(factor(countryname)) + jitter,
                            color = fill),
                        show.legend = FALSE)
           } +
           {if(pol == "tax_GHG emissions")  
             geom_point(aes(y     = relative_change,
                            x     = countryname,
                            color = fill),
                        show.legend = FALSE)
           } +
           scale_colour_manual(values = rev(food_colours_10)) +
           geom_blank(aes(y = ymin)) +
           geom_blank(aes(y = ymax)) +
           scale_y_continuous(labels = scales::percent) +
           scale_x_discrete(labels = NULL) +
           {if(pol == "VAT_increase")
             labs(x     = NULL,
                  y     = "policy-induced \n price change (in %)",
                  title = "VAT reform")} +
           {if(pol == "tax_GHG emissions")
             labs(x     = NULL,
                  y     = "",
                  title = "GHG emission price ("%&%price%&%
                    " EUR/"%&%unit_tax%&%")")} +
           theme_c1m3() +
           theme(axis.title.y = element_text(size   = 18,
                                             face   = "bold",
                                             margin = margin(t = 0, r = 20,
                                                             b = 0, l = 0)),
                 plot.title   = element_text(size   = 20,
                                             hjust  = 0.5,
                                             margin = margin(t = 0,  r = 0,
                                                             b = 20, l = 0)),
                 axis.line.x  = element_blank())
  )
  
  assign("plot_q_"%&%pol,
         df_plot_pq %>%
           filter(policy == pol & variable == "demand_reduction_rel") %>%
           ggplot() +
           geom_hline(yintercept = 0) +
           geom_point(aes(y     = relative_change,
                          x     = countryname,
                          color = fill),
                      show.legend = FALSE) +
           scale_colour_manual(values = rev(food_colours_10)) +
           geom_blank(aes(y = ymin)) +
           geom_blank(aes(y = ymax)) +
           scale_y_continuous(labels = scales::percent) +
           {if(pol == "VAT_increase")
             labs(x     = NULL,
                  y     = "change in demanded \n quantity (in %)",
                  title = "")} +
           {if(pol == "tax_GHG emissions")
             labs(x     = NULL,
                  y     = "",
                  title = "")} +
           theme_c1m3() +
           theme(axis.title.y = element_text(size   = 18,
                                             face   = "bold",
                                             margin = margin(t = 0, r = 20,
                                                             b = 0, l = 0)),
                 plot.title   = element_text(size   = 20,
                                             hjust  = 0.5,
                                             margin = margin(t = 0,  r = 0,
                                                             b = 20, l = 0)),
                 axis.text.x  = element_text(angle = 90, vjust = 1, hjust=1))
  )
}

# Arrange plot and prepared legend (in plot_functions.R)
ggarrange(ggarrange(plot_p_VAT_increase, `plot_p_tax_GHG emissions`,
                    NULL, NULL,
                    plot_q_VAT_increase, `plot_q_tax_GHG emissions`,
                    align = "hv", nrow = 3, ncol = 2, heights = c(1, -0.2, 1)),
          cat10_legend, 
          nrow = 2, heights = c(1, 0.1)
)

# Save plot
# Create figure directory if required
if(!dir.exists("../build/figures/"%&%configpath%&%"_"%&%config$year_io)){
  dir.create("../build/figures/"%&%configpath%&%"_"%&%config$year_io,
             recursive = TRUE)
}

ggsave("../build/figures/"%&%configpath%&%"_"%&%config$year_io%&%"/figure_pq.pdf",
       width = 16, height = 8)


# __ text: pq changes -----------------------------------------------------

# GHG emission price leading to equivalent GHG emission reductions as VAT
# reform (incl. 5% CI from bootstrapping)
if(boot_exist=="boot_yes"){
  message(paste0(price, " (", price_lo, " - ", price_hi, ")"))
}

# VAT: average demand reduction by category
df_plot_pq %>%
  filter(variable == "demand_reduction_rel" & grepl("VAT", policy)) %>%
  group_by(category, policy) %>%
  summarize(meandemandreduction = mean(relative_change)) %>%
  arrange(policy, -meandemandreduction)

# GHG price: average demand reduction by category
df_plot_pq %>%
  filter(variable == "demand_reduction_rel" & grepl("GHG", policy)) %>%
  group_by(category, policy) %>%
  summarize(meandemandreduction = mean(relative_change)) %>%
  arrange(policy, -meandemandreduction)

# VAT: average price change by category
df_plot_pq %>%
  filter(variable == "price_increase_rel" & grepl("VAT", policy)) %>%
  group_by(category, policy) %>%
  summarize(meandpriceincrease = mean(relative_change)) %>%
  arrange(policy, -meandpriceincrease)

# GHG price: average price change by category
df_plot_pq %>%
  filter(variable == "price_increase_rel" & grepl("GHG", policy)) %>%
  group_by(category, policy) %>%
  summarize(meandpriceincrease = mean(relative_change)) %>%
  arrange(policy, -meandpriceincrease)

# VAT: price change by category by country
df_plot_pq %>%
  filter(variable == "price_increase_rel" & grepl("VAT", policy) & 
           relative_change > 0 ) %>%
  dplyr::select(country, relative_change) %>%
  distinct() %>% arrange(-relative_change) 

# GHG emissions: price change by category by country
df_plot_pq %>%
  filter(variable == "price_increase_rel" & grepl("GHG", policy)) %>% 
  dplyr::select(category, country, relative_change) %>%
  distinct()

# GHG emissions: price change for beef (average) 
df_plot_pq %>%
  filter(variable == "price_increase_rel" & grepl("GHG", policy) & category == "Beef") %>% 
  summarize(rel_change = mean(relative_change))

# GHG emissions: quantity change by category by country >0
df_plot_pq %>%
  filter(variable == "demand_reduction_rel" & grepl("GHG", policy)) %>%
  dplyr::select(category, country, relative_change) %>%
  filter(relative_change > 0)

# VAT: quantity change by category by country >0
df_plot_pq %>%
  filter(variable == "demand_reduction_rel" & grepl("VAT", policy)) %>%
  dplyr::select(category, country, relative_change) %>%
  filter(relative_change > 0)

# Figure 4: Footprint reductions (EU absolute) ----------------------------

# Create mapping between impact number and impact names
mapping_impact_no_name <- stressors_impacts_selected %>%
  select(shortname, code_s_i) %>%
  mutate(code_s_i = as.character(code_s_i)) %>%
  filter(shortname %in% selectedindicators_plot) %>%
  deframe()

# Compute min and max value of error bars from bootstrapping results
if(boot_exist=="boot_yes"){
  
  error <- read.csv("../build/data/bootstrap/"%&%configpath%&%
                      "_"%&%config$year_io%&%"/reduction_VAT_EU27.csv") %>%
    rename_with(~str_remove(., 'impact_')) %>%
    rename(all_of(mapping_impact_no_name)) %>%
    summarize(across(everything(), 
                     .fns = list(lowerbound  = min,
                                 higherbound = max))) %>%  # change here whether sd, min/max, or 5/95 percentile is used for error bars
    pivot_longer(cols          = everything(),
                 values_to     = "error",
                 names_to      = c("impact", "bound"),
                 names_pattern = "(.*)_(.*)") %>%
    pivot_wider(id_cols     = impact,
                names_from  = bound,
                values_from = error) %>%
    mutate(policy = "VAT_increase") %>%
    bind_rows(read.csv("../build/data/bootstrap/"%&%configpath%&%
                         "_"%&%config$year_io%&%"/reduction_tax_EU27.csv") %>%
                rename_with(~str_remove(., 'impact_')) %>%
                rename(all_of(mapping_impact_no_name)) %>%
                summarize(across(everything(), 
                                 .fns = list(lowerbound  = min,
                                             higherbound = max))) %>%  # change here whether sd, min/max or 5/95 percentile is used for error bars # ~stats::quantile(., probs = 0.05)
                pivot_longer(cols          = everything(),
                             values_to     = "error",
                             names_to      = c("impact", "bound"),
                             names_pattern = "(.*)_(.*)") %>%
                pivot_wider(id_cols     = impact,
                            names_from  = bound,
                            values_from = error) %>%
                mutate(policy = "tax_GHG emissions")) 
}

# prepare dataframe to plot
df_plot_fp <- pol_all %>%
  dplyr::filter(impact_name %in% selectedindicators_plot) %>%
  dplyr::transmute(fill, cat_no, policy, country, footprint_reduction_abs,
                   impact_name, unit) %>%
  # correct order
  left_join(selectedindicators_df, by = c("impact_name" = "shortname")) %>% 
  mutate(header = paste0(impact_name, "\n(", unit, ")")) %>%
  mutate(header = reorder(header, n,  decreasing = F)) %>% 
  group_by(policy, fill, impact_name, header, cat_no) %>%
  dplyr::summarise(footprint_reduction_abs = sum(footprint_reduction_abs)) %>%
  # compute total reduction per impact per policy (needed for error bars)
  group_by(impact_name, policy) %>%
  mutate(footprint_reduction_abs_total = sum(footprint_reduction_abs)) %>%
  ungroup() %>% 
  # add value to be substracted/added from absolute value for error bars
  { if(boot_exist == "boot_yes") {
    left_join(., error, by = c("policy" = "policy", "impact_name" = "impact")) 
  } else {
    .
  }
  } %>%
  mutate(policy = ifelse(policy == "tax_GHG emissions", 
                         "GHG emissions price \n("%&%price%&%" EUR/"%&%unit_tax%&%")",
                         ifelse(policy == "VAT_increase",
                                "VAT reform", 
                                NA)))


# plot reductions for each indicator
plotlist <- list()
for(x in 1:length(selectedindicators_plot)){
  
  sel <- selectedindicators_plot[x]
  
  # EU food footprint: total reduction in percent
  red_percent <- df_plot_fp %>%
    filter(impact_name == sel) %>%
    group_by(impact_name, policy) %>%
    dplyr::summarise(footprint_reduction_abs = sum(footprint_reduction_abs)) %>%
    left_join(EU_footprint_food, by = "impact_name") %>%
    mutate(footprint_reduction_rel = footprint_reduction_abs/value.target,
           ratio                   = max(footprint_reduction_abs) / max(footprint_reduction_rel)) %>%
    ungroup() %>% 
    mutate(footprint_reduction_relSCALED = footprint_reduction_rel*ratio) %>%
    dplyr::select(impact_name, policy, footprint_reduction_rel, ratio,
                  footprint_reduction_relSCALED)
  
  # assign scalefactor
  assign(paste0("scalefactor", x),
         red_percent$ratio %>% unique())
  
  # plot
  plotlist[[x]] <- df_plot_fp %>%
    filter(impact_name == sel) %>% 
    left_join(red_percent, by = c("policy", "impact_name")) %>% 
    ggplot(aes(x    = policy,
               y    = footprint_reduction_abs,
               fill = fill)) +
    geom_bar(stat     = "identity",
             position = "stack",
             width    = 0.8) +
    
    {if(boot_exist == "boot_yes")
      geom_errorbar(aes(ymin = lowerbound,
                        ymax = higherbound),
                    width = 0.2)
    } +
    
    {if(sel == "Biodiversity loss")
      scale_y_continuous(sec.axis = sec_axis(~ . /scalefactor1, 
                                             labels = scales::label_percent()))
    }+
    {if(sel == "Land use")  
      scale_y_continuous(sec.axis = sec_axis(~ . /scalefactor2, 
                                             labels = scales::label_percent(suffix = "%    ")))
    }+
    {if(sel == "Nitrogen")  
      scale_y_continuous(sec.axis = sec_axis(~ . /scalefactor3, 
                                             labels = scales::label_percent()))
    }+
    {if(sel == "Phosphorus")  
      scale_y_continuous(sec.axis = sec_axis(~ . /scalefactor4, 
                                             labels = scales::label_percent()))
    }+
    {if(sel == "GHG emissions")  
      scale_y_continuous(sec.axis = sec_axis(~ . /scalefactor5, 
                                             labels = scales::label_percent()))
    }+
    {if(sel == "Water")  
      scale_y_continuous(sec.axis = sec_axis(~ . /scalefactor6, 
                                             labels = scales::label_percent()))
    } +
    
    facet_wrap(~header, scales="free", nrow = 1) +
    theme_c1m3() +
    xlab("") + ylab("") +
    scale_fill_manual(values = rev(food_colours_10)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
          panel.grid.minor.y = element_line(linewidth = .1, color = "black"),
          axis.text.y.right  = element_text(color="#6c757d"),
          axis.line.y.right  = element_line(color="#6c757d"),
          legend.position    = "none"
    )
}

# Arrange
ggarrange(ggarrange(plotlist = plotlist,
                    nrow = 1),
          NULL,
          cat10_legend, 
          nrow = 3, heights = c(1, 0.02, 0.1))

# Save plot
ggsave("../build/figures/"%&%configpath%&%"_"%&%config$year_io%&%"/figure_fp_changes.pdf",
       width = 18, height = 7)


# __ text: footprint reductions -------------------------------------------

# absolute footprint reductions 
deltaabsfootprints <- df_plot_fp %>%
  group_by(policy, header) %>%
  mutate(footprint_reduction_abs = sum(footprint_reduction_abs)) %>% 
  select_if(grepl("policy|header|lowerbound|footprint_reduction_abs|higherbound",
                  names(.))) %>%
  distinct()

# relative footprint reductions
df_c_i <- list()
x <- 0
for (c in countries){
  if(c=="EL"){ c <- "GR"}
  for (g in 1:10){
    x <- x+1
    df_c_i[[x]] <- read.csv(config$procdatapath%&%
                              "aggr_footprints/"%&%config$year_io%&%"/"%&%
                              "by_foodcat/CBF_final_"%&%c%&%"_cat_"%&%g%&%".csv") %>%
      dplyr::transmute(str_imp, imp_reg, in_EU, value, 
                       demandcountry = c, 
                       foodgroup = g)
  }
}

absfootprints <- do.call(rbind.data.frame, df_c_i) %>% 
  # add unit and impact name
  left_join(stressors_impacts_selected, by = c("str_imp" = "code_s_i")) %>% 
  group_by(Unit, shortname, target.unit) %>% 
  dplyr::summarise(value = sum(value, na.rm = T)) %>% ungroup() %>% 
  # convert unit
  mutate(Unit = ifelse(Unit == "kg CO2 eq.",
                       "kg",
                       ifelse(Unit == "Gg CO2-eq",
                              "Gg",
                              Unit))) %>% 
  left_join(read.xlsx("00_data/manual_input/unit_conversion.xlsx"), 
            by = c("Unit" = "from")) %>% 
  mutate(conversion = ifelse(Unit == target.unit,
                             1,
                             conversion)) %>% 
  filter(target.unit == to | is.na(to)) %>% 
  # convert for better readability
  mutate(target.value = value*conversion) %>% 
  dplyr::transmute(shortname = paste0(shortname, "\n(", target.unit, ")"), 
                   target.value) 

deltaabsfootprints %>% 
  left_join(absfootprints, by = c("header" = "shortname")) %>%
  mutate(relreduction = footprint_reduction_abs/target.value)

# absolute difference between policies
deltaabsfootprints %>%
  pivot_wider(id_cols =c("header"),
              names_from = policy,
              values_from = footprint_reduction_abs) %>% 
  setNames(c("header", "GHG_emission_price", "VAT_reform")) %>%
  mutate(diff = VAT_reform-GHG_emission_price)

# relative difference between policies
deltaabsfootprints %>%
  pivot_wider(id_cols     = c("header"),
              names_from  = policy,
              values_from = footprint_reduction_abs) %>% 
  setNames(c("header", "GHG_emission_price", "VAT_reform")) %>%
  mutate(diff = GHG_emission_price/VAT_reform-1) 

# share of meat in total footprint reductions
df_plot_fp %>%
  mutate(meatdairy = ifelse(cat_no %in% c(6,7,8,9), 1, 0)) %>%
  group_by(policy, header, meatdairy) %>% 
  dplyr::summarise(footprint_reduction_abs = sum(footprint_reduction_abs, 
                                                 na.rm = T)) %>% 
  group_by(header, policy) %>% 
  mutate(total=sum(footprint_reduction_abs, na.rm = T)) %>%
  ungroup() %>%
  filter(meatdairy == 1) %>%
  mutate(sharemeat = footprint_reduction_abs/total) %>%
  arrange(header) 

# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------