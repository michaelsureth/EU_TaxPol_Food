# File info ---------------------------------------------------------------

# File:    Plot footprints
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# _____________________________________------------------------------------
# Preparations ------------------------------------------------------------

# Load selected stressors and impacts
stressors_impacts_selected   <- read.csv("../build/data/intermediate_output/"%&%
                                           "MRIO_stressors_impacts_final.csv") %>%
  select(-c("Landusetype", "Landusetype_coeff"))

# Load unit conversion
units_conversion <- read.xlsx("00_data/manual_input/unit_conversion.xlsx")

# Load EU population data
suppressMessages(
  EU_pop_year <- read_excel("00_data/tps00001_page_spreadsheet.xlsx",
                            sheet = "Sheet 1",
                            skip  = 7) %>% 
    dplyr::select(TIME, !!sym(config$year_io)) %>%
    setNames(c("name", "population")) %>% 
    filter(complete.cases(.)) %>% 
    mutate(name = ifelse(name == "Germany (until 1990 former territory of the FRG)",
                         "Germany",
                         name)) %>%
    arrange(name))

# _____________________________________------------------------------------
# Figure 1: Barplot of EU footprints --------------------------------------

# (1) Load food vs nonfood results (categorization = by_food_nonfood)
df_1 <- prepare_data_barplot(demandcountries    = countries, 
                             categorization     = "by_food_nonfood",
                             type               = "aggregate",
                             inout              = FALSE,
                             selectedindicators = selectedindicators_plot) %>%
  mutate(categorization = "Total\nconsumption",
         foodgroup      = "0") %>%
  # compute share
  group_by(x) %>%
  mutate(sum = sum(value.target, na.rm = T)) %>%
  ungroup() %>%
  mutate(share = paste0(round(100*value.target/sum,1), "%"))

# (2) Load food category results (categorization = by_foodcat)
df_2 <- prepare_data_barplot(demandcountries    = countries, 
                             categorization     = "by_foodcat",
                             type               = "aggregate",
                             inout              = FALSE,
                             selectedindicators = selectedindicators_plot) %>%
  mutate(categorization = "Food\nconsumption") %>%
  mutate(share = NA)

# (3) Copy nonfood values and with categorization = by_foodcat)
# (adding invisible bar on top of food consumption)
df_3 <- df_1 %>%
  filter(catname == "Non-food") %>%
  mutate(catname = "invisible nonfood", 
         share = NA,
         categorization = "Food\nconsumption",
         foodgroup = "100")

# (4) Add food system planetary boundaries from Willet et al (2019) Table 2
df_4 <- df_1 %>% filter(catname == "Food") %>% dplyr::select(shortname, value.target) %>% 
  dplyr::transmute(shortname, 
                   pb = with(.,case_when((shortname == "Land use \n(Mha)") ~ 1300, 
                                         (shortname == "Nitrogen \n(Mt)" ) ~ 90,
                                         (shortname == "Phosphorus \n(Mt)") ~ 8,
                                         (shortname == "GHG emissions \n(Mt)") ~ 5000,
                                         (shortname == "Water \n(Mm3)") ~ 2500000,
                                         # assign an irrelevant scale for biodiversity loss (no pb for global PDF)
                                         (shortname == "Biodiversity loss \n(PDF)") ~ 1,
                                         TRUE ~ NA)),
                   pb_rel = value.target/pb,
                   ratio = value.target/pb_rel)

# (5) Combine
df_all <- bind_rows(df_1, df_2, df_3) %>%
  mutate(fill = reorder(catname, as.numeric(foodgroup),  decreasing = F)) %>%
  dplyr::select(shortname, categorization, value.target, fill, share) %>%
  # add planetary boundaries
  bind_rows(df_4) %>%
  mutate(categorization = ifelse(!is.na(pb), "Food\nconsumption", categorization)) %>%
  mutate(categorization = ordered(categorization, 
                                  levels = c("Total\nconsumption", 
                                             "Food\nconsumption"))) 

# (6) Determine colours
custom_palette <- c("#495057", "#6c757d")
custom_palette <- c(custom_palette,
                    rev(food_colours_10),
                    "#FFFFFF00")

# (7a) Plot with share of planetary boundary on second axis  
plotlist <- list()
for(x in 1:6){
  
  sel <- levels(df_all$shortname)[x]
  df_plot <- df_all %>%  filter(shortname == sel & !is.na(categorization))
  
  # assign scalefactor
  assign(paste0("scalefactor", x), 
         df_plot %>% filter(!is.na(pb)) %>% 
           dplyr::select(ratio) %>% pull() %>% unique())
  
  plotlist[[x]] <- df_plot[order(df_plot$fill), ] %>% 
    ggplot(aes(x = categorization,
               y = value.target,
               fill = fill,
               group = shortname))+
    geom_bar(stat="identity") +
    geom_text(aes(label = ifelse(is.na(share), "", 
                                 paste0(fill,":\n",
                                        share))), 
              size = 5, 
              position = position_stack(vjust = 0.5, reverse = TRUE), 
              colour="white")+
    
    {if(sel == "Biodiversity loss \n(PDF)")  
      scale_y_continuous(label=comma,
                         sec.axis = sec_axis(~ . /scalefactor1, 
                                             labels = scales::label_percent()))
    }+
    {if(sel == "Land use \n(Mha)")  
      scale_y_continuous(label=comma,
                         sec.axis = sec_axis(~ . /scalefactor2, 
                                             labels = scales::label_percent()))
    }+
    {if(sel == "Nitrogen \n(Mt)")  
      scale_y_continuous(label=comma,
                         sec.axis = sec_axis(~ . /scalefactor3, 
                                             labels = scales::label_percent(suffix="%    ")))
    }+
    {if(sel == "Phosphorus \n(Mt)")  
      scale_y_continuous(label=comma,
                         sec.axis = sec_axis(~ . /scalefactor4, 
                                             labels = scales::label_percent()))
    }+
    {if(sel == "GHG emissions \n(Mt)")  
      scale_y_continuous(label=comma,
                         sec.axis = sec_axis(~ . /scalefactor5, 
                                             labels = scales::label_percent()))
    }+
    {if(sel == "Water \n(Mm3)")  
      scale_y_continuous(label=comma,
                         sec.axis = sec_axis(~ . /scalefactor6, 
                                             labels = scales::label_percent()))
    }+
    
    facet_wrap(~shortname, scales="free", nrow=1)+
    scale_fill_manual(values = custom_palette,
                      breaks = levels(df_plot$fill))+
    theme_c1m3()+
    
    {if(sel == "Biodiversity loss \n(PDF)")  
      theme(axis.text.x = element_text(angle = 0,vjust =0.5, hjust=0.5),
            axis.text.y.right=element_text(color="white"),
            axis.line.y.right=element_line(color="white"),
            legend.position="none") 
    }+
    {if(sel != "Biodiversity loss \n(PDF)")  
      theme(axis.text.x = element_text(angle = 0,vjust =0.5, hjust=0.5),
            axis.text.y.right=element_text(color="#6c757d"),
            axis.line.y.right=element_line(color="#6c757d"),
            legend.position="none") 
    }
}

# Arrange plot and prepared legend
ggarrange(ggarrange(plotlist = plotlist,
                    nrow = 1),
          NULL,
          cat10_legend, 
          nrow = 3, heights = c(1, 0.02, 0.1))

# Save
ggsave("../build/figures/figure_footprints_pb_"%&%config$year_io%&%".pdf",
       width = 22, height = 7)

rm(df_1, df_2, df_3, df_plot)


# other: for text and understanding ---------------------------------------

# for text: all food categories
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

# share of vegetarian categories in total footprint
do.call(rbind.data.frame, df_c_i) %>%
  mutate(veg = ifelse(foodgroup %in% c(1,2,3), 1, 0)) %>%
  group_by(str_imp, veg) %>%
  dplyr::summarise(value = sum(value, na.rm=T)) %>% ungroup() %>%
  group_by(str_imp) %>% mutate(total=sum(value, na.rm=T)) %>% ungroup() %>%
  filter(veg ==1) %>%
  mutate(shareveg = value/total) %>%
  left_join(stressors_impacts_selected, by=c("str_imp" = "code_s_i")) %>% 
  dplyr::select(-S_row_stressor_impact) %>%
  group_by(shortname) %>%
  dplyr::summarise(mean=mean(shareveg)) %>% arrange(mean)%>%
  filter(shortname %in% selectedindicators_plot) 

# share of meat and dairy in total footprint
do.call(rbind.data.frame, df_c_i) %>%
  mutate(meatdairy = ifelse(foodgroup %in% c(4, 6,7,8,9), 1, 0)) %>%
  group_by(str_imp, meatdairy) %>%
  dplyr::summarise(value = sum(value, na.rm=T)) %>% ungroup() %>%
  group_by(str_imp) %>% mutate(total=sum(value, na.rm=T)) %>% ungroup() %>%
  filter(meatdairy ==1) %>%
  mutate(sharemeat = value/total) %>%
  left_join(stressors_impacts_selected, by=c("str_imp" = "code_s_i")) %>% 
  dplyr::select(-S_row_stressor_impact) %>%
  group_by(shortname) %>%
  dplyr::summarise(mean=mean(sharemeat)) %>% arrange(mean) %>%
  filter(shortname %in% selectedindicators_plot)

# absolute impacts of food consumption 
# (also checked with total food footprints - same result as expected)
do.call(rbind.data.frame, df_c_i) %>% 
  # add unit and impact name
  left_join(stressors_impacts_selected, by = c("str_imp" = "code_s_i")) %>% 
  group_by(Unit, shortname, target.unit) %>% 
  dplyr::summarise(value = sum(value, na.rm=T)) %>% ungroup() %>% 
  # convert unit
  mutate(Unit = ifelse(Unit == "kg CO2 eq.","kg",
                       ifelse(Unit == "Gg CO2-eq", "Gg", Unit))) %>% 
  left_join(read.xlsx("00_data/manual_input/unit_conversion.xlsx"), 
            by = c("Unit" = "from")) %>% 
  mutate(conversion = ifelse(Unit == target.unit, 1, conversion)) %>% 
  filter(target.unit == to | is.na(to)) %>% 
  # convert for better readability
  mutate(target.value = value*conversion) %>% 
  dplyr::select(shortname, target.value, target.unit)

# absolute impacts expressed as share of planetary boundary
df_4 %>% arrange(pb_rel)

# by indicator: where are impacts generated (top 5)
do.call(rbind.data.frame, df_c_i) %>% 
  # add Unit and impact name
  left_join(stressors_impacts_selected, by = c("str_imp" = "code_s_i")) %>%
  filter(shortname %in% selectedindicators_plot) %>% 
  mutate(in_eu_ext = ifelse(in_EU == 1, "EU", imp_reg)) %>% 
  # alternatively: group EXIOBASE by region
  mutate(in_eu_ext = ifelse(in_EU == 1, "EU",
                            ifelse(imp_reg %in% c("Japan", "China", "South Korea", 
                                                  "India", "Taiwan", "Indonesia", 
                                                  "RoW Asia and Pacific"), "Asia",
                                   ifelse(imp_reg %in% c("South Africa","RoW Africa"), "Africa",
                                          ifelse(imp_reg %in% c("Brazil","Mexico","RoW America"), "MS America",
                                                 imp_reg))))) %>%
  group_by(in_eu_ext, shortname, Unit, target.unit) %>% 
  summarise(value = sum(value)) %>% 
  # add share
  group_by(shortname) %>% 
  mutate(total_value = sum(value),
         share = value/ total_value) %>%
  arrange(shortname, -value) %>% slice(1:4) %>% print(n = 24)

# Figure 2: Maps ----------------------------------------------------------

# _ load world shape file (in line with EXIOBASE regions) -----------------
load("./00_data/manual_input/mapdata/world_df.RData")

# _ prepare shape file: EU-------------------------------------------------

# Load EXIO region data
EXIO_region_df <- read.xlsx("00_data/manual_input/Bruckner2023.xlsx", 
                            sheet = "EXIOBASE_381_regions") %>% 
  mutate(iso_2_upd = ifelse(iso_2 == "GR",
                            "EL",
                            iso_2),
         region    = ifelse(iso_2 == "GB",
                            "UK",
                            ifelse(iso_2 == "US",
                                   "USA",
                                   cntry)))


# Load EU country-level shapefile from eurostat
years <- c(2003,2006,2010,2013,2016,2021)
shp_eu <- get_eurostat_geospatial(resolution = 10, 
                                  nuts_level = 0, 
                                  year       = years[which.min(abs(years - as.numeric(config$year_io)))]) %>% 
  dplyr::select(geo = NUTS_ID, geometry)  


# _ create Figure 2 -------------------------------------------------------

# Determine common legend scale (determined by max of either footprints or impacts)
legendscale_imp <- lapply(selectedindicators_plot, function(ind){
  create_footprint_map(
    what                 = "importedfrom",
    demandcountries      = countries, 
    indicator_name_short = ind,     
    categorization       = "by_food_nonfood",
    catnum               = 1,
    type                 = "total", 
    lims                 = "default")[[2]] 
}) %>% as.data.frame %>% setNames(selectedindicators_plot)

legendscale_fp <- lapply(selectedindicators_plot, function(ind){
  create_footprint_map(
    what                 = "footprint",
    demandcountries      = countries, 
    indicator_name_short = ind,     
    categorization       = "by_food_nonfood",
    catnum               = 1,
    type                 = "total", 
    lims                 = "default")[[2]] 
}) %>% as.data.frame %>% setNames(selectedindicators_plot)

legendscale <- as.data.frame(sapply(colnames(legendscale_imp), 
                                    function(col) pmax(legendscale_imp[[col]], 
                                                       legendscale_fp[[col]])))

# (A) EU food consumption footprints
plotlist_footprint <- lapply(selectedindicators_plot, function(ind){
  create_footprint_map(
    what                 = "footprint",
    demandcountries      = countries, 
    indicator_name_short = ind,     
    categorization       = "by_food_nonfood",
    catnum               = 1,
    type                 = "total", 
    lims                 = legendscale[[ind]])[[1]] +
    ggtitle(NULL)
}) %>%
  setNames(selectedindicators_plot)

# (B) impacts from EU food consumption
plotlist_impact <- lapply(selectedindicators_plot, function(ind){
  create_footprint_map(
    what                 = "importedfrom",
    demandcountries      = countries, 
    indicator_name_short = ind,     
    categorization       = "by_food_nonfood",
    catnum               = 1,
    type                 = "total", 
    lims                 = legendscale[[ind]])[[1]] +
    ggtitle(NULL)
}) %>%
  setNames(selectedindicators_plot)

# (C) impact share accruing in EU
plotlist_bar <- lapply(selectedindicators_plot, function(ind){
  simplebar_footprint_inout(demandcountries   = countries,
                            selectedindicator = ind,
                            categorization    = "by_food_nonfood",
                            category          = "Food",
                            type              = "aggregate")
}) %>%
  setNames(selectedindicators_plot)

# (D) Legend
plotlist_legend <- lapply(selectedindicators_plot, function(ind){
  get_legend(plotlist_footprint[[ind]])
}) %>%
  setNames(selectedindicators_plot)

# combine (A) with name of impact (add "y-axis labels" to figure)
plotlist_footprint_label <- lapply(selectedindicators_plot, function(ind){
  # create "first column" (impact name + footprint plot)
  ggarrange(
    # add padding around label
    ggarrange(
      NULL,
      # generate empty plot with impact name as text
      ggplot() +
        annotate(label    = ind,
                 geom     = "text",
                 x        = 0,
                 y        = 0,
                 angle    = 90,
                 size     = 8,
                 fontface = "bold") +
        theme_void() +
        ggtitle(""),
      NULL, 
      nrow = 3, heights = c(0.1, 1, 0.1)),
    # add padding around footprint plot (A)
    ggarrange(
      NULL,
      plotlist_footprint[[ind]]  +
        theme(legend.position = "none"),
      NULL,
      nrow = 3, heights = c(0.1, 1, 0.1)),
    ncol = 2, widths = c(0.1, 1))
})

# combine (B) and (C)
plotlist_impact_bar <- lapply(selectedindicators_plot, function(ind){
  plotlist_impact[[ind]]  +
    theme(legend.position = "none") +
    annotation_custom(ggplotGrob(plotlist_bar[[ind]]),
                      xmin = -200, ymin = -75, xmax = 200, ymax = -62)
}) %>%
  setNames(selectedindicators_plot)

# combine column titles ("Footprints" and "Impacts) with respective figure
# columns ((A)+label, (B)+(C)) and legend (D)
ggarrange(
  # add column title to footprint maps
  ggarrange(ggplot() +
              annotate(label    = "Footprints",
                       geom     = "text",
                       x        = 0,
                       y        = 0,
                       size     = 8,
                       fontface = "bold") +
              theme_void() +
              ggtitle(""),
            plotlist = align_plots(plotlist = plotlist_footprint_label,
                                   axis     = "l"),
            ncol = 1, heights = c(0.15, rep(1, 6))),
  NULL,
  # add column title to impact maps
  ggarrange(ggplot() +
              annotate(label    = "Impacts",
                       geom     = "text",
                       x        = 0,
                       y        = 0,
                       size     = 8,
                       fontface = "bold") +
              theme_void() +
              ggtitle(""),
            plotlist = align_plots(plotlist = plotlist_impact_bar,
                                   axis     = "l"),
            ncol = 1, heights = c(0.15, rep(1, 6))),
  NULL,
  # add legends
  ggarrange(ggplot() +
              annotate(label    = "",
                       geom     = "text",
                       x        = 0,
                       y        = 0,
                       size     = 8,
                       fontface = "bold") +
              theme_void() +
              ggtitle(""),
            plotlist = lapply(plotlist_legend, function(x){
              ggdraw(x)
            }),
            ncol = 1, heights = c(0.15, rep(1, 6))),
  ncol = 5, widths = c(0.7, 0.02, 1, 0.02, 0.2))


# save output
ggsave("../build/figures/figure_footprint_maps_"%&%config$year_io%&%".pdf",
       width = 15, height = 28)

# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------