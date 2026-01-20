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
  mutate(categorization = recode(categorization,
                           "Total\nconsumption" = "Total",
                           "Food\nconsumption" = "Food"),
         categorization = ordered(categorization, levels = c("Total", "Food")))

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
    geom_hline(yintercept = 0, linewidth = 0.1) +
    geom_text(aes(label = ifelse(is.na(share), "", 
                                 paste0(fill,":\n",
                                        share))), 
              angle = 90,
              size = 5 / .pt, 
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
            axis.ticks.y.right=element_blank(),
            legend.position="none") 
    }+
    {if(sel != "Biodiversity loss \n(PDF)")  
      theme(axis.text.x = element_text(angle = 0,vjust =0.5, hjust=0.5),
            axis.text.y.right=element_text(color="#6c757d"),
            axis.line.y.right=element_line(color="#6c757d"),
            axis.ticks.y.right=element_line(color="#6c757d"),
            legend.position="none") 
    }
}

# Arrange plot and prepared legend
ggarrange(ggarrange(plotlist = plotlist,
                    nrow = 1),
          NULL,
          cat10_legend, 
          nrow = 3, heights = c(1, 0.02, 0.15))

# Save plot
name_fig1 <- "../build/figures/Figure1"
ggsave(name_fig1%&%".pdf",
       width = 180,
       height = 80,
       units = "mm")

# (7b) Save source data
write.csv(df_all, 
          name_fig1%&%".csv",
          row.names = FALSE)

rm(df_1, df_2, df_3, df_plot, name_fig1)


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

# For source data: define filename
name_fig2 <- "../build/figures/Figure2"
file_footprints <- name_fig2%&%".csv"

# _ prepare shape file: EU and World --------------------------------------

# Load EXIO region data (needed for both EU and world maps)
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

# __ prepare world shape file (in line with EXIOBASE regions) -----------------
world_id_mapping <- read.csv("00_data/manual_input/mapdata/world_id1.csv") %>%
  select(cntry, iso2, EXIO_code)

world_sf_original <- ne_countries(scale = "small", returnclass = "sf") %>%
  filter(iso_a2 != "AQ") %>%
  mutate(
    # Normalize country names to match world_id1.csv
    name_normalized = case_when(
      name == "W. Sahara" ~ "Western Sahara",
      name == "Falkland Is." ~ "Falkland Islands / Malvinas",
      name == "Fr. S. Antarctic Lands" ~ "French Southern and Antarctic Lands",
      TRUE ~ name
    ),
    # Fix iso codes for mapping consistency
    iso_a2 = case_when(
      iso_a2 == "NA" ~ "NAM",
      iso_a2 == "-99" & name == "France" ~ "FR",
      iso_a2 == "-99" & name == "Norway" ~ "NO",
      iso_a2 == "-99" ~ name_long,
      TRUE ~ iso_a2
    )
  ) %>%
  # Join with EXIO regions (direct countries, then iso mapping, then name mapping)
  left_join(EXIO_region_df[, c("iso_2", "EXIO_code")], 
            by = c("iso_a2" = "iso_2")) %>%
  left_join(world_id_mapping %>% select(iso2, EXIO_code), 
            by = c("iso_a2" = "iso2"), suffix = c("", ".map1")) %>%
  left_join(world_id_mapping %>% select(cntry, EXIO_code), 
            by = c("name_normalized" = "cntry"), suffix = c("", ".map2")) %>%
  mutate(id = as.character(coalesce(EXIO_code, EXIO_code.map1, EXIO_code.map2)),
         id = ifelse(is.na(id) | id == "NA", as.character(row_number() + 1000), id)) %>%
  select(-starts_with("EXIO_code"), -name_normalized)

# Reassign Crimea to Ukraine's EXIO code
crimea_point_world <- st_sfc(st_point(c(34, 45)), crs = st_crs(world_sf_original))

ukraine_exio <- world_sf_original %>% filter(iso_a2 == "UA") %>% pull(id) %>% first()
russia_row <- world_sf_original %>% filter(iso_a2 == "RU")
russia_polygons <- st_cast(russia_row$geometry, "POLYGON")

crimea_mask <- sapply(russia_polygons, function(x) {
  st_intersects(x, crimea_point_world, sparse = FALSE)[1]
})

world_sf_corrected <- world_sf_original %>%
  filter(iso_a2 != "RU") %>%
  bind_rows(
    russia_row %>%
      mutate(geometry = st_combine(russia_polygons[!crimea_mask]) %>% st_cast("MULTIPOLYGON")),
    russia_row %>%
      mutate(geometry = russia_polygons[crimea_mask] %>% st_cast("POLYGON"),
             id = ukraine_exio)
  )

# Dissolve borders within each EXIOBASE region
sf_use_s2(FALSE)
world_sf_dissolved <- world_sf_corrected %>%
  mutate(geometry = suppressWarnings(st_buffer(geometry, dist = 0))) %>%
  group_by(id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  # Cast to MULTIPOLYGON to ensure proper structure
  st_cast("MULTIPOLYGON")
sf_use_s2(TRUE)

# Convert to data.frame for ggplot
world_sf_polygons <- world_sf_dissolved %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

world_df <- do.call(rbind, lapply(seq_len(nrow(world_sf_polygons)), function(i) {
  coords <- st_coordinates(world_sf_polygons[i,])
  if(nrow(coords) == 0) return(NULL)
  data.frame(
    long = coords[,1],
    lat = coords[,2],
    order = seq_len(nrow(coords)),
    piece = paste(i, coords[,3], sep = "."),
    group = paste(i, coords[,3], coords[,4], sep = "."),
    id = world_sf_polygons$id[i],
    stringsAsFactors = FALSE
  )
}))

# __ prepare shape file: EU-------------------------------------------------

shp_europe <- ne_countries(scale = "large", returnclass = "sf", continent = "Europe")
shp_missing <- ne_countries(scale = "large", returnclass = "sf", 
                            country = c("France", "Cyprus", "Turkey"))
shp_eu_all_original <- bind_rows(shp_europe, shp_missing)

# Reassign Crimea from Russia to Ukraine
crimea_point <- st_sfc(st_point(c(34, 45)), crs = st_crs(shp_eu_all_original))

russia_polygons <- shp_eu_all_original %>% 
  filter(iso_a2 == "RU") %>% 
  st_geometry() %>% 
  st_cast("POLYGON")

crimea_index <- sapply(russia_polygons, function(x) {
  st_intersects(x, crimea_point, sparse = FALSE)[1]
})
crimea_polygon <- russia_polygons[crimea_index]

new_russia <- russia_polygons[!crimea_index] %>% 
  st_combine() %>% 
  st_cast("MULTIPOLYGON")

ukraine_polygons <- shp_eu_all_original %>% 
  filter(iso_a2 == "UA") %>% 
  st_geometry() %>% 
  st_cast("POLYGON")

new_ukraine <- st_union(c(ukraine_polygons, crimea_polygon)) %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid()  # Clean up any invalid geometries from the union

sf_use_s2(FALSE)
shp_eu_all <- shp_eu_all_original %>%
  mutate(geometry = suppressWarnings(st_buffer(geometry, dist = 0)),
         geometry = case_when(
           iso_a2 == "UA" ~ new_ukraine,
           iso_a2 == "RU" ~ new_russia,
           .default = geometry
         ),
         geo = case_when(
           iso_a2 == "GR" ~ "EL",
           iso_a2 == "GB" ~ "UK",
           iso_a2_eh == "GB" ~ "UK",
           name == "France" ~ "FR",
           name == "Cyprus" ~ "CY",
           name == "Turkey" ~ "TR",
           TRUE ~ iso_a2
         )) %>%
  distinct(geo, .keep_all = TRUE)
sf_use_s2(TRUE)

# EU countries only (for footprint maps)
shp_eu <- shp_eu_all %>%
  filter(geo %in% countries) %>%
  dplyr::select(geo, geometry)

# Non-EU European countries (for background in grey)
shp_noneu <- shp_eu_all %>%
  filter(!geo %in% countries) %>%
  dplyr::select(geometry)

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

# (A) Plot EU food consumption footprints (saving footprint source data)
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


# (B) impacts from EU food consumption (saving impacts source data)
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
                 size     = 7/.pt,
                 family   = "Helvetica",
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
  NULL,
  # add column title to footprint maps
  ggarrange(ggplot() +
              annotate(label    = "Footprints",
                       geom     = "text",
                       x        = 0,
                       y        = 0,
                       size     = 7/.pt,
                       family   = "Helvetica",
                       fontface = "bold") +
              theme_void() +
              ggtitle(""),
            plotlist = align_plots(plotlist = unlist(lapply(plotlist_footprint_label, function(x) list(x, NULL)), recursive = FALSE),
                                   axis = "l"),
            ncol = 1, heights = c(0.15, rep(c(0.5, 0.001), length(plotlist_footprint_label)))),
  
  NULL,
  # add column title to impact maps
  ggarrange(ggplot() +
              annotate(label    = "Impacts",
                       geom     = "text",
                       x        = 0,
                       y        = 0,
                       size     = 7/.pt,
                       family   = "Helvetica",
                       fontface = "bold") +
              theme_void() +
              ggtitle(""),
            plotlist = align_plots(plotlist = unlist(lapply(plotlist_impact_bar, function(x) list(x, NULL)), recursive = FALSE),
                                   axis     = "l"),
            ncol = 1, heights = c(0.15, rep(c(0.5, 0.001), length(plotlist_impact_bar)))),
  
  NULL,
  # add legends
  ggarrange(ggplot() +
              annotate(label    = "",
                       geom     = "text",
                       x        = 0,
                       y        = 0,
                       size     = 5/.pt,
                       fontface = "bold") +
              theme_void() +
              ggtitle(""),
            plotlist = lapply(plotlist_legend, function(x){
              ggdraw(x)
            }),
            ncol = 1, heights = c(0.15, rep(0.5, 6))),
  NULL,
  
  ncol = 7, widths = c(0.5, # NULL
                       0.5, # Footprints (incl indicator name)
                       0.02, # NULL
                       0.8, # Impacts (incl bar)
                       0.1, # NULL
                       0.1, # Legend
                       0.5 # NULL
  ))


# save output
ggsave(name_fig2%&%".pdf",
       width = 180,
       height = 210,
       units = "mm")

# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------