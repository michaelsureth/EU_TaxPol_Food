# File info ---------------------------------------------------------------

# File:    Functions and themes for plotting
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# Colour vectors ----------------------------------------------------------

# Food item colours
food_colours_10  <- c("#bdbdbd", "#FFA500", "#FF1000", "#CB0033", "#7B0E25", 
                      "#9ABBE0", "#558DCD", "#115F1B", "#5C9C4C", "#A8DB7F")

# Policy colours
policy_colours   <- c("#F4B942", "#053061")

# Neutral colours (used to define palettes of flexible length)
colours_blue_red <- c("#053061", "#D1E5F0", "#D6604D", "#67001F")

# Environmental indicator colours
col_intensity_indicators <- data.frame(indicator = 
                                         c("Biodiversity loss", "Carbon dioxide (CO2)", 
                                           "GHG emissions", "Methane (CH4)", 
                                           "Nitrogen", "Nitrous Oxide (N2O)",
                                           "Phosphorus","Land use", 
                                           "Water"),
                                       col_intensity = c("#264653", "#fff8d6", 
                                                         "#a50f15", "#fff8d6",
                                                         "#e9c46a", "#fff8d6", 
                                                         "#f4a261", "#2a9d8f", 
                                                         "#457b9d"))


# Food category legend ----------------------------------------------------

# Create any plot
cat10_plot <- fread(config$procdatapath%&%"intermediate_output/"%&%
                      "HBS_catexplain_cat10.csv",
                    data.table = FALSE) %>% 
  mutate(fill  = reorder(category_name_new, as.numeric(category_code_new), 
                         decreasing = F),
         value = 1) %>%
  ggplot(aes(x = category_name_new, y = value, fill = fill)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rev(food_colours_10)) + 
  theme(legend.title = element_blank(),
        legend.text  = element_text(size = 5),
        legend.key.size = unit(0.3, "cm"),
        legend.key = element_rect(fill = NA, colour = NA))+
  guides(fill = guide_legend(reverse = F, nrow = 2, byrow = TRUE,
                             override.aes = list(colour = NA))) 

# Extract legend
cat10_legend <- cowplot::get_legend(cat10_plot)

rm(cat10_plot)


# Standard plot format ----------------------------------------------------
# building on BBC formatting scripts (https://github.com/bbc/bbplot)
    
theme_c1m3 <- function(){
  
  # choose font (e.g., from “Font Book” application on Mac)
  font <- "Helvetica"
  
  ggplot2::theme(
    
    base_size = 6,
    
    # title, subtitle and caption
    plot.title = ggplot2::element_text(hjust  = 0,
                                       family = font,
                                       size   = 7,
                                       face   = "bold",
                                       margin=margin(0,0,15,0)),
    
    plot.subtitle = ggplot2::element_text(family = font,
                                          size   = 6,
                                          margin = ggplot2::margin(9, 0, 9, 0)),
    
    plot.caption = ggplot2::element_text(family = font,
                                         size   = 6),
    
    # legend
    legend.position   = "bottom",
    legend.text       = ggplot2::element_text(family = font,
                                              size   = 5),
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title      = ggplot2::element_blank(),
    legend.key        = ggplot2::element_blank(),
    
    # axes
    axis.title  = ggplot2::element_blank(),
    axis.text   = ggplot2::element_text(family = font,
                                        size   = 5,
                                        color  = "#222222"),
    axis.line   = ggplot2::element_line(color = "#222222"),
    
    # panel grid 
    panel.grid.minor   = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    
    # background
    panel.background = ggplot2::element_blank(),
    # background facet plots
    strip.background = element_blank(),
    # title for facet-wrapped plots
    strip.text = ggplot2::element_text(size  = 5,
                                       hjust = 0.5,
                                       face = "bold")
  )
}

  


# DSE - Plot functions ----------------------------------------------------

plot_CPEs <- function(type, 
                      direction, 
                      lims,
                      averaging,
                      p_to_norm       = config$p_to_norm,
                      prices          = config$prices, 
                      y               = config$y,
                      cens_yesno      = config$cens_yesno,
                      weight_yesno    = config$weight_yesno,
                      equations       = config$equations,
                      ycentered_yesno = config$ycentered_yesno,
                      shares          = config$shares,
                      dse             = config$dse){ 
  
  #' @name plot_CPEs
  #' @title Plot cross-price elasticites as density plots (for a single 
  #' selected demand system specification)
  #' @param type can be one of "comp" or "uncomp"
  #' @param direction can be one of "to" or "from" (choose row/col assignment)
  #' @param lims x-axis limits (elasticity values)
  #' @param averaging can be one of "ref" or "mean" 
  #' @return plot_return
  
  # see https://cran.r-project.org/web/packages/ggridges/vignettes/gallery.html
  # for documentation 
  
  # Load elasticity results
  files <- list.files(config$procdatapath%&%"easi_estimates/elasticities/"%&%
                        configpath%&%"/", pattern = "cpe_"%&%type)
  
  df <- list()
  x <- 0
  
  for(f in files){
    
    x <- x + 1
    
    # generate country abbreviation from loaded file
    c <- as.vector(str_match(f, "(?<=comp_).{2}"))
    
    # load elasticity results 
    df[[x]] <- fread(config$procdatapath%&%"easi_estimates/elasticities/"%&%
                       configpath%&%"/"%&%f,
                     data.table = FALSE) %>%
      setNames(c("coef", "value")) %>%
      mutate(country       = c,
             configuration = configpath)
    
  }
  
  # Turn list into df
  df_all <- do.call(rbind.data.frame, df) 
  
  df_plot_list <- list()
  
  for(cat in as.character(1:nrow(catexplain))){
    
    # save category name
    catname <- catexplain %>%
      dplyr::filter(category_code_new == cat) %>%
      dplyr::select(category_name_new) %>% pull()
    
    # Filter relevant elasticity estimates
    if(direction == "from"){
      pattern <- "^(CPE_"%&%type%&%"_i"%&%cat%&%")(?!(10|[[:digit:]]))"
      other   <- "to"
    } else if(direction == "to"){
      pattern <- "^(CPE_"%&%type%&%"_i)(10|[[:digit:]])(j"%&%cat%&%")(_"%&%averaging%&%")"
      other   <- "from"
    }
    
    # Highlight OPEs 
    pattern_ope <- "CPE_"%&%type%&%"_i"%&%cat%&%"j"%&%cat
    
    # Create dataframe to plot
    df_plot_list[[cat]] <- df_all %>% 
      
      # filter relevant coefficients
      filter(coef %in% grep(pattern,   coef, value = TRUE, perl = TRUE)) %>%
      # filter relevant averaging
      filter(coef %in% grep(averaging, coef, value = TRUE, perl = TRUE)) %>% 
      
      # generate variables
      transmute(coef, value, country, configuration,
                # ope or cpe
                ope_or_cpe = as.factor(ifelse(coef %in% grep(pattern_ope, 
                                                             coef, 
                                                             value = TRUE, 
                                                             perl  = TRUE),
                                              1,
                                              0)), 
                # keep everything after i and before j
                from = as.integer(sub("\\j.*", "", sub(".*i", "", coef))),
                # keep everything after j and before _
                to   = as.integer(sub("\\_.*", "", sub(".*j", "", coef)))) %>%
      # rename to match with category names 
      dplyr::rename(category_code_new = !!sym(other)) %>% 
      left_join(catexplain, by = "category_code_new") %>%
      mutate(term    = reorder(category_name_new, category_code_new, decreasing = T),
             catname = catname)
  }
  
  # List to dataframe (arrange order for facet_grid)
  df_plot <- do.call(rbind.data.frame, df_plot_list) %>%
    left_join(catexplain, by = c("catname" = "category_name_new")) %>%
    mutate(catname = reorder(catname, category_code_new.y, decreasing = FALSE)) 
  
  # Save source data
    write.csv(df_plot %>% 
                dplyr::select(category.x = catname, category.y = category_name_new, country, value), 
              name_figE1%&%".csv", 
              row.names = FALSE)

  # Plot own- and cross-price elasticities with density plot
  plot_return <- df_plot %>%
    ggplot(aes(x           = value,
               y           = term, 
               color       = ope_or_cpe, # line
               point_color = ope_or_cpe, # points
               fill        = ope_or_cpe)) + 
    geom_density_ridges(aes(height = after_stat(density)),
                        stat           = "density",
                        rel_min_height = 0.001,
                        scale          = 0.8,
                        linewidth = 0.3) +
    geom_point(shape = 108, size = 1) +
    geom_vline(xintercept =  0, colour = "black",  linetype = 1, linewidth = 0.2) +
    geom_vline(xintercept = -1, colour = "grey60", linetype = 2, linewidth = 0.2) +
    facet_grid(~catname,
               labeller = label_wrap_gen(width = 11, multi_line = TRUE)) +
    scale_fill_manual( values = c("#669bbc40","#00304940")) +
    scale_color_manual(values = c("#669bbc",  "#003049"), 
                       guide  = "none") +
    scale_discrete_manual("point_color", 
                          values = c("#669bbc","#003049"), 
                          guide  = "none") +
    scale_y_discrete(expand = c(0.0, 0)) +
    scale_x_continuous(breaks = c(-1, 0, 1), limits = lims)+
    ylab("") + xlab("") +
    theme_c1m3() +
    theme(
      strip.text      = element_text(size = 5, hjust = 0.7, face = "plain"),
      axis.line.x     = element_line(color = "grey", linewidth = 0.3),
      panel.grid.major.y =  element_line(color = "grey", linewidth = 0.2), 
      axis.line.y     = element_blank(),
      axis.text.x     = element_text(size = 5),
      axis.text.y     = element_text(size = 5),
      legend.position = "none"
    )
  
  return(plot_return)
}


# MRIO - Plot functions ---------------------------------------------------

prepare_data_barplot <- function(demandcountries,
                                 categorization,
                                 type               = "aggregate",
                                 inout              = TRUE,
                                 year_io            = config$year_io,
                                 selectedindicators = c("Biodiversity loss",
                                                        "Carbon dioxide (CO2)", 
                                                        "GHG emissions",
                                                        "Methane (CH4)",
                                                        "Nitrogen",
                                                        "Nitrous Oxide (N2O)",
                                                        "Phosphorus",
                                                        "Land use",
                                                        "Water")){
  
  #' @name prepare_data_barplot
  #' @title Prepare data for footprint barplots
  #' @param demandcountries Vector of country codes (e.g. EU_all_codes)
  #' @param categorization can be one of "by_food_nonfood" or "by_foodcat"
  #' @param type can be one of "aggregate" or "bycountry"
  #' @param inout can be TRUE or FALSE depending on whether distinction should
  #' be made between within/outside EU27
  #' @param selectedindicators Vector of environmental indicators to plot
  #' @param year_io selected EXIOBASE year
  #' @return df_plot
  
  # determine results path depending on categorization
  resultpath <- config$procdatapath%&%"aggr_footprints/"%&%year_io%&%"/"%&%
    categorization%&%"/"
  
  # list configuration options
  files <- list.files(resultpath, pattern = "CBF_final_") 
  cats  <- gsub( "\\D", "", files) %>% unique()
  
  # change categorization name if "by_foodcat"
  if(categorization == "by_foodcat"){categorization <- config$categorization}
  
  # load option names
  option_names <- openxlsx::read.xlsx("00_data/manual_input/exiobase_product_grouping.xlsx",
                                      sheet = categorization,
                                      rows  = 1:2,
                                      colNames = FALSE) %>% 
    t() %>% as.data.frame() %>% setNames(c("catname", "catno"))
  
  # Load data
  df_c_i <- list()
  x <- 0
  
  # For all selected countries
  for (c in demandcountries){
    
    if(c=="EL"){ c <- "GR"}
    
    # load EE-MRIO results
    for (g in cats){
      x <- x+1
      df_c_i[[x]] <- read.csv(resultpath%&%
                                "CBF_final_"%&%c%&%"_cat_"%&%g%&%".csv") %>%
        dplyr::transmute(str_imp, imp_reg, in_EU, value, 
                         demandcountry = c, 
                         foodgroup     = g)
    }
  }
  
  # Create dataframe to plot
  df_all <- do.call(rbind.data.frame, df_c_i) %>% 
    
    # filter for correct indicator
    left_join(stressors_impacts_selected, by = c("str_imp" = "code_s_i")) %>% 
    dplyr::select(-S_row_stressor_impact) %>%
    filter(shortname %in% selectedindicators) %>%
    
    # add category names
    full_join(option_names, by = c("foodgroup" = "catno")) %>%
    
    # drop CO2eq from original unit to enable flexible conversion
    mutate(Unit = gsub(" CO2 eq.| CO2-eq", "", Unit)) %>%
    # unit conversion
    left_join(units_conversion, by = c("Unit"        = "from",
                                       "target.unit" = "to")) %>%
    mutate(conversion = ifelse(is.na(conversion) & Unit == target.unit, 
                               1, 
                               conversion)) %>%
    mutate(value.target = value*conversion)
  
  if(inout == FALSE){
    df_all$in_EU <- NA
  }
  
  # sum over all importing regions and demandcountries
  if(type =="aggregate"){
    df_all <- df_all %>% 
      mutate(x = shortname) %>%
      group_by(str_imp, in_EU, foodgroup, catname, x, shortname, target.unit) 
  } else if(type == "bycountry"){
    df_all <- df_all %>% 
      mutate(x = demandcountry) %>%
      group_by(x, str_imp, in_EU, foodgroup, catname, shortname, target.unit) 
  }
  
  df_plot <- df_all %>% 
    dplyr::summarise(value.target = sum(value.target, na.rm = T)) %>% ungroup() %>%
    mutate(in_EU = ifelse(in_EU == 1,
                          "within",
                          ifelse(in_EU == 0,
                                 "outside",
                                 NA))) %>%
    mutate(catname_displ = as.factor(paste0(catname, "_", in_EU))) %>% 
    mutate(fill = reorder(catname_displ, as.numeric(foodgroup),  
                          decreasing = FALSE)) %>% 
    # correct order
    left_join(unique(selectedindicators_df[,c("plotgroup", "n")]), 
              by = c("shortname"="plotgroup")) %>%
    mutate(shortname = paste0(shortname, " \n(", target.unit,")")) %>% 
    mutate(shortname = reorder(shortname, n,  decreasing = FALSE)) %>% 
    # compute share
    group_by(str_imp, shortname, target.unit) %>%
    mutate(total = sum(value.target)) %>% ungroup() %>%
    mutate(share = value.target/total) 
  
  return(df_plot)
  
}



simplebar_footprint_inout <- function(demandcountries,
                                      categorization,
                                      category,
                                      selectedindicator,
                                      type = "aggregate"){
  
  #' @name simplebar_footprint_inout
  #' @title Horizontal bar displaying within vs. outside EU share
  #' @param demandcountries Vector of country codes (e.g. EU_all_codes)
  #' @param categorization can be one of "by_food_nonfood" or "by_foodcat"
  #' @param category can be one of the categories in the respective categorization (e.g. "Food")
  #' @param type can be one of "aggregate" or "bycountry"
  #' @param selectedindicator Can be one of "Biodiversity loss, "Carbon dioxide (CO2)", "GHG emissions",
  #' "Methane (CH4)", "Nitrogen", "Nitrous Oxide (N2O)", "Phosphorus", "Land use", "Water"
  #' @return barplot
  
  df_plot <- prepare_data_barplot(demandcountries, 
                                  categorization,
                                  type,
                                  inout = TRUE,
                                  selectedindicators = selectedindicator) %>%
    # filter for category and recompute shares within category
    filter(catname == category) %>%
    mutate(total = sum(value.target),
           share = value.target/total) 
  
  # Save source data
  df_save <- df_plot %>% 
    transmute(country = NA, 
              value = share, 
              var = catname_displ, 
              indicator = selectedindicator)
  
  if (file.exists(file_footprints)) {
    write.csv(unique(rbind(read.csv(file_footprints), df_save)), 
              file_footprints, 
              row.names = FALSE)
  } else {
    write.csv(df_save, 
              file_footprints, 
              row.names = FALSE)
  }
  
  # pull indicator colour
  colour <- col_intensity_indicators %>%
    filter(indicator == selectedindicator) %>% 
    dplyr::select(col_intensity) %>% pull()
  
  custom_palette <- c(colour, "white")
  
  # create plot
  barplot <- df_plot %>%
    ggplot(aes(fill = forcats::fct_rev(fill),
               y    = value.target,
               x    = x)) +
    geom_bar(position = position_stack(reverse = TRUE),
             stat     = "identity",
             color    = "black",
             size     = 0.2,
             width = 2) +
    geom_text(aes(label  = paste0(in_EU, " EU: ", round(100*share,0), "%"),
                  colour = forcats::fct_rev(in_EU)),
              size     = 5/.pt,
              position = position_stack(vjust = 0.5, reverse = TRUE)) +
    scale_color_manual(values = c("white", "black")) +
    scale_fill_manual(values  = custom_palette) +
    scale_y_continuous(label  = comma) +
    coord_flip() +
    theme_void() +
    xlab("") + ylab("") + 
    theme(legend.position = "none") 
  
  return(barplot)
}




create_footprint_map <- function(what, 
                                 demandcountries, 
                                 indicator_name_short, 
                                 categorization,
                                 catnum,
                                 type = "total", 
                                 lims = "default"){
  
  #' @name create_footprint_map
  #' @title Create footprint map of EU27 footprints or global impacts from 
  #' EU27 consumption
  #' @param what can be one of "footprint" or "importedfrom"
  #' @param demandcountries vector of country codes
  #' @param indicator_name_short Can be one of "Biodiversity loss", "Carbon dioxide (CO2)", 
  #' "GHG emissions", "Methane (CH4)", "Nitrogen", "Nitrous Oxide (N2O)", "Phosphorus",
  #' "Land use", "Water")
  #' @param categorization choose by which categorization footprints should be plotted. 
  #' can be one of "by_food_nonfood" or "by_foodcat".
  #' @param catnum category number within categorization
  #' @param type "total" or "percapita" (only relevant if what==footprint)
  #' @param lims Can be "default" or own vector of plot limits
  #' @return list containing plot_return and plot_lims 
  
  # Determine colour palette for chosen indicator
  colour_indicator <- col_intensity_indicators %>%
    dplyr::filter(indicator == indicator_name_short) %>%
    dplyr::select(col_intensity) %>% pull()
  col_intensity <- colorRampPalette(c("#FFFFFF", colour_indicator))(7)
  
  # Determine results path depending on categorization
  resultpath <- config$procdatapath%&%
    "aggr_footprints/"%&%config$year_io%&%"/"%&%categorization%&%"/"
  
  # change categorization name if "by_foodcat"
  if(categorization == "by_foodcat"){categorization <- config$categorization}
  
  # Load option name
  option_name <- openxlsx::read.xlsx("00_data/manual_input/exiobase_product_grouping.xlsx",
                                     sheet    = categorization,
                                     rows     = 1:2,
                                     colNames = FALSE) %>% 
    t() %>% as.data.frame() %>% setNames(c("catname", "catno")) %>%
    filter(catno == catnum) %>% dplyr::select(catname) %>% pull()
  
  # Stop if catnum-categorization combination does not exist
  if(is.empty(option_name)){
    stop(paste0("catnum ", catnum, " is not available in categorization ", categorization))
  }
  
  # Load footprints for all countries for selected category
  
  # create empty list
  df_all_c <- list()
  
  # For all selected countries
  for (c in demandcountries){
    
    if(c=="EL"){ c <- "GR"}
    
    df_all_c[[c]] <- fread(resultpath%&%
                             "CBF_final_"%&%c%&%"_cat_"%&%catnum%&%".csv",
                           data.table = FALSE) %>% 
      
      # save demand country
      mutate(demandcountry = c) %>% 
      
      # filter for correct indicator
      filter(shortname == indicator_name_short) %>% 
      
      # drop CO2eq from original unit to enable flexible conversion
      mutate(Unit = gsub(" CO2 eq.| CO2-eq", "", Unit)) %>% 
      # unit conversion
      left_join(units_conversion, by = c("Unit"        = "from",
                                         "target.unit" = "to")) %>%
      mutate(conversion   = ifelse(is.na(conversion) & Unit == target.unit,
                                   1,
                                   conversion),
             value.target = value*conversion) %>%
      
      # add country population of demand country
      left_join(EU27, by=c("demandcountry" = "geo")) %>% 
      rename(demandcountry_name = name) %>%
      mutate(demandcountry_name = ifelse(demandcountry == "GR", 
                                         "Greece", 
                                         demandcountry_name)) %>%
      left_join(EU_pop_year, by=c("demandcountry_name" = "name"))
  }
  
  # bind list to dataframe
  df_all <- do.call(rbind.data.frame, df_all_c)
  df_all$demandcountry[df_all$demandcountry == "GR"] <- "EL"
  
  # Two options ("what")
  # (1) show footprints of consuming country (requires EU map only)
  # (2) show where impacts are imported from (requires global map)
  
  if (what == "footprint"){
    
    # Data to plot
    if(type=="total"){
      df_plot <- df_all %>%
        group_by(demandcountry) %>%
        dplyr::summarise(value.target = sum(value.target, na.rm = TRUE))
      
      # Save source data
      df_save <- df_plot %>% 
        transmute(country = demandcountry, 
                  value = value.target, 
                  var = what, 
                  indicator = indicator_name_short)
      
      if (file.exists(file_footprints)) {
        write.csv(unique(rbind(read.csv(file_footprints), df_save)), 
                  file_footprints, 
                  row.names = FALSE)
      } else {
        write.csv(df_save, 
                  file_footprints, 
                  row.names = FALSE)
      }
      
    }else if(type=="percapita"){
      df_plot <- df_all %>%
        mutate(value.target = value/population) %>% # note we call this value.target even though we use orig value
        group_by(demandcountry) %>%
        dplyr::summarise(value.target = sum(value.target, na.rm = TRUE))
    }
    
    # Plot settings: title
    titleprep <- indicator_name_short%&%" footprint ("%&%type%&%
      " consumption of "%&%option_name%&%")"
    
    # Plot settings: Legend title
    if(type=="total"){
      plot_legend <- str_pad(indicator_name_short%&%" ("%&%df_all$target.unit[1]%&%")",
                             width = 23,
                             side  = "right",
                             pad   = " ")     
      plot_legend <- str_pad(df_all$target.unit[1],
                             width = 3,
                             side  = "right",
                             pad   = " ")
    } else if(type=="percapita"){
      plot_legend <- str_pad(df_all$Unit[1]%&%" (p.c.)",
                             width = 15,
                             side  = "right",
                             pad   = " ")
    }
    
    # Plot settings: Limits
    if(identical(lims, "default")){
      plot_lims <- c(0, max(df_plot$value.target))
    } else {
      plot_lims <- lims 
    }
    
    # Add spatial dimension to dataframe - EU shape file
    df_plot_shp <- df_plot %>% 
      full_join(shp_eu, by = c("demandcountry"="geo")) %>% 
      st_as_sf()
    
    # Plot EU with non-EU countries in background
    plot_return <- ggplot() +
      geom_sf(data = shp_noneu, fill = "#7F7F7F", color = "#595959", size = 0.05) +  # Match border styling
      geom_sf(data = df_plot_shp, aes(fill = value.target), color = "#595959", size = 0.05) +  # Add explicit border color
      scale_fill_gradientn(limits  = plot_lims,
                           colours = col_intensity, 
                           name    = plot_legend) +
      theme_void(base_size = 5)+
      theme(legend.text       = element_text(size   = 5,
                                             family = "Helvetica"),
            legend.title      = element_text(size   = 5,
                                             family = "Helvetica"),
            legend.key.size   = unit(0.3, "cm")) +
      ggtitle(titleprep)+
      scale_x_continuous(limits = c(-10, 35)) +
      scale_y_continuous(limits = c( 35, 70))
    
  } else if(what == "importedfrom"){
    
    # Data to plot
    if(type=="total"){
      df_plot <- df_all %>%
        group_by(imp_reg, imp_reg_code) %>%
        dplyr::summarise(value.target = sum(value.target, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(imp_reg_code = as.character(imp_reg_code))
      
      # Save source data
      df_save <- df_plot %>% 
        mutate(imp_reg_code = as.double(imp_reg_code)) %>%
        left_join(EXIO_region_df[,c("EXIO_code", "iso_2")], by = c("imp_reg_code"="EXIO_code")) %>%
        transmute(country = iso_2, 
                  value = value.target, 
                  var = "impact", 
                  indicator = indicator_name_short)
      if (file.exists(file_footprints)) {
        write.csv(unique(rbind(read.csv(file_footprints), df_save)), 
                  file_footprints, 
                  row.names = FALSE)
      } else {
        write.csv(df_save, 
                  file_footprints, 
                  row.names = FALSE)
      }
      
    }else if(type=="percapita"){
      break("Option what==importedfrom not available in combination with type==percapita.")
    }
    
    # Plot settings: title
    titleprep <- "Distribution of "%&%indicator_name_short%&%
      " associated with category "%&%option_name%&%" consumption"
    
    # Plot settings: Legend title
    plot_legend <- str_pad(df_all$target.unit[1],
                           width = 3,
                           side  = "right",
                           pad   = " ")
    
    # Plot settings: Limits
    if(identical(lims, "default")){
      plot_lims <- c(0,max(df_plot$value.target))
    } else {
      plot_lims <- lims
    }
    
    # Add spatial dimension to dataframe
    world_for_plot <- world_df %>%
      left_join(df_plot, by = c("id" = "imp_reg_code"))
    
    # Plot world map
    plot_return <- ggplot(data = world_for_plot) + 
      geom_polygon(aes(x     = long,
                       y     = lat,
                       group = group,
                       fill  = value.target), 
                   colour = "black", linewidth = 0.05) +
      scale_fill_gradientn(colours = col_intensity,  
                           limits  = plot_lims, 
                           name    = plot_legend) +
      coord_equal(1.3) +
      theme_void() +
      ggtitle(titleprep)
  }
  
  return(list(plot_return, plot_lims))
}



plot_intensities_point <- function(categorization     = "by_foodcat",
                                   selectedindicators = c("Biodiversity loss",
                                                          "Carbon dioxide (CO2)", 
                                                          "GHG emissions",
                                                          "Methane (CH4)",
                                                          "Nitrogen",
                                                          "Nitrous Oxide (N2O)",
                                                          "Phosphorus",
                                                          "Land use",
                                                          "Water")){ 
  
  #' @name plot_intensities_point
  #' @title Plot environmental intensities by country and category
  #' @param categorization can be one of "by_foodcat" or "by_food_nonfood" 
  #' @param selectedindicators Vector of environmental indicators to plot
  #' @return return_plot
  
  # Load category names
  if(categorization == "by_foodcat"){categorization <- "cat10"}
  
  option_names <- openxlsx::read.xlsx("00_data/manual_input/exiobase_product_grouping.xlsx",
                                      sheet    = categorization,
                                      rows     = 1:2,
                                      colNames = FALSE) %>% 
    t() %>% as.data.frame() %>% setNames(c("catname", "catno"))
  
  if(categorization == "cat10"){categorization <- "by_foodcat"}
  
  # Load data
  intensities_all <- fread("../build/data/intensities/"%&%config$year_io%&%
                             "/intensities_"%&%categorization%&%".csv",
                           data.table = FALSE) %>% 
    # filter relevant countries and indicators
    filter(impact_shortname %in% selectedindicators) %>%
    # add category names
    mutate(catno = as.character(category)) %>%
    full_join(option_names, by = "catno") %>%
    # add country names
    left_join(EU27, by = c("demandcountry" = "geo")) %>%
    mutate(name = ifelse(demandcountry == "GR",
                         "Greece",
                         name)) %>%
    # arrange category names in correct order
    mutate(catname = reorder(catname, as.numeric(catno),  decreasing = FALSE)) %>%
    # correct impact order
    left_join(selectedindicators_df, by = c("impact_shortname" = "shortname")) %>%
    mutate(header = paste0(impact_shortname, " intensity\n (", unit_intensity_final,")")) %>%
    mutate(header = reorder(header, n,  decreasing = F)) 
  
  # Point plot: mean intensities + country intensities 
  return_plot <- intensities_all %>%
    filter(demandcountry %in% countries) %>%
    filter(impact_shortname %in% selectedindicators) %>%
    # keep only variables of interest
    transmute(catname, demandcountry, header,
              mean_intensity = intensity_new,
              type           = "country") %>% 
    # compute mean intensity
    group_by(catname, header) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), mean, na.rm = TRUE),
                        across(where(is.character), ~"Mean"))) %>%
    ungroup() %>%
    
    ggplot(aes(x     = catname,
               y     = mean_intensity,
               group = type)) + 
    geom_point(aes(shape  = type, 
                   colour = type,
                   size   = type)) +
    facet_grid(~ header, scales = "free") +
    geom_hline(yintercept = 0,
               colour     = "black",
               linetype   = 1,
               linewidth  = 0.5) +
    coord_flip() +
    scale_x_discrete(limits = rev) +
    scale_shape_manual(values = c(124, 124))+
    scale_color_manual(values = c("darkgrey", "black")) +
    scale_size_manual(values  = c(2.5, 6)) +
    xlab("") + ylab("") + 
    theme_c1m3() + 
    theme(panel.grid.major.x = element_line(linewidth = 0.5,
                                            color     = "grey"),
          strip.text         = element_text(hjust  = 0,
                                            margin = margin(1, 0, 10, 0)),
          axis.line          = element_blank(),
          plot.margin        = margin(0,20,0,0),
          legend.position    = "none")
  
  return(return_plot)
}



# Policy robustness - Plot functions --------------------------------------

plot_policies_compare <- function(countries,
                                  rob_options        = c(),
                                  graph              = "reduction",
                                  selectedindicators = selectedindicators_plot,
                                  pol                = "tax_GHG emissions"){ 
  
  #' @name plot_policies_compare
  #' @title Plot comparing reductions and carbon prices for selected polices 
  #' over different configurations
  #' @param countries 
  #' @param rob_options 
  #' @param graph can be one of "reduction" or "carbonprice"
  #' @param selectedindicators Vector of environmental indicators to plot
  #' @param pol can be one of "tax_GHG emissions" or "VAT_increase" (note that
  #' VAT_increase is only relevant if graph = "reduction")
  #' @return plot_return
  
  # see https://cran.r-project.org/web/packages/ggridges/vignettes/gallery.html
  # for documentation 
  
  # Check compatibility of selected plot options
  if(graph == "carbonprice" & pol!= "tax_GHG emissions"){
    stop("Settings are incompatible (carbon price graph needs to be combined with 
         pol = tax_GHG emissions")
  }
  
  # Define elasticity path
  path_carbonprice <- config$procdatapath%&%"policies/"%&%pol%&%"/"
  
  # Load all available configurations unless otherwise specified
  if(is.null(rob_options)){
    rob_options <- sub(path_carbonprice%&%"/", "",
                       list.dirs(path_carbonprice, recursive = FALSE))
  }
  
  # For all robustness options
  df_rob_all <- list()
  x <- y <- 0
  
  for (rob in rob_options){
    
    # Drop DE from countries if dse = incomplete or partialnoDE
    if(grepl("incomplete|partialnoDE", rob)){
      countries_inc <- countries[!countries%in% c("DE")]
    }else{
      countries_inc <- countries
    }
    
    # Load country emission reductions
    rob_country_df <- list()
    for(c in countries_inc){
      
      countryname <- EU27[EU27$geo == c,]$name
      
      y <- y+1
      
      rob_country_df[[y]] <- as.data.frame(
        fread(path_carbonprice%&%rob%&%"/reduction_"%&%c%&%".csv",
              data.table = FALSE) %>%
          mutate(country     = c, 
                 countryname = countryname))
    }
    
    # Check if final bootstrap results exist for this configuration
    boot_exist <- ifelse(file.exists("../build/data/bootstrap/"%&%rob%&%
                                       "/GHG_price_level.csv"), 
                         "boot_yes", 
                         "boot_no")
    
    if(boot_exist == "boot_yes"){
    
      if(pol == "tax_GHG emissions"){
        # Load lower and upper bound of environmental tax from bootstrapping
        env_tax_lo <- ifelse(file.exists("../build/data/bootstrap/"%&%rob%&%
                                           "/GHG_price_level.csv"),
                             read.csv("../build/data/bootstrap/"%&%rob%&%
                                        "/GHG_price_level.csv") %>%
                               as_vector() %>%
                               min() %>% # change here whether sd, min/max, or 5/95 percentile is used for error bars
                               round(digits = 2),
                             NA)
        
        env_tax_hi <- ifelse(file.exists("../build/data/bootstrap/"%&%rob%&%
                                           "/GHG_price_level.csv"),
                             read.csv("../build/data/bootstrap/"%&%rob%&%
                                        "/GHG_price_level.csv") %>%
                               as_vector() %>%
                               max() %>% # change here whether sd, min/max, or 5/95 percentile is used for error bars
                               round(digits = 2),
                             NA)
      } else {
        env_tax_lo <- NA
        env_tax_hi <- NA
      }
    
    # Compute min and max value of footprint reduction error bars from 
    # bootstrapping results
    error <- 
      { if(pol == "VAT_increase")
          read.csv("../build/data/bootstrap/"%&%rob%&%
                     "/reduction_VAT_EU27.csv")
        else if(pol == "tax_GHG emissions")
          read.csv("../build/data/bootstrap/"%&%rob%&%
                     "/reduction_tax_EU27.csv")
      } %>%
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
      mutate(policy = pol) 
    
    
    # Aggregate over countries and categories
    x <-x+1
    df_rob_all[[x]] <- rob_country_df %>%
      bind_rows() %>% 
      # add env_tax column if it does not exist (VAT_increase)
      mutate(env_tax = ifelse("env_tax" %in% names(.),
                              env_tax,
                              NA),
             env_tax_lo = env_tax_lo,
             env_tax_hi = env_tax_hi) %>% 
      group_by(impact_name, env_tax, env_tax_lo, env_tax_hi, unit) %>%
      dplyr::summarise(footprint_reduction_abs = sum(footprint_reduction_abs)) %>% 
      ungroup() %>%
      filter(impact_name %in% selectedindicators) %>%
      mutate(configuration = rob) %>%
      { if(boot_exist == "boot_yes")
        left_join(., error, by = c("impact_name" = "impact")) 
      }
    }
  }
  
  # Turn list into df
  df_rob_all_combined <- df_rob_all %>%
    bind_rows() %>%
    # highlight main specification 
    mutate(fill = as.factor(ifelse(configuration == configpath,
                                   1, 
                                   ifelse(grepl("incomplete|partialnoDE", configuration),
                                          2, 
                                          0)))) %>% 
    # generate new configuration var that specifies difference to main configuration
    mutate(configuration_diff = ifelse(configuration == configpath,
                                                    "<b>Main*</b>",
                                                    df_rob_all %>%
                                                      bind_rows() %>%
                                                      pull(configuration) %>%
                                                      split(., 1:length(.)) %>%
                                                      lapply(., function(x){
                                                        vsetdiff(x %>% unlist() %>% strsplit(., split = "_") %>% unlist(),
                                                                 strsplit(configpath, split = "_") %>% unlist()) %>%
                                                          paste(., collapse = ", ")
                                                      }) %>%
                                                      unlist() %>%
                                                      as.vector()
                                                    )
    ) %>% 
    distinct()
  

  if(graph == "reduction"){
    
    # Bar plot for absolute footprint change by food category
    plot_return <- df_rob_all_combined %>% 
      # correct order of indicators
      left_join(unique(selectedindicators_df[,c("plotgroup", "n")]), 
                by = c("impact_name" = "plotgroup")) %>% 
      mutate(header = paste0(impact_name, "\n(", unit, ")")) %>%
      mutate(header = reorder(header, n,  decreasing = F)) %>% 
      # correct order of configurations
      mutate(configuration_diff = factor(configuration_diff, 
                                         levels = c("uncensored, nmin1",
                                                    "ytilda",
                                                    "2011",
                                                    "ycentered",
                                                    "unweighted",
                                                    "<b>Main*</b>",
                                                    "incomplete",
                                                    "pzintyes",
                                                    "uvnonadj",
                                                    "partialnoDE"))) %>%
      ggplot(aes(x  = configuration_diff, 
                 fill = fill)) +
      geom_bar(aes(y = footprint_reduction_abs),
               position = "stack",
               stat     = "identity") +
      {if(boot_exist == "boot_yes")
        geom_errorbar(aes(ymin = lowerbound,
                          ymax = higherbound),
                      width = 0.2)
      } +
      theme_c1m3() +
      facet_wrap(~header, scales = "free", nrow = 1) +
      xlab("") + ylab("") +
      scale_fill_manual(values = c("#457b9d", "#1d3557", "#dee2e6"))+
      theme(panel.grid.minor.y = element_line(linewidth = 0.1, color="black" ),
            legend.position    = "none",
            strip.background   = element_blank(),
            strip.text         = element_text(size = 15),
            plot.caption       = element_text(face = "italic"),
            axis.text.x        = element_markdown(angle = 90, hjust = 1, vjust = 0.5))+
      labs(caption = paste0("* Main: ", strsplit(configpath, split = "_") %>% unlist() %>% paste(., collapse = ", ")))
    
  }else if(graph == "carbonprice"){
    
    # Point plot for carbon price
    plot_return <- df_rob_all_combined %>%
      dplyr::select(env_tax, env_tax_lo, env_tax_hi, configuration_diff, fill) %>%
      distinct() %>% 
      ggplot(aes(x      = reorder(configuration_diff, env_tax),
                 y      = env_tax,
                 colour = fill)) +
      geom_point() +
      geom_errorbar(aes(ymin = env_tax_lo, ymax = env_tax_hi),
                    width = 0.2,
                    na.rm = TRUE) +
      theme_c1m3() +
      xlab("") + ylab("GHG emission price (EUR/tCO2eq)") +
      scale_colour_manual(values = c("#457b9d", "#1d3557", "#dee2e6"))+
      theme(panel.grid.minor.y = element_line(linewidth = 0.1, color = "black" ),
            legend.position    = "none",
            strip.background   = element_blank(),
            strip.text         = element_text(size = 15),
            plot.caption       = element_text(face = "italic"),
            axis.text.x        = element_markdown(angle = 90, hjust = 1, vjust = 0.5),
            axis.title         = element_text(size = 15)) +
      labs(caption = paste0("* Main: ", strsplit(configpath, split = "_") %>%
                              unlist() %>%
                              paste(., collapse = ", ")))
  }
  return(plot_return)
}



# Welfare - Plot functions ----------------------------------------------

plot_welfare_dens <- function(df,
                              pol,
                              x_lim,
                              y_lim,
                              mean_tax_inc,
                              mean_col,
                              line_color,
                              fill_color){
  
  #' @name plot_welfare_dens
  #' @title Plot distribution of welfare costs over households
  #' @param df Data frame containing all households to plot density over with 
  #' welfare costs
  #' @param pol Vector with name of simulated policies
  #' @param x_lim Upper limit of x-axis
  #' @param y_lim Upper limit of y-axis
  #' @param mean_tax_inc Weighted mean of tax income used to add vertical line
  #' @param mean_col Weighted mean of costs used to add vertical line
  #' @param line_color line color
  #' @param fill_color fill color
  #' @return plot_return

  df %>%
    as_tibble() %>%
    filter(policy == pol) %>%
    ggplot(aes(x      = lcol_abs,
               weight = hh_wgt)) +
    {
      if(pol == "VAT_increase")
        geom_density(aes(color = "VAT reform"),
                     fill  = fill_color)
        else
          if(pol == "tax_GHG emissions")
          geom_density(aes(color = "GHG emission price"),
                       fill  = fill_color)
    } +
    geom_vline(aes(xintercept = mean_col,
                   color      = "mean costs")) +
    geom_vline(aes(xintercept = mean_tax_inc,
                   color      = "mean additional tax revenue"),
               linetype = "dotted") +
    {
      if(pol == "VAT_increase")
        scale_color_manual(name = "Legend",
                           values = c("VAT reform"                  = line_color,
                                      "mean costs"                  = "black",
                                      "mean additional tax revenue" = "black"))
      else if(pol == "tax_GHG emissions")
        scale_color_manual(name = "Legend",
                           values = c("GHG emission price"          = line_color,
                                      "mean costs"                  = "black",
                                      "mean additional tax revenue" = "black"))
        } +
    scale_y_continuous(labels = label_percent(),
                       limits = c(0, y_lim),
                       breaks = c(0.002, 0.004, 0.006, 0.008)) +
    xlim(0, x_lim) +
    theme_c1m3() +
    theme(panel.grid.major.y = element_blank(),
          plot.margin        = margin(0, 0, 0, 0))
}


# Monetization - Plot functions -------------------------------------------

waterfall_plot <- function(type){
  
  #' @name waterfall_plot
  #' @title Create waterfall plot for monetized benefits
  #' @param type can be one of "total", "percapita" or "perhousehold"

  # Load EU number of households 
  EU_nhh <- EU_hh_year %>% summarize(nhh = sum(nhh)) %>% pull()
  
  # Load EU average household size
  EU_hhsizeav <- read_excel("00_data/lfst_hhantych_page_spreadsheet.xlsx",
                            sheet = "Sheet 1",
                            skip  = 10) %>%
    dplyr::transmute(name = ifelse(TIME == "Germany (until 1990 former territory of the FRG)",
                                   "Germany", TIME),
                     hh_size_av = as.numeric(!!sym(config$year_io))) %>% 
    right_join(EU27[,c("geo", "name")], by="name") %>%
    left_join(EU_hh_year[,c("geo", "weight")], by="geo") %>%
    dplyr::summarise(hh_size_av = weighted.mean(hh_size_av, weight)) %>% pull()
  
  # Load EU total population
  EU_npop <- EU_nhh*EU_hhsizeav 
  
  # Create plots
  plotlist <- list()
  for(p in c("VAT_increase", policy)){
    
    # Create dataframe to plot (depending on type)
    df <- final %>%
      filter(policy == p) %>%
      mutate(plotgroup = ifelse(scc != "diff_moore" | is.na(scc),
                                plotgroup,
                                "GHG emissions (diff_moore)")) %>%
      {
        if(type=="total"){
          group_by(., plotgroup) %>%
          summarize(., value = sum(value_BEUR))
        } else if(type=="percapita"){
          mutate(., value = value_BEUR*1e9/EU_npop) %>%
          group_by(., plotgroup) %>%
          summarize(., value = sum(value))
        } else if(type=="perhousehold"){
          mutate(., value = value_BEUR*1e9/EU_nhh) %>%
          group_by(., plotgroup) %>%
          summarize(., value = sum(value))
        } else{
          .
        }
      } %>%
      arrange(match(plotgroup, c("Consumer surplus", "Tax income",
                                 "Phosphorus", "Nitrogen", "GHG emissions", "GHG emissions (diff_moore)"))) %>%
      # bind_rows(data.frame(plotgroup = "Net change")) %>%
      mutate(row   = row_number(),
             start = lag(value),
             start = ifelse(is.na(start),
                            0,
                            start),
             start = cumsum(start),
             end   = start + value,
             end   = ifelse(is.na(end),
                            0,
                            end),
             start = ifelse(plotgroup == "GHG emissions (diff_moore)",
                            lag(start),
                            start))
    
    # Define y-axis label
    y_label <- if(type=="total"){
      "billion EUR"
    }else if(type=="percapita"){
      "EUR p.c."
    }else if(type=="perhousehold"){
      "EUR per household"
    }else{
      .   
    }
    
    # Define  y-axis limits
    y_limits <- if (type == "total"){
      c(-30, 15)
    }else if(type == "percapita"){
      c(-70, 30)
    }else if(type == "perhousehold"){
      c(-155, 70)
    } else {
      NULL
    }
    
    # Define colours
    colours <- col_intensity_indicators %>%
      dplyr::filter(indicator %in% final$plotgroup) %>%
      add_row(indicator = "Tax income", col_intensity = "#878c8f") %>%
      add_row(indicator = "Consumer surplus", col_intensity = "black") %>%
      arrange(match(indicator, df %>% pull(plotgroup) %>% unique())) %>%
      dplyr::select(col_intensity) %>% pull()
    
    # Plot
    temp_plot <- ggplot(df %>%
                          mutate(plotgroup_display = ifelse(plotgroup == "GHG emissions (diff_moore)",
                                                            "GHG emissions",
                                                            plotgroup),
                                 row = ifelse(plotgroup == "GHG emissions (diff_moore)" | plotgroup == "GHG emissions",
                                              5,
                                              row),
                                 # Add alpha mapping - upper segment (diff_moore) gets transparency
                                 alpha_level = ifelse(plotgroup == "GHG emissions (diff_moore)", 0.6, 1.0)),
                        aes(fill = plotgroup_display, alpha = alpha_level)) +
      geom_rect(aes(xmin = row - 0.4,
                    xmax = row + 0.4,
                    ymin = start,
                    ymax = end)) +
      geom_segment(aes(x    = ifelse(row == max(row), NA, row - 0.4),
                       xend = ifelse(row == max(row), NA, row + 1 + 0.4),
                       y    = end,
                       yend = end),
                   color = "black") +
      geom_segment(aes(x    = ifelse(row == max(row),
                                     NA,
                                     row + 1 + 0.4),
                       xend = ifelse(row == max(row),
                                      NA,
                                      row + 1 + 0.4 + 0.2),
                       y    = end,
                       yend = end),
                   color = "black") +
      geom_segment(aes(x = ifelse(plotgroup_display == "GHG emissions",
                                  row - 0.4,
                                  NA),
                       xend = ifelse(plotgroup_display == "GHG emissions",
                                     row+ 0.4 + 0.2 ,
                                     NA),
                       y = end,
                       yend = end),
                   color = "black")
    
    plotlist[[p]]  <- temp_plot +
      geom_text(x     = 3.1,
                y     = df %>% filter(plotgroup == "Consumer surplus") %>% pull(end) + 0.75,
                label = format(round(df %>% filter(plotgroup == "Consumer surplus") %>% pull(end), 2), nsmall = 2),
                size  = 4,
                color = "#495057",
                check_overlap = TRUE) +
      geom_text(x     = 4,
                y     = df %>% filter(plotgroup == "Tax income") %>% pull(end) + 0.75,
                label = format(round(df %>% filter(plotgroup == "Tax income") %>% pull(end), 2), nsmall = 2),
                size  = 4,
                color = "#495057",
                check_overlap = TRUE) +
      geom_text(x     = 4.9,
                y     = df %>% filter(plotgroup == "Phosphorus") %>% pull(end) + 0.75,
                label = format(round(df %>% filter(plotgroup == "Phosphorus") %>% pull(end), 2), nsmall = 2),
                size  = 4,
                color = "#495057",
                check_overlap = TRUE) +
      geom_text(x     = 5.95,
                y     = df %>% filter(plotgroup == "Nitrogen") %>% pull(end) + 0.75,
                label = format(round(df %>% filter(plotgroup == "Nitrogen") %>% pull(end), 2), nsmall = 2),
                size  = 4,
                color = "#495057",
                check_overlap = TRUE) +
      geom_text(x     = 6.05,
                y     = df %>% filter(plotgroup == "GHG emissions") %>% pull(end) + 0.75,
                label = paste(format(round(df %>% filter(plotgroup == "GHG emissions") %>% pull(end),
                                           2), nsmall = 2),
                              "**"),
                size  = 4,
                color = "#495057",
                check_overlap = TRUE) +
      geom_text(x     = 6.02,
                y     = df %>% filter(plotgroup == "GHG emissions (diff_moore)") %>% pull(end) + 0.75,
                label = paste(format(round(df %>% filter(plotgroup == "GHG emissions (diff_moore)") %>% pull(end),
                                           2), nsmall = 2),
                              "*"),
                size  = 4,
                color = "#495057",
                check_overlap = TRUE) +
      scale_x_continuous(breaks = df[-nrow(df), ] %>% 
                           filter(plotgroup != "GHG emissions (diff_moore)") %>% 
                           pull(row),
                         labels = df[-nrow(df), ] %>% 
                           filter(plotgroup != "GHG emissions (diff_moore)") %>% 
                           pull(plotgroup),
                         limits = c(0.6, nrow(df) + 0.3)) +
      scale_fill_manual(breaks = df[-nrow(df), ] %>%
                          filter(plotgroup != "GHG emissions (diff_moore)") %>%
                          pull(plotgroup),
                        values = colours) +
      scale_alpha_identity() +  # This ensures the alpha values are used as-is
      theme_c1m3() +
      labs(title = ifelse(p == "VAT_increase",
                          "VAT reform",
                          "GHG emission price")) +
      {if(p == policy)
        labs(caption = "*  net change based on Moore et al. (2024)  (283 USD/tCO2)\n
           ** net change based on EPA (2023) (193 USD/tCO2))            ")
        else
          labs(caption = " \n ")} +
      xlab("") +
      theme(legend.title = element_blank(),
            legend.text  = element_text(size = 10),
            axis.text.y  = element_text(size = 10),
            axis.text.x  = element_text(angle = 90, vjust = 1, hjust=1, size = 12),
            plot.title   = element_text(size = 14),
            plot.caption = element_text(size = 10))+
      guides(fill = guide_legend(nrow = 3))+
      ylab(y_label) +
      ylim(y_limits)
    
  }
  
  # Plot for all policies
  ggarrange(plotlist      = plotlist,
            common.legend = T, 
            legend        = "bottom")
}

# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------