# File info --------------------------------------------------------------------

# File:    Plot results of welfare analysis
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

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

# Create figure directory if required
if(!dir.exists("../build/figures/"%&%configpath)){
  dir.create("../build/figures/"%&%configpath,
             recursive = TRUE)
}

# Choose policy colours and transparent colours (30/100)
custom_palette       <- policy_colours
custom_palette_trans <- paste0(custom_palette, "30")

# Choose policy to compare
policy <- "tax_GHG emissions"

# _____________________________________------------------------------------
# Figure 5: Distribution of welfare losses vs tax income ------------------

# _ Load data -------------------------------------------------------------

# __ Welfare impacts by household -----------------------------------------

df <- list()
x <- 0

for(pol in c("VAT_increase", policy)){
  for(c in countries){
    
    x <- x + 1
    
    # load lcol results 
    df[[x]] <- fread("../build/data/welfare/"%&%configpath%&%"/"%&%pol%&%"/"%&%
                       c%&%"_hhs.csv", 
                     data.table = FALSE) %>%
      mutate(country = c,
             policy  = pol)
  }
}; rm(c, pol)


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

# _ Plotting --------------------------------------------------------------

# __ EU total -------------------------------------------------------------

# Mean additional tax income by policy
mean_tax_inc <- tax_inc %>%
  mutate(tax_inc = VAT_diff + GHG_inc) %>%
  # by country
  group_by(policy, country) %>%
  summarize(mean_tax_inc = weighted.mean(tax_inc, hh_wgt)) %>% ungroup() %>%
  # EU-wide weighted (by number of households per country) mean value
  left_join(EU_hh_year %>% dplyr::select(geo, weight),
            by = c("country" = "geo")) %>% 
  group_by(policy) %>%
  summarize(mean_tax_inc = sum(mean_tax_inc*weight)) 

# Mean additional costs by policy
mean_col <- lapply(c("VAT_increase", policy), function(pol){
  
  # Read country means
  fread("../build/data/welfare/"%&%configpath%&%
          "/"%&%pol%&%"/"%&%"country-means_lcol.csv") %>%
    # EU-wide weighted (by number of households per country) mean value
    left_join(EU_hh_year %>% dplyr::select(geo, weight),
              by = c("country" = "geo")) %>%
    summarize(policy   = pol,
              mean_col = sum(mean_lcol_abs*weight))
}
) %>% bind_rows()

# Plot densities for both policies
EU_plots <- lapply(c("VAT_increase", policy), function(pol){
  
  # define line and fill color depending on policy
  line_color <- ifelse(pol == "VAT_increase",
                       policy_colours[1],
                       policy_colours[2])
  fill_color <-  line_color%&%"30"

  # plot
  plot_welfare_dens(df = df %>%
                      bind_rows(),
                    pol,
                    x_lim = 500,
                    y_lim = 0.008,
                    mean_tax_inc = mean_tax_inc[mean_tax_inc$policy == pol,]$mean_tax_inc,
                    mean_col = mean_col[mean_col$policy == pol,]$mean_col,
                    line_color,
                    fill_color
                    ) +
    xlab("Cost-of-living (EUR)")+ylab("")+
    theme(legend.position = "none",
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12)) +
    {if(pol == "VAT_increase")
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank())
    } 
})


# __ Generate legends -----------------------------------------------------

# Generate legend for densities
legend_dens <- lapply(c("VAT_increase", policy), function(pol){
    
    # define line and fill color depending on policy
    line_color <- ifelse(pol == "VAT_increase",
                         policy_colours[1],
                         policy_colours[2])
    fill_color <-  line_color%&%"30"
    
    # retrieve legend from plot
    return(
      get_plot_component(plot_welfare_dens(df = df %>%
                                             bind_rows(),
                                           pol, 
                                           x_lim = 500,
                                           y_lim = 0.007,
                                           mean_tax_inc = mean_tax_inc[mean_tax_inc$policy == pol,]$mean_tax_inc,
                                           mean_col = mean_col[mean_col$policy == pol,]$mean_col,
                                           line_color,
                                           fill_color) +
                           {
                             if(pol == "VAT_increase")
                               scale_color_manual(name = "Legend",
                                                  values = c("VAT reform" = line_color))
                             else if(pol == "tax_GHG emissions")
                               scale_color_manual(name = "Legend",
                                                  values = c("GHG emission price" = line_color))
                           } +
                           theme(legend.text = element_text(size = 11))
                         ,
                         'guide-box',
                         return_all = TRUE)[[3]]
    )
  })


# Generate legend for vertical lines indicating means
legend_lines <- lapply(c("mean costs", "mean additional tax revenue"),
                    function(lab){
                      get_plot_component(plot_welfare_dens(df = df%>%
                                                             bind_rows(),
                                                           pol = "VAT_increase",
                                                           x_lim = 500,
                                                           y_lim = 0.007,
                                                           mean_tax_inc = mean_tax_inc[mean_tax_inc$policy == "VAT_increase",]$mean_tax_inc,
                                                           mean_col = mean_col[mean_col$policy == "VAT_increase",]$mean_col,
                                                           line_color = policy_colours[[1]],
                                                           fill_color = policy_colours[[1]]%&%"30") +
                                           scale_color_manual(name = "Legend",
                                                              values = c("black") |> `names<-`(c(lab))) +
                                           theme(legend.text = element_text(size = 11)),
                                         'guide-box',
                                         return_all = TRUE)[[3]]
                    })



# Combine density plots with legends
ggarrange(
  ggarrange(plotlist = EU_plots,
            ncol = 1,
            heights = c(1, 1.1)),
  NULL,
  ggarrange(legend_dens[[1]],
            legend_dens[[2]],
            legend_lines[[2]],
            legend_lines[[1]], nrow = 1),
  ncol = 1, heights = c(1.5, 0.05, 0.2)
)

# _ Save figure -----------------------------------------------------------
ggsave("../build/figures/"%&%configpath%&%"/figure_lcol_EU.pdf",
       width = 8, height = 5)


# _ text ------------------------------------------------------------------

# Mean change in COL, tax inc and net costs
mean_col %>% left_join(mean_tax_inc, by="policy") %>%
  mutate(mean_col-mean_tax_inc)

# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------