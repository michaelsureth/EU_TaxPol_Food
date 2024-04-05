# File info --------------------------------------------------------------------

# File:    Generate LaTeX code for table on Environmental Footprints p.c.
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption


# _____________________________________------------------------------------
# Preparations ------------------------------------------------------------

# Create table directory if required
if(!dir.exists("../build/tables/")){
  dir.create("../build/tables/",
             recursive = TRUE)
}

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
# _ Load data -------------------------------------------------------------

# Determine results path depending on categorization
resultpath <- config$procdatapath%&%
  "aggr_footprints/"%&%config$year_io%&%"/by_food_nonfood/"

# Determine catnum (1 = food)
catnum <- 1

# Load footprints for all countries for selected category

# create empty list
df_all_c <- list()

# For all selected countries
for (c in countries){
  
  if(c=="EL"){ c <- "GR"}
  
  df_all_c[[c]] <- fread(resultpath%&%
                           "CBF_final_"%&%c%&%"_cat_"%&%catnum%&%".csv",
                         data.table = FALSE) %>% 
    
    # save demand country
    mutate(demandcountry = ifelse(c=="GR", "EL", c)) %>% 
    
    # filter for correct indicator
    filter(shortname %in% selectedindicators_plot) %>%
    
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
    left_join(EU_pop_year, by=c("demandcountry_name" = "name")) %>%
    
    # per capita footprint
    group_by(demandcountry, demandcountry_name, population, 
             shortname, Unit) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% ungroup() %>%
    mutate(value_pc = value/population) %>% dplyr::select(-value) %>%

    # adjust units
    mutate(value_pc = ifelse(shortname=="Biodiversity loss", round(value_pc*1e10,2), 
                      ifelse(shortname=="Water",round(value_pc*1e6),
                      ifelse(shortname=="Land use", round(value_pc*1e6),
                      ifelse(shortname=="GHG emissions", round(value_pc),
                             round(value_pc,2)))))) %>%
    mutate(Unit = ifelse(shortname=="Biodiversity loss", "PDFe-10", 
                  ifelse(shortname=="Water", "m3", 
                  ifelse(shortname=="Land use", "m2",
                         Unit))))
}

# bind list to dataframe
Table_fp <- do.call(rbind.data.frame, df_all_c) %>% 
  # table format 
  mutate(Title = paste0(shortname, "\n(", Unit, ")")) %>% 
  pivot_wider(id_cols = c("demandcountry_name"), 
              names_from = Title, 
              values_from = c("value_pc")) %>%
  # reorder columns in line with other plots
  arrange(demandcountry_name) %>%
dplyr::select(` ` = demandcountry_name,
                `Biodiversity loss\n(PDFe-10)`, `Land use\n(m2)`, `Nitrogen\n(kg)`,
                `Phosphorus\n(kg)`,`GHG emissions\n(kg)`, `Water\n(m3)`)


# _ Save table as .tex  ---------------------------------------------------

# Save as .tex
write(Table_fp %>%
        kable(format     = "latex",
              escape     = TRUE,
              booktabs   = TRUE,
              linesep    = "",
              caption    = "Environmental footprints of households’ food consumption per capita",
              label      = "si_fppc",
              row.names  = FALSE,
              align      = "c") %>%
        kable_styling(font_size = 8) %>%
        column_spec(2:7, width = "5em") %>%
        footnote(general = c("The table presents environmental footprints associated with households' food consumption per capita",
                             "and year by EU27 country. Computations are based on EXIOBASE v.3.8.2 (year 2019).")) %>%
        kable_paper(),
      file = "../build/tables/footprints_pc.tex") 

# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------