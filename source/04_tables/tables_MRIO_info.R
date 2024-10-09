# File info --------------------------------------------------------------------

# File:    Generate tables related to MRIO data calculation
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# _____________________________________-----------------------------------------
# List of stressors and impacts ------------------------------------------------

# _ Load data ------------------------------------------------------------------
tab_s_i <- fread("../build/data/intermediate_output/"%&%
                   "MRIO_stressors_impacts_final.csv",
                 header = TRUE, data.table = FALSE) %>%
  # drop non-used indicators
  dplyr::filter(!(Unit=="Gg CO2-eq"|Unit=="kg CO2 eq.")) %>%
  filter(!is.na(S_row_stressor_impact)) %>% 
  # add higher-order group names (GHG emissions)
  left_join(selectedindicators_df, by = c("s_group" = "shortname")) %>%
  # keep only variables of interest
  dplyr::transmute(Stressor, 
                   Type = plotgroup, 
                   Subtype = ifelse(is.na(Landusetype), s_group, Landusetype),
                   Row = ifelse(is.na(Type), paste0(S_row_stressor_impact, " (imp)"),
                                paste0(S_row_stressor_impact, " (str)"))) %>%
  mutate_at(vars(Type,Subtype), ~replace(., is.na(.), "")) %>%
  arrange(Type, Subtype)

# _ Generate and save LaTeX code -----------------------------------------------

# create directory if necessary
if(!dir.exists("../build/tables/")){
  dir.create("../build/tables/",
             recursive = TRUE)
}

write("\\renewcommand{\\baselinestretch}{1}",
      file = "../build/tables/MRIO_stressors_impacts.tex")

write(tab_s_i %>%
        kable(format     = "latex",
              escape     = TRUE,
              booktabs   = TRUE,
              longtable = TRUE, 
              linesep    = "\\addlinespace",
              caption    = "Stressors/Impacts from EXIOBASE (v3.8.2)",
              label      = "MRIO_stressors_impacts",
              row.names  = FALSE,
              col.names  = NA,
              align      = "l") %>%
        #kable_styling(font_size = 8) %>%
        kable_styling(font_size = 8, latex_options = c("hold_position", "repeat_header")) %>%
        row_spec(0, bold = TRUE) %>%
        column_spec(1, width = "5cm") %>%
        column_spec(2, width = "2.1cm") %>%
        column_spec(3, width =  "2.3cm") %>%
        column_spec(4, width = "1.3cm") %>%
        footnote(general = c("The table shows EXIOBASE environmental extensions included in the analysis, as well as",
                             "their corresponding rows in the stressor (str) and impact (imp) matrices. Stressors are ",
                             "characterized according to their attributed subtype and aggregated for final output according",
                             "to their type.")) %>%
        kable_paper(),
      file   = "../build/tables/MRIO_stressors_impacts.tex",
      append = TRUE)

write("\\renewcommand{\\baselinestretch}{1.5}",
      file   = "../build/tables/MRIO_stressors_impacts.tex",
      append = TRUE)


# _____________________________________-----------------------------------------
# Correspondence EXIOBASE and COICOP -------------------------------------------

# _ Load data ------------------------------------------------------------------

# Load COICOP categorization
  COICOP_cat <- read.xlsx("00_data/manual_input/categorization_fooditems.xlsx",
                       sheet = "HBS_"%&%config$categorization,
                       colNames = TRUE) %>%
    dplyr::select(item_code_orig, category_name_new, category_code_new) %>%
    mutate(item_code_orig = gsub("EUR_HE", "", item_code_orig)) %>% 
    filter(!is.na(category_code_new)) %>% 
    distinct() %>%
  # arrange for table
  group_by(category_name_new, category_code_new) %>%
  summarise(COICOP=paste(item_code_orig,collapse=", ")) %>% ungroup() 

# Load EXIOBASE categorization
  EXIOBASE_cat <- read.xlsx("00_data/manual_input/exiobase_product_grouping.xlsx",
                                   sheet    = config$categorization,
                                   rows     = c(2:202)) %>% 
    pivot_longer(cols = -X1, names_to = "category_code_new", values_to = "yes_no") %>%
    filter(yes_no == 1) %>%
    # arrange for table
    group_by(category_code_new) %>%
    summarise(EXIOBASE=paste(X1,collapse=", ")) %>% ungroup() %>%
    mutate(category_code_new = as.numeric(category_code_new))
  
# Combine EXIOBASE and COICOP concordance
  C_E_concordance <- COICOP_cat %>%
    full_join(EXIOBASE_cat, by = "category_code_new")%>%
    arrange(category_code_new)
  
# _ Generate and save LaTeX code -----------------------------------------------
  
  write("\\renewcommand{\\baselinestretch}{1}",
        file = "../build/tables/coicop_exiobase_correspondence.tex")
  
  write(C_E_concordance %>%
          dplyr::select(Category = category_name_new, COICOP, EXIOBASE) %>% 
          kable(format     = "latex",
                escape     = TRUE,
                booktabs   = TRUE,
                linesep    = "\\addlinespace",
                caption    = "Correspondence table of food categories to COICOP and EXIOBASE products",
                label      = "COICOP_EXIOBASE_conc",
                row.names  = FALSE,
                col.names  = NA,
                align      = "l") %>%
          kable_styling(font = 8) %>%
          column_spec(1, width = "2.75cm") %>%
          column_spec(2, width = "4.5cm") %>%
          column_spec(3, width = "4.75cm") %>%
          footnote(general = c("The table shows the mapping of COICOP (structure level 4/ subclasses (five-digit)) as well as",
                               "EXIOBASE agricultural and food processing sectors included in the analysis (24 out of 200) and", 
                               "their assignment to the corresponding food category. NEC denotes not elsewhere classified food",
                               "items.")) %>%
          kable_paper(),
        file = "../build/tables/coicop_exiobase_correspondence.tex",
        append = TRUE)
  
  write("\\renewcommand{\\baselinestretch}{1.5}",
        file   = "../build/tables/coicop_exiobase_correspondence.tex",
        append = TRUE)

# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------