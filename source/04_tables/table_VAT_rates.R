# File info --------------------------------------------------------------------

# File:    Generate table with reduced and standard VAT rates for meat products
#          by country
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# _____________________________________-----------------------------------------
# Load data --------------------------------------------------------------------

# _ VAT rates ------------------------------------------------------------------

tab_VAT_rates <- read_excel("00_data/manual_input/VAT_rates.xlsx",
                            col_types = c("text", "numeric", "numeric", "text",
                                          "text", "text", "text","text", "text",
                                          "text", "text", "text", "text","text",
                                          "text"),
                            sheet     = "Sheet1") %>%
  left_join(EU27, by = c("country" = "geo")) %>%
  dplyr::select(name, VAT_meat_2023, VAT_standard_2023) %>% 
  mutate(VAT_meat_2023     = format(round(VAT_meat_2023,1),     nsmall = 1),
         VAT_standard_2023 = format(round(VAT_standard_2023,1), nsmall = 1)) %>%
  arrange(name)

# _ VAT Table to LaTeX ------------------------------------------------------

# create directory if necessary
if(!dir.exists("../build/tables/")){
  dir.create("../build/tables/",
             recursive = TRUE)
}

write(
  tab_VAT_rates %>%
    kable(format     = "latex",
          escape     = TRUE,
          booktabs   = TRUE,
          linesep    = "",
          caption    = "Reduced meat and standard VAT rates by country",
          label      = "VAT_rates",
          row.names  = FALSE,
          col.names  = c("", "Meat VAT (2023)", "Standard VAT (2023)"),
          align      = "c") %>%
    kable_styling(font_size = 8) %>%
    row_spec(0, bold = TRUE) %>%
    kable_paper(),
  file = "../build/tables/VAT_rates.tex")

# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------