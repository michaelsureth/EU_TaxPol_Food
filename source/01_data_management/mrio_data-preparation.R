# File info --------------------------------------------------------------------

# File:    Preparation of multi-regional input-output data for environmental 
#          footprint computation
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# _____________________________________-----------------------------------------
# Preparations -----------------------------------------------------------------

# _ Selection of analyzed stressors and impacts --------------------------------
stressors_selected <- read.xlsx("00_data/manual_input/exiobase_stressors_impacts.xlsx",
                                sheet = "stressors") %>%
  dplyr::filter(include == 1) %>%
  dplyr::select(stressor) %>% pull()

impacts_selected <- read.xlsx("00_data/manual_input/exiobase_stressors_impacts.xlsx",
                              sheet = "impacts") %>%
  dplyr::filter(include == 1) %>%
  dplyr::select(impact) %>% pull()


# _____________________________________-----------------------------------------
# Prepare data -----------------------------------------------------------------

# _ Load Y (final demand) ------------------------------------------------------

# Dimensions of final demand matrix: 9800 x 343
# (200 products*49 countries = 9800)
# (49 countries*7 final demand categories = 343)
Y <- fread("00_data/MRIO/IOT_"%&%config$year_io%&%"_"%&%config$IorP%&%
             "/Y.txt",
           header = FALSE, data.table = FALSE)

# note: no demand for pork and cattle (only for their products)
# Y %>% dplyr::filter(grepl("region|category", V1) | grepl("attle", V2)) %>% View()
# Y %>% dplyr::filter(grepl("region|category", V1) | grepl("igs", V2)) %>% View()
# Y %>% dplyr::filter(grepl("region|category", V1) | grepl("oultry", V2)) %>% View()

# production sector labels 
sector_label_df <- Y[4:nrow(Y), 1:2] 
sector_labels   <- Y[4:nrow(Y), 2  ] %>% unique()

# final demand category labels
FDCats_label_df <- Y[1:2, 3:ncol(Y)]
FDCats_labels   <- Y[2  , 3:9] %>% t() %>% as.vector()

# region/country list
RegionList      <- Y[1, 3:ncol(Y)] %>%
  t() %>% as.data.frame() %>% distinct() %>% pull()

# data only (no labels), and convert to numeric
Y           <- Y[4:nrow(Y), 3:ncol(Y)]
Y[is.na(Y)] <- 0
Y           <- mapply(Y, FUN = as.numeric)


# _ Load or compute L (Leontief inverse) ---------------------------------------

if(robust.file.exists(config$procdatapath%&%"/MRIO_proc/"%&%config$year_io%&%
                       "/L.csv")) {
  L <- as.matrix(fread(config$procdatapath%&%"/MRIO_proc/"%&%config$year_io%&%"/L.csv",
                       header = FALSE, data.table = TRUE))
} else {
  # A - inter-industry coefficient matrix (inputs required per unit of output)
  #     also called direct requirements matrix / technical coefficients matrix
  A <- fread("00_data/MRIO/IOT_"%&%config$year_io%&%"_"%&%
               config$IorP%&%"/A.txt",
             header = FALSE, data.table = FALSE)
  
  # data only (no labels), and convert to numeric
  A <- A[4:nrow(A), 3:ncol(A)]
  A <- mapply(A, FUN = as.numeric)
  A <- matrix(data = A, ncol = nrow(Y), nrow = nrow(Y))
  
  # solve the Leontief inverse (solving I - A)
  # each column of the Leontief inverse shows what is required per sector for
  # meeting the final demand in the respective sector in a country
  L <- solve(diag(dim(A)[1])-A)
  L[is.na(L)] <- 0
  
  # save
  if(!dir.exists(config$procdatapath%&%"/MRIO_proc/"%&%config$year_io)){
    dir.create(config$procdatapath%&%"/MRIO_proc/"%&%config$year_io,
               recursive = TRUE)
  }
  
  fwrite(L, config$procdatapath%&%"/MRIO_proc/"%&%config$year_io%&%"/L.csv",
         col.names = FALSE)
}



# _ Load satellites (uncharacterized stressors) --------------------------------
# e.g. CO2 emissions, land use per category, etc.

# Note: We do not include satellite/F_Y (direct emission matrix for stressors).
# These are pressures that occur during the final use of the product; for food 
# products, this is not relevant.

# __ Stressor labels and units -------------------------------------------------

Stressors_list   <- fread("00_data/MRIO/IOT_"%&%config$year_io%&%
                            "_"%&%config$IorP%&%"/satellite/unit.txt",
                          header = FALSE, data.table = FALSE)

Stressors_labels <- Stressors_list[2:nrow(Stressors_list), 1]
Stressors_units  <- Stressors_list[2:nrow(Stressors_list), 2]


# __ S_stress (direct stressor coefficients) -----------------------------------
# unit: the unit of the satellite account per unit of the economic core
# (e.g. kg CO2eq/Million Euro)

S_stress <- fread("00_data/MRIO//IOT_"%&%config$year_io%&%
                    "_"%&%config$IorP%&%"/satellite/S.txt",
                  header = FALSE, data.table = FALSE)

# data only (no labels), and convert to numeric
S_stress                  <- S_stress[4:nrow(S_stress), 2:ncol(S_stress)] 
S_stress[is.na(S_stress)] <- 0
S_stress                  <- mapply(S_stress, FUN = as.numeric)



# _ Load impacts (characterized stressors) -------------------------------------
# e.g. total GWP100, Phosphorus, etc.

# Note: We do not include impacts/F_Y (direct emission matrix for impacts).
# These are pressures that occur during the final use of the product, need to be
# added to footprints calculated here if relevant for food products, we do not
# consider this relevant.

# __ Impact labels and units ---------------------------------------------------

Impacts_list   <- fread("00_data/MRIO/IOT_"%&%config$year_io%&%
                          "_"%&%config$IorP%&%"/impacts/unit.txt",
                        header = FALSE, data.table = FALSE)

Impacts_labels <- Impacts_list[2:nrow(Impacts_list), 1]
Impacts_units  <- Impacts_list[2:nrow(Impacts_list), 2]


# __ S_impact (direct impact coefficients) -------------------------------------

S_impact <- fread("00_data/MRIO/IOT_"%&%config$year_io%&%
                    "_"%&%config$IorP%&%"/impacts/S.txt",
                  header = FALSE, data.table = FALSE)

# data only (no labels), and convert to numeric
S_impact                  <- S_impact[4:nrow(S_impact), 2:ncol(S_impact)] 
S_impact[is.na(S_impact)] <- 0
S_impact                  <- mapply(S_impact, FUN = as.numeric)



# _____________________________________-----------------------------------------
# MRIO computation for selected stressors/impacts ------------------------------

# we need
# - S_stress
# - S_impact
# - L
# - Y

# _ Select stressors (land use) ------------------------------------------------

# select stressors (land use) 
stress_vec <- Stressors_labels %>% as.data.frame() %>%
  rownames_to_column() %>%
  setNames(c('rowname', 'stressor')) %>%
  dplyr::filter(stressor %in% stressors_selected) %>%
  transmute(rowname = as.integer(rowname)) %>% pull()

Stressors_labels[stress_vec] 
Stressors_units[stress_vec] 

# select stressor coefficients:
# reduce S_stress to selected only -> shows stressors by country and product
S_stress_final <- matrix(0, length(stress_vec), ncol(S_stress))
x1 <- 1
for (i1 in c(1:nrow(S_stress))){
  if(i1 %in% stress_vec){
    S_stress_final[x1, ] <- S_stress[i1, ] 
    x1 <- x1 + 1
  }
}; rm(x1)


# _ Select impacts (GHG) -------------------------------------------------------

# select impacts (GHG emissions) 
impact_vec <- Impacts_labels %>% as.data.frame() %>%
  rownames_to_column() %>%
  setNames(c('rowname', 'impact')) %>%
  dplyr::filter(impact %in% impacts_selected) %>%
  transmute(rowname = as.integer(rowname)) %>% pull()

Impacts_labels[impact_vec] 
Impacts_units[impact_vec]

# select impact coefficients (GHG emissions):
# reduce S_impact to selected only -> shows impact by country and product
S_impact_final <- matrix(0, length(impact_vec), ncol(S_impact))
x2 <- 1
for (i2 in c(1:nrow(S_impact))){
  if(i2 %in% impact_vec){
    S_impact_final[x2, ] <- S_impact[i2, ] 
    x2 <- x2 + 1
  }
}; rm(x2)


# _ Save stressors and impacts -------------------------------------------------

# Load stressor information (group, land use type, target unit)
  Stressor_info <- read.xlsx("00_data/manual_input/exiobase_stressors_impacts.xlsx",
                                  sheet = "stressors") %>%
    dplyr::filter(include == 1) %>%
    dplyr::select(stressor, unit, s_group, 
                  Landusetype,  Landusetype_coeff, 
                  target.unit, characterization_factor)

# Load impact information (shortname, target unit)
  Impact_info <- read.xlsx("00_data/manual_input/exiobase_stressors_impacts.xlsx",
                                sheet = "impacts") %>%
    dplyr::filter(include == 1) %>%
    dplyr::select(impact, unit, shortname, target.unit)


# Load stressor groups

# Create data.frame of stressors 
selected_s_i <- data.frame(Stressor              = Stressors_labels[stress_vec],
                           Unit                  = Stressors_units[stress_vec],
                           S_row_stressor_impact = stress_vec) %>%
  # assign 6 land use types to 20 land use stressors
  left_join(Stressor_info, by = c("Stressor"="stressor","Unit"="unit")) %>%
  
  # add impacts
  add_row(Stressor              = Impacts_labels[impact_vec],
          Unit                  = Impacts_units[impact_vec],
          S_row_stressor_impact = impact_vec) %>%
  
  # add shortnames and target units
  left_join(Impact_info, by = c("Stressor"="impact","Unit"="unit")) %>%
  mutate(target.unit = ifelse(is.na(target.unit.x), target.unit.y, target.unit.x)) %>%
  dplyr::select(-target.unit.x, -target.unit.y) %>%
  # generate id
  mutate(code_s_i = sequence(n())) 
  
  write.csv(selected_s_i, "../build/data/intermediate_output/"%&%
            "MRIO_selected_stressors_impacts.csv",
          row.names = FALSE)


# _ Compute footprints (effect of household consumption by country) ------------

# Repeat footprint computation for different product groupings defined in
# exiobase_product_grouping.xlsx
for(grouping in c("by_foodcat", "by_food_nonfood")){
  
  # load grouping
  if(grouping == "by_foodcat"){
    grouping_matrix <- read.xlsx("00_data/manual_input/exiobase_product_grouping.xlsx",
                                 sheet = config$categorization,
                                 cols  = 2:11,
                                 rows  = 2:202)
    }
  if(grouping == "by_food_nonfood"){
    grouping_matrix <- read.xlsx("00_data/manual_input/exiobase_product_grouping.xlsx",
                                 sheet = "by_food_nonfood",
                                 cols  = 2:3,
                                 rows  = 2:202)
  }
  
  # repeat matrix so that for each country-product combination we have info 1/0
  grouping_matrix_full <- do.call(rbind,
                                  replicate(length(RegionList),
                                            grouping_matrix,
                                            simplify = FALSE)) 
  
  future_lapply(countries, function(c){
    
    if(c == "EL"){ c <- "GR"}
    
    # determine correct demand column in Y (final consumption expenditure by hh)
    colno <- FDCats_label_df %>% t() %>% as.data.frame() %>%
      mutate(count = sequence(n())) %>%
      setNames(c("geo", "FDCat","rowno")) %>% 
      dplyr::filter(geo   == c,
                    FDCat == "Final consumption expenditure by households") %>%
      dplyr::select(rowno) %>% pull()
    # double-check
    message(cat(FDCats_label_df[ , colno], sep = " "))
    
    # create vector (1x9800) corresponding to household demand in selected country
    Y_hh_c <- Y[ , colno]
    
    # Grouping EXIOBASE 3.8.2 pxp 200 products into 10 product groups
    # (as specified in grouping_matrix_full)
    for (g in c(1:ncol(grouping_matrix))){
      
      message("Category ", g)
      
      # select relevant products for group g
      # (e.g., for g=1 (first category is Bread and cereals), household FD of 
      # country c for "Paddy rice", "Wheat", "Cereal grains nec", "Processed rice" 
      # of all other countries is relevant)
      Y_hh_c_g <- Y_hh_c * grouping_matrix_full[ , g]
      
      # run MRIO function to compute hh consumption based footprints (CBF)
      CBF <- compute_footprints(Y_c      = Y_hh_c_g,
                                L        = L,
                                S_stress = S_stress_final,
                                S_impact = S_impact_final)
      
      
      # _ Save footprints and final demand vecots ------------------------------
      
      # save regionally and product specific footprints for final hh demand of
      # product group g in country c
      ifelse(!dir.exists(file.path(config$procdatapath%&%"MRIO_proc/"%&%
                                     config$year_io%&%"/"%&%grouping)), 
             dir.create(file.path(config$procdatapath%&%"MRIO_proc/"%&%
                                    config$year_io%&%"/"%&%grouping),
                        recursive = TRUE), 
             FALSE)
      
      write.csv(CBF,
                file = config$procdatapath%&%"MRIO_proc/"%&%config$year_io%&%
                "/"%&%grouping%&%"/CBF_"%&%config$IorP%&%"_"%&%c%&%"_cat_"%&%
                  g%&%".csv",
                row.names = FALSE)
      
      # save final demand for food group g in country c (in MEUR, to be used for
      # intensities)
      Y_sel_cat     <- data.frame(Y_hh_c_g)
      Y_sel_cat_agg <- data.frame(matrix(0,
                                         nrow = length(RegionList),
                                         ncol = 1))
      
      for (i in 1:length(RegionList)) {
        rows <- seq(from = (i-1)*length(sector_labels) + 1,
                    to   =  i   *length(sector_labels))
        Y_sel_cat_agg[i , ] <- sum(Y_sel_cat[rows, ])
      }
      
      Y_sel_cat_agg$impreg <- RegionList
      
      write.table(Y_sel_cat_agg,
                  file = config$procdatapath%&%"MRIO_proc/"%&%config$year_io%&%
                  "/"%&%grouping%&%"/Y_"%&%config$IorP%&%"_"%&%c%&%"_cat"%&%
                    g%&%".csv",
                  row.names = FALSE,
                  col.names = c("FD_hh", "impreg"),
                  sep       = ",")
      
      # clean
      rm(CBF, Y_sel_cat_agg)
    }
  })
}

# _____________________________________-----------------------------------------
# END OF FILE ------------------------------------------------------------------