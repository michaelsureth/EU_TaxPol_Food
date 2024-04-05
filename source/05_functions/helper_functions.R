# File info --------------------------------------------------------------------

# File:    Helper functions
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
#          Impacts from European Food Consumption

# ------------------------------------------------------------------------------

install_packages <- function(pkg){
  
  #'@name install_packages
  #'@title Package Manager
  #'@description Install required packages and load them
  #'@param pkg List of packages to be loaded 
  #'@return NONE
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}



"%&%" <- function(x, y){
  
  #' @name %&%
  #' @title Easy string pasting
  #' @description Takes arguments before and after symbol %&% and pastes them as
  #' string without space
  #' @param x string or symbol of string
  #' @param y string or symbol of string
  #' @return string
  
  paste0(x, y)
}



eval_obj <- function(x){
  
  #' @name eval_obj
  #' @title Evaluate object referred to as string
  #' @description Helper function to shorten code looping over naming vector
  #' @param i looping indicator
  #' @param text (optional) additional text that can be added after indicator i
  #' @return evaluated object of name paste0(i, text)

eval(as.symbol(x))
}



robust.file.exists <- function(x) {
  #' @name robust.file.exists
  #' @title Check file
  #' @description Same es file.exists but robust against different file path
  #' specifications between Windows and macOS/Linux machines
  #' @param x file path
  #' @return logical vector
  
  if (.Platform$OS == "windows" && grepl("[/\\]$", x)) { 
    file.exists(dirname(x)) 
  } else file.exists(x) 
}



unit.conv <- function(from, to) {
  #' @name unit.conv
  #' @title Unit conversion
  #' @description Converts units specified in unit_conversion.xlsx
  #' @param x unit to be converted
  #' @param y unit into which should be converted 
  #' @return conversion factor

  unit_conversion <- read.xlsx("00_data/manual_input/unit_conversion.xlsx")
  
  factor <- unit_conversion %>%
    filter(from == !!from & to == !!to) %>%
    select(conversion) %>% pull()
  
  if(from == to){
    return(1)
  } else {
    return(factor)
  }
}

  
  
load_elasticity_matrix <- function(type, country, averaging){
  
  #' @name load_elasticity_matrix
  #' @title Load elasticity matrix
  #' @description Load elasticity matrix according to config parameters
  #' @param type can be one of "cpe_uncomp" or "cpe_comp"
  #' @param country
  
  # determine data path according to config specification
  loadpath <- config$procdatapath%&%"easi_estimates/elasticities/"%&%
    configpath%&%"/"
  
  # determine file name according to country 
  file_name <- list.files(loadpath, pattern = paste0(type, "_", country))
  
  # load elasticities
  df_elas <- fread(loadpath%&%file_name, data.table = FALSE) 
  
  # arrange elasticities as matrix
  mat_elas <- df_elas %>% 
    setNames(c("V1", "V2")) %>% 
    # filter for correct averaging
    filter(grepl(averaging, V1)) %>% 
    # ensure correct ordering
    mutate(i = as.numeric(sub("\\j.*", "", sub(".*i", "", V1))),
           j = as.numeric(sub("\\_"%&%averaging%&%".*", "", sub(".*j", "", V1)))) %>%
    arrange(i,j) %>% dplyr::select(V2) %>% pull() %>%
    matrix(byrow = TRUE, nrow = nrow(catexplain), ncol = nrow(catexplain)) %>% 
    as.data.frame()
  
  # return matrix
  return(mat_elas)
  
}


adjust_config <- function(parameter, value){
  
  #' @name adjust_config
  #' @title Adjust config.do
  #' @description This function adjusts a selected parameter in config.do 
  #' @param parameter
  #' @param value 
  #' @return config_adj
  
  oldvalue <- config[[parameter]]
  
  config[[parameter]] <- value
  
  config_adj <- config %>% as.data.frame() %>% t() %>% as.data.frame() %>%
    rownames_to_column() %>%
    mutate(V0 = "global") %>%
    rename(V2 = "V1", V1 = "rowname") %>% dplyr::select(order(colnames(.)))
  
  write.table(config_adj, "config.do", 
              row.names = FALSE, col.names = FALSE, quote = c(3))
  
  config_new <- read.table("config.do") %>%
    dplyr::select(V2, V3) %>%
    column_to_rownames(var = "V2") %>%
    t() %>% as.data.frame() %>% as.list()
  
  message(paste0("File config.do was adjusted: config$", parameter,
               " was changed from ", oldvalue, " to ", value))
  
  return(config_new)
}