# File info --------------------------------------------------------------------

# File:    Generate LaTeX code for tables containing of price and expenditure
#          elasticities per country
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# _____________________________________-----------------------------------------
# Generate LaTeX code ----------------------------------------------------------

# Set configpath according to config.do
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

# potentially remove existing tex-file with latex tables from previous runs
for(i in c("comp", "uncomp")){
  
  for(av in c("mean", "ref")){
  
  # Remove elasticity output table if existent
  if(robust.file.exists("../build/tables/"%&%i%&%"_elas_estimates_"%&%av%&%".tex")){
    file.remove("../build/tables/"%&%i%&%"_elas_estimates_"%&%av%&%".tex")
  }
  
  # Load names for categories
  catexplain <- fread("../build/data/intermediate_output/"%&%
                        "HBS_catexplain_"%&%config$categorization%&%".csv",
                      data.table = FALSE)
  
  # _ Load data ------------------------------------------------------------------
  
  files <- list.files(config$procdatapath%&%"easi_estimates/elasticities/"%&%
                        configpath%&%"/", pattern = "cpe_"%&%i)
    for(j in files){
      message(cat(i, av, j, sep = " "))
      
      # generate country abbreviation from loaded file
      c <- as.vector(str_match(j, "(?<=comp_).{2}"))
      
      # generate country name from loaded file
      countryname <- eu_countries %>%
        filter(code != 'UK') %>%
        select(geo = code, name) %>% 
        mutate(code_iso3 = countrycode(geo,
                                       origin      = "iso2c",
                                       destination = "iso3c")) %>%
        mutate(code_iso3 = ifelse(is.na(code_iso3) & geo == "EL",
                                  "GRC",
                                  code_iso3)) %>%
        filter(geo == c) %>%
        dplyr::select(name) %>% pull()
      
      # load price and expenditure elasticity data
      data <- load_elasticity_matrix(type      = paste0("cpe_", i),
                                     country   = c,
                                     averaging = av)
  
      ee_filename <- list.files(config$procdatapath%&%"easi_estimates/elasticities/"%&%
                                  configpath%&%"/", pattern = "ee_"%&%c)
        
      ee   <- read.csv(config$procdatapath%&%"easi_estimates/elasticities/"%&%
                         configpath%&%"/"%&%ee_filename) %>%
        filter(grepl(av, X)) %>% dplyr::select(V1) %>% pull()
      
      
      # _ Prepare data ------------------------------------------------------------------
      n_cat <- nrow(data)
      
      data <- data %>%
        # round all values to three digits
        mutate_all(., ~round(., 3)) %>% as.matrix() %>%
        # change number format to character keeping three digits even for trailing zeros
        sprintf("%.3f", .) %>%
        gsub("^(?!-)", "\\\\hphantom{-}", ., perl = TRUE) %>%
        # create matrix from vector of dimensions n_cat x n_cat
        matrix(byrow = TRUE, nrow = n_cat, ncol = n_cat) %>%
        # add rownames
        `rownames<-`(catexplain$category_name_new[1:n_cat])
      
      
      for (k in 1:ncol(data)) {
        data[k, k] <- cell_spec(data[k, k], "latex", bold = TRUE)
      }
      
      # add column with expenditure elasticity estimates to matrix
      data <- cbind(data, ee = ee %>%
                      # round all values to three digits
                      round(., 3) %>%
                      # change number format to character keeping three digits
                      # even for trailing zeros
                      sprintf("%.3f", .))
      
      
      # _ Generate tex-file ----------------------------------------------------
      
      # create directory if necessary
      if(!dir.exists("../build/tables/"%&%configpath%&%"")){
        dir.create("../build/tables/"%&%configpath%&%"",
                   recursive = TRUE)
      }
      
      # if first country in loop: generate tex-file and add first line setting line 
      # spacing to "single"
      if(!robust.file.exists("../build/tables/"%&%configpath%&%"/"%&%"elasticities/"%&%
                             i%&%"_elas_estimates_"%&%av%&%".tex")){
        write("\\renewcommand{\\baselinestretch}{1}",
              file = "../build/tables/"%&%configpath%&%"/"%&%
                i%&%"_elas_estimates_"%&%av%&%".tex")
      }
      
      # append latex table to tex-file
      write(
        data %>%
          kbl(format    = "latex",
              escape    = FALSE,
              booktabs  = TRUE,
              linesep   = "",
              caption   = countryname%&%": Compensated price and expenditure elasticities",
              label     = "App:comp_elas_"%&%c,
              row.names = TRUE,
              col.names = c("\\centering "%&%catexplain$category_name_new[1:n_cat],
                            "Expenditure elasticities"),
              align     = "c",
          ) %>%
          kable_styling(font_size = 8) %>%
          add_header_above(c(" ",
                             "Price elasticities" = n_cat,
                             " ")) %>%
          column_spec(1,           bold  = TRUE) %>%
          column_spec(2:(n_cat+1), width = "4.25em") %>%
          column_spec(n_cat + 2,   width = "6em") %>%
          # Footnote: the length of 175 characters roughly equals the width of the table
          footnote(general = c("Estimation based on following configuration: "%&%
                                 config$p_to_norm%&%", "%&%
                                 config$prices%&%", "%&%
                                 config$y%&%", "%&%
                                 config$cens_yesno%&%", "%&%
                                 config$weight_yesno%&%", "%&%
                                 config$equations%&%", "%&%
                                 config$ycentered_yesno%&%", "%&%
                                 config$shares%&%", "%&%
                                 config$dse,
                               "NEC = Not elsewhere classified. Own-price elasticities in bold. For price elasticities, each column gives the percentage change in quantity demanded due to a one percent price",
                               "increase of the good given in the respective row. Expenditure elasticities show the percentage change in quantity demanded for the good given",
                               "in the respective row due to a one percent increase in total food expenditures.")) %>%
          landscape() %>% kable_paper(),
        file   = "../build/tables/"%&%configpath%&%"/"%&%
          i%&%"_elas_estimates_"%&%av%&%".tex",
        append = TRUE)
      
      # append line to tex-file creating new page
      if(j != files[length(files)]){
        write("\\newpage",
              append = TRUE,
              file   = "../build/tables/"%&%configpath%&%"/"%&%
                i%&%"_elas_estimates_"%&%av%&%".tex")
      }
      
    } # close "files" loop
  } # close "averaging" loop
} # close "comp/uncomp" loop


# _____________________________________------------------------------------
# Generate xlsx file ------------------------------------------------------

# Load names for categories
catexplain <- fread("../build/data/intermediate_output/"%&%
                      "HBS_catexplain_"%&%config$categorization%&%".csv",
                    data.table = FALSE)

# Number of categories
n_cat <- nrow(catexplain)

for(i in c("comp", "uncomp")){
  
  for(av in c("mean", "ref")){
    
    # create workbook using openxlsx
    wb <- createWorkbook()
    
    # list all available country files
    files <- list.files(config$procdatapath%&%"easi_estimates/elasticities/"%&%
                          configpath%&%"/", pattern = "cpe_"%&%i)
    
    for(f in files){  
      
      # generate country abbreviation from loaded file
      c <- as.vector(str_match(f, "(?<=comp_).{2}"))
      
      # load price elasticity matrix
      data <- load_elasticity_matrix(type      = paste0("cpe_", i),
                                     country   = c,
                                     averaging = av)
      
      # load expenditure elasticities
      ee_filename <- list.files(config$procdatapath%&%"easi_estimates/elasticities/"%&%
                                  configpath%&%"/", pattern = "ee_"%&%c)
      
      ee   <- read.csv(config$procdatapath%&%"easi_estimates/elasticities/"%&%
                         configpath%&%"/"%&%ee_filename) %>%
        filter(grepl(av, X)) %>% dplyr::select(V1) %>% pull()
      
      # format price elasticity table
      data <- data %>%
        # round all values to three digits
        mutate_all(., ~round(., 3)) %>% as.matrix() %>%
        # change number format to character keeping three digits even for trailing zeros
        sprintf("%.3f", .) %>%
        # create matrix from vector of dimensions n_cat x n_cat
        matrix(byrow = TRUE, nrow = n_cat, ncol = n_cat) %>%
        # add rownames
        `rownames<-`(catexplain$category_name_new[1:n_cat]) 
      
      # add column with expenditure elasticity estimates to matrix
      data <- cbind(data, ee = ee %>%
                      # round all values to three digits
                      round(., 3) %>%
                      # change number format to character keeping three digits
                      # even for trailing zeros
                      sprintf("%.3f", .))
      
      # add column names as additional row
      data <- rbind(c(catexplain$category_name_new[1:n_cat]," "),
                    data)
      
      # add rownames (price/ expenditure elasticity)
      colnames(data) <- c(rep("Price elasticity", n_cat), "Expenditure elasticity")

      # add sheets for country c
      lapply(c("_elas", "_sd"), function(x){
        addWorksheet(wb, c%&%x)
        
        # merge cells for price elasticity header
        mergeCells(wb, c%&%x, cols = 1:n_cat+1, rows = 1)
        
        # center merged cell
        addStyle(wb, c%&%x, createStyle(halign = "center"), 
                 cols = 1:n_cat+1, rows = 1)
      })
      
      # write data frame to new sheet
      writeData(wb, c%&%"_elas", data, rowNames = TRUE)
      
      # add standard deviation to new sheet
      data_sd <-
        # load bootstrap samples
        fread(config$procdatapath%&%"bootstrap/"%&%configpath%&%"_"%&%config$year_io%&%
              "/temp_"%&%c%&%"/all_samples_cpe_"%&%i%&%".csv") %>%
        # compute standard deviation of CPEs
        summarize(across(everything(), sd)) %>%
        pivot_longer(cols = everything()) %>%
        filter(grepl(av, .$name)) %>%
        # round all values to three digits
        pull(value) %>%
        round(., 5) %>%
        # change number format to character keeping five digits even for trailing zeros
        sprintf("%.5f", .) %>%
        matrix(byrow = TRUE, nrow = nrow(catexplain), ncol = nrow(catexplain)) %>%
        # add rownames
        `rownames<-`(catexplain$category_name_new[1:n_cat]) %>%
        # add standard devation of EEs
        cbind(fread(config$procdatapath%&%"bootstrap/"%&%configpath%&%"_"%&%config$year_io%&%
                      "/temp_"%&%c%&%"/all_samples_ee.csv") %>%
                summarize(across(everything(), sd)) %>%
                pivot_longer(cols = everything()) %>%
                filter(grepl(av, .$name)) %>%
                pull(value) %>%
                round(., 5) %>%
                sprintf("%.5f", .))
      
      # add column names as additional row
      data_sd <- rbind(c(catexplain$category_name_new[1:n_cat]," "),
                    data_sd)
      
      # add rownames (price/ expenditure elasticity)
      colnames(data_sd) <- c(rep("Price elasticity (SD)", n_cat), "Expenditure elasticity (SD)")

      # add standard deviations to new sheet
      writeData(wb, c%&%"_sd", data_sd, rowNames = TRUE)
      
    } # close country file loop
    
    # save file
    saveWorkbook(wb, 
                 file = "../build/tables/"%&%configpath%&%"/"%&%
                   i%&%"_elas_estimates_"%&%av%&%".xlsx",
                 overwrite = TRUE)
  } # close "averaging" loop
} # close "comp/uncomp" loop

# _____________________________________------------------------------------
# END OF FILE -------------------------------------------------------------