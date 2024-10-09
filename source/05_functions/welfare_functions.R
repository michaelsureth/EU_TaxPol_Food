# File info --------------------------------------------------------------------

# File:    Welfare functions
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform

# Compute welfare ---------------------------------------------------------

welfare_comp_u <-  function(coef, 
                            log.price,
                            var.soc, 
                            log.exp,
                            shares){
  
  #' @name welfare_comp_u
  #' @title Compute u and its components for a given vector of prices and expenditures
  #' @description This function builds on easi::intermediate.blocs. (archived package)
  #' @param coef Named vector of estimated coefficients
  #' @param log.price dataframe with household log prices p1-pN as columns
  #' @param var.soc dataframe with household characteristics z1-zL as columns
  #' @param log.exp vector of log total real household food expenditures
  #' @param shares dataframe with household exp shares w1-wN as columns
  #' @return df_return List with elements tot (EEEazpipj), tot2 (EEbpipj), 
  #' tot0 (Ewipi) and y (u).
  
  # Number of households
  n <- length(log.exp)
  # Number of equations --> we use ALL (different to Reasi code)
  neq <- ncol(log.price)#-1
  # Labels or names of the equations:
  noms <- c()
  for (i in 1:neq) noms <- c(noms, paste0("eq", i))
  # Number of sociodemographic characteristics
  ndem <- ncol(var.soc)
  # vector of demographic characteristics (Italy does not have income-z)
  if(c == "IT"){
    ndem_vec <- c(1:ndem)[-4]
  }else{
    ndem_vec <- c(1:ndem)
  }
  
  # w (shares) into matrix
  w = matrix(0, n, neq)
  for (i in 1:(neq)) w[, i] <- shares[, i]
  
  
  # (1) construction of 'sum_i sum_j sum_l a_ijl z_l p_i p_j' (tot)
  
  # gamma parameters (p coefficients) - a
  tot_gamma = 0
  for (i in 1:neq){
    for (j in 1:neq){
      tot_gamma_comp <- coef[[paste0("a_restr_i",i,"j",j)]]*log.price[,i]*log.price[,j]
      tot_gamma <- tot_gamma + tot_gamma_comp
    }
  }
  
  # theta parameters (p-z interaction coefficients) - e                                    
  tot_theta = 0
  if(config$pzint == "pzintyes"){
    for (i in 1:neq){
      for (j in 1:neq){
        for (l in ndem_vec){
          tot_theta_comp <- coef[[paste0("e_restr_i",i,"j",j,"_z",l)]]*log.price[,i]*log.price[,j]*var.soc[,l]
          tot_theta <- tot_theta + tot_theta_comp 
        }
      }
    }
  }
  
  # add gamma and theta for tot price effects
  EEEazpipj <- tot_gamma + tot_theta
  
  # (2) construction of 'sum_i sum_j b_ij p_i p_j' (tot2)                             
  # zeta parameters (p-y interaction coefficients) - b
  EEbpipj <- 0
  for (i in 1:neq) {
    for (j in 1:neq) {
      EEbpipj_comp <- coef[[paste0("b_restr_i",i,"j",j)]]*log.price[, i]*log.price[, j]
      EEbpipj <- EEbpipj + EEbpipj_comp
    }
  }

  # (3) construction of 'sum_i w_i p_i' (tot0)                                        
  Ewipi = 0
  for (i in 1:neq) {
    Ewipi_comp <- w[, i] * log.price[, i]
    Ewipi <- Ewipi + Ewipi_comp
  }
  
  # Calculation of u
  u <- (log.exp - Ewipi + 1/2 * EEEazpipj)/(1 - 1/2 * EEbpipj)
  
  # Return
  list_return <- list(
    EEEazpipj = EEEazpipj,
    Ewipi = Ewipi,
    EEbpipj = EEbpipj,
    u=u
  )
  return(list_return)
}  
