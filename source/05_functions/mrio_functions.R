# File info --------------------------------------------------------------------

# File:    Functions for MRIO computations
# Authors: Charlotte Plinke & Michael Sureth
# Paper:   Environmental Impacts from European Food Consumption Can Be Reduced 
#          with Carbon Pricing or a Value-Added Tax Reform


# Footprint computation ---------------------------------------------------

compute_footprints <- function(Y_c, L, S_stress, S_impact){
  
  #' @name compute_footprints
  #' @title Compute footprints
  #' @description Compute footprints (based on Unequal_Exchanges_function_mrio_EXIOBASE_v1 from Bruckner et al 2023)
  #' @param Y_c Final demand vector for a single country (dimensions: 49*200 = 9800 country-products x 1)
  #' @param L Leontief inverse (49*200 x 49x200)
  #' @param S_stress Matrix of selected stressors (nstress x 49*200)
  #' @param S_impact Matrix of selected impacts (nimpacts x 49*200)
  
  # create empty matrices for results
  CBF_reg_matrix <- matrix(0,
                         nrow = nrow(L),
                         ncol = (nrow(S_stress) + nrow(S_impact)))
    
  # compute total output X_c (in MEUR) given L and FDhh_c_g (in MEUR):
  # X_c = L %*% FDhh_c_g
  # - FDhh_c_g is final demand of households in country c in product group g
  #   (200*49 = 9800 items-vector with entries >0 if contained in group g)
  # - L is total requirements matrix
  # - X_c is vector of total output produced in all 9800 country-product categories
  #   attributed to meet final demand of households in category g in region/country c
  X_c <- L %*% Y_c

  x <- 0
  # compute regionally explicit vector of stress:
  # consumption-based footprints = diag(S_vec) %*% X_c
  # - S_vec = single row of stressor coefficients (coefficients for all product-
  #   countries with respect to single stressor) (1x9800)
  for (j1 in 1:nrow(S_stress)) {
    x <- x + 1
    # (select single stressor, e.g., "Cropland - Cereal grains nec") 
    S_vec <- S_stress[j1 , ] 
    # diagonalize vector (9800 x 9800)
    S_vec_diag <- diag(S_vec)
    # compute regionally explicit stress, i.e., stress produced by each product
    # in each country (200*49 = 9800) to meet final demand in country c of
    # product group g (in unit of respective stressor row, e.g., km^2)
    CBF_reg_matrix[ , x] <- S_vec_diag %*% X_c
  }
  
  # repeat for impacts
  for (j2 in 1:nrow(S_impact)) { 
    x <- x + 1
    
    I_vec <- S_impact[j2 , ]
    I_vec_diag <- diag(I_vec)
    
    CBF_reg_matrix[ , x] <- I_vec_diag %*% X_c
  }
    
  return(CBF_reg_matrix)
}
