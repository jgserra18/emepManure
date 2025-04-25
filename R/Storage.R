#' storage_slurry
#'
#' @param housing_slurry TAN/N deposited in housing from slurry (kg TAN/yr or kg N/yr)
#' @param housing_slurry_NH3 N-NH3 emissions in housing from slurry (kg N-NH3/yr)
#' @param yard TAN/N deposited in yards (kg TAN/yr or kg N/yr)
#' @param yard_NH3 N-NH3 emissions in yards (kg N-NH3/yr)
#' @param f_man_usage_slurry Proportion of manure going to either biogas as feedstock OR stored must be (0-1)
#' @description
#' Estimates the amount of TAN/N from slurry either going to biogas as feedstock or ready for manure storage
#' 
#' @returns
#' @export
#' @unit kg TAN/yr or kg N/yr
#' @examples
storage_slurry = function(housing_slurry,
                          housing_slurry_NH3,
                          yard,
                          yard_NH3,
                          f_man_usage_slurry) {
  
  if (f_man_usage_slurry > 1) { stop('Proportion of manure going to either biogas as feedstock OR stored must be [0,1]')}
  
  return ( 
    ((housing_slurry - housing_slurry_NH3) + (yard - yard_NH3)) * f_man_usage_slurry
  )
}

#' updated_storage_slurry
#'
#' @param out_storage_slurry_TAN TAN from slurry stored (kg TAN/yr)
#' @param out_storage_slurry_N N from slurry stored (kg N/yr)
#' @param f_min Fraction of organic N that is mineralised (0,1)
#' @description
#' Updates storage TAN from slurry to account for organic mineralisation of TAN before emission losses
#' 
#' @returns
#' @export
#' @unit kg TAN/yr
#' @examples
updated_storage_slurry = function(out_storage_slurry_TAN,
                                  out_storage_slurry_N,
                                  f_min) {
  
  if (f_min>1) { stop('Fraction of organic N that is mineralised must be [0,1]')}
  
  return(out_storage_slurry_TAN + ((out_storage_slurry_N-out_storage_slurry_TAN)*f_min))
}


#' storage_solid
#'
#' @param ex_housing_solid N/TAN solid manure that leaves housing and passed to storage (kg TAN/yr or kg N/yr)
#' @param f_man_usage_solid Proportion of manure going to either biogas as feedstock OR stored must be (0-1)
#' @description
#' Estimates the amount of TAN/N from solid manure either going to biogas as feedstock or ready for manure storage
#' 
#' @returns
#' @export
#' @unit kg TAN/yr or kg N/yr
#' @examples
storage_solid = function(ex_housing_solid,
                         f_man_usage_solid) {
  
  if (f_man_usage_solid>1) { stop('Proportion of manure going to either biogas as feedstock OR stored must be [0,1]')}
  
  return( ex_housing_solid * f_man_usage_solid)
}


#' storage_emissions
#'
#' @param storage_TAN TAN flows from slurry or solid manure in storage (kg TAN/yr)
#' @param EF_NH3 Storage emission factor (kg N-NH3/kg TAN)
#' @param EF_N2O Storage emission factor (kg N-N2O/kg TAN)
#' @param EF_NO Storage emission factor (kg N-NO/kg TAN)
#' @param EF_N2 Storage emission factor (kg N-N2/kg TAN)
#' @description
#' computes storage gaseous losses (NH3, N2O, NO, N2) as well as their total 
#' 
#' @returns list for each one of the gases, and their total
#' @export
#' @unit kg N-N_compound/yr
#' @examples
storage_emissions = function(storage_TAN,
                             EF_NH3,
                             EF_N2O,
                             EF_NO,
                             EF_N2) {
  
  storage_NH3 = storage_TAN * EF_NH3
  storage_N2O = storage_TAN * EF_N2O
  storage_NO = storage_TAN * EF_NO
  storage_N2 = storage_TAN * EF_N2
  
  return(list(
    NH3 = storage_NH3,
    N2O = storage_N2O,
    NO = storage_NO,
    N2 = storage_N2,
    TOTAL = storage_NH3 + storage_N2O + storage_NO + storage_N2
  ))
}


