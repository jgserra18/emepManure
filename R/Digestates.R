#' digested_manure_TAN
#'
#' @param housing_slurry TAN/N deposited in housing from slurry (kg TAN/yr or kg N/yr)
#' @param housing_slurry_NH3 N-NH3 emissions in housing from slurry (kg N-NH3/yr)
#' @param yard TAN/N deposited in yards (kg TAN/yr or kg N/yr)
#' @param yard_NH3 N-NH3 emissions in yards (kg N-NH3/yr)
#' @param f_man_usage_biogas_slurry Proportion of slurry going to either biogas as feedstock (0-1)
#' @description
#' Estimates the amount of TAN/N from slurry either going to biogas as feedstock or ready for manure storage
#' 
#' @returns
#' @export
#' @unit kg TAN/yr or kg N/yr
#' @examples
digested_manure_slurry = function(housing_slurry,
                                  housing_slurry_NH3,
                                  yard,
                                  yard_NH3,
                                  f_man_usage_biogas_slurry) {
  
  if (f_man_usage_biogas_slurry>1) { stop('Proportion of manure going to either biogas as feedstock must be [0,1]')}
  
  return ( 
    ((housing_slurry - housing_slurry_NH3) + (yard - yard_NH3)) * f_man_usage_biogas_slurry
  )
}


#' storage_solid
#'
#' @param ex_housing_solid N/TAN solid manure that leaves housing and passed to storage (kg TAN/yr or kg N/yr)
#' @param f_man_usage_biogas_solid Proportion of solid manure going to either biogas as feedstock (0-1)
#' @description
#' Estimates the amount of TAN/N from solid manure either going to biogas as feedstock or ready for manure storage
#' 
#' @returns
#' @export
#' @unit kg TAN/yr or kg N/yr
#' @examples
digested_manure_solid = function(ex_housing_solid,
                                 f_man_usage_biogas_solid) {
  
  if (f_man_usage_solid>1) { stop('Proportion of manure going to either biogas as feedstock OR stored must be [0,1]')}
  
  return( ex_housing_solid * f_man_usage_biogas_solid )
}


#' digested_application
#'
#' @param biogas_slurry_TAN TAN flow from slurry for biogas (kg TAN/yr)
#' @param biogas_solid_TAN TAN flow from solid manure for biogas (kg TAN/yr)
#' @param biogas_slurry_N N flow from slurry for biogas (kg N/yr)
#' @param biogas_solid_N N flow from solid manure for biogas (kg N/yr
#' @param f_min_digester share of organic N entering the digester that is mineralized to TAN in the digester (0-1)
#' @param EF EF for N-NH3 emissions in storage
#' @description
#' calculates TAN in digestate that will be applied to the field
#' 
#' @returns
#' @export
#' @unit kg TAN/yr
#' @examples
digested_application = function(biogas_slurry_TAN,
                                biogas_solid_TAN,
                                biogas_slurry_N,
                                biogas_solid_N,
                                f_min_digester = 0.32,
                                EF) {
  
  if (f_min_digester>1) { stop('Share of organic N entering the digester that is mineralized to TAN in the digester must be [0,1].')}
  
  TAN_sub = biogas_slurry_TAN + biogas_solid_TAN
  N_sub = biogas_slurry_N+biogas_solid_N
  
  return(
    TAN_sub + f_min_digester * (N_sub - TAN_sub) - (EF * N_sub)
  )
  rm(list=c('TAN_sub','N_sub'))
}

