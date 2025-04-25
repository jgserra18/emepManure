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
digested_application_TAN = function(biogas_slurry_TAN,
                                biogas_solid_TAN,
                                biogas_slurry_N,
                                biogas_solid_N,
                                f_min_digester = 0.0067,
                                EF) {
  
  if (f_min_digester>1) { stop('Share of organic N entering the digester that is mineralized to TAN in the digester must be [0,1].')}
  
  TAN_sub = biogas_slurry_TAN + biogas_solid_TAN
  N_tot = biogas_slurry_N+biogas_solid_N
  
  return(
    TAN_sub + f_min_digester * (N_tot - TAN_sub) - (EF * N_tot)
  )
}

#' digested_application_N
#'
#' @param biogas_slurry_N N flow from slurry for biogas (kg N/yr)
#' @param biogas_solid_N N flow from solid manure for biogas (kg N/yr
#' @param EF EF for N-NH3 emissions in storage
#' @description
#' calculates N in digestate that will be applied to the field
#' 
#' @returns
#' @export
#' @unit kg N/yr
#' @examples
digested_application_N = function(biogas_slurry_N,
                                  biogas_solid_N,
                                  EF) {
  
  
  N_tot = biogas_slurry_N+biogas_solid_N
  return(
    N_tot * EF
  )
}

