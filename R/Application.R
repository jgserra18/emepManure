#' manure_direct_application
#'
#' @param out_storage output from storage_slurry (not updated_storage_slurry!) and storage_solid
#' @param c_f_man_usage 
#'
#' @returns
#' @export
#'
#' @examples
manure_direct_application = function(out_storage,
                                     c_f_man_usage) {
  
  if (class(c_f_man_usage)!='list') { stop('This is meant to be a vector (n=2) with the allocation to biogas and stored.') }
  if (length(c_f_man_usage)!=2) { stop('Must have a length of 2; if no use, please set it to 0.')}
  
  # sum manure proportions and check for consistency
  f_man_usage = sum(c_f_man_usage) # sum fraction of manure to be used as feedstock or stored
  if (f_man_usage>1) { stop('Sum of proportion of manure used as feedstock + stored cannot be higher than 1 (or 100%).') }
  
  return(out_storage * (1-c_f_man_usage)/f_man_usage)
}


#' slurry_application
#'
#' @param appl_direct_slurry TAN/N slurry with immediate application (kg TAN/yr or kg N/yr)
#' @param out_storage_slurry  output from storage_slurry (not updated_storage_slurry!) and storage_solid (kg TAN/yr or kg N/yr)
#' @param digestate N or TAN with field application (kg TAN/yr or kg N/yr)
#' @param storage_em_slurry Total gaseous N emissions from slurry storage
#' @description
#' COmputes total N/TAN of slurry applied to the field
#' 
#' @returns
#' @export
#' @unit kg TAN/yr or kg N/yr
#' @examples
slurry_application = function(appl_direct_slurry,
                              out_storage_slurry,
                              digestate,
                              storage_em_slurry) {
  
  return( appl_direct_slurry + out_storage_slurry + digestate - storage_em_slurry$TOTAL )
}

#' solid_manure_application
#'
#' @param appl_direct_solid TAN/N solid manure with immediate application (kg TAN/yr or kg N/yr)
#' @param out_storage_solid  output from storage_slurry (not updated_storage_slurry!) and storage_solid (kg TAN/yr or kg N/yr)
#' @param storage_em_solid Total gaseous N emissions from solid manure storage
#' @description
#' COmputes total N/TAN of solid manure applied to the field
#' 
#' @returns
#' @export
#' @unit kg TAN/yr or kg N/yr
#' @examples
solid_manure_application = function(appl_direct_solid,
                                    out_storage_solid,
                                    storage_em_solid) {
  
  return( appl_direct_solid + out_storage_solid - storage_em_solid$TOTAL )
}


#' application_NH3
#'
#' @param manure_appl_TAN solid manure or slurry TAN application (kg TAN/yr)
#' @param EF NH3 EF for manure application (kg N-NH3/kg TAN)
#' @description
#' N-NH3 volatilisation during field application of solid manure or slurry
#' 
#' @returns
#' @export
#' @unit kg N-NH3/yr
#' @examples
application_NH3 = function(manure_appl_TAN,
                           EF) {
  
  return(manure_appl_TAN * EF)
}


#' net_manure_application
#'
#' @param manure_appl solid manure or slurry N/TAN application (kg TAN/yr or kg N/yr)
#' @param appl_NH3 N-NH3 emissions from solid manure or slurry application (kg N-NH3/yr)
#' @description
#' computes net N/TAN amounts of solid manure or solid returned to the soil 
#' 
#' @returns
#' @export
#' @unit kg TAN/yr or kg N/yr 
#' @examples
net_manure_application = function(manure_appl,
                                  appl_NH3) {
  
  return(manure_appl-appl_NH3)
}
