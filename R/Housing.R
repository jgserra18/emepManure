#' housing_deposited
#'
#' @param housing_nutrient total amount of either N/TAN deposited onto animal housing (kg N/yr or kg TAN/yr)
#' @param f_man_type  proportion of livestock manure handled as slurry or solid (0-1)
#' @description
#' amounts of N or TAN deposited during housing handled as either slurry or solid
#' 
#' @returns
#' @export
#' @unit kg N/yr or kg TAN/yr
#' @examples 
housing_deposited = function(housing_nutrient,
                             f_man_type) {
  
  if (f_man_type < 0 || f_man_type > 1) { 
    stop('ERROR in housing_deposited(): The proportion of manure handled as either liquid/solid (f_man_type) must be between 0 and 1. You provided: ', f_man_type, 
         '\nPlease check your input parameters for this animal type.')
  }
  
  return( housing_nutrient * f_man_type)
}

#' housing_NH3
#'
#' @param housing_tan total amount of TAN deposited onto animal housing (kg TAN/yr)
#' @param EF Housing emission factor (kg N-NH3/kg TAN)
#' @description
#' calculates N-NH3 losses from livestock housing for both slurry and solid
#' 
#' @returns
#' @export
#' @unit kg N-NH3/yr
#' @examples
housing_NH3 = function(housing_tan,
                       EF) {
  
  return( housing_tan * EF)
}
