#' ex_housing_solid_TAN
#'
#' @param housing_solid_TAN TAN deposited in housing from solid manure (kg TAN/head(head/yr))
#' @param housing_solid_NH3 N-NH3 emissions in housing from solid manure (kg N-NH3/yr)
#' @param animal_no livestock number (head/yr)
#' @param f_man_solid Proportion of manure handled as solid (0-1)
#' @param bedding_amount Annual straw use in bedding (kg N/(head.yr))
#' @param f_imm fraction of TAN immobilised in organic matter (0-1)
#' @description
#' Calculates the total amount of TAN in solid manure removed from livestock due to immobilisation of TAN in bedding and NH3 emissions
#' 
#' @return Total TAN in solid manure including bedding TAN minus NH3 emissions
#' @export
#' @examples
#' # Example usage
#' ex_housing_solid_TAN(100, 10, 50, 0.3, 500, 0.0067)
ex_housing_solid_TAN = function(housing_solid_TAN,
                                housing_solid_NH3,
                                animal_no,
                                f_man_solid,
                                bedding_amount,
                                f_imm) {
  
  ex_house = housing_solid_TAN - (housing_solid_NH3 + (animal_no * f_man_solid * bedding_amount * f_imm ))
  ex_house = ifelse(ex_house<0, 0, ex_house)

  return(ex_house)
}

#' ex_housing_solid_N
#'
#' @param housing_solid_N  N deposited in housing from solid manure (kg N/head(head/yr))
#' @param housing_solid_NH3 N-NH3 emissions in housing from solid manure (kg N-NH3/yr)
#' @param animal_no livestock number (head/yr)
#' @param bedding_amount Annual straw use in bedding (kg N/(head.yr))
#' @param f_man_solid Fraction of manure handled as solid [0-1]
#' @description
#' Calculates the total amount of N in solid manure removed due to N-NH3 emissions
#' 
#' @return Total N in solid manure including bedding N minus NH3 emissions
#' @export
#' @unit kg N/yr
#' @examples
#' # Example usage
#' ex_housing_solid_N(100, 10, 50, 500, 0.3)
ex_housing_solid_N = function(housing_solid_N,
                              housing_solid_NH3,
                              animal_no,
                              bedding_amount,
                              f_man_solid) {
  
  return( housing_solid_N + (animal_no * bedding_amount * 0.04 * f_man_solid ) - housing_solid_NH3 )
}
