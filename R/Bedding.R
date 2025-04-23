#' ex_housing_solid_TAN
#'
#' @param housing_solid_TAN TAN deposited in housing from solid manure (kg TAN/head(head/yr))
#' @param housing_solid_NH3 N-NH3 emissions in housing from solid manure (kg N-NH3/yr)
#' @param animal_no livestock number (head/yr)
#' @param bedding_amount Annual straw use in bedding (kg N/(head.yr))
#' @param f_imm fraction of TAN immobilised in organic matter (0-1)
#' @description
#' Calculates the total amount of TAN in solid manure removed from livestock due to immobilisation of TAN in bedding and NH3 emissions
#' 
#' @returns
#' @export
#' @unit kg TAN/yr
#' @examples
ex_housing_solid_TAN = function(housing_solid_TAN,
                                housing_solid_NH3,
                                animal_no,
                                bedding_amount,
                                f_imm) {
  
  # Input validation for feeding component parameters
  if (!is.numeric(housing_solid_TAN) || housing_solid_TAN < 0) {
    stop('ERROR in ex_housing_solid_TAN(): housing_solid_TAN must be a non-negative numeric value.', 
         '\nYou provided: ', housing_solid_TAN, 
         '\nThis parameter represents the total TAN deposited in housing from solid manure.')
  }
  
  if (!is.numeric(bedding_amount) || bedding_amount < 0) {
    stop('ERROR in ex_housing_solid_TAN(): bedding_amount must be a non-negative numeric value.', 
         '\nYou provided: ', bedding_amount, 
         '\nThis parameter represents the annual straw use in bedding (kg N/(head.yr)).')
  }
  
  if (!is.numeric(f_imm) || f_imm < 0 || f_imm > 1) {
    stop('ERROR in ex_housing_solid_TAN(): f_imm (fraction of TAN immobilised) must be between 0 and 1.', 
         '\nYou provided: ', f_imm, 
         '\nTypical values range from 0.0 to 0.4 depending on the C:N ratio of the bedding material.')
  }
  
  # Calculate result
  result = housing_solid_TAN - (housing_solid_NH3 + (animal_no * bedding_amount * f_imm ))
  
  # Check for negative result which would indicate a physical impossibility
  if (result < 0) {
    warning('FEEDING COMPONENT WARNING: Calculated ex_housing_solid_TAN is negative (', result, ').', 
            '\nThis suggests more TAN is being immobilized or lost than is available in the system.', 
            '\nCheck your input parameters, especially bedding_amount (', bedding_amount, ') and f_imm (', f_imm, ').')
  }
  
  return(result)
}

#' ex_housing_solid_N
#'
#' @param housing_solid_N  N deposited in housing from solid manure (kg N/head(head/yr))
#' @param housing_solid_NH3 N-NH3 emissions in housing from solid manure (kg N-NH3/yr)
#' @param animal_no livestock number (head/yr)
#' @param straw_N N added in straw (kg N/(head.yr))
#' @description
#' Calculates the total amount of N in solid manure removed due to N-NH3 emissions
#' 
#' @returns
#' @export
#' @unit kg N/yr
#' @examples
ex_housing_solid_N = function(housing_solid_N,
                              housing_solid_NH3,
                              animal_no,
                              straw_N) {
  
  # Input validation for feeding component parameters
  if (!is.numeric(housing_solid_N) || housing_solid_N < 0) {
    stop('ERROR in ex_housing_solid_N(): housing_solid_N must be a non-negative numeric value.', 
         '\nYou provided: ', housing_solid_N, 
         '\nThis parameter represents the total N deposited in housing from solid manure.')
  }
  
  if (!is.numeric(straw_N) || straw_N < 0) {
    stop('ERROR in ex_housing_solid_N(): straw_N must be a non-negative numeric value.', 
         '\nYou provided: ', straw_N, 
         '\nThis parameter represents the N added in straw (kg N/(head.yr)).')
  }
  
  # Calculate result
  result = housing_solid_N - (housing_solid_NH3 + (animal_no * straw_N))
  
  # Check for negative result which would indicate a physical impossibility
  if (result < 0) {
    warning('FEEDING COMPONENT WARNING: Calculated ex_housing_solid_N is negative (', result, ').', 
            '\nThis suggests more N is being added or lost than is available in the system.', 
            '\nCheck your input parameters, especially straw_N (', straw_N, ').')
  }
  
  return(result)
}

