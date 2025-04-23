
#' livestock_excretion
#'
#' @param animal_no livestock number (head /yr)
#' @param exc_coef nutrient excretion coefficient (kg nutrient/yr)
#' @description 
#' calculates total nutrient excreted from livestock
#' 
#' @returns int
#' @export
#' @unit kg N/yr
#' @examples livestock_excretion(animal_no=50, exc_coef = 100)
livestock_excretion = function(animal_no,
                               exc_coef) {
  
  return(animal_no * exc_coef)
}


#' allocate_livestock_excretion
#'
#' @param exc_total Total nutrient excreted from livestock (kg N/yr)
#' @param f_allocation (0-1) allocation of time spent in different systems (ie, grazing, yards, housed)
#' @description 
#' allocates livestock excretion to different systems
#' 
#' @returns
#' @export
#' @unit kg N/yr
#' @examples allocate_livestock_excretion(animal_no=50, exc_coef = 100, f_allocation = 0.33)
allocate_livestock_excretion = function(exc_total,
                                        f_allocation) {
  
  
  if (f_allocation < 0 || f_allocation > 1) { 
    stop('ERROR in allocate_livestock_excretion(): The allocation fraction (f_allocation) must be between 0 and 1.', 
         '\nThis parameter represents the fraction of time an animal spends in a specific setting (housing, yards, grazing).', 
         '\nYou provided: ', f_allocation, 
         '\nPlease ensure the sum of all allocation fractions across all settings equals 1.')
  }
  
  return( exc_total * f_allocation )
}

#' allocate_TAN_excretion
#' @param exc_allocation Nutrient excreted for a given pathway (yards, housing, grazing) (kg N/yr)
#' @param f_TAN (0-1) TAN fraction of manure
#' @description
#' Allocates manure excreted (in TAN) to the different systems
#' 
#' @returns
#' @export
#' @unit kg TAN/yr
#' @examples
allocate_TAN_excretion = function(exc_allocation,
                                  f_TAN) {
  
  if (f_TAN < 0 || f_TAN > 1) { 
    stop('ERROR in allocate_TAN_excretion(): The TAN fraction (f_TAN) must be between 0 and 1.', 
         '\nThis parameter represents the proportion of total N excreted as TAN.', 
         '\nYou provided: ', f_TAN, 
         '\nTypical values range from 0.5 to 0.7 for most livestock types.')
  }
  return( exc_allocation * f_TAN )                                   
}

