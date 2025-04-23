
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
#' @param animal_no livestock number (head /yr)
#' @param exc_coef nutrient excretion coefficient (kg nutrient/(head.yr))
#' @param f_allocation (0-1) allocation of time spent in different systems (ie, grazing, yards, housed)
#' @description 
#' allocates livestock excretion to different systems
#' 
#' @returns
#' @export
#' @unit kg N/yr
#' @examples allocate_livestock_excretion(animal_no=50, exc_coef = 100, f_allocation = 0.33)
allocate_livestock_excretion = function(animal_no,
                                        exc_coef,
                                        f_allocation) {
  
  
  if (f_allocation>1) { stop('f_allocation concerns the fraction of the time animal spends in a given setting (housing, yards, grazing). Must be always below 100')}
  
  return( livestock_excretion(animal_no, exc_coef) * f_allocation )
}

#' allocate_TAN_excretion
#'
#' @param animal_no livestock number (head /yr)
#' @param exc_coef nutrient excretion coefficient (kg nutrient/(head.yr))
#' @param f_allocation (0-1) allocation of time spent in different systems (ie, grazing, yards, housed)
#' @param f_TAN (0-1) TAN fraction of manure
#' @description
#' Allocates manure excreted (in TAN) to the different systems
#' 
#' @returns
#' @export
#' @unit kg TAN/yr
#' @examples
allocate_TAN_excretion = function(animal_no, 
                                  exc_coef, 
                                  f_allocation,
                                  f_TAN) {
  
  if (f_TAN>1) { stop('TAN fraction should be [0,1].') }
  return( allocate_TAN_excretion(animal_no, exc_coef, f_allocation, f_TNA) )                                   
}