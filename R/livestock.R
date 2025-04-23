#' Create an Animal object
#'
#' @param animal_type The type of animal (e.g., dairy_cattle_tied, dairy_cattle)
#' @return An Animal object with emission factors for feeding and housing components
#' @export
#' @examples
#' \dontrun{
#' # Create an Animal object for dairy cattle
#' dairy_cattle = set_livestock_parameters("dairy_cattle")
#' 
#' # Get emission factors for solid manure
#' solid_factors = dairy_cattle$get_solid_emission_factors()
#' }
set_livestock_parameters = function(animal_type) {
  animal = Animal$new(animal_type)
  return(animal)
}
