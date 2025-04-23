#' grazing_NH3
#'
#' @param grazing_tan total amount of TAN deposited onto pastures (kg TAN/yr)
#' @param EF Grazing emission factor (kg N-NH3/kg TAN)
#' @description
#' calculates N-NH3 losses from livestock yards
#' 
#' @returns
#' @export
#' @unit kg N-NH3/yr
#' @examples
grazing_NH3 = function(grazing_tan,
                       EF) {
  
  return( grazing_tan * EF)
}

#' net_manure_application
#'
#' @param manure_grazing Grazing N/TAN droppings (kg TAN/yr or kg N/yr)
#' @param grazing_NH3 N-NH3 emissions from grazing (kg N-NH3/yr)
#' @description
#' computes net N/TAN amounts of manure excreted onto pastures
#' 
#' @returns
#' @export
#' @unit kg TAN/yr or kg N/yr 
#' @examples
net_grazing = function(manure_grazing,
                       grazing_NH3) {
  
  return(manure_grazing-grazing_NH3)
}
