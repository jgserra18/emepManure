#' yards_NH3
#'
#' @param yards_tan total amount of TAN deposited onto yards (kg TAN/yr)
#' @param EF Yards emission factor (kg N-NH3/kg TAN)
#' @description
#' calculates N-NH3 losses from livestock yards
#' 
#' @returns
#' @export
#' @unit kg N-NH3/yr
#' @examples
yards_NH3 = function(yards_tan,
                     EF) {
  
  return( yards_tan * EF)
}
