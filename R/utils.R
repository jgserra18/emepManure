#' @title Utility functions for animal data processing
#' @description Helper functions for processing animal data,
#' standardizing animal types, and extracting emission factors.
#' @keywords internal

#' Standardize animal type based on livestock class conversion
#'
#' @param animal_type Character string representing the animal type
#' @param config_dir Path to the configuration directory
#' @return Standardized animal type
#' @keywords internal
.standardize_animal_type = function(animal_type, config_dir) {
  conversion_file = file.path(config_dir, "livestock_class_conversion.yaml")
  if (!file.exists(conversion_file)) {
    stop("Livestock class conversion file not found: ", conversion_file)
  }
  
  # Read the YAML file as text to handle the non-standard format
  yaml_text = readLines(conversion_file)
  
  # Check if the animal_type is already a standard type
  standard_types = c("dairy_cattle", "cattle", "pigs", "sows", "birds")
  if (animal_type %in% standard_types) {
    return(animal_type)
  }
  
  # Manual parsing for the specific format
  current_type = NULL
  for (line in yaml_text) {
    # Skip the first line with livestock_conversions:
    if (grepl("livestock_conversions:", line)) next
    
    # Check if this is a main category line (has a colon)
    if (grepl(":\\s*$", line)) {
      current_type = gsub("\\s+|:", "", line)
    } else if (!is.null(current_type) && grepl(animal_type, line)) {
      # If we found the animal type in a subtypes line
      return(current_type)
    }
  }
  
  # If we couldn't find a match, use the original type
  # This allows for animal types that might not be in the conversion file
  warning("Animal type '", animal_type, "' not found in conversion file. Using as is.")
  return(animal_type)
}

#' Extract data for a specific animal type from a data structure
#'
#' @param data_structure The data structure to extract from
#' @param original_type The original animal type
#' @param std_type The standardized animal type
#' @param config_dir Path to the configuration directory
#' @return The extracted data for the animal
#' @keywords internal
.extract_animal_data = function(data_structure, original_type, std_type, config_dir) {
  # First try with the original animal type
  if (!is.null(data_structure[[original_type]])) {
    return(data_structure[[original_type]])
  }
  
  # Then try with the standardized type
  if (!is.null(data_structure[[std_type]])) {
    return(data_structure[[std_type]])
  }
  
  # For birds category, check if the animal is in the birds category
  # and if there's a "birds" entry in the data structure
  conversion_file = file.path(config_dir, "livestock_class_conversion.yaml")
  conversions = yaml::read_yaml(conversion_file)$livestock_conversions
  
  if ("birds" %in% names(conversions) && 
      (original_type %in% conversions$birds || std_type %in% conversions$birds) &&
      !is.null(data_structure$birds)) {
    return(data_structure$birds)
  }
  
  # Return NULL if no data found
  return(NULL)
}


#' dairy_excretion_coeff
#'
#' @param milk_prod milk produced per dairy cow kg milk/(head.yr)
#' @description
#' updates dairy cattle nitrogen excreted (105) based on milk produced
#' 
#' @returns
#' @export
#' @unit kg N/
#' @examples dairy_excretion_coeff(milk_prod = 10000)
dairy_excretion_coeff = function(milk_prod) {
  
  if (length(milk_prod)==0) { stop('Please provide at least a value for milk produced per dairy cow.') }
  
  standard_Nexc = 115 # kg N/(head.yr)
  standard_productivity = 7000 # kg milk/(head.yr)
  
  dif_productivity = milk_prod - standard_productivity
  
  return(ifelse(dif_productivity<=0,
                standard_Nexc+standard_Nexc*0.1*dif_productivity/1000,
                standard_Nexc+standard_Nexc*0.02*dif_productivity/1000)
  )
}
