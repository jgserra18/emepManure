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
  
  conversions = yaml::read_yaml(conversion_file)$livestock_conversions
  
  # Check if the animal_type is already a standard type (a key in the conversions)
  if (animal_type %in% names(conversions)) {
    return(animal_type)
  }
  
  # Find which standard type this animal_type belongs to
  for (std_type in names(conversions)) {
    if (animal_type %in% conversions[[std_type]]) {
      return(std_type)
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
