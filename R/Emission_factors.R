#' @title Emission Factors for Manure Management System
#' @description Functions for loading and retrieving emission factors for different stages
#' of the manure management system (housing, yards, grazing, storage, application).
#'
#' @importFrom yaml read_yaml

#' @importFrom utils message warning

#' @description Get emission factor for a specific animal type, stage, and gas
#' @param animal_type Type of animal (e.g., "dairy_cattle", "pigs")
#' @param stage Stage in the manure management system (e.g., "housing", "yards", "grazing", "storage", "application")
#' @param gas Type of gas (e.g., "NH3", "N2O", "NO", "N2")
#' @param manure_type Type of manure (e.g., "slurry", "solid", NULL for stages like yards and grazing)
#' @param config_paths List of paths to configuration files
#' @param debug_mode Enable debug mode for more verbose output
#' @return Emission factor value or NULL if not found
get_emission_factor = function(animal_type, stage, gas, manure_type = NULL, 
                               config_paths = list(
                                 mms_NH3 = system.file("extdata/mms_NH3.yaml", package = "rEMEP"),
                                 storage_N2O = system.file("extdata/storage_N2O.yaml", package = "rEMEP"),
                                 storage_OTHERS = system.file("extdata/storage_OTHERS.yaml", package = "rEMEP"),
                                 digestate_NH3 = system.file("extdata/digestate_NH3.yaml", package = "rEMEP")
                               ),
                               debug_mode = FALSE,
                               slurry_crust = FALSE) {
  
  # Validate inputs
  if (is.null(animal_type) || is.null(stage) || is.null(gas)) {
    warning("Required parameters missing: animal_type, stage, and gas must be provided.")
    return(NULL)
  }
  
  # For stages that require manure_type, validate it's provided
  if (stage %in% c("housing", "storage", "application") && is.null(manure_type)) {
    warning(paste("manure_type must be provided for stage:", stage))
    return(NULL)
  }
  
  # Load emission factors based on gas type
  emission_factors = switch(gas,
    "NH3" = load_NH3_emission_factors(animal_type, config_paths, debug_mode),
    "N2O" = load_N2O_emission_factors(animal_type, config_paths, debug_mode),
    "NOx" = load_OTHER_emission_factors(config_paths, debug_mode)$NOx,
    "N2" = load_OTHER_emission_factors(config_paths, debug_mode)$N2,
    {
      warning(paste("Unsupported gas type:", gas))
      return(NULL)
    }
  )
  
  if (is.null(emission_factors)) {
    warning(paste("No emission factors found for gas:", gas))
    return(NULL)
  }
  
  # Get emission factor for the specific stage and manure type
  ef = NULL
  
  if (gas == "NH3") {
    if (stage %in% c("yards", "grazing")) {
      # For yards and grazing, manure_type is not needed
      if (stage %in% names(emission_factors)) {
        ef = emission_factors[[stage]]
      }
    } else if (stage %in% names(emission_factors)) {
      # For housing, storage, application, manure_type is needed
      if (manure_type %in% names(emission_factors[[stage]])) {
        ef = emission_factors[[stage]][[manure_type]]
      }
    }
  } else if (gas == "N2O") {
    # N2O emission factors are only for storage stage
    if (stage == "storage") {
      if (manure_type == "solid" && "solid_manure_heaps" %in% names(emission_factors) &&
          "EF" %in% names(emission_factors$solid_manure_heaps)) {
        ef = emission_factors$solid_manure_heaps$EF
      } else if (manure_type == "slurry") {
        # For slurry, check if crust information is available
        has_crust = slurry_crust  # Use the function parameter
        
        if (has_crust && "slurry_with_crust" %in% names(emission_factors) &&
            "EF" %in% names(emission_factors$slurry_with_crust)) {
          ef = emission_factors$slurry_with_crust$EF
        } else if (!has_crust && "slurry_without_crust" %in% names(emission_factors) &&
                  "EF" %in% names(emission_factors$slurry_without_crust)) {
          ef = emission_factors$slurry_without_crust$EF
        } else if ("slurry_without_crust" %in% names(emission_factors) &&
                  "EF" %in% names(emission_factors$slurry_without_crust)) {
          # Default to without crust if specific option not found
          ef = emission_factors$slurry_without_crust$EF
        }
      }
    }
  } else if (gas %in% c("NOx", "N2")) {
    # NO and N2 emission factors are only for storage stage
    if (stage == "storage" && !is.null(manure_type) && manure_type %in% names(emission_factors)) {
      ef = emission_factors[[manure_type]]
    }
  }
  
  if (is.null(ef)) {
    warning(paste("Emission factor not found for animal_type:", animal_type, 
                  "stage:", stage, "gas:", gas, 
                  if (!is.null(manure_type)) paste("manure_type:", manure_type) else ""))
  }
  
  return(ef)
}

#' @description Load NH3 emission factors from mms_NH3.yaml
#' @param animal_type Type of animal (e.g., "dairy_cattle", "pigs")
#' @param config_paths List of paths to configuration files
#' @param debug_mode Enable debug mode for more verbose output
#' @return List of NH3 emission factors
load_NH3_emission_factors = function(animal_type, config_paths, debug_mode = FALSE) {
  # Get file path
  nh3_file = config_paths$mms_NH3
  if (debug_mode) {
    message(paste("Using NH3 emission factors file:", nh3_file))
  }
  
  # Check if file exists
  if (!file.exists(nh3_file)) {
    warning(paste("NH3 emission factors file not found:", nh3_file))
    return(NULL)
  }
  
  tryCatch({
    if (debug_mode) {
      message("Reading NH3 emission factors file...")
    }
    
    nh3_data = yaml::read_yaml(nh3_file)
    
    if (debug_mode) {
      message("NH3 emission factors file read successfully.")
      message(paste("Available animal types:", paste(names(nh3_data$MMS_NH3_EF), collapse = ", ")))
    }
    
    # Try to find emission factors for the specific animal type
    if (animal_type %in% names(nh3_data$MMS_NH3_EF)) {
      if (debug_mode) {
        message(paste("Found NH3 emission factors for animal type:", animal_type))
      }
      return(nh3_data$MMS_NH3_EF[[animal_type]])
    }
    
    # If not found, try with the main animal type
    main_animal_type = get_main_animal_type(animal_type)
    if (!is.null(main_animal_type) && main_animal_type %in% names(nh3_data$MMS_NH3_EF)) {
      if (debug_mode) {
        message(paste("Found NH3 emission factors for main animal type:", main_animal_type))
      }
      return(nh3_data$MMS_NH3_EF[[main_animal_type]])
    }
    
    # If still not found, return NULL and issue a warning
    warning(paste("NH3 emission factors not found for animal type:", animal_type))
    return(NULL)
  }, error = function(e) {
    warning(paste("Error loading NH3 emission factors:", e$message))
    return(NULL)
  })
}

#' @description Load N2O emission factors from storage_N2O.yaml
#' @param animal_type Type of animal (e.g., "dairy_cattle", "pigs")
#' @param config_paths List of paths to configuration files
#' @param debug_mode Enable debug mode for more verbose output
#' @return List of N2O emission factors
load_N2O_emission_factors = function(animal_type, config_paths, debug_mode = FALSE) {
  # Get file path
  n2o_file = config_paths$storage_N2O
  if (debug_mode) {
    message(paste("Using N2O emission factors file:", n2o_file))
  }
  
  # Check if file exists
  if (!file.exists(n2o_file)) {
    warning(paste("N2O emission factors file not found:", n2o_file))
    return(NULL)
  }
  
  tryCatch({
    if (debug_mode) {
      message("Reading N2O emission factors file...")
    }
    
    n2o_data = yaml::read_yaml(n2o_file)
    
    if (debug_mode) {
      message("N2O emission factors file read successfully.")
      message(paste("Available animal types:", paste(names(n2o_data$storage_N2O_EF), collapse = ", ")))
    }
    
    # Try to find emission factors for the specific animal type
    if (animal_type %in% names(n2o_data$storage_N2O_EF)) {
      if (debug_mode) {
        message(paste("Found N2O emission factors for animal type:", animal_type))
      }
      return(n2o_data$storage_N2O_EF[[animal_type]])
    }
    
    # If not found, try with the main animal type
    main_animal_type = get_main_animal_type(animal_type)
    if (!is.null(main_animal_type) && main_animal_type %in% names(n2o_data$storage_N2O_EF)) {
      if (debug_mode) {
        message(paste("Found N2O emission factors for main animal type:", main_animal_type))
      }
      return(n2o_data$storage_N2O_EF[[main_animal_type]])
    }
    
    # If still not found, return NULL and issue a warning
    warning(paste("N2O emission factors not found for animal type:", animal_type))
    return(NULL)
  }, error = function(e) {
    warning(paste("Error loading N2O emission factors:", e$message))
    return(NULL)
  })
}

#' @param config_paths List of paths to configuration files
#' @param debug_mode Enable debug mode for more verbose output
#' @return List of other emission factors
load_OTHER_emission_factors = function(config_paths, debug_mode = FALSE) {
  # Get file path
  others_file = config_paths$storage_OTHERS
  if (debug_mode) {
    message(paste("Using OTHER emission factors file:", others_file))
  }
  
  # Check if file exists
  if (!file.exists(others_file)) {
    warning(paste("OTHER emission factors file not found:", others_file))
    return(NULL)
  }
  
  tryCatch({
    if (debug_mode) {
      message("Reading OTHER emission factors file...")
    }
    
    others_data = yaml::read_yaml(others_file)
    
    if (debug_mode) {
      message("OTHER emission factors file read successfully.")
      message(paste("YAML structure keys:", paste(names(others_data), collapse = ", ")))
      if (!is.null(others_data$storage_OTHERS_EF)) {
        message(paste("Available manure types:", paste(names(others_data$storage_OTHERS_EF), collapse = ", ")))
      }
    }
    
    # These emission factors are universal across animal types
    if (!is.null(others_data$storage_OTHERS_EF)) {
      if (debug_mode) {
        message("Found OTHER emission factors.")
      }
      return(others_data$storage_OTHERS_EF)
    } else {
      warning("Other emission factors not found in the expected format")
      if (debug_mode) {
        message(paste("YAML content structure:", paste(capture.output(str(others_data)), collapse = "\n")))
      }
      return(NULL)
    }
  }, error = function(e) {
    warning(paste("Error loading other emission factors:", e$message))
    return(NULL)
  })
}

#' @description Load digestate NH3 emission factors from digestate_NH3.yaml
#' @param config_paths List of paths to configuration files
#' @param debug_mode Enable debug mode for more verbose output
#' @return Digestate NH3 emission factor or NULL if not found
load_digestate_NH3_emission_factors = function(config_paths, debug_mode = FALSE) {
  # Get file path
  digestate_file = config_paths$digestate_NH3
  if (debug_mode) {
    message(paste("Using digestate NH3 emission factors file:", digestate_file))
  }
  
  # Check if file exists
  if (!file.exists(digestate_file)) {
    warning(paste("Digestate NH3 emission factors file not found:", digestate_file))
    return(NULL)
  }
  
  tryCatch({
    if (debug_mode) {
      message("Reading digestate NH3 emission factors file...")
    }
    
    digestate_data = yaml::read_yaml(digestate_file)
    
    if (debug_mode) {
      message("Digestate NH3 emission factors file read successfully.")
    }
    
    # Get the digestate emission factor
    if (!is.null(digestate_data$digestate_NH3_EF) && !is.null(digestate_data$digestate_NH3_EF$digestate)) {
      if (debug_mode) {
        message(paste("Found digestate NH3 emission factor:", digestate_data$digestate_NH3_EF$digestate))
      }
      return(digestate_data$digestate_NH3_EF$digestate)
    }
    
    # If not found, return NULL and issue a warning
    warning("Digestate NH3 emission factor not found in the expected format")
    return(NULL)
  }, error = function(e) {
    warning(paste("Error loading digestate NH3 emission factors:", e$message))
    return(NULL)
  })
}

#' @description Get the main animal type from a specific animal type
#' @param animal_type Type of animal (e.g., "dairy_cattle", "pigs")
#' @return Main animal type or NULL if not found
get_main_animal_type = function(animal_type) {
  # Map specific animal types to main animal types
  if (grepl("cattle", animal_type, ignore.case = TRUE)) {
    return("cattle")
  } else if (grepl("pig", animal_type, ignore.case = TRUE)) {
    return("pigs")
  } else if (grepl("sheep", animal_type, ignore.case = TRUE)) {
    return("sheep")
  } else if (grepl("goat", animal_type, ignore.case = TRUE)) {
    return("goats")
  } else if (grepl("poultry", animal_type, ignore.case = TRUE) || 
             grepl("chicken", animal_type, ignore.case = TRUE) ||
             grepl("duck", animal_type, ignore.case = TRUE) ||
             grepl("turkey", animal_type, ignore.case = TRUE)) {
    return("poultry")
  } else {
    return(NULL)
  }
}
#' @description Compile all emission factors for a specific animal type
#' @param animal_type Type of animal (e.g., "dairy_cattle", "pigs")
#' @param config_paths List of paths to configuration files
#' @param debug_mode Enable debug mode for more verbose output
#' @return List of all emission factors for the animal type
compile_emission_factors = function(animal_type, 
                                   config_paths = list(
                                     mms_NH3 = system.file("extdata/mms_NH3.yaml", package = "rEMEP"),
                                     storage_N2O = system.file("extdata/storage_N2O.yaml", package = "rEMEP"),
                                     storage_OTHERS = system.file("extdata/storage_OTHERS.yaml", package = "rEMEP"),
                                     digestate_NH3 = system.file("extdata/digestate_NH3.yaml", package = "rEMEP")
                                   ),
                                   debug_mode = FALSE) {
  
  if (debug_mode) {
    message(paste("Compiling emission factors for animal type:", animal_type))
  }
  
  # Load emission factors
  nh3_ef = load_NH3_emission_factors(animal_type, config_paths, debug_mode)
  n2o_ef = load_N2O_emission_factors(animal_type, config_paths, debug_mode)
  other_ef = load_OTHER_emission_factors(config_paths, debug_mode)
  digestate_ef = load_digestate_NH3_emission_factors(config_paths, debug_mode)
  
  # Organize emission factors by stage
  ef_by_stage = list(
    housing = list(NH3 = list()),
    yards = list(NH3 = NULL),  # For yards, we'll use a single value
    grazing = list(NH3 = NULL),  # For grazing, we'll use a single value
    storage = list(NH3 = list(), N2O = list(), NOx = list(), N2 = list()),
    application = list(NH3 = list()),
    digestate = list(NH3 = NULL)  # For digestate, we'll use a single value
  )
  
  # NH3 emission factors by stage
  if (!is.null(nh3_ef)) {
    # Housing - need to handle slurry and solid separately
    if (!is.null(nh3_ef$slurry) && !is.null(nh3_ef$slurry$housing)) {
      ef_by_stage$housing$NH3$slurry = nh3_ef$slurry$housing
    }
    
    if (!is.null(nh3_ef$solid) && !is.null(nh3_ef$solid$housing)) {
      ef_by_stage$housing$NH3$solid = nh3_ef$solid$housing
    }
    
    # Yards
    if (!is.null(nh3_ef$yards)) {
      ef_by_stage$yards$NH3 = nh3_ef$yards
    }
    
    # Grazing
    if (!is.null(nh3_ef$grazing)) {
      ef_by_stage$grazing$NH3 = nh3_ef$grazing
    }
    
    # Storage - need to handle slurry and solid separately
    if (!is.null(nh3_ef$slurry) && !is.null(nh3_ef$slurry$storage)) {
      ef_by_stage$storage$NH3$slurry = nh3_ef$slurry$storage
    }
    
    if (!is.null(nh3_ef$solid) && !is.null(nh3_ef$solid$storage)) {
      ef_by_stage$storage$NH3$solid = nh3_ef$solid$storage
    }
    
    # Application - need to handle slurry and solid separately
    if (!is.null(nh3_ef$slurry) && !is.null(nh3_ef$slurry$application)) {
      ef_by_stage$application$NH3$slurry = nh3_ef$slurry$application
    }
    
    if (!is.null(nh3_ef$solid) && !is.null(nh3_ef$solid$application)) {
      ef_by_stage$application$NH3$solid = nh3_ef$solid$application
    }
  }
  
  # N2O emission factors (storage only)
  if (!is.null(n2o_ef)) {
    ef_by_stage$storage$N2O = n2o_ef
  }
  
  # Other emission factors (NOx, N2) (storage only)
  if (!is.null(other_ef)) {
    if (!is.null(other_ef$slurry) && !is.null(other_ef$slurry$NOx)) {
      ef_by_stage$storage$NOx$slurry = other_ef$slurry$NOx
    }
    
    if (!is.null(other_ef$slurry) && !is.null(other_ef$slurry$N2)) {
      ef_by_stage$storage$N2$slurry = other_ef$slurry$N2
    }
    
    if (!is.null(other_ef$solid) && !is.null(other_ef$solid$NOx)) {
      ef_by_stage$storage$NOx$solid = other_ef$solid$NOx
    }
    
    if (!is.null(other_ef$solid) && !is.null(other_ef$solid$N2)) {
      ef_by_stage$storage$N2$solid = other_ef$solid$N2
    }
  }
  
  # Add digestate emission factor
  if (!is.null(digestate_ef)) {
    if (debug_mode) {
      message(paste("Adding digestate NH3 emission factor:", digestate_ef))
    }
    ef_by_stage$digestate$NH3 = digestate_ef
  }
  
  # Return the compiled emission factors
  return(ef_by_stage)
}
