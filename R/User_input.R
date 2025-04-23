#' @title User Input Class for Manure Management System
#' @description Class for validating user input parameters for livestock emission calculations
#' with a focus on feeding and housing components in manure management systems.
#'
#' The User_input class handles validation of all parameters needed for the manure management
#' workflow: animal excretion → housing/yards/grazing → storage → field application.
#' It ensures that input parameters are valid before they are used in calculations.
#'
#' @details This class validates the following parameters:
#' \itemize{
#'   \item Animal type and number
#'   \item Excretion coefficient
#'   \item Allocation fractions (grazing, yards, housing)
#'   \item TAN fraction
#'   \item Manure type fractions (slurry, solid)
#'   \item Bedding amount
#'   \item Manure usage fractions (storage and biogas for both slurry and solid)
#' }
#'
#' @examples
#' # Create a new User_input object for dairy cattle
#' input = User_input$new(
#'   animal_type = "dairy_cattle",
#'   animal_number = 100,
#'   excretion_coefficient = 105,
#'   fraction_grazing = 0.3,
#'   fraction_yards = 0.1,
#'   fraction_housing = 0.6,
#'   fraction_TAN = 0.6,
#'   fraction_manure_slurry = 0.7,
#'   fraction_manure_solid = 0.3,
#'   bedding_amount = 500,
#'   fraction_storage_slurry = 0.9,
#'   fraction_biogas_slurry = 0.1,
#'   fraction_storage_solid = 0.8,
#'   fraction_biogas_solid = 0.2
#' )
#'
#' # Check if the input is valid
#' if (input$valid) {
#'   # Get the parameters as a list for use in calculations
#'   params = input$get_parameters()
#'   # Use the parameters in your calculations
#' } else {
#'   # Print validation errors
#'   cat(input$get_validation_message_string())
#' }
#'
#' @importFrom utils menu
#' @importFrom R6 R6Class
NULL

#' User_input Class for Manure Management System
#'
#' @description An R6 class to validate user input for manure management simulations
#' with a focus on feeding and housing components. This class only handles input validation
#' and does not run any calculations.
#'
#' @export
User_input = R6Class("User_input",
  public = list(
    #' @field animal_type Character string representing the animal type (e.g., "dairy_cattle", "pigs")
    animal_type = NULL,
    
    #' @field animal_number Number of animals (head/yr)
    animal_number = NULL,
    
    #' @field excretion_coefficient Excretion coefficient (kg N/(head.yr)). If NULL, default values will be used.
    excretion_coefficient = NULL,
    
    #' @field fraction_grazing Fraction of time spent grazing [0-1]. Must sum to 1 with yards and housing.
    fraction_grazing = NULL,

    #' @field fraction_yards Fraction of time spent in yards [0-1]. Must sum to 1 with grazing and housing.
    fraction_yards = NULL,

    #' @field fraction_housing Fraction of time spent in housing [0-1]. Must sum to 1 with grazing and yards.
    fraction_housing = NULL,
    
    #' @field fraction_TAN Fraction of total N excreted as TAN [0-1], typically 0.5-0.7
    fraction_TAN = NULL,
    
    #' @field fraction_manure_slurry Fraction of manure handled as slurry [0-1]. Must sum to 1 with solid.
    fraction_manure_slurry = NULL,
    
    #' @field fraction_manure_solid Fraction of manure handled as solid [0-1]. Must sum to 1 with slurry.
    fraction_manure_solid = NULL,
    
    #' @field bedding_amount Bedding amount (kg/(head.yr)). Required when solid manure is used.
    bedding_amount = NULL,
    
    #' @field fraction_storage_slurry Fraction of slurry going to storage [0-1]. Must not exceed 1 with biogas.
    fraction_storage_slurry = NULL,
    
    #' @field fraction_biogas_slurry Fraction of slurry going to biogas [0-1]. Must not exceed 1 with storage.
    fraction_biogas_slurry = NULL,
    
    #' @field fraction_storage_solid Fraction of solid manure going to storage [0-1]. Must not exceed 1 with biogas.
    fraction_storage_solid = NULL,
    
    #' @field fraction_biogas_solid Fraction of solid manure going to biogas [0-1]. Must not exceed 1 with storage.
    fraction_biogas_solid = NULL,
    
    #' @field valid Boolean indicating if all input parameters are valid
    valid = FALSE,
    
    #' @field validation_messages List of validation error messages if any parameters are invalid
    validation_messages = NULL,
    
    #' @field config_paths Paths to configuration files
    config_paths = list(
      livestock_class_conversion = "inst/extdata/livestock_class_conversion.yaml",
      livestock_excretion = "inst/extdata/livestock_excretion.yaml",
      straw_litter = "inst/extdata/straw_litter.yaml"
    ),
    
    #' @field main_animal_type The main animal type category from livestock_class_conversion.yaml
    main_animal_type = NULL,
    
    #' @description Create a new User_input object and validate the input parameters
    #' @param animal_type Character string representing the animal type (e.g., "dairy_cattle", "pigs")
    #' @param animal_number Number of animals (head/yr)
    #' @param excretion_coefficient Excretion coefficient (kg N/(head.yr)). If NULL, default values will be used.
    #' @param fraction_grazing Fraction of time spent grazing [0-1]. Must sum to 1 with yards and housing.
    #' @param fraction_yards Fraction of time spent in yards [0-1]. Must sum to 1 with grazing and housing.
    #' @param fraction_housing Fraction of time spent in housing [0-1]. Must sum to 1 with grazing and yards.
    #' @param fraction_TAN Fraction of total N excreted as TAN [0-1], typically 0.5-0.7
    #' @param fraction_manure_slurry Fraction of manure handled as slurry [0-1]. Must sum to 1 with solid.
    #' @param fraction_manure_solid Fraction of manure handled as solid [0-1]. Must sum to 1 with slurry.
    #' @param bedding_amount Bedding amount (kg/(head.yr)). Required when solid manure is used.
    #' @param fraction_storage_slurry Fraction of slurry going to storage [0-1]. Must not exceed 1 with biogas.
    #' @param fraction_biogas_slurry Fraction of slurry going to biogas [0-1]. Must not exceed 1 with storage.
    #' @param fraction_storage_solid Fraction of solid manure going to storage [0-1]. Must not exceed 1 with biogas.
    #' @param fraction_biogas_solid Fraction of solid manure going to biogas [0-1]. Must not exceed 1 with storage.
    #' @return A new User_input object with validation results
    initialize = function(animal_type,
                          animal_number,
                          excretion_coefficient = NULL,
                          fraction_grazing = NULL,
                          fraction_yards = NULL,
                          fraction_housing = NULL,
                          fraction_TAN = NULL,
                          fraction_manure_slurry,
                          fraction_manure_solid,
                          bedding_amount = NULL,
                          fraction_storage_slurry,
                          fraction_biogas_slurry,
                          fraction_storage_solid,
                          fraction_biogas_solid) {
      
      # Store the input parameters
      self$animal_type = animal_type
      self$animal_number = animal_number
      
      # Initialize validation messages
      self$validation_messages = list()
      
      # Determine the main animal type category
      self$get_main_animal_type()
      
      # Fill in missing parameters from configuration files
      self$fill_default_values()
      
      # Override defaults with provided values if they're not NULL
      if (!is.null(excretion_coefficient)) self$excretion_coefficient = excretion_coefficient
      if (!is.null(fraction_grazing)) self$fraction_grazing = fraction_grazing
      if (!is.null(fraction_yards)) self$fraction_yards = fraction_yards
      if (!is.null(fraction_housing)) self$fraction_housing = fraction_housing
      if (!is.null(fraction_TAN)) self$fraction_TAN = fraction_TAN
      if (!is.null(bedding_amount)) self$bedding_amount = bedding_amount
      
      # Store remaining parameters
      self$fraction_manure_slurry = fraction_manure_slurry
      self$fraction_manure_solid = fraction_manure_solid
      self$fraction_storage_slurry = fraction_storage_slurry
      self$fraction_biogas_slurry = fraction_biogas_slurry
      self$fraction_storage_solid = fraction_storage_solid
      self$fraction_biogas_solid = fraction_biogas_solid
      
      # Validate the input parameters
      self$validate()
    },
    
    #' @description Validate all input parameters for the manure management system
    #' Checks that all fractions sum to 1 where appropriate and that all values are within valid ranges.
    #' @return TRUE if all parameters are valid, FALSE otherwise
    validate = function() {
      # Reset validation status
      self$valid = TRUE
      self$validation_messages = list()
      
      # Validate animal_type
      if (!is.character(self$animal_type) || length(self$animal_type) != 1) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, 
                                   "ERROR: animal_type must be a single character string.")
      }
      
      # Validate animal_number
      if (!is.numeric(self$animal_number) || self$animal_number <= 0) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, 
                                   "ERROR: animal_number must be a positive number.")
      }
      
      # Check if allocation fractions sum to 1
      allocation_sum = self$fraction_grazing + self$fraction_yards + self$fraction_housing
      if (abs(allocation_sum - 1) > 0.001) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, 
                                   paste0("ERROR: Allocation fractions (grazing + yards + housing) should sum to 1. ",
                                         "Current sum is ", round(allocation_sum, 3), "."))
      }
      
      # Check if manure type fractions sum to 1
      man_type_sum = self$fraction_manure_solid + self$fraction_manure_slurry
      if (abs(man_type_sum - 1) > 0.001) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, 
                                   paste0("ERROR: Manure type fractions (slurry + solid) should sum to 1. ",
                                         "Current sum is ", round(man_type_sum, 3), "."))
      }
      
      # Check if TAN fraction is between 0 and 1
      if (!is.numeric(self$fraction_TAN) || self$fraction_TAN < 0 || self$fraction_TAN > 1) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, 
                                   "ERROR: fraction_TAN must be a number between 0 and 1.")
      }
      
      # Check if manure usage fractions are valid
      slurry_usage_sum = self$fraction_storage_slurry + self$fraction_biogas_slurry
      if (slurry_usage_sum > 1) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, 
                                   paste0("ERROR: Sum of slurry usage fractions exceeds 1. ",
                                         "Current sum is ", round(slurry_usage_sum, 3), "."))
      }
      
      solid_usage_sum = self$fraction_storage_solid + self$fraction_biogas_solid
      if (solid_usage_sum > 1) {
        self$valid = FALSE
        self$validation_messages = c(self$validation_messages, 
                                   paste0("ERROR: Sum of solid manure usage fractions exceeds 1. ",
                                         "Current sum is ", round(solid_usage_sum, 3), "."))
      }
      return(self$valid)
    },
    
    #' @description Get all validation error messages as a single formatted string
    #' @return A string containing all validation error messages, or a success message if all parameters are valid
    get_validation_message_string = function() {
      if (length(self$validation_messages) == 0) {
        return("All input parameters are valid.")
      } else {
        return(paste(self$validation_messages, collapse = "\n"))
      }
    },
    
    #' @description Print a formatted summary of all input parameters and validation results
    #' Shows all parameter values and indicates whether they are valid or not.
    #' @return Invisibly returns the User_input object for method chaining
    print = function() {
      cat("=== User Input Parameters ===\n")
      cat("Animal type:", self$animal_type, "\n")
      cat("Number of animals:", self$animal_number, "head/yr\n")
      cat("Excretion coefficient:", self$excretion_coefficient, "kg N/(head.yr)\n")
      cat("Allocation fractions:\n")
      cat("  - Grazing:", self$fraction_grazing, "\n")
      cat("  - Yards:", self$fraction_yards, "\n")
      cat("  - Housing:", self$fraction_housing, "\n")
      cat("TAN fraction:", self$fraction_TAN, "\n")
      cat("Manure type fractions:\n")
      cat("  - Slurry:", self$fraction_manure_slurry, "\n")
      cat("  - Solid:", self$fraction_manure_solid, "\n")
      if (self$fraction_manure_solid > 0) {
        cat("Bedding amount:", self$bedding_amount, "kg/(head.yr)\n")
      }
      cat("Manure usage fractions:\n")
      cat("  - Storage (slurry):", self$fraction_storage_slurry, "\n")
      cat("  - Biogas (slurry):", self$fraction_biogas_slurry, "\n")
      cat("  - Storage (solid):", self$fraction_storage_solid, "\n")
      cat("  - Biogas (solid):", self$fraction_biogas_solid, "\n")
      
      cat("\n=== Validation Results ===\n")
      if (self$valid) {
        cat("All input parameters are valid.\n")
      } else {
        cat("Input parameters are invalid. Please fix the following issues:\n")
        cat(self$get_validation_message_string(), "\n")
      }
      
      return(invisible(self))
    },
    
    #' @description Get all input parameters as a structured list for use in calculations
    #' The list format matches the expected structure for manure management calculations.
    #' @return A nested list containing all input parameters organized by category
    get_parameters = function() {
      if (!self$valid) {
        warning("Input parameters are invalid. Please check validation messages.")
      }
      
      return(list(
        animal_type = self$animal_type,
        animal_number = self$animal_number,
        excretion_coefficient = self$excretion_coefficient,
        fraction_allocation = list(
          grazing = self$fraction_grazing,
          yards = self$fraction_yards,
          housing = self$fraction_housing
        ),
        fraction_TAN = self$fraction_TAN,
        fraction_man_type = list(
          slurry = self$fraction_manure_solid,
          solid = self$fraction_manure_slurry
        ),
        bedding_amount = self$bedding_amount,
        fraction_man_usage = list(
          storage = list(
            slurry = self$fraction_storage_slurry,
            solid = self$fraction_storage_solid
          ),
          biogas = list(
            slurry = self$fraction_biogas_slurry,
            solid = self$fraction_biogas_solid
          )
        )
      ))
    },
    
    #' @description Determine the main animal type category from livestock_class_conversion.yaml
    #' Maps specific animal types to main categories (e.g., 'dairy_cattle_tied' -> 'dairy_cattle')
    #' @return The main animal type category or the original animal type if not found
    get_main_animal_type = function() {
      tryCatch({
        # Load the livestock class conversion file
        conversion_file = system.file(self$config_paths$livestock_class_conversion, package = "rEMEP")
        if (conversion_file == "") {
          conversion_file = self$config_paths$livestock_class_conversion
        }
        
        conversions = yaml::read_yaml(conversion_file)
        
        # Check if animal_type is a main category
        if (self$animal_type %in% names(conversions$livestock_conversions)) {
          self$main_animal_type = self$animal_type
          return(self$main_animal_type)
        }
        
        # Check if animal_type is a subtype
        for (type in names(conversions$livestock_conversions)) {
          subtypes = conversions$livestock_conversions[[type]]
          
          # With proper YAML format, subtypes will be a list
          if (self$animal_type %in% subtypes) {
            self$main_animal_type = type
            return(self$main_animal_type)
          }
        }
        
        # If not found, use the original animal type
        warning(paste("Animal type", self$animal_type, "not found in livestock_class_conversion.yaml. Using as is."))
        self$main_animal_type = self$animal_type
        return(self$main_animal_type)
      }, error = function(e) {
        warning(paste("Error determining main animal type:", e$message))
        self$main_animal_type = self$animal_type
        return(self$main_animal_type)
      })
    },
    
    #' @description Fill in missing parameters with default values from configuration files
    #' @return Invisibly returns the User_input object for method chaining
    fill_default_values = function() {
      tryCatch({
        # Load the livestock excretion file for excretion coefficient, TAN fraction, and allocation fractions
        excretion_file = system.file(self$config_paths$livestock_excretion, package = "rEMEP")
        if (excretion_file == "") {
          excretion_file = self$config_paths$livestock_excretion
        }
        
        excretion_data = yaml::read_yaml(excretion_file)
        
        # First try with the specific animal type
        if (self$animal_type %in% names(excretion_data$livestock_excretion)) {
          self$load_defaults_for_animal(excretion_data$livestock_excretion[[self$animal_type]])
        } 
        # If not found, try with the main animal type
        else if (!is.null(self$main_animal_type) && 
                 self$main_animal_type %in% names(excretion_data$livestock_excretion)) {
          self$load_defaults_for_animal(excretion_data$livestock_excretion[[self$main_animal_type]])
        }
        
        # Load the straw litter file for bedding amount
        if (is.null(self$bedding_amount)) {
          self$load_bedding_amount()
        }
        
        return(invisible(self))
      }, error = function(e) {
        warning(paste("Error loading defaults:", e$message))
        return(invisible(self))
      })
    },
    
    #' @description Load bedding amount from straw_litter.yaml
    #' @return Invisibly returns the User_input object for method chaining
    load_bedding_amount = function() {
      tryCatch({
        # Load the straw litter file
        straw_file = system.file(self$config_paths$straw_litter, package = "rEMEP")
        if (straw_file == "") {
          straw_file = self$config_paths$straw_litter
        }
        
        straw_data = yaml::read_yaml(straw_file)
        
        # Check if animal_type exists in the straw data
        animal_key = NULL
        
        # First try with the specific animal type
        if (self$animal_type %in% names(straw_data$livestock_categories)) {
          animal_key = self$animal_type
        }
        # If not found, try with the main animal type
        else if (!is.null(self$main_animal_type) && 
                 self$main_animal_type %in% names(straw_data$livestock_categories)) {
          animal_key = self$main_animal_type
        }
        # Special case for sheep (singular/plural difference)
        else if (self$animal_type == "sheep" && "sheeps" %in% names(straw_data$livestock_categories)) {
          animal_key = "sheeps"
        }
        else if (self$main_animal_type == "sheep" && "sheeps" %in% names(straw_data$livestock_categories)) {
          animal_key = "sheeps"
        }
        
        # If we found a matching key, load the bedding amount
        if (!is.null(animal_key) && 
            "straw_kg_per_AAP_per_year" %in% names(straw_data$livestock_categories[[animal_key]])) {
          self$bedding_amount = straw_data$livestock_categories[[animal_key]]$straw_kg_per_AAP_per_year
        }
        
        return(invisible(self))
      }, error = function(e) {
        warning(paste("Error loading bedding amount:", e$message))
        return(invisible(self))
      })
    },
    
    #' @description Load default values for a specific animal from the data
    #' @param animal_data List of default values for the animal
    #' @return Invisibly returns the User_input object for method chaining
    load_defaults_for_animal = function(animal_data) {
      # Step 2: Fill in excretion_coefficient if missing
      if (is.null(self$excretion_coefficient) && "Nex_kg_head" %in% names(animal_data)) {
        self$excretion_coefficient = animal_data$Nex_kg_head
      }
      
      # Step 3: Fill in fraction_TAN if missing
      if (is.null(self$fraction_TAN) && "TAN_proportion" %in% names(animal_data)) {
        self$fraction_TAN = animal_data$TAN_proportion
      }
      
      # Step 4: Fill in allocation fractions if missing
      if (is.null(self$fraction_grazing) && "fraction_grazing" %in% names(animal_data)) {
        self$fraction_grazing = animal_data$fraction_grazing
      }
      
      if (is.null(self$fraction_yards) && "fraction_yards" %in% names(animal_data)) {
        self$fraction_yards = animal_data$fraction_yards
      }
      
      if (is.null(self$fraction_housing) && "fraction_housing" %in% names(animal_data)) {
        self$fraction_housing = animal_data$fraction_housing
      }
      
      # Step 5: Fill in bedding amount if missing
      # Currently not implemented as we need to know which config file contains bedding amounts
      
      return(invisible(self))
    }
  )
)
