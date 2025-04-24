#' @title Manure Management System (MMS) Class
#' @description Class for calculating emissions from livestock manure management systems
#' based on user input parameters and emission factors from configuration files.
#'
#' The MMS class handles the entire calculation workflow:
#' animal excretion → housing/yards/grazing → storage → field application.
#' It loads appropriate emission factors based on animal type and performs calculations.
#'
#' @details This class performs calculations for:
#' \itemize{
#'   \item Manure excretion and allocation to different systems
#'   \item Emissions from housing, yards, and grazing
#'   \item Storage emissions and transformations
#'   \item Biogas/digestate handling
#'   \item Field application emissions
#' }
#'
#' @examples
#' # Create a User_input object
#' input = User_input$new(
#'   animal_type = "dairy_cattle",
#'   animal_number = 100,
#'   fraction_manure_slurry = 0.7,
#'   fraction_manure_solid = 0.3,
#'   fraction_storage_slurry = 0.9,
#'   fraction_biogas_slurry = 0.1,
#'   fraction_storage_solid = 0.8,
#'   fraction_biogas_solid = 0.2
#' )
#'
#' # Create MMS object and run calculations
#' mms = MMS$new(input)
#' results = mms$run_simulation()
#'
#' @importFrom yaml read_yaml
#' @importFrom R6 R6Class
#' @importFrom utils menu
#' @importFrom stats setNames

# Import functions from other files
# These functions are defined in the package and will be available when the package is loaded
NULL

#' MMS Class for Manure Management System
#'
#' @description An R6 class to perform manure management calculations based on User_input
#' parameters and emission factors from configuration files.
#'
#' @export
MMS = R6Class("MMS",
  public = list(
    #' @field user_input User_input object containing input parameters
    user_input = NULL,
    
    #' @field config_paths Paths to configuration files
    config_paths = list(
      mms_NH3 = "inst/extdata/mms_NH3.yaml",
      storage_N2O = "inst/extdata/storage_N2O.yaml",
      storage_OTHERS = "inst/extdata/storage_OTHERS.yaml",
      digestate_NH3 = "inst/extdata/digestate_NH3.yaml",
      global_parameters = "inst/extdata/global_parameters.yaml"
    ),
    
    #' @field debug_mode Enable debug mode for more verbose output
    debug_mode = TRUE,
    
    #' @field emission_factors List of emission factors loaded from configuration files
    emission_factors = NULL,
    
    #' @field global_params Global parameters loaded from configuration files
    global_params = NULL,
    
    #' @field results Results of the simulation
    results = NULL,
    
    #' @field EF_by_stage Emission factors organized by stage
    EF_by_stage = NULL,
    
    #' @description Create a new MMS object and load emission factors
    #' @param user_input User_input object containing input parameters
    #' @description Initialize a new MMS object
    #' @param user_input User_input object
    #' @param debug Enable debug mode (default: FALSE)
    initialize = function(user_input, debug = FALSE) {
      self$user_input = user_input
      self$emission_factors = list()
      self$global_params = list()
      self$EF_by_stage = list()
      self$debug_mode = debug
      
      if (self$debug_mode) {
        message("Initializing MMS object...")
        message(paste("Animal type:", user_input$animal_type))
      }
      
      # Load emission factors
      self$load_global_parameters()
      self$compile_emission_factors()
      
      if (self$debug_mode) {
        message("MMS object initialized successfully.")
      }
      
      invisible(self)
    },
    
    #' @description Compile emission factors from various sources
    #' @return Invisibly returns the MMS object for method chaining
    compile_emission_factors = function() {
      if (self$debug_mode) {
        message("Compiling emission factors...")
      }
      
      # Load emission factors
      self$emission_factors$NH3 = self$load_NH3_emission_factors()
      self$emission_factors$N2O = self$load_N2O_emission_factors()
      self$emission_factors$OTHER = self$load_OTHER_emission_factors()
      self$emission_factors$digestate = self$load_digestate_emission_factors()
      
      # Organize emission factors by stage
      self$EF_by_stage = list(
        housing = list(),
        storage = list(),
        application = list()
      )
      
      if (self$debug_mode) {
        message("Organizing emission factors by stage...")
      }
      
      # NH3 emission factors by stage
      if (!is.null(self$emission_factors$NH3)) {
        if (self$debug_mode) {
          message("Processing NH3 emission factors...")
        }
        
        if (!is.null(self$emission_factors$NH3$housing)) {
          self$EF_by_stage$housing$NH3 = self$emission_factors$NH3$housing
          if (self$debug_mode) {
            message("Added NH3 housing emission factors.")
          }
        }
        if (!is.null(self$emission_factors$NH3$storage)) {
          self$EF_by_stage$storage$NH3 = self$emission_factors$NH3$storage
          if (self$debug_mode) {
            message("Added NH3 storage emission factors.")
          }
        }
        if (!is.null(self$emission_factors$NH3$application)) {
          self$EF_by_stage$application$NH3 = self$emission_factors$NH3$application
          if (self$debug_mode) {
            message("Added NH3 application emission factors.")
          }
        }
      } else {
        warning("NH3 emission factors not loaded.")
      }
      
      # N2O emission factors by stage
      if (!is.null(self$emission_factors$N2O)) {
        if (self$debug_mode) {
          message("Processing N2O emission factors...")
        }
        
        # N2O emission factors are organized differently
        # They are directly in the N2O structure, not in a 'storage' sublist
        # So we need to add them directly to the storage stage
        self$EF_by_stage$storage$N2O = self$emission_factors$N2O
        if (self$debug_mode) {
          message("Added N2O storage emission factors.")
          message(paste("Available N2O emission factor types:", paste(names(self$emission_factors$N2O), collapse = ", ")))
        }
      } else {
        warning("N2O emission factors not loaded.")
      }
      
      # Other emission factors by stage
      if (!is.null(self$emission_factors$OTHER)) {
        if (self$debug_mode) {
          message("Processing OTHER emission factors...")
        }
        
        # Structure is different for OTHER emission factors
        # They are organized by manure type (slurry/solid) first, then by gas type
        
        # Initialize storage NO and N2 structures if they don't exist
        if (is.null(self$EF_by_stage$storage$NO)) {
          self$EF_by_stage$storage$NO = list()
        }
        
        if (is.null(self$EF_by_stage$storage$N2)) {
          self$EF_by_stage$storage$N2 = list()
        }
        
        # Process slurry emission factors
        if (!is.null(self$emission_factors$OTHER$slurry)) {
          if (!is.null(self$emission_factors$OTHER$slurry$NO)) {
            self$EF_by_stage$storage$NO$slurry = self$emission_factors$OTHER$slurry$NO
            if (self$debug_mode) {
              message("Added NO storage emission factors for slurry.")
            }
          }
          
          if (!is.null(self$emission_factors$OTHER$slurry$N2)) {
            self$EF_by_stage$storage$N2$slurry = self$emission_factors$OTHER$slurry$N2
            if (self$debug_mode) {
              message("Added N2 storage emission factors for slurry.")
            }
          }
        }
        
        # Process solid emission factors
        if (!is.null(self$emission_factors$OTHER$solid)) {
          if (!is.null(self$emission_factors$OTHER$solid$NO)) {
            self$EF_by_stage$storage$NO$solid = self$emission_factors$OTHER$solid$NO
            if (self$debug_mode) {
              message("Added NO storage emission factors for solid.")
            }
          }
          
          if (!is.null(self$emission_factors$OTHER$solid$N2)) {
            self$EF_by_stage$storage$N2$solid = self$emission_factors$OTHER$solid$N2
            if (self$debug_mode) {
              message("Added N2 storage emission factors for solid.")
            }
          }
        }
      } else {
        warning("OTHER emission factors not loaded.")
      }
      
      # Digestate emission factors
      if (!is.null(self$emission_factors$digestate)) {
        if (self$debug_mode) {
          message("Processing digestate emission factors...")
        }
        
        self$EF_by_stage$digestate = self$emission_factors$digestate
        if (self$debug_mode) {
          message("Added digestate emission factors.")
        }
      } else {
        warning("Digestate emission factors not loaded.")
      }
      
      if (self$debug_mode) {
        message("Emission factors compiled successfully.")
      }
      
      return(invisible(self))
    },
    
    #' @description Load NH3 emission factors from mms_NH3.yaml
    #' @return List of NH3 emission factors
    load_NH3_emission_factors = function() {
      # Get animal type from user input
      animal_type = self$user_input$animal_type
      
      # Try both package and direct file path
      nh3_file = system.file(self$config_paths$mms_NH3, package = "rEMEP")
      if (nh3_file == "") {
        nh3_file = self$config_paths$mms_NH3
        if (self$debug_mode) {
          message(paste("Using direct file path for NH3 emission factors:", nh3_file))
        }
      } else {
        if (self$debug_mode) {
          message(paste("Using package file path for NH3 emission factors:", nh3_file))
        }
      }
      
      # Check if file exists
      if (!file.exists(nh3_file)) {
        warning(paste("NH3 emission factors file not found:", nh3_file))
        return(NULL)
      }
      
      tryCatch({
        if (self$debug_mode) {
          message("Reading NH3 emission factors file...")
        }
        
        nh3_data = yaml::read_yaml(nh3_file)
        
        if (self$debug_mode) {
          message("NH3 emission factors file read successfully.")
          message(paste("Available animal types:", paste(names(nh3_data$MMS_NH3_EF), collapse = ", ")))
        }
        
        # Try to find emission factors for the specific animal type
        if (animal_type %in% names(nh3_data$MMS_NH3_EF)) {
          if (self$debug_mode) {
            message(paste("Found NH3 emission factors for animal type:", animal_type))
          }
          return(nh3_data$MMS_NH3_EF[[animal_type]])
        }
        
        # If not found, try with the main animal type
        main_animal_type = NULL
        if ("get_main_animal_type" %in% names(self$user_input)) {
          main_animal_type = self$user_input$get_main_animal_type()
          if (self$debug_mode) {
            message(paste("Using main animal type:", main_animal_type))
          }
        }
        
        if (!is.null(main_animal_type) && main_animal_type %in% names(nh3_data$MMS_NH3_EF)) {
          if (self$debug_mode) {
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
    },
    
    #' @description Load N2O emission factors from storage_N2O.yaml
    #' @return List of N2O emission factors
    load_N2O_emission_factors = function() {
      # Get animal type from user input
      animal_type = self$user_input$animal_type
      
      # Try both package and direct file path
      n2o_file = system.file(self$config_paths$storage_N2O, package = "rEMEP")
      if (n2o_file == "") {
        n2o_file = self$config_paths$storage_N2O
        if (self$debug_mode) {
          message(paste("Using direct file path for N2O emission factors:", n2o_file))
        }
      } else {
        if (self$debug_mode) {
          message(paste("Using package file path for N2O emission factors:", n2o_file))
        }
      }
      
      # Check if file exists
      if (!file.exists(n2o_file)) {
        warning(paste("N2O emission factors file not found:", n2o_file))
        return(NULL)
      }
      
      tryCatch({
        if (self$debug_mode) {
          message("Reading N2O emission factors file...")
        }
        
        n2o_data = yaml::read_yaml(n2o_file)
        
        if (self$debug_mode) {
          message("N2O emission factors file read successfully.")
          message(paste("Available animal types:", paste(names(n2o_data$storage_N2O_EF), collapse = ", ")))
        }
        
        # Try to find emission factors for the specific animal type
        if (animal_type %in% names(n2o_data$storage_N2O_EF)) {
          if (self$debug_mode) {
            message(paste("Found N2O emission factors for animal type:", animal_type))
          }
          
          # Get the appropriate emission factors based on slurry_crust parameter
          ef_data = n2o_data$storage_N2O_EF[[animal_type]]
          
          # For cattle and pigs, we need to select the appropriate slurry EF based on crust presence
          if (animal_type %in% c("dairy_cattle", "cattle", "pigs")) {
            # Check if user has specified slurry_crust parameter
            has_crust = self$user_input$slurry_crust
            
            if (self$debug_mode) {
              message(paste("Slurry crust present:", ifelse(has_crust, "Yes", "No")))
            }
            
            # Select the appropriate slurry type based on crust presence
            if (has_crust && "slurry_with_crust" %in% names(ef_data)) {
              if (self$debug_mode) {
                message("Using emission factors for slurry with crust")
              }
            } else if (!has_crust && "slurry_without_crust" %in% names(ef_data)) {
              if (self$debug_mode) {
                message("Using emission factors for slurry without crust")
              }
            }
          }
          
          return(ef_data)
        }
        
        # If not found, try with the main animal type
        main_animal_type = NULL
        if ("get_main_animal_type" %in% names(self$user_input)) {
          main_animal_type = self$user_input$get_main_animal_type()
          if (self$debug_mode) {
            message(paste("Using main animal type:", main_animal_type))
          }
        }
        
        if (!is.null(main_animal_type) && main_animal_type %in% names(n2o_data$storage_N2O_EF)) {
          if (self$debug_mode) {
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
    },
    
    #' @description Load other emission factors (NO, N2) from storage_OTHERS.yaml
    #' @return List of other emission factors
    load_OTHER_emission_factors = function() {
      # Try both package and direct file path
      others_file = system.file(self$config_paths$storage_OTHERS, package = "rEMEP")
      if (others_file == "") {
        others_file = self$config_paths$storage_OTHERS
        if (self$debug_mode) {
          message(paste("Using direct file path for OTHER emission factors:", others_file))
        }
      } else {
        if (self$debug_mode) {
          message(paste("Using package file path for OTHER emission factors:", others_file))
        }
      }
      
      # Check if file exists
      if (!file.exists(others_file)) {
        warning(paste("OTHER emission factors file not found:", others_file))
        # Try with absolute path
        absolute_path = file.path(getwd(), self$config_paths$storage_OTHERS)
        if (self$debug_mode) {
          message(paste("Trying absolute path:", absolute_path))
        }
        if (file.exists(absolute_path)) {
          others_file = absolute_path
          if (self$debug_mode) {
            message(paste("Found file at absolute path:", others_file))
          }
        } else {
          if (self$debug_mode) {
            message(paste("File not found at absolute path either:", absolute_path))
            message(paste("Current working directory:", getwd()))
            message(paste("Checking if directory exists:", dirname(absolute_path), ", exists:", dir.exists(dirname(absolute_path))))
          }
          return(NULL)
        }
      }
      
      tryCatch({
        if (self$debug_mode) {
          message("Reading OTHER emission factors file...")
        }
        
        others_data = yaml::read_yaml(others_file)
        
        if (self$debug_mode) {
          message("OTHER emission factors file read successfully.")
          message(paste("YAML structure keys:", paste(names(others_data), collapse = ", ")))
          if (!is.null(others_data$storage_OTHERS_EF)) {
            message(paste("Available manure types:", paste(names(others_data$storage_OTHERS_EF), collapse = ", ")))
          }
        }
        
        # These emission factors are universal across animal types
        if (!is.null(others_data$storage_OTHERS_EF)) {
          if (self$debug_mode) {
            message("Found OTHER emission factors.")
          }
          return(others_data$storage_OTHERS_EF)
        } else {
          warning("Other emission factors not found in the expected format")
          if (self$debug_mode) {
            message(paste("YAML content structure:", paste(capture.output(str(others_data)), collapse = "\n")))
          }
          return(NULL)
        }
      }, error = function(e) {
        warning(paste("Error loading other emission factors:", e$message))
        return(NULL)
      })
    },
    
    #' @description Load digestate emission factors from digestate_NH3.yaml
    #' @return List of digestate emission factors
    load_digestate_emission_factors = function() {
      # Try both package and direct file path
      digestate_file = system.file(self$config_paths$digestate_NH3, package = "rEMEP")
      if (digestate_file == "") {
        digestate_file = self$config_paths$digestate_NH3
        if (self$debug_mode) {
          message(paste("Using direct file path for digestate emission factors:", digestate_file))
        }
      } else {
        if (self$debug_mode) {
          message(paste("Using package file path for digestate emission factors:", digestate_file))
        }
      }
      
      # Check if file exists
      if (!file.exists(digestate_file)) {
        warning(paste("Digestate emission factors file not found:", digestate_file))
        return(NULL)
      }
      
      tryCatch({
        if (self$debug_mode) {
          message("Reading digestate emission factors file...")
        }
        
        digestate_data = yaml::read_yaml(digestate_file)
        
        if (self$debug_mode) {
          message("Digestate emission factors file read successfully.")
        }
        
        # Check for the correct key name
        if ("digestate_NH3_EF" %in% names(digestate_data)) {
          if (self$debug_mode) {
            message("Found digestate emission factors with key 'digestate_NH3_EF'.")
          }
          return(digestate_data$digestate_NH3_EF)
        } else if ("digestate_NH#_EF" %in% names(digestate_data)) {
          warning("Using deprecated key 'digestate_NH#_EF', please update to 'digestate_NH3_EF'")
          if (self$debug_mode) {
            message("Found digestate emission factors with deprecated key 'digestate_NH#_EF'.")
          }
          return(digestate_data$`digestate_NH#_EF`)
        } else {
          warning("Digestate emission factors not found in the expected format")
          return(NULL)
        }
      }, error = function(e) {
        warning(paste("Error loading digestate emission factors:", e$message))
        return(NULL)
      })
    },
    
    #' @description Load global parameters from global_parameters.yaml
    #' @return Invisibly returns the MMS object for method chaining
    load_global_parameters = function() {
      # Try both package and direct file path
      global_file = system.file(self$config_paths$global_parameters, package = "rEMEP")
      if (global_file == "") {
        global_file = self$config_paths$global_parameters
        if (self$debug_mode) {
          message(paste("Using direct file path for global parameters:", global_file))
        }
      } else {
        if (self$debug_mode) {
          message(paste("Using package file path for global parameters:", global_file))
        }
      }
      
      # Check if file exists
      if (!file.exists(global_file)) {
        warning(paste("Global parameters file not found:", global_file))
        return(invisible(self))
      }
      
      tryCatch({
        if (self$debug_mode) {
          message("Reading global parameters file...")
        }
        
        global_data = yaml::read_yaml(global_file)
        
        if (self$debug_mode) {
          message("Global parameters file read successfully.")
        }
        
        self$global_params = global_data$global_parameters
        
        if (self$debug_mode) {
          message(paste("Loaded global parameters:", 
                        paste(names(self$global_params), collapse = ", ")))
        }
        
        return(invisible(self))
      }, error = function(e) {
        warning(paste("Error loading global parameters:", e$message))
        return(invisible(self))
      })
    },
    
    #' @description Get NH3 emission factor for a specific manure type and stage
    #' @param manure_type Type of manure (slurry or solid)
    #' @param stage Stage in the manure management system (housing, yard, storage, application, grazing)
    #' @return Emission factor value or NULL if not found
    get_NH3_EF = function(manure_type, stage) {
      if (is.null(self$emission_factors$NH3)) {
        warning("NH3 emission factors not loaded")
        return(NULL)
      }
      
      if (manure_type %in% names(self$emission_factors$NH3)) {
        if (stage %in% names(self$emission_factors$NH3[[manure_type]])) {
          return(self$emission_factors$NH3[[manure_type]][[stage]])
        }
      }
      
      warning(paste("NH3 emission factor not found for", manure_type, "in stage", stage))
      return(NULL)
    },
    

    
    #' @description Get other emission factor (NO, N2) for a specific manure type
    #' @param manure_type Type of manure (slurry or solid)
    #' @param gas Type of gas (NO or N2)
    #' @return Emission factor value or NULL if not found
    get_OTHER_EF = function(manure_type, gas) {
      if (is.null(self$emission_factors$OTHER)) {
        warning("Other emission factors not loaded")
        return(NULL)
      }
      
      if (manure_type %in% names(self$emission_factors$OTHER)) {
        if (gas %in% names(self$emission_factors$OTHER[[manure_type]])) {
          return(self$emission_factors$OTHER[[manure_type]][[gas]])
        }
      }
      
      warning(paste("Emission factor for", gas, "not found for", manure_type))
      return(NULL)
    },
    
    #' @description Get digestate emission factor
    #' @return Emission factor value or NULL if not found
    get_digestate_EF = function() {
      if (is.null(self$emission_factors$digestate)) {
        warning("Digestate emission factors not loaded")
        return(NULL)
      }
      
      if ("digestate" %in% names(self$emission_factors$digestate)) {
        return(self$emission_factors$digestate$digestate)
      }
      
      warning("Digestate emission factor not found")
      return(NULL)
    },
    
    #' @description Get global parameter value
    #' @param param_name Name of the parameter (f_imm, f_min, f_min_digester)
    #' @return Parameter value or NULL if not found
    get_global_param = function(param_name) {
      if (is.null(self$global_params)) {
        warning("Global parameters not loaded")
        return(NULL)
      }
      
      if (param_name %in% names(self$global_params)) {
        return(self$global_params[[param_name]])
      }
      
      warning(paste("Global parameter", param_name, "not found"))
      return(NULL)
    },
    
    #' @description Store emission factors in results for reference
    #' @return List of emission factors and global parameters
    store_emission_factors = function() {
      # Extract user input parameters for easier access
      input = self$user_input
      
      # Get emission factors for different stages from the organized EF_by_stage
      # Store emission factors in results for reference
      self$results$emission_factors = self$EF_by_stage
      
      # Store global parameters
      self$results$global_params = self$global_params
      
      # Return the results
      return(self$results)
    },
    

    
    #' @description Get N2O emission factor based on manure type and slurry crust presence
    #' @param manure_type Type of manure ("slurry" or "solid")
    #' @return Emission factor value or NULL if not found
    get_N2O_EF = function(manure_type) {
      if (is.null(self$emission_factors$N2O)) {
        warning("N2O emission factors not loaded")
        return(NULL)
      }
      
      ef_data = self$emission_factors$N2O
      animal_type = self$user_input$animal_type
      has_crust = self$user_input$slurry_crust
      
      if (self$debug_mode) {
        message(paste("Getting N2O EF for", animal_type, "and manure type:", manure_type))
        if (manure_type == "slurry") {
          message(paste("Slurry crust present:", ifelse(has_crust, "Yes", "No")))
        }
      }
      
      # Handle solid manure
      if (manure_type == "solid") {
        if ("solid_manure_heaps" %in% names(ef_data) && 
            "EF" %in% names(ef_data$solid_manure_heaps)) {
          ef = ef_data$solid_manure_heaps$EF
          if (self$debug_mode) {
            message(paste("Using N2O EF for solid manure heaps:", ef))
          }
          return(ef)
        }
      }
      
      # Handle slurry based on crust presence for cattle and pigs
      if (manure_type == "slurry") {
        if (animal_type %in% c("dairy_cattle", "cattle", "pigs")) {
          # With crust
          if (has_crust && "slurry_with_crust" %in% names(ef_data) && 
              "EF" %in% names(ef_data$slurry_with_crust)) {
            ef = ef_data$slurry_with_crust$EF
            if (self$debug_mode) {
              message(paste("Using N2O EF for slurry with crust:", ef))
            }
            return(ef)
          }
          # Without crust
          else if (!has_crust && "slurry_without_crust" %in% names(ef_data) && 
                   "EF" %in% names(ef_data$slurry_without_crust)) {
            ef = ef_data$slurry_without_crust$EF
            if (self$debug_mode) {
              message(paste("Using N2O EF for slurry without crust:", ef))
            }
            return(ef)
          }
          # Default to without crust if specific option not found
          else if ("slurry_without_crust" %in% names(ef_data) && 
                   "EF" %in% names(ef_data$slurry_without_crust)) {
            ef = ef_data$slurry_without_crust$EF
            if (self$debug_mode) {
              message(paste("Using default N2O EF for slurry without crust:", ef))
            }
            return(ef)
          }
        }
        # For other animal types, just use slurry_without_crust
        else if ("slurry_without_crust" %in% names(ef_data) && 
                 "EF" %in% names(ef_data$slurry_without_crust)) {
          ef = ef_data$slurry_without_crust$EF
          if (self$debug_mode) {
            message(paste("Using N2O EF for slurry without crust:", ef))
          }
          return(ef)
        }
      }
      
      warning(paste("N2O emission factor not found for manure type:", manure_type))
      return(NULL)
    },
    
    #' @description Get emission factors for other gases (N2, NO)
    #' @param animal_type Animal type
    #' @return Emission factors for other gases
      
    },
    
    #' @description Get emission factors for other gases (N2, NO)
    #' @param animal_type Animal type
    #' @return Emission factors for other gases
    get_OTHER_EF = function(animal_type) {
      if (self$debug_mode) {
        message(paste("Getting OTHER emission factors for animal type:", animal_type))
      }
      
      # Check if emission factors are loaded
      if (is.null(self$emission_factors$OTHER)) {
        warning("OTHER emission factors not loaded")
        return(NULL)
      }
      
      # Get the main animal type
      main_animal_type = self$user_input$get_main_animal_type()
      
      # Try to get emission factors for the specific animal type
      ef_data = self$emission_factors$OTHER[[animal_type]]
      
      # If not found, try to get emission factors for the main animal type
      if (is.null(ef_data) && !is.null(main_animal_type)) {
        ef_data = self$emission_factors$OTHER[[main_animal_type]]
      }
      
      # If still not found, try to get default emission factors
      if (is.null(ef_data)) {
        ef_data = self$emission_factors$OTHER$default
      }
      
      # If emission factors are found, return them
      if (!is.null(ef_data)) {
        return(ef_data)
      }
      
      warning(paste("OTHER emission factors not found for animal type:", animal_type))
      return(NULL)
      
      # Get housing emission factors
      ef_housing_slurry = 0
      ef_housing_solid = 0
      
      if (!is.null(self$emission_factors$NH3$housing) && 
          !is.null(self$emission_factors$NH3$housing$slurry)) {
        ef_housing_slurry = self$emission_factors$NH3$housing$slurry
      }
      
      if (!is.null(self$emission_factors$NH3$housing) && 
          !is.null(self$emission_factors$NH3$housing$solid)) {
        ef_housing_solid = self$emission_factors$NH3$housing$solid
      }
      
      # Calculate emissions
      housing_slurry_NH3 = housing_slurry_TAN * ef_housing_slurry
      housing_solid_NH3 = housing_solid_TAN * ef_housing_solid
      housing_total_NH3 = housing_slurry_NH3 + housing_solid_NH3
      
      if (self$debug_mode) {
        message(paste("Housing NH3 emissions from slurry:", round(housing_slurry_NH3, 3), "kg N"))
        message(paste("Housing NH3 emissions from solid:", round(housing_solid_NH3, 3), "kg N"))
        message(paste("Total housing NH3 emissions:", round(housing_total_NH3, 3), "kg N"))
      }
      
      return(list(
        slurry = housing_slurry_NH3,
        solid = housing_solid_NH3,
        total = housing_total_NH3
      ))
    },
    
    #' @description Calculate NH3 emissions from manure storage
    #' @return NH3 emissions in kg N
    calculate_storage_NH3_emissions = function() {
      if (self$debug_mode) {
        message("Calculating storage NH3 emissions...")
      }
      
      # Get user input parameters
      input = self$user_input
      
      # Calculate TAN in storage
      # First calculate housing emissions to get TAN flows
      housing_emissions = self$calculate_housing_NH3_emissions()
      
      # Calculate excretion values
      exc_total_N = input$animal_number * input$excretion_coefficient
      exc_housing_N = exc_total_N * input$fraction_housing
      exc_housing_TAN = exc_housing_N * input$fraction_TAN
      exc_yards_N = exc_total_N * input$fraction_yard
      exc_yards_TAN = exc_yards_N * input$fraction_TAN
      
      # Calculate TAN deposited as slurry and solid
      housing_slurry_TAN = exc_housing_TAN * input$fraction_manure_slurry
      housing_solid_TAN = exc_housing_TAN * input$fraction_manure_solid
      housing_slurry_N = exc_housing_N * input$fraction_manure_slurry
      housing_solid_N = exc_housing_N * input$fraction_manure_solid
      
      # For simplicity, assume yard emissions are 0 for now
      yard_NH3 = 0
      
      # Calculate storage TAN and N
      storage_slurry_TAN = ((housing_slurry_TAN - housing_emissions$slurry) + 
                           (exc_yards_TAN - yard_NH3)) * input$fraction_storage_slurry
      
      storage_slurry_N = ((housing_slurry_N - housing_emissions$slurry) + 
                          (exc_yards_N - yard_NH3)) * input$fraction_storage_slurry
      
      # Get f_min from global parameters (default to 0.1 if not available)
      f_min = 0.1
      if (!is.null(self$global_params) && !is.null(self$global_params$f_min)) {
        f_min = self$global_params$f_min
      }
      
    },
    
    #' @description Print the MMS object
    #' @return Invisible self
    print = function() {
      cat("MMS object\n")
      cat("User input:\n")
      self$user_input$print()
      cat("\nEmission factors loaded:\n")
      cat("NH3: ", ifelse(!is.null(self$emission_factors$NH3), "Yes", "No"), "\n")
      cat("N2O: ", ifelse(!is.null(self$emission_factors$N2O), "Yes", "No"), "\n")
      cat("OTHER: ", ifelse(!is.null(self$emission_factors$OTHER), "Yes", "No"), "\n")
      cat("Digestate: ", ifelse(!is.null(self$emission_factors$digestate), "Yes", "No"), "\n")
      cat("Global parameters: ", ifelse(!is.null(self$global_params), "Yes", "No"), "\n")
      
      invisible(self)
    },
    
    #' @description Run the inventory calculation
    #' @return List of results
    run_inventory = function() {
      if (self$debug_mode) {
        message("Running inventory calculation...")
      }
      
      # Initialize results structure
      results = list(
        excretion = list(),
        grazing = list(),
        yards = list(),
        housing = list(),
        storage = list(),
        digestate = list(),
        application = list()
      )
      
      if (self$debug_mode) {
        message("Initialized results list.")
      }
      
      # Get user input parameters
      input = self$user_input
      
      # Manure excreted by livestock, allocated to different pathways
      Exc_total_N = livestock_excretion(animal_no = input$animal_no, exc_coef = input$excretion_coefficient)
      
      Exc_grazing_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_grazing)
      Exc_yards_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_yards)
      Exc_housing_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_housing)
      
      Exc_grazing_TAN = allocate_TAN_excretion(exc_allocation = Exc_grazing_N, f_TAN = input$fraction_TAN)
      Exc_yards_TAN = allocate_TAN_excretion(exc_allocation = Exc_yards_N, f_TAN = input$fraction_TAN)
      Exc_housing_TAN = allocate_TAN_excretion(exc_allocation = Exc_housing_N, f_TAN = input$fraction_TAN)
      
      # Store excretion results
      results$excretion = list(
        total_N = Exc_total_N,
        grazing_N = Exc_grazing_N,
        yards_N = Exc_yards_N,
        housing_N = Exc_housing_N,
        grazing_TAN = Exc_grazing_TAN,
        yards_TAN = Exc_yards_TAN,
        housing_TAN = Exc_housing_TAN
      )
      if (self$debug_mode) { message("Excretion calculated.") }
    
      # Calculate yards emissions
      if (self$debug_mode) { message("Calculating yards NH3 emissions...") }

      # Get yards emission factors
      yards_ef = self$get_NH3_EF("yards", NULL, input$animal_type)
      
      # Calculate yards emissions
      Yards_emNH3 = yards_NH3(yards_tan = Exc_yards_TAN, EF = yards_ef)
      
      # Store yards results
      results$yards = list(
        NH3_N = Yards_emNH3,
        NH3 = Yards_emNH3 * 17/14  # Convert NH3-N to NH3
      )
      if (self$debug_mode) {  message("Yards NH3 emissions calculated.")}
      
      # Calculate grazing emissions
      if (self$debug_mode) {message("Calculating grazing NH3 emissions...")}
      
      # Get grazing emission factors
      grazing_ef = self$get_NH3_EF("grazing", NULL, input$animal_type)
      
      # Calculate grazing emissions
      Grazing_emNH3 = grazing_NH3(grazing_tan = Exc_grazing_TAN, EF = grazing_ef)
      
      # Calculate net grazing
      netGrazing_TAN = net_grazing(manure_grazing = Exc_grazing_TAN, grazing_NH3 = Grazing_emNH3)
      netGrazing_N = net_grazing(manure_grazing = Exc_grazing_N, grazing_NH3 = Grazing_emNH3)
      
      # Store grazing results
      results$grazing = list(
        NH3_N = Grazing_emNH3,
        NH3 = Grazing_emNH3 * 17/14,  # Convert NH3-N to NH3
        net_TAN = netGrazing_TAN,
        net_N = netGrazing_N
      )
      
      if (self$debug_mode) { message("Grazing NH3 emissions calculated.") }
      
      # Calculate housing emissions
      if (self$debug_mode) { message("Calculating housing emissions...") }
      
      # Get housing emission factors
      housing_ef = self$get_NH3_EF("housing", NULL, input$animal_type)
      
      # Calculate housing deposited
      Housing_slurry_TAN = housing_deposited(housing_nutrient = Exc_housing_TAN, f_man_type = input$fraction_manure_slurry)
      Housing_slurry_N = housing_deposited(housing_nutrient = Exc_housing_N, f_man_type = input$fraction_manure_slurry)
      
      Housing_solid_TAN = housing_deposited(housing_nutrient = Exc_housing_TAN, f_man_type = input$fraction_manure_solid)
      Housing_solid_N = housing_deposited(housing_nutrient = Exc_housing_N, f_man_type = input$fraction_manure_solid)
      
      # Calculate housing emissions
      Housing_slurry_emNH3 = housing_NH3(housing_tan = Housing_slurry_TAN, EF = housing_ef$slurry)
      Housing_solid_emNH3 = housing_NH3(housing_tan = Housing_solid_TAN, EF = housing_ef$solid)
      
      # Housing bedding (solid manure only)
      
      # Get f_min from global parameters (default to 0.1 if not available)
      f_imm = 0.0067
      if (!is.null(self$global_params) && !is.null(self$global_params$f_imm)) {
        f_imm = self$global_params$f_imm
      }
      
      Housing_ex_TAN = ex_housing_solid_TAN(housing_solid_TAN = Housing_solid_TAN, 
                                            housing_solid_NH3 = Housing_solid_emNH3, 
                                            animal_no = input$animal_number, 
                                            bedding_amount = input$bedding_amount, 
                                            f_imm = f_imm) 
      Housing_ex_N = ex_housing_solid_N(housing_solid_N = Housing_solid_N, 
                                        housing_solid_NH3 = Housing_solid_emNH3, 
                                        animal_no = input$animal_number, 
                                        straw_N = input$bedding_N) 
      

      # Store housing results
      results$housing = list(
        slurry = list(
          TAN = Housing_slurry_TAN,
          N = Housing_slurry_N,
          NH3 = Housing_slurry_emNH3
        ),
        solid = list(
          TAN = Housing_solid_TAN,
          N = Housing_solid_N,
          Housing_ex_TAN = Housing_ex_TAN,
          Housing_ex_N = Housing_ex_N,
          NH3_N = Housing_solid_emNH3
        ),
        total_NH3 = Housing_slurry_emNH3 + Housing_solid_emNH3
        )
      )
      if (self$debug_mode) {message("Housing emissions calculated.") }
      
  
      # Calculate storage emissions
      if (self$debug_mode) {message("Calculating storage emissions...") }
    
      # Calculate storage TAN and N
      Storage_solid_TAN = storage_solid(ex_housing_solid = Housing_ex_TAN, f_man_usage_solid = input$fraction_storage_solid)
      Storage_solid_N = storage_solid(ex_housing_solid = Housing_ex_N, f_man_usage_solid = input$fraction_storage_solid)
      
      Storage_slurry_TAN = storage_slurry(housing_slurry = Housing_slurry_TAN, 
                                          housing_slurry_NH3 = Housing_slurry_emNH3, 
                                          yard = Exc_yards_TAN, 
                                          yard_NH3 = Yards_emNH3,
                                          f_man_usage_slurry = input$fraction_storage_slurry)
      Storage_slurry_N = storage_slurry(housing_slurry = Housing_slurry_N, 
                                        housing_slurry_NH3 = Housing_slurry_emNH3, 
                                        yard = Exc_yards_N, 
                                        yard_NH3 = Yards_emNH3, 
                                        f_man_usage_slurry = input$fraction_storage_slurry)
  
      # Get f_min from global parameters (default to 0.1 if not available)
      f_min = 0.1
      if (!is.null(self$global_params) && !is.null(self$global_params$f_min)) {
        f_min = self$global_params$f_min
      }
      
      # Update storage slurry TAN to account for mineralization
      updStorage_slurry_TAN = updated_storage_slurry(out_storage_slurry_TAN = Storage_slurry_TAN, 
                                                     out_storage_slurry_N = Storage_slurry_N, 
                                                     f_min = f_min) 
      
      # Get emission factors for storage
      storage_NH3_ef = self$get_NH3_EF("storage", NULL, input$animal_type)
      storage_N2O_ef = self$get_N2O_EF(input$animal_type, input$slurry_crust)
      
      # Get emission factors for N2, NO, and other gases
      storage_OTHER_ef = self$get_OTHER_EF(input$animal_type)
      
      # Calculate storage emissions for slurry
      Storage_solid_emNH3 = storage_emissions(storage_TAN = Storage_solid_TAN, 
                                              EF_NH3 = storage_NH3_ef$solid,
                                              EF_N2O = storage_N2O_ef$solid, 
                                              EF_NO = storage_OTHER_ef$solid$NO, 
                                              EF_N2 = storage_OTHER_ef$solid$N2) 
      
      Storage_slurry_emN2O = Updated_storage_slurry_TAN * storage_N2O_ef$slurry
      Storage_slurry_emNO = Updated_storage_slurry_TAN * storage_OTHER_ef$slurry$NO
      Storage_slurry_emN2 = Updated_storage_slurry_TAN * storage_OTHER_ef$slurry$N2
      
      # Calculate storage emissions for solid manure
      Storage_solid_emNH3 = storage_emissions(storage_tan = Storage_solid_TAN, 
                                            EF = storage_NH3_ef$solid)
      
      Storage_solid_emN2O = Storage_solid_TAN * storage_N2O_ef$solid
      Storage_solid_emNO = Storage_solid_TAN * storage_OTHER_ef$solid$NO
      Storage_solid_emN2 = Storage_solid_TAN * storage_OTHER_ef$solid$N2
      
      # Calculate TAN and N after storage emissions
      Storage_slurry_TAN_after = Updated_storage_slurry_TAN - Storage_slurry_emNH3 - 
                                Storage_slurry_emN2O - Storage_slurry_emNO - Storage_slurry_emN2
      
      Storage_solid_TAN_after = Storage_solid_TAN - Storage_solid_emNH3 - 
                               Storage_solid_emN2O - Storage_solid_emNO - Storage_solid_emN2
      
      # Store storage results
      results$storage = list(
        slurry = list(
          TAN = Updated_storage_slurry_TAN,
          N = Storage_slurry_N,
          NH3_N = Storage_slurry_emNH3,
          NH3 = Storage_slurry_emNH3 * 17/14,
          N2O_N = Storage_slurry_emN2O,
          N2O = Storage_slurry_emN2O * 44/28,
          NO_N = Storage_slurry_emNO,
          NO = Storage_slurry_emNO * 30/14,
          N2 = Storage_slurry_emN2,
          TAN_after_storage = Storage_slurry_TAN_after
        ),
        solid = list(
          TAN = Storage_solid_TAN,
          N = Storage_solid_N,
          NH3_N = Storage_solid_emNH3,
          NH3 = Storage_solid_emNH3 * 17/14,
          N2O_N = Storage_solid_emN2O,
          N2O = Storage_solid_emN2O * 44/28,
          NO_N = Storage_solid_emNO,
          NO = Storage_solid_emNO * 30/14,
          N2 = Storage_solid_emN2,
          TAN_after_storage = Storage_solid_TAN_after
        ),
        total = list(
          NH3_N = Storage_slurry_emNH3 + Storage_solid_emNH3,
          NH3 = (Storage_slurry_emNH3 + Storage_solid_emNH3) * 17/14,
          N2O_N = Storage_slurry_emN2O + Storage_solid_emN2O,
          N2O = (Storage_slurry_emN2O + Storage_solid_emN2O) * 44/28,
          NO_N = Storage_slurry_emNO + Storage_solid_emNO,
          NO = (Storage_slurry_emNO + Storage_solid_emNO) * 30/14,
          N2 = Storage_slurry_emN2 + Storage_solid_emN2
        )
      )
      
      if (self$debug_mode) {
        message("Storage emissions calculated.")
      }
      
      # Calculate digestate
      if (self$debug_mode) {
        message("Calculating digestate...")
      }
      
      # Calculate biogas TAN and N
      Biogas_slurry_TAN = ((Housing_slurry_TAN - Housing_slurry_emNH3) + 
                          (Exc_yards_TAN - Yards_emNH3)) * input$fraction_biogas_slurry
      
      Biogas_slurry_N = ((Housing_slurry_N - Housing_slurry_emNH3) + 
                         (Exc_yards_N - Yards_emNH3)) * input$fraction_biogas_slurry
      
      Biogas_solid_TAN = Housing_solid_TAN * input$fraction_biogas_solid
      Biogas_solid_N = Housing_solid_N * input$fraction_biogas_solid
      
      # Get digestate emission factors
      digestate_ef = NULL
      if (!is.null(self$emission_factors$digestate) && !is.null(self$emission_factors$digestate$NH3)) {
        digestate_ef = self$emission_factors$digestate$NH3
      } else {
        warning("Digestate emission factors not available. Using default values.")
        digestate_ef = list(slurry = 0.2, solid = 0.2)  # Default values
      }
      
      # Calculate digestate application TAN and N
      Digestate_application_TAN = digested_application_TAN(
        biogas_slurry_TAN = Biogas_slurry_TAN,
        biogas_solid_TAN = Biogas_solid_TAN
      )
      
      Digestate_application_N = digested_application_N(
        biogas_slurry_N = Biogas_slurry_N,
        biogas_solid_N = Biogas_solid_N
      )
      
      # Calculate digestate emissions
      Digestate_emNH3 = Digestate_application_TAN * digestate_ef$slurry  # Using slurry EF for digestate
      
      # Store digestate results
      results$digestate = list(
        TAN = Digestate_application_TAN,
        N = Digestate_application_N,
        NH3_N = Digestate_emNH3,
        NH3 = Digestate_emNH3 * 17/14,
        TAN_after_application = Digestate_application_TAN - Digestate_emNH3
      )
      
      if (self$debug_mode) {
        message("Digestate calculated.")
      }
      
      # Calculate application emissions
      if (self$debug_mode) {
        message("Calculating application NH3 emissions...")
      }
      
      # Get application emission factors
      application_ef = self$get_NH3_EF("application", NULL, input$animal_type)
      
      # Calculate application emissions for slurry
      Application_slurry_emNH3 = 0
      Application_slurry_TAN_after = 0
      
      if (input$fraction_storage_slurry > 0) {
        # Calculate emissions for different application methods
        for (method in names(input$f_man_usage$slurry)) {
          if (input$f_man_usage$slurry[[method]] > 0) {
            # Get method-specific emission factor
            ef_method = application_ef$slurry[[method]]
            
            if (!is.null(ef_method)) {
              # Calculate NH3-N emissions for this method
              method_emNH3 = Storage_slurry_TAN_after * input$f_man_usage$slurry[[method]] * ef_method
              Application_slurry_emNH3 = Application_slurry_emNH3 + method_emNH3
              
              # Calculate TAN after application for this method
              method_TAN_after = Storage_slurry_TAN_after * input$f_man_usage$slurry[[method]] * (1 - ef_method)
              Application_slurry_TAN_after = Application_slurry_TAN_after + method_TAN_after
            }
          }
        }
      }
      
      # Calculate application emissions for solid manure
      Application_solid_emNH3 = 0
      Application_solid_TAN_after = 0
      
      if (input$fraction_storage_solid > 0) {
        # Calculate emissions for different application methods
        for (method in names(input$f_man_usage$solid)) {
          if (input$f_man_usage$solid[[method]] > 0) {
            # Get method-specific emission factor
            ef_method = application_ef$solid[[method]]
            
            if (!is.null(ef_method)) {
              # Calculate NH3-N emissions for this method
              method_emNH3 = Storage_solid_TAN_after * input$f_man_usage$solid[[method]] * ef_method
              Application_solid_emNH3 = Application_solid_emNH3 + method_emNH3
              
              # Calculate TAN after application for this method
              method_TAN_after = Storage_solid_TAN_after * input$f_man_usage$solid[[method]] * (1 - ef_method)
              Application_solid_TAN_after = Application_solid_TAN_after + method_TAN_after
            }
          }
        }
      }
      
      # Store application results
      results$application = list(
        slurry = list(
          TAN_applied = Storage_slurry_TAN_after,
          NH3_N = Application_slurry_emNH3,
          NH3 = Application_slurry_emNH3 * 17/14,
          TAN_after_application = Application_slurry_TAN_after
        ),
        solid = list(
          TAN_applied = Storage_solid_TAN_after,
          NH3_N = Application_solid_emNH3,
          NH3 = Application_solid_emNH3 * 17/14,
          TAN_after_application = Application_solid_TAN_after
        ),
        total = list(
          NH3_N = Application_slurry_emNH3 + Application_solid_emNH3,
          NH3 = (Application_slurry_emNH3 + Application_solid_emNH3) * 17/14
        )
      )
      
      if (self$debug_mode) {
        message("Application NH3 emissions calculated.")
      }
      
      # Calculate total emissions across all stages
      total_NH3_N = results$grazing$NH3_N + results$yards$NH3_N + 
                   results$housing$total$NH3_N + results$storage$total$NH3_N + 
                   results$digestate$NH3_N + results$application$total$NH3_N
      
      total_NH3 = results$grazing$NH3 + results$yards$NH3 + 
                results$housing$total$NH3 + results$storage$total$NH3 + 
                results$digestate$NH3 + results$application$total$NH3
      
      total_N2O_N = results$storage$total$N2O_N
      total_N2O = results$storage$total$N2O
      
      total_NO_N = results$storage$total$NO_N
      total_NO = results$storage$total$NO
      
      total_N2 = results$storage$total$N2
      
      # Add totals to results
      results$total = list(
        NH3_N = total_NH3_N,
        NH3 = total_NH3,
        N2O_N = total_N2O_N,
        N2O = total_N2O,
        NO_N = total_NO_N,
        NO = total_NO,
        N2 = total_N2
      )
      
      if (self$debug_mode) {
        message("Inventory calculation completed.")
      }
      
      return(results)
    },
    
    #' @description Print a summary of the MMS object
    #' @return Invisibly returns the MMS object for method chaining
    print = function() {
      cat("=== Manure Management System (MMS) ===\n")
      cat("Animal type:", self$user_input$animal_type, "\n")
      cat("Number of animals:", self$user_input$animal_number, "head/yr\n\n")
      
      cat("=== Loaded Emission Factors ===\n")
      
      # NH3 emission factors
      nh3_loaded = !is.null(self$emission_factors$NH3)
      cat("NH3 emission factors loaded:", ifelse(nh3_loaded, "Yes", "No"))
      if (nh3_loaded) {
        stages = character(0)
        if (!is.null(self$emission_factors$NH3$housing)) stages = c(stages, "housing")
        if (!is.null(self$emission_factors$NH3$storage)) stages = c(stages, "storage")
        if (!is.null(self$emission_factors$NH3$application)) stages = c(stages, "application")
        if (length(stages) > 0) {
          cat(" (Stages: ", paste(stages, collapse = ", "), ")")
        }
      }
      cat("\n")
      
      # N2O emission factors
      n2o_loaded = !is.null(self$emission_factors$N2O)
      cat("N2O emission factors loaded:", ifelse(n2o_loaded, "Yes", "No"))
      if (n2o_loaded) {
        stages = character(0)
        if (!is.null(self$emission_factors$N2O$storage)) stages = c(stages, "storage")
        if (length(stages) > 0) {
          cat(" (Stages: ", paste(stages, collapse = ", "), ")")
        }
      }
      cat("\n")
      
      # Other emission factors
      other_loaded = !is.null(self$emission_factors$OTHER)
      cat("Other emission factors loaded:", ifelse(other_loaded, "Yes", "No"))
      if (other_loaded) {
        manure_types = character(0)
        gases = character(0)
        
        if (!is.null(self$emission_factors$OTHER$slurry)) {
          manure_types = c(manure_types, "slurry")
          if (!is.null(self$emission_factors$OTHER$slurry$NO)) gases = c(gases, "NO")
          if (!is.null(self$emission_factors$OTHER$slurry$N2)) gases = c(gases, "N2")
        }
        
        if (!is.null(self$emission_factors$OTHER$solid)) {
          manure_types = c(manure_types, "solid")
          if (!is.null(self$emission_factors$OTHER$solid$NO)) gases = unique(c(gases, "NO"))
          if (!is.null(self$emission_factors$OTHER$solid$N2)) gases = unique(c(gases, "N2"))
        }
        
        if (length(manure_types) > 0) {
          cat(" (Manure types: ", paste(manure_types, collapse = ", "), ")")
        }
        
        if (length(gases) > 0) {
          cat(" (Gases: ", paste(gases, collapse = ", "), ")")
        }
      }
      cat("\n")
      
      # Digestate emission factors
      digestate_loaded = !is.null(self$emission_factors$digestate)
      cat("Digestate emission factors loaded:", ifelse(digestate_loaded, "Yes", "No"))
      if (digestate_loaded && !is.null(names(self$emission_factors$digestate))) {
        cat(" (Types: ", paste(names(self$emission_factors$digestate), collapse = ", "), ")")
      }
      cat("\n\n")
      
      # Global parameters
      cat("=== Global Parameters ===\n")
      if (!is.null(self$global_params)) {
        if (!is.null(self$global_params$f_imm)) cat("f_imm:", self$global_params$f_imm, "\n")
        if (!is.null(self$global_params$f_min)) cat("f_min:", self$global_params$f_min, "\n")
        if (!is.null(self$global_params$f_min_digester)) cat("f_min_digester:", self$global_params$f_min_digester, "\n")
      } else {
        cat("No global parameters loaded.\n")
      }
      
      # File paths
      cat("\n=== Configuration File Paths ===\n")
      for (name in names(self$config_paths)) {
        path = self$config_paths[[name]]
        exists = file.exists(path)
        if (!exists) {
          # Try with absolute path
          abs_path = file.path(getwd(), path)
          exists = file.exists(abs_path)
          if (exists) {
            path = abs_path
          }
        }
        cat(name, ":", path, "(Exists:", exists, ")\n")
      }
      
      return(invisible(self))
    }
  )
)
