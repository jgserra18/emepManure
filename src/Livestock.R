#' @title Animal Class Module
#' @description This module provides the Animal class for handling different animal types
#' and retrieving emission factors for solid/manure from configuration files.
#' @details The Animal class is used to standardize animal types and retrieve 
#' emission factors from various configuration files.

# Load required libraries
library(yaml)
library(R6)

# Source utility functions
source(file.path(getwd(), "src/utils/animal_utils.R"))

#' @title Animal Class
#' @description Class that handles different animal types and retrieves emission factors
#' for solid/manure from the configuration files.
#' @export
Animal = R6Class("Animal",
  public = list(
    #' @field animal_type The type of animal
    animal_type = NULL,
    
    #' @field standardized_type The standardized animal type after conversion
    standardized_type = NULL,
    
    #' @field config_dir The directory containing configuration files
    config_dir = NULL,
    
    #' @field emission_data A list containing all emission data for the animal
    emission_data = NULL,
    
    #' @description Initialize a new Animal object
    #' @param animal_type The type of animal (e.g., dairy_cattle_tied, dairy_cattle)
    #' @param config_dir The directory containing configuration files (default: "config")
    #' @return A new Animal object
    initialize = function(animal_type, config_dir = file.path(getwd(), "config")) {
      self$animal_type = animal_type
      self$config_dir = config_dir
      self$standardized_type = .standardize_animal_type(animal_type, config_dir)
      self$emission_data = self$load_emission_data()
    },
    
    #' @description Load all emission data for the animal from configuration files
    #' @return A list containing all emission data
    load_emission_data = function() {
      data = list()
      
      # Load NH3 emission factors
      nh3_file = file.path(self$config_dir, "mms_NH3.yaml")
      if (file.exists(nh3_file)) {
        nh3_data = yaml::read_yaml(nh3_file)
        data$nh3 = .extract_animal_data(nh3_data$MMS_NH3_EF, self$animal_type, 
                                       self$standardized_type, self$config_dir)
      }
      
      # Load N2O emission factors
      n2o_file = file.path(self$config_dir, "storage_N2O.yaml")
      if (file.exists(n2o_file)) {
        n2o_data = yaml::read_yaml(n2o_file)
        data$n2o = .extract_animal_data(n2o_data$storage_N2O_EF, self$animal_type, 
                                       self$standardized_type, self$config_dir)
      }
      
      # Load livestock excretion data
      excretion_file = file.path(self$config_dir, "livestock_excretion.yaml")
      if (file.exists(excretion_file)) {
        excretion_data = yaml::read_yaml(excretion_file)
        data$excretion = .extract_animal_data(excretion_data$livestock_excretion, 
                                             self$animal_type, self$standardized_type, 
                                             self$config_dir)
      }
      
      # Load straw/litter data
      straw_file = file.path(self$config_dir, "straw_litter.yaml")
      if (file.exists(straw_file)) {
        straw_data = yaml::read_yaml(straw_file)
        data$straw = .extract_animal_data(straw_data$livestock_categories, 
                                         self$animal_type, self$standardized_type, 
                                         self$config_dir)
      }
      
      # Load other storage emission factors if available
      others_file = file.path(self$config_dir, "storage_OTHERS.yaml")
      if (file.exists(others_file)) {
        others_data = yaml::read_yaml(others_file)
        data$others = .extract_animal_data(others_data, self$animal_type, 
                                          self$standardized_type, self$config_dir)
      }
      
      return(data)
    },
    
    #' @description Get emission factors for solid manure
    #' @return A list of emission factors for solid manure
    get_solid_emission_factors = function() {
      solid_factors = list()
      
      # Extract NH3 emission factors for solid manure
      if (!is.null(self$emission_data$nh3) && !is.null(self$emission_data$nh3$solid)) {
        solid_factors$nh3 = self$emission_data$nh3$solid
      }
      
      # Extract N2O emission factors for solid manure systems
      if (!is.null(self$emission_data$n2o)) {
        # Filter for solid storage systems
        solid_n2o = list()
        for (system_name in names(self$emission_data$n2o)) {
          if (grepl("solid", system_name, ignore.case = TRUE)) {
            solid_n2o[[system_name]] = self$emission_data$n2o[[system_name]]
          }
        }
        if (length(solid_n2o) > 0) {
          solid_factors$n2o = solid_n2o
        }
      }
      
      # Add other relevant data for solid manure
      if (!is.null(self$emission_data$excretion)) {
        solid_factors$excretion = self$emission_data$excretion
      }
      
      if (!is.null(self$emission_data$straw)) {
        solid_factors$straw = self$emission_data$straw
      }
      
      return(solid_factors)
    },
    
    #' @description Get emission factors for liquid/slurry manure
    #' @return A list of emission factors for liquid/slurry manure
    get_slurry_emission_factors = function() {
      slurry_factors = list()
      
      # Extract NH3 emission factors for slurry
      if (!is.null(self$emission_data$nh3) && !is.null(self$emission_data$nh3$slurry)) {
        slurry_factors$nh3 = self$emission_data$nh3$slurry
      }
      
      # Extract N2O emission factors for slurry systems
      if (!is.null(self$emission_data$n2o)) {
        # Filter for slurry storage systems
        slurry_n2o = list()
        for (system_name in names(self$emission_data$n2o)) {
          if (grepl("slurry", system_name, ignore.case = TRUE)) {
            slurry_n2o[[system_name]] = self$emission_data$n2o[[system_name]]
          }
        }
        if (length(slurry_n2o) > 0) {
          slurry_factors$n2o = slurry_n2o
        }
      }
      
      # Add other relevant data for slurry
      if (!is.null(self$emission_data$excretion)) {
        slurry_factors$excretion = self$emission_data$excretion
      }
      
      return(slurry_factors)
    },
    
    #' @description Get all emission factors for the animal
    #' @return A list of all emission factors
    get_all_emission_factors = function() {
      return(self$emission_data)
    },
    
    #' @description Print a summary of the animal data
    #' @return None
    print = function() {
      cat("Animal:", self$animal_type, "\n")
      cat("Standardized type:", self$standardized_type, "\n")
      cat("Data available:\n")
      
      if (!is.null(self$emission_data$nh3)) {
        cat("- NH3 emission factors\n")
      }
      if (!is.null(self$emission_data$n2o)) {
        cat("- N2O emission factors\n")
      }
      if (!is.null(self$emission_data$excretion)) {
        cat("- Excretion data\n")
      }
      if (!is.null(self$emission_data$straw)) {
        cat("- Straw/litter data\n")
      }
      if (!is.null(self$emission_data$others)) {
        cat("- Other emission factors\n")
      }
    }
  )
)