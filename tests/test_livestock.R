#' Unit tests for Livestock module
#'
#' This file contains unit tests for the Livestock module using the testthat package.

library(testthat)
library(yaml)
library(R6)

# Set working directory to the project root for consistent file paths
if (!grepl("EMEP_MMS$", getwd())) {
  # Try to find and set the project root directory
  possible_root = file.path(getwd(), "EMEP_MMS")
  if (dir.exists(possible_root)) {
    setwd(possible_root)
  } else {
    stop("Please run tests from the project root directory (EMEP_MMS)")
  }
}

# Source the files to test
source("src/utils/animal_utils.R")
source("src/Livestock.R")

# Create a test context
context("Livestock module tests")

# Test standardize_animal_type function
test_that(".standardize_animal_type correctly standardizes animal types", {
  # Test with a standard type
  expect_equal(.standardize_animal_type("dairy_cattle", "config"), "dairy_cattle")
  
  # Test with a non-standard type that should be converted
  expect_equal(.standardize_animal_type("dairy_cattle_tied", "config"), "dairy_cattle")
  
  # Test with a bird type
  expect_equal(.standardize_animal_type("laying_hens", "config"), "birds")
  
  # Test with an unknown type (should return the original type with a warning)
  expect_warning(.standardize_animal_type("unknown_animal", "config"))
  expect_equal(suppressWarnings(.standardize_animal_type("unknown_animal", "config")), "unknown_animal")
})

# Test extract_animal_data function
test_that(".extract_animal_data correctly extracts data for animal types", {
  # Create a mock data structure
  mock_data = list(
    dairy_cattle = list(value = "dairy_cattle_value"),
    birds = list(value = "birds_value")
  )
  
  # Test with direct match
  expect_equal(.extract_animal_data(mock_data, "dairy_cattle", "dairy_cattle", "config")$value, 
               "dairy_cattle_value")
  
  # Test with standardized match
  expect_equal(.extract_animal_data(mock_data, "dairy_cattle_tied", "dairy_cattle", "config")$value, 
               "dairy_cattle_value")
  
  # Test with bird category
  expect_equal(.extract_animal_data(mock_data, "laying_hens", "birds", "config")$value, 
               "birds_value")
  
  # Test with no match
  expect_null(.extract_animal_data(mock_data, "unknown_animal", "unknown_animal", "config"))
})

# Test Animal class initialization
test_that("Animal class initializes correctly", {
  # Create an Animal object
  animal = Animal$new("dairy_cattle")
  
  # Check that the animal_type is set correctly
  expect_equal(animal$animal_type, "dairy_cattle")
  
  # Check that the standardized_type is set correctly
  expect_equal(animal$standardized_type, "dairy_cattle")
  
  # Check that the config_dir is set correctly
  expect_equal(animal$config_dir, file.path(getwd(), "config"))
  
  # Check that emission_data is a list
  expect_true(is.list(animal$emission_data))
})

# Test get_solid_emission_factors method with focus on housing
test_that("get_solid_emission_factors returns correct housing data", {
  # Create an Animal object
  animal = Animal$new("dairy_cattle")
  
  # Get solid emission factors
  solid_factors = animal$get_solid_emission_factors()
  
  # Check that solid_factors is a list
  expect_true(is.list(solid_factors))
  
  # If NH3 data is available, check that it contains housing emission factor
  if (!is.null(solid_factors$nh3)) {
    expect_true("housing" %in% names(solid_factors$nh3))
    expect_true(is.numeric(solid_factors$nh3$housing))
  }
  
  # Check for straw/litter data (related to feeding/housing)
  if (!is.null(solid_factors$straw)) {
    expect_true("straw_kg_per_AAP_per_year" %in% names(solid_factors$straw))
    expect_true("nitrogen_added_in_straw_kg_per_AAP_per_year" %in% names(solid_factors$straw))
  }
})

# Test get_slurry_emission_factors method with focus on housing
test_that("get_slurry_emission_factors returns correct housing data", {
  # Create an Animal object
  animal = Animal$new("dairy_cattle")
  
  # Get slurry emission factors
  slurry_factors = animal$get_slurry_emission_factors()
  
  # Check that slurry_factors is a list
  expect_true(is.list(slurry_factors))
  
  # If NH3 data is available, check that it contains housing emission factor
  if (!is.null(slurry_factors$nh3)) {
    expect_true("housing" %in% names(slurry_factors$nh3))
    expect_true(is.numeric(slurry_factors$nh3$housing))
  }
  
  # Check for excretion data (related to housing systems)
  if (!is.null(slurry_factors$excretion)) {
    expect_true("housing_period_days" %in% names(slurry_factors$excretion))
    expect_true("Nex_kg_head" %in% names(slurry_factors$excretion))
    expect_true("TAN_proportion" %in% names(slurry_factors$excretion))
  }
})

# Test get_all_emission_factors method
test_that("get_all_emission_factors returns all emission data", {
  # Create an Animal object
  animal = Animal$new("dairy_cattle")
  
  # Get all emission factors
  all_factors = animal$get_all_emission_factors()
  
  # Check that all_factors is a list
  expect_true(is.list(all_factors))
  
  # Check that it contains expected keys
  expect_true(any(c("nh3", "n2o", "excretion", "straw") %in% names(all_factors)))
})

# Test set_livestock_parameters function
test_that("set_livestock_parameters creates an Animal object", {
  # Create an Animal object using set_livestock_parameters
  animal = set_livestock_parameters("dairy_cattle")
  
  # Check that it's an Animal object
  expect_true("Animal" %in% class(animal))
  
  # Check that the animal_type is set correctly
  expect_equal(animal$animal_type, "dairy_cattle")
})

# Tests will be automatically executed when this file is sourced
# The results will be displayed in the R console
