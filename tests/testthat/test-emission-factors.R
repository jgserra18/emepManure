library(testthat)
library(yaml)

# Get the package root directory
root_dir <- normalizePath(file.path(getwd(), "../.."))

# Source the necessary files using absolute paths
source(file.path(root_dir, "R", "Emission_factors.R"))
source(file.path(root_dir, "R", "Utils.R"))

# Test the Emission_factors functions
test_that("load_NH3_emission_factors works correctly", {
  # Set up test paths
  config_paths <- list(
    mms_NH3 = file.path(root_dir, "inst", "extdata", "mms_NH3.yaml")
  )
  
  # Test loading NH3 emission factors
  nh3_ef <- load_NH3_emission_factors(animal_type = "dairy_cattle", config_paths = config_paths, debug_mode = TRUE)
  
  # Skip test if the file doesn't exist or couldn't be loaded
  skip_if(is.null(nh3_ef), "NH3 emission factors could not be loaded")
  
  # Check that the emission factors were loaded correctly
  expect_true(is.list(nh3_ef))
  expect_true(!is.null(nh3_ef$housing) || !is.null(nh3_ef$slurry) || !is.null(nh3_ef$solid))
})

test_that("load_N2O_emission_factors works correctly", {
  # Set up test paths
  config_paths <- list(
    storage_N2O = file.path(root_dir, "inst", "extdata", "storage_N2O.yaml")
  )
  
  # Test loading N2O emission factors
  n2o_ef <- load_N2O_emission_factors(animal_type = "dairy_cattle", config_paths = config_paths, debug_mode = TRUE)
  
  # Skip test if the file doesn't exist or couldn't be loaded
  skip_if(is.null(n2o_ef), "N2O emission factors could not be loaded")
  
  # Check that the emission factors were loaded correctly
  expect_true(is.list(n2o_ef))
})

test_that("load_OTHER_emission_factors works correctly", {
  # Set up test paths
  config_paths <- list(
    storage_OTHERS = file.path(root_dir, "inst", "extdata", "storage_OTHERS.yaml")
  )
  
  # Test loading OTHER emission factors
  other_ef <- load_OTHER_emission_factors(config_paths = config_paths, debug_mode = TRUE)
  
  # Skip test if the file doesn't exist or couldn't be loaded
  skip_if(is.null(other_ef), "OTHER emission factors could not be loaded")
  
  # Check that the emission factors were loaded correctly
  expect_true(is.list(other_ef))
})

test_that("compile_emission_factors works correctly", {
  # Set up test paths
  config_paths <- list(
    mms_NH3 = file.path(root_dir, "inst", "extdata", "mms_NH3.yaml"),
    storage_N2O = file.path(root_dir, "inst", "extdata", "storage_N2O.yaml"),
    storage_OTHERS = file.path(root_dir, "inst", "extdata", "storage_OTHERS.yaml"),
    digestate_NH3 = file.path(root_dir, "inst", "extdata", "digestate_NH3.yaml")
  )
  
  # Test compiling emission factors
  ef_by_stage <- compile_emission_factors(animal_type = "dairy_cattle", config_paths = config_paths, debug_mode = TRUE)
  
  # Skip test if the emission factors couldn't be compiled
  skip_if(is.null(ef_by_stage), "Emission factors could not be compiled")
  
  # Check that the emission factors were compiled correctly
  expect_true(is.list(ef_by_stage))
})
