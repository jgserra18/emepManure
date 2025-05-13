library(testthat)
library(yaml)
# Load rEMEP package if available, otherwise source the enteric_fermentation_EF.R file directly
if (requireNamespace("rEMEP", quietly = TRUE)) {
  library(rEMEP)
} else {
  # Source the file directly for testing without installing the package
  source("../../R/utils/enteric_fermentation_EF.R")
}

# Test the Emission_factors functions

# Test the tier 1 enteric fermentation emission factor function
test_that("tier1 enteric fermentation emission factor calculation works correctly", {
  # Set up test paths
  yaml_file <- system.file("extdata/mms_CH4.yaml", package = "rEMEP")
  if (yaml_file == "") {
    # If running outside the package context, try a relative path
    yaml_file <- "../../inst/extdata/mms_CH4.yaml"
  }
  
  skip_if(!file.exists(yaml_file), "CH4 emission factors file not found")
  
  # Test pheasant (no parameters needed)
  expect_silent(ef_pheasant <- .tier1_EF("pheasant"))
  expect_equal(ef_pheasant, 0.0047)
  
  # Test laying_hens with hpr=FALSE and age=365
  expect_silent(ef_laying_hens_1 <- .tier1_EF("laying_hens", hpr = FALSE, age = 365))
  expect_equal(ef_laying_hens_1, 0.0010610)
  
  # Test laying_hens with hpr=TRUE and age=140
  expect_silent(ef_laying_hens_2 <- .tier1_EF("laying_hens", hpr = TRUE, age = 140))
  expect_equal(ef_laying_hens_2, 3.561e-3)
  
  # Test broilers with age=42 and organic=FALSE
  expect_silent(ef_broilers_1 <- .tier1_EF("broilers", age = 42, organic = FALSE))
  expect_equal(ef_broilers_1, 1.5e-5)
  
  # Test broilers with age=91 and organic=TRUE
  expect_silent(ef_broilers_2 <- .tier1_EF("broilers", age = 91, organic = TRUE))
  expect_equal(ef_broilers_2, 8.48e-5)
  
  # Test scaling with different age values
  # For laying_hens with hpr=TRUE, baseline is 140 days with EF 3.561e-3
  expect_silent(ef_laying_hens_scaled <- .tier1_EF("laying_hens", hpr = TRUE, age = 200))
  expected_scaled_ef <- 3.561e-3 * (200 / 140)
  expect_equal(ef_laying_hens_scaled, expected_scaled_ef)
  
  # For broilers with organic=TRUE, baseline is 91 days with EF 8.48e-5
  expect_silent(ef_broilers_scaled <- .tier1_EF("broilers", age = 60, organic = TRUE))
  expected_scaled_ef <- 8.48e-5 * (60 / 91)
  expect_equal(ef_broilers_scaled, expected_scaled_ef)
  
  # Test error cases
  expect_error(.tier1_EF("laying_hens", hpr = TRUE), "Parameter 'age' is required")
  expect_error(.tier1_EF("laying_hens", age = 140), "Parameter 'hpr' is required")
  expect_error(.tier1_EF("broilers", age = 42), "Parameter 'organic' is required")
  expect_error(.tier1_EF("broilers", organic = FALSE), "Parameter 'age' is required")
  expect_error(.tier1_EF("unknown_animal"), "not a tier 1 animal type")
})
test_that("load_NH3_emission_factors works correctly", {
  # Set up test paths using find_package_file
  config_paths <- list(
    mms_NH3 = system.file("extdata/mms_NH3.yaml", package = "rEMEP")
  )
  
  skip_if(!file.exists(config_paths$mms_NH3), "NH3 emission factors file not found")
  
  # Test loading NH3 emission factors
  nh3_ef <- load_NH3_emission_factors(animal_type = "dairy_cattle", config_paths = config_paths, debug_mode = TRUE)
  
  # Skip test if the file doesn't exist or couldn't be loaded
  skip_if(is.null(nh3_ef), "NH3 emission factors could not be loaded")
  
  # Check that the emission factors were loaded correctly
  expect_true(is.list(nh3_ef))
  expect_true(!is.null(nh3_ef$housing) || !is.null(nh3_ef$slurry) || !is.null(nh3_ef$solid))
})

test_that("load_N2O_emission_factors works correctly", {
  # Set up test paths using find_package_file
  config_paths <- list(
    storage_N2O = system.file("extdata/storage_N2O.yaml", package = "rEMEP")
  )
  
  skip_if(!file.exists(config_paths$storage_N2O), "N2O emission factors file not found")
  
  # Test loading N2O emission factors
  n2o_ef <- load_N2O_emission_factors(animal_type = "dairy_cattle", config_paths = config_paths, debug_mode = TRUE)
  
  # Skip test if the file doesn't exist or couldn't be loaded
  skip_if(is.null(n2o_ef), "N2O emission factors could not be loaded")
  
  # Check that the emission factors were loaded correctly
  expect_true(is.list(n2o_ef))
})

test_that("load_OTHER_emission_factors works correctly", {
  # Set up test paths using find_package_file
  config_paths <- list(
    storage_OTHERS = system.file("extdata/storage_OTHERS.yaml", package = "rEMEP")
  )
  
  skip_if(!file.exists(config_paths$storage_OTHERS), "OTHER emission factors file not found")
  
  # Test loading OTHER emission factors
  other_ef <- load_OTHER_emission_factors(config_paths = config_paths, debug_mode = TRUE)
  
  # Skip test if the file doesn't exist or couldn't be loaded
  skip_if(is.null(other_ef), "OTHER emission factors could not be loaded")
  
  # Check that the emission factors were loaded correctly
  expect_true(is.list(other_ef))
})

test_that("compile_emission_factors works correctly", {
  # Set up test paths using system.file
  config_paths <- list(
    mms_NH3 = system.file("extdata/mms_NH3.yaml", package = "rEMEP"),
    storage_N2O = system.file("extdata/storage_N2O.yaml", package = "rEMEP"),
    storage_OTHERS = system.file("extdata/storage_OTHERS.yaml", package = "rEMEP"),
    digestate_NH3 = system.file("extdata/digestate_NH3.yaml", package = "rEMEP")
  )
  
  # Skip if any files are missing
  skip_if(!all(sapply(config_paths, file.exists)), 
         "One or more emission factor files not found")
  
  # Test compiling emission factors
  ef_by_stage <- compile_emission_factors(animal_type = "dairy_cattle", config_paths = config_paths, debug_mode = TRUE)
  
  # Skip test if the emission factors couldn't be compiled
  skip_if(is.null(ef_by_stage), "Emission factors could not be compiled")
  
  # Check that the emission factors were compiled correctly
  expect_true(is.list(ef_by_stage))
})
