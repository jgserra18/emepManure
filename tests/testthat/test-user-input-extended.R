library(testthat)
library(R6)
library(yaml)

# Test edge cases in input validation
test_that("User_input validation handles edge cases correctly", {
  # Test zero values
  expect_error(
    User_input$new(
      animal_type = "dairy_cattle",
      animal_number = 0
    ),
    "animal_number must be positive"
  )
  
  # Test very small fractions (numerical precision)
  small_input <- User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100,
    fraction_grazing = 0.333333333333333,
    fraction_yards = 0.333333333333333,
    fraction_housing = 0.333333333333334  # Sum should be exactly 1
  )
  expect_true(small_input$validate())
  
  # Test exactly 1.0 sums
  exact_input <- User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100,
    fraction_grazing = 0.4,
    fraction_yards = 0.3,
    fraction_housing = 0.3,
    fraction_manure_slurry = 0.5,
    fraction_manure_solid = 0.5
  )
  expect_true(exact_input$validate())
})

# Test configuration file handling
test_that("User_input handles configuration files correctly", {
  # Create temporary YAML config
  yaml_config <- list(
    animal_type = "dairy_cattle",
    animal_number = 100,
    fraction_grazing = 0.2,
    fraction_yards = 0.05,
    fraction_housing = 0.75,
    fraction_manure_slurry = 0.7,
    fraction_manure_solid = 0.3,
    fraction_storage_slurry = 0.8,
    fraction_biogas_slurry = 0.2
  )
  
  temp_yaml <- tempfile(fileext = ".yaml")
  yaml::write_yaml(yaml_config, temp_yaml)
  
  # Test loading from YAML
  input_from_yaml <- User_input$new_from_yaml(temp_yaml)
  expect_equal(input_from_yaml$animal_type, yaml_config$animal_type)
  expect_equal(input_from_yaml$animal_number, yaml_config$animal_number)
  expect_equal(input_from_yaml$fraction_grazing, yaml_config$fraction_grazing)
  
  # Clean up
  unlink(temp_yaml)
})

# Test integration with MMS
test_that("User_input works correctly with MMS class", {
  input <- User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100,
    fraction_grazing = 0.2,
    fraction_yards = 0.05,
    fraction_housing = 0.75,
    fraction_manure_slurry = 0.7,
    fraction_manure_solid = 0.3,
    fraction_storage_slurry = 0.8,
    fraction_biogas_slurry = 0.2,
    fraction_storage_solid = 0.9,
    fraction_biogas_solid = 0.1,
    slurry_crust = TRUE
  )
  
  mms <- MMS$new(input)
  results <- mms$run_inventory()
  
  # Test results structure - just check that we get a non-null result with expected components
  expect_true(!is.null(results))
  expect_true(is.list(results))
  expect_true(!is.null(results$excretion))
  
  # Check that at least some of the expected components exist
  # We don't need to check all of them, just enough to confirm the method ran
  expect_true(any(c("housing", "storage", "application", "grazing", "yards") %in% names(results)))
})

# Test animal type validation
test_that("User_input validates animal types correctly", {
  # Test valid animal types
  expect_no_error(User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100
  ))
  
  # Test invalid animal type
  expect_error(
    User_input$new(
      animal_type = "invalid_animal",
      animal_number = 100
    ),
    "Invalid animal type"
  )
})

# Test mass balance in fractions
test_that("User_input fractions maintain mass balance", {
  input <- User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100,
    fraction_grazing = 0.2,
    fraction_yards = 0.05,
    fraction_housing = 0.75,
    fraction_manure_slurry = 0.7,
    fraction_manure_solid = 0.3,
    fraction_storage_slurry = 0.8,
    fraction_biogas_slurry = 0.2,
    fraction_storage_solid = 0.9,
    fraction_biogas_solid = 0.1
  )
  
  # Test time fractions sum to 1
  expect_equal(
    input$fraction_grazing + input$fraction_yards + input$fraction_housing,
    1.0,
    tolerance = 1e-10
  )
  
  # Test manure fractions sum to 1
  expect_equal(
    input$fraction_manure_slurry + input$fraction_manure_solid,
    1.0,
    tolerance = 1e-10
  )
  
  # Test storage/biogas fractions sum to 1 for both types
  expect_equal(
    input$fraction_storage_slurry + input$fraction_biogas_slurry,
    1.0,
    tolerance = 1e-10
  )
  expect_equal(
    input$fraction_storage_solid + input$fraction_biogas_solid,
    1.0,
    tolerance = 1e-10
  )
})

# Test default values and NULL handling
test_that("User_input handles defaults and NULL values correctly", {
  # Test with minimal parameters
  minimal_input <- User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100
  )
  
  # Check that defaults are loaded from animal data
  expect_false(is.null(minimal_input$excretion_coefficient))
  expect_false(is.null(minimal_input$fraction_TAN))
  
  # Test with NULL values for optional parameters
  null_input <- User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100,
    bedding_amount = NULL,
    slurry_crust = NULL
  )
  
  # Optional parameters should remain NULL if not provided
  expect_null(null_input$bedding_amount)
  expect_null(null_input$slurry_crust)
})

# Test error messages
test_that("User_input provides descriptive error messages", {
  # Test negative animal number
  expect_error(
    User_input$new(
      animal_type = "dairy_cattle",
      animal_number = -10
    ),
    "animal_number must be positive"
  )
  
  # Test invalid fractions
  expect_error(
    User_input$new(
      animal_type = "dairy_cattle",
      animal_number = 100,
      fraction_grazing = 1.5  # > 1
    ),
    "fraction_grazing must be between 0 and 1"
  )
  
  # Test missing required parameters
  expect_error(
    User_input$new(),
    "animal_type must be specified"
  )
})
