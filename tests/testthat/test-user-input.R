library(testthat)
library(R6)
library(yaml)

# No need for manual file sourcing and path handling
# The package should be loaded by devtools::test()

# Test the User_input class
test_that("User_input class initializes correctly", {
  # Create a User_input object
  test_input <- User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100,
    excretion_coefficient = 105,
    fraction_TAN = 0.6,
    fraction_grazing = 0.2,
    fraction_yards = 0.05,
    fraction_housing = 0.75,
    fraction_manure_slurry = 0.7,
    fraction_manure_solid = 0.3,
    bedding_amount = 500,
    fraction_storage_slurry = 0.8,
    fraction_biogas_slurry = 0.2,
    fraction_storage_solid = 0.9,
    fraction_biogas_solid = 0.1,
    slurry_crust = TRUE
  )
  
  # Check that the User_input object was created correctly
  expect_s3_class(test_input, "R6")
  expect_equal(test_input$animal_type, "dairy_cattle")
  expect_equal(test_input$animal_number, 100)
  expect_equal(test_input$excretion_coefficient, 105)
  expect_equal(test_input$fraction_TAN, 0.6)
  expect_equal(test_input$fraction_grazing, 0.2)
  expect_equal(test_input$fraction_yards, 0.05)
  expect_equal(test_input$fraction_housing, 0.75)
  expect_equal(test_input$fraction_manure_slurry, 0.7)
  expect_equal(test_input$fraction_manure_solid, 0.3)
  expect_equal(test_input$bedding_amount, 500)
  expect_equal(test_input$fraction_storage_slurry, 0.8)
  expect_equal(test_input$fraction_biogas_slurry, 0.2)
  expect_equal(test_input$fraction_storage_solid, 0.9)
  expect_equal(test_input$fraction_biogas_solid, 0.1)
  expect_equal(test_input$slurry_crust, TRUE)
})

test_that("User_input validation works correctly for invalid parameters", {
  # Create a User_input object with invalid parameters
  invalid_input <- suppressWarnings(
    User_input$new(
      animal_type = "dairy_cattle",
      animal_number = 100,
      excretion_coefficient = 105,
      fraction_TAN = 0.6,
      fraction_grazing = 0.4,  # Sum with yards and housing will be > 1
      fraction_yards = 0.3,
      fraction_housing = 0.4,
      fraction_manure_slurry = 0.7,
      fraction_manure_solid = 0.3,
      bedding_amount = 500,
      fraction_storage_slurry = 0.8,
      fraction_biogas_slurry = 0.2,
      fraction_storage_solid = 0.9,
      fraction_biogas_solid = 0.1,
      slurry_crust = TRUE
    )
  )
  
  # Check that the validation failed
  expect_false(invalid_input$valid)
})

test_that("User_input validation works correctly for valid parameters", {
  # Create a User_input object with valid parameters
  valid_input <- User_input$new(
    animal_type = "dairy_cattle",
    animal_number = 100,
    excretion_coefficient = 105,
    fraction_TAN = 0.6,
    fraction_grazing = 0.2,
    fraction_yards = 0.3,
    fraction_housing = 0.5,
    fraction_manure_slurry = 0.7,
    fraction_manure_solid = 0.3,
    bedding_amount = 500,
    fraction_storage_slurry = 0.8,
    fraction_biogas_slurry = 0.2,
    fraction_storage_solid = 0.9,
    fraction_biogas_solid = 0.1,
    slurry_crust = TRUE
  )
  
  # Check that the validation passed
  expect_true(valid_input$valid)
})
