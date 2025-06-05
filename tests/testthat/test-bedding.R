library(testthat)
library(R6)
library(rEMEP)

test_that("ex_housing_solid_TAN calculates correctly with f_man_solid parameter", {
  # Test case with typical values
  housing_solid_TAN <- 1000  # kg TAN/yr
  housing_solid_NH3 <- 100   # kg N-NH3/yr
  animal_no <- 100           # head/yr
  f_man_solid <- 0.3         # proportion
  bedding_amount <- 5        # kg N/(head.yr)
  f_imm <- 0.0067            # fraction (typical value)
  
  # Calculate expected result
  expected <- 1000 - (100 + (100 * 0.3 * bedding_amount * f_imm))
  
  # Call the function
  result <- ex_housing_solid_TAN(
    housing_solid_TAN = housing_solid_TAN,
    housing_solid_NH3 = housing_solid_NH3,
    animal_no = animal_no,
    f_man_solid = f_man_solid,
    bedding_amount = bedding_amount,
    f_imm = f_imm
  )
  
  # Check result
  expect_equal(result, expected)
  
  # Test case with negative result (should return 0)
  housing_solid_TAN_small <- 50  # kg TAN/yr
  
  # This calculation would result in a negative value
  expected_negative <- 50 - (100 + (100 * 0.3 * bedding_amount * f_imm))
  expect_true(expected_negative < 0)  # Verify our test setup
  
  # Function should return 0 instead of negative value
  result_negative <- ex_housing_solid_TAN(
    housing_solid_TAN = housing_solid_TAN_small,
    housing_solid_NH3 = housing_solid_NH3,
    animal_no = animal_no,
    f_man_solid = f_man_solid,
    bedding_amount = bedding_amount,
    f_imm = f_imm
  )
  
  expect_equal(result_negative, 0)
})

test_that("ex_housing_solid_N calculates correctly with bedding_amount and f_man_solid parameters", {
  # Test case with typical values
  housing_solid_N <- 1500    # kg N/yr
  housing_solid_NH3 <- 100   # kg N-NH3/yr
  animal_no <- 100           # head/yr
  bedding_amount <- 5             # kg/(head.yr)
  f_man_solid <- 0.3         # proportion
  
  # Calculate expected result
  expected <- 1500 + (100 * 5 * 0.3) - 100
  
  # Call the function
  result <- ex_housing_solid_N(
    housing_solid_N = housing_solid_N,
    housing_solid_NH3 = housing_solid_NH3,
    animal_no = animal_no,
    bedding_amount = bedding_amount,
    f_man_solid = f_man_solid
  )
  
  # Check result
  expect_equal(result, expected)
})
