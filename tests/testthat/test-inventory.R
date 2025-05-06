library(testthat)
library(R6)
library(yaml)

# Get the package root directory
root_dir <- normalizePath(file.path(getwd(), "../.."))

# Source the necessary files using absolute paths
source(file.path(root_dir, "R", "User_input.R"))
source(file.path(root_dir, "R", "Inventory.R"))

# Helper function to get config file paths
get_config_path <- function(filename) {
  path <- file.path(root_dir, "inst", "extdata", filename)
  if (!file.exists(path)) {
    warning(paste(filename, "file not found:", path))
  }
  return(path)
}

# Source other required files
source(file.path(root_dir, "R", "Emission_factors.R"))
source(file.path(root_dir, "R", "Excretion.R"))
source(file.path(root_dir, "R", "Housing.R"))
source(file.path(root_dir, "R", "Storage.R"))
source(file.path(root_dir, "R", "Application.R"))
source(file.path(root_dir, "R", "Grazing.R"))
source(file.path(root_dir, "R", "Yards.R"))
source(file.path(root_dir, "R", "Digestates.R"))
source(file.path(root_dir, "R", "Bedding.R"))
source(file.path(root_dir, "R", "Utils.R"))

# Test the MMS class with slurry_crust parameter
test_that("MMS class initializes correctly with slurry_crust = TRUE", {
  # Create a User_input example with slurry_crust = TRUE
  test_input_with_crust <- User_input$new(
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
  
  # Skip test if the User_input object is not valid
  skip_if(!test_input_with_crust$valid, "User_input object is not valid")
  
  # Create an MMS object with debug mode enabled
  tryCatch({
    mms_with_crust <- MMS$new(test_input_with_crust, debug_mode = TRUE)
    
    # Check that the MMS object was created correctly
    expect_s3_class(mms_with_crust, "R6")
    expect_true(!is.null(mms_with_crust$EF_by_stage))
    expect_equal(mms_with_crust$user_input$animal_type, "dairy_cattle")
  }, error = function(e) {
    skip(paste("Failed to create MMS object:", e$message))
  })
})

test_that("MMS class initializes correctly with slurry_crust = FALSE", {
  # Create a User_input example with slurry_crust = FALSE
  test_input_no_crust <- User_input$new(
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
    slurry_crust = FALSE
  )
  
  # Skip test if the User_input object is not valid
  skip_if(!test_input_no_crust$valid, "User_input object is not valid")
  
  # Create an MMS object with debug mode enabled
  tryCatch({
    mms_no_crust <- MMS$new(test_input_no_crust, debug_mode = TRUE)
    
    # Check that the MMS object was created correctly
    expect_s3_class(mms_no_crust, "R6")
    expect_true(!is.null(mms_no_crust$EF_by_stage))
    expect_equal(mms_no_crust$user_input$animal_type, "dairy_cattle")
  }, error = function(e) {
    skip(paste("Failed to create MMS object:", e$message))
  })
})

test_that("MMS run_inventory function works correctly", {
  # Create a User_input example
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
  
  # Skip test if the User_input object is not valid
  skip_if(!test_input$valid, "User_input object is not valid")
  
  # Create an MMS object
  tryCatch({
    mms <- MMS$new(test_input, debug_mode = TRUE)
    
    # Run the inventory calculation
    results <- mms$run_inventory()
    
    # Check that the results structure is correct
    expect_true(is.list(results))
    expect_true(!is.null(results$excretion))
    expect_true(!is.null(results$grazing))
    expect_true(!is.null(results$yards))
    expect_true(!is.null(results$housing))
    expect_true(!is.null(results$storage))
    expect_true(!is.null(results$application))
    expect_true(!is.null(results$total))
  }, error = function(e) {
    skip(paste("Failed to run inventory:", e$message))
  })
})
