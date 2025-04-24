# Test script for MMS class with slurry_crust parameter
# Load required libraries
library(R6)
library(yaml)

# Source the necessary files
source("R/User_input.R")
source("R/MMS_class.R")

# Create a User_input example with slurry_crust = TRUE
test_input_with_crust <- User_input$new(
  animal_type = "dairy_cattle",
  animal_number = 100,
  excretion_coefficient = 105,
  fraction_TAN = 0.6,
  fraction_grazing = 0.2,
  fraction_yard = 0.05,
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

# Create an MMS object with debug mode enabled
cat("Creating MMS object with slurry_crust = TRUE...\n")
mms_with_crust <- MMS$new(test_input_with_crust, debug = TRUE)

# Compile emission factors
cat("Compiling emission factors...\n")
mms_with_crust$compile_emission_factors()

# Print the MMS object
cat("\n=== MMS Object with Emission Factors (With Crust) ===\n")
mms_with_crust$print()

# Create a User_input example with slurry_crust = FALSE
test_input_no_crust <- User_input$new(
  animal_type = "dairy_cattle",
  animal_number = 100,
  excretion_coefficient = 105,
  fraction_TAN = 0.6,
  fraction_grazing = 0.2,
  fraction_yard = 0.05,
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

# Create an MMS object with debug mode enabled
cat("\nCreating MMS object with slurry_crust = FALSE...\n")
mms_no_crust <- MMS$new(test_input_no_crust, debug = TRUE)

# Compile emission factors
cat("Compiling emission factors...\n")
mms_no_crust$compile_emission_factors()

# Print the MMS object
cat("\n=== MMS Object with Emission Factors (No Crust) ===\n")
mms_no_crust$print()

# Test the get_N2O_EF method
cat("\n=== Testing get_N2O_EF method ===\n")
cat("With crust - Slurry EF: ")
slurry_ef_with_crust <- mms_with_crust$get_N2O_EF("slurry")
cat(slurry_ef_with_crust, "\n")

cat("No crust - Slurry EF: ")
slurry_ef_no_crust <- mms_no_crust$get_N2O_EF("slurry")
cat(slurry_ef_no_crust, "\n")

cat("Solid manure EF: ")
solid_ef <- mms_with_crust$get_N2O_EF("solid")
cat(solid_ef, "\n")
