# Test script for MMS class inventory calculations
# This script creates a User_input example and runs the inventory using the MMS class

# Load required libraries
library(R6)
library(yaml)

# Source the necessary files
source("R/User_input.R")

# Create a User_input example
# This creates a sample dairy cattle farm with 100 animals
test_input <- User_input$new(
  animal_type = "dairy_cattle",
  animal_number = 100,
  excretion_coefficient = 105,  # kg N/head/yr
  fraction_TAN = 0.6,           # 60% of N is TAN
  fraction_grazing = 0.2,       # 20% of time spent grazing
  fraction_yard = 0.05,         # 5% of time spent in yards
  fraction_housing = 0.75,      # 75% of time spent in housing
  fraction_manure_slurry = 0.7, # 70% of manure as slurry
  fraction_manure_solid = 0.3,  # 30% of manure as solid
  bedding_amount = 500,         # kg/head/yr of bedding material
  fraction_storage_slurry = 0.8, # 80% of slurry to storage
  fraction_biogas_slurry = 0.2,  # 20% of slurry to biogas
  fraction_storage_solid = 0.9,  # 90% of solid manure to storage
  fraction_biogas_solid = 0.1,   # 10% of solid manure to biogas
  slurry_crust = TRUE            # Slurry has a crust (default)
)

# Print the input parameters
cat("=== Test Input Parameters ===\n")
test_input$print()


source("R/MMS_class.R")

# Create an MMS object with the test input
mms <- MMS$new(test_input)

# Compile emission factors
mms$compile_emission_factors()

# Print the MMS object to verify emission factors are loaded
cat("\n=== MMS Object with Emission Factors ===\n")
mms$print()

# Run the inventory calculations
cat("\n=== Running Inventory Calculations ===\n")
results <- mms$run_inventory()

# Print the results
cat("\n=== Inventory Results ===\n")
print(results)

# Now test with slurry_crust = FALSE to compare N2O emissions
cat("\n\n=== Testing the effect of slurry_crust parameter on N2O emissions ===\n")

# Create a new input with slurry_crust = FALSE
test_input_no_crust <- User_input$new(
  animal_type = "dairy_cattle",
  animal_number = 100,
  excretion_coefficient = 105,  # kg N/head/yr
  fraction_TAN = 0.6,           # 60% of N is TAN
  fraction_grazing = 0.2,       # 20% of time spent grazing
  fraction_yard = 0.05,         # 5% of time spent in yards
  fraction_housing = 0.75,      # 75% of time spent in housing
  fraction_manure_slurry = 0.7, # 70% of manure as slurry
  fraction_manure_solid = 0.3,  # 30% of manure as solid
  bedding_amount = 500,         # kg/head/yr of bedding material
  fraction_storage_slurry = 0.8, # 80% of slurry to storage
  fraction_biogas_slurry = 0.2,  # 20% of slurry to biogas
  fraction_storage_solid = 0.9,  # 90% of solid manure to storage
  fraction_biogas_solid = 0.1,   # 10% of solid manure to biogas
  slurry_crust = FALSE           # Slurry without crust
)

# Create an MMS object with the test input without crust
mms_no_crust <- MMS$new(test_input_no_crust, debug_mode = TRUE)

# Compile emission factors
mms_no_crust$compile_emission_factors()

# Run the inventory calculations
cat("\n=== Running Inventory Calculations (No Crust) ===\n")
results_no_crust <- mms_no_crust$run_inventory()

# Print the results
cat("\n=== Inventory Results (No Crust) ===\n")
print(results_no_crust)

# Compare N2O emissions with and without crust
cat("\n=== Comparison of N2O Emissions With and Without Slurry Crust ===\n")
cat("With crust (default):    ", results$storage$N2O$total, "kg N\n")
cat("Without crust:          ", results_no_crust$storage$N2O$total, "kg N\n")
cat("Difference:             ", results$storage$N2O$total - results_no_crust$storage$N2O$total, "kg N\n")
cat("Percentage difference:  ", round((results$storage$N2O$total - results_no_crust$storage$N2O$total) / results_no_crust$storage$N2O$total * 100, 1), "%\n")
cat("\n=== Inventory Results ===\n")
if (!is.null(results)) {
  # Print total N excretion
  cat("Total N excretion:", results$excretion$total, "kg N/yr\n")
  
  # Print N allocation to different systems
  cat("\nN Allocation:\n")
  cat("  Grazing:", results$excretion$grazing, "kg N/yr\n")
  cat("  Yards:", results$excretion$yards, "kg N/yr\n")
  cat("  Housing:", results$excretion$housing, "kg N/yr\n")
  
  # Print TAN allocation
  cat("\nTAN Allocation:\n")
  cat("  Grazing TAN:", results$excretion$grazing_TAN, "kg TAN/yr\n")
  cat("  Yards TAN:", results$excretion$yards_TAN, "kg TAN/yr\n")
  cat("  Housing TAN:", results$excretion$housing_TAN, "kg TAN/yr\n")
  
  # Print emissions from different sources
  cat("\nEmissions:\n")
  
  # Grazing emissions
  if (!is.null(results$emissions$grazing)) {
    cat("  Grazing NH3-N:", results$emissions$grazing$NH3, "kg NH3-N/yr\n")
  }
  
  # Yards emissions
  if (!is.null(results$emissions$yards)) {
    cat("  Yards NH3-N:", results$emissions$yards$NH3, "kg NH3-N/yr\n")
  }
  
  # Housing emissions
  if (!is.null(results$emissions$housing)) {
    cat("  Housing NH3-N (slurry):", results$emissions$housing$slurry$NH3, "kg NH3-N/yr\n")
    cat("  Housing NH3-N (solid):", results$emissions$housing$solid$NH3, "kg NH3-N/yr\n")
  }
  
  # Storage emissions
  if (!is.null(results$emissions$storage)) {
    cat("\n  Storage Emissions (slurry):\n")
    cat("    NH3-N:", results$emissions$storage$slurry$NH3, "kg NH3-N/yr\n")
    cat("    N2O-N:", results$emissions$storage$slurry$N2O, "kg N2O-N/yr\n")
    cat("    NO-N:", results$emissions$storage$slurry$NO, "kg NO-N/yr\n")
    cat("    N2-N:", results$emissions$storage$slurry$N2, "kg N2-N/yr\n")
    
    cat("\n  Storage Emissions (solid):\n")
    cat("    NH3-N:", results$emissions$storage$solid$NH3, "kg NH3-N/yr\n")
    cat("    N2O-N:", results$emissions$storage$solid$N2O, "kg N2O-N/yr\n")
    cat("    NO-N:", results$emissions$storage$solid$NO, "kg NO-N/yr\n")
    cat("    N2-N:", results$emissions$storage$solid$N2, "kg N2-N/yr\n")
  }
  
  # Application emissions
  if (!is.null(results$emissions$application)) {
    cat("\n  Application Emissions:\n")
    cat("    Slurry NH3-N:", results$emissions$application$slurry$NH3, "kg NH3-N/yr\n")
    cat("    Solid NH3-N:", results$emissions$application$solid$NH3, "kg NH3-N/yr\n")
  }
  
  # Total emissions by gas
  if (!is.null(results$emissions$total)) {
    cat("\nTotal Emissions:\n")
    cat("  Total NH3-N:", results$emissions$total$NH3, "kg NH3-N/yr\n")
    cat("  Total N2O-N:", results$emissions$total$N2O, "kg N2O-N/yr\n")
    cat("  Total NO-N:", results$emissions$total$NO, "kg NO-N/yr\n")
    cat("  Total N2-N:", results$emissions$total$N2, "kg N2-N/yr\n")
  }
  
  # N flows to field
  cat("\nN Flows to Field:\n")
  if (!is.null(results$field)) {
    cat("  Slurry N:", results$field$slurry$N, "kg N/yr\n")
    cat("  Slurry TAN:", results$field$slurry$TAN, "kg TAN/yr\n")
    cat("  Solid N:", results$field$solid$N, "kg N/yr\n")
    cat("  Solid TAN:", results$field$solid$TAN, "kg TAN/yr\n")
  }
  
} else {
  cat("No results returned from inventory calculations.\n")
}

# Run a second example with different parameters
cat("\n\n=== Second Example: Pig Farm ===\n")
pig_input <- User_input$new(
  animal_type = "fattening_pigs",
  animal_number = 500,
  excretion_coefficient = 12,    # kg N/head/yr
  fraction_TAN = 0.7,            # 70% of N is TAN
  fraction_grazing = 0,          # No grazing for pigs
  fraction_yard = 0.1,           # 10% of time spent in yards
  fraction_housing = 0.9,        # 90% of time spent in housing
  fraction_manure_slurry = 0.9,  # 90% of manure as slurry
  fraction_manure_solid = 0.1,   # 10% of manure as solid
  bedding_amount = 50,           # kg/head/yr of bedding material
  fraction_storage_slurry = 0.7, # 70% of slurry to storage
  fraction_biogas_slurry = 0.3,  # 30% of slurry to biogas
  fraction_storage_solid = 0.8,  # 80% of solid manure to storage
  fraction_biogas_solid = 0.2    # 20% of solid manure to biogas
)

# Create an MMS object with the pig input
pig_mms <- MMS$new(pig_input)

# Compile emission factors
pig_mms$compile_emission_factors()

# Run the inventory calculations
pig_results <- pig_mms$run_inventory()

# Print summary of pig farm results
cat("\n=== Pig Farm Inventory Results Summary ===\n")
if (!is.null(pig_results)) {
  cat("Total N excretion:", pig_results$excretion$total, "kg N/yr\n")
  
  cat("\nTotal Emissions:\n")
  if (!is.null(pig_results$emissions$total)) {
    cat("  Total NH3-N:", pig_results$emissions$total$NH3, "kg NH3-N/yr\n")
    cat("  Total N2O-N:", pig_results$emissions$total$N2O, "kg N2O-N/yr\n")
  }
  
  cat("\nN Flows to Field:\n")
  if (!is.null(pig_results$field)) {
    cat("  Total N to field:", 
        pig_results$field$slurry$N + pig_results$field$solid$N, "kg N/yr\n")
    cat("  Total TAN to field:", 
        pig_results$field$slurry$TAN + pig_results$field$solid$TAN, "kg TAN/yr\n")
  }
} else {
  cat("No results returned from pig farm inventory calculations.\n")
}
