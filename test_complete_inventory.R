# Test script for the complete manure management system
# This script tests all components of the MMS class

# Load required libraries
library(R6)
library(yaml)

# Source the necessary files
source("R/User_input.R")
source("R/MMS_class.R")
source("R/Excretion.R")
source("R/Grazing.R")
source("R/Yards.R")
source("R/Housing.R")
source("R/Storage.R")
source("R/Digestates.R")

# Create a test user input
test_input <- User_input$new(
  animal_type = "dairy_cattle",
  animal_no = 100,
  excretion_coefficient = 105,  # kg N/animal/year
  fraction_grazing = 0.2,
  fraction_yards = 0.1,
  fraction_housing = 0.7,
  fraction_TAN = 0.6,
  fraction_manure_slurry = 0.7,
  fraction_manure_solid = 0.3,
  fraction_storage_slurry = 0.9,
  fraction_biogas_slurry = 0.1,
  fraction_storage_solid = 0.8,
  fraction_biogas_solid = 0.2,
  f_man_usage = list(
    slurry = list(
      broadcast = 0.3,
      trailing_hose = 0.4,
      trailing_shoe = 0.2,
      injection = 0.1
    ),
    solid = list(
      broadcast = 0.8,
      incorporation = 0.2
    )
  ),
  slurry_crust = TRUE  # Test with slurry crust
)

# Create an MMS instance with debug mode enabled
mms <- MMS$new(user_input = test_input, debug_mode = TRUE)

# Run the inventory calculation
results <- mms$run_inventory()

# Print the results
cat("\n===== INVENTORY RESULTS =====\n\n")

# Print total emissions
cat("Total emissions:\n")
cat("NH3-N:", results$total$NH3_N, "kg N/year\n")
cat("NH3:", results$total$NH3, "kg NH3/year\n")
cat("N2O-N:", results$total$N2O_N, "kg N/year\n")
cat("N2O:", results$total$N2O, "kg N2O/year\n")
cat("NO-N:", results$total$NO_N, "kg N/year\n")
cat("NO:", results$total$NO, "kg NO/year\n")
cat("N2:", results$total$N2, "kg N2/year\n\n")

# Print emissions by stage
cat("Emissions by stage (NH3-N, kg N/year):\n")
cat("Grazing:", results$grazing$NH3_N, "\n")
cat("Yards:", results$yards$NH3_N, "\n")
cat("Housing:", results$housing$total$NH3_N, "\n")
cat("Storage:", results$storage$total$NH3_N, "\n")
cat("Digestate:", results$digestate$NH3_N, "\n")
cat("Application:", results$application$total$NH3_N, "\n\n")

# Test with slurry crust = FALSE
cat("===== TESTING SLURRY CRUST EFFECT =====\n\n")

# Create a new user input with slurry_crust = FALSE
test_input_no_crust <- User_input$new(
  animal_type = "dairy_cattle",
  animal_no = 100,
  excretion_coefficient = 105,  # kg N/animal/year
  fraction_grazing = 0.2,
  fraction_yards = 0.1,
  fraction_housing = 0.7,
  fraction_TAN = 0.6,
  fraction_manure_slurry = 0.7,
  fraction_manure_solid = 0.3,
  fraction_storage_slurry = 0.9,
  fraction_biogas_slurry = 0.1,
  fraction_storage_solid = 0.8,
  fraction_biogas_solid = 0.2,
  f_man_usage = list(
    slurry = list(
      broadcast = 0.3,
      trailing_hose = 0.4,
      trailing_shoe = 0.2,
      injection = 0.1
    ),
    solid = list(
      broadcast = 0.8,
      incorporation = 0.2
    )
  ),
  slurry_crust = FALSE  # Test without slurry crust
)

# Create an MMS instance with debug mode enabled
mms_no_crust <- MMS$new(user_input = test_input_no_crust, debug_mode = TRUE)

# Run the inventory calculation
results_no_crust <- mms_no_crust$run_inventory()

# Compare N2O emissions with and without slurry crust
cat("N2O emissions comparison:\n")
cat("With slurry crust (N2O-N):", results$storage$slurry$N2O_N, "kg N/year\n")
cat("Without slurry crust (N2O-N):", results_no_crust$storage$slurry$N2O_N, "kg N/year\n")
cat("Difference:", results$storage$slurry$N2O_N - results_no_crust$storage$slurry$N2O_N, "kg N/year\n")
