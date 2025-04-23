library(yaml)
library(R6)

# Source the User_input.R file
source("R/User_input.R")

# Test 1: Dairy cattle with minimal parameters (auto-filling the rest)
cat("\n===== TEST 1: Dairy Cattle (Subtype) =====\n")
input1 = User_input$new(
  animal_type = "turkeys",  # A subtype, should map to dairy_cattle
  animal_number = 100,
  #excretion_coefficient = 105, 
  fraction_manure_slurry = 0.7,
  fraction_manure_solid = 0.3,
  fraction_storage_slurry = 0.9,
  fraction_biogas_slurry = 0.1,
  fraction_storage_solid = 0.8,
  fraction_biogas_solid = 0.2
)

# Print the input parameters and validation results
input1$print()

# Test 2: Poultry with minimal parameters
cat("\n===== TEST 2: Laying Hens (Birds) =====\n")
input2 = User_input$new(
  animal_type = "laying_hens",  # A subtype of birds
  animal_number = 1000,
  fraction_manure_slurry = 0.0,
  fraction_manure_solid = 1.0,
  fraction_storage_slurry = 0.0,
  fraction_biogas_slurry = 0.0,
  fraction_storage_solid = 1.0,
  fraction_biogas_solid = 0.0
)

# Print the input parameters and validation results
input2$print()

# Test 3: Pig with minimal parameters (to show bedding amount auto-filling)
cat("\n===== TEST 3: Pigs with Bedding =====\n")
input3 = User_input$new(
  animal_type = "pigs",
  animal_number = 200,
  fraction_manure_slurry = 0.4,
  fraction_manure_solid = 0.6,  # High solid fraction to demonstrate bedding
  fraction_storage_slurry = 0.9,
  fraction_biogas_slurry = 0.1,
  fraction_storage_solid = 0.7,
  fraction_biogas_solid = 0.3
)

# Print the input parameters and validation results
input3$print()

# Test 4: Sheep with minimal parameters (to test singular/plural handling)
cat("\n===== TEST 4: Sheep (Singular/Plural Handling) =====\n")
input4 = User_input$new(
  animal_type = "sheep",
  animal_number = 50,
  fraction_manure_slurry = 0.0,
  fraction_manure_solid = 1.0,
  fraction_storage_slurry = 0.0,
  fraction_biogas_slurry = 0.0,
  fraction_storage_solid = 1.0,
  fraction_biogas_solid = 0.0
)

# Print the input parameters and validation results
input4$print()

# Show the main animal type mapping for each test
cat("\n===== ANIMAL TYPE MAPPING =====\n")
cat("Test 1 - Original type:", input1$animal_type, "-> Main type:", input1$main_animal_type, "\n")
cat("Test 2 - Original type:", input2$animal_type, "-> Main type:", input2$main_animal_type, "\n")
cat("Test 3 - Original type:", input3$animal_type, "-> Main type:", input3$main_animal_type, "\n")
cat("Test 4 - Original type:", input4$animal_type, "-> Main type:", input4$main_animal_type, "\n")
