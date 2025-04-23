setwd('C:\\Users\\João Serra\\OneDrive\\Modelling\\EMEP_MMS')

library(yaml)
library(R6)

# Source the User_input.R file
source("R/User_input.R")

# Test 1: Dairy cattle with minimal parameters (auto-filling the rest)
input = User_input$new(
  animal_type = "dairy_cattle",  # A subtype, should map to dairy_cattle
  animal_number = 100,
  fraction_manure_slurry = 0.7,
  fraction_manure_solid = 0.3,
  fraction_storage_slurry = 0.9,
  fraction_biogas_slurry = 0.1,
  fraction_storage_solid = 0.8,
  fraction_biogas_solid = 0.2
)

# // function that reads animal_type and populates with all the different emission factors from the YAML

# Manure excreted by livestock, allocated to different pathways ----
Exc_total_N = livestock_excretion(animal_no = input$animal_number, exc_coef = input$excretion_coefficient)

Exc_grazing_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_grazing)
Exc_yards_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_yards)
Exc_housing_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_housing)

# // check 1; Exc_housing + Exc_yards + Exc_grazing = Exc_total

Exc_grazing_TAN = allocate_TAN_excretion(exc_allocation = Exc_grazing_N, f_TAN = input$fraction_TAN)
Exc_yards_TAN = allocate_TAN_excretion(exc_allocation = Exc_yards_N, f_TAN = input$fraction_TAN)
Exc_housing_TAN = allocate_TAN_excretion(exc_allocation = Exc_housing_N, f_TAN = input$fraction_TAN)

# Housing ----
Housing_slurry_TAN = housing_deposited(housing_nutrient = Exc_housing_TAN, f_man_type = input$fraction_manure_slurry)
Housing_slurry_N = housing_deposited(housing_nutrient = Exc_housing_TAN, f_man_type = input$fraction_manure_slurry)

Housing_solid_TAN = housing_deposited(housing_nutrient = Exc_housing_TAN, f_man_type = input$fraction_manure_solid)
Housing_solid_N = housing_deposited(housing_nutrient = Exc_housing_N, f_man_type = input$fraction_manure_solid)

## Emissions ----
Housing_slurry_NH3 = housing_NH3(housing_tan = Housing_slurry_TAN, EF = EF) # // populate with correct EF
Housing_solid_NH3 = housing_NH3(housing_tan = Housing_solid_NH3, EF = EF) # // populate with correct EF

# Housing bedding (solid manure only) ----
Housing_ex_TAN = ex_housing_solid_TAN(housing_solid_TAN = Housing_solid_TAN, housing_solid_NH3 = Housing_solid_NH3, animal_no = input$animal_number, bedding_amount = input$bedding_amount, f_imm = ) # / populate with f_imm from global_parameters
Housing_ex_N = ex_housing_solid_N(housing_solid_N = Housing_solid_N, housing_solid_NH3 = Housing_solid_NH3, animal_no = input$animal_number, straw_N = input$bedding_N) # / populate with f_imm from global_parameters; input$bedding_N must be included in User_input.R




