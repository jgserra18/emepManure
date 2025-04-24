
# Manure excreted by livestock, allocated to different pathways ----
Exc_total_N = livestock_excretion(animal_no = input$animal_number, exc_coef = input$excretion_coefficient)

Exc_grazing_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_grazing)
Exc_yards_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_yards)
Exc_housing_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_housing)

Exc_grazing_TAN = allocate_TAN_excretion(exc_allocation = Exc_grazing_N, f_TAN = input$fraction_TAN)
Exc_yards_TAN = allocate_TAN_excretion(exc_allocation = Exc_yards_N, f_TAN = input$fraction_TAN)
Exc_housing_TAN = allocate_TAN_excretion(exc_allocation = Exc_housing_N, f_TAN = input$fraction_TAN)


# Yards ----
Yards_emNH3 = yards_NH3(yards_tan = Exc_yards_TAN, EF = EF) # // NH3 EFs from yards 

# Grazing ----
Grazing_emNH3 = grazing_NH3(grazing_tan = Exc_grazing_TAN, EF = EF) # // NH3 EFs from grazing

## Net N from grazing ----
netGrazing_TAN = net_grazing(manure_grazing = Exc_grazing_TAN, grazing_NH3 = Grazing_emNH3)
netGrazing_N = net_grazing(manure_grazing = Exc_grazing_N, grazing_NH3 = Grazing_emNH3)


# Housing ----
Housing_slurry_TAN = housing_deposited(housing_nutrient = Exc_housing_TAN, f_man_type = input$fraction_manure_slurry)
Housing_slurry_N = housing_deposited(housing_nutrient = Exc_housing_TAN, f_man_type = input$fraction_manure_slurry)

Housing_solid_TAN = housing_deposited(housing_nutrient = Exc_housing_TAN, f_man_type = input$fraction_manure_solid)
Housing_solid_N = housing_deposited(housing_nutrient = Exc_housing_N, f_man_type = input$fraction_manure_solid)

## Emissions ----
Housing_slurry_emNH3 = housing_NH3(housing_tan = Housing_slurry_TAN, EF = EF) # // populate with correct EF
Housing_solid_emNH3 = housing_NH3(housing_tan = Housing_solid_emNH3, EF = EF) # // populate with correct EF


# Housing bedding (solid manure only) ----
Housing_ex_TAN = ex_housing_solid_TAN(housing_solid_TAN = Housing_solid_TAN, housing_solid_NH3 = Housing_solid_emNH3, animal_no = input$animal_number, bedding_amount = input$bedding_amount, f_imm = ) # / populate with f_imm from global_parameters
Housing_ex_N = ex_housing_solid_N(housing_solid_N = Housing_solid_N, housing_solid_NH3 = Housing_solid_emNH3, animal_no = input$animal_number, straw_N = input$bedding_N) # / populate with f_imm from global_parameters; input$bedding_N must be included in User_input.R


# Storage ----
Storage_solid_TAN = storage_solid(ex_housing_solid = Housing_ex_TAN, f_man_usage_solid = input$fraction_storage_solid)
Storage_solid_N = storage_solid(ex_housing_solid = Housing_ex_N, f_man_usage_solid = input$fraction_storage_solid)

Storage_slurry_TAN = storage_slurry(housing_slurry = Housing_slurry_TAN, housing_slurry_NH3 = Housing_slurry_emNH3, yard = Exc_yards_TAN, yard_NH3 = Yards_emNH3, f_man_usage_slurry = input$fraction_storage_slurry)
Storage_slurry_N = storage_slurry(housing_slurry = Housing_slurry_N, housing_slurry_NH3 = Housing_slurry_emNH3, yard = Exc_yards_N, yard_NH3 = Yards_emNH3, f_man_usage_slurry = input$fraction_storage_slurry)

updStorage_slurry_TAN = updated_storage_slurry(out_storage_slurry_TAN = Storage_slurry_TAN, out_storage_slurry_N = Storage_slurry_N, f_min = f_min) # f_min from global_parameters.YAML

## Emissions ----
Storage_solid_emNH3 = storage_emissions(storage_TAN = Storage_solid_TAN, EF_NH3 = EF, EF_N2O = Ef, EF_NO = Ef, EF_N2 = EF) # parse EFs according to YAML
Storage_slurry_emNH3 = storage_emissions(storage_TAN = updStorage_slurry_TAN, EF_NH3 = EF, EF_N2O = Ef, EF_NO = Ef, EF_N2 = EF) # parse EFs according to YAML


# Manure to biogas ----
Biogas_solid_TAN = storage_solid(ex_housing_solid = Housing_ex_TAN, f_man_usage_solid = input$fraction_biogas_solid)
Biogas_solid_N = storage_solid(ex_housing_solid = Housing_ex_N, f_man_usage_solid = input$fraction_biogas_solid)

Biogas_slurry_TAN = storage_slurry(housing_slurry = Housing_slurry_TAN, housing_slurry_NH3 = Housing_slurry_emNH3, yard = Exc_yards_TAN, yard_NH3 = Yards_emNH3, f_man_usage_slurry = input$fraction_biogas_slurry)
Biogas_slurry_N = storage_slurry(housing_slurry = Housing_slurry_N, housing_slurry_NH3 = Housing_slurry_emNH3, yard = Exc_yards_N, yard_NH3 = Yards_emNH3, f_man_usage_slurry = input$fraction_biogas_slurry)


# Digestate application ----
Appl_Digestate_TAN = digested_application_TAN(Biogas_slurry_TAN, Biogas_solid_TAN, Biogas_slurry_N, Biogas_solid_N, f_min_digester, EF) # f_min_digester from global_parameters; EF from digestate_NH3
Appl_Digestate_N = digested_application_N(Biogas_slurry_N, Biogas_solid_N, EF) # EF from digestate_NH3


# Direct field of manure application ----
f_man_usage_solid = c(input$fraction_biogas_solid, input$fraction_storage_solid)
f_man_usage_slurry = c(input$fraction_biogas_slurry, input$fraction_storage_slurry)

DirAppl_slurry_TAN = manure_direct_application(out_storage = Storage_slurry_TAN, c_f_man_usage = f_man_usage_slurry)
DirAppl_slurry_N = manure_direct_application(out_storage = Storage_slurry_N, c_f_man_usage = f_man_usage_slurry)

DirAppl_solid_TAN = manure_direct_application(out_storage = Storage_solid_TAN, c_f_man_usage = f_man_usage_solid)
DirAppl_solid_N = manure_direct_application(out_storage = Storage_solid_N, c_f_man_usage = f_man_usage_solid)


# Manure application ----

Appl_slurry_TAN = slurry_application(appl_direct_slurry = DirAppl_slurry_TAN, out_storage_slurry = Storage_slurry_TAN, storage_em_slurry = Storage_slurry_emNH3, digestate = Appl_Digestate_TAN)
Appl_slurry_N = slurry_application(appl_direct_slurry = DirAppl_slurry_N, out_storage_slurry = Storage_slurry_N, storage_em_slurry = Storage_slurry_emNH3, digestate = Appl_Digestate_N)

Appl_solid_TAN = solid_manure_application(appl_direct_solid = DirAppl_solid_TAN, out_storage_solid = Storage_solid_TAN, storage_em_solid = Storage_solid_emNH3)
Appl_solid_N = solid_manure_application(appl_direct_solid = DirAppl_solid_N, out_storage_solid = Storage_solid_N, storage_em_solid = Storage_solid_emNH3)

## Emissions ----
Appl_slurry_emNH3 = application_NH3(manure_appl_TAN = Appl_slurry_TAN, EF = EF) # NH3 EF for manure application
Appl_solid_emNH3 = application_NH3(manure_appl_TAN = Appl_solid_TAN, EF = EF) # NH3 EF for manure application

## Net N manure application ----
netAppl_slurry_TAN = net_manure_application(manure_appl = Appl_slurry_TAN, appl_NH3 = Appl_slurry_NH3)
netAppl_slurry_N = net_manure_application(manure_appl = Appl_slurry_N, appl_NH3 = Appl_slurry_NH3)

netAppl_solid_TAN = net_manure_application(manure_appl = Appl_solid_TAN, appl_NH3 = Appl_solid_NH3)
netAppl_solid_N = net_manure_application(manure_appl = Appl_solid_N, appl_NH3 = Appl_solid_NH3)