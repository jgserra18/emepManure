#' @title Manure Management System (MMS) Class
#' @description Class for calculating emissions from livestock manure management systems
#' based on user input parameters and emission factors from configuration files.
#'
#' The MMS class handles the entire calculation workflow:
#' animal excretion → housing/yards/grazing → storage → field application.
#' It loads appropriate emission factors based on animal type and performs calculations.
#'
#' @details This class performs calculations for:
#' \itemize{
#'   \item Manure excretion and allocation to different systems
#'   \item Emissions from housing, yards, and grazing
#'   \item Storage emissions and transformations
#'   \item Biogas/digestate handling
#'   \item Field application emissions
#' }
#'
#' @examples
#' # Create a User_input object
#' input = User_input$new(
#'   animal_type = "dairy_cattle",
#'   animal_number = 100,
#'   fraction_manure_slurry = 0.7,
#'   fraction_manure_solid = 0.3,
#'   fraction_storage_slurry = 0.9,
#'   fraction_biogas_slurry = 0.1,
#'   fraction_storage_solid = 0.8,
#'   fraction_biogas_solid = 0.2
#' )
#'
#' # Create MMS object and run calculations
#' mms = MMS$new(input)
#' results = mms$run_simulation()
#'
#' @importFrom yaml read_yaml
#' @importFrom R6 R6Class
#' @importFrom utils menu
#' @importFrom stats setNames

# Explicitly import R6 package
library(R6)

# Import functions from other files
# These functions are defined in the package and will be available when the package is loaded
NULL

#' MMS Class for Manure Management System
#'
#' @description An R6 class to perform manure management calculations based on User_input
#' parameters and emission factors from configuration files.
#'
#' @export
MMS = R6Class("MMS",
  public = list(
    #' @field user_input User_input object containing input parameters
    user_input = NULL,

    #' @field config_paths Paths to configuration files
    config_paths = list(
      mms_NH3 = system.file("extdata/mms_NH3.yaml", package = "rEMEP"),
      storage_N2O = system.file("extdata/storage_N2O.yaml", package = "rEMEP"),
      storage_OTHERS = system.file("extdata/storage_OTHERS.yaml", package = "rEMEP"),
      digestate_NH3 = system.file("extdata/digestate_NH3.yaml", package = "rEMEP"),
      global_parameters = system.file("extdata/global_parameters.yaml", package = "rEMEP")
    ),

    #' @field debug_mode Enable debug mode for more verbose output
    debug_mode = TRUE,

    #' @field check Enable mass check balance across all steps
    check = FALSE,

    #' @field global_params Global parameters loaded from configuration files
    global_params = NULL,

    #' @field results Results of the simulation
    results = NULL,

    #' @field EF_by_stage Emission factors organized by stage
    EF_by_stage = NULL,

    #' @description Create a new MMS object
    #' @param user_input User_input object containing input parameters
    #' @param debug_mode Enable debug mode for more verbose output
    initialize = function(user_input, debug_mode = FALSE) {
      self$user_input = user_input
      self$EF_by_stage = list()
      self$debug_mode = debug_mode

      if (self$debug_mode) {
        message("Initializing MMS object...")
        message(paste("Animal type:", user_input$animal_type))
      }

      # Compile emission factors
      self$compile_emission_factors_from_input(slurry_crust = user_input$slurry_crust)

      if (self$debug_mode) {
        message("MMS object initialized successfully.")
      }

      invisible(self)
    },

    #' @description Compile emission factors from user input
    #' @param slurry_crust Logical indicating whether slurry has a crust (default: TRUE)
    #' @return List of emission factors organized by stage
    compile_emission_factors_from_input = function(slurry_crust = TRUE) {
      if (self$debug_mode) {
        message("Compiling emission factors from user input...")
      }

      # Get animal type from user input
      animal_type = self$user_input$animal_type

      # Set up config paths
      config_paths = list(
        mms_NH3 = system.file("extdata/mms_NH3.yaml", package = "rEMEP"),
        storage_N2O = system.file("extdata/storage_N2O.yaml", package = "rEMEP"),
        storage_OTHERS = system.file("extdata/storage_OTHERS.yaml", package = "rEMEP"),
        digestate_NH3 = system.file("extdata/digestate_NH3.yaml", package = "rEMEP")
      )

      # Compile emission factors
      self$EF_by_stage = compile_emission_factors(animal_type, config_paths, self$debug_mode)

      # Handle N2O emission factors based on slurry_crust parameter
      if (!is.null(self$EF_by_stage$storage$N2O)) {
        n2o_ef = self$EF_by_stage$storage$N2O

        # Process N2O emission factors for slurry based on crust presence
        if (self$debug_mode) {
          message("Processing N2O emission factors for slurry (crust: ", slurry_crust, ")")
        }

        # Check if we have the appropriate N2O emission factors
        if (slurry_crust && !is.null(n2o_ef$slurry_with_crust)) {
          # Use slurry with crust emission factor
          self$EF_by_stage$storage$N2O$slurry = n2o_ef$slurry_with_crust$EF
          if (self$debug_mode) {
            message("Using N2O emission factor for slurry with crust: ", self$EF_by_stage$storage$N2O$slurry)
          }
        } else if (!slurry_crust && !is.null(n2o_ef$slurry_without_crust)) {
          # Use slurry without crust emission factor
          self$EF_by_stage$storage$N2O$slurry = n2o_ef$slurry_without_crust$EF
          if (self$debug_mode) {
            message("Using N2O emission factor for slurry without crust: ", self$EF_by_stage$storage$N2O$slurry)
          }
        }

        # Process N2O emission factors for solid manure
        if (!is.null(n2o_ef$solid)) {
          self$EF_by_stage$storage$N2O$solid = n2o_ef$solid$EF
          if (self$debug_mode) {
            message("Using N2O emission factor for solid manure: ", self$EF_by_stage$storage$N2O$solid)
          }
        }
      }

      if (self$debug_mode) {
        message("Emission factors compiled successfully.")
      }

      return(self$EF_by_stage)
    },


    #' @description Run the inventory calculation
    #' @return List of results
    run_inventory = function() {
      if (self$debug_mode) {message("Running inventory calculation...")  }

      # Initialize results structure
      results = list(
        excretion = list(),
        grazing = list(),
        yards = list(),
        housing = list(),
        storage = list(),
        digestate = list(),
        application = list()
      )

      if (self$debug_mode) {   message("Initialized results list.") }

      # Get user input parameters
      input = self$user_input


      if (self$debug_mode) { message("Using emission factors for animal type: ", input$animal_type)  }
      # Compile emission factors (or refresh them if already compiled)
      self$EF_by_stage = self$compile_emission_factors_from_input(slurry_crust = input$slurry_crust)


      # Manure excreted by livestock, allocated to different pathways ----
      if (self$debug_mode) {
        message("DEBUG: animal_number = ", input$animal_number)
        message("DEBUG: excretion_coefficient = ", input$excretion_coefficient)
      }

      # Use animal_number instead of animal_no to match User_input field name
      Exc_total_N = livestock_excretion(animal_no = input$animal_number, exc_coef = input$excretion_coefficient)

      if (self$debug_mode) {
        message("DEBUG: Exc_total_N = ", Exc_total_N)
      }

      if (self$debug_mode) {
        message("DEBUG: fraction_grazing = ", input$fraction_grazing)
        message("DEBUG: fraction_yards = ", input$fraction_yards)
        message("DEBUG: fraction_housing = ", input$fraction_housing)
      }

      Exc_grazing_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_grazing)
      Exc_yards_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_yards)
      Exc_housing_N = allocate_livestock_excretion(exc_total = Exc_total_N, f_allocation = input$fraction_housing)

      if (self$check) {
        if (self$debug_mode) { message('DEBUG: mass balance check between total N excreted AND the sum of its different allocations\n(Grazing, Yards, Housing).')}
        mass_balance_checks(in_arg = c(Exc_grazing_N, Exc_yards_N, Exc_housing_N),
                            out_arg = Exc_total_N,
                            step = 1)
      }

      if (self$debug_mode) {
        message("DEBUG: Exc_grazing_N = ", Exc_grazing_N)
        message("DEBUG: Exc_yards_N = ", Exc_yards_N)
        message("DEBUG: Exc_housing_N = ", Exc_housing_N)
      }

      Exc_grazing_TAN = allocate_TAN_excretion(exc_allocation = Exc_grazing_N, f_TAN = input$fraction_TAN)
      Exc_yards_TAN = allocate_TAN_excretion(exc_allocation = Exc_yards_N, f_TAN = input$fraction_TAN)
      Exc_housing_TAN = allocate_TAN_excretion(exc_allocation = Exc_housing_N, f_TAN = input$fraction_TAN)

      # Store excretion results
      results$excretion = list(
        total_N = Exc_total_N,
        grazing_N = Exc_grazing_N,
        yards_N = Exc_yards_N,
        housing_N = Exc_housing_N,
        grazing_TAN = Exc_grazing_TAN,
        yards_TAN = Exc_yards_TAN,
        housing_TAN = Exc_housing_TAN
      )
      if (self$debug_mode) { message("Excretion calculated.") }

      # Calculate yards emissions ----
      if (self$debug_mode) { message("Calculating yards NH3 emissions...") }


      # Calculate yards emissions
      Yards_emNH3 = yards_NH3(yards_tan = Exc_yards_TAN, EF = self$EF_by_stage$yards$NH3)

      # Store yards results
      results$yards = list(
        NH3_N = Yards_emNH3
      )
      if (self$debug_mode) {  message("Yards NH3 emissions calculated.")}

      # Calculate grazing emissions ----
      if (self$debug_mode) {message("Calculating grazing NH3 emissions...")}

      # Calculate grazing emissions
      Grazing_emNH3 = grazing_NH3(grazing_tan = Exc_grazing_TAN, EF = self$EF_by_stage$grazing$NH3)

      # Calculate net grazing
      netGrazing_TAN = net_grazing(manure_grazing = Exc_grazing_TAN, grazing_NH3 = Grazing_emNH3)
      netGrazing_N = net_grazing(manure_grazing = Exc_grazing_N, grazing_NH3 = Grazing_emNH3)

      # Store grazing results
      results$grazing = list(
        NH3_N = Grazing_emNH3,
        net_TAN = netGrazing_TAN,
        net_N = netGrazing_N
      )

      if (self$debug_mode) { message("Grazing NH3 emissions calculated.") }

      # Calculate housing emissions ----
      if (self$debug_mode) { message("Calculating housing emissions...") }

      # Calculate housing deposited
      Housing_slurry_TAN = housing_deposited(housing_nutrient = Exc_housing_TAN, f_man_type = input$fraction_manure_slurry)
      Housing_slurry_N = housing_deposited(housing_nutrient = Exc_housing_N, f_man_type = input$fraction_manure_slurry)

      Housing_solid_TAN = housing_deposited(housing_nutrient = Exc_housing_TAN, f_man_type = input$fraction_manure_solid)
      Housing_solid_N = housing_deposited(housing_nutrient = Exc_housing_N, f_man_type = input$fraction_manure_solid)

      # Calculate housing emissions
      Housing_slurry_emNH3 = housing_NH3(housing_tan = Housing_slurry_TAN, EF = self$EF_by_stage$housing$NH3$slurry)
      Housing_solid_emNH3 = housing_NH3(housing_tan = Housing_solid_TAN, EF = self$EF_by_stage$housing$NH3$solid)

      # Housing bedding (solid manure only)

      # Get f_min from global parameters (default to 0.1 if not available)
      f_imm = 0.0067
      if (!is.null(self$global_params) && !is.null(self$global_params$f_imm)) {
        f_imm = self$global_params$f_imm
      }

      Housing_ex_TAN = ex_housing_solid_TAN(housing_solid_TAN = Housing_solid_TAN,
                                            housing_solid_NH3 = Housing_solid_emNH3,
                                            animal_no = input$animal_number,
                                            bedding_amount = input$bedding_amount,
                                            f_imm = f_imm)
      Housing_ex_N = ex_housing_solid_N(housing_solid_N = Housing_solid_N,
                                        housing_solid_NH3 = Housing_solid_emNH3,
                                        animal_no = input$animal_number,
                                        bedding_amount = input$bedding_amount)


      # Store housing results
      results$housing = list(
        slurry = list(
          TAN = Housing_slurry_TAN,
          N = Housing_slurry_N,
          NH3_N = Housing_slurry_emNH3
        ),
        solid = list(
          TAN = Housing_solid_TAN,
          N = Housing_solid_N,
          Housing_ex_TAN = Housing_ex_TAN,
          Housing_ex_N = Housing_ex_N,
          NH3_N = Housing_solid_emNH3
        ),
        total = list(
          NH3_N = Housing_slurry_emNH3 + Housing_solid_emNH3
        )
      )
      if (self$debug_mode) {message("Housing emissions calculated.") }


      # Calculate storage emissions ----
      if (self$debug_mode) {message("Calculating storage emissions...") }

      # Calculate storage TAN and N
      Storage_solid_TAN = storage_solid(ex_housing_solid = Housing_ex_TAN, f_man_usage_solid = input$fraction_storage_solid)
      Storage_solid_N = storage_solid(ex_housing_solid = Housing_ex_N, f_man_usage_solid = input$fraction_storage_solid)

      Storage_slurry_TAN = storage_slurry(housing_slurry = Housing_slurry_TAN,
                                          housing_slurry_NH3 = Housing_slurry_emNH3,
                                          yard = Exc_yards_TAN,
                                          yard_NH3 = Yards_emNH3,
                                          f_man_usage_slurry = input$fraction_storage_slurry)
      Storage_slurry_N = storage_slurry(housing_slurry = Housing_slurry_N,
                                        housing_slurry_NH3 = Housing_slurry_emNH3,
                                        yard = Exc_yards_N,
                                        yard_NH3 = Yards_emNH3,
                                        f_man_usage_slurry = input$fraction_storage_slurry)

      # Get f_min from global parameters (default to 0.1 if not available)
      f_min = 0.1
      if (!is.null(self$global_params) && !is.null(self$global_params$f_min)) {
        f_min = self$global_params$f_min
      }

      # Update storage slurry TAN to account for mineralization
      updStorage_slurry_TAN = updated_storage_slurry(out_storage_slurry_TAN = Storage_slurry_TAN,
                                                     out_storage_slurry_N = Storage_slurry_N,
                                                     f_min = f_min)

      # Calculate storage emissions for solid manure
      Storage_solid_em = storage_emissions(storage_TAN = Storage_solid_TAN,
                                          EF_NH3 = self$EF_by_stage$storage$NH3$solid,
                                          EF_N2O = self$EF_by_stage$storage$N2O$solid,
                                          EF_NO = self$EF_by_stage$storage$NOx$solid,
                                          EF_N2 = self$EF_by_stage$storage$N2$solid)

      # Calculate storage emissions for slurry
      Storage_slurry_em = storage_emissions(storage_TAN = Storage_slurry_TAN,
                                            EF_NH3 = self$EF_by_stage$storage$NH3$slurry,
                                            EF_N2O = self$EF_by_stage$storage$N2O$slurry,
                                            EF_NO = self$EF_by_stage$storage$NOx$slurry,
                                            EF_N2 = self$EF_by_stage$storage$N2$slurry)

      # Store storage results
      results$storage = list(
        slurry = list(
          TAN = Storage_slurry_TAN,
          updTAN = updStorage_slurry_TAN,
          N = Storage_slurry_N,
          NH3_N = Storage_slurry_em$NH3,
          N2O_N = Storage_slurry_em$N2O,
          NO_N = Storage_slurry_em$NO,
          N2 = Storage_slurry_em$N2
        ),
        solid = list(
          TAN = Storage_solid_TAN,
          N = Storage_solid_N,
          NH3_N = Storage_solid_em$NH3,
          N2O_N = Storage_solid_em$N2O,
          NO_N = Storage_solid_em$NO,
          N2 = Storage_solid_em$N2
        ),
        total = list(
          NH3_N = Storage_solid_em$NH3 + Storage_slurry_em$NH3,
          N2O_N = Storage_solid_em$N2O + Storage_slurry_em$N2O,
          NO_N = Storage_solid_em$NO + Storage_slurry_em$NO,
          N2 = Storage_solid_em$N2 + Storage_slurry_em$N2
        )
      )
      if (self$debug_mode) { message("Storage emissions calculated.") }

      # Calculate digestate ----
      if (self$debug_mode) {  message("Calculating digestate in storage and applied...") }

      # Calculate biogas TAN and N
      Biogas_solid_TAN = storage_solid(ex_housing_solid = Housing_ex_TAN, f_man_usage_solid = input$fraction_biogas_solid)
      Biogas_solid_N = storage_solid(ex_housing_solid = Housing_ex_N, f_man_usage_solid = input$fraction_biogas_solid)

      Biogas_slurry_TAN = storage_slurry(housing_slurry = Housing_slurry_TAN,
                                         housing_slurry_NH3 = Housing_slurry_emNH3,
                                         yard = Exc_yards_N, yard_NH3 = Yards_emNH3,
                                         f_man_usage_slurry = input$fraction_biogas_slurry)
      Biogas_slurry_N = storage_slurry(housing_slurry = Housing_slurry_N,
                                       housing_slurry_NH3 = Housing_slurry_emNH3,
                                       yard = Exc_yards_N,
                                       yard_NH3 = Yards_emNH3,
                                       f_man_usage_slurry = input$fraction_biogas_slurry)


      # Get f_min_digester from global parameters (default to 0067 if not available)
      f_min_digester = 0.0067
      if (!is.null(self$global_params) && !is.null(self$global_params$f_min_digester)) {
        f_min_digester = self$global_params$f_min_digester
      }

      # Calculate digestate application TAN and N ----
      Appl_Digestate_TAN = digested_application_TAN(biogas_slurry_TAN = Biogas_slurry_TAN,
                                                    biogas_solid_TAN = Biogas_solid_TAN,
                                                    biogas_slurry_N = Biogas_slurry_N,
                                                    biogas_solid_N = Biogas_solid_N,
                                                    f_min_digester = f_min_digester,
                                                    EF = self$EF_by_stage$digestate$NH3)
      Appl_Digestate_N = digested_application_N(biogas_slurry_N = Biogas_slurry_N,
                                                biogas_solid_N = Biogas_solid_N,
                                                EF = self$EF_by_stage$digestate$NH3)


      # Store digestate results
      results$digestate = list(
        TAN = Appl_Digestate_TAN,
        N = Appl_Digestate_N
      )
      if (self$debug_mode) { message("Digestate application calculated.") }


      # Calculate direct manure application ----
      if (self$debug_mode) { message("Calculating direct manure application...")}

      # Create lists for manure usage fractions (not vectors)
      f_man_usage_solid = c(input$fraction_biogas_solid, input$fraction_storage_solid)
      f_man_usage_slurry = c(input$fraction_biogas_slurry, input$fraction_storage_slurry)

      DirAppl_slurry_TAN = manure_direct_application(out_storage = Storage_slurry_TAN, c_f_man_usage = f_man_usage_slurry)
      DirAppl_slurry_N = manure_direct_application(out_storage = Storage_slurry_N, c_f_man_usage = f_man_usage_slurry)

      DirAppl_solid_TAN = manure_direct_application(out_storage = Storage_solid_TAN, c_f_man_usage = f_man_usage_solid)
      DirAppl_solid_N = manure_direct_application(out_storage = Storage_solid_N, c_f_man_usage = f_man_usage_solid)

      # Store direct application results
      results$direct_application = list(
        slurry = list(
          TAN = DirAppl_slurry_TAN,
          N = DirAppl_slurry_N
        ),
        solid = list(
          TAN = DirAppl_solid_TAN,
          N = DirAppl_solid_N
        )
      )
      if (self$debug_mode) { message("Direct manure application calculated.") }


      # Calculate manure application ----
      if (self$debug_mode) { message("Calculating application NH3 emissions...")}

      Appl_slurry_TAN = slurry_application(appl_direct_slurry = DirAppl_slurry_TAN, out_storage_slurry = Storage_slurry_TAN, digestate = Appl_Digestate_TAN, storage_em_slurry = Storage_slurry_em)
      Appl_slurry_N = slurry_application(appl_direct_slurry = DirAppl_slurry_N, out_storage_slurry = Storage_slurry_N, digestate = Appl_Digestate_N, storage_em_slurry = Storage_slurry_em)

      Appl_solid_TAN = solid_manure_application(appl_direct_solid = DirAppl_solid_TAN, out_storage_solid = Storage_solid_TAN, storage_em_solid = Storage_solid_em)
      Appl_solid_N = solid_manure_application(appl_direct_solid = DirAppl_solid_N, out_storage_solid = Storage_solid_N, storage_em_solid = Storage_solid_em)

      # Calculate NH3-N emissions for slurry application
      Appl_slurry_emNH3 = application_NH3(manure_appl_TAN = Appl_slurry_TAN, EF = self$EF_by_stage$application$NH3$slurry)
      # Calculate NH3-N emissions for solid manure application
      Appl_solid_emNH3 = application_NH3(manure_appl_TAN = Appl_solid_TAN, EF = self$EF_by_stage$application$NH3$solid)


      ## Net N manure application
      netAppl_slurry_TAN = net_manure_application(manure_appl = Appl_slurry_TAN, appl_NH3 = Appl_slurry_emNH3)
      netAppl_slurry_N = net_manure_application(manure_appl = Appl_slurry_N, appl_NH3 = Appl_slurry_emNH3)

      netAppl_solid_TAN = net_manure_application(manure_appl = Appl_solid_TAN, appl_NH3 = Appl_solid_emNH3)
      netAppl_solid_N = net_manure_application(manure_appl = Appl_solid_N, appl_NH3 = Appl_solid_emNH3)


      # Store application results
      results$application = list(
        slurry = list(
          TAN = Appl_slurry_TAN,
          N = Appl_slurry_N,
          NH3_N = Appl_slurry_emNH3,
          netN = netAppl_slurry_N,
          netTAN = netAppl_slurry_TAN
        ),
        solid = list(
          TAN = Appl_solid_TAN,
          N = Appl_solid_N,
          NH3_N = Appl_solid_emNH3,
          netN = netAppl_solid_N,
          netTAN = netAppl_solid_TAN
        ),
        total = list(
          NH3_N = Appl_slurry_emNH3 + Appl_solid_emNH3
        )
      )
      if (self$debug_mode) { message("Application NH3 emissions calculated.") }

      # Calculate total emissions across all stages
      if (self$debug_mode) {
        message("DEBUG: results$grazing$NH3_N = ", results$grazing$NH3_N)
        message("DEBUG: results$yards$NH3_N = ", results$yards$NH3_N)
        message("DEBUG: results$housing$total$NH3_N = ", results$housing$total$NH3_N)
        message("DEBUG: results$storage$total$NH3_N = ", results$storage$total$NH3_N)
        message("DEBUG: results$application$total$NH3_N = ", results$application$total$NH3_N)
      }


      if (self$debug_mode) {message('Estimating total emissions') }

      # NH3 emissions in kg NH3 (not NH3-N)
      # Note: This assumes that the NH3 values are already converted from NH3-N to NH3
      # If they are not, this calculation needs to be adjusted
      total_NH3_N = results$grazing$NH3 + results$yards$NH3 +
                results$housing$total$NH3 + results$storage$total$NH3 +
                results$application$total$NH3

      total_N2O_N = results$storage$total$N2O_N
      total_NO_N = results$storage$total$NO_N
      total_N2 = results$storage$total$N2

      # Add totals to results
      results$total = list(
        NH3_N = total_NH3_N,
        N2O_N = total_N2O_N,
        NO_N = total_NO_N,
        N2 = total_N2
      )

      if (self$debug_mode) {  message("Inventory MMS calculation completed.") }

      return(results)
    },

    #' @description Print a summary of the MMS object configuration
    #' @return Invisibly returns the MMS object for method chaining
    print_inventory_ini = function() {
      cat("=== Manure Management System (MMS) ===\n")
      cat("Animal type:", self$user_input$animal_type, "\n")
      cat("Number of animals:", self$user_input$animal_number, "head/yr\n\n")

      # File paths
      cat("=== Configuration File Paths ===\n")
      for (name in names(self$config_paths)) {
        path = self$config_paths[[name]]
        exists = file.exists(path)
        if (!exists) {
          # Try with absolute path
          abs_path = file.path(getwd(), path)
          exists = file.exists(abs_path)
          if (exists) {
            path = abs_path
          }
        }
        cat(name, ":", path, "(Exists:", exists, ")\n")
      }

      return(invisible(self))
    },

    #' @description Print method (required for R6 objects)
    #' @return Invisibly returns the MMS object for method chaining
    print = function() {
      self$print_inventory_ini()
      return(invisible(self))
    }
  )
)
