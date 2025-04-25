#devtools::install_github("jgserra18/rEMEP")
library(rEMEP)
library(ggplot2)

#TODO: 
## Install dependencies
## Provide post-processing [optional; not priority]
## Check documentations of some functions
## Include animal_type tests (fattening_pigs were going wild)

# Create a User_input object with default parameters for 100 dairy cows
input = User_input$new(
  animal_type='dairy_cattle',
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

# Create an MMS object
mms = MMS$new(input)

# Run the inventory calculation
results = mms$run_inventory()

# View the results
print("Total NH3-N emissions (kg N/year):")
print(results$total$NH3_N)

print("Emissions by stage (kg N/year):")
print(paste("Housing:", results$housing$total$NH3_N))
print(paste("Storage:", results$storage$total$NH3_N))
print(paste("Application:", results$application$total$NH3_N))
print(paste("Grazing:", results$grazing$NH3_N))


# NH3 quick plot 
emNH3 = reshape2::melt(
  data.frame(housing_solid = results$housing$solid$NH3_N,
                   housing_slurry = results$housing$slurry$NH3_N,
                   storage_slurry = results$storage$slurry$NH3_N,
                   storage_solid = results$storage$solid$NH3_N,
                   app_solid = results$application$solid$NH3_N,
                   app_slurry = results$application$slurry$NH3_N,
                   yards = results$yards$NH3_N,
                   grazing = results$grazing$NH3_N)
)
emNH3$stage = c('Housing','Housing','Storage','Storage','Application','Application','Yards','Grazing')
emNH3$manure = c('Solid','Slurry','Slurry','Solid','Solid','Slurry', NA, NA)

ggplot(emNH3, aes(x=stage, y=value, fill=manure)) + 
  geom_bar(stat = 'identity',colour='white') + 
  scale_fill_manual(na.value = 'burlywood4', values = c('Slurry'='goldenrod1','Solid'='darkorange4')) + 
  labs(fill=NULL,
       x='Stage', 
       y=expression('NH'[3]~'emissions (kg N-NH'[3]~'yr'^{-1}*')')) + 
  ggthemes::theme_wsj(color = 'white', base_family = 'sans', title_family = 'sans') + 
  theme(
    axis.title = element_text(size=16),
    axis.text = element_text(size=14.5),
    legend.text = element_text(size=13)
  )  
