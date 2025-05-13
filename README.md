# rEMEP

An R package for calculating emissions from livestock manure management systems.
![Alt text](images/my-image.png)

## Installation

```r
# Install the devtools package if you haven't already
install.packages("devtools")

# Install rEMEP from GitHub
devtools::install_github("jgserra18/rEMEP")
```

## Basic Usage

```r
library(rEMEP)

# Create a User_input object with default parameters for 100 dairy cows
input = User_input$new(
  animal_type = 'dairy_cattle',
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
```

## Visualization Example

You can easily visualize the results using ggplot2:

```r
library(ggplot2)
library(reshape2)

# NH3 emissions by stage and manure type
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
```
```{r nh3-plot, fig.width=10, fig.height=6, dpi=300}
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

## Advanced Example: Dairy cattle with custom milk production

This example shows how to calculate emissions for dairy cattle with a milk production of 10,000 kg/head/year, which affects the N excretion coefficient:

```r
library(rEMEP)

# Calculate updated N excretion coefficient based on milk production
milk_production <- 10000  # kg milk/(head.year)
updated_excretion <- dairy_excretion_coeff(milk_prod = milk_production)

# Create a User_input object with the updated excretion coefficient
input <- User_input$new(
  animal_type = "dairy_cattle",
  animal_number = 100,
  excretion_coefficient = updated_excretion,
  fraction_grazing = 0.2,
  fraction_yards = 0.05,
  fraction_housing = 0.75,
  fraction_manure_slurry = 0.8,
  fraction_manure_solid = 0.2,
  fraction_storage_slurry = 0.9,
  fraction_biogas_slurry = 0.1,
  fraction_storage_solid = 0.8,
  fraction_biogas_solid = 0.2,
  slurry_crust = TRUE
)

# Create an MMS object
mms <- MMS$new(input)

# Run the inventory calculation
results <- mms$run_inventory()

# View the total emissions
print(paste("Total NH3-N emissions (kg N/year):", results$total$NH3_N))
print(paste("Total N2O-N emissions (kg N/year):", results$total$N2O_N))
print(paste("Total NOx-N emissions (kg N/year):", results$total$NO_N))

# Calculate emission factors per animal
ef_nh3 <- results$total$NH3_N / input$animal_number
print(paste("NH3-N emission factor (kg N/animal/year):", round(ef_nh3, 1)))
```

## Features

- Calculate emissions from different stages of manure management (housing, storage, application, grazing)
- Support for different animal types
- Customizable parameters for detailed emission inventories
- Based on EMEP/EEA emission inventory methodology
