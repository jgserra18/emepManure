setwd("c:/Users/João Serra/OneDrive/Modelling/EMEP_MMS")

# Install required packages if not already installed
if (!require("testthat")) install.packages("testthat")
if (!require("yaml")) install.packages("yaml")
if (!require("R6")) install.packages("R6")

# Run the tests
source("tests/test_livestock.R")
