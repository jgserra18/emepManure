library(testthat)

# Set the working directory to the package root
root_dir <- "C:/Users/João Serra/OneDrive/Modelling/rEMEP"

# Run all tests in the testthat directory using the full path
test_dir(file.path(root_dir, "tests", "testthat"), reporter = "summary")
