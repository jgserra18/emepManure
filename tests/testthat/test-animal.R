test_that(".standardize_animal_type correctly standardizes animal types", {
  # We need to mock the config directory and YAML file for testing
  # This is a simplified approach for testing purposes
  mock_config_dir = tempdir()
  mock_yaml_content = "
livestock_conversions:
  dairy_cattle:
    - dairy_cattle_tied
    - dairy_cattle
  birds:
    - laying_hens
    - broilers
"
  mock_yaml_file = file.path(mock_config_dir, "livestock_class_conversion.yaml")
  writeLines(mock_yaml_content, mock_yaml_file)
  
  # Test with a standard type
  expect_equal(.standardize_animal_type("dairy_cattle", mock_config_dir), "dairy_cattle")
  
  # Test with a non-standard type that should be converted
  expect_equal(.standardize_animal_type("dairy_cattle_tied", mock_config_dir), "dairy_cattle")
  
  # Test with a bird type
  expect_equal(.standardize_animal_type("laying_hens", mock_config_dir), "birds")
  
  # Test with an unknown type (should return the original type with a warning)
  expect_warning(.standardize_animal_type("unknown_animal", mock_config_dir))
  expect_equal(suppressWarnings(.standardize_animal_type("unknown_animal", mock_config_dir)), "unknown_animal")
  
  # Clean up
  unlink(mock_yaml_file)
})

test_that(".extract_animal_data correctly extracts data for animal types", {
  # Create a mock data structure
  mock_data = list(
    dairy_cattle = list(value = "dairy_cattle_value"),
    birds = list(value = "birds_value")
  )
  
  # Create mock config directory and YAML file
  mock_config_dir = tempdir()
  mock_yaml_content = "
livestock_conversions:
  dairy_cattle:
    - dairy_cattle_tied
    - dairy_cattle
  birds:
    - laying_hens
    - broilers
"
  mock_yaml_file = file.path(mock_config_dir, "livestock_class_conversion.yaml")
  writeLines(mock_yaml_content, mock_yaml_file)
  
  # Test with direct match
  expect_equal(.extract_animal_data(mock_data, "dairy_cattle", "dairy_cattle", mock_config_dir)$value, 
               "dairy_cattle_value")
  
  # Test with standardized match
  expect_equal(.extract_animal_data(mock_data, "dairy_cattle_tied", "dairy_cattle", mock_config_dir)$value, 
               "dairy_cattle_value")
  
  # Test with bird category
  expect_equal(.extract_animal_data(mock_data, "laying_hens", "birds", mock_config_dir)$value, 
               "birds_value")
  
  # Test with no match
  expect_null(.extract_animal_data(mock_data, "unknown_animal", "unknown_animal", mock_config_dir))
  
  # Clean up
  unlink(mock_yaml_file)
})
