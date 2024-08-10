# Load necessary libraries for testing
library(testthat)
library(sf)
library(ggplot2)
library(ggiraph)

# Define a sample spatial dataframe for testing
sample_data <- st_as_sf(data.frame(
  id = 1:3,
  fill = c("A", "B", "C"),
  tooltip = c("Area 1", "Area 2", "Area 3"),
  data_id = 1:3,
  geometry = st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), st_point(c(2, 2)))
))

# Test 1: Check if the function returns a girafe object
test_that("make_map returns a girafe object", {
  result <- make_map(sample_data, palette = "Set1")
  expect_s3_class(result, "girafe")
})

# Test 2: Check if the function stops when 'fill' column is missing
test_that("make_map throws an error when 'fill' column is missing", {
  sample_data_no_fill <- sample_data
  sample_data_no_fill$fill <- NULL
  expect_error(make_map(sample_data_no_fill, palette = "Set1"),
               "The input object must have a 'fill' column.")
})

# Test 3: Check if the function applies the correct scale function for numeric 'fill' values
test_that("make_map applies correct scale function for numeric 'fill'", {
  sample_data_numeric_fill <- sample_data
  sample_data_numeric_fill$fill <- c(1.0, 2.5, 3.0)
  result <- make_map(sample_data_numeric_fill, palette = "Blues")
  expect_true(grepl("scale_fill_distiller", result$ggobj$scales$scales[[1]]$call))
})

# Test 4: Check if the function applies the correct scale function for factor 'fill' values
test_that("make_map applies correct scale function for factor 'fill'", {
  sample_data_factor_fill <- sample_data
  sample_data_factor_fill$fill <- factor(sample_data_factor_fill$fill)
  result <- make_map(sample_data_factor_fill, palette = "Set1")
  expect_true(grepl("scale_fill_brewer", result$ggobj$scales$scales[[1]]$call))
})

# Test 5: Check if the function works with a custom title and legend title
test_that("make_map sets custom title and legend title", {
  custom_title <- "Custom Map Title"
  custom_legend_title <- "Custom Legend"
  result <- make_map(sample_data, title = custom_title, legend.title = custom_legend_title)
  expect_equal(result$ggobj$labels$title, custom_title)
  expect_equal(result$ggobj$labels$fill, custom_legend_title)
})

# Test 6: Check if the function applies a custom theme
test_that("make_map applies custom theme", {
  custom_theme <- theme_minimal()
  result <- make_map(sample_data, theme = custom_theme)
  expect_identical(result$ggobj$theme, custom_theme)
})