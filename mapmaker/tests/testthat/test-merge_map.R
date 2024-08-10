# Load necessary libraries for testing
library(testthat)
library(sf)

# Define sample spatial data for testing
sample_data_x <- st_as_sf(data.frame(
  id_x = 1:3,
  value = c("A", "B", "C"),
  geometry = st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), st_point(c(2, 2)))
))

sample_data_y <- st_as_sf(data.frame(
  id_y = c(1, 2),
  description = c("Area A", "Area B"),
  geometry = st_sfc(st_point(c(0, 0)), st_point(c(1, 1)))
))

# Test 1: Check if the function returns an sf object
test_that("merge_map returns an sf object", {
  result <- merge_map(sample_data_x, sample_data_y, by.x = "id_x", by.y = "id_y")
  expect_s3_class(result, "sf")
})

# Test 2: Check if the function throws an error when columns are missing
test_that("merge_map throws an error for missing columns", {
  expect_error(merge_map(sample_data_x, sample_data_y, by.x = "missing_x", by.y = "id_y"),
               "Column missing_x not found in the first dataset (x).")
  expect_error(merge_map(sample_data_x, sample_data_y, by.x = "id_x", by.y = "missing_y"),
               "Column missing_y not found in the second dataset (y).")
})

# Test 3: Check if the function issues a warning for unmatched values
test_that("merge_map issues a warning for unmatched values", {
  expect_warning(merge_map(sample_data_x, sample_data_y, by.x = "value", by.y = "description"),
                 "The following values in value (x) are not found in description (y):\nC")
})

# Test 4: Check if the function merges data frames correctly
test_that("merge_map merges data frames correctly", {
  result <- merge_map(sample_data_x, sample_data_y, by.x = "id_x", by.y = "id_y")
  expected_result <- st_as_sf(data.frame(
    id_x = c(1, 2, 3),
    value = c("A", "B", "C"),
    id_y = c(1, 2, NA),
    description = c("Area A", "Area B", NA),
    geometry = st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), st_point(c(2, 2)))
  ))
  expect_equal(result, expected_result)
})

# Test 5: Check if the function handles the 'by' parameter correctly
test_that("merge_map handles 'by' parameter correctly", {
  sample_data_x_2 <- st_as_sf(data.frame(
    common_id = 1:3,
    value = c("A", "B", "C"),
    geometry = st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), st_point(c(2, 2)))
  ))

  sample_data_y_2 <- st_as_sf(data.frame(
    common_id = c(1, 2),
    description = c("Area A", "Area B"),
    geometry = st_sfc(st_point(c(0, 0)), st_point(c(1, 1)))
  ))

  result <- merge_map(sample_data_x_2, sample_data_y_2, by = "common_id")
  expected_result <- st_as_sf(data.frame(
    common_id = c(1, 2, 3),
    value = c("A", "B", "C"),
    description = c("Area A", "Area B", NA),
    geometry = st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), st_point(c(2, 2)))
  ))

  expect_equal(result, expected_result)
})