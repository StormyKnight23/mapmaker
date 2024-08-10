# Load libraries
library(sf)

# Create function
merge_map <- function(x, y, by = NULL, by.x = NULL, by.y = NULL) {

  # Ensure x and y are data frames
  x <- if (inherits(x, "sf")) as.data.frame(x) else x
  y <- if (inherits(y, "sf")) as.data.frame(y) else y

  # If 'by' is provided, use it for both by.x and by.y
  if (!is.null(by)) {
    by.x <- by
    by.y <- by
  }

  # Check if specified columns exist in the data frames
  if (is.null(by.x) || is.null(by.y)) {
    stop("Both 'by.x' and 'by.y' must be specified, or 'by' must be provided.")
  }
  if (!by.x %in% names(x)) {
    stop(paste("Column", by.x, "not found in the first dataset (x)."))
  }
  if (!by.y %in% names(y)) {
    stop(paste("Column", by.y, "not found in the second dataset (y)."))
  }

  # Check if all values in by.x exist in by.y
  unmatched_values <- setdiff(x[[by.x]], y[[by.y]])
  if (length(unmatched_values) > 0) {
    warning("The following values in ", by.x, " (x) are not found in ", by.y, " (y):\n",
            paste(unmatched_values, collapse = ",\n"))
  }

  # Merge the data frames and convert back to sf object
  merged_data <- merge(x, y, by.x = by.x, by.y = by.y, all.x = TRUE)
  merged_data <- st_as_sf(merged_data)

  return(merged_data)
}