# Create function
prep_map <- function(x, fill, tooltip, data_id, breaks = NULL, labels = NULL) {

  # Check that lengths of fill, tooltip, and data_id match the number of rows in x
  if (!all(lengths(list(fill, tooltip, data_id)) == nrow(x))) {
    stop("The lengths of 'fill', 'tooltip', and 'data_id' must match the number of rows in the input object.")
  }

  # Assign the vectors to the dataframe
  x$fill <- fill
  x$tooltip <- tooltip
  x$data_id <- data_id

  # Handle breaks and labels for numeric fill
  if (!is.null(breaks) && !is.null(labels)) {
    if (length(breaks) != length(labels) + 1) {
      stop("The length of 'breaks' should be one more than the length of 'labels'.")
    }
    if (!is.numeric(fill)) {
      stop("'fill' must be numeric to cut categories.")
    }
    x$fill <- cut(fill, breaks = breaks, labels = labels, right = TRUE)
  }

  return(x)
}
