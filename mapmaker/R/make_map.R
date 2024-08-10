# Load libraries
library(sf)
library(ggplot2)
library(ggiraph)

# Create function
make_map <- function(x, palette = NULL, title = NULL, legend.title = NULL, theme = NULL) {

  # Simplify the spatial object
  x <- st_simplify(x, dTolerance = 75)

  # Check if 'fill' column exists
  if (!"fill" %in% names(x)) {
    stop("The input object must have a 'fill' column.")
  }

  # Determine the appropriate scale function based on 'fill' type
  scale_fill <- if (is.numeric(x$fill)) {
    scale_fill_distiller(palette = palette)
  } else {
    scale_fill_brewer(palette = palette)
  }

  # Create ggplot with interactive elements
  gg <- ggplot(x) +
    geom_sf_interactive(aes(
      fill = fill,
      tooltip = tooltip,
      data_id = data_id
    )) +
    scale_fill +
    labs(x = "Latitude",
         y = "Longitude",
         title = title,
         fill = legend.title) +
    theme

  # Convert to girafe with custom zoom options
  giraf <- girafe(ggobj = gg)
  giraf <- girafe_options(giraf, opts_zoom(min = 0.7, max = 2))

  # Return the interactive map
  return(giraf)
}
