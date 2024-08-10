\name{make_map}
\alias{make_map}
\title{Create an Interactive Map with ggplot2 and ggiraph}
\description{
  \code{make_map} generates an interactive map using \pkg{ggplot2} and \pkg{ggiraph}. The function simplifies a spatial object and creates a map with interactive features, allowing users to zoom in and out.
}
\usage{
make_map(x, palette = NULL, title = NULL, legend.title = NULL, theme = NULL)
}
\arguments{
  \item{x}{
    An \code{sf} object with a 'fill' column. The spatial object should be simplified before use.
  }
  \item{palette}{
    A character vector specifying the color palette to use. If \code{NULL}, defaults to the palette appropriate for the fill type.
  }
  \item{title}{
    A character string specifying the title of the map. If \code{NULL}, no title is added.
  }
  \item{legend.title}{
    A character string specifying the title of the legend. If \code{NULL}, no legend title is added.
  }
  \item{theme}{
    A \code{theme} object to customize the appearance of the map. If \code{NULL}, the default ggplot2 theme is used.
  }
}
\details{
  The function simplifies the spatial object using \code{st_simplify} with a tolerance of 75. It checks for the presence of a 'fill' column in the input object and determines the appropriate fill scale function based on the type of data in the 'fill' column. The map is created with interactive elements, allowing users to view tooltips and zoom in or out.
}
\value{
  An interactive map object of class \code{girafe}, which can be rendered in a web browser or an R Markdown document.
}
\examples{
# Example usage of make_map
library(sf)
library(ggplot2)
library(ggiraph)

# Load or create an sf object with a 'fill' column
# sf_object <- ...

# Create the interactive map
# map <- make_map(sf_object, palette = "Blues", title = "My Interactive Map", legend.title = "Legend", theme = theme_minimal())

# Print the map
# print(map)
}
\seealso{
  \code{\link[ggplot2]{ggplot}}, \code{\link[ggiraph]{girafe}}, \code{\link[sf]{st_simplify}}
}