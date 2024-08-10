\name{prep_map}
\alias{prep_map}
\title{Prepare Spatial Data for Mapping}
\description{
  \code{prep_map} prepares a spatial data frame by adding columns for fill color, tooltips, and data IDs, and optionally categorizes the fill values into specified breaks and labels. This function ensures that the input data is correctly formatted for use in an interactive map.
}
\usage{
prep_map(x, fill, tooltip, data_id, breaks = NULL, labels = NULL)
}
\arguments{
  \item{x}{
    An \code{sf} or data frame object to be prepared for mapping.
  }
  \item{fill}{
    A vector of values used for the fill color in the map. If \code{breaks} and \code{labels} are provided, this should be numeric.
  }
  \item{tooltip}{
    A vector of text strings for tooltips to display on hover. The length should match the number of rows in \code{x}.
  }
  \item{data_id}{
    A vector of unique identifiers for each row in \code{x}. The length should match the number of rows in \code{x}.
  }
  \item{breaks}{
    A numeric vector defining the breakpoints for categorizing the \code{fill} values. Required if \code{labels} is provided.
  }
  \item{labels}{
    A character vector of labels for the categories defined by \code{breaks}. Required if \code{breaks} is provided.
  }
}
\details{
  The function checks that the lengths of the \code{fill}, \code{tooltip}, and \code{data_id} vectors match the number of rows in \code{x}. It assigns these vectors as new columns in \code{x}. If \code{breaks} and \code{labels} are provided, the function categorizes the numeric \code{fill} values accordingly, with the length of \code{breaks} being one more than \code{labels}.
}
\value{
  The modified spatial data frame \code{x} with additional columns for \code{fill}, \code{tooltip}, and \code{data_id}. If \code{breaks} and \code{labels} are specified, the \code{fill} column is categorized accordingly.
}
\examples{
# Example usage of prep_map
library(sf)

# Load or create an sf or data frame object
# df <- ...

# Prepare the data with fill, tooltip, and data_id
# df_prepared <- prep_map(df, fill = c(1, 2, 3), tooltip = c("A", "B", "C"), data_id = c("ID1", "ID2", "ID3"))

# Categorize fill values with breaks and labels
# df_prepared_categorized <- prep_map(df, fill = c(10, 20, 30), tooltip = c("A", "B", "C"), data_id = c("ID1", "ID2", "ID3"), breaks = c(0, 15, 25, 35), labels = c("Low", "Medium", "High"))
}
\seealso{
  \code{\link[sf]{st_simplify}}
}