\name{merge_map}
\alias{merge_map}
\title{Merge Spatial Data with a Data Frame}
\description{
  \code{merge_map} merges a spatial object (or data frame) with another data frame based on specified columns. The function converts the spatial object to a data frame for merging, performs the merge, and then converts the result back to an \pkg{sf} object.
}
\usage{
merge_map(x, y, by = NULL, by.x = NULL, by.y = NULL)
}
\arguments{
  \item{x}{
    A spatial object of class \code{sf} or a data frame. If \code{sf}, it will be converted to a data frame before merging.
  }
  \item{y}{
    A data frame to merge with \code{x}.
  }
  \item{by}{
    A character string specifying the column name to use for merging in both \code{x} and \code{y}. If specified, \code{by.x} and \code{by.y} will be set to this value.
  }
  \item{by.x}{
    A character string specifying the column name in \code{x} to use for merging. Required if \code{by} is not provided.
  }
  \item{by.y}{
    A character string specifying the column name in \code{y} to use for merging. Required if \code{by} is not provided.
  }
}
\details{
  The function first checks if \code{x} is an \code{sf} object and converts it to a data frame if necessary. It then checks if \code{by.x} and \code{by.y} are specified, or if \code{by} is provided for both columns. The function performs a merge operation on the specified columns and converts the merged result back to an \code{sf} object. It also checks for unmatched values and issues a warning if discrepancies are found.
}
\value{
  An \code{sf} object containing the merged data from \code{x} and \code{y}. The resulting object retains the spatial attributes from \code{x}.
}
\examples{
# Example usage of merge_map
library(sf)

# Create or load spatial object and data frame
# sf_object <- ...
# df <- ...

# Merge the spatial object with the data frame by a common column
# merged_sf <- merge_map(sf_object, df, by = "id")

# Alternatively, specify columns explicitly
# merged_sf <- merge_map(sf_object, df, by.x = "sf_id", by.y = "df_id")
}
\seealso{
  \code{\link[sf]{st_as_sf}}, \code{\link[base]{merge}}
}