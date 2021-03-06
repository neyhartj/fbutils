% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fb_spatial_adj.R
\name{fb_spatial_adj}
\alias{fb_spatial_adj}
\title{Spatial adjustment of observations}
\usage{
fb_spatial_adj(fbt, traits, checks, grid.size = NULL,
  max.grid.size = NULL, use.rel.eff = TRUE)
}
\arguments{
\item{fbt}{A Field Book Table object.}

\item{traits}{Traits to be adjusted. Defaults to all numeric traits.}

\item{checks}{A \code{character} of field.book.table entry names designated
as checks.}

\item{grid.size}{A nested \code{list} of grid dimensions that define the 
moving average grid. If \code{grid.size} is \code{NULL}, the grid size is 
optimized. See \emph{Details} for more information.}

\item{max.grid.size}{A nested \code{list}, similar to \code{grid.size}, but 
instead defining the maximum grid dimensions to use when optimizing the 
grid size. Here the components \code{grid.rows}, \code{grid.cols}, and 
\code{grid.layers} are integers defining the maximum rows, columns, and diagonal
layers to use, respectively. Use this option if the field dimensions are large.
This argument is ignored if \code{grid.size} is passed.}

\item{use.rel.eff}{If \code{TRUE}, the relative efficiency is
used to determine whether the adjusted data for a trait replaces the raw data
in the fbt object. If \code{FALSE}, the adjusted data replaces the raw data.}
}
\value{
A list with two elements:

\describe{
  \item{fbt}{An fbt object with adjusted phenotypic values replacing the 
  original values, if applicable.}
  \item{summary}{A list with 2 elements: the first returns the grid size 
  used for each trait. The second returns summary statistics of the adjustment
  procedure, including the within-environment variance, relative efficiency,
  and whether the adjusted values were returned in place of the original values.}
}
}
\description{
Uses a moving grid to adjust the phenotypic observations recorded in a Field Book 
table. This function wraps around functions provided in the \code{\link[mvngGrAd]{mvngGrAd}}
package.
}
\details{
This function uses the moving average procedure implemented in \code{\link[mvngGrAd]{mvngGrAd}}
to spatially adjust the raw phenotypic observations from a grid. Briefly,
the function uses the mean of observations surrounding a particularly plot
in a field as a covariate to calculate the adjusted phenotypic value.

One can define the dimensions of the grid by passing a list to the \code{grid.size}
argument. The first layer of the list must be the same length as the number 
of traits, and the names of this layer must be the trait names. The second 
layer of the list must have the components \code{grid.rows}, \code{grid.cols}, 
and \code{grid.layers}. \code{grid.rows} and \code{grid.cols} define the
number of \emph{field} rows and columns covered by the grid, respectively,
while \code{grid.layers} defines the number of diagonal plots.

Alternatively, one can optimize the grid size for each trait. Grid optimization
iterates over all possible grid dimensions and identifies the grid size that
maximizes the correlation between the adjusted phenotypic values and the mean
of the phenotypic values within that grid.

Finally, the adjusted phenotypic values can be kept or ignored depending 
on whether the within-environment variance (i.e. \eqn{V_R}) is reduced after
adjustment. This function uses the individuals specified in \code{checks} to
calculate this variance. Generally, traits that are less heritable tend to 
benefit from spatial adjustment.
}
\examples{
data("fbt_sample")

# Specify a grid size
grid.size <- list(SpatialGradient = list(grid.rows = 5, grid.cols = 5, grid.layers = 5))

adj_out <- fb_spatial_adj(fbt = fbt_sample, traits = "SpatialGradient", 
                          checks = c("Kharkof", "TAM 107", "Scout 66"), 
                          grid.size = grid.size)                         
                          
\dontrun{

# Run spatial adjustment without conditions
adj_out <- fb_spatial_adj(fbt = fbt_sample, traits = "SpatialGradient", 
                          checks = c("Kharkof", "TAM 107", "Scout 66"))
                          
# Observe the output

                          
# Restrict the grid size
max.grid.size <- list(SpatialGradient = list(grid.rows = 5, grid.cols = 5, grid.layers = 5))

# Re-run spatial adjustment
adj_out <- fb_spatial_adj(fbt = fbt_sample, traits = "SpatialGradient", 
                          checks = c("Kharkof", "TAM 107", "Scout 66"), 
                          max.grid.size = max.grid.size)
   
}                       

}
