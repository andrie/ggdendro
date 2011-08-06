\docType{package}
\name{ggdendro-package}
\alias{ggdendro}
\alias{ggdendro-package}
\title{Tools for creating dendrograms and tree plots using ggplot in [R]}
\description{
  Tools for creating dendrograms using ggplot.
}
\details{
  This is a set of tools for dendrograms and tree plots
  using \code{\link[ggplot2]{ggplot}}

  The ggplot philosophy is to clearly separate data from
  the presentation.  Unfortunately the plot method for
  dendrograms (\code{\link{plot.dendrogram}} plots directly
  to a plot device without exposing the data.  The ggdendro
  package resolves this by making available functions that
  extract the dendrogram plot data.  This data can be used
  with ggplot.

  \code{\link{dendro_data}} extracts data from several
  cluster algorithms.  It is a generic function with
  specific implementations for:

  \itemize{ \item hclust: \code{\link{dendro_data.hclust}}
  \item dendrogram: \code{\link{dendro_data.dendrogram}}
  \item tree: \code{\link{dendro_data.tree}} }
}
\author{
  Andrie de Vries \email{andrie.de.vries@pentalibra.com}
}
\seealso{
  \code{\link{dendro_data}}
}
\keyword{package}

