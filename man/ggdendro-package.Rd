% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggdendro-package.R
\docType{package}
\name{ggdendro-package}
\alias{ggdendro}
\alias{ggdendro-package}
\title{Create Dendrograms and Tree Diagrams using 'ggplot2'}
\description{
This package enables you to create dendrograms and tree plots using
\code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}.
}
\details{
The \code{ggplot2} philosophy is to clearly separate data from the presentation.
Unfortunately the plot method for dendrograms (\code{\link[=plot.dendrogram]{plot.dendrogram()}}) plots
directly to a plot device without exposing the data.  The \code{ggdendro} package
resolves this by making available functions that extract the dendrogram plot
data.  This data can be used with \code{ggplot}.

The function \code{\link[=dendro_data]{dendro_data()}} extracts data from different objects that
contain dendrogram information.  It is a generic function with methods for:
\itemize{
\item \code{hclust}: \code{\link[=dendro_data.hclust]{dendro_data.hclust()}}
\item dendrogram: \code{\link[=dendro_data.dendrogram]{dendro_data.dendrogram()}}
\item regression trees: \code{\link[=dendro_data.tree]{dendro_data.tree()}}
\item partition trees: \code{\link[=dendro_data.rpart]{dendro_data.rpart()}}
\item \code{agnes} and \code{diana}: \code{\link[=dendro_data.twins]{dendro_data.twins()}}
}

These methods create an object of class \code{dendro}, consisting of a list of
data frames.  To extract the relevant data frames from the list, you can use
the accessor functions:
\itemize{
\item \code{\link[=segment]{segment()}}: the line segment data
\item \code{\link[=label]{label()}}: the text for each end segment
\item \code{\link[=leaf_label]{leaf_label()}}: the leaf labels of a tree diagram
}

To plot a dendrogram, either construct a plot with \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} or use
the function \code{\link[=ggdendrogram]{ggdendrogram()}}.
}
\seealso{
\code{\link[=dendro_data]{dendro_data()}}
}
\author{
Andrie de Vries - \email{apdevries@gmail.com}
}
\keyword{package}
