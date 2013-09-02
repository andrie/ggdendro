\name{rpart_labels}
\alias{rpart_labels}
\title{Extract labels data frame from rpart object for plotting using ggplot.}
\usage{
  rpart_labels(x)
}
\arguments{
  \item{model}{object of class "rpart", e.g. the output of
  rpart()}

  \item{...}{ignored}
}
\value{
  a list with two elements: $labels and $leaf_labels
}
\description{
  This code is modified from the original plot.rpart in
  package rpart.
}
\author{
  Original author Brian Ripley
}
\seealso{
  \code{\link{ggdendrogram}}

  Other dendro_data methods:
  \code{\link{dendro_data.dendrogram}},
  \code{\link{dendro_data.rpart}},
  \code{\link{dendro_data.tree}},
  \code{\link{dendrogram_data}}

  Other rpart functions: \code{\link{dendro_data.rpart}},
  \code{\link{rpart_segments}}
}
\keyword{internal}

