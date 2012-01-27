\name{tree_labels}
\alias{tree_labels}
\title{Extract labels data frame from tree object for plotting using ggplot.}
\usage{
  tree_labels(model, ...)
}
\arguments{
  \item{model}{object of class "tree", e.g. the output of
  tree()}

  \item{...}{ignored}
}
\value{
  a list with two elements: $labels and $leaf_labels
}
\description{
  Extract labels data frame from tree object for plotting
  using ggplot.
}
\seealso{
  \code{\link{ggdendrogram}}

  Other tree functions: \code{\link{dendro_data.tree}},
  \code{\link{get_data_tree_leaf_labels}},
  \code{\link{tree_segments}}
}
\keyword{internal}

