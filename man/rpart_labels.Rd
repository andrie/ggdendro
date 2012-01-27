\name{rpart_labels}
\alias{rpart_labels}
\title{Extract labels data frame from rpart object for plotting using ggplot.}
\usage{
  rpart_labels(model, splits = TRUE, label, FUN = text,
    all = FALSE, pretty = NULL,
    digits = getOption("digits") - 3, use.n = FALSE,
    fancy = FALSE, fwidth = 0.8, fheight = 0.8, ...)
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
  Extract labels data frame from rpart object for plotting
  using ggplot.
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

