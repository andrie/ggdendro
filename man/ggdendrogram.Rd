\name{ggdendrogram}
\alias{ggdendrogram}
\title{Creates dendrogram plot using ggplot.}
\usage{
  ggdendrogram(data, segments = TRUE, labels = TRUE, rotate
  = FALSE, theme_dendro = TRUE)
}
\arguments{
  \item{data}{Either a dendro object or an object that can
  be coerced to class dendro using the
  \code{\link{dendro_data}} function, i.e. objects of class
  dendrogram, hclust or tree}

  \item{segments}{If TRUE, show line segments}

  \item{labels}{if TRUE, shows segment labels}

  \item{rotate}{if TRUE, rotates plot by 90 degrees}

  \item{theme_dendro}{if TRUE, applies a blank theme to
  plot (see \code{\link{theme_dendro}})}
}
\value{
  A \code{\link[ggplot2]{ggplot}} object
}
\description{
  Creates dendrogram plot using ggplot.
}
\examples{
library(ggplot2)
hc <- hclust(dist(USArrests), "ave")
### demonstrate plotting directly from object class hclust
ggdendrogram(hc, rotate=FALSE)
ggdendrogram(hc, rotate=TRUE)
### demonstrate converting hclust to dendro using dendro_data first
hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate=TRUE) + opts(title="Dendrogram in ggplot2")
}
\seealso{
  \code{\link{dendro_data}}
}

