\name{dendro_data.dendrogram}
\alias{dendro_data.dendrogram}
\alias{dendro_data.hclust}
\title{Extract line segment and label data from dendrogram or hclust object.}
\usage{
  \method{dendro_data}{dendrogram} (model, type =
  c("rectangle", "triangle"), ...)
}
\arguments{
  \item{model}{object of class "dendrogram", e.g. the
  output of as.dendrogram()}

  \item{type}{The type of plot, indicating the shape of the
  dendrogram.  "rectangle" will draw rectangular lines,
  while "triangle" will draw triangular lines.}

  \item{...}{ignored}
}
\value{
  A list with the following elements: \item{segments}{Line
  segment data} \item{labels}{Label data}
}
\description{
  Extract line segment and label data from dendrogram or
  hclust object.  Results are stored in a list of data
  frames containing line segment data and label data.
}
\examples{
require(ggplot2)
#
# Demonstrate dendro_data.dendrogram
#
hc <- hclust(dist(USArrests), "ave")
dhc <- as.dendrogram(hc)
# Rectangular lines
ddata <- dendro_data(dhc, type="rectangle")
ggplot(segment(ddata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + theme_dendro()
# Triangular lines
ddata <- dendro_data(dhc, type="triangle")
ggplot(segment(ddata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + theme_dendro()
#
# Demonstrate dendro_data.hclust
#
require(ggplot2)
hc <- hclust(dist(USArrests), "ave")
# Rectangular lines
hcdata <- dendro_data(hc, type="rectangle")
ggplot(segment(hcdata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + theme_dendro()
# Triangular lines
hcdata <- dendro_data(hc, type="triangle")
ggplot(segment(hcdata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
theme_dendro()
}
\seealso{
  \code{\link{dendro_data}}, \code{\link{ggdendrogram}}
}

