\name{dendro_data.rpart}
\alias{dendro_data.rpart}
\title{Extract data from classification tree object for plotting using ggplot.}
\usage{
  \method{dendro_data}{rpart} (model, ...)
}
\arguments{
  \item{model}{object of class "tree", e.g. the output of
  tree()}

  \item{...}{ignored}
}
\value{
  A list of three data frames: \item{segments}{a data frame
  containing the line segment data} \item{labels}{a data
  frame containing the label text data}
  \item{leaf_labels}{a data frame containing the leaf label
  text data}
}
\description{
  Extracts data to plot line segments and labels from a
  \code{\link[rpart]{rpart}} classification tree object.
  This data can then be manipulated or plotted, e.g. using
  \code{\link[ggplot2]{ggplot}}.
}
\examples{
require(rpart)
require(ggplot2)
fit <- rpart(Kyphosis ~ Age + Number + Start,   method="class", data=kyphosis)
fitr <- dendro_data(fit)
ggplot() +
    geom_segment(data=fitr$segments, aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_text(data=fitr$labels, aes(x=x, y=y, label=label)) +
    geom_text(data=fitr$leaf_labels, aes(x=x, y=y, label=label)) +
    theme_dendro()
}
\seealso{
  \code{\link{ggdendrogram}}

  Other dendro_data methods:
  \code{\link{dendro_data.dendrogram}},
  \code{\link{dendro_data.tree}},
  \code{\link{dendrogram_data}}, \code{\link{rpart_labels}}

  Other rpart functions: \code{\link{rpart_labels}},
  \code{\link{rpart_segments}}
}

