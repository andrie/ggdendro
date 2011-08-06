\name{dendro_data.tree}
\alias{dendro_data.tree}
\title{Extract data from tree object for plotting using ggplot.}
\usage{
  \method{dendro_data}{tree} (model, ...)
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
  tree() object.  This data can then be manipulated or
  plotted, e.g. using ggplot().
}
\examples{
require(tree)
require(ggplot2)
data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
tree_data <- dendro_data(cpus.ltr)
ggplot(segment(tree_data)) +
geom_segment(aes(x=x, y=y, xend=xend, yend=yend, size=n),
colour="blue", alpha=0.5) +
scale_size("n", to=c(0, 3)) +
geom_text(data=label(tree_data),
aes(x=x, y=y, label=label), vjust=-0.5, size=4) +
geom_text(data=leaf_label(tree_data),
aes(x=x, y=y, label=label), vjust=0.5, size=3)
}
\seealso{
  \code{\link{dendro_data}}
}

