# dendro_data function
# 
# Author: Andrie
###############################################################################


#' Extract cluster data from a model into a list of data frames.
#' 
#' This function provides a generic mechanism to extract relevant plotting data, 
#' typically line segments and labels, from a variety of cluster models.
#' 
#' In the case of dendrograms and tree models, the function will extract line 
#' segment data and labels.
#' 
#' In the case of kmeans or Mclust models, the function extracts the cluster allocation.
#' 
#' @param model object of type hclust, dendrogram, tree or kmeans
#' @param ... ignored
#' @export dendro_data dendro_data.default
#' @aliases dendro_data.default
#' @return a list of data frames that contain the data appropriate to each cluster model
#' @seealso 
#' There are several implementations for specific cluster algorithms:
#'
#' \itemize{ 
#' \item \code{\link{dendro_data.hclust}}
#' \item \code{\link{dendro_data.dendrogram}}		 
#' \item \code{\link{dendro_data.tree}}	
#' }
dendro_data <- function(model, ...){
	UseMethod("dendro_data", model)
}

dendro_data.default <- function(model, ...){
  x <- class(model)
  stop(paste("No dendro_data method defined for class", x))
  return(NULL)
}

#' Tests whether an object is of class dendro.
#' 
#' Is a dendro?  Tests whether an object is of class dendro.
#' 
#' @param x Object to check
#' @method is dendro
#' @seealso \code{\link{dendro_data}} and \code{\link{ggdendro-package}}
is.dendro <- function(x){
  inherits(x, "dendro")
}

#' Coerces object to class dendro.
#' 
#' Method for coercing object to class dendro.
#' 
#' @param x Object to check
#' @seealso \code{\link{dendro_data}} and \code{\link{ggdendro-package}}
#' @export 
as.dendro <- function(x){
  #stopifnot(inherits(x, list))
  class(x) <- "dendro"
  x
}


#' Returns segment data from dendro object.
#' 
#' Returns segment data from dendro object.
#' 
#' @param x dendro object
#' @export
segment <- function(x){
  x$segments
}

#' Returns label data from dendro object.
#' 
#' Returns label data from dendro object.
#' 
#' @param x dendro object
#' @export
label <- function(x){
  x$labels
}

#' Returns leaf label data from dendro object.
#' 
#' Returns leaf label data from dendro object.
#' 
#' @param x dendro object
#' @export
leaf_label <- function(x){
  x$leaf_labels
}

#' Creates completely blank theme in ggplot.
#' 
#' Sets most of the \code{ggplot} options to blank, by returning blank \code{opts} for the panel grid, panel background, axis title, axis text, axis line and axis ticks.
#' @export
theme_dendro <- function(){
  opts(
      panel.grid.major = theme_blank(),
      panel.grid.minor = theme_blank(),
      panel.background = theme_blank(),
      axis.title.x = theme_text(colour=NA),
      axis.title.y = theme_blank(),
      axis.text.x = theme_blank(),
      axis.text.y = theme_blank(),
      axis.line = theme_blank(),
      axis.ticks = theme_blank()
  )
}

#' Creates dendrogram plot using ggplot.
#' 
#' Creates dendrogram plot using ggplot.
#' 
#' @param data Either a dendro object or an object that can be coerced to class dendro using the \code{\link{dendro_data}} function, i.e. objects of class dendrogram, hclust or tree
#' @param segments If TRUE, show line segments
#' @param labels if TRUE, shows segment labels
#' @param rotate if TRUE, rotates plot by 90 degrees
#' @param theme_dendro if TRUE, applies a blank theme to plot (see \code{\link{theme_dendro}}) 
#' @export
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @seealso \code{\link{dendro_data}}
#' @examples
#' library(ggplot2)
#' hc <- hclust(dist(USArrests), "ave")
#' ### demonstrate plotting directly from object class hclust
#' ggdendrogram(hc, rotate=FALSE)
#' ggdendrogram(hc, rotate=TRUE)
#' ### demonstrate converting hclust to dendro using dendro_data first
#' hcdata <- dendro_data(hc)
#' ggdendrogram(hcdata, rotate=TRUE) + opts(title="Dendrogram in ggplot2")
ggdendrogram <- function(data, segments=TRUE, labels=TRUE, rotate=FALSE, theme_dendro=TRUE){
  stopifnot(require(ggplot2))
  if(!is.dendro(data)) data <- dendro_data(data)
  angle <- ifelse(rotate, 0, 90)
  hjust <- ifelse(rotate, 0, 1)
  p <- ggplot()
  if(segments){
    p <- p + geom_segment(data=segment(data), aes_string(x="x0", y="y0", xend="x1", yend="y1"))
  }
  if(labels){
    p <- p + geom_text(data=label(data), aes_string(x="x", y="y", label="text"), hjust=hjust, size=3, angle=angle)
  }
  if(rotate){
    p <- p + coord_flip()
    p <- p + scale_y_reverse(expand=c(0.2, 0))
  } else {
    p <- p + scale_y_continuous(expand=c(0.2, 0))
  }
  if(theme_dendro) p <- p + theme_dendro()
  p
}

