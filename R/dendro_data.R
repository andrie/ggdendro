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
#' To extract the data for line segments, labels or leaf labels use:
#' \itemize{
#' \item{\code{\link{segment}}}{the line segment data}
#' \item{\code{\link{label}}}{the text for each end segment}
#' \item{\code{\link{leaf_label}}}{the leaf labels of a tree diagram}
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
#' @param segments data.frame with segment data
#' @param labels data.frame with labels data
#' @param leaf_labels data.frame with leaf label data
#' @param class The class of the original model object, e.g. "hclust".  This is used by \code{\link{ggdendrogram}} to determine the angle and jutification of labels
#' @seealso \code{\link{dendro_data}} and \code{\link{ggdendro-package}}
#' @export 
as.dendro <- function(segments, labels, leaf_labels=NULL, class){
  #stopifnot(inherits(x, list))
  if(missing(class)) stop("Missing class in as.dendro")
  x <- list(
      segments=segments,
      labels=labels,
      leaf_labels=leaf_labels,
      class=class
  )
  class(x) <- "dendro"
  x
}


#' Returns segment, label or leaf-label data from dendro object.
#' 
#' \code{segment} extracts line segments, \code{label} extracts labels, and \code{leaf_label} extracts leaf labels from a dendro object.
#' 
#' @param x dendro object
#' @aliases segment label leaf_label
#' @seealso \code{\link{dendro_data}}
#' @export
segment <- function(x){
  x$segments
}

#' @rdname segment
#' @export
label <- function(x){
  x$labels
}

#' @rdname segment
#' @export
leaf_label <- function(x){
  x$leaf_labels
}

