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

