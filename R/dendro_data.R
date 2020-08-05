#
#  ggdendro/R/dendro_tree.R by Andrie de Vries  Copyright (C) 2011-2015
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#


#' Extract cluster data from a model into a list of data frames.
#'
#' This function provides a generic mechanism to extract relevant plotting data,
#' typically line segments and labels, from a variety of cluster models.
#'
#' For [stats::dendrogram()] and `tree::tree()` models, extracts line segment
#' data and labels.
#'
#'
#' @param model object of type [stats::hclust()], [stats::dendrogram()] or
#'   `tree::tree()`
#'
#' @param ... ignored
#'
#' @export dendro_data dendro_data.default
#'
#' @return a list of data frames that contain the data appropriate to each
#'   cluster model
#'
#' @seealso There are several implementations for specific cluster algorithms:
#'
#' \itemize{ \item [dendro_data.hclust()] \item [dendro_data.dendrogram()] \item
#' [dendro_data.tree()] \item [dendro_data.rpart()] } To extract the data for
#' line segments, labels or leaf labels use: \itemize{ \item{[segment()]}: {the
#' line segment data} \item{[label()]}: {the text for each end segment}
#' \item{[leaf_label()]}: {the leaf labels of a tree diagram} }
dendro_data <- function(model, ...) {
  UseMethod("dendro_data", model)
}

#' @rdname dendro_data
#' @export
dendro_data.default <- function(model, ...) {
  x <- class(model)
  stop(paste("No dendro_data method defined for class", x))
  return(NULL)
}

#' Tests whether an object is of class dendro.
#'
#' Is a dendro?  Tests whether an object is of class dendro.
#'
#' @param x Object to check
#' @export
#' @seealso [dendro_data()] and [ggdendro-package()]
is.dendro <- function(x) {
  inherits(x, "dendro")
}

#' Coerces object to class dendro.
#'
#' Method for coercing object to class dendro.
#'
#' @param segments data.frame with segment data
#'
#' @param labels data.frame with labels data
#'
#' @param leaf_labels data.frame with leaf label data
#'
#' @param class The class of the original model object, e.g. "hclust".  This is
#'   used by [ggdendrogram()] to determine the angle and justification
#'   of labels
#'
#' @seealso [dendro_data()] and [ggdendro-package()]
#'
#' @export
as.dendro <- function(segments, labels, leaf_labels = NULL, class) {
  # stopifnot(inherits(x, list))
  if (missing(class)) stop("Missing class in as.dendro")
  x <- list(
    segments = segments,
    labels = labels,
    leaf_labels = leaf_labels,
    class = class
  )
  class(x) <- "dendro"
  x
}


#' Returns segment, label or leaf-label data from dendro object.
#'
#' `segment` extracts line segments, `label` extracts labels, and `leaf_label`
#' extracts leaf labels from a dendro object.
#'
#' @param x dendro object
#' @aliases segment label leaf_label
#' @seealso [dendro_data()]
#' @export
segment <- function(x) {
  x$segments
}

#' @rdname segment
#' @export
label <- function(x) {
  x$labels
}

#' @rdname segment
#' @export
leaf_label <- function(x) {
  x$leaf_labels
}
