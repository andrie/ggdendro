#
#  ggdendro/R/dendro_tree.R by Andrie de Vries  Copyright (C) 2011-2015
#  Contains code adapted from stats/hclust.R Copyright (C) 1995-2015 The R Core Team
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




#' Extract line segment and label data from dendrogram or hclust object.
#'
#' Extract line segment and label data from [stats::dendrogram()] or
#' [stats::hclust()] object.  The resulting object is a list of data frames
#' containing line segment data and label data.
#'
#' @param model object of class "dendrogram", e.g. the output of as.dendrogram()
#'
#' @param type The type of plot, indicating the shape of the dendrogram.
#'   "rectangle" will draw rectangular lines, while "triangle" will draw
#'   triangular lines.
#' 
#' @param ... ignored
#' 
#' 
#' @export
#' @return
#' A list with components:
#' \item{segments}{Line segment data}
#' \item{labels}{Label data}
#'
#' @seealso [ggdendrogram()]
#' @rdname dendro_data
#' @family dendro_data methods
#' @family dendrogram/hclust functions
#' @example inst/examples/example_dendro_data.R
#'
dendro_data.dendrogram <- function(model, type = c("rectangle", "triangle"), ...) {
  hcdata <- dendrogram_data(model, type = type, ...)
  as.dendro(
    segments = hcdata$segments,
    labels = hcdata$labels,
    class = "dendrogram"
  )
}

#' @rdname dendro_data
#' @export
dendro_data.hclust <- function(model, type = c("rectangle", "triangle"), ...) {
  dhc <- as.dendrogram(model)
  hcdata <- dendrogram_data(dhc, type = type, ...)
  as.dendro(
    segments = hcdata$segments,
    labels = hcdata$labels,
    class = "hclust"
  )
}

#' @rdname dendro_data
#' @example inst/examples/example_dendro_twins.R
#' @export
dendro_data.twins <- function(model, type = c("rectangle", "triangle"), ...) {
  dhc <- as.dendrogram(model)
  hcdata <- dendrogram_data(dhc, type = type, ...)
  as.dendro(
    segments = hcdata$segments,
    labels = hcdata$labels,
    class = "hclust"
  )
}





#' Extract data frame from dendrogram object for plotting using ggplot.
#'
#' Extract data frame from dendrogram object for plotting using ggplot
#'
#' @param x object of class "dendrogram", e.g. the output of as.dendrogram()
#' 
#' @param type The type of plot, indicating the shape of the dendrogram:
#'   "rectangle" will draw rectangular lines, while "triangle" will draw
#'   triangular lines.
#' 
#' @param ... ignored
#' @seealso [ggdendrogram()]
#' @family dendro_data methods
#' @family dendrogram/hclust functions
#' @keywords internal
dendrogram_data <- function(x, type = c("rectangle", "triangle"), ...) {

  # Initialise variables that used to be in parameter list
  leaflab <- "perpendicular"
  center <- FALSE
  xlab <- ""
  ylab <- ""
  horiz <- FALSE
  # frame.plot <- FALSE
  xaxt <- "n"
  yaxt <- "s"
  nodePar <- NULL
  edgePar <- list()
  dLeaf <- NULL
  edge.root <- is.leaf(x) || !is.null(attr(x, "edgetext"))

  type <- match.arg(type)
  # leaflab <- match.arg(leaflab)
  hgt <- attr(x, "height")
  if (edge.root && is.logical(edge.root)) {
    edge.root <- 0.0625 * if (is.leaf(x)) 1 else hgt
  }
  mem.x <- .memberDend(x)
  yTop <- hgt + edge.root
  if (center) {
    x1 <- 0.5
    x2 <- mem.x + 0.5
  }
  else {
    x1 <- 1
    x2 <- mem.x
  }
  xl. <- c(x1 - 1 / 2, x2 + 1 / 2)
  yl. <- c(0, yTop)
  # 	if (missing(xlim) || is.null(xlim)) xlim <- xl.
  # 	if (missing(ylim) || is.null(ylim)) ylim <- yl.
  # 	plot(0, xlim = xlim, ylim = ylim, type = "n", xlab = xlab,
  # 			ylab = ylab, xaxt = xaxt, yaxt = yaxt, frame.plot = frame.plot,
  # 			...)
  # 	if (is.null(dLeaf))
  # 		dLeaf <- 0.75 * (if (horiz)
  # 						strwidth("w")
  # 					else strheight("x"))
  if (edge.root) {
    # 		x0 <- stats:::plotNodeLimit(x1, x2, x, center)$x
    # 		(x0, hgt, x0, yTop)
    if (!is.null(et <- attr(x, "edgetext"))) {
      my <- mean(hgt, yTop)
      # 			text(x0, my, et)
    }
  }
  ret <- plotNode(x1, x2, x,
    type = type, center = center, leaflab = leaflab,
    dLeaf = dLeaf, nodePar = nodePar, edgePar = edgePar, horiz = FALSE
  )
  ret$segments <- as.data.frame(matrix(
    ret$segments, ncol = 4, byrow = TRUE, 
    dimnames = list(NULL, c("x", "y", "xend", "yend"))
    ))
  
  ret$labels <- cbind(
    as.data.frame(matrix(ret$labels$xy, ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x", "y")))),
    data.frame(label = ret$labels$text)
  )
  
  ret
}


