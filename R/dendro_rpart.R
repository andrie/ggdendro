#
#  ggdendro/R/dendro_rpart.R by Andrie de Vries  Copyright (C) 2011-2015
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


# Classification Tree with rpart


#' Extract data from classification tree object for plotting using ggplot.
#' 
#' Extracts data to plot line segments and labels from a \code{\link[rpart]{rpart}} classification tree object.  This data can then be manipulated or plotted, e.g. using \code{\link[ggplot2]{ggplot}}.
#' 
#' This code is in essence a copy of \code{\link[rpart]{plot.rpart}}, retaining the plot data but without plotting toa plot device.
#' 
#' @param model object of class "tree", e.g. the output of tree()
#' @param uniform if TRUE, uniform vertical spacing of the nodes is used; this may be less cluttered when fitting a large plot onto a page. The default is to use a non-uniform spacing proportional to the error in the fit.
#' @param branch controls the shape of the branches from parent to child node. Any number from 0 to 1 is allowed. A value of 1 gives square shouldered branches, a value of 0 give V shaped branches, with other values being intermediate.
#' @param compress if FALSE, the leaf nodes will be at the horizontal plot coordinates of 1:nleaves. If TRUE, the routine attempts a more compact arrangement of the tree. The compaction algorithm assumes uniform=TRUE; surprisingly, the result is usually an improvement even when that is not the case.
#' @param nspace the amount of extra space between a node with children and a leaf, as compared to the minimal space between leaves. Applies to compressed trees only. The default is the value of branch.
#' @param margin an extra fraction of white space to leave around the borders of the tree. (Long labels sometimes get cut off by the default computation).
#' @param minbranch	set the minimum length for a branch to minbranch times the average branch length. This parameter is ignored if uniform=TRUE. Sometimes a split will give very little improvement, or even (in the classification case) no improvement at all. A tree with branch lengths strictly proportional to improvement leaves no room to squeeze in node labels.
#' @param ... ignored
#' @export
#' @return
#' A list of three data frames:
#' \item{segments}{a data frame containing the line segment data}
#' \item{labels}{a data frame containing the label text data}
#' \item{leaf_labels}{a data frame containing the leaf label text data}
#' 
#' @seealso \code{\link{ggdendrogram}}
#' @family dendro_data methods
#' @family rpart functions
#' @example inst/examples/example_dendro_rpart.R
#' 
dendro_data.rpart <- function(model, uniform = FALSE, branch = 1, compress = FALSE,
                              nspace, margin = 0, minbranch = 0.3, ...){
  x <- model
  if (!inherits(x, "rpart")) stop("Not a legitimate \"rpart\" object")
  if (nrow(x$frame) <= 1L) stop("fit is not a tree, just a root")
  
  if (compress & missing(nspace)) nspace <- branch
  if (!compress) nspace <- -1L     # means no compression
  ## if (dev.cur() == 1L) dev.new() # not needed in R
  
  parms <- list(uniform = uniform, branch = branch, nspace = nspace,
                minbranch = minbranch)
  
  ## define the plot region
  temp <- rpartco(x, parms)
  xx <- temp$x
  yy <- temp$y
  temp1 <- range(xx) + diff(range(xx)) * c(-margin, margin)
  temp2 <- range(yy) + diff(range(yy)) * c(-margin, margin)
#   plot(temp1, temp2, type = "n", axes = FALSE, xlab = "", ylab = "", ...)
  ## Save information per device, once a new device is opened.
   assign(paste0("device", dev.cur()), parms, envir = rpart_env)
  
  # Draw a series of horseshoes or V's, left son, up, down to right son
  #   NA's in the vector cause lines() to "lift the pen"
  node <- as.numeric(row.names(x$frame))
  temp <- rpart.branch(xx, yy, node, branch)
  
#   if (branch > 0) text(xx[1L], yy[1L], "|")
#   lines(c(temp$x), c(temp$y))
#   invisible(list(x = xx, y = yy))
  
  labels <- text.rpart(x)
  
  segments <- rpart_segments(temp)
#   labels <- rpart_labels(xx, ...)
  as.dendro(
    segments = segments,
    labels = labels$labels,
    leaf_labels = labels$leaf_labels,
    class="rpart"
  )
}

#' Extract data frame from rpart object for plotting using ggplot.
#' @param model object of class "tree", e.g. the output of tree()
#' @param ... ignored
#' @keywords internal
#' @seealso \code{\link{ggdendrogram}}
#' @family rpart functions
rpart_segments <- function (x, ...) {
  dat <- data.frame(
    stack(as.data.frame(x$x)), 
    stack(as.data.frame(x$y))
  )[, c("ind", "values", "values.1")]
  
  dat <- cbind(head(dat, -1), tail(dat, -1))
  dat <- dat[complete.cases(dat), -4]
  names(dat) <- c("n", "x", "y", "xend", "yend")
  dat
  
}


#' Extract labels data frame from rpart object for plotting using ggplot.
#' 
#' This code is modified from the original plot.rpart in package rpart.
#' @param model object of class "rpart", e.g. the output of rpart()
#' @param ... ignored
#' @return a list with two elements: $labels and $leaf_labels
#' @author Original author Brian Ripley
#' @keywords internal
#' @seealso \code{\link{ggdendrogram}}
#' @family dendro_data methods
#' @family rpart functions
rpart_labels <- function (x) 
{
  labelSplits <- labelLeaves <- NULL
  list(
    labels=labelSplits,
    leaf_labels=labelLeaves
  )
}

