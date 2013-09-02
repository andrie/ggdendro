#
#  ggdendro/R/dendro_tree.R by Andrie de Vries  Copyright (C) 2011-2013
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


#' Tools for extracting dendrogram and tree diagram plot data for use with ggplot.
#'
#' This is a set of tools for dendrograms and tree plots using \code{\link[ggplot2]{ggplot}}
#' 
#' The ggplot philosophy is to clearly separate data from the presentation.  Unfortunately the plot method for dendrograms (\code{\link{plot.dendrogram}}) plots directly to a plot device without exposing the data.  The ggdendro package resolves this by making available functions that extract the dendrogram plot data.  This data can be used with ggplot.
#' 
#' \code{\link{dendro_data}} extracts data from several cluster algorithms.  It is a generic function with specific implementations for:
#' 
#' \itemize{
#' \item hclust: \code{\link{dendro_data.hclust}} 
#' \item dendrogram: \code{\link{dendro_data.dendrogram}} 
#' \item regression trees: \code{\link{dendro_data.tree}} 
#' }
#' 
#' These methods create an object of class \code{dendro}, consisting of a list of data.frames.  To extract the relevant data frames from the list, there are three accessor functions:
#'     
#' \itemize{
#' \item{\code{\link{segment}}}{the line segment data}
#' \item{\code{\link{label}}}{the text for each end segment}
#' \item{\code{\link{leaf_label}}}{the leaf labels of a tree diagram}
#' }
#' 
#' To plot a dendrogram, either construct a plot with \code{\link[ggplot2]{ggplot}} or use the function \code{\link{ggdendrogram}}
#' 
#' @name ggdendro-package
#' @aliases ggdendro
#' @docType package
#' @title Tools for creating dendrograms, regresion tree and classification tree plots using ggplot in [R]
#' @import MASS ggplot2
#' @author Andrie de Vries \email{apdevries@@gmail.com}
#' @keywords package
#' @seealso \code{\link{dendro_data}}
NULL

