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



#  ------------------------------------------------------------------------



#' Create Dendrograms and Tree Diagrams using 'ggplot2'
#'
#' This package enables you to create dendrograms and tree plots using
#' [ggplot2::ggplot()].
#'
#' The `ggplot2` philosophy is to clearly separate data from the presentation.
#' Unfortunately the plot method for dendrograms ([plot.dendrogram()]) plots
#' directly to a plot device without exposing the data.  The `ggdendro` package
#' resolves this by making available functions that extract the dendrogram plot
#' data.  This data can be used with `ggplot`.
#'
#' The function [dendro_data()] extracts data from different objects that
#' contain dendrogram information.  It is a generic function with methods for:
#'
#' - `hclust`: [dendro_data.hclust()]
#' - dendrogram: [dendro_data.dendrogram()]
#' - regression trees: [dendro_data.tree()]
#' - partition trees: [dendro_data.rpart()]
#' - `agnes` and `diana`: [dendro_data.twins()]
#'
#' These methods create an object of class `dendro`, consisting of a list of
#' data frames.  To extract the relevant data frames from the list, you can use
#' the accessor functions:
#'
#' - [segment()]: the line segment data
#' - [label()]: the text for each end segment
#' - [leaf_label()]: the leaf labels of a tree diagram
#'
#' To plot a dendrogram, either construct a plot with [ggplot2::ggplot()] or use
#' the function [ggdendrogram()].
#' 
#' @name ggdendro-package
#' @aliases ggdendro
#' @docType package
#' @import MASS ggplot2
#' @importFrom grDevices col2rgb dev.cur dev.new
#' @importFrom graphics par points polygon text
#' @importFrom stats as.dendrogram complete.cases is.leaf
#' @importFrom utils head stack tail
#' @author Andrie de Vries - \email{apdevries@@gmail.com}
#' @keywords package
#' @seealso [dendro_data()]
NULL
