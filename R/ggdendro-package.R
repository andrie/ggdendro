# package documentation
# 
# Author: Andrie
###############################################################################


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
#' \item classification trees: \code{\link{dendro_data.rpart}} 
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
#' To plot a dendrogram, either construct a plot with \code{\link[ggplot2]{ggplot}} or use the function \code{ggdendrogram}
#' 
#' @name ggdendro-package
#' @aliases ggdendro
#' @docType package
#' @import ggplot2
#' @title Tools for creating dendrograms, regresion tree and classification tree plots using ggplot in [R]
#' @author Andrie de Vries \email{andrie.de.vries@@pentalibra.com}
#' @keywords package
#' @seealso \code{\link{dendro_data}}
NULL

