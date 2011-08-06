# package documentation
# 
# Author: Andrie
###############################################################################


#' Tools for creating dendrograms using ggplot.
#'
#' This is a set of tools for dendrograms and tree plots using \code{\link[ggplot2]{ggplot}}
#' 
#' The ggplot philosophy is to clearly separate data from the presentation.  Unfortunately the plot method for dendrograms (\code{\link{plot.dendrogram}} plots directly to a plot device without exposing the data.  The ggdendro package resolves this by making available functions that extract the dendrogram plot data.  This data can be used with ggplot.
#' 
#' \code{\link{dendro_data}} extracts data from several cluster algorithms.  It is a generic function with specific implementations for:
#' 
#' \itemize{
#' \item hclust: \code{\link{dendro_data.hclust}} 
#' \item dendrogram: \code{\link{dendro_data.dendrogram}} 
#' \item tree: \code{\link{dendro_data.tree}} 
#' }
#' 
#' @name ggdendro-package
#' @aliases ggdendro
#' @docType package
#' @title Tools for creating dendrograms and tree plots using ggplot in [R]
#' @author Andrie de Vries \email{andrie.de.vries@@pentalibra.com}
#' @keywords package
#' @seealso \code{\link{dendro_data}}
NULL

