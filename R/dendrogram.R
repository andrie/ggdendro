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
#' Extract line segment and label data from \code{\link[stats]{dendrogram}} or \code{\link[stats]{hclust}} object.  The resulting object is a list of data frames containing line segment data and label data.
#' 
# @param model object of class "dendrogram", e.g. the output of as.dendrogram()
#' @param type The type of plot, indicating the shape of the dendrogram.  "rectangle" will draw rectangular lines, while "triangle" will draw triangular lines.
# @param ... ignored
#' @export
#' @return
#' A list with components:
#' \item{segments}{Line segment data}
#' \item{labels}{Label data}
#' 
#' @seealso \code{\link{ggdendrogram}}
#' @rdname dendro_data
#' @family dendro_data methods
#' @family dendrogram/hclust functions
#' @example inst/examples/example_dendro_data.R
#' 
dendro_data.dendrogram <- function (model, type = c("rectangle", "triangle"), ...){
	hcdata <- dendrogram_data(model, type=type, ...)
	as.dendro(
      segments = hcdata$segments,
  		labels = hcdata$labels,
      class="dendrogram"
  	)
} 

#' @rdname dendro_data
#' @export
dendro_data.hclust <- function (model, type = c("rectangle", "triangle"), ...){
  dhc <- as.dendrogram(model)
  hcdata <- dendrogram_data(dhc, type=type, ...)
  as.dendro(
      segments = hcdata$segments,
      labels = hcdata$labels,
      class="hclust"
  )
} 

#' @rdname dendro_data
#' @example inst/examples/example_dendro_twins.R
#' @export
dendro_data.twins <- function (model, type = c("rectangle", "triangle"), ...){
  dhc <- as.dendrogram(model)
  hcdata <- dendrogram_data(dhc, type=type, ...)
  as.dendro(
      segments = hcdata$segments,
      labels = hcdata$labels,
      class="hclust"
  )
} 






#' Extract data frame from dendrogram object for plotting using ggplot.
#' 
#' Extract data frame from dendrogram object for plotting using ggplot
#' 
#' @param x object of class "dendrogram", e.g. the output of as.dendrogram()
#' @param type The type of plot, indicating the shape of the dendrogram.  "Rectangle" will draw
#' rectangular lines, while "triangle" will draw triangular lines.
#' @param ... ignored
#' @seealso \code{\link{ggdendrogram}}
#' @family dendro_data methods
#' @family dendrogram/hclust functions
#' @keywords internal
dendrogram_data <- function (x, type = c("rectangle", "triangle"), ...){ 

	# Initialise variables that used to be in parameter list
	leaflab <- "perpendicular"
	center <- FALSE
	xlab <- ""
	ylab <- ""
	horiz <- FALSE
	#frame.plot <- FALSE
	xaxt <- "n"
	yaxt <- "s"
	nodePar <- NULL
	edgePar <- list()
	dLeaf <- NULL 
	edge.root <- is.leaf(x) || !is.null(attr(x, "edgetext"))

	type <- match.arg(type)
	#leaflab <- match.arg(leaflab)
	hgt <- attr(x, "height")
	if (edge.root && is.logical(edge.root)) 
		edge.root <- 0.0625 * if (is.leaf(x)) 1 else hgt
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
	xl. <- c(x1 - 1/2, x2 + 1/2)
	yl. <- c(0, yTop)
#	if (missing(xlim) || is.null(xlim)) xlim <- xl.
#	if (missing(ylim) || is.null(ylim)) ylim <- yl.
#	plot(0, xlim = xlim, ylim = ylim, type = "n", xlab = xlab, 
#			ylab = ylab, xaxt = xaxt, yaxt = yaxt, frame.plot = frame.plot, 
#			...)
#	if (is.null(dLeaf)) 
#		dLeaf <- 0.75 * (if (horiz) 
#						strwidth("w")
#					else strheight("x"))
	if (edge.root) {
#		x0 <- stats:::plotNodeLimit(x1, x2, x, center)$x
#		(x0, hgt, x0, yTop)
		if (!is.null(et <- attr(x, "edgetext"))) {
			my <- mean(hgt, yTop)
#			text(x0, my, et)
		}
	}
	
	gg.plotNode <- function (x1, x2, subtree, type, center, leaflab, dLeaf, nodePar, 
			edgePar, horiz=FALSE, ddsegments=NULL, ddlabels=NULL) {
		inner <- !is.leaf(subtree) && x1 != x2
		yTop <- attr(subtree, "height")
		bx <- plotNodeLimit(x1, x2, subtree, center)
		xTop <- bx$x
		hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
		if (!hasP) nPar <- nodePar
		Xtract <- function(nam, L, default, indx) rep(if (nam %in% 
									names(L)) L[[nam]] else default, length.out = indx)[indx]
		asTxt <- function(x){
			if (is.character(x) || is.expression(x))
				x else
			if (is.null(x)) "" else as.character(x)
		}
		i <- if (inner || hasP) 
					1
				else 2
		if (!is.null(nPar)) {
			pch <- Xtract("pch", nPar, default = 1L:2, i)
			cex <- Xtract("cex", nPar, default = c(1, 1), i)
			col <- Xtract("col", nPar, default = par("col"), i)
			bg <- Xtract("bg", nPar, default = par("bg"), i)
			points(if (horiz) 
								cbind(yTop, xTop)
							else cbind(xTop, yTop), pch = pch, bg = bg, col = col, cex = cex)
		}
# 		lab.col <- Xtract("lab.col", nPar, default = par("col"), i)
# 		lab.cex <- Xtract("lab.cex", nPar, default = c(1, 1), i)
# 		lab.font <- Xtract("lab.font", nPar, default = par("font"), i)
    lab.cex <- 1
		if (is.leaf(subtree)) {
			if (leaflab == "perpendicular") {
				Y <- yTop - dLeaf * lab.cex
				X <- xTop
				srt <- 90
				adj <- 1
				nodeText <- asTxt(attr(subtree, "label"))
				# *************************
#			text(X, Y, nodeText, xpd = TRUE, srt = srt, adj = adj, 
#					cex = lab.cex, col = lab.col, font = lab.font)
				ddlabels <- rbind(ddlabels, data.frame(x=X, y=0, text=nodeText))
			}
		}
		else if (inner) {
			segmentsHV <- function(x0, y0, x1, y1) {
				# *************************
#			segments(x0, y0, x1, y1, col = col, lty = lty, lwd = lwd)
#			ddsegments <- rbind(ddsegments, data.frame(x0, y0, x1, y1)) #AdV
				data.frame(x0, y0, x1, y1) #AdV
			}
			for (k in seq_along(subtree)) {
				child <- subtree[[k]]
				yBot <- attr(child, "height")
				if (getOption("verbose")) 
					cat("ch.", k, "@ h=", yBot, "; ")
				if (is.null(yBot)) 
					yBot <- 0
				xBot <- if (center) 
							mean(bx$limit[k:(k + 1)])
						else bx$limit[k] + .midDend(child)
#			hasE <- !is.null(ePar <- attr(child, "edgePar"))
#			if (!hasE) ePar <- edgePar
#			i <- if (!is.leaf(child) || hasE) 1 else 2
#			col <- Xtract("col", ePar, default = par("col"), i)
#			lty <- Xtract("lty", ePar, default = par("lty"), i)
#			lwd <- Xtract("lwd", ePar, default = par("lwd"), i)
				if (type == "triangle") {
					# *************************
					ddsegments <- rbind(ddsegments, segmentsHV(xTop, yTop, xBot, yBot))
				}
				else {
					# *************************
					ddsegments <- rbind(ddsegments, segmentsHV(xTop, yTop, xBot, yTop))
					ddsegments <- rbind(ddsegments, segmentsHV(xBot, yTop, xBot, yBot))
				}
				vln <- NULL
#			if (is.leaf(child) && leaflab == "textlike") {
#				nodeText <- asTxt(attr(child, "label"))
#				hln <- 0.6 * strwidth(nodeText, cex = lab.cex)/2
#				vln <- 1.5 * strheight(nodeText, cex = lab.cex)/2
				# *************************
#				rect(xBot - hln, yBot, xBot + hln, yBot + 2 * 
#								vln, col = p.col)
				# *************************
#				text(xBot, yBot + vln, nodeText, xpd = TRUE, 
#						cex = lab.cex, col = lab.col, font = lab.font)
#			}
				if (!is.null(attr(child, "edgetext"))) {
					edgeText <- asTxt(attr(child, "edgetext"))
					if (!is.null(vln)) {
						mx <- if (type == "triangle") 
									(xTop + xBot + ((xTop - xBot)/(yTop - yBot)) * vln)/2
								else xBot
						my <- (yTop + yBot + 2 * vln)/2
					}
					else {
						mx <- if (type == "triangle") 
									(xTop + xBot)/2
								else xBot
						my <- (yTop + yBot)/2
					}
#				p.col <- Xtract("p.col", ePar, default = "white", i)
#				p.border <- Xtract("p.border", ePar, default = par("fg"), i)
#				p.lwd <- Xtract("p.lwd", ePar, default = lwd, i)
#				p.lty <- Xtract("p.lty", ePar, default = lty, i)
#				t.col <- Xtract("t.col", ePar, default = col, i)
#				t.cex <- Xtract("t.cex", ePar, default = 1, i)
#				t.font <- Xtract("t.font", ePar, default = par("font"), i)
#				vlm <- strheight(c(edgeText, "h"), cex = t.cex)/2
#				hlm <- strwidth(c(edgeText, "m"), cex = t.cex)/2
#				hl3 <- c(hlm[1L], hlm[1L] + hlm[2L], hlm[1L])
					# *************************
#				polygon(mx + c(-hl3, hl3), my + sum(vlm) * 
#								c(-1L:1L, 1L:-1L), col = p.col, border = p.border, 
#						lty = p.lty, lwd = p.lwd)
					# *************************
#				text(mx, my, edgeText, cex = t.cex, col = t.col, font = t.font)
				}
				plotNode_result <- gg.plotNode(bx$limit[k], bx$limit[k + 1], subtree = child, 
						type, center, leaflab, dLeaf, nodePar, edgePar, horiz, ddsegments, ddlabels)
				ddsegments <- plotNode_result$segments
				ddlabels <- plotNode_result$labels
			}
		}
		return(list(segments=ddsegments, labels=ddlabels))
	}
	
	ret <- gg.plotNode(x1, x2, x, type = type, center = center, leaflab = leaflab, 
			dLeaf = dLeaf, nodePar = nodePar, edgePar = edgePar, horiz=FALSE, 
			ddsegments=NULL, ddlabels=NULL)
  names(ret$segments) <- c("x", "y", "xend", "yend")
  names(ret$labels) <- c("x", "y", "label")
  ret
}


# .memberDend -------------------------------------------------------------

### Code copied from stats:::.memberDend

.memberDend <- function (x) 
{
  r <- attr(x, "x.member")
  if (is.null(r)) {
    r <- attr(x, "members")
    if (is.null(r)) 
      r <- 1L
  }
  r
}


# plotNodeLimit -----------------------------------------------------------

### Code copied from stats:::plotNodeLimit

plotNodeLimit <- function (x1, x2, subtree, center) 
{
  inner <- !is.leaf(subtree) && x1 != x2
  if (inner) {
    K <- length(subtree)
    mTop <- .memberDend(subtree)
    limit <- integer(K)
    xx1 <- x1
    for (k in 1L:K) {
      m <- .memberDend(subtree[[k]])
      xx1 <- xx1 + (if (center) 
        (x2 - x1) * m/mTop
                    else m)
      limit[k] <- xx1
    }
    limit <- c(x1, limit)
  }
  else {
    limit <- c(x1, x2)
  }
  mid <- attr(subtree, "midpoint")
  center <- center || (inner && !is.numeric(mid))
  x <- if (center) 
    mean(c(x1, x2))
  else x1 + (if (inner) 
    mid
             else 0)
  list(x = x, limit = limit)
}

# .midDend --------------------------------------------------------------------

### Code copied from stats:::.midDend


.midDend <- function (x) 
  if (is.null(mp <- attr(x, "midpoint"))) 0 else mp
