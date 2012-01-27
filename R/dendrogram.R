
###############################################################################
#' Extract line segment and label data from dendrogram or hclust object.
#' 
#' Extract line segment and label data from dendrogram or hclust object.  Results are stored in a
#' list of data frames containing line segment data and label data.
#' 
#' @param model object of class "dendrogram", e.g. the output of as.dendrogram()
#' @param type The type of plot, indicating the shape of the dendrogram.  "rectangle" will draw
#' rectangular lines, while "triangle" will draw triangular lines.
#' @param ... ignored
#' @aliases dendro_data.dendrogram dendro_data.hclust
#' @method dendro_data dendrogram
#' @method dendro_data hclust
#' @export dendro_data.dendrogram dendro_data.hclust
#' @return
#' A list with the following elements:
#' \item{segments}{Line segment data}
#' \item{labels}{Label data}
#' @seealso \code{\link{ggdendrogram}}
#' @family dendro_data methods
#' @family dendrogram/hclust functions
#' @examples
#' require(ggplot2)
#' #
#' # Demonstrate dendro_data.dendrogram
#' #
#' hc <- hclust(dist(USArrests), "ave")
#' dhc <- as.dendrogram(hc)
#' # Rectangular lines
#' ddata <- dendro_data(dhc, type="rectangle")
#' ggplot(segment(ddata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
#' 		coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + theme_dendro()
#' # Triangular lines
#' ddata <- dendro_data(dhc, type="triangle")
#' ggplot(segment(ddata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + theme_dendro()
#' #
#' # Demonstrate dendro_data.hclust
#' #
#' require(ggplot2)
#' hc <- hclust(dist(USArrests), "ave")
#' # Rectangular lines
#' hcdata <- dendro_data(hc, type="rectangle")
#' ggplot(segment(hcdata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
#'    coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + theme_dendro()
#' # Triangular lines
#' hcdata <- dendro_data(hc, type="triangle")
#' ggplot(segment(hcdata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
#'   theme_dendro()
dendro_data.dendrogram <- function (model, type = c("rectangle", "triangle"), ...){
	hcdata <- dendrogram_data(model, type=type, ...)
	as.dendro(
      segments = hcdata$segments,
  		labels = hcdata$labels,
      class="dendrogram"
  	)
} 

dendro_data.hclust <- function (model, type = c("rectangle", "triangle"), ...){
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
	mem.x <- stats:::.memberDend(x)
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
		bx <- stats:::plotNodeLimit(x1, x2, subtree, center)
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
		lab.col <- Xtract("lab.col", nPar, default = par("col"), i)
		lab.cex <- Xtract("lab.cex", nPar, default = c(1, 1), i)
		lab.font <- Xtract("lab.font", nPar, default = par("font"), i)
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
						else bx$limit[k] + stats:::.midDend(child)
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

