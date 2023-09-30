# adapted from stats:::plotNode
ggPlotNode <- function(x1, x2, subtree, type, center, leaflab, dLeaf, nodePar,
                        edgePar, horiz = FALSE, ddsegments = NULL, ddlabels = NULL) {
  inner <- !is.leaf(subtree) && x1 != x2
  yTop <- attr(subtree, "height")
  bx <- plotNodeLimit(x1, x2, subtree, center)
  xTop <- bx$x
  hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
  if (!hasP) nPar <- nodePar
  Xtract <- function(nam, L, default, indx) {
    rep(if (nam %in%
            names(L)) {
      L[[nam]]
    } else {
      default
    }, length.out = indx)[indx]
  }
  asTxt <- function(x) {
    if (is.character(x) || is.expression(x)) {
      x
    } else
      if (is.null(x)) "" else as.character(x)
  }
  i <- if (inner || hasP) {
    1
  } else {
    2
  }
  if (!is.null(nPar)) {
    pch <- Xtract("pch", nPar, default = 1L:2, i)
    cex <- Xtract("cex", nPar, default = c(1, 1), i)
    col <- Xtract("col", nPar, default = par("col"), i)
    bg <- Xtract("bg", nPar, default = par("bg"), i)
    points(if (horiz) {
      cbind(yTop, xTop)
    } else {
      cbind(xTop, yTop)
    }, pch = pch, bg = bg, col = col, cex = cex)
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
      # 			text(X, Y, nodeText, xpd = TRUE, srt = srt, adj = adj,
      # 					cex = lab.cex, col = lab.col, font = lab.font)
      ddlabels <- rbind(ddlabels, data.frame(x = X, y = 0, text = nodeText))
    }
  }
  else if (inner) {
    segmentsHV <- function(x0, y0, x1, y1) {
      # *************************
      # 			segments(x0, y0, x1, y1, col = col, lty = lty, lwd = lwd)
      # 			ddsegments <- rbind(ddsegments, data.frame(x0, y0, x1, y1)) #AdV
      data.frame(x0, y0, x1, y1) # AdV
    }
    for (k in seq_along(subtree)) {
      child <- subtree[[k]]
      yBot <- attr(child, "height")
      if (getOption("verbose")) {
        cat("ch.", k, "@ h=", yBot, "; ")
      }
      if (is.null(yBot)) {
        yBot <- 0
      }
      xBot <- if (center) {
        mean(bx$limit[k:(k + 1)])
      } else {
        bx$limit[k] + .midDend(child)
      }
      # 			hasE <- !is.null(ePar <- attr(child, "edgePar"))
      # 			if (!hasE) ePar <- edgePar
      # 			i <- if (!is.leaf(child) || hasE) 1 else 2
      # 			col <- Xtract("col", ePar, default = par("col"), i)
      # 			lty <- Xtract("lty", ePar, default = par("lty"), i)
      # 			lwd <- Xtract("lwd", ePar, default = par("lwd"), i)
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
      # 			if (is.leaf(child) && leaflab == "textlike") {
      # 				nodeText <- asTxt(attr(child, "label"))
      # 				hln <- 0.6 * strwidth(nodeText, cex = lab.cex)/2
      # 				vln <- 1.5 * strheight(nodeText, cex = lab.cex)/2
      # *************************
      # 				rect(xBot - hln, yBot, xBot + hln, yBot + 2 *
      # 								vln, col = p.col)
      # *************************
      # 				text(xBot, yBot + vln, nodeText, xpd = TRUE,
      # 						cex = lab.cex, col = lab.col, font = lab.font)
      # 			}
      if (!is.null(attr(child, "edgetext"))) {
        edgeText <- asTxt(attr(child, "edgetext"))
        if (!is.null(vln)) {
          mx <- if (type == "triangle") {
            (xTop + xBot + ((xTop - xBot) / (yTop - yBot)) * vln) / 2
          } else {
            xBot
          }
          my <- (yTop + yBot + 2 * vln) / 2
        }
        else {
          mx <- if (type == "triangle") {
            (xTop + xBot) / 2
          } else {
            xBot
          }
          my <- (yTop + yBot) / 2
        }
        # 				p.col <- Xtract("p.col", ePar, default = "white", i)
        # 				p.border <- Xtract("p.border", ePar, default = par("fg"), i)
        # 				p.lwd <- Xtract("p.lwd", ePar, default = lwd, i)
        # 				p.lty <- Xtract("p.lty", ePar, default = lty, i)
        # 				t.col <- Xtract("t.col", ePar, default = col, i)
        # 				t.cex <- Xtract("t.cex", ePar, default = 1, i)
        # 				t.font <- Xtract("t.font", ePar, default = par("font"), i)
        # 				vlm <- strheight(c(edgeText, "h"), cex = t.cex)/2
        # 				hlm <- strwidth(c(edgeText, "m"), cex = t.cex)/2
        # 				hl3 <- c(hlm[1L], hlm[1L] + hlm[2L], hlm[1L])
        # *************************
        # 				polygon(mx + c(-hl3, hl3), my + sum(vlm) *
        # 								c(-1L:1L, 1L:-1L), col = p.col, border = p.border,
        # 						lty = p.lty, lwd = p.lwd)
        # *************************
        # 				text(mx, my, edgeText, cex = t.cex, col = t.col, font = t.font)
      }
      plotNode_result <- ggPlotNode(bx$limit[k], bx$limit[k + 1],
                                     subtree = child,
                                     type, center, leaflab, dLeaf, nodePar, edgePar, horiz, ddsegments, ddlabels
      )
      ddsegments <- plotNode_result$segments
      ddlabels <- plotNode_result$labels
    }
  }
  return(list(segments = ddsegments, labels = ddlabels))
}
