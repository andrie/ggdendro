# Classification trees using rpart
# 
# Author: Andrie
#----------------------------------------------------------------------------------



# Classification Tree with rpart


#' Extract data from classification tree object for plotting using ggplot.
#' 
#' Extracts data to plot line segments and labels from a \code{\link[rpart]{rpart}} classification tree object.  This data can then be manipulated or plotted, e.g. using \code{\link[ggplot2]{ggplot}}.
#' 
#' @param model object of class "tree", e.g. the output of tree()
#' @param ... ignored
#' @method dendro_data rpart
#' @export
#' @return
#' A list of three data frames:
#' \item{segments}{a data frame containing the line segment data}
#' \item{labels}{a data frame containing the label text data}
#' \item{leaf_labels}{a data frame containing the leaf label text data}
#' @seealso \code{\link{dendro_data}}, \code{\link{ggdendrogram}}
#' @examples
#' require(rpart)
#' require(ggplot2)
#' fit <- rpart(Kyphosis ~ Age + Number + Start,   method="class", data=kyphosis)
#' fitr <- dendro_data(fit)
#' ggplot() + 
#'     geom_segment(data=fitr$segments, aes(x=x, y=y, xend=xend, yend=yend)) + 
#'     geom_text(data=fitr$labels, aes(x=x, y=y, label=label)) +
#'     geom_text(data=fitr$leaf_labels, aes(x=x, y=y, label=label)) +
#'     theme_dendro()
dendro_data.rpart <- function(model, ...){
  segments <- rpart_segments(model, )
  labels <- rpart_labels(model, ...)
  as.dendro(
      list(
          segments = segments,
          labels = labels$labels,
          leaf_labels = labels$leaf_labels
      ))
  
}

#' Extract data frame from rpart object for plotting using ggplot.
#' @param model object of class "tree", e.g. the output of tree()
#' @param ... ignored
#' @keywords internal
#' @seealso \code{\link{dendro_data.tree}}
rpart_segments <- function (model, ...) {
  x <- model
  uniform = FALSE
  branch = 1
  compress = FALSE
  nspace <- -1L
  margin = 0
  minbranch = 0.3
  require(rpart)
  if (!inherits(x, "rpart")) 
    stop("Not an rpart object")
  if (!is.null(x$frame$splits)) 
    x <- rpconvert(x)
  if (nrow(x$frame) <= 1L) 
    stop("fit is not a tree, just a root")
  if (compress & missing(nspace)) 
    nspace <- branch
  if (!compress) 
    nspace <- -1L
  if (dev.cur() == 1L) 
    dev.new()
  assign(paste(".rpart.parms", dev.cur(), sep = "."), list(uniform = uniform, 
          branch = branch, nspace = nspace, minbranch = minbranch), 
      envir = .GlobalEnv)
  temp <- rpart:::rpartco(x)
  xx <- temp$x
  yy <- temp$y
  temp1 <- range(xx) + diff(range(xx)) * c(-margin, margin)
  temp2 <- range(yy) + diff(range(yy)) * c(-margin, margin)
  #plot(temp1, temp2, type = "n", axes = FALSE, xlab = "", ylab = "", ...)
  node <- as.numeric(row.names(x$frame))
  temp <- rpart:::rpart.branch(xx, yy, node, branch)
  #if (branch > 0) text(xx[1L], yy[1L], "|")
  #lines(c(temp$x), c(temp$y))


  dat <- data.frame(
      stack(as.data.frame(temp$x)), 
      stack(as.data.frame(temp$y))
  )[, c("ind", "values", "values.1")]
  dat
  
  dat2 <- cbind(head(dat, -1), tail(dat, -1))
  dat3 <- dat2[complete.cases(dat2), -4]
  names(dat3) <- c("n", "x", "y", "xend", "yend")
  dat3

}


#' Extract labels data frame from rpart object for plotting using ggplot.
#' @param model object of class "rpart", e.g. the output of rpart()
#' @param ... ignored
#' @return a list with two elements: $labels and $leaf_labels
#' @keywords internal
#' @seealso \code{\link{dendro_data}}
rpart_labels <- function (model, splits = TRUE, label, FUN = text, all = FALSE, pretty = NULL, 
    digits = getOption("digits") - 3, use.n = FALSE, fancy = FALSE, 
    fwidth = 0.8, fheight = 0.8, ...) 
{
  require(rpart)
  x <- model
  if (!inherits(x, "rpart")) 
    stop("Not legitimate rpart")
  if (!is.null(x$frame$splits)) 
    x <- rpconvert(x)
  if (nrow(x$frame) <= 1) 
    stop("fit is not a tree, just a root")
  frame <- x$frame
  if (!missing(label)) 
    warning("argument 'label' is currently unused")
  cxy <- par("cxy")
  if (!is.null(srt <- list(...)$srt) && srt == 90) 
    cxy <- rev(cxy)
  xy <- rpart:::rpartco(x)
  node <- as.numeric(row.names(x$frame))
  is.left <- (node%%2 == 0)
  node.left <- node[is.left]
  parent <- match(node.left/2, node)
  if (splits) {
    left.child <- match(2 * node, node)
    right.child <- match(node * 2 + 1, node)
    rows <- labels(x, pretty = pretty)
    if (fancy) {
#      xytmp <- rpart.branch(x = xy$x, y = xy$y, node = node)
#      leftptx <- (xytmp$x[2L, ] + xytmp$x[1L, ])/2
#      leftpty <- (xytmp$y[2L, ] + xytmp$y[1L, ])/2
#      rightptx <- (xytmp$x[3L, ] + xytmp$x[4, ])/2
#      rightpty <- (xytmp$y[3L, ] + xytmp$y[4L, ])/2
#      FUN(leftptx, leftpty + 0.52 * cxy[2L], rows[left.child[!is.na(left.child)]], ...)
#      FUN(rightptx, rightpty - 0.52 * cxy[2L], rows[right.child[!is.na(right.child)]], ...)
    }
    else {
      #FUN(xy$x, xy$y + 0.5 * cxy[2L], rows[left.child],  ...)
      labelSplits <- data.frame(
          x = xy$x, 
          y = xy$y, 
          label = rows[left.child])
      labelSplits <- labelSplits[complete.cases(labelSplits), ]
      labelSplits$type <= "splits"
    }
  }
  leaves <- if (all) 
        rep(TRUE, nrow(frame))
      else frame$var == "<leaf>"
  ylevels <- attr(x, "ylevels")
  stat <- if (is.null(frame$yval2)) 
        x$functions$text(yval = frame$yval[leaves], dev = frame$dev[leaves], 
            wt = frame$wt[leaves], ylevel = ylevels, digits = digits, 
            n = frame$n[leaves], use.n = use.n)
      else x$functions$text(yval = frame$yval2[leaves, ], dev = frame$dev[leaves], 
            wt = frame$wt[leaves], ylevel = ylevels, digits = digits, 
            n = frame$n[leaves], use.n = use.n)
  oval <- function(middlex, middley, a, b) {
    theta <- seq(0, 2 * pi, pi/30)
    newx <- middlex + a * cos(theta)
    newy <- middley + b * sin(theta)
    polygon(newx, newy, border = TRUE, col = 0)
  }
  rectangle <- function(middlex, middley, a, b) {
    newx <- middlex + c(a, a, -a, -a)
    newy <- middley + c(b, -b, -b, b)
    polygon(newx, newy, border = TRUE, col = 0)
  }
#  if (fancy) {
#    maxlen <- max(string.bounding.box(stat)$columns) + 1L
#    maxht <- max(string.bounding.box(stat)$rows) + 1L
#    if (fwidth < 1) 
#      a.length <- fwidth * cxy[1L] * maxlen
#    else a.length <- fwidth * cxy[1L]
#    if (fheight < 1) 
#      b.length <- fheight * cxy[2L] * maxht
#    else b.length <- fheight * cxy[2L]
#    for (i in parent) oval(xy$x[i], xy$y[i], a = sqrt(2) * 
#              a.length/2, b = sqrt(2) * b.length/2)
#    child <- match(node[frame$var == "<leaf>"], node)
#    for (i in child) rectangle(xy$x[i], xy$y[i], a = a.length/2, b = b.length/2)
#  }
  if (fancy) 
    FUN(xy$x[leaves], xy$y[leaves] + 0.5 * cxy[2], stat, ...)
  else {
    #FUN(xy$x[leaves], xy$y[leaves] - 0.5 * cxy[2], stat, adj = 0.5, ...)
    labelLeaves <- data.frame(
        x = xy$x[leaves], 
        y = xy$y[leaves], 
        label = stat
        )
  }
  
  list(
      labels=labelSplits,
      leaf_labels=labelLeaves
  )
}

