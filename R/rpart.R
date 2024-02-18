#
#  ggdendro/R/dendro_rpart.R by Andrie de Vries and Brian Ripley
#  Copyright (C) 2011-2015
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


# labels.rpart ----------------------------------------------------------------


#' Make the nice labels used by print and summary.
#'
#' @param digits obvious
#'
#' @param minlength 0 = don't abbrev factors; 1 = use single letters; 2+= the
#'   same arg as the "abbreviate" function
#'
#' @param collapse an oddly named argument: FALSE = return a matrix with two
#'   columns, containing the labels of the left and right descendants of each
#'   node; TRUE = return a vector of 1 column, with the label of the parent
#'
#' @param pretty: for historical compatibility: 0   -> minlength = 0; NULL ->
#'   minlength = 1; TRUE   -> minlength = 4
#'
#' @param ... = other args for abbreviate()
#'
#' @keywords internal
#' @noRd
labels.rpart <- function(object, digits = 4, minlength = 1L, pretty,
                         collapse = TRUE, ...) {
  if (missing(minlength) && !missing(pretty)) {
    minlength <- if (is.null(pretty)) {
      1L
    } else if (is.logical(pretty)) {
      if (pretty) 4L else 0L
    } else {
      0L
    }
  }

  ff <- object$frame
  n <- nrow(ff)
  if (n == 1L) {
    return("root")
  } # special case of no splits

  is.leaf <- (ff$var == "<leaf>")
  whichrow <- !is.leaf
  vnames <- ff$var[whichrow] # the variable names for the primary splits

  index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + !is.leaf))
  irow <- index[c(whichrow, FALSE)] # we only care about the primary split
  ncat <- object$splits[irow, 2L]

  ## Now to work: first create labels for the left and right splits,
  ##  but not for leaves of course
  ##
  lsplit <- rsplit <- character(length(irow))

  if (any(ncat < 2L)) { # any continuous vars ?
    jrow <- irow[ncat < 2L]
    cutpoint <- formatg(object$splits[jrow, 4L], digits)
    temp1 <- (ifelse(ncat < 0, "< ", ">="))[ncat < 2L]
    temp2 <- (ifelse(ncat < 0, ">=", "< "))[ncat < 2L]
    lsplit[ncat < 2L] <- paste0(temp1, cutpoint)
    rsplit[ncat < 2L] <- paste0(temp2, cutpoint)
  }

  if (any(ncat > 1L)) { # any categorical variables ?
    xlevels <- attr(object, "xlevels")
    ##
    ## jrow will be the row numbers of factors within lsplit and rsplit
    ## crow the row number in "csplit"
    ## and cindex the index on the "xlevels" list
    ##
    jrow <- seq_along(ncat)[ncat > 1L]
    crow <- object$splits[irow[ncat > 1L], 4L] # row number in csplit
    cindex <- (match(vnames, names(xlevels)))[ncat > 1L]

    ## Now, abbreviate the levels
    if (minlength == 1L) {
      if (any(ncat > 52L)) {
        warning("more than 52 levels in a predicting factor, truncated for printout",
          domain = NA
        )
      }
      xlevels <- lapply(xlevels, function(z) c(letters, LETTERS)[pmin(seq_along(z), 52L)])
    } else if (minlength > 1L) {
      xlevels <- lapply(xlevels, abbreviate, minlength, ...)
    }

    ## Now tuck in the labels
    ## I'll let some other clever person vectorize this
    for (i in seq_along(jrow)) {
      j <- jrow[i]
      splits <- object$csplit[crow[i], ]
      ## splits will contain 1=left, 3=right, 2= neither
      cl <- if (minlength == 1L) "" else ","
      lsplit[j] <-
        paste((xlevels[[cindex[i]]])[splits == 1L], collapse = cl)
      rsplit[j] <-
        paste((xlevels[[cindex[i]]])[splits == 3L], collapse = cl)
    }
  }

  if (!collapse) { # called by no routines that I know of
    ltemp <- rtemp <- rep("<leaf>", n)
    ltemp[whichrow] <- lsplit
    rtemp[whichrow] <- rsplit
    return(cbind(ltemp, rtemp))
  }

  lsplit <- paste0(ifelse(ncat < 2L, "", "="), lsplit)
  rsplit <- paste0(ifelse(ncat < 2L, "", "="), rsplit)

  ## Now match them up to node numbers
  ##   The output will have one label per row of object$frame, each
  ##   corresponding the the line segement joining this node to its parent
  varname <- (as.character(vnames))
  node <- as.numeric(row.names(ff))
  parent <- match(node %/% 2L, node[whichrow])
  odd <- (as.logical(node %% 2L))

  labels <- character(n)
  labels[odd] <- paste0(varname[parent[odd]], rsplit[parent[odd]])
  labels[!odd] <- paste0(varname[parent[!odd]], lsplit[parent[!odd]])
  labels[1L] <- "root"
  labels
}


# rpart_ggdendro_env ------------------------------------------------------

# rpart_ggdendro_env <- new.env()


# rpart.branch ------------------------------------------------------------



##
## Compute the "branches" to be drawn for an rpart object
##
## most likely this could simply default to branch = 1
rpart.branch <- function(x, y, node, branch) {
  # if (missing(branch)) {
  #   pn <- paste0("device", dev.cur())
  #   if (!exists(pn, envir = rpart_ggdendro_env, inherits = FALSE))
  #     stop("no information available on parameters from previous call to plot()")
  #   parms <- get(pn, envir = rpart_ggdendro_env, inherits = FALSE)
  #   branch <- parms$branch
  # }
  if (missing(branch)) branch <- 1

  ## Draw a series of horseshoes, left son, up, over, down to right son
  ##   NA's in the vector cause lines() to "lift the pen"
  is.left <- (node %% 2L == 0L) # left hand sons
  node.left <- node[is.left]
  parent <- match(node.left / 2L, node)
  sibling <- match(node.left + 1L, node)
  temp <- (x[sibling] - x[is.left]) * (1 - branch) / 2
  xx <- rbind(
    x[is.left], x[is.left] + temp,
    x[sibling] - temp, x[sibling], NA
  )
  yy <- rbind(y[is.left], y[parent], y[parent], y[sibling], NA)
  list(x = xx, y = yy)
}


# tree.depth ------------------------------------------------------------------

tree.depth <- function(nodes) {
  depth <- floor(log(nodes, base = 2) + 1e-7)
  depth - min(depth)
}


# rpartco ---------------------------------------------------------------------

#' Compute the x-y coordinates for a tree.
#'
#' @param tree Tree model
#' @param parms Graphics parameters
#' @keywords internal
rpartco <- function(tree, parms) {
  # if (missing(parms)) {
  #   pn <- paste0("device", dev.cur())
  #   if (!exists(pn, envir = rpart_ggdendro_env, inherits = FALSE))
  #     stop("no information available on parameters from previous call to plot()")
  #   parms <- get(pn, envir = rpart_ggdendro_env, inherits = FALSE)
  # }

  frame <- tree$frame
  node <- as.numeric(row.names(frame))
  depth <- tree.depth(node)
  is.leaf <- (frame$var == "<leaf>")
  if (length(parms)) {
    uniform <- parms$uniform
    nspace <- parms$nspace
    minbranch <- parms$minbranch
  } else {
    uniform <- FALSE
    nspace <- -1
    minbranch <- 0.3
  }

  if (uniform) {
    y <- (1 + max(depth) - depth) / max(depth, 4L)
  } else { # make y- (parent y) = change in deviance
    y <- dev <- frame$dev
    temp <- split(seq(node), depth) # depth 0 nodes, then 1, then ...
    parent <- match(node %/% 2L, node)
    sibling <- match(ifelse(node %% 2L, node - 1L, node + 1L), node)

    ## assign the depths
    for (i in temp[-1L]) {
      temp2 <- dev[parent[i]] - (dev[i] + dev[sibling[i]])
      y[i] <- y[parent[i]] - temp2
    }
    ##
    ## For some problems, classification & loss matrices in particular
    ##   the gain from a split may be 0.  This is ugly on the plot.
    ## Hence the "fudge" factor of  0.3 * the average step
    ##
    fudge <- minbranch * diff(range(y)) / max(depth)
    for (i in temp[-1L]) {
      temp2 <- dev[parent[i]] - (dev[i] + dev[sibling[i]])
      haskids <- !(is.leaf[i] & is.leaf[sibling[i]])
      y[i] <- y[parent[i]] - ifelse(temp2 <= fudge & haskids, fudge, temp2)
    }
    y <- y / (max(y))
  }

  # Now compute the x coordinates, by spacing out the leaves and then
  #   filling in
  x <- double(length(node)) # allocate, then fill it in below
  x[is.leaf] <- seq(sum(is.leaf)) # leaves at 1, 2, 3, ....
  left.child <- match(node * 2L, node)
  right.child <- match(node * 2L + 1L, node)

  ## temp is a list of non-is.leaf, by depth
  temp <- split(seq(node)[!is.leaf], depth[!is.leaf])
  for (i in rev(temp)) {
    x[i] <- 0.5 * (x[left.child[i]] + x[right.child[i]])
  }

  if (nspace < 0) {
    return(list(x = x, y = y))
  }

  ##
  ## Now we get fancy, and try to do overlapping
  ##
  ##  The basic algorithm is, at each node:
  ##      1: get the left & right edges, by depth, for the left and
  ##           right sons, of the x-coordinate spacing.
  ##      2: find the minimal free spacing.  If this is >0, slide the
  ##           right hand son over to the left
  ##      3: report the left & right extents of the new tree up to the
  ##           parent
  ##   A way to visualize steps 1 and 2 is to imagine, for a given node,
  ##      that the left son, with all its descendants, is drawn on a
  ##      slab of wood.  The left & right edges, per level, give the
  ##      width of this board.  (The board is not a rectangle, it has
  ##      'stair step' edges). Do the same for the right son.  Now
  ##      insert some spacers, one per level, and slide right hand
  ##      board over until they touch.  Glue the boards and spacer
  ##      together at that point.
  ##
  ##  If a node has children, its 'space' is considered to extend left
  ##    and right by the amount "nspace", which accounts for space
  ##    used by the arcs from this node to its children.  For
  ##    horseshoe connections nspace usually is 1.
  ##
  compress <- function(x, me, depth) {
    lson <- me + 1L
    if (is.leaf[lson]) {
      left <- list(
        left = x[lson], right = x[lson],
        depth = depth + 1L, sons = lson
      )
    } else {
      left <- compress(x, me + 1L, depth + 1L)
      x <- left$x
    }

    rson <- me + 1L + length(left$sons) # index of right son
    if (is.leaf[rson]) {
      right <- list(
        left = x[rson], right = x[rson],
        depth = depth + 1L, sons = rson
      )
    } else {
      right <- compress(x, rson, depth + 1L)
      x <- right$x
    }

    maxd <- max(left$depth, right$depth) - depth
    mind <- min(left$depth, right$depth) - depth

    ## Find the smallest distance between the two subtrees
    ##   But only over depths that they have in common
    ## 1 is a minimum distance allowed
    slide <- min(right$left[1L:mind] - left$right[1L:mind]) - 1L
    if (slide > 0) { # slide the right hand node to the left
      x[right$sons] <- x[right$sons] - slide
      x[me] <- (x[right$sons[1L]] + x[left$sons[1L]]) / 2
    }
    else {
      slide <- 0
    }

    ## report back
    if (left$depth > right$depth) {
      templ <- left$left
      tempr <- left$right
      tempr[1L:mind] <- pmax(tempr[1L:mind], right$right - slide)
    } else {
      templ <- right$left - slide
      tempr <- right$right - slide
      templ[1L:mind] <- pmin(templ[1L:mind], left$left)
    }

    list(
      x = x,
      left = c(x[me] - nspace * (x[me] - x[lson]), templ),
      right = c(x[me] - nspace * (x[me] - x[rson]), tempr),
      depth = maxd + depth, sons = c(me, left$sons, right$sons)
    )
  }
  x <- compress(x, 1L, 1L)$x
  list(x = x, y = y)
}


# text.rpart ------------------------------------------------------------------

## This is a modification of text.tree.
## Fancy option has been added in (to mimic post.tree)
##

text_rpart <- function(x, splits = TRUE, label, FUN = text, all = FALSE,
                       pretty = NULL, digits = getOption("digits") - 3L,
                       use.n = FALSE, fancy = FALSE, fwidth = 0.8, fheight = 0.8,
                       bg = par("bg"), minlength = 1L, parms, ...) {
  string.bounding.box <- NULL
  if (!inherits(x, "rpart")) stop("Not a legitimate \"rpart\" object")
  if (nrow(x$frame) <= 1L) stop("fit is not a tree, just a root")

  frame <- x$frame
  if (!missing(label)) warning("argument 'label' is no longer used")
  col <- names(frame)
  ylevels <- attr(x, "ylevels")
  if (!is.null(ylevels <- attr(x, "ylevels"))) col <- c(col, ylevels)
  # cxy <- par("cxy")                   # character width and height
  cxy <- c(0.1, 0.1)
  if (!is.null(srt <- list(...)$srt) && srt == 90) cxy <- rev(cxy)
  xy <- rpartco(x, parms = parms)

  node <- as.numeric(row.names(frame))
  is.left <- (node %% 2L == 0L) # left hand sons
  node.left <- node[is.left]
  parent <- match(node.left / 2L, node)

  ## Put left splits at the parent node
  if (splits) {
    left.child <- match(2L * node, node)
    right.child <- match(node * 2L + 1L, node)
    rows <- if (!missing(pretty) && missing(minlength)) {
      labels(x, pretty = pretty)
    } else {
      labels(x, minlength = minlength)
    }
    if (fancy) {
      ## put split labels on branches instead of nodes
      xytmp <- rpart.branch(x = xy$x, y = xy$y, node = node)
      leftptx <- (xytmp$x[2L, ] + xytmp$x[1L, ]) / 2
      leftpty <- (xytmp$y[2L, ] + xytmp$y[1L, ]) / 2
      rightptx <- (xytmp$x[3L, ] + xytmp$x[4L, ]) / 2
      rightpty <- (xytmp$y[3L, ] + xytmp$y[4L, ]) / 2

      #         FUN(leftptx, leftpty + 0.52 * cxy[2L],
      #             rows[left.child[!is.na(left.child)]], ...)
      #         FUN(rightptx, rightpty - 0.52 * cxy[2L],
      #             rows[right.child[!is.na(right.child)]], ...)
    } else {
      #         FUN(xy$x, xy$y + 0.5 * cxy[2L], rows[left.child], ...)

      ### Insertion by Andrie de Vries to capture label splits
      labelSplits <- data.frame(
        x = xy$x,
        y = xy$y,
        label = rows[left.child]
      )
    }
    labelSplits <- labelSplits[complete.cases(labelSplits), ]
    labelSplits$type <= "splits"
  }

  leaves <- if (all) rep(TRUE, nrow(frame)) else frame$var == "<leaf>"

  stat <-
    x$functions$text(
      yval = if (is.null(frame$yval2)) {
        frame$yval[leaves]
      } else {
        frame$yval2[leaves, ]
      },
      dev = frame$dev[leaves], wt = frame$wt[leaves],
      ylevel = ylevels, digits = digits,
      n = frame$n[leaves], use.n = use.n
    )

  # if (fancy) {
  #   if (col2rgb(bg, alpha = TRUE)[4L, 1L] < 255) bg <- "white"
  #   oval <- function(middlex, middley, a, b)
  #   {
  #     theta <- seq(0, 2 * pi, pi/30)
  #     newx <- middlex + a * cos(theta)
  #     newy <- middley + b * sin(theta)
  #     polygon(newx, newy, border = TRUE, col = bg)
  #   }
  #
  #   ## FIXME: use rect()
  #   rectangle <- function(middlex, middley, a, b)
  #   {
  #     newx <- middlex + c(a, a, -a, -a)
  #     newy <- middley + c(b, -b, -b, b)
  #     polygon(newx, newy, border = TRUE, col = bg)
  #   }
  #
  #   ## find maximum length of stat
  #   maxlen <- max(string.bounding.box(stat)$columns) + 1L
  #   maxht <- max(string.bounding.box(stat)$rows) + 1L
  #
  #   a.length <- if (fwidth < 1)  fwidth * cxy[1L] * maxlen else fwidth * cxy[1L]
  #
  #   b.length <- if (fheight < 1) fheight * cxy[2L] * maxht else fheight * cxy[2L]
  #
  #   ## create ovals and rectangles here
  #   ## sqrt(2) creates the smallest oval that fits around the
  #   ## best fitting rectangle
  #   for (i in parent)
  #     oval(xy$x[i], xy$y[i], sqrt(2) * a.length/2, sqrt(2) * b.length/2)
  #   child <- match(node[frame$var == "<leaf>"], node)
  #   for (i in child)
  #     rectangle(xy$x[i], xy$y[i], a.length/2, b.length/2)
  # }

  ## if FUN=text then adj=1 puts the split label to the left of the
  ##    split rather than centered
  ## Allow labels at all or just leaf nodes

  ## stick values on nodes
  #     if (fancy) FUN(xy$x[leaves], xy$y[leaves] + 0.5 * cxy[2L], stat, ...)
  #     else FUN(xy$x[leaves], xy$y[leaves] - 0.5 * cxy[2L], stat, adj = 0.5, ...)
  labelLeaves <- data.frame(
    x = xy$x[leaves],
    y = xy$y[leaves],
    label = stat
  )



  #     return(list(xy=xy, cxy=cxy))
  list(
    labels = labelSplits,
    leaf_labels = labelLeaves
  )
}



# formatg -----------------------------------------------------------------



## format a set of numbers using C's "g" format
## No longer exported.
formatg <- function(x, digits = getOption("digits"),
                    format = paste0("%.", digits, "g")) {
  if (!is.numeric(x)) stop("'x' must be a numeric vector")

  temp <- sprintf(format, x)
  if (is.matrix(x)) matrix(temp, nrow = nrow(x)) else temp
}
