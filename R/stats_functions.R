
# .memberDend -------------------------------------------------------------

### Code copied from stats:::.memberDend

.memberDend <- function(x) {
  r <- attr(x, "x.member")
  if (is.null(r)) {
    r <- attr(x, "members")
    if (is.null(r)) {
      r <- 1L
    }
  }
  r
}


# plotNodeLimit -----------------------------------------------------------

### Code copied from stats:::plotNodeLimit

plotNodeLimit <- function(x1, x2, subtree, center) {
  inner <- !is.leaf(subtree) && x1 != x2
  if (inner) {
    K <- length(subtree)
    mTop <- .memberDend(subtree)
    limit <- integer(K)
    xx1 <- x1
    for (k in 1L:K) {
      m <- .memberDend(subtree[[k]])
      xx1 <- xx1 + (if (center) {
        (x2 - x1) * m / mTop
      } else {
        m
      })
      limit[k] <- xx1
    }
    limit <- c(x1, limit)
  }
  else {
    limit <- c(x1, x2)
  }
  mid <- attr(subtree, "midpoint")
  center <- center || (inner && !is.numeric(mid))
  x <- if (center) {
    mean(c(x1, x2))
  } else {
    x1 + (if (inner) {
      mid
    } else {
      0
    })
  }
  list(x = x, limit = limit)
}

# .midDend --------------------------------------------------------------------

### Code copied from stats:::.midDend


.midDend <- function(x) {
  if (is.null(mp <- attr(x, "midpoint"))) 0 else mp
}