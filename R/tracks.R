#' Creates dendrogram plot with annotation tracks using ggplot.
#' 
#' Creates dendrogram plot with annotation tracks using ggplot.
#' 
#' @param data Either a dendro object or an object that can be coerced to class dendro using the \code{\link{dendro_data}} function, i.e. objects of class dendrogram, hclust or tree
#' @param tracks annotation data.frame with rownames matching the leaf labels of \code{data}
#' @param segments If TRUE, show line segments
#' @param labels if TRUE, shows segment labels
#' @param leaf_labels if TRUE, shows leaf labels
#' @param rotate if TRUE, rotates plot by 90 degrees
#' @param theme_dendro if TRUE, applies a blank theme to plot (see \code{\link{theme_dendro}}) 
#' @param trackheight each annotation tracks will have height \code{trackheight}. Default is 0.1.
#' @param ... other parameters passed to \code{\link[ggplot2]{geom_text}}
#' @export
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @seealso \code{\link{dendro_data}}
#' @seealso \code{\link{ggdendrogram}}
#' @author Chris Wallace, based on code in ggdendrogram by Andrie de Vries
#' @examples
#' library(ggplot2)
#' hc <- hclust(dist(USArrests), "ave")
#' ### demonstrate plotting directly from object class hclust
#' p <- ggdendrogram(hc, rotate=FALSE)
#' print(p)
#' ggdendrotracks(hc,tracks=USArrests[,"UrbanPop",drop=FALSE],trackheight=10)
#' ggdendrotracks(hc,tracks=USArrests[,"UrbanPop",drop=FALSE],trackheight=10, rotate=TRUE)
#' ### demonstrate converting hclust to dendro using dendro_data first
#' hcdata <- dendro_data(hc)
#' ggdendrogram(hcdata, rotate=TRUE, size=2) + labs(title="Dendrogram in ggplot2")
#' ggdendrotracks(hcdata, tracks=USArrests[,"UrbanPop",drop=FALSE],rotate=TRUE, size=2, trackheight=10) + labs(title="Dendrogram in ggplot2")
ggdendrotracks <- function(data, tracks=NULL, segments=TRUE, labels=TRUE, leaf_labels=TRUE, 
    rotate=FALSE, theme_dendro=TRUE, trackheight=0.1, ...){
  dataClass <- if(inherits(data, "dendro")) data$class else class(data)
  angle <- if(dataClass %in% c("dendrogram", "hclust")){
        90 #ifelse(rotate, 0, 90)
      } else {
        0 #ifelse(rotate, 90, 0)
      }
  yangle <- if(dataClass %in% c("dendrogram", "hclust")){
        0 #ifelse(rotate, 90, 0)
      } else {
        90 #ifelse(rotate, 0, 90)
      }
  hjust <- if(dataClass %in% c("dendrogram", "hclust")){
        ifelse(rotate, 0, 1)
      } else {
        0.5
      }
  if(!is.dendro(data)) data <- dendro_data(data)
  p <- ggplot()
  if(all(segments, !is.null(data$segments))){
    p <- p + geom_segment(data=segment(data), 
        aes_string(x="x", y="y", xend="xend", yend="yend"))
  }
#   if(all(labels, !is.null(data$labels))){
#     p <- p + geom_text(data=label(data), 
#         aes_string(x="x", y="y", label="label"), hjust=hjust, angle=angle, ...)
#   }
  if(!is.null(tracks)) {
      labs <- levels(data$labels$label)      
      tracks <- tracks[labs,,drop=FALSE]
      tracks$x <- 1:nrow(tracks)
      tracks <- melt(tracks,"x")
      tracks$y <- - as.numeric(as.factor(tracks$variable))*trackheight
      tracks$ymin <- tracks$y - trackheight/2
      tracks$ymax <- tracks$y + trackheight/2
      tracks$xmin <- tracks$x - 0.5
      tracks$xmax <- tracks$x + 0.5
      p <- p +
        geom_rect(data=tracks,
                  aes_string(xmin="xmin",xmax="xmax",ymin="ymin",
                             ymax="ymax",fill="value"),
                  colour="grey")
    }
  if(all(leaf_labels, !is.null(data$leaf_labels))){    
    leaves <- leaf_label(data)
    if(!is.null(tracks))
      leaves$y <- leaves$y - trackheight * length(unique(tracks$y))
    p <- p + geom_text(data=leaves, 
        aes_string(x="x", y="y", label="label"), hjust=hjust, angle=angle, ...)
  }
  if(rotate){
    p <- p + scale_x_discrete(breaks=data$labels$x,labels=data$labels$label)
  } else {
    p <- p + scale_x_discrete(breaks=data$labels$x,labels=data$labels$label)
  }
  ## construct yscale
  y.breaks <- c(unique(tracks$y),pretty_breaks(4)(data$segments$y))
  y.labels <- c(unique(as.character(tracks$variable)),pretty_breaks(4)(data$segments$y))
  if(rotate){
    p <- p + coord_flip()
    p <- p + scale_y_continuous(breaks=y.breaks,labels=y.labels)
  } else {
    p <- p + scale_y_continuous(breaks=y.breaks,labels=y.labels)
  }
  if(theme_dendro) p <- p + theme_dendro()
  p <- p + theme(axis.text.x = element_text(angle=angle, hjust=1))
  p <- p + theme(axis.text.y = element_text(angle=yangle, hjust=1))

  p
}


#' Creates completely blank theme in ggplot.
#' 
#' Sets most of the \code{ggplot} options to blank, by returning blank \code{theme} elements for the panel grid, panel background, axis title, axis text, axis line and axis ticks.
#' @export
theme_dendro <- function(){
  element_blank <- ggplot2::element_blank
  ggplot2::theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title.x = element_text(colour=NA),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank()
  )
}


