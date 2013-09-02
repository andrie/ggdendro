### R code from vignette source 'ggdendro.Rnw'

###################################################
### code chunk number 1: initialise
###################################################
library(ggplot2)
library(ggdendro)


###################################################
### code chunk number 2: dendrogram
###################################################
hc <- hclust(dist(USArrests), "ave")
p <- ggdendrogram(hc, rotate=FALSE, size=2)
print(p)


###################################################
### code chunk number 3: dendro1
###################################################
hc <- hclust(dist(USArrests), "ave")
dhc <- as.dendrogram(hc)
# Rectangular lines
ddata <- dendro_data(dhc, type="rectangle")
p <- ggplot(segment(ddata)) + 
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
    coord_flip() + scale_y_reverse(expand=c(0.2, 0))
print(p)


###################################################
### code chunk number 4: dendro2
###################################################
p <- p + coord_flip() + theme_dendro()
print(p)


###################################################
### code chunk number 5: dendro3
###################################################
ddata <- dendro_data(dhc, type="triangle")
p <- ggplot(segment(ddata)) + 
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
    coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
    theme_dendro()
print(p)


###################################################
### code chunk number 6: tree1
###################################################
require(tree)
data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ 
        syct+mmin+mmax+cach+chmin+chmax, cpus)
tree_data <- dendro_data(cpus.ltr)
p <- ggplot(segment(tree_data)) +
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend, size=n), 
        colour="blue", alpha=0.5) +
    scale_size("n") +
    geom_text(data=label(tree_data), 
        aes(x=x, y=y, label=label), vjust=-0.5, size=3) +
    geom_text(data=leaf_label(tree_data), 
        aes(x=x, y=y, label=label), vjust=0.5, size=2) +
    theme_dendro()
print(p)


###################################################
### code chunk number 7: rpart1
###################################################
library(rpart)
fit <- rpart(Kyphosis ~ Age + Number + Start, 
    method="class", data=kyphosis)
fitr <- dendro_data(fit)
p <- ggplot() + 
    geom_segment(data=fitr$segments, 
        aes(x=x, y=y, xend=xend, yend=yend)) + 
    geom_text(data=fitr$labels, 
        aes(x=x, y=y, label=label), size=3, vjust=0) +
    geom_text(data=fitr$leaf_labels, 
        aes(x=x, y=y, label=label), size=3, vjust=1) +
    theme_dendro()
print(p)


