require(ggplot2)

# Demonstrate dendro_data.dendrogram

hc <- hclust(dist(USArrests), "ave")
dhc <- as.dendrogram(hc)

# Rectangular lines
ddata <- dendro_data(dhc, type="rectangle")
ggplot(segment(ddata)) + 
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
  coord_flip() + 
  scale_y_reverse(expand=c(0.2, 0)) + 
  theme_dendro()

# Triangular lines
ddata <- dendro_data(dhc, type="triangle")
ggplot(segment(ddata)) + 
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
  theme_dendro()

# Demonstrate dendro_data.hclust

require(ggplot2)
hc <- hclust(dist(USArrests), "ave")

# Rectangular lines
hcdata <- dendro_data(hc, type="rectangle")
ggplot(segment(hcdata)) + 
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
  coord_flip() + 
  scale_y_reverse(expand=c(0.2, 0)) + 
  theme_dendro()

# Triangular lines
hcdata <- dendro_data(hc, type="triangle")
ggplot(segment(hcdata)) + 
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
  theme_dendro()
