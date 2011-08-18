# Test functionality using testthat library
# 
# Author: Andrie
###############################################################################





#dendro_data(dhc, type="rectangle")$segments
#data_dendrogram(dhc, type="rectangle")$text

#data_dendrogram(dhc, type="triangle")
# ggplot(dhcdata) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1)) + coord_flip()
# dhcdata <- data_dendrogram(dhc, type="triangle")
# ggplot(dhcdata) + geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1))


### tree
data(cpus, package="MASS")
require(tree)
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
#dtree <- dendro_data(cpus.ltr)
#str(dtree)


### hclust
#hc <- hclust(dist(USArrests), "ave")
#dhcdata <- dendro_data(hc, type="rectangle")
#dhclabels <- cluster_label(hc, type="rectangle")
#ggplot() + 
#		geom_segment(data=dhcdata, aes(x=x0, y=y0, xend=x1, yend=y1)) +
#		geom_text(data=dhclabels, aes(x=x, y=y, label=text), size=3, hjust=0) +
#		coord_flip() + scale_y_reverse(expand=c(0.2, 0))

### kmeans
data(iris)
iris <- iris[, -5]
#model <- kmeans(d, 3)
#cdata <- dendro_data(model)
#pc <- princomp(d)
#pcdata <- dendro_data(pc)
#eedata <- cbind(cdata, pcdata)
#eldata <- ellipsoid_data(eedata)
#
#ggplot() + 
#		geom_point(data=cbind(cdata, pcdata), aes(x=x, y=y, colour=factor(cluster))) +
#		geom_polygon(data=eldata, aes(x=x, y=y, colour=factor(cluster), group=cluster), alpha=0.1)



###############################################################################

context("dendrogram")

test_that("data_dendrogram() returns a correct classes", {

			hc <- hclust(dist(USArrests), "ave")
			dhc <- as.dendrogram(hc)
			
			expect_that(dendro_data(dhc, type="rectangle"), is_a("dendro"))

			ddata <- dendro_data(dhc, type="rectangle")
			expect_that(ddata$segments, is_a("data.frame"))
			expect_that(ddata$labels, is_a("data.frame"))
			expect_that(nrow(ddata$segments), equals(196))
			
			ddata <- dendro_data(dhc, type="triangle")
			expect_that(ddata$segments, is_a("data.frame"))
			expect_that(ddata$labels, is_a("data.frame"))
			expect_that(nrow(ddata$segments), equals(98))
			
		})

context("hclust")

test_that("data_hclust() returns the correct classes", {
			
			hc <- hclust(dist(USArrests), "ave")

			expect_that(dendro_data(hc, type="rectangle"), is_a("dendro"))
			
			ddata <- dendro_data(hc, type="rectangle")
			expect_that(segment(ddata), is_a("data.frame"))
			expect_that(label(ddata), is_a("data.frame"))
			expect_that(nrow(segment(ddata)), equals(196))
			
		})


context("tree")

test_that("data_tree() returns the correct classes", {
			
			expect_that(dendro_data(cpus.ltr), is_a("dendro"))
			tdata <- dendro_data(cpus.ltr)
			expect_that(segment(tdata), is_a("data.frame"))
			expect_that(label(tdata), is_a("data.frame"))
			expect_that(leaf_label(tdata), is_a("data.frame"))
			
		})

context("clusterplots and dendrograms")

test_that("dendrogram plots", {
			
    require(ggplot2)  
		hc <- hclust(dist(USArrests), "ave")
		hcdata <- dendro_data(hc, type="rectangle")
		p <- ggplot() + 
				geom_segment(data=segment(hcdata), aes(x=x0, y=y0, xend=x1, yend=y1)) +
				geom_text(data=label(hcdata), aes(x=x, y=y, label=text)) +
				coord_flip() + scale_y_reverse(expand=c(0.2, 0))
		expect_that(p, is_a("ggplot"))
    print(p)
	 
		})


context("dendro_data default")

test_that("undefined model type throws error", {
      expect_that(dendro_data(USArrests), throws_error())
    })

context("ggdendrogram")

test_that("ggdendrogram plots and accepts ... parameters", {
      
      require(ggplot2)  
      hc <- hclust(dist(USArrests), "ave")
      p1 <- ggdendrogram(hc)
      expect_that(p1, is_a("ggplot"))
      p2 <- ggdendrogram(hc, size=2)
      expect_that(p2, is_a("ggplot"))
      print(p1)
      print(p2)
      
    })

