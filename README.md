

<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggdendro <img src='man/figures/logo.png' align="right" height="139" />

Provides functions for creating dendrograms and tree plots using
`ggplot2`.

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggdendro)](https://CRAN.R-project.org/package=ggdendro)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/ggdendro)](http://www.r-pkg.org/pkg/ggdendro)
[![R-CMD-check](https://github.com/andrie/ggdendro/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/andrie/ggdendro/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/andrie/ggdendro/branch/main/graph/badge.svg)](https://app.codecov.io/gh/andrie/ggdendro?branch=main)
[![Project Status: Inactive – The project has reached a stable, usable
state but is no longer being actively developed; support/maintenance
will be provided as time
allows.](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
<!-- badges: end -->

The `ggdendro` package offers a generic function to extract data and
text from the various clustering models:

- `dendro_data()` extracts cluster information from the model object,
  e.g. cluster allocation, line segment data or label data.

The `dendro_data` object has methods for the following classes:

- `tree`
- `hclust`
- `dendrogram`
- `rpart`

These methods create an object of class `dendro`, which is essentially a
list of data frames. To extract the relevant data frames from the list,
use the three accessor functions:

- `segment()` for the line segment data
- `label()` for the text for each end segment
- `leaf_label()` for the leaf labels of a tree diagram

The results of these functions can then be passed to `ggplot()` for
plotting.

## Examples

``` r
library(ggplot2)
library(ggdendro)
hc <- hclust(dist(USArrests), "ave")
hcdata <- dendro_data(hc, type = "rectangle")
ggplot() +
  geom_segment(data = segment(hcdata), 
               aes(x = x, y = y, xend = xend, yend = yend)
  ) +
  geom_text(data = label(hcdata), 
            aes(x = x, y = y, label = label, hjust = 0), 
            size = 3
  ) +
  coord_flip() +
  scale_y_reverse(expand = c(0.2, 0))
```

<img src="man/figures/README-unnamed-chunk-2-1.png"
style="width:100.0%" />

``` r

### demonstrate plotting directly from object class hclust
ggdendrogram(hc)
```

<img src="man/figures/README-unnamed-chunk-2-2.png"
style="width:100.0%" />

``` r
ggdendrogram(hc, rotate = TRUE)
```

<img src="man/figures/README-unnamed-chunk-2-3.png"
style="width:100.0%" />

``` r

### demonstrate converting hclust to dendro using dendro_data first
hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate = TRUE) +
  labs(title = "Dendrogram in ggplot2")
```

<img src="man/figures/README-unnamed-chunk-2-4.png"
style="width:100.0%" />

# Use `dendextend` instead

Most of the functionality in `ggdendro` is included in the excellent
`dendextend` package. In most cases, if you need additional
functionality, please use the `dendextend` package instead.

The `ggdendro` package will only get minimal maintenance in future.

Refer to <https://cran.r-project.org/web/packages/dendextend/index.html>
