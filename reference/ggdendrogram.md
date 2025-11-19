# Creates dendrogram plot using ggplot.

This is a convenience function

## Usage

``` r
ggdendrogram(
  data,
  segments = TRUE,
  labels = TRUE,
  leaf_labels = TRUE,
  rotate = FALSE,
  theme_dendro = TRUE,
  ...
)
```

## Arguments

- data:

  Either a dendro object or an object that can be coerced to class
  dendro using the [`dendro_data()`](dendro_data.md) function, i.e.
  objects of class dendrogram, hclust or tree

- segments:

  If TRUE, show line segments

- labels:

  if TRUE, shows segment labels

- leaf_labels:

  if TRUE, shows leaf labels

- rotate:

  if TRUE, rotates plot by 90 degrees

- theme_dendro:

  if TRUE, applies a blank theme to plot (see
  [`theme_dendro()`](theme_dendro.md))

- ...:

  other parameters passed to
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object

## See also

[`dendro_data()`](dendro_data.md)

## Examples

``` r
### Demonstrate ggdendrogram

library(ggplot2)
hc <- hclust(dist(USArrests), "ave")

# Demonstrate plotting directly from object class hclust
p <- ggdendrogram(hc, rotate = FALSE)
print(p)

ggdendrogram(hc, rotate = TRUE)


# demonstrate converting hclust to dendro using dendro_data first
hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate = TRUE, size = 2) + 
  labs(title = "Dendrogram in ggplot2")
```
