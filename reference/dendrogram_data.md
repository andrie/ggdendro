# Extract data frame from dendrogram object for plotting using ggplot.

Extract data frame from dendrogram object for plotting using ggplot

## Usage

``` r
dendrogram_data(x, type = c("rectangle", "triangle"), ...)
```

## Arguments

- x:

  object of class "dendrogram", e.g. the output of as.dendrogram()

- type:

  The type of plot, indicating the shape of the dendrogram: "rectangle"
  will draw rectangular lines, while "triangle" will draw triangular
  lines.

- ...:

  ignored

## See also

[`ggdendrogram()`](ggdendrogram.md)

Other dendro_data methods: [`dendro_data()`](dendro_data.md),
[`dendro_data.rpart()`](dendro_data.rpart.md),
[`dendro_data.tree()`](dendro_data.tree.md),
[`rpart_labels()`](rpart_labels.md)

Other dendrogram/hclust functions: [`dendro_data()`](dendro_data.md)
