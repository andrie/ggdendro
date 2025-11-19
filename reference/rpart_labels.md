# Extract labels data frame from rpart object for plotting using ggplot.

This code is modified from the original plot.rpart in package rpart.

## Usage

``` r
rpart_labels(x)
```

## Value

a list with two elements: \$labels and \$leaf_labels

## See also

[`ggdendrogram()`](ggdendrogram.md)

Other dendro_data methods: [`dendro_data()`](dendro_data.md),
[`dendro_data.rpart()`](dendro_data.rpart.md),
[`dendro_data.tree()`](dendro_data.tree.md),
[`dendrogram_data()`](dendrogram_data.md)

Other rpart functions: [`dendro_data.rpart()`](dendro_data.rpart.md),
[`rpart_segments()`](rpart_segments.md)

## Author

Original author Brian Ripley
