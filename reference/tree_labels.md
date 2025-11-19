# Extract labels data frame from tree object for plotting using ggplot.

Extract labels data frame from tree object for plotting using ggplot.

## Usage

``` r
tree_labels(model, uniform, ...)
```

## Arguments

- model:

  object of class "tree", e.g. the output of tree()

- ...:

  ignored

## Value

a list with two elements: \$labels and \$leaf_labels

## See also

[`ggdendrogram()`](ggdendrogram.md)

Other tree functions: [`dendro_data.tree()`](dendro_data.tree.md),
[`get_data_tree_leaf_labels()`](get_data_tree_leaf_labels.md),
[`tree_segments()`](tree_segments.md)

## Author

Code modified from original by Brian Ripley
