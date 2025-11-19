# Coerces object to class dendro.

Method for coercing object to class dendro.

## Usage

``` r
as.dendro(segments, labels, leaf_labels = NULL, class)
```

## Arguments

- segments:

  data.frame with segment data

- labels:

  data.frame with labels data

- leaf_labels:

  data.frame with leaf label data

- class:

  The class of the original model object, e.g. "hclust". This is used by
  [`ggdendrogram()`](ggdendrogram.md) to determine the angle and
  justification of labels

## See also

[`dendro_data()`](dendro_data.md) and
[`ggdendro-package()`](ggdendro-package.md)
