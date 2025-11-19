# Create Dendrograms and Tree Diagrams using 'ggplot2'

This package enables you to create dendrograms and tree plots using
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Details

The `ggplot2` philosophy is to clearly separate data from the
presentation. Unfortunately the plot method for dendrograms
([`plot.dendrogram()`](https://rdrr.io/r/stats/dendrogram.html)) plots
directly to a plot device without exposing the data. The `ggdendro`
package resolves this by making available functions that extract the
dendrogram plot data. This data can be used with `ggplot`.

The function [`dendro_data()`](dendro_data.md) extracts data from
different objects that contain dendrogram information. It is a generic
function with methods for:

- `hclust`: [`dendro_data.hclust()`](dendro_data.md)

- dendrogram: [`dendro_data.dendrogram()`](dendro_data.md)

- regression trees: [`dendro_data.tree()`](dendro_data.tree.md)

- partition trees: [`dendro_data.rpart()`](dendro_data.rpart.md)

- `agnes` and `diana`: [`dendro_data.twins()`](dendro_data.md)

These methods create an object of class `dendro`, consisting of a list
of data frames. To extract the relevant data frames from the list, you
can use the accessor functions:

- [`segment()`](segment.md): the line segment data

- [`label()`](segment.md): the text for each end segment

- [`leaf_label()`](segment.md): the leaf labels of a tree diagram

To plot a dendrogram, either construct a plot with
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
or use the function [`ggdendrogram()`](ggdendrogram.md).

## See also

[`dendro_data()`](dendro_data.md)

## Author

Andrie de Vries - <apdevries@gmail.com>
