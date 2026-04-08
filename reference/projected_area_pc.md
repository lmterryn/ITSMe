# Projected area point cloud

Returns the projected area measured from a point cloud.

## Usage

``` r
projected_area_pc(
  pc,
  concavity = 2,
  plot = FALSE,
  plotcolors = c("#000000", "#08aa7c")
)
```

## Arguments

- pc:

  The point cloud as a data.frame with columns X,Y,Z (e.g. output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).

- concavity:

  Numeric value (default=2) concavity for the computation of a concave
  hull based on
  [`concaveman`](https://joelgombin.github.io/concaveman/reference/concaveman.html).

- plot:

  Logical (default=FALSE), indicates if the optimised circle fitting is
  plotted.

- plotcolors:

  list of two colors for plotting. Only relevant when plot = TRUE. The
  stem points and the concave hull are colored by the first and second
  element of this list respectively.

## Value

The projected area (numeric value) as the area of the concave hull
computed from the points of point cloud. Also optionally (plot=TRUE)
plots the concave hull fitting and in this case returns a list with the
area as first element and the plot as the second element.

## Details

This function uses
[`st_area`](https://r-spatial.github.io/sf/reference/geos_measures.html)
and
[`concaveman`](https://joelgombin.github.io/concaveman/reference/concaveman.html)
to calculate the area of the concave hull fitted to the provided point
clouds.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree point cloud and calculate the projected tree area
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
pta <- projected_crown_area_pc(pc = pc_tree)
# and plot the concave hull fitting
output <- projected_crown_area_pc(pc = pc_tree, plot = TRUE)
pca <- output$pca
# classify the tree point cloud and calculate the projected crown area
crown_pc <- classify_crown_pc(
  pc, thresholdbranch, minheight, buttress,
  thresholdR2, thresholdbuttress,
  maxbuttressheight, FALSE
)
pca <- projected_crown_area_pc(pc = crown_pc$crownpoints)
} # }
```
