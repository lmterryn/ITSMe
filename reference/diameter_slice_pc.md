# Diameter at certain height point cloud

Returns the diameter at a certain height of a tree measured from a tree
point cloud.

## Usage

``` r
diameter_slice_pc(
  pc,
  slice_height = 0.1,
  slice_thickness = 0.06,
  functional = FALSE,
  concavity = 4,
  dtm = NA,
  r = 5,
  plot = FALSE,
  plotcolors = c("#000000", "#1c027a", "#08aa7c", "#fac87f")
)
```

## Arguments

- pc:

  The tree point cloud as a data.frame with columns X,Y,Z. Output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).

- slice_height:

  Numeric value (default = 1.3) that determines the height above the
  lowest point of the point cloud at which the diameter is measured.

- slice_thickness:

  Numeric value (default = 0.6) that determines the thickness of the
  slice which is used to measure the diameter.

- functional:

  Logical (default=FALSE), indicates if the functional diameter should
  be calculated.

- concavity:

  Numeric value (default=4) concavity for the computation of the
  functional diameter using a concave hull based on
  [`concaveman`](https://joelgombin.github.io/concaveman/reference/concaveman.html).

- dtm:

  The digital terrain model as a data.frame with columns X,Y,Z (default
  = NA). If the digital terrain model is in the same format as a point
  cloud it can also be read with
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).

- r:

  Numeric value (default=5) r which determines the range taken for the
  dtm. Should be at least the resolution of the dtm. Only relevant when
  a dtm is provided.

- plot:

  Logical (default=FALSE), indicates if the optimized circle fitting is
  plotted.

- plotcolors:

  list of four colors for plotting. Only relevant when plot = TRUE. The
  stem points, fitted circle, the concave hull and the estimated center
  are colored by the first, second, third and fourth element of this
  list respectively.

## Value

A list with the diameter at a specified height (numeric value), the
residual between circle fit and the points, the center of the circle
fit, and the functional diameter calculated from the concave hull
fitting. Also optionally (plot=TRUE) plots the circle fitting on the
horizontal slice.

## Details

The diameter is measured of the optimal circle fitted through a
horizontal slice. A least squares circle fitting algorithm was applied
to find the optimal fit. The height and thickness of the slice can be
specified using slice_height and slice_thickness parameters.
Additionally, the functional diameter is calculated. For this the area
of the concave hull with (concavity 4) is determined on the slice. From
this area the diameter is determined as the diameter of a circle with
this area. When the bottom of the point cloud is incomplete or
obstructed you can choose to add a digital terrain model as an input
which is used to estimate lowest point of the point cloud in order to
obtain slices at the correct height of the tree. This function is also a
Support function used to determine the DBH from a tree point cloud with
[`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree point cloud and calculate the diameter
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
diameter <- diameter_slice_pc(pc = pc_tree)
# and plot the circle fitting
output <- diameter_slice_pc(pc = pc_tree, plot = TRUE)
diameter <- output$diameter
residual <- output$R2
center <- out$center
} # }
```
