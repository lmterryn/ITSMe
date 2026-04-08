# Diameter at breast height point cloud

Returns the diameter at breast height (DBH) and functional diameter at
breast height (fDBH) of a tree measured from a tree point cloud. There
should be only one stem at breast height.

## Usage

``` r
dbh_pc(
  pc,
  thresholdR2 = 0.001,
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

- thresholdR2:

  Numeric value (default=0.001) that is multiplied with the radius to
  determine if at breast height (1.3 m above the lowest point of the
  point cloud) the circle fit is influenced by branches. If the
  resulting value is exceeded, the lower trunk without branches is
  extracted using
  [`extract_lower_trunk_pc`](https://lmterryn.github.io/ITSMe/reference/extract_lower_trunk_pc.md).
  Increase the thresholdR2 if your point cloud quality is low (for
  example, errors in co-registration of point clouds in multi-scan due
  to wind-effect).

- slice_thickness:

  Numeric value (default = 0.06) that determines the thickness of the
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

  Logical (default=FALSE), indicates if the optimised circle fitting is
  plotted.

- plotcolors:

  list of four colors for plotting. Only relevant when plot = TRUE. The
  stem points, fitted circle, the concave hull and the estimated center
  are colored by the first, second and third and fourth element of this
  list respectively.

## Value

List with the diameter of the stem at breast height, the residuals on
the fitting, the estimated center of the circle fit, and the functional
diameter at breast height. Also optionally (plot=TRUE) plots the circle
fitting on the horizontal slice which is then included in the list
output.

## Details

The DBH is measured as the diameter of the optimal circle fitted through
a 6mm thick horizontal slice (from 1.27 m to 1.33 m above the lowest
tree point) using
[`diameter_slice_pc`](https://lmterryn.github.io/ITSMe/reference/diameter_slice_pc.md).
A least squares circle fitting algorithm is applied to find the optimal
fit. Also the functional diameter at breast height (fDBH) is determined
using
[`diameter_slice_pc`](https://lmterryn.github.io/ITSMe/reference/diameter_slice_pc.md).
For this the area of the concave hull with (concavity 4) is determined
on the slice. From this area the diameter is determined as the diameter
of a circle with this area. In case there are branches or foliage at
this height, the lower trunk is extracted using
[`extract_lower_trunk_pc`](https://lmterryn.github.io/ITSMe/reference/extract_lower_trunk_pc.md).
Wether this is the case is determined using the thresholdR2 parameter.
When the bottom of the point cloud is incomplete or obstructed you can
choose to add a digital terrain model as an input which is used to
estimate lowest point of the point cloud in order to obtain slices at
the correct height of the tree.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree point cloud and calculate the DBH
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
dbh <- dbh_pc(pc = pc_tree)
# and plot the circle fitting
output <- dbh_pc(pc = pc_tree, plot = TRUE)
dbh <- output$dbh
} # }
```
