# Diameter above buttresses point cloud

Returns the diameter above buttresses (DAB) and the functional diameter
above buttresses (fDAB) of a tree measured from a tree point cloud.

## Usage

``` r
dab_pc(
  pc,
  thresholdbuttress = 0.001,
  maxbuttressheight = 7,
  slice_thickness = 0.06,
  functional = FALSE,
  concavity = 4,
  dtm = NA,
  r = 5,
  plot = FALSE,
  plotcolors = c("#000000", "#808080", "#1c027a", "#08aa7c", "#fac87f")
)
```

## Arguments

- pc:

  The tree point cloud as a data.frame with columns X,Y,Z. Output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).

- thresholdbuttress:

  Numeric value (default=0.001) that is multiplied with the radius to
  determine if the stem is circular or irregular at the height the slice
  is taken. For example with the default value 0.001: when the average
  residual (obtained after an initial circle fitting at 1.3 m) exceeds a
  value of 0.001 times the radius, indicating a non-circular (irregular)
  stem shape and presumably buttresses, the circle fitting process is
  repeated with a new slice 6 mm higher than the previous one until a
  slice above the buttresses is reached.

- maxbuttressheight:

  Numeric value (default=7) that limits the height at which the diameter
  is measured. When this height is reached (because residuals do not
  become smaller than thresholdbuttress \* R), the thresholdbuttress
  value is increased with 0.0005 and the fitting starts again at 1.3 m.

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

  list of five colors for plotting. Only relevant when plot = TRUE. The
  stem points above buttresses, stem points at breast height, fitted
  circle, the concave hull and the estimated center are colored by the
  first, second, third, fourth and fifth element of this list
  respectively.

## Value

List with the diameter of the stem above buttresses, the residuals on
the fitting, the estimated center of the circle fit, and the functional
diameter at breast height. Also optionally (plot=TRUE) plots the circle
fitting on the horizontal slice which is then included in the list
output.

## Details

The DAB is measured as the diameter of the optimal circle fitted through
a 6mm thick horizontal slice taken above the buttresses using using
[`diameter_slice_pc`](https://lmterryn.github.io/ITSMe/reference/diameter_slice_pc.md).
A least squares circle fitting algorithm was applied to find the optimal
fit. The height at which the horizontal slice is taken, is determined
iteratively. Starting at 1.27 m to 1.33 m from the lowest point of the
tree point cloud. The average residual between the points and the fitted
circle is calculated. When the average residual exceeds a value of
"thresholdbuttress" times the radius, indicating a non-circular
(irregular) stem shape and presumably buttresses, the process is
repeated with a new slice 6 mm higher than the previous one until a
slice above the buttresses is reached. When the "maxbuttressheight" is
exceeded the iterative process is restarted with a "thresholdbuttress"
increased with 0.0005. At the determined height above buttresses also
the functional diameter is calculated using
[`diameter_slice_pc`](https://lmterryn.github.io/ITSMe/reference/diameter_slice_pc.md).
When the bottom of the point cloud is incomplete or obstructed you can
choose to add a digital terrain model as an input which is used to
estimate lowest point of the point cloud in order to obtain slices at
the correct height of the tree.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree point cloud and calculate the DAB
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
dab <- dab_pc(pc = pc_tree)
# and plot the circle fitting
output <- dab_pc(pc = pc_tree, plot = TRUE)
dab <- output$dab
# with non-default settings
dab <- dab_pc(pc = pc_tree, thresholdbuttress = 0.002, maxbuttressheight = 5)
} # }
```
