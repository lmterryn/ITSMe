# Crown classification point cloud

Returns the crown points from a tree point cloud.

## Usage

``` r
classify_crown_pc(
  pc,
  thresholdbranch = 1.5,
  minheight = 1,
  buttress = FALSE,
  thresholdR2 = 0.001,
  slice_thickness = 0.06,
  thresholdbuttress = 0.001,
  maxbuttressheight = 7,
  concavity = 4,
  dtm = NA,
  r = 5,
  plot = FALSE,
  plotcolors = c("#08aa7c", "#fac87f")
)
```

## Arguments

- pc:

  The tree point cloud as a data.frame with columns X,Y,Z. Output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).

- thresholdbranch:

  Numeric value (default=1.5) that is multiplied with the diameter of
  the tree (calculated with
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md) or
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md) when
  buttress =TRUE) which determines the cutt-off where a branch emerges
  and the crown begins.

- minheight:

  Numeric value (default=1) with the minimum height at which the crown
  begins. Should be above the widest part of the buttresses for
  buttressed trees (value of 4 is recommended). For non-buttressed trees
  choose a lower value (such as 1).

- buttress:

  Logical (default=FALSE), indicates if the trees have buttresses
  (higher than breast height).

- thresholdR2:

  Numeric value (default=0.001). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md)
  function used to calculate the diameter at breast height. Only
  relevant when buttress == FALSE.

- slice_thickness:

  Numeric value (default = 0.06). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md)
  function used to calculate the diameter at breast height. Only
  relevant when buttress == FALSE.

- thresholdbuttress:

  Numeric value (default=0.001). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter above buttresses. Only
  relevant when buttress == TRUE.

- maxbuttressheight:

  Numeric value (default=7). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter at breast height. Only
  relevant when buttress == TRUE.

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

  Logical (default=FALSE), indicates if the classified tree is plotted.

- plotcolors:

  list of two colors for plotting. Only relevant when plot = TRUE. The
  crown and trunk are colored by the first and second element of this
  list respectively.

## Value

List with data.frame with the crown point cloud (part of the tree above
the first branch) in crownpoints and data.frame with the trunk point
cloud in trunkpoints. Also optionally (plot=TRUE) plots the crown vs
non-crown points and in this case returns a list with the crown point
cloud as first element, trunk point clouds as a second element and the
plots as the third to fifth elements.

## Details

The classification is based on the increased distance between the
minimum and maximum X (and Y) coordinates of the tree points within a
horizontal slice when the first branch is reached with increasing
height. When the bottom of the point cloud is incomplete or obstructed
you can choose to add a digital terrain model as an input which is used
to estimate lowest point of the point cloud in order to obtain slices at
the correct height of the tree.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree point cloud and extract the crown points
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
crown_pc <- classify_crown_pc(pc = pc_tree)
# and plot the classification results
output <- classify_crown_pc(pc = pc_tree, plot = TRUE)
crown_pc <- output$crownpoints
# with non-default settings for a buttressed tree
crown_pc <- classify_crown_pc(pc = pc_tree, minheight = 4, buttress = TRUE)
} # }
```
