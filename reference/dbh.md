# Diameter at breast height/above buttresses

Returns the diameter at breast height or diameter above buttresses.

## Usage

``` r
dbh(
  treedata,
  pc = NA,
  buttress = FALSE,
  thresholdR2 = 0.001,
  slice_thickness = 0.06,
  thresholdbuttress = 0.001,
  maxbuttressheight = 7,
  concavity = 4,
  dtm = NA,
  r = 5
)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- pc:

  The tree point cloud as a data.frame with columns X,Y,Z. Output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).
  Default is NA and indicates no tree point cloud is available.

- buttress:

  Logical (default=FALSE), indicates if the trees have buttresses. Only
  relevant if pc is available.

- thresholdR2:

  Numeric value (default=0.001). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md)
  function used to calculate the diameter at breast height. Only
  relevant if the tree point cloud is available and buttress == FALSE.

- slice_thickness:

  Numeric value (default = 0.06). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md) and
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  functions used to calculate the diameter at breast height and above
  buttresses. Only relevant if the tree point cloud is available.

- thresholdbuttress:

  Numeric value (default=0.001). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter above buttresses. Only
  relevant if the tree point cloud is available and buttress == TRUE.

- maxbuttressheight:

  Numeric value (default=7). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter at breast height. Only
  relevant if the tree point cloud is available and buttress == TRUE.

- concavity:

  Numeric value (default=4) concavity for the computation of the
  functional diameter using a concave hull based on
  [`concaveman`](https://joelgombin.github.io/concaveman/reference/concaveman.html).
  Only relevant if the tree point cloud is available.

- dtm:

  The digital terrain model as a data.frame with columns X,Y,Z (default
  = NA). If the digital terrain model is in the same format as a point
  cloud it can also be read with
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).

- r:

  Numeric value (default=5) r which determines the range taken for the
  dtm. Should be at least the resolution of the dtm. Only relevant when
  a dtm is provided.

## Value

The dbh or dab in meters.

## Details

If the tree point cloud is available the calculations are based on the
point cloud (most accurate). In this case the diameter at breast height
(dbh) or diameter above buttresses (dab) is calculated with
[`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md) or
[`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
respectively. If the tree point cloud is not available the dbh is based
on the treeQSM with
[`dbh_qsm`](https://lmterryn.github.io/ITSMe/reference/DBH_qsm.md). When
the bottom of the point cloud is incomplete or obstructed you can choose
to add a digital terrain model as an input which is used to estimate
lowest point of the point cloud in order to obtain slices at the correct
height of the tree.

## Examples

``` r
if (FALSE) { # \dontrun{
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
DBH <- dbh(treedata = qsm$treedata)
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
DBH <- dbh(treedata = qsm$treedata, pc = pc_tree, buttress = TRUE)
} # }
```
