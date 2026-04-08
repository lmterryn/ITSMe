# Tree height

Returns the tree height.

## Usage

``` r
tree_height(treedata, pc = NA, dtm = NA, r = 5)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- pc:

  The tree point cloud as a data.frame with columns X,Y,Z. Output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).
  If the point cloud is not available NA is used as input (default=NA).

- dtm:

  The digital terrain model as a data.frame with columns X,Y,Z (default
  = NA). If the digital terrain model is in the same format as a point
  cloud it can also be read with
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).
  only relevant when a point cloud is provided.

- r:

  Numeric value (default=5) r which determines the range taken for the
  dtm. Should be at least the resolution of the dtm. Only relevant when
  a dtm is provided.

## Value

The tree height in meters.

## Details

If the tree point cloud is available the tree_height calculation is
based on the point cloud (most accurate) with
[`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md).
If the tree point cloud is not available the tree height is based on the
treeQSM with
[`tree_height_qsm`](https://lmterryn.github.io/ITSMe/reference/tree_height_qsm.md).
When the bottom of the point cloud is incomplete or obstructed you can
choose to add a digital terrain model as an input which is used to
estimate lowest point of the point cloud in order to obtain slices at
the correct height of the tree.

## Examples

``` r
if (FALSE) { # \dontrun{
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
h <- tree_height(treedata = qsm$treedata)
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
h <- tree_height(treedata = qsm$treedata, pc = pc_tree)
} # }
```
