# Extract lower trunk point cloud

Returns the trunk points below 1.5 m (above the lowest point of the tree
point cloud).

## Usage

``` r
extract_lower_trunk_pc(
  pc,
  slice_thickness = 0.08,
  concavity = 4,
  dtm = NA,
  r = 5
)
```

## Arguments

- pc:

  The tree point cloud as a data.frame with columns X,Y,Z. Output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).

- slice_thickness:

  Numeric value (default = 0.08) that determines the thickness of the
  slice used to determine the lower trunk points.

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

## Value

Data.frame with the lower trunk point cloud (part of the trunk below 1.5
m).

## Details

This function iteratively adds trunk points to the trunk point cloud
starting from 0.15 m above the lowest point of the tree point cloud
(everything below 0.15 m is assumed to be trunk). For each slice as many
crown/branch points are removed based on kmeans clustering and the
distance of the clusters to the center of the previous slice. When the
bottom of the point cloud is incomplete or obstructed you can choose to
add a digital terrain model as an input which is used to estimate lowest
point of the point cloud in order to obtain slices at the correct height
of the tree. Support function used to determine the DBH from a tree
point cloud with
[`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree point cloud and calculate the DBH
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
trunk_pc <- extract_lower_trunk_pc(pc = pc_tree)
} # }
```
