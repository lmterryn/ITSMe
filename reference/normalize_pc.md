# Normalize a tree point cloud

Normalizes a tree point cloud by subtracting from each column its
respective the min value (e.g. all X-values - min(all X-values)). When
the bottom of the point cloud is incomplete or obstructed you can choose
to add a digital terrain model as an input which is used to estimate
lowest point of the point cloud in order to obtain the correct
normalized height of the tree.

## Usage

``` r
normalize_pc(pc, dtm = NA, r = 5)
```

## Arguments

- pc:

  The tree point cloud as a data.frame with columns X,Y,Z. Output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).

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

Normalized point cloud as a data.frame with columns X,Y,Z.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree point cloud and normalise the point cloud
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
pc_norm <- normalize_pc(pc)
} # }
```
