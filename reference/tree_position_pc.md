# Tree point cloud position

Returns the (X,Y,Z)-position of a tree point cloud based on the mean X,
Y and Z value of the 100 lowest points of the tree point cloud.

## Usage

``` r
tree_position_pc(pc)
```

## Arguments

- pc:

  The tree point cloud as a data.frame with columns X,Y,Z. Output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).

## Value

Numeric with the X, Y, Z coordinates (location) of the tree stem.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree point cloud and calculate the tree position
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
pos <- tree_position_pc(pc = pc_tree)
} # }
```
