# Alpha-shape Volume point cloud

Returns the alpha shape volume measured from a tree point cloud.

## Usage

``` r
alpha_volume_pc(pc, alpha = 1, plot = FALSE, plotcolor = "#fac87f")
```

## Arguments

- pc:

  The point cloud as a data.frame with columns X,Y,Z (e.g. output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md)).

- alpha:

  Numeric value (default=1) alpha for the computation of the 3D
  alpha-shape of the point cloud based on
  [`ashape3d`](https://rdrr.io/pkg/alphashape3d/man/ashape3d.html).

- plot:

  Logical (default=FALSE), indicates if the alpha-shape is plotted.

- plotcolor:

  color for plotting 3D shape. Only relevant when plot = TRUE.

## Value

The volume of the point cloud (numeric value) as the volume of the 3D
alpha-shape computed from the points of point cloud. Also optionally
(plot=TRUE) plots the alpha-shape and in this case returns a list with
the volume of the point cloud as first element and the alphashape3d
object as the second element. The 3D plot can be reconstructed using
plot(output\$alphashape3d).

## Details

This function uses
[`ashape3d`](https://rdrr.io/pkg/alphashape3d/man/ashape3d.html) and
[`volume_ashape3d`](https://rdrr.io/pkg/alphashape3d/man/volume_ashape3d.html)
to calculate the volume of 3D the alpha-shape fitted to the point cloud.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree point cloud and calculate the volume
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
vol_tree <- alpha_volume_pc(pc = pc_tree)
# and plot the 3D alpha-shape
output <- alpha_volume_pc(pc = pc_tree, plot = TRUE)
vol_tree <- output$volume
# classify the tree point cloud and calculate the crown volume
crown_pc <- classify_crown_pc(
  pc, thresholdbranch, minheight, buttress,
  thresholdR2, thresholdbuttress,
  maxbuttressheight, FALSE
)
vol_crown <- alpha_volume_pc(pc = crown_pc$crownpoints, alpha = 2)
} # }
```
