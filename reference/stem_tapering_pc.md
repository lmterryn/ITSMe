# Tapering of a trunk point cloud

Calculates the tapering of the trunk based on a linear regression on the
diameters at each height of the trunk.

## Usage

``` r
stem_tapering_pc(
  pc,
  slice_thickness = 0.1,
  maxtaperheight = 10,
  interval = 1,
  buttress = FALSE,
  thresholdbuttress = 0.0015,
  maxbuttressheight = 5,
  plot = FALSE
)
```

## Arguments

- pc:

  The trunk point cloud as a data.frame with columns X,Y,Z.

- slice_thickness:

  Numeric value (default = 0.1) that determines the thickness of the
  slice which is used to measure the diameter at each height.

- maxtaperheight:

  Numeric value (default = 10) that determines the maximum height used
  for the tapering calculation.

- interval:

  Numeric value (default = 1) that determines the interval over which
  the median of the diameters is taken to reduce influence of outliers
  for the taper measurement. Choose interval value equal to the
  slice_thickness if you want to use all the calculated diameters.

- buttress:

  Logical (default=FALSE), indicates if the trees have buttresses
  (higher than breast height).

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

- plot:

  Logical (default=FALSE), indicates if the taper curve is plotted.

## Value

list with tapering coefficients a and b, dataframe T with the diameters
at each respective height, and the taper plot.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read trunk point cloud
pc_trunk <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
# calculate the tapering
output <- stem_tapering_pc(pc_trunk)
} # }
```
