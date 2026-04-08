# Summary basic structural metrics tree point cloud for all trees in a folder.

Returns a summary data.frame containing the tree position
(X,Y-coordinates), tree height, diameter at breast height, functional
diameter at breast height, diameter above buttresses, functional
diameter above buttresses, projected (crown) area and (crown) volume.

## Usage

``` r
summary_basic_pointcloud_metrics(
  PCs_path,
  pattern = "",
  metrics = c("tree position", "stem diameter", "tree height", "projected area",
    "alpha volume"),
  dtm = NA,
  r = 5,
  crown = FALSE,
  thresholdbranch = 1.5,
  minheight = 1,
  concavity = 2,
  alpha = 1,
  buttress = FALSE,
  thresholdR2 = 0.001,
  slice_thickness = 0.06,
  thresholdbuttress = 0.001,
  maxbuttressheight = 7,
  functional = TRUE,
  concavity_fdiameter = 4,
  OUT_path = FALSE,
  overwrite = FALSE,
  plot = FALSE,
  plotcolors = c("#000000", "#808080", "#1c027a", "#08aa7c", "#fac87f"),
  parallel = FALSE,
  ncores = 4
)
```

## Arguments

- PCs_path:

  A character with the path to the folder that contains the tree point
  clouds.

- pattern:

  An optional regular expression. Only file names within the PCs_path
  which match the regular expression will be returned. This can be used
  to target specific extensions (e.g. ".txt") or target certain trees.
  Default is ” which means it will select all files in the PCs_path
  folder.

- metrics:

  A list of strings referring to the metrics you want to calculate. The
  possibilities are "tree position", "stem diameter", "tree height",
  "projected area", and "alpha volume".

- dtm:

  The digital terrain model from
  [`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md).

- r:

  Numeric value (default=5) r which determines the range taken for the
  dtm from
  [`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md).
  Only relevant if a dtm is provided.

- crown:

  Logical (default=FALSE), indicates if the area and volume is
  calculated based on the full point clouds (crown = FALSE) or only on
  the crown point clouds (crown = TRUE).

- thresholdbranch:

  Numeric value (default=1.5) from
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).
  Only relevant when crown == TRUE.

- minheight:

  Numeric value (default=1) from
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).
  The default value is based on non-buttressed trees. Choose a higher
  value (e.g. 4) for buttressed trees. Only relevant when crown == TRUE.

- concavity:

  Numeric value (default=2). Parameter of the
  [`projected_area_pc`](https://lmterryn.github.io/ITSMe/reference/projected_area_pc.md)
  function used to calculate the projected crown area.

- alpha:

  Numeric value (default=1). Parameter of the
  [`alpha_volume_pc`](https://lmterryn.github.io/ITSMe/reference/alpha_volume_pc.md)
  function used to calculate the crown volume.

- buttress:

  Logical (default=FALSE), indicates if the trees have buttresses
  (higher than breast height).

- thresholdR2:

  Numeric value (default=0.001). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md)
  function used to calculate the diameter at breast height. Only
  relevant if buttress == FALSE.

- slice_thickness:

  Numeric value (default = 0.06). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md) and
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  functions used to calculate the diameter at breast height and above
  buttresses.

- thresholdbuttress:

  Numeric value (default=0.001). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter above buttresses. Only
  relevant when buttress == TRUE.

- maxbuttressheight:

  Numeric value (default=7). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter above buttresses. Only
  relevant when buttress == TRUE.

- functional:

  Logical (default=FALSE), indicates if the functional diameter should
  be calculated.

- concavity_fdiameter:

  Numeric value (default=4) concavity for the computation of the
  functional diameter using a concave hull based on
  [`concaveman`](https://joelgombin.github.io/concaveman/reference/concaveman.html).
  This concavity value is used in the functions
  [`diameter_slice_pc`](https://lmterryn.github.io/ITSMe/reference/diameter_slice_pc.md),
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md),
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md), and
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).

- OUT_path:

  A character with name of the output file (including the path to the
  folder), where the summary csv file should be saved or logical
  (default=FALSE) in this case no csv file is produced.

- overwrite:

  Logical (default=FALSE), indicates if the output file can be
  overwritten.

- plot:

  Logical (default=FALSE), indicates if summary figure for each tree
  point cloud is plotted. If an OUT_path is provided, the figures are
  saved in the OUT_path.

- plotcolors:

  list of five colors for plotting. Only relevant when plot = TRUE. The
  stem points above buttresses, stem points at breast height, fitted
  circle, the concave hull and the estimated center are colored by the
  first, second, third, fourth and fifth element of this list
  respectively.

- parallel:

  Logical (default=FALSE), indicates if the function should be run in
  parallel on the trees.

- ncores:

  Number of cores used in the parallel computation. Only relevant if
  parallel == TRUE.

## Value

The summary of the basic structural metrics for multiple tree point
clouds as a data.frame. Includes the tree height, diameter at breast
height, diameter above buttresses, projected (crown) area and (crown)
volume. The summary is saved in a csv file if an output folder is
provided.

## Details

The tree position, tree height, diameter at breast height, functional
diameter at breast height, diameter above buttresses, functional
diameter above buttresses, projected (crown) area and (crown) volume are
otained with
[`tree_position_pc`](https://lmterryn.github.io/ITSMe/reference/tree_position_pc.md),
[`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md),
[`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md),
[`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md),
[`projected_area_pc`](https://lmterryn.github.io/ITSMe/reference/projected_area_pc.md)
and
[`alpha_volume_pc`](https://lmterryn.github.io/ITSMe/reference/alpha_volume_pc.md)
respectively.

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate the summary with default parameters and export to csv
summary <- summary_basic_pointcloud_metrics(
  PCs_path = "path/to/folder/PCs/",
  OUT_path = "path/to/out/folder/"
)
# Calculate the summary with non-default parameter values
# recommended for buttressed trees
summary <- summary_basic_pointcloud_metrics(
  PCs_path = "path/to/folder/PCs/",
  extension = ".ply", crown = TRUE,
  minheight = 4, buttress = TRUE
)
} # }
```
