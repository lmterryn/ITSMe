# Calculate and save figures of [`alpha_volume_pc`](https://lmterryn.github.io/ITSMe/reference/alpha_volume_pc.md) function

Calculates the (crown) volume and saves the figures acquired when
running
[`alpha_volume_pc`](https://lmterryn.github.io/ITSMe/reference/alpha_volume_pc.md)
on multiple tree point clouds in a folder.

## Usage

``` r
plot_av_pcs(
  PCs_path,
  extension = ".txt",
  OUT_path = "./",
  alpha = 1,
  crown = FALSE,
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
  plotcolor = "#fac87f"
)
```

## Arguments

- PCs_path:

  A character with the path to the folder that contains the tree point
  clouds.

- extension:

  A character refering to the file extension of the point cloud files
  (default=".txt"). Can be ".txt", ".ply" or ".las".

- OUT_path:

  A character with the path to the folder where the figures should be
  saved (default = current folder).

- alpha:

  Numeric value (default=1) alpha for the computation of the 3D
  alpha-shape of the tree crown based on
  [`ashape3d`](https://rdrr.io/pkg/alphashape3d/man/ashape3d.html) in
  [`alpha_volume_pc`](https://lmterryn.github.io/ITSMe/reference/alpha_volume_pc.md).

- crown:

  Logical (default=FALSE), indicates if the volume is calculated based
  on the full point clouds (crown = FALSE) or only on the crown point
  clouds (crown = TRUE).

- thresholdbranch:

  Numeric value (default=1.5) from
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).
  Only relevant when crown == TRUE.

- minheight:

  Numeric value (default=1) from
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).
  The default value is based on non-buttressed trees. Choose a higher
  value (e.g. 4) for buttressed trees.

- buttress:

  Logical (default=FALSE), indicates if the trees have buttresses
  (higher than breast height). Only relevant when crown == TRUE.

- thresholdR2:

  Numeric value (default=0.001). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md)
  function used to calculate the diameter at breast height. Only
  relevant when crown == TRUE and buttress == FALSE.

- slice_thickness:

  Numeric value (default = 0.06). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md) and
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  functions used to calculate the diameter at breast height and above
  buttresses.

- thresholdbuttress:

  Numeric value (default=0.001). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter above buttresses which is used
  in
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).
  Only relevant when crown == TRUE and buttress == FALSE.

- maxbuttressheight:

  Numeric value (default=7). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter above buttresses which is used
  in
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).
  Only relevant when crown == TRUE and buttress == FALSE.

- concavity:

  Numeric value (default=4) concavity for the computation of the
  functional diameter using a concave hull based on
  [`concaveman`](https://joelgombin.github.io/concaveman/reference/concaveman.html)
  which is used in
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).

- dtm:

  The digital terrain model (default = NA), parameter of
  [`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md).

- r:

  Numeric value (default=5) r, parameter of
  [`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md).
  Only relevant if a dtm is provided.

- plotcolor:

  color for plotting 3D shape. Only relevant when plot = TRUE.

## Value

a numeric containing the volume values for each tree point cloud.
Figures are saved in the output folder.

## Details

Uses
[`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md)
to read the point clouds and
[`alpha_volume_pc`](https://lmterryn.github.io/ITSMe/reference/alpha_volume_pc.md)
with parameter plot = TRUE to calculate the volume and plot the 3D the
alpha-shape fitting. Choose crown = TRUE, if you want to calculate and
plot the crown volume. In this case a crown classification is done using
[`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md)
and the crown points are used as an input to
[`alpha_volume_pc`](https://lmterryn.github.io/ITSMe/reference/alpha_volume_pc.md).
For buttressed trees, first optimise the thresholdbuttress,
maxbuttressheight, thresholdbranch and minheight parameter values using
[`plot_dab_fit_pcs`](https://lmterryn.github.io/ITSMe/reference/plot_dab_fit_pcs.md)
and
[`plot_crown_classification_pcs`](https://lmterryn.github.io/ITSMe/reference/plot_crown_classification_pcs.md)
and use those optimised values in this function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate the volume with default settings and save alpha shape figures
vs <- plot_av_pcs(
  PCs_path = "path/to/folder/PCs/", extension = ".txt",
  OUT_path = "path/to/figure/folder/"
)
# Calculate the volume with non-default settings and save alpha shape figures
vs <- plot_av_pcs(
  PCs_path = "path/to/folder/PCs/", extension = ".txt",
  OUT_path = "path/to/figure/folder/", alpha = 2
)
# Calculate crown volume and save alpha shape figures
cvs <- plot_av_pcs(
  PCs_path = "path/to/folder/PCs/", extension = ".txt",
  OUT_path = "path/to/figure/folder/", alpha = 2,
  crown = TRUE, minheight = 4, buttress = TRUE
)
} # }
```
