# Calculate and save figures of [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md) function

Calculates the dbh and saves the figures acquired when running
[`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md) on
multiple tree point clouds in a folder.

## Usage

``` r
plot_dbh_fit_pcs(
  PCs_path,
  extension = ".txt",
  thresholdR2 = 0.001,
  slice_thickness = 0.06,
  functional = TRUE,
  concavity = 4,
  dtm = NA,
  r = 5,
  OUT_path = "./",
  plotcolors = c("#000000", "#1c027a", "#08aa7c", "#fac87f")
)
```

## Arguments

- PCs_path:

  A character with the path to the folder that contains the tree point
  clouds.

- extension:

  A character refering to the file extension of the point cloud files
  (default=".txt"). Can be ".txt", ".ply" or ".las".

- thresholdR2:

  Numeric value (default=0.001). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md)
  function used to calculate the diameter at breast height.

- slice_thickness:

  Numeric value (default = 0.06). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md)
  function used to calculate the diameter at breast height.

- functional:

  Logical (default=FALSE), indicates if the functional diameter should
  be calculated.

- concavity:

  Numeric value (default=4) concavity for the computation of the
  functional diameter using a concave hull based on
  [`concaveman`](https://joelgombin.github.io/concaveman/reference/concaveman.html).

- dtm:

  The digital terrain model (default = NA), parameter of
  [`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md).

- r:

  Numeric value (default=5) r, parameter of
  [`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md).
  Only relevant if a dtm is provided.

- OUT_path:

  A character with the path to the folder where the figures should be
  saved (default = current folder).

- plotcolors:

  list of three colors for plotting. Only relevant when plot = TRUE. The
  stem points, fitted circle, the concave hull and the estimated center
  are colored by the first, second and third and fourth element of this
  list respectively.

## Value

A list with in the first element a numeric containing the dbh values for
each tree point cloud, the second element the residuals on the circle
fittings, the third element the functional diameters. In the fourth
element there is the list with the plots. Figures are also saved in the
output folder.

## Details

Uses
[`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md)
to read the point clouds and
[`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md) with
parameter plot = TRUE to calculate the dbh and plot the circle fitting.

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate DBHs and save circle fitting figures
dbh_values <- plot_dbh_fit_pcs(
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt",
  OUT_path = "path/to/figure/folder/"
)
} # }
```
