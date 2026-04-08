# Calculate and save figures of [`projected_area_pc`](https://lmterryn.github.io/ITSMe/reference/projected_area_pc.md) function

Calculates the projected (crown) area and saves the figures acquired
when running
[`projected_area_pc`](https://lmterryn.github.io/ITSMe/reference/projected_area_pc.md)
on multiple tree point clouds in a folder.

## Usage

``` r
plot_pa_pcs(
  PCs_path,
  extension = ".txt",
  OUT_path = "./",
  concavity = 2,
  crown = FALSE,
  thresholdbranch = 1.5,
  minheight = 1,
  buttress = FALSE,
  thresholdR2 = 0.001,
  slice_thickness = 0.06,
  thresholdbuttress = 0.001,
  maxbuttressheight = 7,
  concavity_classify = 4,
  dtm = NA,
  r = 5,
  plotcolors = c("#000000", "#08aa7c")
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

- concavity:

  Numeric value (default=2) concavity for the computation of a concave
  hull based on
  [`concaveman`](https://joelgombin.github.io/concaveman/reference/concaveman.html)
  in
  [`projected_area_pc`](https://lmterryn.github.io/ITSMe/reference/projected_area_pc.md).

- crown:

  Logical (default=FALSE), indicates if the area is calculated based on
  the full point clouds (crown = FALSE) or only on the crown point
  clouds (crown = TRUE).

- thresholdbranch:

  Numeric value (default=1.5) from
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).
  Only relevant when crown == TRUE.

- minheight:

  Numeric value (default=1) from
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).
  The default value is based on non-buttressed trees. Choose a higher
  value (e.g. 4) for buttressed trees. Only relevant when crown == TRUE.

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

- concavity_classify:

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

- plotcolors:

  list of two colors for plotting. Only relevant when plot = TRUE. The
  stem points and the concave hull are colored by the first and second
  element of this list respectively.

## Value

A list with in the first element a numeric containing the projected area
values for each tree point cloud. In the second element there is the
list with the plots. Figures are also saved in the output folder.

## Details

Uses
[`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md)
to read the point clouds and
[`projected_area_pc`](https://lmterryn.github.io/ITSMe/reference/projected_area_pc.md)
with parameter plot = TRUE to calculate the projected area and plot the
crown projection and fitting. Choose crown = TRUE, if you want to
calculate and plot the projected crown area. In this case a crown
classification is done using
[`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md)
and the crown points are used as an input to
[`projected_area_pc`](https://lmterryn.github.io/ITSMe/reference/projected_area_pc.md).
For buttressed trees, first optimise the thresholdbuttress,
maxbuttressheight, thresholdbranch and minheight parameter values using
[`plot_dab_fit_pcs`](https://lmterryn.github.io/ITSMe/reference/plot_dab_fit_pcs.md)
and
[`plot_crown_classification_pcs`](https://lmterryn.github.io/ITSMe/reference/plot_crown_classification_pcs.md)
and use those optimised values in this function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate projected area with default settings and save projection figures
pas <- plot_pa_pcs(
  PCs_path = "path/to/folder/PCs/", extension = ".txt",
  OUT_path = "path/to/figure/folder/"
)
# With non-default settings and save projection figures
pas <- plot_pa_pcs(
  PCs_path = "path/to/folder/PCs/", extension = ".txt",
  OUT_path = "path/to/figure/folder/", concavity = 3
)
# Calculate projected crown area and save projection figures
pcas <- plot_pa_pcs(
  PCs_path = "path/to/folder/PCs/", extension = ".txt",
  OUT_path = "path/to/figure/folder/", concavity = 3,
  crown = TRUE, minheight = 4, buttress = TRUE
)
} # }
```
