# Calculate and save figures of [`diameter_slice_pc`](https://lmterryn.github.io/ITSMe/reference/diameter_slice_pc.md) function

Calculates the diameter and saves the figures acquired when running
[`diameter_slice_pc`](https://lmterryn.github.io/ITSMe/reference/diameter_slice_pc.md)
on multiple tree point clouds in a folder.

## Usage

``` r
plot_circle_fit_pcs(
  PCs_path,
  extension = ".txt",
  slice_height = 1.3,
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

- slice_height:

  Numeric value (default = 1.3) that determines the height above the
  lowest point of the point cloud at which the diameter is measured.
  Parameter of the
  [`diameter_slice_pc`](https://lmterryn.github.io/ITSMe/reference/diameter_slice_pc.md)
  function used to calculate the diameter of a stem slice.

- slice_thickness:

  Numeric value (default = 0.6) that determines the thickness of the
  slice which is used to measure the diameter. Parameter of the
  [`diameter_slice_pc`](https://lmterryn.github.io/ITSMe/reference/diameter_slice_pc.md)
  function used to calculate the diameter of a stem slice.

- functional:

  Logical (default=FALSE), indicates if the functional diameter should
  be calculated.

- concavity:

  Numeric value (default=2) concavity for the computation of the
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

A list with in the first element a numeric containing the diameter
values for each tree point cloud, the second element the residuals on
the circle fittings, the third element the functional diameters. In the
fourth element there is the list with the plots. Figures are also saved
in the output folder.

## Details

Uses
[`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md)
to read the point clouds and
[`diameter_slice_pc`](https://lmterryn.github.io/ITSMe/reference/diameter_slice_pc.md)
with parameter plot = TRUE to calculate the diameter and plot the circle
fitting.

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate diameters at breast height (1.3m) and save circle fitting figures
diam_values <- plot_circle_fit_pcs(
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt",
  OUT_path = "path/to/figure/folder/"
)
# Calculate diameters at 2.5 m with a slice of 20 cm and save the figures
diam_values <- plot_circle_fit_pcs(
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt", slice_height = 2.5,
  slice_thickness = 0.2,
  OUT_path = "path/to/figure/folder/"
)
} # }
```
