# Save figures of [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md) function

Classifies the tree point clouds into crown and non crown points and
saves the figures with
[`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md)
for multiple tree point clouds in a folder. Use different values for the
thresholdbranch and minheight parameter to optimise the crown
classification for your tree point clouds. Mainly minheight parameter
needs to be optimised, small values (e.g

1.  when the trees don't have buttresses and higher values (e.g 4) for
    trees with buttresses. For buttressed trees, first optimise the
    thresholdbuttress and maxbuttressheight parameter values using
    [`plot_dab_fit_pcs`](https://lmterryn.github.io/ITSMe/reference/plot_dab_fit_pcs.md)
    and use those optimised values in this function.

## Usage

``` r
plot_crown_classification_pcs(
  PCs_path,
  extension = ".txt",
  OUT_path = "./",
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
  plotcolors = c("#08aa7c", "#fac87f")
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

- thresholdbranch:

  Numeric value (default=1.5) from
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).

- minheight:

  Numeric value (default=1) from
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).
  The default value is based on non-buttressed trees. Choose a higher
  value (e.g. 4) for buttressed trees.

- buttress:

  Logical (default=FALSE), indicates if the trees have buttresses
  (higher than breast height).

- thresholdR2:

  Numeric value (default=0.001). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md)
  function used to calculate the diameter at breast height. Only
  relevant when buttress == FALSE.

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
  Only relevant when buttress == TRUE.

- maxbuttressheight:

  Numeric value (default=7). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter above buttresses which is used
  in
  [`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md).
  Only relevant when buttress == TRUE.

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

- plotcolors:

  list of two colors for plotting. Only relevant when plot = TRUE. The
  crown and trunk are colored by the first and second element of this
  list respectively.

## Value

Returns a list with the plots and individual plots saved in the output
folder.

## Details

Uses
[`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md)
to read the point clouds and
[`classify_crown_pc`](https://lmterryn.github.io/ITSMe/reference/classify_crown_pc.md)
with parameter plot = TRUE to classify the tree point cloud and plot the
classification.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run the crown classification with default settings and save figures
plot_crown_classification_pcs(
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt",
  OUT_path = "path/to/figure/folder/"
)
# Run the crown classification with non-default settings and save figures
plot_crown_classification_pcs(
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt",
  OUT_path = "path/to/figure/folder/",
  thresholdbranch = 2, minheight = 4
)
# Run the crown classification with non-default settings and save figures
# for buttressed trees
plot_crown_classification_pcs(
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt",
  OUT_path = "path/to/figure/folder/",
  thresholdbranch = 2, minheight = 4,
  buttress = TRUE, thresholdbuttress = 0.002,
  maxbuttressheight = 5
)
} # }
```
