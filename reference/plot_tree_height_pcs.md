# Calculate and save figures of [`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md) function

Calculates the tree height and saves the figures acquired when running
[`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md)
on multiple tree point clouds in a folder.

## Usage

``` r
plot_tree_height_pcs(
  PCs_path,
  extension = ".txt",
  dtm = NA,
  r = 5,
  OUT_path = "./",
  plotcolors = c("#000000", "#08aa7c", "#fac87f")
)
```

## Arguments

- PCs_path:

  A character with the path to the folder that contains the tree point
  clouds.

- extension:

  A character refering to the file extension of the point cloud files
  (default=".txt"). Can be ".txt", ".ply" or ".las".

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
  tree points, the lowest point height and the DTM points are colored by
  the first, second and third element of this list respectively.

## Value

A list with in the first element a numeric containing the tree height
values for each tree point cloud. In the second element there is the
list with the plots. Figures are also saved in the output folder.

## Details

Uses
[`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md)
to read the point clouds and
[`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md)
with parameter plot = TRUE to calculate the tree height and plot the
tree point cloud.

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate tree height and save figures
height_values <- plot_tree_height_pcs(
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt",
  OUT_path = "path/to/figure/folder/"
)
# Calculate tree height using dtm of resolution 2 and save figures
dtm_df <- read_tree_pc("path/to/dtm.txt")
height_values <- plot_tree_height_pcs(
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt", dtm = dtm_df, r = 2,
  OUT_path = "path/to/figure/folder/"
)
} # }
```
