# TreeQSM position

Returns the (X,Y)-position of a treeQSM based on the start position of
the first cylinder that is higher than 1.3 m above ground.

## Usage

``` r
tree_position_qsm(cylinder)
```

## Arguments

- cylinder:

  Cylinder field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

Numeric with the XY coordinates (location) of the tree stem.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and calculate tree position
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
pos <- tree_position_qsm(cylinder = qsm$cylinder)
} # }
```
