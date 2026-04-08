# Tree height TreeQSM

Extracts the tree height from the treedata of a TreeQSM.

## Usage

``` r
tree_height_qsm(treedata)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The tree height of the TreeQSM in meters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and extract tree height
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
height <- tree_height_qsm(treedata = qsm$treedata)
} # }
```
