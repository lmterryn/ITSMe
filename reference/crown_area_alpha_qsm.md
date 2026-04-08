# Alpha crown area TreeQSM

Extracts the alpha crown area from the treedata of a TreeQSM. This is
calculated during the TreeQSM process as the area (m\$^2\$) of the
crown's planar projection's alpha shape.

## Usage

``` r
crown_area_alpha_qsm(treedata)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The alpha crown area of the TreeQSM in square meters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and extract tree height
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
caa <- crown_area_alpha_qsm(treedata = qsm$treedata)
} # }
```
