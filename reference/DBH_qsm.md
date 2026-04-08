# Diameter at breast height TreeQSM

Extracts the DBH from the treedata of a TreeQSM.

## Usage

``` r
dbh_qsm(treedata)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The DBH of the TreeQSM in meters.

## Details

The DBH is calculated as the diameter of the cylinder in the QSM at the
right height (cylinder at 1.3 m). If the trunk was modeled with
triangulation the DBH is calculated as mean length of the diagonals in
the triangulation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and extract DBH
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
dbh <- dbh_qsm(treedata = qsm$treedata)
} # }
```
