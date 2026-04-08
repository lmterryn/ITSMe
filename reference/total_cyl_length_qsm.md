# Total cylinder length TreeQSM

Extracts the total cylinder length from the treedata of a TreeQSM.

## Usage

``` r
total_cyl_length_qsm(treedata)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The total length of all the cylinders (branch and trunk) of a TreeQSM in
meters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and extract total cylinder length
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
tot_len <- total_cyl_length_qsm(treedata = qsm$treedata)
} # }
```
