# Total branch volume TreeQSM

Extracts the total branch volume from the treedata of a TreeQSM.

## Usage

``` r
total_branch_volume_qsm(treedata)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The total branch volume of the TreeQSM in liters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and extract total branch volume
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
branchvol <- total_branch_volume_qsm(treedata = qsm$treedata)
} # }
```
