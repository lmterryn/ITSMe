# Total branch length TreeQSM

Extracts the total branch length from the treedata of a TreeQSM.

## Usage

``` r
total_branch_length_qsm(treedata)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The total branch length of the TreeQSM in meters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and extract total branch length
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
branchlen <- total_branch_length_qsm(treedata = qsm$treedata)
} # }
```
