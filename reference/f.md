# Algebraic distance from the center

Calculates the algebraic distance between the data points and the mean
circle centered at c=(xc, yc) based on
[`calc_r`](https://lmterryn.github.io/ITSMe/reference/calc_r.md).

## Usage

``` r
f(c, x, y)
```

## Arguments

- c:

  First estimate of the center coordinates to be optimised (xc,yc).

- x:

  X values of the points.

- y:

  Y values of the points.

## Value

When optimised returns the optimised center estimate of the circle
fitting.

## Details

Support function used to determine the DBH from a tree point cloud with
the functions
[`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md) and
[`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md).

## Examples

``` r
if (FALSE) { # \dontrun{
center_estimate <- optim(par = c(x_m, y_m), fn = f, x = x_dbh, y = y_dbh)
} # }
```
