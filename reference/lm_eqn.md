# Taper plot

Plots of the linear regression between diameter and height

## Usage

``` r
lm_eqn(df)
```

## Arguments

- df:

  Data.frame with diameter height information.

## Value

Plot of the linear regression

## Examples

``` r
if (FALSE) { # \dontrun{
taper_plot <- ggplot2::ggplot(T, ggplot2::aes(diameter, height)) +
ggplot2::geom_point() +
ggplot2::geom_smooth(method='lm') +
ggplot2::geom_text(x = (max(T$diameter)+min(T$diameter))/2,
                   y = max(T$height)-0.5, label = lm_eqn(T), parse = TRUE)
} # }
```
