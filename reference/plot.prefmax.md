# Plot method for prefmax

Plot method for prefmax

## Usage

``` r
# S3 method for class 'prefmax'
plot(
  x,
  axes = c(1, 2),
  scale_desc = 1,
  cluster = NULL,
  centroids = NULL,
  label_consumers = FALSE,
  subj_illu = NULL,
  ...
)
```

## Arguments

- x:

  A `prefmax` object.

- axes:

  Axes pair to plot (default: c(1, 2)).

- scale_desc:

  Vector scaling factor.

- cluster:

  Clustering object.

- centroids:

  Centroids data frame.

- label_consumers:

  Display consumer labels.

- subj_illu:

  Illustrative subject variables.

- ...:

  Additional arguments.

## Value

A ggplot object.
