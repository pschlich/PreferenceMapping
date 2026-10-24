# Performs Unfolding analysis

Performs Unfolding analysis

## Usage

``` r
unfolding_analysis(
  file_path,
  sheet_liking = NULL,
  mode = "internal",
  sheet_products = NULL,
  sheet_subjects = NULL,
  coef_biplot = 1,
  scale_subj = 1,
  min_r2 = 1,
  ridge_x = 0.001,
  ridge_y = 0.001,
  ndim = 2,
  itmax = 10000,
  type_unfold = "ordinal",
  conditionality = "row",
  method_diss = "reverse",
  clustering = TRUE,
  n_clusters = NULL,
  max_k = 6,
  label_consumers = FALSE,
  plot3D = FALSE,
  short_id = NULL,
  out_dir = NULL,
  save_tiff = TRUE,
  dpi = 300,
  ...
)
```

## Arguments

- file_path:

  Path to the input Excel file.

- sheet_liking:

  Name of sheet containing liking data.

- mode:

  Unfolding mode: "internal", "external", "restricted_prod",
  "restricted_subj", or "restricted_both".

- sheet_products:

  Sheet name for product variables.

- sheet_subjects:

  Sheet name for consumer variables.

- coef_biplot:

  Scaling factor for product vectors.

- scale_subj:

  Scaling factor for subject vectors.

- min_r2:

  Minimum R2 threshold for product vectors (default: 1.0).

- ridge_x:

  Ridge regularization parameter for products.

- ridge_y:

  Ridge regularization parameter for subjects.

- ndim:

  Dimensions.

- itmax:

  Maximum iterations (default: 10000).

- type_unfold:

  Unfolding type ("ordinal", "interval", "ratio").

- conditionality:

  Conditionality ("row", "unconditional").

- method_diss:

  Dissimilarity method ("reverse", "rank").

- clustering:

  Logical for consumer clustering.

- n_clusters:

  Forced cluster count.

- max_k:

  Max clusters for Silhouette search.

- label_consumers:

  Display consumer labels on plot.

- plot3D:

  Generate 3D interactive plot.

- short_id:

  Short identifier for files.

- out_dir:

  Output directory.

- save_tiff:

  Export TIFF files.

- dpi:

  DPI resolution.

- ...:

  Additional arguments.

## Value

A list with Unfolding model results and plots.
