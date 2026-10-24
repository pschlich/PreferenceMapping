# Main wrapper for Preference Mapping methods

Unified interface to compute Internal, External, Restricted Unfolding,
and PrefMaX preference mapping analyses across one or multiple
dimensions.

## Usage

``` r
PreferenceMapping(
  file_path,
  method = c("internal", "external", "restricted", "prefmax"),
  sheet_liking = NULL,
  sheet_products = NULL,
  sheet_subjects = NULL,
  ndim = 2,
  max_dim = NULL,
  clustering = TRUE,
  n_clusters = NULL,
  max_k = 6,
  scale.unit = FALSE,
  scale_desc = 1,
  scale_subj = 1,
  min_r2 = 1,
  ridge_x = 0.001,
  ridge_y = 0.001,
  type_unfold = "ordinal",
  conditionality = "row",
  method_diss = "reverse",
  itmax = 10000,
  label_consumers = FALSE,
  plot3D = FALSE,
  short_id = NULL,
  out_dir = NULL,
  save_tiff = TRUE,
  dpi = 300,
  sheet_hedo = NULL,
  sheet_qda = NULL,
  ...
)
```

## Arguments

- file_path:

  Path to the Excel input file (.xlsx or .xls).

- method:

  Vector of methods to compute: "internal", "external", "restricted",
  "restricted_prod", "restricted_subj", "restricted_both", or "prefmax".

- sheet_liking:

  Name of the sheet containing consumer liking data (required).

- sheet_products:

  Name of the sheet containing product variables.

- sheet_subjects:

  Name of the sheet containing consumer variables.

- ndim:

  Starting dimension for analysis (default: 2).

- max_dim:

  Maximum dimension for analysis. If specified, iterates from `ndim` to
  `max_dim`.

- clustering:

  Logical. Performs consumer clustering if TRUE (default: TRUE).

- n_clusters:

  Numeric. Forced number of clusters.

- max_k:

  Maximum number of clusters tested during Silhouette search (default:
  6).

- scale.unit:

  Logical. Standardizes product variables in PrefMaX if TRUE (default:
  FALSE).

- scale_desc:

  Scale factor for product variable vectors on biplots (default: 1).

- scale_subj:

  Scale factor for consumer background variables on biplots (default:
  1).

- min_r2:

  Minimum R2 threshold to display product variables on biplots (default:
  1.0).

- ridge_x:

  Ridge regularization parameter for product constraints in Unfolding
  (default: 1e-3).

- ridge_y:

  Ridge regularization parameter for subject constraints in Unfolding
  (default: 1e-3).

- type_unfold:

  Unfolding transformation type for smacofRect (default: "ordinal").

- conditionality:

  Matrix conditionality for smacofRect (default: "row").

- method_diss:

  Dissimilarity conversion method for sim2diss (default: "reverse").

- itmax:

  Maximum number of iterations for smacofRect algorithm (default:
  10000).

- label_consumers:

  Logical. Displays individual consumer labels on biplots if TRUE
  (default: FALSE).

- plot3D:

  Logical. Generates an interactive 3D plot for Unfolding if TRUE
  (default: FALSE).

- short_id:

  Short identifier string for exported file naming.

- out_dir:

  Output directory path. If NULL, uses input file directory.

- save_tiff:

  Logical. Exports biplot figures as TIFF images if TRUE (default:
  TRUE).

- dpi:

  Image resolution in DPI for saved TIFF files (default: 300).

- sheet_hedo:

  Legacy alias for sheet_liking.

- sheet_qda:

  Legacy alias for sheet_products.

- ...:

  Additional optional arguments.

## Value

An object of class `preference_mapping_multi` containing results and
ggplot objects.
