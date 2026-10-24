# Executes full PrefMaX workflow

Executes full PrefMaX workflow

## Usage

``` r
run_prefmax(
  path,
  file,
  ShortID = NULL,
  sheet_liking = NULL,
  sheet_products = NULL,
  sheet_subjects = NULL,
  scale.unit = FALSE,
  ncp = 2,
  nb.clust = NULL,
  max.clust = 6,
  scale_desc = 1,
  scale_subj = 1,
  label_consumers = FALSE,
  output_dir = getwd(),
  res_dpi = 300,
  save_tiff = TRUE,
  sheet_hedo = NULL,
  sheet_qda = NULL,
  ...
)
```

## Arguments

- path:

  Input directory.

- file:

  Input Excel filename.

- ShortID:

  Short identifier string.

- sheet_liking:

  Sheet with liking data.

- sheet_products:

  Sheet with product data.

- sheet_subjects:

  Sheet with subject data.

- scale.unit:

  Standardize product variables in PCA.

- ncp:

  Dimensions count.

- nb.clust:

  Forced cluster count.

- max.clust:

  Max cluster search.

- scale_desc:

  Descriptor scaling.

- scale_subj:

  Subject variables scaling.

- label_consumers:

  Display consumer labels.

- output_dir:

  Output directory path.

- res_dpi:

  Resolution DPI for TIFF.

- save_tiff:

  Export TIFF files.

- sheet_hedo:

  Legacy alias for sheet_liking.

- sheet_qda:

  Legacy alias for sheet_products.

- ...:

  Additional arguments.

## Value

A list with PrefMaX execution results and plots.
