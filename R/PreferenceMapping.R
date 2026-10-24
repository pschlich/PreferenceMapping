# ==============================================================================
# R PACKAGE: PreferenceMapping
# Unified interface for Unfolding (Internal, External, Restricted) & PrefMaX
# Author: Pascal Schlich
# ==============================================================================

#' @importFrom stats anova coef cor dist kmeans lm prcomp predict var
#' @importFrom readxl read_excel excel_sheets
#' @importFrom smacof smacofRect sim2diss
#' @importFrom cluster silhouette
#' @importFrom ggplot2 ggplot geom_hline geom_vline geom_point geom_path geom_segment aes coord_fixed theme_bw theme element_rect labs ggsave arrow unit
#' @importFrom ggrepel geom_text_repel
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom FactoMineR PCA
#' @importFrom grDevices pdf dev.off
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("DimX", "DimY", "Cluster", "Consumer", "Product", "Label", "Var", "Weight", "x", "y"))
}

#' Helper pour la gestion du repertoire de sortie
#' @param out_dir Output directory path
#' @keywords internal
setup_out_dir <- function(out_dir) {
  if (dir.exists(out_dir)) {
    message(sprintf("--> Output directory '%s' already exists.", out_dir))
  } else {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    message(sprintf("--> Created output directory '%s'.", out_dir))
  }
}

#' Helper unifie pour la projection des variables sujets
#' @param subj_mat Matrix of subject variables
#' @param cons_coords Consumer coordinates data frame
#' @param ndim Number of dimensions
#' @param scale_subj Scale factor for subject variables
#' @keywords internal
compute_subj_illustrative <- function(subj_mat, cons_coords, ndim, scale_subj = 1) {
  if (is.null(subj_mat)) return(list(quanti = NULL, quali = NULL))

  common_cons <- intersect(rownames(subj_mat), cons_coords$Consumer)
  if (length(common_cons) == 0) return(list(quanti = NULL, quali = NULL))

  S_sub <- subj_mat[common_cons, , drop = FALSE]
  C_sub <- cons_coords[match(common_cons, cons_coords$Consumer), paste0("Dim", 1:ndim), drop = FALSE]

  df_quanti <- NULL
  df_quali <- NULL

  for (col in colnames(S_sub)) {
    val <- S_sub[, col]
    num_val <- suppressWarnings(as.numeric(val))
    is_num <- !any(is.na(num_val))

    if (is_num && length(unique(num_val)) > 2) {
      cors <- numeric(ndim)
      for (d in 1:ndim) {
        raw_c <- stats::cor(num_val, C_sub[, d], use = "complete.obs")
        cors[d] <- max(-1, min(1, raw_c))
      }
      row_df <- data.frame(Var = col, as.data.frame(t(cors * scale_subj)), stringsAsFactors = FALSE)
      colnames(row_df)[-1] <- paste0("Dim", 1:ndim)
      df_quanti <- rbind(df_quanti, row_df)
    } else {
      fac_val <- as.factor(val)
      for (lev in levels(fac_val)) {
        if (lev %in% c("0", "FALSE", "false", "F")) next

        idx <- which(fac_val == lev)
        if (length(idx) > 0) {
          bary <- colMeans(C_sub[idx, , drop = FALSE], na.rm = TRUE)
          label <- if (all(levels(fac_val) %in% c("0", "1"))) col else paste0(col, "_", lev)
          row_df <- data.frame(Var = label, as.data.frame(t(bary)), stringsAsFactors = FALSE)
          colnames(row_df)[-1] <- paste0("Dim", 1:ndim)
          df_quali <- rbind(df_quali, row_df)
        }
      }
    }
  }
  return(list(quanti = df_quanti, quali = df_quali))
}

#' Helper pour l'ecriture des metadonnees Subjects dans Data_Verification
#' @param wb Workbook object
#' @param sheet_name Target sheet name
#' @param subj_mat_full Full subject matrix
#' @param start_row Starting row
#' @keywords internal
write_subjects_verification <- function(wb, sheet_name, subj_mat_full, start_row = 1) {
  df_quanti_bounds <- NULL
  df_quali_counts  <- NULL

  for (col in colnames(subj_mat_full)) {
    val <- subj_mat_full[, col]
    num_val <- suppressWarnings(as.numeric(val))
    is_num <- !any(is.na(num_val))

    if (is_num && length(unique(num_val)) > 2) {
      df_quanti_bounds <- rbind(df_quanti_bounds, data.frame(
        Variable = col,
        Min = min(num_val, na.rm = TRUE),
        Max = max(num_val, na.rm = TRUE),
        stringsAsFactors = FALSE
      ))
    } else {
      count_1 <- sum(num_val == 1 | val %in% c("1", "TRUE", "true", "T"), na.rm = TRUE)
      df_quali_counts <- rbind(df_quali_counts, data.frame(
        Variable = col,
        Category = "1",
        Count = count_1,
        stringsAsFactors = FALSE
      ))
    }
  }

  curr_row <- start_row
  if (!is.null(df_quanti_bounds) && nrow(df_quanti_bounds) > 0) {
    openxlsx::writeData(wb, sheet_name, "Domaine de variation des variables quantitatives Sujets (Min / Max) :", startRow = curr_row, startCol = 1)
    openxlsx::writeData(wb, sheet_name, df_quanti_bounds, startRow = curr_row + 1, startCol = 1)
    curr_row <- curr_row + nrow(df_quanti_bounds) + 3
  }

  if (!is.null(df_quali_counts) && nrow(df_quali_counts) > 0) {
    openxlsx::writeData(wb, sheet_name, "Effectifs des variables qualitatives / dummies Sujets (valeur 1) :", startRow = curr_row, startCol = 1)
    openxlsx::writeData(wb, sheet_name, df_quali_counts, startRow = curr_row + 1, startCol = 1)
    curr_row <- curr_row + nrow(df_quali_counts) + 3
  }

  return(curr_row)
}

# ==============================================================================
# 1. MAIN WRAPPER FUNCTION (MULTI-METHOD & MULTI-DIMENSION)
# ==============================================================================

#' Main wrapper for Preference Mapping methods
#'
#' @description
#' Unified interface to compute Internal, External, Restricted Unfolding, and PrefMaX preference mapping analyses across one or multiple dimensions.
#'
#' @param file_path Path to the Excel input file (.xlsx or .xls).
#' @param method Vector of methods to compute: "internal", "external", "restricted", "restricted_prod", "restricted_subj", "restricted_both", or "prefmax".
#' @param sheet_liking Name of the sheet containing consumer liking data (required).
#' @param sheet_products Name of the sheet containing product variables.
#' @param sheet_subjects Name of the sheet containing consumer variables.
#' @param ndim Starting dimension for analysis (default: 2).
#' @param max_dim Maximum dimension for analysis. If specified, iterates from \code{ndim} to \code{max_dim}.
#' @param clustering Logical. Performs consumer clustering if TRUE (default: TRUE).
#' @param n_clusters Numeric. Forced number of clusters.
#' @param max_k Maximum number of clusters tested during Silhouette search (default: 6).
#' @param scale.unit Logical. Standardizes product variables in PrefMaX if TRUE (default: FALSE).
#' @param scale_desc Scale factor for product variable vectors on biplots (default: 1).
#' @param scale_subj Scale factor for consumer background variables on biplots (default: 1).
#' @param min_r2 Minimum R2 threshold to display product variables on biplots (default: 1.0).
#' @param ridge_x Ridge regularization parameter for product constraints in Unfolding (default: 1e-3).
#' @param ridge_y Ridge regularization parameter for subject constraints in Unfolding (default: 1e-3).
#' @param type_unfold Unfolding transformation type for smacofRect (default: "ordinal").
#' @param conditionality Matrix conditionality for smacofRect (default: "row").
#' @param method_diss Dissimilarity conversion method for sim2diss (default: "reverse").
#' @param itmax Maximum number of iterations for smacofRect algorithm (default: 10000).
#' @param label_consumers Logical. Displays individual consumer labels on biplots if TRUE (default: FALSE).
#' @param plot3D Logical. Generates an interactive 3D plot for Unfolding if TRUE (default: FALSE).
#' @param short_id Short identifier string for exported file naming.
#' @param out_dir Output directory path. If NULL, uses input file directory.
#' @param save_tiff Logical. Exports biplot figures as TIFF images if TRUE (default: TRUE).
#' @param dpi Image resolution in DPI for saved TIFF files (default: 300).
#' @param sheet_hedo Legacy alias for sheet_liking.
#' @param sheet_qda Legacy alias for sheet_products.
#' @param ... Additional optional arguments.
#'
#' @return An object of class \code{preference_mapping_multi} containing results and ggplot objects.
#' @export
PreferenceMapping <- function(file_path,
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
                              min_r2 = 1.0,
                              ridge_x = 1e-3,
                              ridge_y = 1e-3,
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
                              ...) {

  if (!is.null(sheet_hedo)) sheet_liking <- sheet_hedo
  if (!is.null(sheet_qda))  sheet_products <- sheet_qda

  if (is.null(sheet_liking)) {
    stop("Error: 'sheet_liking' is a required parameter and must be specified with the name of the sheet containing consumer liking data.")
  }

  valid_methods <- c("internal", "external", "restricted", "restricted_prod", "restricted_subj", "restricted_both", "prefmax")
  selected_methods <- unique(tolower(method))
  selected_methods[selected_methods == "restricted"] <- "restricted_prod"
  selected_methods <- unique(selected_methods)

  invalid <- setdiff(selected_methods, valid_methods)
  if (length(invalid) > 0) {
    warning(sprintf("Unrecognized method(s) ignored: %s", paste(invalid, collapse = ", ")))
    selected_methods <- intersect(selected_methods, valid_methods)
  }

  if (length(selected_methods) == 0) {
    stop("Error: No valid method specified to compute.")
  }

  if (is.null(out_dir)) out_dir <- dirname(file_path)
  setup_out_dir(out_dir)

  if (is.null(short_id) || nchar(trimws(short_id)) == 0) {
    short_id <- substr(sub("\\.(xlsx|xls)$", "", basename(file_path), ignore.case = TRUE), 1, 8)
  }

  target_dims <- if (is.null(max_dim) || max_dim <= ndim) ndim else ndim:max_dim
  results_list <- list()
  all_plots_collected <- list()

  for (m in selected_methods) {
    for (d_val in target_dims) {
      cat("\n=======================================================\n")
      cat(sprintf("   RUNNING PREFERENCE MAPPING - Method: %s (%dD)\n", toupper(m), d_val))
      cat("=======================================================\n\n")

      res_m <- tryCatch({
        if (m %in% c("internal", "external", "restricted_prod", "restricted_subj", "restricted_both")) {
          unfolding_analysis(
            file_path = file_path,
            sheet_liking = sheet_liking,
            mode = m,
            sheet_products = sheet_products,
            sheet_subjects = sheet_subjects,
            ndim = d_val,
            clustering = clustering,
            n_clusters = n_clusters,
            max_k = max_k,
            coef_biplot = scale_desc,
            scale_subj = scale_subj,
            min_r2 = min_r2,
            ridge_x = ridge_x,
            ridge_y = ridge_y,
            type_unfold = type_unfold,
            conditionality = conditionality,
            method_diss = method_diss,
            itmax = itmax,
            label_consumers = label_consumers,
            plot3D = plot3D,
            short_id = short_id,
            out_dir = out_dir,
            save_tiff = save_tiff,
            dpi = dpi,
            ...
          )
        } else {
          run_prefmax(
            path = dirname(file_path),
            file = basename(file_path),
            ShortID = short_id,
            sheet_liking = sheet_liking,
            sheet_products = sheet_products,
            sheet_subjects = sheet_subjects,
            scale.unit = scale.unit,
            ncp = d_val,
            nb.clust = if (isTRUE(clustering)) n_clusters else 1,
            max.clust = max_k,
            scale_desc = scale_desc,
            scale_subj = scale_subj,
            label_consumers = label_consumers,
            output_dir = out_dir,
            res_dpi = dpi,
            save_tiff = save_tiff,
            ...
          )
        }
      }, error = function(e) {
        cat(sprintf("[METHOD SKIPPED] %s\n", e$message))
        return(NULL)
      })

      if (!is.null(res_m)) {
        res_key <- sprintf("%s_%dD", m, d_val)
        results_list[[res_key]] <- res_m
        if (!is.null(res_m$plots) && length(res_m$plots) > 0) {
          all_plots_collected <- c(all_plots_collected, res_m$plots)
        }
      }
    }
  }

  if (length(results_list) == 0) {
    warning("No method could be computed due to missing required sheets.")
  } else if (length(all_plots_collected) > 0) {
    pdf_filename <- file.path(out_dir, sprintf("%s_all_plots.pdf", short_id))
    grDevices::pdf(file = pdf_filename, width = 8.5, height = 6.5)
    for (p_key in names(all_plots_collected)) {
      print(all_plots_collected[[p_key]])
    }
    grDevices::dev.off()
    cat(sprintf("\n[PDF EXPORT] Consolidated multi-method PDF successfully created: %s\n", basename(pdf_filename)))
  }

  class(results_list) <- "preference_mapping_multi"
  return(invisible(results_list))
}

# ==============================================================================
# 2. UNFOLDING MODULE
# ==============================================================================

#' Performs Unfolding analysis
#'
#' @param file_path Path to the input Excel file.
#' @param sheet_liking Name of sheet containing liking data.
#' @param mode Unfolding mode: "internal", "external", "restricted_prod", "restricted_subj", or "restricted_both".
#' @param sheet_products Sheet name for product variables.
#' @param sheet_subjects Sheet name for consumer variables.
#' @param coef_biplot Scaling factor for product vectors.
#' @param scale_subj Scaling factor for subject vectors.
#' @param min_r2 Minimum R2 threshold for product vectors (default: 1.0).
#' @param ridge_x Ridge regularization parameter for products.
#' @param ridge_y Ridge regularization parameter for subjects.
#' @param ndim Dimensions.
#' @param itmax Maximum iterations (default: 10000).
#' @param type_unfold Unfolding type ("ordinal", "interval", "ratio").
#' @param conditionality Conditionality ("row", "unconditional").
#' @param method_diss Dissimilarity method ("reverse", "rank").
#' @param clustering Logical for consumer clustering.
#' @param n_clusters Forced cluster count.
#' @param max_k Max clusters for Silhouette search.
#' @param label_consumers Display consumer labels on plot.
#' @param plot3D Generate 3D interactive plot.
#' @param short_id Short identifier for files.
#' @param out_dir Output directory.
#' @param save_tiff Export TIFF files.
#' @param dpi DPI resolution.
#' @param ... Additional arguments.
#'
#' @return A list with Unfolding model results and plots.
#' @export
unfolding_analysis <- function(file_path, sheet_liking = NULL, mode = "internal",
                               sheet_products = NULL, sheet_subjects = NULL,
                               coef_biplot = 1, scale_subj = 1, min_r2 = 1.0,
                               ridge_x = 1e-3, ridge_y = 1e-3, ndim = 2, itmax = 10000,
                               type_unfold = "ordinal", conditionality = "row",
                               method_diss = "reverse", clustering = TRUE, n_clusters = NULL,
                               max_k = 6, label_consumers = FALSE, plot3D = FALSE,
                               short_id = NULL, out_dir = NULL, save_tiff = TRUE, dpi = 300, ...) {

  if (is.null(sheet_liking)) {
    stop("'sheet_liking' is a required parameter and must be specified with the name of the sheet containing consumer liking data.")
  }

  mode_clean <- match.arg(tolower(mode), c("internal", "external", "restricted", "restricted_prod", "restricted_subj", "restricted_both"))
  if (mode_clean == "restricted") mode_clean <- "restricted_prod"

  if (!file.exists(file_path)) stop(sprintf("Input file not found: %s", file_path))

  if (is.null(out_dir)) out_dir <- dirname(file_path)
  setup_out_dir(out_dir)

  avail_sheets <- readxl::excel_sheets(file_path)

  if (!sheet_liking %in% avail_sheets) {
    stop(sprintf("Liking sheet '%s' not found in file '%s'.", sheet_liking, basename(file_path)))
  }

  has_products <- !is.null(sheet_products) && (sheet_products %in% avail_sheets)
  has_subjects <- !is.null(sheet_subjects) && (sheet_subjects %in% avail_sheets)

  if (mode_clean == "external" && !has_products) {
    stop("For this method, sheet_products must be specified with the name of the sheet in the input data file containing the product variables.")
  }

  if (mode_clean == "restricted_prod" && !has_products) {
    stop("For this method, sheet_products must be specified with the name of the sheet in the input data file containing the product variables.")
  }

  if (mode_clean == "restricted_subj" && !has_subjects) {
    stop("For this method, sheet_subjects must be specified with the name of the sheet in the input data file containing the consumer variables.")
  }

  if (mode_clean == "restricted_both" && (!has_products || !has_subjects)) {
    stop("For this method, both sheet_products and sheet_subjects must be specified with the names of the sheets in the input data file.")
  }

  df_raw <- readxl::read_excel(file_path, sheet = sheet_liking)
  data_mat <- as.matrix(df_raw[, -1])
  rownames(data_mat) <- df_raw[[1]]

  n_prod <- nrow(data_mat); n_cons <- ncol(data_mat)
  prefix <- if (!is.null(short_id) && nchar(trimws(short_id)) > 0) paste0(trimws(short_id), "_") else ""

  qda_mat_full <- NULL
  if (has_products) {
    df_qda_raw <- readxl::read_excel(file_path, sheet = sheet_products)
    qda_mat_full <- as.matrix(df_qda_raw[, -1])
    rownames(qda_mat_full) <- df_qda_raw[[1]]
  }

  subj_mat_full <- NULL
  if (has_subjects) {
    df_subj_raw <- readxl::read_excel(file_path, sheet = sheet_subjects)
    subj_mat_full <- as.matrix(df_subj_raw[, -1])
    rownames(subj_mat_full) <- df_subj_raw[[1]]
  }

  cat(sprintf("[DATA INFO] File: %s\n", basename(file_path)))
  cat(sprintf("            Liking Sheet   : %d Products (N) x %d Consumers (P)\n", n_prod, n_cons))
  if (has_products) cat(sprintf("            Products Sheet : %d Products (N) x %d Attributes (Q)\n", nrow(qda_mat_full), ncol(qda_mat_full)))
  if (has_subjects) cat(sprintf("            Subjects Sheet : %d Consumers (P) x %d Variables\n", nrow(subj_mat_full), ncol(subj_mat_full)))

  t_data_diss <- t(smacof::sim2diss(data_mat, method = method_diss))
  df_vars <- NULL
  df_subj_active_loadings <- NULL

  if (mode_clean == "external") {
    qda_mat <- qda_mat_full[colnames(t_data_diss), , drop = FALSE]
    pca_res <- stats::prcomp(qda_mat, center = TRUE, scale. = FALSE)
    pca_coords <- pca_res$x[, 1:ndim, drop = FALSE]

    res_unfold <- smacof::smacofRect(t_data_diss, type = type_unfold, conditionality = conditionality,
                                     ndim = ndim, fixed = "col", fixed.coord = pca_coords, itmax = itmax)

    # Calcul des loadings de l'ACP pour les représenter comme sur un biplot d'ACP
    loadings <- pca_res$rotation[, 1:ndim, drop = FALSE] %*% diag(pca_res$sdev[1:ndim])
    scale_auto <- max(abs(res_unfold$conf.col)) / max(abs(loadings)) * 0.8

    df_vars <- data.frame(
      Var = colnames(qda_mat),
      as.data.frame(loadings * scale_auto * coef_biplot),
      stringsAsFactors = FALSE
    )
    colnames(df_vars)[-1] <- paste0("Dim", 1:ndim)

  } else if (startsWith(mode_clean, "restricted")) {
    init_fit <- smacof::smacofRect(t_data_diss, type = type_unfold, conditionality = conditionality, ndim = ndim, itmax = itmax)
    X_prod <- init_fit$conf.col
    Y_cons <- init_fit$conf.row
    final_stress <- init_fit$stress
    final_niter  <- init_fit$niter

    Z_prod <- NULL
    if (has_products && mode_clean %in% c("restricted_prod", "restricted_both")) {
      Z_prod <- scale(qda_mat_full[colnames(t_data_diss), , drop = FALSE])
    }

    W_subj <- NULL
    if (has_subjects && mode_clean %in% c("restricted_subj", "restricted_both")) {
      Y_raw <- subj_mat_full[rownames(t_data_diss), , drop = FALSE]
      W_list <- lapply(1:ncol(Y_raw), function(c_idx) {
        suppressWarnings(as.numeric(as.character(Y_raw[, c_idx])))
      })
      W_mat <- do.call(cbind, W_list)
      rownames(W_mat) <- rownames(Y_raw)
      colnames(W_mat) <- colnames(Y_raw)
      valid_cols <- apply(W_mat, 2, function(col) !any(is.na(col)) && stats::var(col) > 0)
      if (sum(valid_cols) == 0) {
        stop("No valid numerical or dummy subject variables found for restricted subject unfolding.")
      }
      W_subj <- scale(W_mat[, valid_cols, drop = FALSE])
    }

    max_als_iter <- 20
    tol <- 1e-4
    prev_stress <- final_stress

    for (als in 1:max_als_iter) {
      if (!is.null(Z_prod) && is.null(W_subj)) {
        XtX <- t(Z_prod) %*% Z_prod
        B_prod <- solve(XtX + ridge_x * diag(ncol(Z_prod)), t(Z_prod) %*% X_prod)
        res_tmp <- smacof::smacofRect(t_data_diss, type = type_unfold, conditionality = conditionality,
                                      ndim = ndim, fixed = "col", fixed.coord = Z_prod %*% B_prod, itmax = itmax)
        X_prod <- res_tmp$conf.col
        Y_cons <- res_tmp$conf.row
        final_stress <- res_tmp$stress
        final_niter  <- res_tmp$niter
      } else if (is.null(Z_prod) && !is.null(W_subj)) {
        WtW <- t(W_subj) %*% W_subj
        C_subj <- solve(WtW + ridge_y * diag(ncol(W_subj)), t(W_subj) %*% Y_cons)
        res_tmp <- smacof::smacofRect(t_data_diss, type = type_unfold, conditionality = conditionality,
                                      ndim = ndim, fixed = "row", fixed.coord = W_subj %*% C_subj, itmax = itmax)
        Y_cons <- res_tmp$conf.row
        X_prod <- res_tmp$conf.col
        final_stress <- res_tmp$stress
        final_niter  <- res_tmp$niter
      } else if (!is.null(Z_prod) && !is.null(W_subj)) {
        XtX <- t(Z_prod) %*% Z_prod
        B_prod <- solve(XtX + ridge_x * diag(ncol(Z_prod)), t(Z_prod) %*% X_prod)
        res_tmp1 <- smacof::smacofRect(t_data_diss, type = type_unfold, conditionality = conditionality,
                                       ndim = ndim, fixed = "col", fixed.coord = Z_prod %*% B_prod, itmax = itmax)
        X_prod <- res_tmp1$conf.col
        Y_cons <- res_tmp1$conf.row

        WtW <- t(W_subj) %*% W_subj
        C_subj <- solve(WtW + ridge_y * diag(ncol(W_subj)), t(W_subj) %*% Y_cons)
        res_tmp2 <- smacof::smacofRect(t_data_diss, type = type_unfold, conditionality = conditionality,
                                       ndim = ndim, fixed = "row", fixed.coord = W_subj %*% C_subj, itmax = itmax)
        Y_cons <- res_tmp2$conf.row
        X_prod <- res_tmp2$conf.col
        final_stress <- res_tmp2$stress
        final_niter  <- res_tmp2$niter
      }

      if (abs(prev_stress - final_stress) < tol) break
      prev_stress <- final_stress
    }

    if (!is.null(Z_prod) && mode_clean %in% c("restricted_prod", "restricted_both")) {
      XtX <- t(Z_prod) %*% Z_prod
      B_prod <- solve(XtX + ridge_x * diag(ncol(Z_prod)), t(Z_prod) %*% X_prod)
      scale_auto <- max(abs(X_prod)) / max(abs(B_prod)) * 0.8
      df_vars <- data.frame(Var = colnames(Z_prod), as.data.frame(B_prod * scale_auto * coef_biplot), stringsAsFactors = FALSE)
      colnames(df_vars)[-1] <- paste0("Dim", 1:ndim)
    }

    if (!is.null(W_subj) && mode_clean %in% c("restricted_subj", "restricted_both")) {
      WtW <- t(W_subj) %*% W_subj
      C_subj <- solve(WtW + ridge_y * diag(ncol(W_subj)), t(W_subj) %*% Y_cons)
      df_subj_active_loadings <- data.frame(
        Var = colnames(W_subj),
        as.data.frame(C_subj * scale_subj),
        stringsAsFactors = FALSE
      )
      colnames(df_subj_active_loadings)[-1] <- paste0("Dim", 1:ndim)
    }

    if (mode_clean == "restricted_subj" && has_products) {
      qda_sub <- qda_mat_full[colnames(t_data_diss), , drop = FALSE]
      n_desc <- ncol(qda_sub)
      r2_vec <- numeric(n_desc)
      cor_mat <- matrix(0, nrow = n_desc, ncol = ndim)
      rownames(cor_mat) <- colnames(qda_sub)
      colnames(cor_mat) <- paste0("Dim", 1:ndim)

      for (q in 1:n_desc) {
        y_q <- qda_sub[, q]
        fit <- stats::lm(y_q ~ X_prod)
        r2_vec[q] <- summary(fit)$r.squared
        for (d in 1:ndim) {
          cor_mat[q, d] <- stats::cor(y_q, X_prod[, d], use = "complete.obs")
        }
      }

      for (q in 1:n_desc) {
        v_coords <- cor_mat[q, 1:ndim]
        norm_v <- sqrt(sum(v_coords^2))
        max_allowed <- coef_biplot
        if (norm_v > max_allowed && norm_v > 0) {
          cor_mat[q, 1:ndim] <- v_coords * (max_allowed / norm_v)
        }
      }

      df_vars <- data.frame(
        Var = colnames(qda_sub),
        R2 = round(r2_vec, 4),
        as.data.frame(cor_mat),
        stringsAsFactors = FALSE
      )
      colnames(df_vars)[3:(2 + ndim)] <- paste0("Dim", 1:ndim)
    }

    res_unfold <- list(stress = final_stress, niter = final_niter, conf.row = Y_cons, conf.col = X_prod)

  } else {
    res_unfold <- smacof::smacofRect(t_data_diss, type = type_unfold, conditionality = conditionality, ndim = ndim, itmax = itmax)

    if (has_products && mode_clean == "internal") {
      qda_sub <- qda_mat_full[colnames(t_data_diss), , drop = FALSE]
      P_coords <- res_unfold$conf.col
      n_desc <- ncol(qda_sub)
      r2_vec <- numeric(n_desc)
      cor_mat <- matrix(0, nrow = n_desc, ncol = ndim)
      rownames(cor_mat) <- colnames(qda_sub)
      colnames(cor_mat) <- paste0("Dim", 1:ndim)

      for (q in 1:n_desc) {
        y_q <- qda_sub[, q]
        fit <- stats::lm(y_q ~ P_coords)
        r2_vec[q] <- summary(fit)$r.squared
        for (d in 1:ndim) {
          cor_mat[q, d] <- stats::cor(y_q, P_coords[, d], use = "complete.obs")
        }
      }

      for (q in 1:n_desc) {
        v_coords <- cor_mat[q, 1:ndim]
        norm_v <- sqrt(sum(v_coords^2))
        max_allowed <- coef_biplot
        if (norm_v > max_allowed && norm_v > 0) {
          cor_mat[q, 1:ndim] <- v_coords * (max_allowed / norm_v)
        }
      }

      df_vars <- data.frame(
        Var = colnames(qda_sub),
        R2 = round(r2_vec, 4),
        as.data.frame(cor_mat),
        stringsAsFactors = FALSE
      )
      colnames(df_vars)[3:(2 + ndim)] <- paste0("Dim", 1:ndim)
    }
  }

  cat(sprintf("[UNFOLDING RESULT] Final Stress-1 (%s %dD): %.4f (%.2f %%), Iterations: %d\n",
              toupper(mode_clean), ndim, res_unfold$stress, res_unfold$stress * 100, res_unfold$niter))

  prod_coords <- data.frame(Product = colnames(t_data_diss), as.data.frame(res_unfold$conf.col))
  colnames(prod_coords)[-1] <- paste0("Dim", 1:ndim)

  cons_coords <- data.frame(Consumer = rownames(t_data_diss), as.data.frame(res_unfold$conf.row))
  colnames(cons_coords)[-1] <- paste0("Dim", 1:ndim)

  subj_illu <- if (has_subjects) compute_subj_illustrative(subj_mat_full, cons_coords, ndim, scale_subj) else list(quanti = NULL, quali = NULL)

  actual_k <- 1
  df_weighted_centroids <- NULL
  dist_prod_cluster <- NULL
  df_weighted_liking_clusters <- NULL
  df_cluster_sensory_profiles <- NULL
  silhouette_scores <- NULL

  if (clustering) {
    X_cons <- cons_coords[, paste0("Dim", 1:ndim)]
    if (is.null(n_clusters)) {
      max_k_val <- min(max_k, n_cons - 1)
      sil_scores <- numeric(max_k_val - 1)
      cat("\n[SILHOUETTE SEARCH] Average Silhouette coefficients (Unfolding):\n")
      for (k in 2:max_k_val) {
        set.seed(123)
        km_tmp <- stats::kmeans(X_cons, centers = k, nstart = 25)
        ss <- cluster::silhouette(km_tmp$cluster, stats::dist(X_cons))
        sil_scores[k - 1] <- mean(ss[, 3])
        cat(sprintf("   - k = %d clusters: Mean Silhouette = %.4f\n", k, sil_scores[k - 1]))
      }
      actual_k <- which.max(sil_scores) + 1
      cat(sprintf("[SILHOUETTE SELECTION] Optimal number of clusters selected by Silhouette criterion: k = %d\n", actual_k))

      silhouette_scores <- data.frame(
        Clusters = as.character(2:max_k_val),
        Average_Silhouette = round(sil_scores, 4),
        Selected = ifelse(2:max_k_val == actual_k, "YES", ""),
        stringsAsFactors = FALSE
      )
      footer_row <- data.frame(
        Clusters = sprintf("Clustering performed on %d dimensions with %d clusters.", ndim, actual_k),
        Average_Silhouette = "",
        Selected = "",
        stringsAsFactors = FALSE
      )
      silhouette_scores <- rbind(silhouette_scores, footer_row)

    } else {
      actual_k <- n_clusters
      if (actual_k > 1) {
        set.seed(123)
        km_tmp <- stats::kmeans(X_cons, centers = actual_k, nstart = 25)
        ss <- cluster::silhouette(km_tmp$cluster, stats::dist(X_cons))

        silhouette_scores <- data.frame(
          Clusters = as.character(actual_k),
          Average_Silhouette = round(mean(ss[, 3]), 4),
          Selected = "YES (User Defined)",
          stringsAsFactors = FALSE
        )
      } else {
        silhouette_scores <- data.frame(
          Clusters = "1",
          Average_Silhouette = "N/A",
          Selected = "YES (User Defined)",
          stringsAsFactors = FALSE
        )
      }
      footer_row <- data.frame(
        Clusters = sprintf("Clustering performed on %d dimensions with %d clusters.", ndim, actual_k),
        Average_Silhouette = "",
        Selected = "",
        stringsAsFactors = FALSE
      )
      silhouette_scores <- rbind(silhouette_scores, footer_row)
    }

    set.seed(123)
    km_res <- stats::kmeans(X_cons, centers = actual_k, nstart = 25)
    sil_indiv <- if (actual_k > 1) cluster::silhouette(km_res$cluster, stats::dist(X_cons))[, 3] else rep(1, nrow(X_cons))

    counts <- table(km_res$cluster)
    cluster_labels_legend <- paste0("Cluster_", 1:actual_k, " (n=", counts[as.character(1:actual_k)], ")")
    cluster_labels_clean <- paste0("Cluster_", 1:actual_k)

    cons_coords$Cluster_Legend <- factor(cluster_labels_legend[km_res$cluster], levels = cluster_labels_legend)
    cons_coords$Cluster <- factor(cluster_labels_clean[km_res$cluster], levels = cluster_labels_clean)
    cons_coords$Silhouette <- round(sil_indiv, 4)
    cons_coords$Weight_Sil <- pmax(0, cons_coords$Silhouette)

    c_centroids_list <- list()
    for (k in 1:actual_k) {
      sub_c <- cons_coords[km_res$cluster == k, ]
      w_k <- sub_c$Weight_Sil
      if (sum(w_k) == 0) w_k <- rep(1, nrow(sub_c))
      c_centroids_list[[k]] <- colSums(sub_c[, paste0("Dim", 1:ndim), drop = FALSE] * w_k) / sum(w_k)
    }
    c_centroids <- do.call(rbind, c_centroids_list)

    df_weighted_centroids <- data.frame(
      Cluster = paste0("Cluster_", 1:actual_k),
      Cluster_Legend = factor(cluster_labels_legend, levels = cluster_labels_legend),
      as.data.frame(round(c_centroids, 4)), check.names = FALSE
    )

    P_mat <- as.matrix(prod_coords[, paste0("Dim", 1:ndim)])
    dist_mat <- matrix(0, nrow = n_prod, ncol = actual_k)
    rownames(dist_mat) <- prod_coords$Product
    colnames(dist_mat) <- paste0("Cluster_", 1:actual_k)

    for (i in 1:n_prod) {
      for (j in 1:actual_k) dist_mat[i, j] <- round(sqrt(sum((P_mat[i, ] - c_centroids[j, ])^2)), 4)
    }
    dist_prod_cluster <- data.frame(Product = rownames(dist_mat), as.data.frame(dist_mat), check.names = FALSE)

    liking_mat <- matrix(0, nrow = n_prod, ncol = actual_k)
    rownames(liking_mat) <- prod_coords$Product
    colnames(liking_mat) <- paste0("Cluster_", 1:actual_k)

    for (j in 1:actual_k) {
      sub_c <- cons_coords[km_res$cluster == j, ]
      w_k <- sub_c$Weight_Sil
      if (sum(w_k) == 0) w_k <- rep(1, nrow(sub_c))
      cons_ids <- as.character(sub_c$Consumer)
      valid_ids <- intersect(cons_ids, colnames(data_mat))
      if (length(valid_ids) > 0) {
        l_sub <- data_mat[prod_coords$Product, valid_ids, drop = FALSE]
        w_sub <- w_k[match(valid_ids, cons_ids)]
        for (i in 1:n_prod) liking_mat[i, j] <- round(sum(l_sub[i, ] * w_sub) / sum(w_sub), 4)
      }
    }
    df_weighted_liking_clusters <- data.frame(Product = prod_coords$Product, as.data.frame(liking_mat), check.names = FALSE)

    if (has_products) {
      qda_sub <- qda_mat_full[prod_coords$Product, , drop = FALSE]
      n_desc <- ncol(qda_sub)
      prof_mat <- matrix(0, nrow = n_desc, ncol = actual_k)
      rownames(prof_mat) <- colnames(qda_sub)
      colnames(prof_mat) <- paste0("Cluster_", 1:actual_k)

      for (m in 1:n_desc) {
        desc_vals <- qda_sub[, m]
        for (j in 1:actual_k) {
          d_j <- sqrt(colSums((t(P_mat) - c_centroids[j, ])^2))
          d_j <- pmax(d_j, 1e-6)
          w_inv_d <- (1 / d_j) / sum(1 / d_j)
          prof_mat[m, j] <- round(sum(desc_vals * w_inv_d), 4)
        }
      }
      df_cluster_sensory_profiles <- data.frame(Descriptor = colnames(qda_sub), as.data.frame(prof_mat), check.names = FALSE)
    }
  } else {
    cons_coords$Cluster <- factor("All")
    cons_coords$Cluster_Legend <- factor("All")
  }

  mode_prefix <- paste0(toupper(mode_clean), "_")
  trans_str <- if (tolower(type_unfold) == "ordinal") "non-metric" else "metric"
  wb <- openxlsx::createWorkbook()

  df_data_verif <- data.frame(
    Parametre = c("ShortID", "Repertoire (PATH)", "Fichier source", "Methode", "Type Unfold", "Conditionnalite", "Methode Dissimilarite", "Feuille Liking", "Feuille Products", "Feuille Subjects", "Nombre de Produits (N)", "Nombre de Consommateurs (P)", "Nombre de Descripteurs (Q)", "Dimensions (ndim)"),
    Valeur = c(short_id, dirname(file_path), basename(file_path), sprintf("%s Unfolding (%s)", toupper(mode_clean), trans_str), type_unfold, conditionality, method_diss, sheet_liking, ifelse(has_products, sheet_products, "N/A"), ifelse(has_subjects, sheet_subjects, "N/A"), n_prod, n_cons, ifelse(has_products, ncol(qda_mat_full), 0), ndim),
    stringsAsFactors = FALSE
  )
  openxlsx::addWorksheet(wb, "Data_Verification")
  openxlsx::writeData(wb, "Data_Verification", df_data_verif, startRow = 1, startCol = 1)

  curr_verif_row <- 16
  if (has_products) {
    bounds_qda <- data.frame(
      Min = apply(qda_mat_full, 2, min, na.rm = TRUE),
      Max = apply(qda_mat_full, 2, max, na.rm = TRUE),
      row.names = colnames(qda_mat_full)
    )
    openxlsx::writeData(wb, "Data_Verification", "Domaine de variation des descripteurs Produits (Min / Max) :", startRow = curr_verif_row, startCol = 1)
    openxlsx::writeData(wb, "Data_Verification", bounds_qda, startRow = curr_verif_row + 1, startCol = 1, rowNames = TRUE)
    curr_verif_row <- curr_verif_row + nrow(bounds_qda) + 3
  }

  if (has_subjects) {
    curr_verif_row <- write_subjects_verification(wb, "Data_Verification", subj_mat_full, start_row = curr_verif_row)
  }

  df_stress_summary <- data.frame(
    Analysis_Type = sprintf("%sUnfolding_%s", mode_prefix, trans_str),
    Dimension = ndim,
    Stress_1 = res_unfold$stress,
    Stress_Percent = round(res_unfold$stress * 100, 2)
  )
  openxlsx::addWorksheet(wb, "Stress_Summary"); openxlsx::writeData(wb, "Stress_Summary", df_stress_summary)

  openxlsx::addWorksheet(wb, "Coordinates_Products"); openxlsx::writeData(wb, "Coordinates_Products", prod_coords)

  if (clustering && !is.null(silhouette_scores)) {
    openxlsx::addWorksheet(wb, "Silhouette_Scores"); openxlsx::writeData(wb, "Silhouette_Scores", silhouette_scores)
  }

  cons_export <- cons_coords
  cons_export$Cluster_Legend <- NULL
  cons_export$Weight_Sil <- NULL
  cols_order <- c("Consumer", "Cluster", "Silhouette", paste0("Dim", 1:ndim))
  cons_export <- cons_export[, intersect(cols_order, colnames(cons_export))]
  openxlsx::addWorksheet(wb, "Consumers"); openxlsx::writeData(wb, "Consumers", cons_export)

  if (!is.null(df_vars)) {
    prod_sheet_name <- if (mode_clean %in% c("external", "restricted_prod", "restricted_both")) "Loadings_Var_Products" else "Corr_Var_Products"
    openxlsx::addWorksheet(wb, prod_sheet_name); openxlsx::writeData(wb, prod_sheet_name, df_vars)
  }

  if (has_subjects) {
    if (mode_clean %in% c("restricted_subj", "restricted_both")) {
      if (!is.null(df_subj_active_loadings)) {
        openxlsx::addWorksheet(wb, "Loadings_Var_Subjects"); openxlsx::writeData(wb, "Loadings_Var_Subjects", df_subj_active_loadings)
      }
    } else {
      df_subj_corr <- rbind(subj_illu$quanti, subj_illu$quali)
      if (!is.null(df_subj_corr) && nrow(df_subj_corr) > 0) {
        openxlsx::addWorksheet(wb, "Corr_Var_Subjects"); openxlsx::writeData(wb, "Corr_Var_Subjects", df_subj_corr)
      }
    }
  }

  if (clustering && !is.null(df_weighted_centroids)) {
    df_cent_export <- df_weighted_centroids
    df_cent_export$Cluster_Legend <- NULL
    openxlsx::addWorksheet(wb, "Clusters_Centroids"); openxlsx::writeData(wb, "Clusters_Centroids", df_cent_export)
  }

  if (clustering && !is.null(dist_prod_cluster)) {
    openxlsx::addWorksheet(wb, "Distances_Products_Clusters"); openxlsx::writeData(wb, "Distances_Products_Clusters", dist_prod_cluster)
  }

  if (clustering && !is.null(df_weighted_liking_clusters)) {
    openxlsx::addWorksheet(wb, "Cluster_Products_Liking"); openxlsx::writeData(wb, "Cluster_Products_Liking", df_weighted_liking_clusters)
  }

  if (clustering && !is.null(df_cluster_sensory_profiles)) {
    openxlsx::addWorksheet(wb, "Cluster_Sensory_Profiles"); openxlsx::writeData(wb, "Cluster_Sensory_Profiles", df_cluster_sensory_profiles)
  }

  clus_suffix <- if (clustering && actual_k > 1) paste0("_Clus", actual_k) else ""
  excel_filename <- sprintf("%s%sUnfolding_%s_Results_%dD%s.xlsx", prefix, mode_prefix, trans_str, ndim, clus_suffix)
  openxlsx::saveWorkbook(wb, file = file.path(out_dir, excel_filename), overwrite = TRUE)

  planes_to_plot <- if (ndim >= 2) lapply(1:(ndim - 1), function(i) c(i, i + 1)) else list(c(1, 1))
  plot_list <- list()

  has_passive_qda <- !is.null(df_vars) && mode_clean == "internal"
  draw_circle <- has_passive_qda && min_r2 < 1.0
  radius_circle <- if (has_passive_qda) coef_biplot else scale_subj

  circle_df <- NULL
  if (draw_circle) {
    theta <- seq(0, 2 * pi, length.out = 200)
    circle_df <- data.frame(x = radius_circle * cos(theta), y = radius_circle * sin(theta))
  }

  for (pair in planes_to_plot) {
    ax1 <- pair[1]; ax2 <- pair[2]
    col_x <- paste0("Dim", ax1); col_y <- paste0("Dim", ax2)

    df_p <- prod_coords[, c("Product", col_x, col_y)]; colnames(df_p) <- c("Product", "DimX", "DimY")
    df_c <- cons_coords[, c("Consumer", "Cluster_Legend", col_x, col_y)]; colnames(df_c) <- c("Consumer", "Cluster", "DimX", "DimY")

    df_v_filt <- df_vars
    n_total_vars <- if (!is.null(df_vars)) nrow(df_vars) else 0
    if (!is.null(df_v_filt) && "R2" %in% colnames(df_v_filt) && !is.null(min_r2) && mode_clean == "internal" && min_r2 < 1.0) {
      df_v_filt <- df_v_filt[df_v_filt$R2 >= min_r2, , drop = FALSE]
      n_filtered_vars <- nrow(df_v_filt)
      if (min_r2 > 0) {
        cat(sprintf("[FILTER INFO] Displaying %d out of %s product variables with R2 >= %.2f\n", n_filtered_vars, n_total_vars, min_r2))
      }
    }

    x_vals <- c(df_p$DimX, df_c$DimX)
    y_vals <- c(df_p$DimY, df_c$DimY)
    if (!is.null(circle_df)) { x_vals <- c(x_vals, circle_df$x); y_vals <- c(y_vals, circle_df$y) }

    df_cent <- NULL
    if (clustering && !is.null(df_weighted_centroids)) {
      df_cent <- data.frame(
        Label = paste0("C", 1:actual_k),
        Cluster = df_weighted_centroids$Cluster_Legend,
        DimX = df_weighted_centroids[[col_x]],
        DimY = df_weighted_centroids[[col_y]],
        stringsAsFactors = FALSE
      )
      x_vals <- c(x_vals, df_cent$DimX)
      y_vals <- c(y_vals, df_cent$DimY)
    }

    p <- ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray60")

    if (!is.null(circle_df)) {
      p <- p + ggplot2::geom_path(data = circle_df, ggplot2::aes(x = x, y = y), linetype = "dotted", color = "steelblue", linewidth = 0.8)
    }

    p <- p +
      ggplot2::geom_point(data = df_c, ggplot2::aes(x = DimX, y = DimY, color = Cluster), size = 2.2, alpha = 0.8)

    if (label_consumers) {
      p <- p + ggrepel::geom_text_repel(data = df_c, ggplot2::aes(x = DimX, y = DimY, label = Consumer, color = Cluster), size = 2.8, max.overlaps = Inf)
    }

    p <- p +
      ggplot2::geom_point(data = df_p, ggplot2::aes(x = DimX, y = DimY), color = "firebrick", shape = 17, size = 4) +
      ggrepel::geom_text_repel(data = df_p, ggplot2::aes(x = DimX, y = DimY, label = Product), color = "firebrick", fontface = "bold", max.overlaps = Inf)

    if (!is.null(df_cent)) {
      p <- p +
        ggplot2::geom_point(data = df_cent, ggplot2::aes(x = DimX, y = DimY, color = Cluster), shape = 15, size = 3.5) +
        ggrepel::geom_text_repel(data = df_cent, ggplot2::aes(x = DimX, y = DimY, label = Label, color = Cluster), fontface = "bold", size = 4.5, max.overlaps = Inf)
    }

    if (!is.null(df_v_filt) && nrow(df_v_filt) > 0) {
      df_v_plane <- df_v_filt[, c("Var", col_x, col_y)]; colnames(df_v_plane) <- c("Var", "DimX", "DimY")
      p <- p +
        ggplot2::geom_segment(data = df_v_plane, ggplot2::aes(x = 0, y = 0, xend = DimX, yend = DimY), arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), color = "steelblue") +
        ggrepel::geom_text_repel(data = df_v_plane, ggplot2::aes(x = DimX, y = DimY, label = Var), color = "steelblue", fontface = "bold", max.overlaps = Inf)
    }

    if (!is.null(subj_illu$quanti)) {
      df_sq <- subj_illu$quanti[, c("Var", col_x, col_y)]; colnames(df_sq) <- c("Var", "DimX", "DimY")
      p <- p +
        ggplot2::geom_segment(data = df_sq, ggplot2::aes(x = 0, y = 0, xend = DimX, yend = DimY), arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), color = "darkgreen", linetype = "solid") +
        ggrepel::geom_text_repel(data = df_sq, ggplot2::aes(x = DimX, y = DimY, label = Var), color = "darkgreen", fontface = "bold.italic", max.overlaps = Inf)
    }

    if (!is.null(subj_illu$quali)) {
      df_skl <- subj_illu$quali[, c("Var", col_x, col_y)]; colnames(df_skl) <- c("Var", "DimX", "DimY")
      p <- p +
        ggplot2::geom_point(data = df_skl, ggplot2::aes(x = DimX, y = DimY), color = "darkgreen", shape = 18, size = 3.5) +
        ggrepel::geom_text_repel(data = df_skl, ggplot2::aes(x = DimX, y = DimY, label = Var), color = "darkgreen", fontface = "bold.italic", max.overlaps = Inf)
    }

    stress_pct <- res_unfold$stress * 100

    method_display_name <- switch(
      mode_clean,
      "internal"        = "Internal",
      "external"        = "External",
      "restricted_prod" = "Product Restricted",
      "restricted_subj" = "Subject Restricted",
      "restricted_both" = "Product & Subject restricted",
      tools::toTitleCase(mode_clean)
    )

    plot_title <- sprintf("%s %s Unfolding (%dD - Plane %d-%d - Stress-1 = %.2f %%)",
                          method_display_name, trans_str, ndim, ax1, ax2, stress_pct)

    plot_subtitle <- switch(
      mode_clean,
      "restricted_prod" = "Dimensions are constrained to be linear combinations of product variables",
      "restricted_subj" = "Dimensions are constrained to be linear combinations of product variables",
      "restricted_both" = "Dimensions are constrained to be linear combinations of product variables",
      "internal"        = if (is.null(min_r2) || min_r2 == 1.0) "Attributes represented by their correlations with dimensions" else sprintf("Attributes represented if their correlations are large enough (min_r2=%.2f).", min_r2),
      "external"        = "Product configuration is constrained to be from PCA of product variables",
      ""
    )

    p <- p + ggplot2::coord_fixed(ratio = 1) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "bottom"
      ) +
      ggplot2::labs(title = plot_title, subtitle = plot_subtitle, x = paste("Dimension", ax1), y = paste("Dimension", ax2), color = "Consumer Clusters")

    plot_key <- sprintf("%s%sUnfolding_%s_%dD_Plane_%d-%d%s", prefix, mode_prefix, trans_str, ndim, ax1, ax2, clus_suffix)
    plot_list[[plot_key]] <- p

    if (interactive()) {
      print(p)
    }

    if (save_tiff) {
      ggplot2::ggsave(filename = file.path(out_dir, paste0(plot_key, ".tif")), plot = p,
                      device = "tiff", width = 8.5, height = 6.5, dpi = dpi, compression = "lzw")
    }
  }

  return(invisible(list(mode = mode_clean, model = res_unfold, prod_coords = prod_coords, cons_coords = cons_coords, plots = plot_list)))
}

# ==============================================================================
# 3. OFFICIAL PREFMAX MODULE & CLUSTERING
# ==============================================================================

#' Computes PrefMaX analysis
#'
#' @param Y Liking matrix.
#' @param X Product variables matrix.
#' @param scale.unit Standardize product variables.
#' @param ncp Number of dimensions.
#'
#' @return A list with PrefMaX model results.
#' @export
prefmax <- function(Y, X, scale.unit = FALSE, ncp = 5) {
  Y <- as.matrix(Y); X <- as.matrix(X)
  n <- nrow(Y); p <- ncol(Y); q <- ncol(X)

  R_mat <- matrix(NA, nrow = p, ncol = q, dimnames = list(colnames(Y), colnames(X)))
  XOPT_mat <- matrix(NA, nrow = p, ncol = q, dimnames = list(colnames(Y), colnames(X)))
  bounds <- data.frame(Min = apply(X, 2, min, na.rm = TRUE), Max = apply(X, 2, max, na.rm = TRUE), row.names = colnames(X))

  for (k in 1:q) {
    x_k <- X[, k]
    for (j in 1:p) {
      fit <- stats::lm(Y[, j] ~ x_k + I(x_k^2))
      R_mat[j, k] <- sqrt(summary(fit)$r.squared)

      coeffs <- stats::coef(fit)
      b <- ifelse(is.na(coeffs["x_k"]), 0, coeffs["x_k"])
      c <- ifelse(is.na(coeffs["I(x_k^2)"]), 0, coeffs["I(x_k^2)"])

      candidates <- c(bounds$Min[k], bounds$Max[k])
      if (c < 0) {
        x_top <- -b / (2 * c)
        if (x_top >= bounds$Min[k] && x_top <= bounds$Max[k]) candidates <- c(candidates, x_top)
      }
      preds <- stats::predict(fit, newdata = data.frame(x_k = candidates))
      XOPT_mat[j, k] <- candidates[which.max(preds)]
    }
  }

  weights <- rowMeans(R_mat)
  pca_res <- FactoMineR::PCA(XOPT_mat, scale.unit = scale.unit, ncp = ncp, row.w = weights, graph = FALSE)

  res <- list(Y = Y, X = X, R = R_mat, XOPT = XOPT_mat, bounds = bounds, weights = weights, pca = pca_res)
  class(res) <- "prefmax"
  return(res)
}

#' Clustering for PrefMaX
#'
#' @param x PrefMaX object.
#' @param ncp Dimensions.
#' @param nb.clust Number of clusters.
#' @param max.clust Max cluster search count.
#'
#' @return A list with PrefMaX clustering results.
#' @export
prefmax_cluster <- function(x, ncp = 2, nb.clust = NULL, max.clust = 6) {
  coords <- x$pca$ind$coord[, 1:min(ncp, ncol(x$pca$ind$coord)), drop = FALSE]
  q <- ncol(x$XOPT)

  sil_df <- NULL

  if (is.null(nb.clust)) {
    sil_scores <- numeric(max.clust - 1)
    cat("\n[SILHOUETTE SEARCH] Average Silhouette coefficients (PrefMaX):\n")
    for (k in 2:max.clust) {
      set.seed(123)
      km_test <- stats::kmeans(coords, centers = k, nstart = 25)
      sil <- cluster::silhouette(km_test$cluster, stats::dist(coords))
      sil_scores[k - 1] <- mean(sil[, 3])
      cat(sprintf("   - k = %d clusters: Mean Silhouette = %.4f\n", k, sil_scores[k - 1]))
    }
    nb.clust <- which.max(sil_scores) + 1
    cat(sprintf("[SILHOUETTE SELECTION] Optimal number of clusters selected by Silhouette criterion: k = %d\n", nb.clust))

    sil_df <- data.frame(
      Clusters = as.character(2:max.clust),
      Average_Silhouette = round(sil_scores, 4),
      Selected = ifelse(2:max.clust == nb.clust, "YES", ""),
      stringsAsFactors = FALSE
    )
    footer_row <- data.frame(
      Clusters = sprintf("Clustering performed on %d dimensions with %d clusters.", min(ncp, ncol(x$pca$ind$coord)), nb.clust),
      Average_Silhouette = "", Selected = "", stringsAsFactors = FALSE
    )
    sil_df <- rbind(sil_df, footer_row)

  } else {
    if (nb.clust > 1) {
      set.seed(123)
      km_test <- stats::kmeans(coords, centers = nb.clust, nstart = 25)
      sil <- cluster::silhouette(km_test$cluster, stats::dist(coords))
      sil_df <- data.frame(
        Clusters = as.character(nb.clust),
        Average_Silhouette = round(mean(sil[, 3]), 4),
        Selected = "YES (User Defined)", stringsAsFactors = FALSE
      )
    } else {
      sil_df <- data.frame(
        Clusters = "1",
        Average_Silhouette = "N/A",
        Selected = "YES (User Defined)", stringsAsFactors = FALSE
      )
    }
  }

  set.seed(123)
  km_res <- stats::kmeans(coords, centers = nb.clust, nstart = 50)
  counts <- table(km_res$cluster)

  cluster_labels_legend <- paste0("Cluster_", 1:nb.clust, " (n=", counts[as.character(1:nb.clust)], ")")
  cluster_labels_clean <- paste0("Cluster_", 1:nb.clust)

  clusters_legend <- factor(cluster_labels_legend[km_res$cluster], levels = cluster_labels_legend)
  names(clusters_legend) <- rownames(coords)

  clusters_clean <- factor(cluster_labels_clean[km_res$cluster], levels = cluster_labels_clean)
  names(clusters_clean) <- rownames(coords)

  sil_values <- if (nb.clust > 1) cluster::silhouette(km_res$cluster, stats::dist(coords))[, 3] else rep(1, nrow(coords))
  names(sil_values) <- rownames(coords)

  optima_by_cluster <- matrix(NA, nrow = nb.clust, ncol = q, dimnames = list(cluster_labels_clean, colnames(x$XOPT)))
  for (c_idx in 1:nb.clust) {
    c_label <- cluster_labels_clean[c_idx]
    cons_in_clust <- names(clusters_clean)[clusters_clean == c_label]
    for (k in 1:q) {
      xopt_vals <- x$XOPT[cons_in_clust, k]
      r_weights <- x$R[cons_in_clust, k]
      if (sum(r_weights, na.rm = TRUE) > 0) {
        optima_by_cluster[c_idx, k] <- sum(xopt_vals * r_weights, na.rm = TRUE) / sum(r_weights, na.rm = TRUE)
      } else {
        optima_by_cluster[c_idx, k] <- mean(xopt_vals, na.rm = TRUE)
      }
    }
  }

  res <- list(clusters_legend = clusters_legend, clusters_clean = clusters_clean, sil_values = sil_values,
              nb.clust = nb.clust, counts = counts, optima_means = as.data.frame(optima_by_cluster),
              silhouette_table = sil_df, prefmax_obj = x)
  class(res) <- "prefmax_cluster"
  return(res)
}

#' Summary method for prefmax_cluster
#'
#' @param object A \code{prefmax_cluster} object.
#' @param ... Additional arguments.
#'
#' @return A data frame containing ANOVA test statistics and cluster optimum means.
#' @export
summary.prefmax_cluster <- function(object, ...) {
  xopt_mat <- object$prefmax_obj$XOPT
  r_mat    <- object$prefmax_obj$R
  clusters <- object$clusters_clean
  nb_clust <- object$nb.clust
  q        <- ncol(xopt_mat)
  desc_names <- colnames(xopt_mat)

  counts <- object$counts
  cluster_cols_labels <- paste0("Cluster_", 1:nb_clust, " (n = ", counts[as.character(1:nb_clust)], ")")

  res_df <- data.frame(Attribute = desc_names, F_stat = numeric(q), probF = numeric(q), stringsAsFactors = FALSE, check.names = FALSE)

  for (col in cluster_cols_labels) res_df[[col]] <- NA_real_

  for (k in 1:q) {
    var_name <- desc_names[k]
    df_k <- data.frame(XOPT = xopt_mat[, k], Weight = r_mat[, k], Cluster = clusters[rownames(xopt_mat)])
    df_k <- df_k[!is.na(df_k$XOPT) & !is.na(df_k$Weight) & df_k$Weight > 0, ]

    if (nrow(df_k) > nb_clust && length(unique(df_k$Cluster)) > 1) {
      fit <- stats::lm(XOPT ~ Cluster, data = df_k, weights = Weight)
      anova_res <- stats::anova(fit)
      res_df$F_stat[k] <- anova_res["Cluster", "F value"]
      res_df$probF[k]  <- anova_res["Cluster", "Pr(>F)"]
    }

    for (c_idx in 1:nb_clust) {
      clean_label <- paste0("Cluster_", c_idx)
      col_label   <- cluster_cols_labels[c_idx]
      res_df[k, col_label] <- object$optima_means[clean_label, var_name]
    }
  }

  res_df <- res_df[order(res_df$F_stat, decreasing = TRUE, na.last = TRUE), ]
  res_df$F_stat <- round(res_df$F_stat, 2)
  res_df$probF  <- format.pval(res_df$probF, digits = 3, eps = 0.001)
  for (col in cluster_cols_labels) res_df[[col]] <- round(res_df[[col]], 2)

  rownames(res_df) <- NULL
  return(res_df)
}

#' Plot method for prefmax
#'
#' @param x A \code{prefmax} object.
#' @param axes Axes pair to plot (default: c(1, 2)).
#' @param scale_desc Vector scaling factor.
#' @param cluster Clustering object.
#' @param centroids Centroids data frame.
#' @param label_consumers Display consumer labels.
#' @param subj_illu Illustrative subject variables.
#' @param ... Additional arguments.
#'
#' @return A ggplot object.
#' @export
plot.prefmax <- function(x, axes = c(1, 2), scale_desc = 1, cluster = NULL, centroids = NULL, label_consumers = FALSE, subj_illu = NULL, ...) {
  pca_res <- x$pca
  cons_coords <- as.data.frame(pca_res$ind$coord[, axes])
  colnames(cons_coords) <- c("DimX", "DimY")
  cons_coords$Consumer <- rownames(cons_coords)

  if (!is.null(cluster)) {
    cons_coords$Cluster <- cluster$clusters_legend[rownames(cons_coords)]
  } else {
    cons_coords$Cluster <- factor("Consumers")
  }

  raw_coords <- pca_res$var$coord[, axes]
  scale_factor <- if (is.null(scale_desc) || scale_desc == 1) {
    max(abs(c(cons_coords$DimX, cons_coords$DimY))) / max(abs(raw_coords)) * 0.75
  } else scale_desc

  desc_coords <- as.data.frame(raw_coords * scale_factor)
  colnames(desc_coords) <- c("DimX", "DimY")
  desc_coords$Label <- rownames(desc_coords)

  X_centered <- scale(x$X, center = pca_res$call$centre, scale = FALSE)
  prod_coords <- as.data.frame(X_centered %*% pca_res$svd$V[, axes])
  colnames(prod_coords) <- c("DimX", "DimY")
  prod_coords$Label <- rownames(prod_coords)

  var_ax1 <- round(pca_res$eig[axes[1], 2], 1)
  var_ax2 <- round(pca_res$eig[axes[2], 2], 1)
  total_var <- var_ax1 + var_ax2

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
    ggplot2::geom_point(data = cons_coords, ggplot2::aes(x = DimX, y = DimY, color = Cluster), alpha = 0.8, size = 2.5)

  if (label_consumers) {
    p <- p + ggrepel::geom_text_repel(data = cons_coords, ggplot2::aes(x = DimX, y = DimY, label = Consumer, color = Cluster), size = 2.8, max.overlaps = Inf)
  }

  p <- p +
    ggplot2::geom_segment(data = desc_coords, ggplot2::aes(x = 0, y = 0, xend = DimX, yend = DimY), arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), color = "steelblue") +
    ggrepel::geom_text_repel(data = desc_coords, ggplot2::aes(x = DimX, y = DimY, label = Label), color = "steelblue", fontface = "bold", max.overlaps = Inf) +
    ggplot2::geom_point(data = prod_coords, ggplot2::aes(x = DimX, y = DimY), color = "firebrick", size = 4, shape = 17) +
    ggrepel::geom_text_repel(data = prod_coords, ggplot2::aes(x = DimX, y = DimY, label = Label), color = "firebrick", fontface = "bold", max.overlaps = Inf)

  if (!is.null(centroids)) {
    col_x <- paste0("Dim", axes[1]); col_y <- paste0("Dim", axes[2])
    df_cent <- data.frame(
      Label = paste0("C", 1:nrow(centroids)),
      Cluster = centroids$Cluster_Legend,
      DimX = centroids[[col_x]],
      DimY = centroids[[col_y]],
      stringsAsFactors = FALSE
    )
    p <- p +
      ggplot2::geom_point(data = df_cent, ggplot2::aes(x = DimX, y = DimY, color = Cluster), shape = 15, size = 3.5) +
      ggrepel::geom_text_repel(data = df_cent, ggplot2::aes(x = DimX, y = DimY, label = Label, color = Cluster), fontface = "bold", size = 4.5, max.overlaps = Inf)
  }

  if (!is.null(subj_illu$quanti)) {
    col_x <- paste0("Dim", axes[1]); col_y <- paste0("Dim", axes[2])
    df_sq <- subj_illu$quanti[, c("Var", col_x, col_y)]; colnames(df_sq) <- c("Var", "DimX", "DimY")
    p <- p +
      ggplot2::geom_segment(data = df_sq, ggplot2::aes(x = 0, y = 0, xend = DimX, yend = DimY), arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), color = "darkgreen", linetype = "solid") +
      ggrepel::geom_text_repel(data = df_sq, ggplot2::aes(x = DimX, y = DimY, label = Var), color = "darkgreen", fontface = "bold.italic", max.overlaps = Inf)
  }

  if (!is.null(subj_illu$quali)) {
    col_x <- paste0("Dim", axes[1]); col_y <- paste0("Dim", axes[2])
    df_skl <- subj_illu$quali[, c("Var", col_x, col_y)]; colnames(df_skl) <- c("Var", "DimX", "DimY")
    p <- p +
      ggplot2::geom_point(data = df_skl, ggplot2::aes(x = DimX, y = DimY), color = "darkgreen", shape = 18, size = 3.5) +
      ggrepel::geom_text_repel(data = df_skl, ggplot2::aes(x = DimX, y = DimY, label = Var), color = "darkgreen", fontface = "bold.italic", max.overlaps = Inf)
  }

  plot_title <- sprintf("PrefMax consumers by attributes COV Biplot (Plane %d-%d, %.1f %%)", axes[1], axes[2], total_var)
  plot_subtitle <- "Products projected as supplementary points"

  p <- p + ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8), legend.position = "bottom") +
    ggplot2::labs(title = plot_title, subtitle = plot_subtitle,
                  x = sprintf("Dimension %d (%.1f %%)", axes[1], var_ax1), y = sprintf("Dimension %d (%.1f %%)", axes[2], var_ax2),
                  color = "Consumer Clusters")

  return(p)
}

#' Executes full PrefMaX workflow
#'
#' @param path Input directory.
#' @param file Input Excel filename.
#' @param ShortID Short identifier string.
#' @param sheet_liking Sheet with liking data.
#' @param sheet_products Sheet with product data.
#' @param sheet_subjects Sheet with subject data.
#' @param scale.unit Standardize product variables in PCA.
#' @param ncp Dimensions count.
#' @param nb.clust Forced cluster count.
#' @param max.clust Max cluster search.
#' @param scale_desc Descriptor scaling.
#' @param scale_subj Subject variables scaling.
#' @param label_consumers Display consumer labels.
#' @param output_dir Output directory path.
#' @param res_dpi Resolution DPI for TIFF.
#' @param save_tiff Export TIFF files.
#' @param sheet_hedo Legacy alias for sheet_liking.
#' @param sheet_qda Legacy alias for sheet_products.
#' @param ... Additional arguments.
#'
#' @return A list with PrefMaX execution results and plots.
#' @export
run_prefmax <- function(path, file, ShortID = NULL, sheet_liking = NULL, sheet_products = NULL, sheet_subjects = NULL,
                        scale.unit = FALSE, ncp = 2, nb.clust = NULL, max.clust = 6, scale_desc = 1, scale_subj = 1,
                        label_consumers = FALSE, output_dir = getwd(), res_dpi = 300, save_tiff = TRUE, sheet_hedo = NULL, sheet_qda = NULL, ...) {

  if (!is.null(sheet_hedo)) sheet_liking <- sheet_hedo
  if (!is.null(sheet_qda))  sheet_products <- sheet_qda

  if (is.null(sheet_liking)) {
    stop("'sheet_liking' is a required parameter and must be specified with the name of the sheet containing consumer liking data.")
  }

  full_file_path <- file.path(path, file)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  avail_sheets <- readxl::excel_sheets(full_file_path)

  if (!sheet_liking %in% avail_sheets) {
    stop(sprintf("Liking sheet '%s' not found in file '%s'.", sheet_liking, file))
  }

  has_products <- !is.null(sheet_products) && (sheet_products %in% avail_sheets)
  if (!has_products) {
    stop("For this method, sheet_products must be specified with the name of the sheet in the input data file containing the product variables.")
  }

  has_subjects <- !is.null(sheet_subjects) && (sheet_subjects %in% avail_sheets)

  Y_df <- readxl::read_excel(full_file_path, sheet = sheet_liking)
  X_df <- readxl::read_excel(full_file_path, sheet = sheet_products)

  Y <- as.matrix(Y_df[, -1]); rownames(Y) <- as.character(as.data.frame(Y_df)[, 1])
  X <- as.matrix(X_df[, -1]); rownames(X) <- as.character(as.data.frame(X_df)[, 1])

  subj_mat_full <- NULL
  if (has_subjects) {
    df_subj_raw <- readxl::read_excel(full_file_path, sheet = sheet_subjects)
    subj_mat_full <- as.matrix(df_subj_raw[, -1])
    rownames(subj_mat_full) <- df_subj_raw[[1]]
  }

  n_prod <- nrow(X); n_cons <- ncol(Y); n_desc <- ncol(X)

  cat(sprintf("[DATA INFO] File: %s\n", file))
  cat(sprintf("            Liking Sheet   : %d Products (N) x %d Consumers (P)\n", n_prod, n_cons))
  cat(sprintf("            Products Sheet : %d Products (N) x %d Attributes (Q)\n", n_prod, n_desc))
  if (has_subjects) cat(sprintf("            Subjects Sheet : %d Consumers (P) x %d Variables\n", nrow(subj_mat_full), ncol(subj_mat_full)))

  res_pref <- prefmax(Y, X, scale.unit = scale.unit, ncp = ncp)

  eig_1 <- round(res_pref$pca$eig[1, 2], 2)
  eig_2 <- round(res_pref$pca$eig[2, 2], 2)
  eig_tot <- round(eig_1 + eig_2, 2)
  cat(sprintf("[PREFMAX RESULT] Inertia explained: Dim 1 = %.2f %%, Dim 2 = %.2f %% (Total = %.2f %%)\n",
              eig_1, eig_2, eig_tot))

  prefix <- if (!is.null(ShortID) && nchar(trimws(ShortID)) > 0) paste0(trimws(ShortID), "_") else ""

  wb <- openxlsx::createWorkbook()

  df_data_verif <- data.frame(
    Parametre = c("ShortID", "Repertoire (PATH)", "Fichier source", "Methode", "Type ACP", "Feuille Liking", "Feuille Products", "Feuille Subjects", "Nombre de Produits (N)", "Nombre de Consommateurs (P)", "Nombre de Descripteurs (Q)", "Dimensions (ncp)"),
    Valeur = c(ShortID, path, file, "PREFMAX",
               ifelse(scale.unit, "Standardisee (scale.unit = TRUE)", "Non standardisee (scale.unit = FALSE)"), sheet_liking, sheet_products, ifelse(has_subjects, sheet_subjects, "N/A"), n_prod, n_cons, n_desc, ncp),
    stringsAsFactors = FALSE
  )
  openxlsx::addWorksheet(wb, "Data_Verification")
  openxlsx::writeData(wb, "Data_Verification", df_data_verif, startRow = 1, startCol = 1)

  curr_verif_row <- 14
  openxlsx::writeData(wb, "Data_Verification", "Domaine de variation des descripteurs Produits (Min / Max) :", startRow = curr_verif_row, startCol = 1)
  openxlsx::writeData(wb, "Data_Verification", res_pref$bounds, startRow = curr_verif_row + 1, startCol = 1, rowNames = TRUE)
  curr_verif_row <- curr_verif_row + nrow(res_pref$bounds) + 3

  if (has_subjects) {
    curr_verif_row <- write_subjects_verification(wb, "Data_Verification", subj_mat_full, start_row = curr_verif_row)
  }

  df_eig <- as.data.frame(res_pref$pca$eig)
  colnames(df_eig) <- c("Eigenvalue", "Variance_Pct", "Cumulative_Pct")
  openxlsx::addWorksheet(wb, "Eigenvalues"); openxlsx::writeData(wb, "Eigenvalues", round(df_eig, 4), rowNames = TRUE)

  X_centered <- scale(res_pref$X, center = res_pref$pca$call$centre, scale = FALSE)
  prod_coords <- as.data.frame(X_centered %*% res_pref$pca$svd$V[, 1:ncp, drop = FALSE])
  colnames(prod_coords) <- paste0("Dim", 1:ncp)
  prod_coords <- data.frame(Product = rownames(prod_coords), prod_coords, stringsAsFactors = FALSE)
  openxlsx::addWorksheet(wb, "Coordinates_Products"); openxlsx::writeData(wb, "Coordinates_Products", prod_coords)

  openxlsx::addWorksheet(wb, "XOPT"); openxlsx::writeData(wb, "XOPT", as.data.frame(res_pref$XOPT), rowNames = TRUE)

  df_r_sqrt <- as.data.frame(res_pref$R)
  df_r_sqrt$`Subject Weight` <- res_pref$weights
  openxlsx::addWorksheet(wb, "R_sqrt"); openxlsx::writeData(wb, "R_sqrt", df_r_sqrt, rowNames = TRUE)

  res_clust <- prefmax_cluster(res_pref, ncp = ncp, nb.clust = nb.clust, max.clust = max.clust)
  actual_nb_clus <- res_clust$nb.clust
  table_res <- summary.prefmax_cluster(res_clust)

  cons_coords <- as.data.frame(res_pref$pca$ind$coord[, 1:ncp, drop = FALSE])
  colnames(cons_coords) <- paste0("Dim", 1:ncp)

  df_consumers <- data.frame(
    Consumer = colnames(Y),
    Cluster = as.character(res_clust$clusters_clean[colnames(Y)]),
    Silhouette = round(as.numeric(res_clust$sil_values[colnames(Y)]), 4),
    cons_coords[colnames(Y), , drop = FALSE],
    stringsAsFactors = FALSE, check.names = FALSE
  )
  df_consumers$Weight_Sil <- pmax(0, df_consumers$Silhouette)
  df_consumers <- df_consumers[order(df_consumers$Cluster, -df_consumers$Silhouette), ]

  c_centroids_list <- list()
  for (k in 1:actual_nb_clus) {
    c_label <- paste0("Cluster_", k)
    sub_c <- df_consumers[df_consumers$Cluster == c_label, ]
    w_k <- sub_c$Weight_Sil
    if (sum(w_k) == 0) w_k <- rep(1, nrow(sub_c))
    c_centroids_list[[k]] <- colSums(sub_c[, paste0("Dim", 1:ncp), drop = FALSE] * w_k) / sum(w_k)
  }
  c_centroids <- do.call(rbind, c_centroids_list)

  counts <- res_clust$counts
  cluster_labels_legend <- paste0("Cluster_", 1:actual_nb_clus, " (n=", counts[as.character(1:actual_nb_clus)], ")")

  df_weighted_centroids <- data.frame(
    Cluster = paste0("Cluster_", 1:actual_nb_clus),
    Cluster_Legend = factor(cluster_labels_legend, levels = cluster_labels_legend),
    as.data.frame(round(c_centroids, 4)),
    check.names = FALSE
  )

  P_mat <- as.matrix(prod_coords[, paste0("Dim", 1:ncp)])
  dist_mat <- matrix(0, nrow = n_prod, ncol = actual_nb_clus)
  rownames(dist_mat) <- prod_coords$Product
  colnames(dist_mat) <- paste0("Cluster_", 1:actual_nb_clus)

  for (i in 1:n_prod) {
    for (j in 1:actual_nb_clus) dist_mat[i, j] <- round(sqrt(sum((P_mat[i, ] - c_centroids[j, ])^2)), 4)
  }
  dist_prod_cluster <- data.frame(Product = rownames(dist_mat), as.data.frame(dist_mat), check.names = FALSE)

  liking_mat <- matrix(0, nrow = n_prod, ncol = actual_nb_clus)
  rownames(liking_mat) <- prod_coords$Product
  colnames(liking_mat) <- paste0("Cluster_", 1:actual_nb_clus)

  for (j in 1:actual_nb_clus) {
    c_label <- paste0("Cluster_", j)
    sub_c <- df_consumers[df_consumers$Cluster == c_label, ]
    w_k <- sub_c$Weight_Sil
    if (sum(w_k) == 0) w_k <- rep(1, nrow(sub_c))
    cons_ids <- as.character(sub_c$Consumer)
    valid_ids <- intersect(cons_ids, colnames(Y))
    if (length(valid_ids) > 0) {
      l_sub <- Y[prod_coords$Product, valid_ids, drop = FALSE]
      w_sub <- w_k[match(valid_ids, cons_ids)]
      for (i in 1:n_prod) liking_mat[i, j] <- round(sum(l_sub[i, ] * w_sub) / sum(w_sub), 4)
    }
  }
  df_weighted_liking_clusters <- data.frame(Product = prod_coords$Product, as.data.frame(liking_mat), check.names = FALSE)

  df_cons_export <- df_consumers
  df_cons_export$Weight_Sil <- NULL

  df_cent_export <- df_weighted_centroids
  df_cent_export$Cluster_Legend <- NULL

  openxlsx::addWorksheet(wb, "Silhouette_Scores"); openxlsx::writeData(wb, "Silhouette_Scores", res_clust$silhouette_table)

  openxlsx::addWorksheet(wb, "Consumers"); openxlsx::writeData(wb, "Consumers", df_cons_export)

  raw_coords <- res_pref$pca$var$coord[, 1:ncp, drop = FALSE]
  scale_factor <- max(abs(c(cons_coords$Dim1, cons_coords$Dim2))) / max(abs(raw_coords)) * 0.75
  df_prod_vars <- data.frame(Var = rownames(raw_coords), as.data.frame(raw_coords * scale_factor), stringsAsFactors = FALSE)
  colnames(df_prod_vars)[-1] <- paste0("Dim", 1:ncp)
  openxlsx::addWorksheet(wb, "Loadings_Var_Products"); openxlsx::writeData(wb, "Loadings_Var_Products", df_prod_vars)

  subj_illu <- if (has_subjects) compute_subj_illustrative(subj_mat_full, df_consumers, ncp, scale_subj) else list(quanti = NULL, quali = NULL)
  if (has_subjects) {
    df_subj_corr <- rbind(subj_illu$quanti, subj_illu$quali)
    if (!is.null(df_subj_corr) && nrow(df_subj_corr) > 0) {
      openxlsx::addWorksheet(wb, "Corr_Var_Subjects"); openxlsx::writeData(wb, "Corr_Var_Subjects", df_subj_corr)
    }
  }

  openxlsx::addWorksheet(wb, "Clusters_Centroids"); openxlsx::writeData(wb, "Clusters_Centroids", df_cent_export)

  openxlsx::addWorksheet(wb, "Distances_Products_Clusters"); openxlsx::writeData(wb, "Distances_Products_Clusters", dist_prod_cluster)

  openxlsx::addWorksheet(wb, "Cluster_Products_Liking"); openxlsx::writeData(wb, "Cluster_Products_Liking", df_weighted_liking_clusters)

  openxlsx::addWorksheet(wb, "Cluster_Sensory_Optimisation"); openxlsx::writeData(wb, "Cluster_Sensory_Optimisation", table_res)

  clus_suffix <- if (actual_nb_clus > 1) paste0("_Clus", actual_nb_clus) else ""
  excel_name <- sprintf("%sPrefMaX_Results_%dD%s.xlsx", prefix, ncp, clus_suffix)
  openxlsx::saveWorkbook(wb, file = file.path(output_dir, excel_name), overwrite = TRUE)

  planes_to_plot <- if (ncp >= 2) lapply(1:(ncp - 1), function(i) c(i, i + 1)) else list(c(1, 1))
  plot_list <- list()

  for (pair in planes_to_plot) {
    p_biplot <- plot.prefmax(res_pref, axes = pair, cluster = res_clust, centroids = df_weighted_centroids, scale_desc = scale_desc, label_consumers = label_consumers, subj_illu = subj_illu)
    if (interactive()) {
      print(p_biplot)
    }

    plot_key <- sprintf("%sPrefMaX_%dD_Plane_%d-%d%s", prefix, ncp, pair[1], pair[2], clus_suffix)
    plot_list[[plot_key]] <- p_biplot

    if (save_tiff) {
      ggplot2::ggsave(filename = file.path(output_dir, paste0(plot_key, ".tif")), plot = p_biplot,
                      device = "tiff", width = 8.5, height = 6.5, dpi = res_dpi, compression = "lzw")
    }
  }

  return(invisible(list(prefmax = res_pref, cluster = res_clust, summary_table = table_res, plots = plot_list)))
}
