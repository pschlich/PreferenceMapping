library(PreferenceMapping)


res_sim <- PreferenceMapping(
  file_path = "dev/Simulated_Dataset.xlsx",
  method = c("internal","external","restricted_prod", "restricted_subj","restricted_both","prefmax"),
  sheet_liking = "Liking",
  sheet_products = "Products",
  sheet_subjects = "Subjects",
  ndim = 2,
  n_clusters = 3,
  label_consumers = FALSE,
  min_r2=0
)
