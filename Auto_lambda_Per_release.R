# Auto_lambda_Per_release: Per-Release λ based on file-level Spearman correlation coefficient
# 2026-08-12 (refactored 2026-10-11)
#
# Main idea:
#   For each V2→V3 pair, use the V2 data corresponding to that pair:
#     1. Calculate the Pearson correlation coefficient between the spectrum.score ranking and the defective label ranking in each file (Spearman approximation)
#     2. Take the average across files to get the release-level MeanRho
#     3. Convert MeanRho to λ = (ρ + 1) / 2 ∈ [0, 1]
#
# Key fixes:
#   1. Use the true defective labels in line-level-defect-prediction/Dataset/Line-level
#   2. Use the mapping files in [Datasets]/map-warning-lines-only/{SAT}/LineMapping/
#      to convert the compressed_line_number of spectrum to original_line_number
#   3. The original scan results of SAT are located in the ./SATs_initial_scan_result/<SAT>/ directory
#      (The source data of SATs like ErrorProne has been统一放置在该目录顶层，不再有 test/ 子目录）

library(tidyverse)

# ==================== Path configuration ====================
LINE_LEVEL_DIR = "./datasets/line-level-defect-prediction/Dataset/Line-level/"
MAPPING_BASE_DIR = "./datasets/map-warning-lines-only/"

SOUND_SPECTRUM_ROOT = "./SATs_result/"

LAMBDA_OUTPUT_FILE = "./Auto_lambda_Per_release_Results.txt"
dir.create(dirname(LAMBDA_OUTPUT_FILE), recursive = TRUE, showWarnings = FALSE)

# ==================== V2→V3 pairs ====================
v2_v3_pairs = list(
  list(v2 = "activemq-5.1.0", v3 = "activemq-5.2.0",  project = "activemq"),
  list(v2 = "activemq-5.2.0", v3 = "activemq-5.3.0",  project = "activemq"),
  list(v2 = "activemq-5.3.0", v3 = "activemq-5.8.0",  project = "activemq"),
  list(v2 = "camel-2.9.0",    v3 = "camel-2.10.0",   project = "camel"),
  list(v2 = "camel-2.10.0",  v3 = "camel-2.11.0",   project = "camel"),
  list(v2 = "derby-10.3.1.4",v3 = "derby-10.5.1.1", project = "derby"),
  list(v2 = "groovy-1_6_BETA_1", v3 = "groovy-1_6_BETA_2", project = "groovy"),
  list(v2 = "hbase-0.95.0",  v3 = "hbase-0.95.2",   project = "hbase"),
  list(v2 = "hive-0.10.0",   v3 = "hive-0.12.0",    project = "hive"),
  list(v2 = "jruby-1.4.0",   v3 = "jruby-1.5.0",    project = "jruby"),
  list(v2 = "jruby-1.5.0",   v3 = "jruby-1.7.0.preview1", project = "jruby"),
  list(v2 = "lucene-2.9.0",  v3 = "lucene-3.0.0",   project = "lucene"),
  list(v2 = "lucene-3.0.0",  v3 = "lucene-3.1",     project = "lucene"),
  list(v2 = "wicket-1.3.0-beta2", v3 = "wicket-1.5.3", project = "wicket")
)

sat.tools = c("PMD", "CheckStyle", "ErrorProne", "Spotbugs", "Codacy", "Codeql", "Sonarqube")

spectrum.names = c("Barinel", "Dstar", "Ochiai", "Op2", "Tarantula")

# ==================== Helper functions ====================
rho_to_lambda = function(rho) {
  lambda = (rho + 1) / 2
  lambda = max(0, min(1, lambda))
  round(lambda, 4)
}

spectrum.subdirs = list(
  Barinel   = "BarinelwithoutCA",
  Dstar     = "DstarWithoutCA",
  Ochiai    = "OchiaiWithoutCA",
  Op2       = "Op2WithoutCA",
  Tarantula = "TarantulaWithoutCA"
)

# Load V2 spectrum data
load.v2.spectrum = function(sat.name, spec.label, v2_release, project) {
  base.dir = paste0(SOUND_SPECTRUM_ROOT,
                     sat.name, "/Result/",
                     spectrum.subdirs[[spec.label]], "/line_result/")
  file.path = paste0(base.dir, project, "/", v2_release, "-result.csv")
  if (!file.exists(file.path)) {
    return(NULL)
  }
  df <- read.csv(file.path, stringsAsFactors = FALSE)
  colnames(df) = c("filename_line", "spectrum.score", "max_jaccard")

  temp <- strsplit(df[["filename_line"]], ":", fixed = TRUE)
  filename <- sapply(temp, function(x) paste(x[-length(x)], collapse = ":"))
  line.number <- as.integer(sapply(temp, function(x) x[length(x)]))
  spectrum.score <- as.numeric(df[["spectrum.score"]])

  result <- data.frame(
    filename = filename,
    compressed_line_number = line.number,
    spectrum.score = spectrum.score,
    stringsAsFactors = FALSE
  )
  return(result)
}

# Load mapping
load.mapping = function(sat.name, v2_release) {
  mapping_file = paste0(MAPPING_BASE_DIR, sat.name, "_Dataset/LineMapping/",
                        v2_release, "_all_files_mapping.csv")
  if (!file.exists(mapping_file)) {
    return(NULL)
  }
  df <- read.csv(mapping_file, stringsAsFactors = FALSE)
  return(df)
}

# Load V2 ground truth
load.v2.ground.truth = function(v2_release) {
  file.path = paste0(LINE_LEVEL_DIR, v2_release, "_defective_lines_dataset.csv")
  if (!file.exists(file.path)) {
    return(NULL)
  }
  df <- read.csv(file.path, stringsAsFactors = FALSE)
  result <- data.frame(
    filename = df$File,
    original_line_number = df$Line_number,
    is_defective = 1,
    stringsAsFactors = FALSE
  )
  return(result)
}

# ==================== Main flow ====================

cat("\n========== V2-MeanRho Per-Release λ calculation ==========\n\n")

lambda_results = list()

for (sat in sat.tools) {
  for (spec in spectrum.names) {
    cat(sprintf("Processing %s - %s ...\n", sat, spec))

    pair_lambdas = list()

    for (pair in v2_v3_pairs) {
      v2_rel = pair$v2
      v3_rel = pair$v3
      project = pair$project

      # 1. Load V2 spectrum data
      v2_spec = load.v2.spectrum(sat, spec, v2_rel, project)
      if (is.null(v2_spec)) next

      # 2. Load mapping
      v2_mapping = load.mapping(sat, v2_rel)
      if (is.null(v2_mapping)) next

      # 3. Load V2 ground truth
      v2_gt = load.v2.ground.truth(v2_rel)
      if (is.null(v2_gt)) next

      # 4. Use mapping to convert compressed_line_number to original_line_number
      spec_with_orig = merge(v2_spec, v2_mapping,
                             by.x = c("filename", "compressed_line_number"),
                             by.y = c("File", "compressed_line_number"),
                             all.x = TRUE)

      # 5. Merge with ground truth using original_line_number
      merged = merge(spec_with_orig, v2_gt,
                     by = c("filename", "original_line_number"),
                     all.x = TRUE)
      merged$is_defective = ifelse(is.na(merged$is_defective), 0, 1)

      n_def = sum(merged$is_defective == 1)

      # 6. File-level Spearman calculation
      #    - Only keep files with at least 1 defective line
      #    - Calculate ρ for each file
      #    - Summarize with median and mean
      file_rhos = c()
      file_counts = c()

      files_with_defect = unique(merged$filename[merged$is_defective == 1])

      for (fname in files_with_defect) {
        file_data = merged[merged$filename == fname, ]
        n_file = nrow(file_data)
        n_def_file = sum(file_data$is_defective == 1)

        # Each file needs at least 5 samples and at least 2 defective lines to calculate meaningful correlation
        if (n_file < 5 || n_def_file < 2) next

        # rank smaller → higher priority
        # spec_rank: higher score → smaller rank; ties sorted by original_line_number
        # label_rank: defective lines first → smaller rank; ties sorted by original_line_number
        spec_order = order(-file_data$spectrum.score, file_data$original_line_number)
        label_order = order(-file_data$is_defective, file_data$original_line_number)
        file_data$spec_rank = NA_integer_
        file_data$spec_rank[spec_order] = 1:n_file
        file_data$label_rank = NA_integer_
        file_data$label_rank[label_order] = 1:n_file
        rho = cor(file_data$spec_rank, file_data$label_rank, method = "pearson")
        if (!is.na(rho) && is.finite(rho)) {
          file_rhos = c(file_rhos, rho)
          file_counts = c(file_counts, n_def_file)
        }
      }

      # 汇总为 release 级别
      if (length(file_rhos) >= 3) {
        spearman_mean = mean(file_rhos, na.rm = TRUE)
      } else {
        spearman_mean = 0
      }

      pair_key = paste0(v2_rel, "->", v3_rel)
      lambda_val = rho_to_lambda(spearman_mean)
      pair_lambdas[[pair_key]] = list(
        spearman_mean = spearman_mean,
        lambda = lambda_val,
        n_files = length(file_rhos)
      )

      cat(sprintf("  %s: MeanRho=%.4f (n_files=%d, n_def=%d)  λ_%s=%.4f\n",
                  pair_key, spearman_mean, length(file_rhos), n_def,
                  v3_rel, lambda_val))
    }

    key = paste0(sat, "_", spec)
    lambda_results[[key]] = list(
      sat = sat,
      spectrum = spec,
      pair_lambdas = pair_lambdas
    )
  }
}

# ==================== Output: MeanRho λ file ====================

sink(LAMBDA_OUTPUT_FILE)
cat("========== Per-Release λ calculation results (Mean) ==========\n\n")

for (sat in sat.tools) {
  cat(sprintf("--- %s ---\n", sat))
  for (spec in spectrum.names) {
    cat(sprintf("  %s:\n", spec))
    key = paste0(sat, "_", spec)
    if (!is.null(lambda_results[[key]])) {
      for (pair_key in names(lambda_results[[key]]$pair_lambdas)) {
        v = lambda_results[[key]]$pair_lambdas[[pair_key]]
        # Use the calculated lambda values directly
        cat(sprintf("    %s: %.4f\n", pair_key, v$lambda))
      }
    }
  }
}

sink()
cat(sprintf("Results saved to: %s\n", LAMBDA_OUTPUT_FILE))
