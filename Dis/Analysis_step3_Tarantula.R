# =====================================================================
# Analysis_step3_Tarantula.R - 2026-09-07
# ---------------------------------------------------------------------
  # Region Movement Analysis (APART-F vs FETCH) using only Tarantula spectrum
  #
# Core question: Does FETCH actually use the additional information to improve
# the ranking compared to APART-F?
#
# For each (filename, test release, SAT) combination:
#   Step 1: Calculate APART-F ranking (arrange(-NFC*NT, Priority, line.number))
#   Step 2: Divide APART-F ranking into 3 regions
#             Top 20%  : rank in [1, ceiling(N*0.20)]
#             20-50%   : rank in [ceiling(N*0.20)+1, ceiling(N*0.50)]
#             Bottom50%: rank in [ceiling(N*0.50)+1, N]
#   Step 3: Calculate FETCH ranking (arrange(desc(Persistent), desc(NEW3.score), Priority, line.number))
#   Step 4: Divide FETCH ranking into 3 regions (same N ratio)
#   Step 5: Compare region-to-region for each warning
#   Step 6: Statistically analyze 4 extreme transitions:
#             (a) 20-50    -> Top20     : promotion,  PQ_a = N_defect / N_warnings
#             (b) Bottom50 -> Top20     : promotion,  PQ_b = N_defect / N_warnings
#             (c) Top20    -> 20-50     : demotion,   DC_c = N_defect / N_warnings
#             (d) Top20    -> Bottom50  : demotion,   DC_d = N_defect / N_warnings
#             Denominator (N_warnings of this transition) is 0 -> "N/A"
#   Step 7: per-file CSV 18 columns per row
#   Step 8: Aggregate (release mean x SAT mean)
#
# Output:
#   Region_Movements_FDR.csv  : 7 SAT aggregated results (release mean x SAT mean)
# =====================================================================
library(tidyverse)
library(gridExtra)
library(lattice)
library(ModelMetrics)
library(caret)
library(reshape2)
library(car)
library(carData)
library(pROC)
library(effsize)
library(ScottKnottESD)
library(dplyr)
library(tibble)
library(stringr)
library(grid)

# ==================== Path Configuration (Linux server) ====================
BASE = '../'

SAT_DIR = paste0(BASE, 'SATs_initial_scan_result/')
PMD.result.dir        = paste0(SAT_DIR, 'PMD/')
CheckStyle.result.dir = paste0(SAT_DIR, 'CheckStyle/')
ErrorProne.result.dir = paste0(SAT_DIR, 'Errorprone/')
Spotbugs.result.dir   = paste0(SAT_DIR, 'Spotbugs/')
Codacy.result.dir     = paste0(SAT_DIR, 'codacy/')
Codeql.result.dir     = paste0(SAT_DIR, 'codeql/')
Sonarqube.result.dir  = paste0(SAT_DIR, 'sonarqube/')

Mapping.dir    = paste0(BASE, 'datasets/map-warning-lines-only/')
prediction_dir = paste0(BASE, 'Baseline/DeepLineDP/output/prediction/DeepLineDP/within-release/')
CEandNFCdir    = paste0(BASE, 'Baseline/GLANCE_and_LineDP/result/Glance_MD_full_threshold/line_result/test/')


output.dir = './'
agg_csv    = paste0(output.dir, 'Region_Movements_FDR.csv')
if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)


sat_order = c("PMD", "CheckStyle", "ErrorProne", "Spotbugs",
              "Codacy", "Codeql", "sonarqube")

# ==================== Spectrum: Only Tarantula ====================
get.spectrum.dir = function(sat.name) {
  paste0(BASE, 'SATs_result/',
         sat.name, '/Result/TarantulaWithoutCA/line_result/test/')
}

# ==================== Load Per-Release lambda (same as Plan7_PerReleaseLambda_MeanRho.R) ====================
v2_v3_pairs = list(
  list(v2 = "activemq-5.1.0",  v3 = "activemq-5.2.0",       project = "activemq"),
  list(v2 = "activemq-5.2.0",  v3 = "activemq-5.3.0",       project = "activemq"),
  list(v2 = "activemq-5.3.0",  v3 = "activemq-5.8.0",       project = "activemq"),
  list(v2 = "camel-2.9.0",     v3 = "camel-2.10.0",        project = "camel"),
  list(v2 = "camel-2.10.0",    v3 = "camel-2.11.0",        project = "camel"),
  list(v2 = "derby-10.3.1.4",  v3 = "derby-10.5.1.1",      project = "derby"),
  list(v2 = "groovy-1_6_BETA_1", v3 = "groovy-1_6_BETA_2", project = "groovy"),
  list(v2 = "hbase-0.95.0",    v3 = "hbase-0.95.2",        project = "hbase"),
  list(v2 = "hive-0.10.0",     v3 = "hive-0.12.0",         project = "hive"),
  list(v2 = "jruby-1.4.0",     v3 = "jruby-1.5.0",         project = "jruby"),
  list(v2 = "jruby-1.5.0",     v3 = "jruby-1.7.0.preview1", project = "jruby"),
  list(v2 = "lucene-2.9.0",    v3 = "lucene-3.0.0",        project = "lucene"),
  list(v2 = "lucene-3.0.0",    v3 = "lucene-3.1",          project = "lucene"),
  list(v2 = "wicket-1.3.0-beta2", v3 = "wicket-1.5.3",     project = "wicket")
)

lambda_file = paste0(BASE, 'Auto_lambda_Per_release_Results.txt')
lambda_lines = readLines(lambda_file)

per_release_lambda = list()
current_sat = NULL
current_spec = NULL

for (line in lambda_lines) {
  m_sat = regexpr("^--- (.+) ---", line)
  if (m_sat > 0) {
    current_sat = sub("^--- (.+) ---", "\\1", line)
    next
  }
  m_spec = regexpr("^  ([A-Za-z0-9]+):$", line)
  if (m_spec > 0) {
    current_spec = sub("^  ([A-Za-z0-9]+):$", "\\1", line)
    next
  }
  m_pair = regexpr("^    ([^:]+): ([0-9.]+)$", line)
  if (m_pair > 0 && !is.null(current_sat) && !is.null(current_spec)) {
    pair = sub("^    ([^:]+): ([0-9.]+)$", "\\1", line)
    lambda_val = as.numeric(sub("^    [^:]+: ([0-9.]+)$", "\\1", line))
    key = paste0(current_sat, "_", current_spec)
    if (is.null(per_release_lambda[[key]])) per_release_lambda[[key]] = list()
    per_release_lambda[[key]][[pair]] = lambda_val
    next
  }
}

cat(sprintf("[Lambda source] %s | Loaded %d (SAT, Spectrum) combinations\n",
            basename(lambda_file), length(per_release_lambda)))

get_lambda_for_release = function(sat_name, spectrum_name, v3_release) {
  key = paste0(sat_name, "_", spectrum_name)
  if (is.null(per_release_lambda[[key]])) return(0.5)
  for (pair in v2_v3_pairs) {
    if (pair$v3 == v3_release) {
      pair_key = paste0(pair$v2, "->", pair$v3)
      val = per_release_lambda[[key]][[pair_key]]
      if (is.null(val)) return(0.5)
      return(val)
    }
  }
  return(0.5)
}

# ==================== Load Ground Truth ====================
all_pred_files = list.files(prediction_dir)
df_all = NULL
for (f in all_pred_files) {
  df = read.csv(paste0(prediction_dir, f), stringsAsFactors = FALSE)
  df_all = rbind(df_all, df)
}

line.ground.truth = df_all %>%
  select(project, train, test, filename, line.number,
         line.level.ground.truth, is.comment.line) %>%
  filter(is.comment.line == "False") %>%
  distinct()
line.ground.truth$filename = gsub("/", "_", line.ground.truth$filename)

cat(sprintf("[Ground Truth] Loaded %d lines, covering %d test releases\n",
            nrow(line.ground.truth), length(unique(line.ground.truth$test))))

# ==================== Load GLANCE NT, NFC ====================
all_CEandNF_files = list.files(CEandNFCdir)
lineLevelMetrics = NULL
for (f in all_CEandNF_files) {
  df = read.csv(paste0(CEandNFCdir, f), stringsAsFactors = FALSE)
  df$test = str_split_fixed(f, "-result", 2)[, 1]
  lineLevelMetrics = rbind(lineLevelMetrics, df)
}

lineLevelMetrics = lineLevelMetrics %>%
  select(predicted_buggy_lines, predicted_buggy_line_numbers, predicted_buggy_score,
         rank, functioncall, controlelements, numbertokens, test)
names(lineLevelMetrics) = c("filename", "line.number", "GLANCEscore",
                             "rank", "NFC", "CE", "NT", "test")
lineLevelMetrics$filename = str_split_fixed(lineLevelMetrics$filename, ":", 2)[, 1]
lineLevelMetrics$filename = gsub("/", "_", lineLevelMetrics$filename)

cat(sprintf("[GLANCE] Loaded %d lines\n", nrow(lineLevelMetrics)))

# ==================== Load Spectrum (Only Tarantula) ====================
load.spectrum.result = function(spectrum.dir, SATname_for_mapping) {
  all_files = list.files(spectrum.dir)
  spec.df = NULL
  for (f in all_files) {
    releasename = sub("-result\\.csv$", "", f)
    df = read.csv(paste0(spectrum.dir, f), stringsAsFactors = FALSE)
    df$test = releasename
    spec.df = rbind(spec.df, df)
  }
  colnames(spec.df)[2] = "spectrum.score"
  temp = strsplit(spec.df[[1]], ":", fixed = TRUE)
  filename    = sapply(temp, function(x) paste(x[-length(x)], collapse = ":"))
  line.number = as.integer(sapply(temp, function(x) x[length(x)]))
  this.df = data.frame(
    filename       = filename,
    line.number    = line.number,
    spectrum.score = as.numeric(spec.df[["spectrum.score"]]),
    max_jaccard    = as.numeric(spec.df[["max_jaccard"]]),
    test           = spec.df[["test"]],
    stringsAsFactors = FALSE
  )
  this.df$filename = gsub("/", "_", this.df$filename)

  mapping_subdir = paste0(Mapping.dir, SATname_for_mapping, "_Dataset/LineMapping/")
  rels = unique(this.df$test)
  new.df.list = list()
  for (rel in rels) {
    sub = this.df[this.df$test == rel, , drop = FALSE]
    mapping_file = paste0(mapping_subdir, rel, "_all_files_mapping.csv")
    if (!file.exists(mapping_file)) {
      new.df.list[[length(new.df.list) + 1]] = sub
      next
    }
    mapping_df = read.csv(mapping_file, stringsAsFactors = FALSE)
    mapping_df$File = gsub("/", "_", mapping_df$File)
    matched = merge(sub, mapping_df,
                    by.x = c("filename", "line.number"),
                    by.y = c("File", "compressed_line_number"),
                    all.x = TRUE)
    matched$line.number = ifelse(is.na(matched$original_line_number),
                                 matched$line.number,
                                 matched$original_line_number)
    matched$original_line_number = NULL
    new.df.list[[length(new.df.list) + 1]] = matched
  }
  this.df = do.call(rbind, new.df.list)
  return(this.df)
}

# ==================== Load SAT Warnings ====================
load.SAT.warnings = function(SAT.result.dir, rel, SATname) {
  if (SATname == "PMD" || SATname == "ErrorProne") {
    allSATresult = read.csv(paste0(SAT.result.dir, rel, '-line-lvl-result.txt'),
                            quote = "", stringsAsFactors = FALSE)
    if (SATname != "ErrorProne") {
      allSATresult$filename = gsub("/", "_", allSATresult$filename)
    }
  } else {
    allSATresult = read.csv(paste0(SAT.result.dir, rel, '-result.csv'),
                           stringsAsFactors = FALSE)
  }

  cur.df.file = line.ground.truth %>%
    filter(test == rel) %>%
    select(filename, line.number, line.level.ground.truth)
  cur.df.file$filename = gsub("/", "_", cur.df.file$filename)

  if (SATname != "CheckStyle") {
    names(allSATresult) = c('filename', 'test.release', 'line_number',
                            'SAT_prediction_result', 'Priority')
    allSATresult$SAT_prediction_result =
      ifelse(allSATresult$SAT_prediction_result %in% c("False", "FALSE"), 0, 1)
    allSATresult = allSATresult %>% filter(SAT_prediction_result == 1)
  }

  if (SATname == "CheckStyle") {
    allSATresult = allSATresult %>% select(filename, line, priority)
    names(allSATresult) = c('filename', 'line_number', 'Priority')
  }

  SAT.base.info = allSATresult %>% select(filename, line_number, Priority)
  names(SAT.base.info) = c('filename', 'line.number', 'Priority')

  return(list(warnings = SAT.base.info, groundtruth = cur.df.file))
}

# ==================== Utility function: region classification ====================
assign_region = function(rank, N) {
  top20_thr = ceiling(N * 0.20)
  top50_thr = ceiling(N * 0.50)
  case_when(
    rank <= top20_thr       ~ "Top20",
    rank <= top50_thr       ~ "20-50",
    TRUE                    ~ "Bottom50"
  )
}

# ==================== Core: region movement analysis for each file ====================
classify.movements.per.file = function(file_data, jaccard_threshold = 0.70,
                                       lambda_T = 0.5) {
  N = nrow(file_data)
  if (N < 4) return(NULL)

  defect = as.integer(file_data$line.level.ground.truth == "True")
  if (sum(defect) == 0) return(NULL)

  file_data = file_data %>%
    mutate(
      baseScore = NFC * NT,
      specScore = spectrum.score,
      NEW3.score = log1p(baseScore) + lambda_T * log1p(specScore),
      Persistent = as.integer(!is.na(max_jaccard) & max_jaccard >= jaccard_threshold)
    )

  # APART-F ranking
  apart_f = file_data %>%
    arrange(-NFC*NT, Priority, line.number) %>%
    mutate(apart_rank = row_number()) %>%
    select(filename, line.number, apart_rank)

  # FETCH ranking
  fetch = file_data %>%
    arrange(desc(Persistent), desc(NEW3.score), Priority, line.number) %>%
    mutate(fetch_rank = row_number()) %>%
    select(filename, line.number, fetch_rank)

  # Region assignment
  ranked = apart_f %>%
    left_join(fetch, by = c("filename", "line.number")) %>%
    mutate(
      apart_region = assign_region(apart_rank, N),
      fetch_region = assign_region(fetch_rank, N)
    )

  # Movement classification
  region_level = c("Top20" = 1, "20-50" = 2, "Bottom50" = 3)
  ranked = ranked %>%
    mutate(
      apart_level = region_level[apart_region],
      fetch_level = region_level[fetch_region],
      movement = case_when(
        fetch_level < apart_level ~ "promoted",
        fetch_level > apart_level ~ "demoted",
        TRUE                     ~ "unchanged"
      )
    )

  defect_df = data.frame(filename = file_data$filename,
                         line.number = file_data$line.number,
                         is_defective = defect,
                         stringsAsFactors = FALSE)
  ranked = ranked %>%
    left_join(defect_df, by = c("filename", "line.number"))

  return(ranked)
}

# ==================== Core: calculate 12 measures ====================
compute.measures.per.file = function(ranked) {
  defect_mask = (ranked$is_defective)

  # (a) 20-50 -> Top20 (promotion)
  keep_a = (ranked$apart_region == "20-50"    & ranked$fetch_region == "Top20")
  # (b) Bottom50 -> Top20 (promotion)
  keep_b = (ranked$apart_region == "Bottom50" & ranked$fetch_region == "Top20")
  # (c) Top20 -> 20-50 (demotion)
  keep_c = (ranked$apart_region == "Top20"    & ranked$fetch_region == "20-50")
  # (d) Top20 -> Bottom50 (demotion)
  keep_d = (ranked$apart_region == "Top20"    & ranked$fetch_region == "Bottom50")

  N_a = sum(keep_a); D_a = sum(keep_a & defect_mask)
  N_b = sum(keep_b); D_b = sum(keep_b & defect_mask)
  N_c = sum(keep_c); D_c = sum(keep_c & defect_mask)
  N_d = sum(keep_d); D_d = sum(keep_d & defect_mask)

  PQ_a = if (N_a > 0) D_a / N_a * 100 else NA_real_
  PQ_b = if (N_b > 0) D_b / N_b * 100 else NA_real_
  DC_c = if (N_c > 0) D_c / N_c * 100 else NA_real_
  DC_d = if (N_d > 0) D_d / N_d * 100 else NA_real_

  data.frame(
    filename                          = ranked$filename[1],
    N_warnings                        = nrow(ranked),
    N_defect                          = sum(defect_mask),
    # --------  Direction convention:  APART-F (Apart_<region>)  →  FETCH (InFetch_<region>)  --------
    # (a) APART-F: 20-50  →  FETCH: Top20      (promotion)
    N_w_Apart_20_50_InFetch_Top20     = N_a,
    N_def_Apart_20_50_InFetch_Top20   = D_a,
    PQ_Apart_20_50_InFetch_Top20      = PQ_a,
    # (b) APART-F: Bottom50  →  FETCH: Top20  (promotion)
    N_w_Apart_Bottom50_InFetch_Top20  = N_b,
    N_def_Apart_Bottom50_InFetch_Top20 = D_b,
    PQ_Apart_Bottom50_InFetch_Top20   = PQ_b,
    # (c) APART-F: Top20  →  FETCH: 20-50     (demotion)
    N_w_Apart_Top20_InFetch_20_50     = N_c,
    N_def_Apart_Top20_InFetch_20_50   = D_c,
    DC_Apart_Top20_InFetch_20_50      = DC_c,
    # (d) APART-F: Top20  →  FETCH: Bottom50  (demotion)
    N_w_Apart_Top20_InFetch_Bottom50  = N_d,
    N_def_Apart_Top20_InFetch_Bottom50 = D_d,
    DC_Apart_Top20_InFetch_Bottom50   = DC_d,
    stringsAsFactors                  = FALSE
  )
}

# ==================== Configuration Section ====================
all_eval_releases = c('activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0',
                      'camel-2.10.0', 'camel-2.11.0',
                      'derby-10.5.1.1',
                      'groovy-1_6_BETA_2',
                      'hbase-0.95.2',
                      'hive-0.12.0',
                      'jruby-1.5.0', 'jruby-1.7.0.preview1',
                      'lucene-3.0.0', 'lucene-3.1', 'wicket-1.5.3')

sat.tools = list(
  list(name = "PMD",        dir = PMD.result.dir,        threshold = 0.70),
  list(name = "CheckStyle", dir = CheckStyle.result.dir, threshold = 0.70),
  list(name = "ErrorProne", dir = ErrorProne.result.dir, threshold = 0.70),
  list(name = "Spotbugs",   dir = Spotbugs.result.dir,   threshold = 0.70),
  list(name = "Codacy",     dir = Codacy.result.dir,     threshold = 0.70),
  list(name = "Codeql",     dir = Codeql.result.dir,     threshold = 0.70),
  list(name = "Sonarqube",  dir = Sonarqube.result.dir,  threshold = 0.70)
)

# ==================== Main Loop ====================
cat("\n========== Region Movement: APART-F vs FETCH (Tarantula only) ==========\n")
cat("Spectrum = Tarantula | 7 SAT x 14 release = 98 combinations\n")
cat("lambda_T = per-release (source: Auto_lambda_Per_release_Results.txt)\n\n")

all.movements = NULL
file_counter  = 0

for (tool in sat.tools) {
  cat(sprintf("===== %s + Tarantula =====\n", tool$name))

  spec.dir = get.spectrum.dir(tool$name)
  if (!dir.exists(spec.dir)) {
    cat(sprintf("  [Skip] spectrum dir does not exist: %s\n", spec.dir))
    next
  }
  spec.df = load.spectrum.result(spec.dir, SATname_for_mapping = tool$name)

  for (rel in all_eval_releases) {
    lambda_T = get_lambda_for_release(tool$name, "Tarantula", rel)

    sat_data     = load.SAT.warnings(tool$dir, rel, tool$name)
    sat.warnings = sat_data$warnings
    gt           = sat_data$groundtruth

    joined = sat.warnings %>%
      left_join(
        lineLevelMetrics %>% filter(test == rel) %>%
          select(filename, line.number, NT, NFC),
        by = c("filename", "line.number")
      ) %>% mutate(
        NT  = replace_na(NT,  0),
        NFC = replace_na(NFC, 0)
      ) %>%
      left_join(
        spec.df %>% filter(test == rel) %>%
          select(filename, line.number, spectrum.score, max_jaccard),
        by = c("filename", "line.number")
      ) %>% mutate(
        spectrum.score = replace_na(spectrum.score, 0),
        max_jaccard    = replace_na(max_jaccard,    0)
      ) %>%
      inner_join(
        gt %>% select(filename, line.number, line.level.ground.truth),
        by = c("filename", "line.number")
      )

    files = unique(joined$filename)
    rel_rows = NULL

    for (f in files) {
      file_data = joined[joined$filename == f, , drop = FALSE]
      ranked = classify.movements.per.file(file_data,
                                           jaccard_threshold = tool$threshold,
                                           lambda_T = lambda_T)
      if (is.null(ranked)) next
      measures = compute.measures.per.file(ranked)
      measures$SAT  = tool$name
      measures$test = rel
      rel_rows = rbind(rel_rows, measures)
      file_counter = file_counter + 1
    }

    if (!is.null(rel_rows) && nrow(rel_rows) > 0) {
      all.movements = rbind(all.movements, rel_rows)
      cat(sprintf("  %s: %d files produced movements\n", rel, nrow(rel_rows)))
    } else {
      cat(sprintf("  %s: No files meet the condition (defect < 1 or n < 4)\n", rel))
    }
  }
}

cat(sprintf("\n[Summary] %d combinations (filename, test, SAT) produced movements\n", file_counter))

# ==================== Output ====================
if (is.null(all.movements) || nrow(all.movements) == 0) {
  cat("\n[No results produced]\n")
} else {
  cat("\n========== Secondary aggregation (release pooled ratio → SAT macro-mean) ==========\n")

  # ==================== Column definitions ====================
  # Count columns (counts) — SUM across files in release
  count_cols = c("N_warnings", "N_defect",
                 # (a) promotion:  APART 20-50  →  FETCH Top20
                 "N_w_Apart_20_50_InFetch_Top20",    "N_def_Apart_20_50_InFetch_Top20",
                 # (b) promotion:  APART Bottom50  →  FETCH Top20
                 "N_w_Apart_Bottom50_InFetch_Top20",  "N_def_Apart_Bottom50_InFetch_Top20",
                 # (c) demotion:   APART Top20  →  FETCH 20-50
                 "N_w_Apart_Top20_InFetch_20_50",    "N_def_Apart_Top20_InFetch_20_50",
                 # (d) demotion:   APART Top20  →  FETCH Bottom50
                 "N_w_Apart_Top20_InFetch_Bottom50",  "N_def_Apart_Top20_InFetch_Bottom50")

  # Ratio columns (PQ / DC) — ΣD_f / ΣN_f  in release (same as PQ/DC definition)
  ratio_specs = list(
    list(ratio = "PQ_Apart_20_50_InFetch_Top20",
         D     = "N_def_Apart_20_50_InFetch_Top20",
         N     = "N_w_Apart_20_50_InFetch_Top20"),
    list(ratio = "PQ_Apart_Bottom50_InFetch_Top20",
         D     = "N_def_Apart_Bottom50_InFetch_Top20",
         N     = "N_w_Apart_Bottom50_InFetch_Top20"),
    list(ratio = "DC_Apart_Top20_InFetch_20_50",
         D     = "N_def_Apart_Top20_InFetch_20_50",
         N     = "N_w_Apart_Top20_InFetch_20_50"),
    list(ratio = "DC_Apart_Top20_InFetch_Bottom50",
         D     = "N_def_Apart_Top20_InFetch_Bottom50",
         N     = "N_w_Apart_Top20_InFetch_Bottom50")
  )
  ratio_cols = sapply(ratio_specs, function(s) s$ratio)

  # ==================== Proportion class: overall Top20进出 ====================
  # All new entries into Top20:  来自 20-50 + Bottom50
  overall_specs = list(
    list(ratio = "PQ_OverallTop20",          # Promotion Quality for overall Top20
         D     = c("N_def_Apart_20_50_InFetch_Top20",    "N_def_Apart_Bottom50_InFetch_Top20"),
         N     = c("N_w_Apart_20_50_InFetch_Top20",      "N_w_Apart_Bottom50_InFetch_Top20")),
    list(ratio = "DC_OverallTop20",          # Damage-Control for overall Top20
         D     = c("N_def_Apart_Top20_InFetch_20_50",    "N_def_Apart_Top20_InFetch_Bottom50"),
         N     = c("N_w_Apart_Top20_InFetch_20_50",      "N_w_Apart_Top20_InFetch_Bottom50"))
  )
  overall_cols = c("PQ_OverallTop20", "DC_OverallTop20")

  # Step 1: across files in release —— counts use SUM, ratios use ΣD/ΣN
  rel_pooled = all.movements %>%
    group_by(SAT, test) %>%
    summarise(
      across(all_of(count_cols), ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # Calculate release-level ratios
  for (s in ratio_specs) {
    D_sum = rowSums(rel_pooled[, s$D, drop = FALSE])
    N_sum = rowSums(rel_pooled[, s$N, drop = FALSE])
    rel_pooled[[s$ratio]] = ifelse(N_sum > 0, D_sum / N_sum * 100, NA_real_)
  }

  # Overall Top20 PQ/DC (release-level)
  for (s in overall_specs) {
    D_sum = rowSums(rel_pooled[, s$D, drop = FALSE])
    N_sum = rowSums(rel_pooled[, s$N, drop = FALSE])
    rel_pooled[[s$ratio]] = ifelse(N_sum > 0, D_sum / N_sum * 100, NA_real_)
  }

  # Step 2: across releases in SAT —— counts use SUM, ratios use macro-mean
  final_df = rel_pooled %>%
    group_by(SAT) %>%
    summarise(
      across(all_of(count_cols),   ~sum(.x, na.rm = TRUE)),
      across(all_of(ratio_cols),   ~mean(.x, na.rm = TRUE)),
      across(all_of(overall_cols), ~mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # ==================== Format output ====================
  pct_cols = c(ratio_cols, overall_cols)
  fmt_value = function(x, col) {
    if (col %in% pct_cols) {
      ifelse(is.na(x), "N/A", sprintf("%.2f%%", x))
    } else {
      ifelse(is.na(x), "N/A", sprintf("%.2f", x))
    }
  }
  format_df = function(df) {
    for (col in c(count_cols, ratio_cols, overall_cols)) df[[col]] = fmt_value(df[[col]], col)
    df
  }

  all_cols = c(count_cols, ratio_cols, overall_cols)
  final_fmt = final_df %>% arrange(factor(SAT, levels = sat_order)) %>% format_df()
  write.csv(final_fmt[, c('SAT', all_cols)], agg_csv, row.names = FALSE)
  cat(sprintf("  [Output] %s  (%d lines)\n", agg_csv, nrow(final_fmt)))

  # Print the most important summary number: Overall Top20 PQ vs DC
  cat("\n[Main result]  Overall Top20 PQ vs DC  (release-pooled → SAT macro-mean)\n")
  for (i in seq_len(nrow(final_df))) {
    sat = final_df$SAT[i]
    pq  = final_df$PQ_OverallTop20[i]
    dc  = final_df$DC_OverallTop20[i]
    verdict = if (is.na(pq) || is.na(dc)) "n/a"
              else if (pq > dc) "PQ > DC"
              else "PQ < DC"
    cat(sprintf("  %-12s  PQ=%7.2f%%   DC=%7.2f%%   %s\n", sat, pq, dc, verdict))
  }
}

cat("\n========== Experiment completed ==========\n")
