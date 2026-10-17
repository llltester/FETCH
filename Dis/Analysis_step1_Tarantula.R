# =====================================================================
# Analysis_step1_Tarantula.R - 2026-09-08
# ---------------------------------------------------------------------
# FDR Analysis using only Tarantula spectrum
#
# Assign Q / HDE / EP labels to each warning line and output detailed information for each line.
#
# Three-dimensional partition:
#   - Q (1-4): NT*NFC sorted in descending order, rank quartiles within each file → "Q1"/"Q2"/"Q3"/"Q4"
#   - EP (0/1): is_persistent = (max_jaccard >= threshold) → 0/1
#   - HDE (high/low): spectrum.score >= per-file mean → "high"/"low"
#
# Output:
#   M2_macro_average_7SAT.csv  : 7-SAT weighted macro average summary (Q × Base/EP/HDE)
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

output.dir  = './'
if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)

# ==================== Spectrum: Only Tarantula ====================
get.spectrum.dir = function(sat.name) {
  paste0(BASE, 'SATs_result/',
         sat.name, '/Result/TarantulaWithoutCA/line_result/test/')
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

# ==================== Assign Q/HDE/EP labels to each line ====================
# Logic:
#   - Sort by score(NT*NFC) in descending order, rank quartiles within each file → Q1/Q2/Q3/Q4
#   - spectrum.score >= per-file median → "high", otherwise → "low"
#   - EP = 1 if max_jaccard >= threshold else 0
# Output: filename, test, SAT, line.number, Q, HDE, EP, defect, FDR_bucket
assign.labels.per.file = function(joined_df, jaccard_threshold = 0.70) {
  joined_df$is_persistent = !is.na(joined_df$max_jaccard) &
                            joined_df$max_jaccard >= jaccard_threshold
  joined_df$EP     = as.integer(joined_df$is_persistent)
  joined_df$score  = joined_df$NT * joined_df$NFC
  joined_df$defect = as.integer(joined_df$line.level.ground.truth == "True")

  files = unique(joined_df$filename)
  all.rows = list()

  for (f in files) {
    file_data = joined_df[joined_df$filename == f, , drop = FALSE]

    # Filter: at least 1 defect and at least 4 lines
    if (sum(file_data$defect) == 0) next
    n_total = nrow(file_data)
    if (n_total < 4) next

    # ---- Split into Q ----
    file_data = file_data[order(-file_data$score), , drop = FALSE]
    q1_end = floor(n_total / 4)
    q2_end = floor(n_total / 2)
    q3_end = floor(3 * n_total / 4)
    Q_vec = rep("Q4", n_total)
    if (q1_end > 0)            Q_vec[1:q1_end]           = "Q1"
    if (q2_end > q1_end)       Q_vec[(q1_end + 1):q2_end] = "Q2"
    if (q3_end > q2_end)       Q_vec[(q2_end + 1):q3_end] = "Q3"
    file_data$Q = Q_vec

    # ---- HDE ----
    hde_mean = mean(file_data$spectrum.score, na.rm = TRUE)
    if (is.na(hde_mean)) next
    file_data$HDE = ifelse(file_data$spectrum.score >= hde_mean, "high", "low")


    line.rows = data.frame(
      filename      = file_data$filename,
      line.number  = file_data$line.number,
      score        = file_data$score,
      spectrum.score = file_data$spectrum.score,
      Q            = file_data$Q,
      HDE          = file_data$HDE,
      EP           = as.integer(file_data$EP == 1L),   
      defect       = ifelse(file_data$defect == 1L, "True", "False"),
      stringsAsFactors = FALSE
    )

    # ---- Pre-calculate: N and D for each (Q) / (Q, EP) / (Q, HDE) sub-bucket ----
    defect_vec = file_data$defect
    Q_vec      = file_data$Q
    EP_vec     = file_data$EP
    HDE_vec    = file_data$HDE

    bucket_ND = function(mask) {
      n = sum(mask)
      d = sum(defect_vec[mask])
      c(n = n, d = d)
    }

    nd_list = list()
    for (q in c("Q1","Q2","Q3","Q4")) {
      nd_list[[paste0("Q_", q)]]          = bucket_ND(Q_vec == q)
      nd_list[[paste0("Q", q, "_EP0")]]    = bucket_ND(Q_vec == q & EP_vec == 0L)
      nd_list[[paste0("Q", q, "_EP1")]]    = bucket_ND(Q_vec == q & EP_vec == 1L)
      nd_list[[paste0("Q", q, "_HDEhigh")]] = bucket_ND(Q_vec == q & HDE_vec == "high")
      nd_list[[paste0("Q", q, "_HDElow")]]  = bucket_ND(Q_vec == q & HDE_vec == "low")
    }


    line.rows$N_Q = sapply(line.rows$Q, function(q) nd_list[[paste0("Q_", q)]][1])
    line.rows$D_Q = sapply(line.rows$Q, function(q) nd_list[[paste0("Q_", q)]][2])

    line.rows$N_Q_EP0 = sapply(seq_len(nrow(line.rows)), function(i)
      nd_list[[paste0("Q", line.rows$Q[i], "_EP0")]][1])
    line.rows$D_Q_EP0 = sapply(seq_len(nrow(line.rows)), function(i)
      nd_list[[paste0("Q", line.rows$Q[i], "_EP0")]][2])
    line.rows$N_Q_EP1 = sapply(seq_len(nrow(line.rows)), function(i)
      nd_list[[paste0("Q", line.rows$Q[i], "_EP1")]][1])
    line.rows$D_Q_EP1 = sapply(seq_len(nrow(line.rows)), function(i)
      nd_list[[paste0("Q", line.rows$Q[i], "_EP1")]][2])

    line.rows$N_Q_High = sapply(seq_len(nrow(line.rows)), function(i)
      nd_list[[paste0("Q", line.rows$Q[i], "_HDEhigh")]][1])
    line.rows$D_Q_High = sapply(seq_len(nrow(line.rows)), function(i)
      nd_list[[paste0("Q", line.rows$Q[i], "_HDEhigh")]][2])
    line.rows$N_Q_Low = sapply(seq_len(nrow(line.rows)), function(i)
      nd_list[[paste0("Q", line.rows$Q[i], "_HDElow")]][1])
    line.rows$D_Q_Low = sapply(seq_len(nrow(line.rows)), function(i)
      nd_list[[paste0("Q", line.rows$Q[i], "_HDElow")]][2])

    all.rows[[length(all.rows) + 1]] = line.rows
  }

  if (length(all.rows) == 0) return(NULL)
  return(do.call(rbind, all.rows))
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
cat("\n========== FDR Analysis Per File (Tarantula only) ==========\n")
cat("Spectrum = Tarantula | 7 SAT × 14 release = 98 combinations\n\n")

all.FDR.results = NULL

for (tool in sat.tools) {
  cat(sprintf("===== %s + Tarantula =====\n", tool$name))

  spec.dir = get.spectrum.dir(tool$name)
  if (!dir.exists(spec.dir)) {
    cat(sprintf("  [Skip] spectrum dir does not exist: %s\n", spec.dir))
    next
  }
  spec.df = load.spectrum.result(spec.dir, SATname_for_mapping = tool$name)

  for (rel in all_eval_releases) {
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

    labeled_rows = assign.labels.per.file(joined, jaccard_threshold = tool$threshold)

    if (!is.null(labeled_rows)) {
      labeled_rows$SAT  = tool$name
      labeled_rows$test = rel
      all.FDR.results = rbind(all.FDR.results, labeled_rows)
      cat(sprintf("  %s: %d lines labeled\n", rel, nrow(labeled_rows)))
    } else {
      cat(sprintf("  %s: No files meet the condition (defect < 1 or n < 4)\n", rel))
    }
  }
}

# ==================== Output ====================
# Only output M2_macro_average_7SAT.csv (7-SAT weighted macro average summary)
# Output columns: APART-F risk stratum | Base FDR | EP=1 | EP=0 | ΔEP | HDE-High | HDE-Low | ΔHDE
if (!is.null(all.FDR.results) && nrow(all.FDR.results) > 0) {
  cat("\n========== Summary: 7-SAT weighted macro-average ==========\n")

  # -------- (SAT, Q) File-level FDR → release-level → SAT-level --------
  m2_agg_Q = all.FDR.results %>%
    distinct(filename, test, SAT, Q, .keep_all = TRUE) %>%
    transmute(filename, test, SAT, Q,
              FDR_file = ifelse(N_Q > 0, D_Q / N_Q, NA_real_)) %>%
    group_by(SAT, test, Q) %>%
    summarise(FDR_release = mean(FDR_file, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(FDR_release)) %>%
    group_by(SAT, Q) %>%
    summarise(N_releases = n(),
              FDR_Q      = mean(FDR_release, na.rm = TRUE) * 100,
              .groups    = "drop")

  # -------- (SAT, Q, EP) File-level FDR → release-level → SAT-level --------
  m2_ep_df = all.FDR.results %>%
    distinct(filename, test, SAT, Q, EP, .keep_all = TRUE) %>%
    transmute(filename, test, SAT, Q, EP,
              N_Q_EP = ifelse(EP == 1, N_Q_EP1, N_Q_EP0),
              D_Q_EP = ifelse(EP == 1, D_Q_EP1, D_Q_EP0),
              FDR_file = ifelse(N_Q_EP > 0, D_Q_EP / N_Q_EP, NA_real_)) %>%
    group_by(SAT, test, Q, EP) %>%
    summarise(FDR_release = mean(FDR_file, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(FDR_release)) %>%
    group_by(SAT, Q, EP) %>%
    summarise(N_releases = n(),
              FDR_Q_EP   = mean(FDR_release, na.rm = TRUE) * 100,
              .groups    = "drop")

  # -------- (SAT, Q, HDE) File-level FDR → release-level → SAT-level --------
  m2_hde_df = all.FDR.results %>%
    distinct(filename, test, SAT, Q, HDE, .keep_all = TRUE) %>%
    transmute(filename, test, SAT, Q, HDE,
              N_Q_HDE  = ifelse(HDE == "high", N_Q_High, N_Q_Low),
              D_Q_HDE  = ifelse(HDE == "high", D_Q_High, D_Q_Low),
              FDR_file = ifelse(N_Q_HDE > 0, D_Q_HDE / N_Q_HDE, NA_real_)) %>%
    group_by(SAT, test, Q, HDE) %>%
    summarise(FDR_release = mean(FDR_file, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(FDR_release)) %>%
    group_by(SAT, Q, HDE) %>%
    summarise(N_releases = n(),
              FDR_Q_HDE  = mean(FDR_release, na.rm = TRUE) * 100,
              .groups    = "drop")

  # Base FDR / EP / HDE 7-SAT weighted macro average
  base_macro = m2_agg_Q %>%
    group_by(Q) %>%
    summarise(`Base FDR` = mean(FDR_Q), .groups = "drop")

  ep_macro = m2_ep_df %>%
    group_by(Q, EP) %>%
    summarise(FDR = mean(FDR_Q_EP), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = EP, values_from = FDR, names_prefix = "EP_") %>%
    rename(`EP=1` = `EP_1`, `EP=0` = `EP_0`) %>%
    mutate(`ΔEP` = `EP=1` - `EP=0`)

  hde_macro = m2_hde_df %>%
    group_by(Q, HDE) %>%
    summarise(FDR = mean(FDR_Q_HDE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = HDE, values_from = FDR) %>%
    rename(`HDE-High` = high, `HDE-Low` = low) %>%
    mutate(`ΔHDE` = `HDE-High` - `HDE-Low`)

  macro_summary = base_macro %>%
    left_join(ep_macro,  by = "Q") %>%
    left_join(hde_macro, by = "Q") %>%
    mutate(across(-Q, ~ sprintf("%.2f%%", .))) %>%
    rename(`APART-F risk stratum` = Q) %>%
    select(`APART-F risk stratum`,
           `Base FDR`,
           `EP=1`, `EP=0`, `ΔEP`,
           `HDE-High`, `HDE-Low`, `ΔHDE`)

  out_path_macro = paste0(output.dir, 'Q1_Q4_Risk_stratum_FDR.csv')
  write.csv(macro_summary, out_path_macro, row.names = FALSE)
  cat(sprintf("  [Output] %s  (%d lines × %d columns)\n",
              out_path_macro, nrow(macro_summary), ncol(macro_summary)))
} else {
  cat("\n[No results produced]\n")
}

cat("\n========== Experiment Completed ==========\n")
