# =====================================================================
# Analysis_step2_Tarantula.R - 2026-09-07
# ---------------------------------------------------------------------
# FDR Analysis using only Tarantula spectrum
#
# Calculate the EP × HDE 2×2 FDR table for each (filename, test release, SAT) combination.
# Spectrum = Tarantula (other 4 spectra have been removed)
#
# Two-dimensional partition:
#   - EP (0/1): is_persistent = (max_jaccard >= threshold)
#   - HDE (high/low): spectrum.score >= per-file mean → "high"/"low"
#
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
if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)


sat_order = c("PMD", "CheckStyle", "ErrorProne", "Spotbugs",
              "Codacy", "Codeql", "sonarqube")

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

# ==================== Calculate FDR (per file, 2×2 EP × HDE) ====================
compute.FDR.2x2.per.file = function(joined_df, jaccard_threshold = 0.70) {
  joined_df$EP     = as.integer(!is.na(joined_df$max_jaccard) &
                                joined_df$max_jaccard >= jaccard_threshold)
  joined_df$defect = as.integer(joined_df$line.level.ground.truth == "True")

  files = unique(joined_df$filename)
  results = list()

  for (f in files) {
    file_data = joined_df[joined_df$filename == f, , drop = FALSE]

    if (sum(file_data$defect) == 0) next

    n_total = nrow(file_data)
    if (n_total < 4) next

    hde_mean = mean(file_data$spectrum.score, na.rm = TRUE)
    if (is.na(hde_mean)) next
    HDE_high = as.integer(file_data$spectrum.score >= hde_mean)

    defect = file_data$defect
    EP     = file_data$EP

    fdr_cell = function(mask) {
      n = sum(mask)
      if (n == 0) return("")
      sprintf("%.2f%%", sum(defect[mask]) / n * 100)
    }

    row = data.frame(
      filename      = f,
      High_HDE_EP1 = fdr_cell(EP == 1L & HDE_high == 1L),  # A
      Low_HDE_EP1  = fdr_cell(EP == 1L & HDE_high == 0L),  # B
      High_HDE_EP0 = fdr_cell(EP == 0L & HDE_high == 1L),  # C
      Low_HDE_EP0  = fdr_cell(EP == 0L & HDE_high == 0L),  # D
      stringsAsFactors = FALSE
    )
    results[[length(results) + 1]] = row
  }

  if (length(results) == 0) return(NULL)
  return(do.call(rbind, results))
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
cat("\n========== EP × HDE 2×2 FDR (Tarantula only) ==========\n")
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

    file_FDRs = compute.FDR.2x2.per.file(joined, jaccard_threshold = tool$threshold)

    if (!is.null(file_FDRs)) {
      file_FDRs$SAT  = tool$name
      file_FDRs$test = rel
      all.FDR.results = rbind(all.FDR.results, file_FDRs)
      cat(sprintf("  %s: %d files produced FDR\n", rel, nrow(file_FDRs)))
    } else {
      cat(sprintf("  %s: No files meet the condition (defect < 1 or n < 4)\n", rel))
    }
  }
}

# ==================== Output ====================
# output EPxHDE_FDR.csv (7 SAT × 4 cells FDR)
agg_csv    = paste0(output.dir, 'EPxHDE_FDR.csv')
if (!is.null(all.FDR.results) && nrow(all.FDR.results) > 0) {
  col_order = c("filename", "test", "SAT",
                "High_HDE_EP1", "Low_HDE_EP1", "High_HDE_EP0", "Low_HDE_EP0")
  all.FDR.results = all.FDR.results[, col_order]
  all.FDR.results = all.FDR.results %>% arrange(factor(SAT, levels = sat_order), test, filename)

  cat(sprintf("\n[Completed] %d combinations (filename, test, SAT)\n",
              nrow(all.FDR.results)))

  # ==================== Secondary aggregation: release mean × SAT mean ====================
  cat("\n========== Secondary aggregation (release mean × SAT mean) ==========\n")

  fdr_cells = c("High_HDE_EP1", "Low_HDE_EP1",
                "High_HDE_EP0", "Low_HDE_EP0")

  to_num = function(s) {
    s = as.character(s)
    s = sub('%', '', s, fixed = TRUE)
    suppressWarnings(as.numeric(s))
  }
  to_pct = function(x) ifelse(is.na(x), '', sprintf('%.2f%%', x))

  rel_mean = all.FDR.results %>%
    group_by(SAT, test) %>%
    summarise(across(all_of(fdr_cells), ~mean(to_num(.x), na.rm = TRUE)),
              .groups = "drop")

  final_df = rel_mean %>%
    group_by(SAT) %>%
    summarise(across(all_of(fdr_cells), ~mean(.x, na.rm = TRUE)),
              .groups = "drop")

  final_fmt = final_df %>%
    mutate(across(all_of(fdr_cells), to_pct)) %>%
    arrange(factor(SAT, levels = sat_order))

  write.csv(final_fmt[, c('SAT', fdr_cells)], agg_csv, row.names = FALSE)
  cat(sprintf("  [Output] %s  (%d lines)\n", agg_csv, nrow(final_fmt)))
} else {
  cat("\n[No results produced]\n")
}

cat("\n========== Experiment Completed ==========\n")
