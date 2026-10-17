# =============================================================================
# RQ3-B: 5 spectrum Sensitivity Analysis (Per-SAT MeanRho Per-Release λ)
# =============================================================================

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

# ==================== Path Configuration ====================
BASE = '../'

SAT_DIR = paste0(BASE, 'SATs_initial_scan_result/')

PMD.result.dir = paste0(SAT_DIR, 'PMD/')
CheckStyle.result.dir = paste0(SAT_DIR, 'CheckStyle/')
ErrorProne.result.dir = paste0(SAT_DIR, 'Errorprone/')   
Spotbugs.result.dir = paste0(SAT_DIR, 'Spotbugs/')
codacy.result.dir = paste0(SAT_DIR, 'codacy/')
codeql.result.dir = paste0(SAT_DIR, 'codeql/')
sonarqube.result.dir = paste0(SAT_DIR, 'sonarqube/')

Mapping.dir = paste0(BASE, 'datasets/map-warning-lines-only/')

prediction_dir = paste0(BASE, 'Baseline/DeepLineDP/output/prediction/DeepLineDP/within-release/')
CEandNFCdir = paste0(BASE, 'Baseline/GLANCE_and_LineDP/result/Glance_MD_full_threshold/line_result/test/')

# Output path
RQ3B.save.dir = paste0(BASE, 'RQ3/B_different_Spectrum/')
RQ3B.txt.path = paste0(RQ3B.save.dir, 'RQ3B_Spectrum_Sensitivity.txt')
RQ3B.csv.path = paste0(RQ3B.save.dir, 'RQ3B_RankDiff.csv')

if (!dir.exists(RQ3B.save.dir)) dir.create(RQ3B.save.dir, recursive = TRUE)

# ==================== Data Loading  ====================
all_files = list.files(prediction_dir)
df_all <- NULL
for(f in all_files)
{
  df <- read.csv(paste0(prediction_dir, f))
  df_all <- rbind(df_all, df)
}

line.ground.truth = select(df_all,  project, train, test, filename, file.level.ground.truth,
                           prediction.prob, line.number, line.level.ground.truth, is.comment.line)
line.ground.truth = filter(line.ground.truth, is.comment.line== "False")
line.ground.truth = distinct(line.ground.truth)

all_CEandNF_files = list.files(CEandNFCdir)
lineLevelMetrics <- NULL
for(f in all_CEandNF_files)
{
  df <- read.csv(paste0(CEandNFCdir, f))
  df$test = str_split_fixed(f, "-result", 2)[,1]
  lineLevelMetrics  <- rbind(lineLevelMetrics, df)
}

lineLevelMetrics = select(lineLevelMetrics, "predicted_buggy_lines", "predicted_buggy_line_numbers",
                          "predicted_buggy_score", "rank", "functioncall", "controlelements",
                          "numbertokens", "test")
names(lineLevelMetrics) = c("filename", "line.number", "GLANCEscore", "rank", "NFC", "CE", "NT", "test")
lineLevelMetrics$filename = str_split_fixed(lineLevelMetrics$filename, ":", 2)[,1]
lineLevelMetrics$filename <- gsub("/", "_", lineLevelMetrics$filename)

# ==================== Spectrum Data Configuration ====================
spectrum.subdirs = list(
  Barinel   = "BarinelwithoutCA",
  Dstar     = "DstarWithoutCA",
  Ochiai    = "OchiaiWithoutCA",
  Op2       = "Op2WithoutCA",
  Tarantula = "TarantulaWithoutCA"
)

get.spectrum.dir = function(sat.name, spec.label) {
  paste0(BASE, "SATs_result/",
         sat.name, "/Result/",
         spectrum.subdirs[[spec.label]], "/line_result/test/")
}

load.spectrum.result = function(spectrum.dir, use.mapping = FALSE, SATname_for_mapping = NULL) {
  all_files = list.files(spectrum.dir)
  if (length(all_files) == 0) {
    warning(sprintf('[spectrum] Directory is empty or does not exist: %s -> return empty df', spectrum.dir))
    return(data.frame(filename = character(0),
                      line.number = integer(0),
                      spectrum.score = numeric(0),
                      max_jaccard = numeric(0),
                      test = character(0),
                      stringsAsFactors = FALSE))
  }
  spec.df = NULL
  for (f in all_files) {
    releasename <- sub("-result\\.csv$", "", f)
    df <- read.csv(paste0(spectrum.dir, f), stringsAsFactors = FALSE)
    df$test <- releasename
    spec.df <- rbind(spec.df, df)
  }
  colnames(spec.df)[2] <- "spectrum.score"
  temp <- strsplit(spec.df[[1]], ":", fixed = TRUE)
  filename <- sapply(temp, function(x) paste(x[-length(x)], collapse = ":"))
  line.number <- as.integer(sapply(temp, function(x) x[length(x)]))
  spectrum.score <- as.numeric(spec.df[["spectrum.score"]])
  max_jaccard <- as.numeric(spec.df[["max_jaccard"]])
  test <- spec.df[["test"]]
  this.df <- data.frame(filename = filename, line.number = line.number,
                        spectrum.score = spectrum.score,
                        max_jaccard = max_jaccard,
                        test = test,
                        stringsAsFactors = FALSE)
  this.df$filename <- gsub("/", "_", this.df$filename)

  if (use.mapping && !is.null(SATname_for_mapping)) {
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
      mapping_df$File <- gsub("/", "_", mapping_df$File)
      matched = merge(sub, mapping_df,
                      by.x = c("filename", "line.number"),
                      by.y = c("File", "compressed_line_number"),
                      all.x = TRUE)
      matched$line.number = ifelse(is.na(matched$original_line_number),
                                   matched$line.number,
                                   matched$original_line_number)
      matched$original_line_number <- NULL
      new.df.list[[length(new.df.list) + 1]] = matched
    }
    this.df = do.call(rbind, new.df.list)
  }

  return(this.df)
}

# ==================== Line-level Metrics Calculation  ====================
get.line.metrics.result = function(baseline.df, cur.df.file)
{
  baseline.df.with.ground.truth = merge(baseline.df, cur.df.file, by=c("filename", "line.number"))

  baseline.df.with.ground.truth = baseline.df.with.ground.truth %>% group_by(filename) %>%
    mutate(actionable.warning = ifelse(any(line.level.ground.truth == 'True'), 1, 0)) %>%
    filter(actionable.warning == 1)

  sorted = baseline.df.with.ground.truth %>% group_by(filename) %>% arrange(rank, .by_group = TRUE) %>%
    mutate(order = row_number()) %>% mutate(totalSLOC = n())

  sorted = sorted %>% filter(totalSLOC >= 5)
  filter.files = sorted %>% select(filename) %>% distinct()

  total_true = sorted %>% group_by(filename) %>%
    summarize(total_true = sum(line.level.ground.truth == "True"))
  total_true = total_true %>% filter(total_true > 0)

  FPavg = sorted %>% group_by(filename) %>%
    mutate(FPI = cumsum(line.level.ground.truth == "False"),
           total_truth = sum(line.level.ground.truth == "True")) %>%
    filter(line.level.ground.truth == "True") %>%
    mutate(S.R = sum(FPI, na.rm = TRUE)) %>%
    mutate(FPavg = round(S.R / total_truth, digits = 2)) %>%
    select(filename, FPavg) %>% distinct()
  FPavg = FPavg %>% arrange(filename)
  FPavg.list = FPavg$FPavg

  IFA = sorted %>% filter(line.level.ground.truth == "True") %>%
    group_by(filename) %>% top_n(1, -order)
  IFA = IFA %>% arrange(filename)
  ifa.list = IFA$order - 1
  total_true = total_true %>% arrange(filename)

  fpa = sorted %>% merge(total_true) %>% group_by(filename) %>%
    arrange(order, .by_group = TRUE) %>%
    mutate(lineFPA = if_else(line.level.ground.truth == 'True', n()-order+1, 0) / (n() * total_true)) %>%
    summarize(FPA = sum(lineFPA))
  fpa = fpa %>% arrange(filename)
  fpa.list = fpa$FPA

  top1 = sorted %>% merge(total_true) %>% group_by(filename) %>%
    filter(order <= 1) %>% summarize(top1 = sum(line.level.ground.truth == "True")/n())
  top1 = top1 %>% arrange(filename)
  top1.list = top1$top1

  top3 = sorted %>% merge(total_true) %>% group_by(filename) %>%
    filter(order <= 3) %>% summarize(top3 = sum(line.level.ground.truth == "True")/n())
  top3 = top3 %>% arrange(filename)
  top3.list = top3$top3

  top5 = sorted %>% merge(total_true) %>% group_by(filename) %>%
    filter(order <= 5) %>% summarize(top5 = sum(line.level.ground.truth == "True")/n())
  top5 = top5 %>% arrange(filename)
  top5.list = top5$top5

  result.df = data.frame(IFA$filename, ifa.list, fpa.list, top1.list, top3.list, top5.list, FPavg.list, IFA$totalSLOC)
  return(result.df)
}


preprocess <- function(x, reverse){
  colnames(x) <- c("variable","value")
  tmp <- do.call(cbind, split(x, x$variable))
  tmp <- tmp[, grep("value", names(tmp))]
  colnames(tmp) <- gsub("\\.value", "", colnames(tmp))
  rownames(tmp) <- NULL
  df <- tmp
  ranking <- NULL

  if (reverse == TRUE) {
    ranking <- (max(sk_esd(df)$group) - sk_esd(df)$group) + 1
  } else {
    ranking <- sk_esd(df)$group
  }

  x$rank <- paste0("R", ranking[as.character(gsub("-", ".", x$variable))])
  return(x)
}

extract.rank.for.technique = function(ifa.pre, fpavg.pre, fpa.pre, t1.pre, t3.pre, t5.pre, technique.name)
{
  get.rank = function(df, tech) {
    ranks = df$rank[df$variable == tech]
    if (length(ranks) == 0) return(NA)
    rank.tab = table(ranks)
    return(gsub("R", "", names(rank.tab)[which.max(rank.tab)]))
  }

  c(
    ifa  = get.rank(ifa.pre, technique.name),
    fpavg = get.rank(fpavg.pre, technique.name),
    fpa  = get.rank(fpa.pre, technique.name),
    top1 = get.rank(t1.pre, technique.name),
    top3 = get.rank(t3.pre, technique.name),
    top5 = get.rank(t5.pre, technique.name)
  )
}

format.rank.line = function(rank.vec) {
  paste(gsub("R", "", rank.vec), collapse = ",")
}


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

cat(sprintf("[Lambda Source] %s | Loaded %d (SAT, Spectrum) combinations\n",
            basename(lambda_file), length(per_release_lambda)))

get_lambda_for_release = function(sat_name, spectrum_name, v3_release) {
  key = paste0(sat_name, "_", spectrum_name)
  if (is.null(per_release_lambda[[key]])) {
    return(0.5)
  }
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

# =============================================================================
# NEW3 Per-Release 
# =============================================================================
get.SAT.result.per.release = function(all_eval_releases, SAT.result.dir, SATname,
                                     line.ground.truth, lineLevelMetrics,
                                     spectrum.result.df,
                                     spectrum_name,
                                     jaccard_threshold = 0.7)
{
  SAT.F.NEW3.result.df = NULL
  SAT_F.result.df = NULL

  for(rel in all_eval_releases)
  {
    lambda_T = get_lambda_for_release(SATname, spectrum_name, rel)

    if (SATname == "PMD" || SATname == "ErrorProne" ) {
      allSATresult = read.csv(paste0(SAT.result.dir, rel, '-line-lvl-result.txt'), quote = "")
      if (SATname != "ErrorProne") {
        allSATresult$filename <- gsub("/", "_", allSATresult$filename)
      }
    } else {
      allSATresult = read.csv(paste0(SAT.result.dir, rel, '-result.csv'))
    }

    cur.df.file = filter(line.ground.truth, test==rel)
    cur.df.file = select(cur.df.file, filename, line.number, line.level.ground.truth)
    cur.df.file$filename <- gsub("/", "_", cur.df.file$filename)

    if (SATname != "CheckStyle" ){
      names(allSATresult) = c('filename','test.release','line_number', 'SAT_prediction_result', 'Priority')
      allSATresult$SAT_prediction_result <- ifelse(allSATresult$SAT_prediction_result %in% c("False", "FALSE"), 0, 1)
      allSATresult = allSATresult %>% filter(SAT_prediction_result == 1)
    }

    if (SATname == "CheckStyle"){
      allSATresult = select(allSATresult, filename, line, priority)
      names(allSATresult) = c('filename', 'line_number', 'Priority')
    }

    SAT.base.info = select(allSATresult, filename, line_number, Priority)
    names(SAT.base.info) = c('filename','line.number','Priority')

    SAT_F.result = SAT.base.info
    GLANCE_F = select(lineLevelMetrics, test, filename, line.number, NT, NFC) %>% filter(test == rel)
    GLANCE_F = select(GLANCE_F, filename, line.number, NT, NFC)
    SAT_F.result = left_join(SAT_F.result, GLANCE_F, by=c('filename', 'line.number')) %>% mutate(
      NT = replace_na(NT, 0),
      NFC = replace_na(NFC, 0))

    SAT_F.result = SAT_F.result %>% group_by(filename) %>%
      arrange(-NFC*NT, Priority, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    SAT_F.result = select(SAT_F.result, filename, line.number, rank)

    spectrum.result.thresholded = spectrum.result.df
    if ("max_jaccard" %in% names(spectrum.result.thresholded)) {
      spectrum.result.thresholded$is_persistent = spectrum.result.thresholded$max_jaccard >= jaccard_threshold
    } else {
      spectrum.result.thresholded$is_persistent = FALSE
    }
    spectrum.result.thresholded$Persistent = ifelse(spectrum.result.thresholded$is_persistent, 1, 0)

    SAT.F.NEW3.result = SAT.base.info
    SAT.F.NEW3.result = left_join(SAT.F.NEW3.result, GLANCE_F, by=c('filename', 'line.number')) %>% mutate(
      NT = replace_na(NT, 0),
      NFC = replace_na(NFC, 0))

    spectrum.result = spectrum.result.thresholded %>% filter(test == rel)
    SAT.F.NEW3.result = left_join(SAT.F.NEW3.result, spectrum.result, by=c('filename', 'line.number')) %>% mutate(
      spectrum.score = replace_na(spectrum.score, 0),
      is_persistent = replace_na(is_persistent, FALSE),
      Persistent = replace_na(Persistent, 0),
      max_jaccard = replace_na(max_jaccard, 0))

    baseScore = SAT.F.NEW3.result$NFC * SAT.F.NEW3.result$NT
    specScore = SAT.F.NEW3.result$spectrum.score
    SAT.F.NEW3.result$NEW3.score = log1p(baseScore) + lambda_T * log1p(specScore)

    SAT.F.NEW3.result = SAT.F.NEW3.result %>%
      group_by(filename) %>%
      arrange(desc(Persistent), desc(NEW3.score), Priority, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>%
      ungroup()
    SAT.F.NEW3.result = select(SAT.F.NEW3.result, filename, line.number, rank)

    SAT.F.NEW3.eval.result = get.line.metrics.result(SAT.F.NEW3.result, cur.df.file) %>% mutate(test=rel)
    SAT_F_eval.result = get.line.metrics.result(SAT_F.result, cur.df.file) %>% mutate(test=rel)

    SAT.F.NEW3.result.df = rbind(SAT.F.NEW3.result.df, SAT.F.NEW3.eval.result)
    SAT_F.result.df = rbind(SAT_F.result.df, SAT_F_eval.result)
  }

  cat(sprintf("############ Finished %s (threshold=%.2f, spectrum=%s) ############\n",
              SATname, jaccard_threshold, spectrum_name))

  sum_sat_F = SAT_F.result.df %>%
    summarise(
      IFA = median(ifa.list),
      FPA = median(fpa.list),
      top1 = mean(top1.list),
      top3 = mean(top3.list),
      top5 = mean(top5.list),
      FPavg = median(FPavg.list),
      .by = test
    ) %>%
    mutate(technique = paste0(SATname, "_F"))

  sum_sat_NEW3 = SAT.F.NEW3.result.df %>%
    summarise(
      IFA = median(ifa.list),
      FPA = median(fpa.list),
      top1 = mean(top1.list),
      top3 = mean(top3.list),
      top5 = mean(top5.list),
      FPavg = median(FPavg.list),
      .by = test
    ) %>%
    mutate(technique = paste0(SATname, "_NEW3"))

  ifa_agg  = rbind(
    data.frame(technique = sum_sat_F$technique,    value = sum_sat_F$IFA),
    data.frame(technique = sum_sat_NEW3$technique, value = sum_sat_NEW3$IFA)
  )
  fpavg_agg = rbind(
    data.frame(technique = sum_sat_F$technique,    value = sum_sat_F$FPavg),
    data.frame(technique = sum_sat_NEW3$technique, value = sum_sat_NEW3$FPavg)
  )
  fpa_agg  = rbind(
    data.frame(technique = sum_sat_F$technique,    value = sum_sat_F$FPA),
    data.frame(technique = sum_sat_NEW3$technique, value = sum_sat_NEW3$FPA)
  )
  t1_agg   = rbind(
    data.frame(technique = sum_sat_F$technique,    value = sum_sat_F$top1),
    data.frame(technique = sum_sat_NEW3$technique, value = sum_sat_NEW3$top1)
  )
  t3_agg   = rbind(
    data.frame(technique = sum_sat_F$technique,    value = sum_sat_F$top3),
    data.frame(technique = sum_sat_NEW3$technique, value = sum_sat_NEW3$top3)
  )
  t5_agg   = rbind(
    data.frame(technique = sum_sat_F$technique,    value = sum_sat_F$top5),
    data.frame(technique = sum_sat_NEW3$technique, value = sum_sat_NEW3$top5)
  )

  ifa_pre  = preprocess(ifa_agg,  TRUE)
  fpavg_pre = preprocess(fpavg_agg, TRUE)
  fpa_pre  = preprocess(fpa_agg,  FALSE)
  t1_pre   = preprocess(t1_agg,   FALSE)
  t3_pre   = preprocess(t3_agg,   FALSE)
  t5_pre   = preprocess(t5_agg,   FALSE)

  new3.tech = paste0(SATname, "_NEW3")
  base.tech = paste0(SATname, "_F")
  new3.ranks = extract.rank.for.technique(ifa_pre, fpavg_pre, fpa_pre, t1_pre, t3_pre, t5_pre, new3.tech)
  base.ranks = extract.rank.for.technique(ifa_pre, fpavg_pre, fpa_pre, t1_pre, t3_pre, t5_pre, base.tech)

  rank_row = data.frame(
    SATname    = SATname,
    technique  = c(new3.tech, base.tech),
    IFA_rank   = as.numeric(c(new3.ranks["ifa"],   base.ranks["ifa"])),
    FPavg_rank = as.numeric(c(new3.ranks["fpavg"], base.ranks["fpavg"])),
    FPA_rank   = as.numeric(c(new3.ranks["fpa"],   base.ranks["fpa"])),
    Top1_rank  = as.numeric(c(new3.ranks["top1"],  base.ranks["top1"])),
    Top3_rank  = as.numeric(c(new3.ranks["top3"],  base.ranks["top3"])),
    Top5_rank  = as.numeric(c(new3.ranks["top5"],  base.ranks["top5"])),
    stringsAsFactors = FALSE
  )

  return(list(rank_row = rank_row))
}

# ==================== Test releases ====================
all_eval_releases = c('activemq-5.2.0','activemq-5.3.0','activemq-5.8.0',
                      'camel-2.10.0','camel-2.11.0',
                      'derby-10.5.1.1',
                      'groovy-1_6_BETA_2',
                      'hbase-0.95.2',
                      'hive-0.12.0',
                      'jruby-1.5.0','jruby-1.7.0.preview1',
                      'lucene-3.0.0','lucene-3.1','wicket-1.5.3')

sat.tools = list(
  list(name = "PMD",          dir = PMD.result.dir,       threshold = 0.70),
  list(name = "CheckStyle",   dir = CheckStyle.result.dir, threshold = 0.70),
  list(name = "ErrorProne",   dir = ErrorProne.result.dir, threshold = 0.70),
  list(name = "Spotbugs",     dir = Spotbugs.result.dir,   threshold = 0.70),
  list(name = "Codacy",       dir = codacy.result.dir,     threshold = 0.70),
  list(name = "Codeql",       dir = codeql.result.dir,     threshold = 0.70),
  list(name = "Sonarqube",    dir = sonarqube.result.dir,  threshold = 0.70)
)

spectrum.names = names(spectrum.subdirs)


cat("\n\n========== RQ3-B: 5 spectrum Sensitivity Analysis ==========\n")


all.spectrum.lines = c()
base_overall.list = list()   # keyed by spectrum
spec_overall.list = list()   # keyed by spectrum

for (spectrum.idx in seq_along(spectrum.subdirs)) {
  spec.label = spectrum.names[spectrum.idx]
  cat(sprintf("----- On %s (MeanRho Per-Release λ) -----\n", spec.label))

  all_rank_df = NULL

  for (tool in sat.tools) {
    spec.dir = get.spectrum.dir(tool$name, spec.label)
    cur.spectrum.df.mapped = load.spectrum.result(spec.dir, use.mapping = TRUE, SATname_for_mapping = tool$name)

    result = get.SAT.result.per.release(
      all_eval_releases, tool$dir, tool$name,
      line.ground.truth, lineLevelMetrics,
      cur.spectrum.df.mapped,
      spectrum_name = spec.label,
      jaccard_threshold = tool$threshold
    )

    all_rank_df = rbind(all_rank_df, result$rank_row)
  }

  cat("\n")


  output_lines = NULL
  for (tool_idx in seq_along(sat.tools)) {
    spec_row = all_rank_df[2 * tool_idx - 1, ]
    base_row = all_rank_df[2 * tool_idx, ]
    tool_info = sat.tools[[tool_idx]]
    line = sprintf("%s: %s-FETCH VS APART-F comparison is %s vs %s",
                   tool_info$name,
                   spec.label,
                   format.rank.line(c(IFA = spec_row$IFA_rank, fpavg = spec_row$FPavg_rank,
                                      fpa = spec_row$FPA_rank, top1 = spec_row$Top1_rank,
                                      top3 = spec_row$Top3_rank, top5 = spec_row$Top5_rank)),
                   format.rank.line(c(IFA = base_row$IFA_rank, fpavg = base_row$FPavg_rank,
                                      fpa = base_row$FPA_rank, top1 = base_row$Top1_rank,
                                      top3 = base_row$Top3_rank, top5 = base_row$Top5_rank)))
    output_lines = c(output_lines, line)
    cat(line, "\n")
  }

  # ========== overall rank:  ==========
  spec_overall = all_rank_df %>%
    filter(grepl("_NEW3$", technique)) %>%
    summarise(
      IFA_overall   = sum(IFA_rank),
      FPavg_overall = sum(FPavg_rank),
      FPA_overall   = sum(FPA_rank),
      Top1_overall  = sum(Top1_rank),
      Top3_overall  = sum(Top3_rank),
      Top5_overall  = sum(Top5_rank)
    )

  base_overall = all_rank_df %>%
    filter(grepl("_F$", technique)) %>%
    summarise(
      IFA_overall   = sum(IFA_rank),
      FPavg_overall = sum(FPavg_rank),
      FPA_overall   = sum(FPA_rank),
      Top1_overall  = sum(Top1_rank),
      Top3_overall  = sum(Top3_rank),
      Top5_overall  = sum(Top5_rank)
    )

  overall_line = sprintf("On %s, the overall rank comparison of the six metrics: %s-FETCH VS APART-F is %s vs %s",
                         spec.label, spec.label,
                         paste(spec_overall$IFA_overall, spec_overall$FPavg_overall,
                               spec_overall$FPA_overall, spec_overall$Top1_overall,
                               spec_overall$Top3_overall, spec_overall$Top5_overall, sep = ","),
                         paste(base_overall$IFA_overall, base_overall$FPavg_overall,
                               base_overall$FPA_overall, base_overall$Top1_overall,
                               base_overall$Top3_overall, base_overall$Top5_overall, sep = ","))
  cat(overall_line, "\n")

  section = c(sprintf("----- On %s (MeanRho Per-Release λ) -----", spec.label),
              "",
              output_lines,
              "",
              overall_line)
  all.spectrum.lines = c(all.spectrum.lines, section, "")

  spec_overall.list[[spec.label]] = c(
    IFA   = spec_overall$IFA_overall[1],
    FPavg = spec_overall$FPavg_overall[1],
    FPA   = spec_overall$FPA_overall[1],
    Top1  = spec_overall$Top1_overall[1],
    Top3  = spec_overall$Top3_overall[1],
    Top5  = spec_overall$Top5_overall[1]
  )
  base_overall.list[[spec.label]] = c(
    IFA   = base_overall$IFA_overall[1],
    FPavg = base_overall$FPavg_overall[1],
    FPA   = base_overall$FPA_overall[1],
    Top1  = base_overall$Top1_overall[1],
    Top3  = base_overall$Top3_overall[1],
    Top5  = base_overall$Top5_overall[1]
  )

  cat(sprintf("\n[%s 7 SAT rank comparison completed]\n\n", spec.label))
}

# =============================================================================
# Summary Table: spectrum × 6 metrics (APART-F - FETCH overall rank difference)
# =============================================================================
cat("===== On 7 SATs, the overall rank difference of APART-F and FETCH on 5 spectra (APART-F - FETCH) =====\n\n")

metric.order = c("IFA", "FPavg", "FPA", "Top1", "Top3", "Top5")

base_mat = do.call(rbind, base_overall.list)
spec_mat = do.call(rbind, spec_overall.list)

base_mat = base_mat[, metric.order, drop = FALSE]
spec_mat = spec_mat[, metric.order, drop = FALSE]
diff_mat = base_mat - spec_mat
diff.mat = diff_mat 

header.line = paste(c("spectrum", metric.order), collapse = "  ")
sep.line    = paste(rep("------", length(metric.order) + 1), collapse = " ")
cat(header.line, "\n")
cat(sep.line, "\n")

all.diff.lines = c()
for (spec in spectrum.names) {
  diff_row = diff.mat[spec, ]
  line = sprintf("%-8s %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f",
                 spec,
                 diff_row["IFA"], diff_row["FPavg"], diff_row["FPA"],
                 diff_row["Top1"], diff_row["Top3"], diff_row["Top5"])
  cat(line, "\n")
  all.diff.lines = c(all.diff.lines, line)
}

pos_count = sum(diff.mat > 0)
zero_count = sum(diff.mat == 0)
neg_count = sum(diff.mat < 0)
net_sum = sum(diff.mat)

cat("\n")
cat(sprintf("# Summary Metrics\n"))
cat(sprintf("Positive number/30 : %d\n", pos_count))
cat(sprintf("Zeros/30            : %d\n", zero_count))
cat(sprintf("NetSum              : %+d\n", net_sum))

# =============================================================================
# Write CSV
# =============================================================================
csv.df = data.frame(
  Spectrum = spectrum.names,
  IFA   = as.integer(diff.mat[, "IFA"]),
  FPavg = as.integer(diff.mat[, "FPavg"]),
  FPA   = as.integer(diff.mat[, "FPA"]),
  Top1  = as.integer(diff.mat[, "Top1"]),
  Top3  = as.integer(diff.mat[, "Top3"]),
  Top5  = as.integer(diff.mat[, "Top5"]),
  stringsAsFactors = FALSE
)
write.csv(csv.df, file = RQ3B.csv.path, row.names = FALSE)
cat(sprintf("\n[CSV] Difference table written: %s\n", RQ3B.csv.path))

# =============================================================================
# Write complete txt report
# =============================================================================
all.output = c(
  "===== RQ3-B: 5 spectrum Sensitivity Analysis (per-spectrum FETCH vs APART-F) =====",
  "",
  "FETCH  ≡ SAT-F.NEW3  (spectrum + λ_T sorting enhancement)",
  "APART-F ≡ SAT-F      (NT + NFC sorting baseline)",
  "",
  "Outer loop = 5 spectrum, inner loop = 7 SAT",
  "Each spectrum outputs 7 lines: each line = a per-spectrum FETCH vs APART-F comparison (1,1,1,1,1,1 vs 2,2,2,2,2,2)",
  "The final output is the rank difference table of spectrum × 6 metrics (same structure as RQ1 txt final table)",
  "",
  all.spectrum.lines,
  "",
  "===== On 7 SATs, the overall rank difference of APART-F and FETCH on 5 spectra (APART-F - FETCH) =====",
  "",
  header.line,
  sep.line,
  all.diff.lines,
  "",
  "# Summary Metrics",
  sprintf("Positive number/30 : %d", pos_count),
  sprintf("Zeros/30            : %d", zero_count),
  sprintf("NetSum              : %+d", net_sum)
)

writeLines(all.output, RQ3B.txt.path)
cat(sprintf("[TXT] Complete report written: %s\n", RQ3B.txt.path))

cat("\n========== RQ3-B Experiment Completed ==========\n")
