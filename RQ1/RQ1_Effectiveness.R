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
library(dplyr)
library(tibble)
library(stringr)
library(grid)

# ==================== Base Path ====================

BASE = '../'

SAT_DIR     = paste0(BASE, 'SATs_initial_scan_result/')
LLM_DIR     = paste0(BASE, 'Baseline/LLM4SA/test/result/')

PMD.result.dir        = paste0(SAT_DIR, 'PMD/')
CheckStyle.result.dir = paste0(SAT_DIR, 'CheckStyle/')
ErrorProne.result.dir = paste0(SAT_DIR, 'Errorprone/')       
Spotbugs.result.dir   = paste0(SAT_DIR, 'Spotbugs/')
codacy.result.dir     = paste0(SAT_DIR, 'codacy/')
codeql.result.dir     = paste0(SAT_DIR, 'codeql/')
sonarqube.result.dir  = paste0(SAT_DIR, 'sonarqube/')

LLM_PMD.result.dir        = paste0(LLM_DIR, 'PMD/')
LLM_CheckStyle.result.dir = paste0(LLM_DIR, 'CheckStyle/')
LLM_ErrorProne.result.dir = paste0(LLM_DIR, 'Errorprone/')
LLM_Spotbugs.result.dir   = paste0(LLM_DIR, 'Spotbugs/')
LLM_Codacy.result.dir     = paste0(LLM_DIR, 'Codacy/')
LLM_Codeql.result.dir     = paste0(LLM_DIR, 'Codeql/')
LLM_Sonarqube.result.dir  = paste0(LLM_DIR, 'Sonarqube/')

spectrum.base.dir = paste0(BASE, 'SATs_result/')
spectrum.subdir   = 'TarantulaWithoutCA'

Mapping.dir = paste0(BASE, 'datasets/map-warning-lines-only/')

prediction_dir = paste0(BASE, 'Baseline/DeepLineDP/output/prediction/DeepLineDP/within-release/')
CEandNFCdir    = paste0(BASE, 'Baseline/GLANCE_and_LineDP/result/Glance_MD_full_threshold/line_result/test/')

n.gram.result.dir = paste0(BASE, 'Baseline/ngram/n_gram_result/')
linedp.result.dir = paste0(BASE, 'Baseline/GLANCE_and_LineDP/result/MIT-LineDP/line_result/test/')
GLANCE_LR.dir     = paste0(BASE, 'Baseline/GLANCE_and_LineDP/result/BASE-Glance-LR/line_result/test/')
linedef_dir       = paste0(BASE, 'Baseline/LineDef/output/prediction/LineDef/within-release/')
BAFLineDP_dir     = paste0(BASE, 'Baseline/BAFLineDP/output/prediction/BAFLineDP/within-release/')

lambda_file = paste0(BASE, 'Auto_lambda_Per_release_Results.txt')
v2_v3_pairs = list(
  list(v2 = 'activemq-5.1.0',       v3 = 'activemq-5.2.0',     project = 'activemq'),
  list(v2 = 'activemq-5.2.0',       v3 = 'activemq-5.3.0',     project = 'activemq'),
  list(v2 = 'activemq-5.3.0',       v3 = 'activemq-5.8.0',     project = 'activemq'),
  list(v2 = 'camel-2.9.0',          v3 = 'camel-2.10.0',       project = 'camel'),
  list(v2 = 'camel-2.10.0',         v3 = 'camel-2.11.0',       project = 'camel'),
  list(v2 = 'derby-10.3.1.4',       v3 = 'derby-10.5.1.1',     project = 'derby'),
  list(v2 = 'groovy-1_6_BETA_1',    v3 = 'groovy-1_6_BETA_2',  project = 'groovy'),
  list(v2 = 'hbase-0.95.0',         v3 = 'hbase-0.95.2',       project = 'hbase'),
  list(v2 = 'hive-0.10.0',          v3 = 'hive-0.12.0',        project = 'hive'),
  list(v2 = 'jruby-1.4.0',          v3 = 'jruby-1.5.0',        project = 'jruby'),
  list(v2 = 'jruby-1.5.0',          v3 = 'jruby-1.7.0.preview1', project = 'jruby'),
  list(v2 = 'lucene-2.9.0',         v3 = 'lucene-3.0.0',       project = 'lucene'),
  list(v2 = 'lucene-3.0.0',         v3 = 'lucene-3.1',         project = 'lucene'),
  list(v2 = 'wicket-1.3.0-beta2',   v3 = 'wicket-1.5.3',       project = 'wicket')
)

# ==================== Output Path ====================
OUTPUT_BASE = './'
if (!dir.exists(OUTPUT_BASE)) dir.create(OUTPUT_BASE, recursive = TRUE)
OUTPUT_DIR_BASE    = OUTPUT_BASE
OUTPUT_FILE   = paste0(OUTPUT_DIR_BASE, '/Table3_OverallComparison.csv')

# ==================== SAT List ====================
SAT_LIST <- c('PMD', 'CheckStyle', 'ErrorProne', 'Spotbugs',
              'Codacy', 'Codeql', 'Sonarqube')

# ==================== Load λ_T ====================
lambda_lines = readLines(lambda_file)
per_release_lambda = list()
current_sat = NULL
current_spec = NULL

for (line in lambda_lines) {
  m_sat = regexpr('^--- (.+) ---', line)
  if (m_sat > 0) {
    current_sat = sub('^--- (.+) ---', '\\1', line)
    next
  }
  m_spec = regexpr('^  ([A-Za-z0-9]+):$', line)
  if (m_spec > 0) {
    current_spec = sub('^  ([A-Za-z0-9]+):$', '\\1', line)
    next
  }
  m_pair = regexpr('^    ([^:]+): ([0-9.]+)$', line)
  if (m_pair > 0 && !is.null(current_sat) && !is.null(current_spec)) {
    pair = sub('^    ([^:]+): ([0-9.]+)$', '\\1', line)
    lambda_val = as.numeric(sub('^    [^:]+: ([0-9.]+)$', '\\1', line))
    key = paste0(current_sat, '_', current_spec)
    if (is.null(per_release_lambda[[key]])) per_release_lambda[[key]] = list()
    per_release_lambda[[key]][[pair]] = lambda_val
  }
}

cat(sprintf('[Lambda] loaded %d combinations (SAT, Spectrum)\n', length(per_release_lambda)))

get_lambda_for_release = function(sat_name, spectrum_name, v3_release) {
  key = paste0(sat_name, '_', spectrum_name)
  if (is.null(per_release_lambda[[key]])) return(0.5)
  for (pair in v2_v3_pairs) {
    if (pair$v3 == v3_release) {
      pair_key = paste0(pair$v2, '->', pair$v3)
      val = per_release_lambda[[key]][[pair_key]]
      if (is.null(val)) return(0.5)
      return(val)
    }
  }
  return(0.5)
}

# ==================== Load DeepLineDP ====================
all_files = list.files(prediction_dir)
df_all <- NULL
for (f in all_files) {
  df <- read.csv(paste0(prediction_dir, f))
  df_all <- rbind(df_all, df)
}
deeplinedp.result = df_all
deeplinedp.result[deeplinedp.result$is.comment.line == 'True', ]$token.attention.score = 0

get.top.k.tokens = function(df, k) {
  top.k <- df %>%
    filter(is.comment.line == 'False' & file.level.ground.truth == 'True' & prediction.label == 'True') %>%
    group_by(test, filename) %>% top_n(k, token.attention.score) %>%
    select('project', 'train', 'test', 'filename', 'token') %>% distinct()
  top.k$flag = 'topk'
  return(top.k)
}

tmp.top.k = get.top.k.tokens(deeplinedp.result, 1500)
merged_df_all = merge(deeplinedp.result, tmp.top.k,
                      by = c('project', 'train', 'test', 'filename', 'token'), all.x = TRUE)
merged_df_all[is.na(merged_df_all$flag), ]$token.attention.score = 0

sum_line_attn = merged_df_all %>%
  filter(file.level.ground.truth == 'True' & prediction.label == 'True') %>%
  group_by(test, filename, is.comment.line, file.level.ground.truth,
           prediction.label, line.number, line.level.ground.truth) %>%
  summarize(attention_score = sum(token.attention.score), num_tokens = n(), .groups = 'drop')
sum_line_attn = sum_line_attn %>% filter(is.comment.line == 'False') %>%
  select(test, filename, line.number, attention_score)
deeplinedp.sorted = sum_line_attn

# ==================== Load GLANCE-LR ====================
GLANCE_LR_files = list.files(GLANCE_LR.dir)
GLANCE_LR_Metric <- NULL
for (f in GLANCE_LR_files) {
  df <- read.csv(paste0(GLANCE_LR.dir, f))
  df$test = str_split_fixed(f, '-result', 2)[, 1]
  GLANCE_LR_Metric <- rbind(GLANCE_LR_Metric, df)
}
GLANCE_LR_Metric = select(GLANCE_LR_Metric, 'predicted_buggy_lines',
                          'predicted_buggy_line_numbers', 'predicted_buggy_score',
                          'rank', 'functioncall', 'controlelements',
                          'numbertokens', 'test')
names(GLANCE_LR_Metric) = c('filename', 'line.number', 'GLANCEscore',
                            'rank', 'NFC', 'CE', 'NT', 'test')
GLANCE_LR_Metric$filename = str_split_fixed(GLANCE_LR_Metric$filename, ':', 2)[, 1]
GLANCE_LR_Metric$filename <- gsub('/', '_', GLANCE_LR_Metric$filename)

# ==================== Load LineDef ====================
linedef.all_files = list.files(linedef_dir)
linedef_df_all <- NULL
for (f in linedef.all_files) {
  df <- read.csv(paste0(linedef_dir, f))
  linedef_df_all <- rbind(linedef_df_all, df)
}
linedef.result = linedef_df_all
linedef.result[linedef.result$is.comment.line == 'True', ]$token.attention.score = 0
tmp.top.k.linedef = get.top.k.tokens(linedef.result, 1500)
linedef.merged_df_all = merge(linedef.result, tmp.top.k.linedef,
                              by = c('project', 'train', 'test', 'filename', 'token'),
                              all.x = TRUE)
linedef.merged_df_all[is.na(linedef.merged_df_all$flag), ]$token.attention.score = 0
linedef.sum_line_attn = linedef.merged_df_all %>%
  filter(file.level.ground.truth == 'True' & prediction.label == 'True') %>%
  group_by(test, filename, is.comment.line, file.level.ground.truth,
           prediction.label, line.number, line.level.ground.truth) %>%
  summarize(attention_score = sum(token.attention.score),
            num_tokens = n(), .groups = 'drop')
linedef.sum_line_attn = linedef.sum_line_attn %>%
  filter(is.comment.line == 'False') %>%
  select(test, filename, line.number, attention_score)
linedef.sorted = linedef.sum_line_attn

# ==================== Load BAFLineDP ====================
BAFLineDP.all_files = list.files(BAFLineDP_dir)
BAFLineDP_df_all <- NULL
for (f in BAFLineDP.all_files) {
  df <- read.csv(paste0(BAFLineDP_dir, f))
  BAFLineDP_df_all <- rbind(BAFLineDP_df_all, df)
}
BAFLineDP_df_all[BAFLineDP_df_all$is.comment.line == 'True', ]$line.attention.score = 0
BAFLineDP.sum_line_attn = BAFLineDP_df_all %>%
  filter(file.level.ground.truth == 'True' & prediction.label == 'True') %>%
  filter(is.comment.line == 'False') %>%
  select(test, filename, line.number, line.attention.score)
BAFLineDP.sorted = BAFLineDP.sum_line_attn

# ==================== Ground Truth ====================
line.ground.truth = select(df_all, project, train, test, filename,
                           file.level.ground.truth, prediction.prob,
                           line.number, line.level.ground.truth, is.comment.line)
line.ground.truth = filter(line.ground.truth, is.comment.line == 'False')
line.ground.truth = distinct(line.ground.truth)

# ==================== Load CE NT NFC ====================
all_CEandNF_files = list.files(CEandNFCdir)
lineLevelMetrics <- NULL
for (f in all_CEandNF_files) {
  df <- read.csv(paste0(CEandNFCdir, f))
  df$test = str_split_fixed(f, '-result', 2)[, 1]
  lineLevelMetrics <- rbind(lineLevelMetrics, df)
}
lineLevelMetrics = select(lineLevelMetrics,
                          'predicted_buggy_lines', 'predicted_buggy_line_numbers',
                          'predicted_buggy_score', 'rank',
                          'functioncall', 'controlelements', 'numbertokens', 'test')
names(lineLevelMetrics) = c('filename', 'line.number', 'GLANCEscore',
                            'rank', 'NFC', 'CE', 'NT', 'test')
lineLevelMetrics$filename = str_split_fixed(lineLevelMetrics$filename, ':', 2)[, 1]
lineLevelMetrics$filename <- gsub('/', '_', lineLevelMetrics$filename)

# ==================== Load Spectrum ====================
get.spectrum.dir = function(sat.name) {
  paste0(spectrum.base.dir, sat.name, '/Result/', spectrum.subdir, '/line_result/test/')
}

load.spectrum.result = function(spectrum.dir, use.mapping = FALSE, SATname_for_mapping = NULL) {
  all_files = list.files(spectrum.dir)
  if (length(all_files) == 0) {
    warning(sprintf('[spectrum] directory is empty or does not exist: %s -> return empty df', spectrum.dir))
    return(data.frame(filename = character(0),
                      line.number = integer(0),
                      spectrum.score = numeric(0),
                      max_jaccard = numeric(0),
                      test = character(0),
                      stringsAsFactors = FALSE))
  }
  spec.df = NULL
  for (f in all_files) {
    releasename <- sub('-result\\.csv$', '', f)
    df <- read.csv(paste0(spectrum.dir, f), stringsAsFactors = FALSE)
    df$test <- releasename
    spec.df <- rbind(spec.df, df)
  }
  colnames(spec.df)[2] <- 'spectrum.score'
  temp <- strsplit(spec.df[[1]], ':', fixed = TRUE)
  filename    <- sapply(temp, function(x) paste(x[-length(x)], collapse = ':'))
  line.number <- as.integer(sapply(temp, function(x) x[length(x)]))
  spectrum.score <- as.numeric(spec.df[['spectrum.score']])
  max_jaccard    <- as.numeric(spec.df[['max_jaccard']])
  test           <- spec.df[['test']]
  this.df <- data.frame(filename = filename, line.number = line.number,
                        spectrum.score = spectrum.score,
                        max_jaccard = max_jaccard,
                        test = test,
                        stringsAsFactors = FALSE)
  this.df$filename <- gsub('/', '_', this.df$filename)

  if (use.mapping && !is.null(SATname_for_mapping)) {
    mapping_subdir = paste0(Mapping.dir, SATname_for_mapping, '_Dataset/LineMapping/')
    rels = unique(this.df$test)
    new.df.list = list()
    for (rel in rels) {
      sub = this.df[this.df$test == rel, , drop = FALSE]
      mapping_file = paste0(mapping_subdir, rel, '_all_files_mapping.csv')
      if (!file.exists(mapping_file)) {
        new.df.list[[length(new.df.list) + 1]] = sub
        next
      }
      mapping_df = read.csv(mapping_file, stringsAsFactors = FALSE)
      mapping_df$File <- gsub('/', '_', mapping_df$File)
      matched = merge(sub, mapping_df,
                      by.x = c('filename', 'line.number'),
                      by.y = c('File', 'compressed_line_number'),
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

# ==================== Line-level Metrics Calculation ====================
get.line.metrics.result = function(baseline.df, cur.df.file) {
  baseline.df.with.ground.truth = merge(baseline.df, cur.df.file,
                                       by = c('filename', 'line.number'))
  baseline.df.with.ground.truth = baseline.df.with.ground.truth %>%
    group_by(filename) %>%
    mutate(actionable.warning = ifelse(any(line.level.ground.truth == 'True'), 1, 0)) %>%
    filter(actionable.warning == 1)

  sorted = baseline.df.with.ground.truth %>%
    group_by(filename) %>% arrange(rank, .by_group = TRUE) %>%
    mutate(order = row_number()) %>% mutate(totalSLOC = n())

  sorted = sorted %>% filter(totalSLOC >= 5)
  filter.files = sorted %>% select(filename) %>% distinct()

  total_true = sorted %>% group_by(filename) %>%
    summarize(total_true = sum(line.level.ground.truth == 'True'))
  total_true = total_true %>% filter(total_true > 0)

  FPavg = sorted %>% group_by(filename) %>%
    mutate(FPI = cumsum(line.level.ground.truth == 'False'),
           total_truth = sum(line.level.ground.truth == 'True')) %>%
    filter(line.level.ground.truth == 'True') %>%
    mutate(S.R = sum(FPI, na.rm = TRUE)) %>%
    mutate(FPavg = round(S.R / total_truth, digits = 2)) %>%
    select(filename, FPavg) %>% distinct()
  FPavg = FPavg %>% arrange(filename)
  FPavg.list = FPavg$FPavg

  IFA = sorted %>% filter(line.level.ground.truth == 'True') %>%
    group_by(filename) %>% top_n(1, -order)
  IFA = IFA %>% arrange(filename)
  ifa.list = IFA$order - 1
  total_true = total_true %>% arrange(filename)

  fpa = sorted %>% merge(total_true) %>% group_by(filename) %>%
    arrange(order, .by_group = TRUE) %>%
    mutate(lineFPA = if_else(line.level.ground.truth == 'True',
                             n() - order + 1, 0) / (n() * total_true)) %>%
    summarize(FPA = sum(lineFPA))
  fpa = fpa %>% arrange(filename)
  fpa.list = fpa$FPA

  top1 = sorted %>% merge(total_true) %>% group_by(filename) %>%
    filter(order <= 1) %>%
    summarize(top1 = sum(line.level.ground.truth == 'True') / n())
  top1 = top1 %>% arrange(filename)
  top1.list = top1$top1

  top3 = sorted %>% merge(total_true) %>% group_by(filename) %>%
    filter(order <= 3) %>%
    summarize(top3 = sum(line.level.ground.truth == 'True') / n())
  top3 = top3 %>% arrange(filename)
  top3.list = top3$top3

  top5 = sorted %>% merge(total_true) %>% group_by(filename) %>%
    filter(order <= 5) %>%
    summarize(top5 = sum(line.level.ground.truth == 'True') / n())
  top5 = top5 %>% arrange(filename)
  top5.list = top5$top5

  result.df = data.frame(IFA$filename, ifa.list, fpa.list, top1.list, top3.list,
                         top5.list, FPavg.list, IFA$totalSLOC)
  return(result.df)
}

# ==================== Evaluate One SAT ====================
evaluate_one_sat = function(all_eval_releases, SAT.result.dir, SATname,
                            LLM.result.dir = NULL,
                            line.ground.truth, lineLevelMetrics,
                            deeplinedp.sorted,
                            spectrum.result.df,
                            jaccard_threshold = 0.70) {

  sat_native.result.df    = NULL
  apart_f.result.df       = NULL
  fetch_result.df         = NULL
  deeplinedp_result.df    = NULL
  ngram_result.df         = NULL
  linedp_result.df        = NULL
  glance_lr_result.df     = NULL
  splice_f_result.df      = NULL
  linedef_result.df       = NULL
  baflinedp_result.df     = NULL
  sat_llm4sa_result.df    = NULL

  for (rel in all_eval_releases) {
    if (SATname == 'PMD' || SATname == 'ErrorProne') {
      allSATresult = read.csv(paste0(SAT.result.dir, rel, '-line-lvl-result.txt'), quote = '')
      if (SATname != 'ErrorProne') {
        allSATresult$filename <- gsub('/', '_', allSATresult$filename)
      }
    } else {
      allSATresult = read.csv(paste0(SAT.result.dir, rel, '-result.csv'))
    }

    cur.df.file = filter(line.ground.truth, test == rel)
    cur.df.file = select(cur.df.file, filename, line.number, line.level.ground.truth)
    cur.df.file$filename <- gsub('/', '_', cur.df.file$filename)

    if (SATname != 'CheckStyle') {
      names(allSATresult) = c('filename', 'test.release', 'line_number',
                               'SAT_prediction_result', 'Priority')
      allSATresult$SAT_prediction_result <-
        ifelse(allSATresult$SAT_prediction_result %in% c('False', 'FALSE'), 0, 1)
      allSATresult = allSATresult %>% filter(SAT_prediction_result == 1)
    } else {
      allSATresult = select(allSATresult, filename, line, priority)
      names(allSATresult) = c('filename', 'line_number', 'Priority')
    }

    SAT.base.info = select(allSATresult, filename, line_number, Priority)
    names(SAT.base.info) = c('filename', 'line.number', 'Priority')

    # GLANCE_F: All models share, pre-calculated
    GLANCE_F = select(lineLevelMetrics, test, filename, line.number, NT, NFC) %>%
      filter(test == rel) %>% select(filename, line.number, NT, NFC)

    # 1) SAT-Native
    sat_native.rank = SAT.base.info %>%
      group_by(filename) %>% arrange(Priority, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    sat_native.rank = select(sat_native.rank, filename, line.number, rank)
    sat_native.eval = get.line.metrics.result(sat_native.rank, cur.df.file) %>%
      mutate(test = rel)
    sat_native.result.df = rbind(sat_native.result.df, sat_native.eval)

    # 2) APART-F
    apart_f.rank = SAT.base.info
    apart_f.rank = left_join(apart_f.rank, GLANCE_F,
                              by = c('filename', 'line.number')) %>%
      mutate(NT = replace_na(NT, 0), NFC = replace_na(NFC, 0))
    apart_f.rank = apart_f.rank %>%
      group_by(filename) %>% arrange(-NFC * NT, Priority, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    apart_f.rank = select(apart_f.rank, filename, line.number, rank)
    apart_f.eval = get.line.metrics.result(apart_f.rank, cur.df.file) %>%
      mutate(test = rel)

    # 3) FETCH
    lambda_T = get_lambda_for_release(SATname, 'Tarantula', rel)

    fetch.rank = SAT.base.info
    fetch.rank = left_join(fetch.rank, GLANCE_F, by = c('filename', 'line.number')) %>%
      mutate(NT = replace_na(NT, 0), NFC = replace_na(NFC, 0))

    spectrum.result = spectrum.result.df %>% filter(test == rel)
    if (nrow(spectrum.result) > 0) {
      spectrum.result$is_persistent = spectrum.result$max_jaccard >= jaccard_threshold
    } else {
      spectrum.result$is_persistent = logical(0)
    }
    fetch.rank = left_join(fetch.rank, spectrum.result,
                           by = c('filename', 'line.number')) %>%
      mutate(spectrum.score = replace_na(spectrum.score, 0),
             max_jaccard    = replace_na(max_jaccard, 0),
             is_persistent  = replace_na(is_persistent, FALSE))

    baseScore = fetch.rank$NFC * fetch.rank$NT
    specScore = fetch.rank$spectrum.score
    fetch.rank$NEW3.score = log1p(baseScore) + lambda_T * log1p(specScore)
    fetch.rank$Persistent = ifelse(fetch.rank$is_persistent, 1, 0)

    fetch.rank = fetch.rank %>%
      group_by(filename) %>%
      arrange(desc(Persistent), desc(NEW3.score), Priority, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    fetch.rank = select(fetch.rank, filename, line.number, rank)
    fetch.eval = get.line.metrics.result(fetch.rank, cur.df.file) %>% mutate(test = rel)

    # SAT.base.info is generated in the #1 SAT-Native block, the other models directly reuse

    # 4) DeepLineDP
    temp.dl = deeplinedp.sorted %>% filter(test == rel) %>%
      select(filename, line.number, attention_score)
    temp.dl$filename <- gsub('/', '_', temp.dl$filename)
    names(temp.dl) = c('filename', 'line.number', 'deeplinedp.score')
    dl.rank = SAT.base.info
    dl.rank = left_join(dl.rank, temp.dl, by = c('filename', 'line.number')) %>%
      mutate(deeplinedp.score = replace_na(deeplinedp.score, 0))
    dl.rank = dl.rank %>%
      group_by(filename) %>%
      arrange(-deeplinedp.score, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    dl.rank = select(dl.rank, filename, line.number, rank)
    dl.eval = get.line.metrics.result(dl.rank, cur.df.file) %>% mutate(test = rel)
    deeplinedp_result.df = rbind(deeplinedp_result.df, dl.eval)

    # 5) N-gram Ranking
    ngram_df = read.csv(paste0(n.gram.result.dir, rel, '-line-lvl-result.txt'),
                       sep = '\t', quote = '')
    ngram_df = select(ngram_df, 'file.name', 'line.number', 'line.score')
    ngram_df = distinct(ngram_df)
    names(ngram_df) = c('filename', 'line.number', 'n.gram.score')
    ngram_df$line.number <- as.integer(ngram_df$line.number)
    ngram_df$filename <- gsub('/', '_', ngram_df$filename)
    ngram.rank = SAT.base.info
    ngram.rank = left_join(ngram.rank, ngram_df,
                           by = c('filename', 'line.number')) %>%
      mutate(n.gram.score = replace_na(n.gram.score, 0))
    ngram.rank = ngram.rank %>%
      group_by(filename) %>%
      arrange(-n.gram.score, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    ngram.rank = select(ngram.rank, filename, line.number, rank)
    ngram.eval = get.line.metrics.result(ngram.rank, cur.df.file) %>%
      mutate(test = rel)
    ngram_result.df = rbind(ngram_result.df, ngram.eval)

    # 6) LineDP Ranking
    linedp_df = read.csv(paste0(linedp.result.dir, rel, '-result.csv'))
    linedp_df$filename = str_split_fixed(linedp_df$predicted_buggy_lines,
                                       ':', 2)[, 1]
    linedp_df$line.number = str_split_fixed(linedp_df$predicted_buggy_lines,
                                           ':', 2)[, 2]
    linedp_df$line.number <- as.integer(linedp_df$line.number)
    linedp_df$filename <- gsub('/', '_', linedp_df$filename)
    linedp_df = select(linedp_df, filename, line.number, predicted_buggy_score)
    names(linedp_df) = c('filename', 'line.number', 'linedp.score')
    linedp.rank = SAT.base.info
    linedp.rank = left_join(linedp.rank, linedp_df,
                            by = c('filename', 'line.number')) %>%
      mutate(linedp.score = replace_na(linedp.score, 0))
    linedp.rank = linedp.rank %>%
      group_by(filename) %>%
      arrange(-linedp.score, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    linedp.rank = select(linedp.rank, filename, line.number, rank)
    linedp.eval = get.line.metrics.result(linedp.rank, cur.df.file) %>%
      mutate(test = rel)
    linedp_result.df = rbind(linedp_result.df, linedp.eval)

    # 7) GLANCE-LR Ranking
    GLANCE_LR_rel = select(GLANCE_LR_Metric, test, filename, line.number,
                            GLANCEscore, CE) %>% filter(test == rel)
    GLANCE_LR_rel = select(GLANCE_LR_rel, filename, line.number,
                            GLANCEscore, CE)
    glance_lr.rank = SAT.base.info
    glance_lr.rank = left_join(glance_lr.rank, GLANCE_LR_rel,
                               by = c('filename', 'line.number')) %>%
      mutate(GLANCEscore = replace_na(GLANCEscore, 0),
             CE = replace_na(CE, 0))
    glance_lr.rank = glance_lr.rank %>%
      group_by(filename) %>%
      arrange(-CE, -GLANCEscore, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    glance_lr.rank = select(glance_lr.rank, filename, line.number, rank)
    glance_lr.eval = get.line.metrics.result(glance_lr.rank, cur.df.file) %>%
      mutate(test = rel)
    glance_lr_result.df = rbind(glance_lr_result.df, glance_lr.eval)

    # 8) SPLICE-F = -NFC*NT*deeplinedp.score
    temp.dl.s = deeplinedp.sorted %>% filter(test == rel) %>%
      select(filename, line.number, attention_score)
    temp.dl.s$filename <- gsub('/', '_', temp.dl.s$filename)
    names(temp.dl.s) = c('filename', 'line.number', 'deeplinedp.score')
    splice_f.rank = SAT.base.info
    splice_f.rank = left_join(splice_f.rank, GLANCE_F,
                               by = c('filename', 'line.number')) %>%
      mutate(NT = replace_na(NT, 0),
             NFC = replace_na(NFC, 0))
    splice_f.rank = left_join(splice_f.rank, temp.dl.s,
                               by = c('filename', 'line.number')) %>%
      mutate(deeplinedp.score = replace_na(deeplinedp.score, 0))
    splice_f.rank = splice_f.rank %>%
      group_by(filename) %>%
      arrange(-NFC * NT * deeplinedp.score, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    splice_f.rank = select(splice_f.rank, filename, line.number, rank)
    splice_f.eval = get.line.metrics.result(splice_f.rank, cur.df.file) %>%
      mutate(test = rel)
    splice_f_result.df = rbind(splice_f_result.df, splice_f.eval)

    # 9) LineDef Ranking
    temp.ld = linedef.sorted %>% filter(test == rel) %>%
      select(filename, line.number, attention_score)
    temp.ld$filename <- gsub('/', '_', temp.ld$filename)
    names(temp.ld) = c('filename', 'line.number', 'linedef.score')
    linedef.rank = SAT.base.info
    linedef.rank = left_join(linedef.rank, temp.ld,
                              by = c('filename', 'line.number')) %>%
      mutate(linedef.score = replace_na(linedef.score, 0))
    linedef.rank = linedef.rank %>%
      group_by(filename) %>%
      arrange(-linedef.score, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    linedef.rank = select(linedef.rank, filename, line.number, rank)
    linedef.eval = get.line.metrics.result(linedef.rank, cur.df.file) %>%
      mutate(test = rel)
    linedef_result.df = rbind(linedef_result.df, linedef.eval)

    # 10) BAFLineDP Ranking
    temp.baf = BAFLineDP.sorted %>% filter(test == rel) %>%
      select(filename, line.number, line.attention.score)
    temp.baf$filename <- gsub('/', '_', temp.baf$filename)
    names(temp.baf) = c('filename', 'line.number', 'baflinedp.score')
    baflinedp.rank = SAT.base.info
    baflinedp.rank = left_join(baflinedp.rank, temp.baf,
                                by = c('filename', 'line.number')) %>%
      mutate(baflinedp.score = replace_na(baflinedp.score, 0))
    baflinedp.rank = baflinedp.rank %>%
      group_by(filename) %>%
      arrange(-baflinedp.score, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    baflinedp.rank = select(baflinedp.rank, filename, line.number, rank)
    baflinedp.eval = get.line.metrics.result(baflinedp.rank, cur.df.file) %>%
      mutate(test = rel)
    baflinedp_result.df = rbind(baflinedp_result.df, baflinedp.eval)

    # 11) LLM4SA (Same as RQ3 script: real bug > false alarm > unknown, add SAT native Priority)
    #     - First key: SAT native Priority (ascending, smaller value = higher priority)
    #     - Second key: LLM4SA.score descending (real=3 > unknown=2 > false=1)
    #     - Third key: natural line number
    #     - Note: LLM4SA data is generated from PMD perspective, but its (filename, line_number) aligns with any SAT,
    #       therefore it can be left-joined with any SAT.base.info (same as RQ3 script)
    if (!is.null(LLM.result.dir)) {
      LLM4SA = read.csv(paste0(LLM.result.dir, rel, '-line-lvl-result.txt'),
                        quote = '')
      LLM4SA = LLM4SA %>% mutate(Priority = case_when(
        PMD_prediction_result == 'real bug'     ~ 3,
        PMD_prediction_result == 'false alarm'  ~ 1,
        PMD_prediction_result == 'unknown'      ~ 2
      ))
      LLM4SA = select(LLM4SA, filename, line_number, Priority)
      names(LLM4SA) = c('filename', 'line.number', 'LLM4SA.score')

      sat_llm4sa.rank = SAT.base.info
      sat_llm4sa.rank = left_join(sat_llm4sa.rank, LLM4SA,
                                  by = c('filename', 'line.number')) %>%
        mutate(LLM4SA.score = replace_na(LLM4SA.score, 0))
      sat_llm4sa.rank = sat_llm4sa.rank %>%
        group_by(filename) %>%
        arrange(Priority, -LLM4SA.score, line.number, .by_group = TRUE) %>%
        mutate(rank = row_number()) %>% ungroup()
      sat_llm4sa.rank = select(sat_llm4sa.rank, filename, line.number, rank)
      sat_llm4sa.eval = get.line.metrics.result(sat_llm4sa.rank, cur.df.file) %>%
        mutate(test = rel)
    } else {
      sat_llm4sa.eval = NULL
    }

    apart_f.result.df    = rbind(apart_f.result.df,    apart_f.eval)
    fetch_result.df      = rbind(fetch_result.df,      fetch.eval)
    sat_llm4sa_result.df = rbind(sat_llm4sa_result.df, sat_llm4sa.eval)

    cat(sprintf('  [%s] finished %s\n', SATname, rel))
  }

  release_aggregate = function(df, technique_name) {
    df %>%
      filter(if_all(c(ifa.list, FPavg.list, fpa.list,
                      top1.list, top3.list, top5.list),
                    ~ !is.na(.x) & !is.nan(.x))) %>%
      summarise(
        IFA   = median(ifa.list),
        FPavg = median(FPavg.list),
        FPA   = median(fpa.list, na.rm = TRUE),
        Top1  = mean(top1.list,   na.rm = TRUE),
        Top3  = mean(top3.list,   na.rm = TRUE),
        Top5  = mean(top5.list,   na.rm = TRUE),
        .by   = test
      ) %>%
      mutate(technique = technique_name)
  }

  sat_native_rel  = release_aggregate(sat_native.result.df,  'SAT-Native')
  apart_f_rel     = release_aggregate(apart_f.result.df,    'APART-F')
  fetch_rel       = release_aggregate(fetch_result.df,      'FETCH')
  deeplinedp_rel  = release_aggregate(deeplinedp_result.df, 'DeepLineDP')
  ngram_rel       = release_aggregate(ngram_result.df,      'N-gram')
  linedp_rel      = release_aggregate(linedp_result.df,    'LineDP')
  glance_lr_rel   = release_aggregate(glance_lr_result.df,   'GLANCE-LR')
  splice_f_rel    = release_aggregate(splice_f_result.df,   'SPLICE-F')
  linedef_rel     = release_aggregate(linedef_result.df,   'LineDef')
  baflinedp_rel   = release_aggregate(baflinedp_result.df, 'BAFLineDP')
  llm4sa_rel      = release_aggregate(sat_llm4sa_result.df, 'LLM4SA')

  sat_rel_table = bind_rows(
    sat_native_rel, apart_f_rel, fetch_rel,
    deeplinedp_rel, ngram_rel, linedp_rel, glance_lr_rel,
    splice_f_rel, linedef_rel, baflinedp_rel,
    llm4sa_rel
  ) %>%
    mutate(SAT = SATname)

  return(list(rel_table = sat_rel_table))
}

# ==================== Project Mapping ====================
PROJECT_MAP <- list(
  'activemq' = c('activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0'),
  'camel'    = c('camel-2.10.0',   'camel-2.11.0'),
  'derby'    = c('derby-10.5.1.1'),
  'groovy'   = c('groovy-1_6_BETA_2'),
  'hbase'    = c('hbase-0.95.2'),
  'hive'     = c('hive-0.12.0'),
  'jruby'    = c('jruby-1.5.0',    'jruby-1.7.0.preview1'),
  'lucene'   = c('lucene-3.0.0',   'lucene-3.1'),
  'wicket'   = c('wicket-1.5.3')
)

test_to_project <- function(test_name) {
  for (proj in names(PROJECT_MAP)) {
    if (test_name %in% PROJECT_MAP[[proj]]) return(proj)
  }
  return(NA)
}

# ==================== Test Releases ====================
all_eval_releases = c(
  'activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0',
  'camel-2.10.0', 'camel-2.11.0',
  'derby-10.5.1.1',
  'groovy-1_6_BETA_2',
  'hbase-0.95.2',
  'hive-0.12.0',
  'jruby-1.5.0', 'jruby-1.7.0.preview1',
  'lucene-3.0.0', 'lucene-3.1',
  'wicket-1.5.3'
)

sat.tools = list(
  list(name = 'PMD',         dir = PMD.result.dir,         llm_result.dir = LLM_PMD.result.dir),
  list(name = 'CheckStyle',  dir = CheckStyle.result.dir,  llm_result.dir = LLM_CheckStyle.result.dir),
  list(name = 'ErrorProne',  dir = ErrorProne.result.dir,  llm_result.dir = LLM_ErrorProne.result.dir),
  list(name = 'Spotbugs',    dir = Spotbugs.result.dir,    llm_result.dir = LLM_Spotbugs.result.dir),
  list(name = 'Codacy',      dir = codacy.result.dir,      llm_result.dir = LLM_Codacy.result.dir),
  list(name = 'Codeql',      dir = codeql.result.dir,      llm_result.dir = LLM_Codeql.result.dir),
  list(name = 'Sonarqube',   dir = sonarqube.result.dir,   llm_result.dir = LLM_Sonarqube.result.dir)
)

METRICS <- c('IFA', 'FPavg', 'FPA', 'Top1', 'Top3', 'Top5')
METHODS <- c('SAT-Native', 'LLM4SA', 'N-gram', 'DeepLineDP',
             'LineDP', 'GLANCE-LR', 'LineDef', 'BAFLineDP',
             'SPLICE-F', 'APART-F', 'FETCH')

# ==================== Main Process ====================
cat('\n========== RQ1 One-Shot Pipeline ==========\n')
cat('Target: 7 SAT + 11 models -> Table3_OverallComparison.csv\n\n')

sat_combined = NULL

for (tool in sat.tools) {
  cat(sprintf('----- Processing %s -----\n', tool$name))

  spec.dir = get.spectrum.dir(tool$name)
  cur.spectrum.df = load.spectrum.result(spec.dir,
                                         use.mapping = TRUE,
                                         SATname_for_mapping = tool$name)

  sat_out = evaluate_one_sat(
    all_eval_releases = all_eval_releases,
    SAT.result.dir    = tool$dir,
    SATname           = tool$name,
    LLM.result.dir    = tool$llm_result.dir,
    line.ground.truth = line.ground.truth,
    lineLevelMetrics  = lineLevelMetrics,
    deeplinedp.sorted  = deeplinedp.sorted,
    spectrum.result.df = cur.spectrum.df,
    jaccard_threshold  = 0.70
  )

  sat_rel_table = sat_out$rel_table
  sat_combined  = bind_rows(sat_combined, sat_rel_table)

  cat(sprintf('  -> %s: %d rows (release × technique, in memory)\n',
              tool$name, nrow(sat_rel_table)))
}

cat(sprintf('\n[Memory Accumulation] sat_combined total rows: %d\n', nrow(sat_combined)))

# ==================== Step 1: Project-level Aggregation (Plan A: mean) ====================
all_data = sat_combined %>%
  mutate(project = sapply(test, test_to_project))

project_results <- all_data %>%
  summarise(
    IFA   = mean(IFA,   na.rm = TRUE),
    FPavg = mean(FPavg, na.rm = TRUE),
    FPA   = mean(FPA,   na.rm = TRUE),
    Top1  = mean(Top1,  na.rm = TRUE),
    Top3  = mean(Top3,  na.rm = TRUE),
    Top5  = mean(Top5,  na.rm = TRUE),
    .by = c(project, SAT, technique)
  )

cat(sprintf('  project-level: %d rows (in memory)\n', nrow(project_results)))

# ==================== Step 2: Global Macro-Average -> 4x6 Wide Table ====================
global_tbl <- project_results %>%
  pivot_longer(cols = all_of(METRICS),
               names_to = 'metric', values_to = 'value') %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    .by = c(technique, metric)
  ) %>%
  pivot_wider(id_cols = technique,
              names_from = metric,
              values_from = mean_val) %>%
  arrange(factor(technique, levels = METHODS))

# ---- Table 5 Wide Table (Final Output) ----
write.csv(global_tbl, OUTPUT_FILE, row.names = FALSE)
cat(sprintf('  saved: %s (%d rows × %d cols)\n',
            basename(OUTPUT_FILE), nrow(global_tbl), ncol(global_tbl)))
cat('\nTable 5 (11 method × 6 metric):\n')
print(global_tbl %>% select(technique, all_of(METRICS)) %>% knitr::kable(digits = 4))

# ==================== Step 4-8: Table 6 (Paired comparison FETCH vs APART-F) ====================
cat('\n========== Step 4-8: Table 6 (Paired comparison FETCH vs APART-F) ==========\n')

# Step 4: Build 60 unit paired data
paired_long <- project_results %>%
  pivot_longer(cols = all_of(METRICS),
               names_to = 'metric',
               values_to = 'value') %>%
  pivot_wider(id_cols = c(project, SAT, metric),
              names_from = technique,
              values_from = value) %>%
  filter(!is.na(FETCH), !is.na(`APART-F`)) %>%
  mutate(diff = FETCH - `APART-F`) %>%
  select(project, SAT, metric, FETCH, `APART-F`, diff)

n_cells <- paired_long %>% distinct(project, SAT) %>% nrow()
cat(sprintf('  paired long table rows: %d (expected %d×6=%d)\n',
            nrow(paired_long), n_cells, n_cells * 6))
cat(sprintf('  unique units: %d\n', n_cells))

# Step 5: Mean Δ + W/T/L
metric_lower_better <- tibble(
  metric       = c('IFA', 'FPavg', 'FPA', 'Top1', 'Top3', 'Top5'),
  lower_better = c(TRUE,    TRUE,    FALSE, FALSE, FALSE, FALSE)
)

mean_delta_tbl <- paired_long %>%
  summarise(mean_diff_raw = mean(diff, na.rm = TRUE), .by = metric) %>%
  left_join(metric_lower_better, by = 'metric') %>%
  mutate(
    sign_factor    = ifelse(lower_better, -1, 1),
    Mean_Delta     = sign_factor * mean_diff_raw,
    Mean_Delta_str = sprintf('%+.4f', Mean_Delta)
  )

wtl_data <- paired_long %>%
  left_join(metric_lower_better, by = 'metric') %>%
  mutate(
    sign_factor = ifelse(lower_better, -1, 1),
    adj_diff    = sign_factor * diff
  ) %>%
  summarise(
    n_win  = sum(adj_diff >  1e-9),
    n_tie  = sum(abs(adj_diff) <= 1e-9),
    n_loss = sum(adj_diff < -1e-9),
    .by = metric
  )

row_stats <- mean_delta_tbl %>%
  left_join(wtl_data, by = 'metric') %>%
  mutate(WTL = sprintf('%d/%d/%d', n_win, n_tie, n_loss)) %>%
  arrange(factor(metric, levels = METRICS))

# Step 6: Wilcoxon signed-rank 
wilcox_signed_rank <- function(adj_d, correct = FALSE) {
  d     <- adj_d[!is.na(adj_d) & !is.nan(adj_d)]
  n_tot <- length(d)
  d_nz  <- d[abs(d) > 1e-12]
  n_nz  <- length(d_nz)

  if (n_nz < 1) {
    return(list(V = NA_real_, Z = NA_real_, p = NA_real_,
                n_total = n_tot, n_nonzero = n_nz))
  }
  if (sd(d_nz) < 1e-12) {
    return(list(V = NA_real_, Z = NA_real_, p = NA_real_,
                n_total = n_tot, n_nonzero = n_nz))
  }

  abs_d_nz <- abs(d_nz)
  r        <- rank(abs_d_nz, ties.method = 'average')
  W_pos    <- sum(r[d_nz > 0])
  W_neg    <- sum(r[d_nz < 0])
  V        <- min(W_pos, W_neg)

  sigma2  <- n_nz * (n_nz + 1) * (2 * n_nz + 1) / 24
  sigma   <- sqrt(sigma2)
  mu      <- n_nz * (n_nz + 1) / 4

  Z_signed <- (W_pos - mu) / sigma
  p_two    <- 2 * pnorm(-abs(Z_signed))

  list(V = V, Z = Z_signed, p = p_two,
       n_total = n_tot, n_nonzero = n_nz)
}

wilcoxon_tbl <- paired_long %>%
  left_join(metric_lower_better, by = 'metric') %>%
  mutate(
    sign_factor = ifelse(lower_better, -1, 1),
    adj_diff    = sign_factor * diff
  ) %>%
  summarise(
    wt = list(wilcox_signed_rank(adj_diff)),
    .by = metric
  ) %>%
  tidyr::unnest_wider(wt) %>%
  rename(n = n_total, n_nz = n_nonzero, p_raw = p)

# Holm Correction (m = 6)
p_raw_tbl <- wilcoxon_tbl %>%
  arrange(p_raw) %>%
  mutate(
    rank_p = row_number(),
    p_holm = pmin(1, p_raw * (6 - rank_p + 1))
  ) %>%
  arrange(p_holm) %>%
  mutate(p_holm = cummax(p_holm)) %>%
  arrange(match(metric, METRICS))

# Step 7: Effect size r = Z / sqrt(N), N = n_cells (actual paired units)
r_label <- function(r) {
  if (is.na(r)) return('-')
  ar <- abs(r)
  if (ar < 0.1) return('trivial')
  if (ar < 0.3) return('small')
  if (ar < 0.5) return('moderate')
  return('large')
}

effect_tbl <- p_raw_tbl %>%
  mutate(
    N          = n_cells,
    r          = Z / sqrt(N),
    r_str      = sprintf('%+.3f', r),
    r_lbl      = sapply(r, r_label),
    p_raw_str  = sprintf('%.8f', p_raw),
    p_holm_str = sprintf('%.8f', p_holm)
  )

# Step 8: Output Table 6 CSV
table6 <- row_stats %>%
  select(metric, Mean_Delta_str, WTL) %>%
  left_join(effect_tbl %>% select(metric, p_raw_str, p_holm_str, r_str),
            by = 'metric') %>%
  rename(
    Metric          = metric,
    `Mean Delta`    = Mean_Delta_str,
    `W/T/L`         = WTL,
    `p raw`         = p_raw_str,
    `Adjusted p`    = p_holm_str,
    `Effect size r` = r_str
  )

fp_t6 <- paste0(OUTPUT_DIR_BASE, '/Table4_PairedComparison.csv')
write.csv(table6, fp_t6, row.names = FALSE)
cat(sprintf('  saved: %s\n', basename(fp_t6)))
print(table6 %>% select(Metric, `Mean Delta`, `W/T/L`, `Adjusted p`, `Effect size r`) %>%
      knitr::kable(digits = 4))

# ==================== Step 9: Figure 5 Heatmap Data (per-SAT × metric) ====================
cat('\n========== Step 9: Figure 5 Heatmap Data (per-SAT × metric W/T/L) ==========\n')

sat_metric_long <- project_results %>%
  filter(technique %in% c('FETCH', 'APART-F')) %>%
  select(project, SAT, technique, all_of(METRICS)) %>%
  pivot_longer(cols = all_of(METRICS),
               names_to = 'metric', values_to = 'value') %>%
  pivot_wider(id_cols = c(project, SAT, metric),
              names_from = technique, values_from = value) %>%
  filter(!is.na(FETCH), !is.na(`APART-F`)) %>%
  mutate(
    diff        = FETCH - `APART-F`,
    lower_flag  = metric %in% c('IFA', 'FPavg'),
    sign_factor = ifelse(lower_flag, -1, 1),
    adj_diff    = sign_factor * diff,
    result = case_when(
      abs(adj_diff) <= 1e-12 ~ 'Tie',
      adj_diff >  1e-12     ~ 'Win',
      TRUE                  ~ 'Loss'
    )
  )

wtl_sat <- sat_metric_long %>%
  summarise(
    n_pairs = sum(!is.na(diff)),
    n_win   = sum(result == 'Win',  na.rm = TRUE),
    n_tie   = sum(result == 'Tie',  na.rm = TRUE),
    n_loss  = sum(result == 'Loss', na.rm = TRUE),
    .by = c(SAT, metric)
  ) %>%
  mutate(
    WTL   = sprintf('%d/%d/%d', n_win, n_tie, n_loss),
    metric = factor(metric, levels = METRICS),
    SAT    = factor(SAT, levels = SAT_LIST)
  ) %>%
  arrange(SAT, metric)

# W/T/L 7×6 Matrix
wtl_wide <- wtl_sat %>%
  select(SAT, metric, WTL) %>%
  pivot_wider(id_cols = SAT, names_from = metric, values_from = WTL)

# score 7×6 Matrix: score = (W + 0.5 * T) / (W + T + L)
score_wide <- wtl_sat %>%
  mutate(
    n_total = n_win + n_tie + n_loss,
    score   = ifelse(n_total > 0,
                     (n_win + 0.5 * n_tie) / n_total,
                     NA_real_)
  ) %>%
  select(SAT, metric, score) %>%
  pivot_wider(id_cols = SAT, names_from = metric, values_from = score)

fp_fig5_wtl   <- paste0(OUTPUT_DIR_BASE, '/Figure5_HeatmapWTL_by_SAT.csv')
fp_fig5_score <- paste0(OUTPUT_DIR_BASE, '/Figure5_HeatmapScore_by_SAT.csv')

write.csv(wtl_wide,   fp_fig5_wtl,   row.names = FALSE)
write.csv(score_wide, fp_fig5_score, row.names = FALSE)
cat(sprintf('  saved: %s\n', basename(fp_fig5_wtl)))
cat(sprintf('  saved: %s\n', basename(fp_fig5_score)))

cat('\n========== Figure 5 Heatmap W/T/L (7×6) ==========\n')
print(wtl_wide %>% knitr::kable())
cat('\n========== Figure 5 Heatmap score (7×6) ==========\n')
print(score_wide %>%
        mutate(across(-SAT, ~sprintf('%.3f', .))) %>%
        knitr::kable())

# ==================== Final Summary ====================
cat('\n==========================================================\n')
cat('========== One-Stop Pipeline Completed ==========\n')
cat('Output Directory: ', OUTPUT_DIR_BASE, '\n')
cat('==========================================================\n')
cat('\nGenerated Files:\n')
print(list.files(OUTPUT_DIR_BASE, pattern = 'Table|Figure5'))
cat('\n')
