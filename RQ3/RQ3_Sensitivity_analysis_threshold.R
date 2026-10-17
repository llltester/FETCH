# =============================================================================
# RQ3-A Overall Aggregation (Table-5-aligned): Cross-SAT mean per τ on top of
# per-(τ, SAT, project) mean
#
# 2026-09-26
#
#
# Output:
#   ./A_different_Threshold/
#     RQ3A_overall_norm.csv  10 τ × 7 columns (τ + 6 metric normalized values)
# =============================================================================

library(tidyverse)
library(stringr)

# ==================== Path Configuration ====================

BASE = '../'

SAT_DIR     = paste0(BASE, 'SATs_initial_scan_result/')

PMD.result.dir       = paste0(SAT_DIR, 'PMD/')
CheckStyle.result.dir= paste0(SAT_DIR, 'CheckStyle/')
ErrorProne.result.dir= paste0(SAT_DIR, 'Errorprone/')     
Spotbugs.result.dir  = paste0(SAT_DIR, 'Spotbugs/')
codacy.result.dir    = paste0(SAT_DIR, 'codacy/')
codeql.result.dir    = paste0(SAT_DIR, 'codeql/')
sonarqube.result.dir = paste0(SAT_DIR, 'sonarqube/')

spectrum.base.dir = paste0(BASE, 'SATs_result/')
spectrum.subdir   = 'TarantulaWithoutCA'

Mapping.dir = paste0(BASE, 'datasets/map-warning-lines-only/')

prediction_dir = paste0(BASE, 'Baseline/DeepLineDP/output/prediction/DeepLineDP/within-release/')
CEandNFCdir    = paste0(BASE, 'Baseline/GLANCE_and_LineDP/result/Glance_MD_full_threshold/line_result/test/')

RQ3.base.dir     = paste0(BASE, 'RQ3/A_different_Threshold/')

lambda_file = paste0(BASE, 'Auto_lambda_Per_release_Results.txt')

if (!dir.exists(RQ3.base.dir)) dir.create(RQ3.base.dir, recursive = TRUE)

# ==================== Project → Releases mapping ====================
PROJECT_MAP <- list(
  'activemq' = c('activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0'),
  'camel'    = c('camel-2.10.0',   'camel-2.11.0'),
  'derby'   = c('derby-10.5.1.1'),
  'groovy'  = c('groovy-1_6_BETA_2'),
  'hbase'   = c('hbase-0.95.2'),
  'hive'    = c('hive-0.12.0'),
  'jruby'   = c('jruby-1.5.0',    'jruby-1.7.0.preview1'),
  'lucene'  = c('lucene-3.0.0',   'lucene-3.1'),
  'wicket'  = c('wicket-1.5.3')
)

test_to_project <- function(test_name) {
  for (proj in names(PROJECT_MAP)) {
    if (test_name %in% PROJECT_MAP[[proj]]) return(proj)
  }
  return(NA)
}

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

cat(sprintf('[Lambda] Loaded %d (SAT, Spectrum) combinations\n',
            length(per_release_lambda)))

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

# ==================== Ground Truth  ====================
all_files = list.files(prediction_dir)
df_all <- NULL
for (f in all_files) {
  df <- read.csv(paste0(prediction_dir, f))
  df_all <- rbind(df_all, df)
}

line.ground.truth = select(df_all, project, train, test, filename,
                           file.level.ground.truth, prediction.prob,
                           line.number, line.level.ground.truth, is.comment.line)
line.ground.truth = filter(line.ground.truth, is.comment.line == 'False')
line.ground.truth = distinct(line.ground.truth)


line.ground.truth$filename <- gsub('/', '_', line.ground.truth$filename)


line.ground.truth.split <- split(
  line.ground.truth[, c('filename', 'line.number', 'line.level.ground.truth')],
  line.ground.truth$test
)

# ==================== GLANCE (NT, NFC) ====================
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


GLANCE_F.split <- lapply(
  split(lineLevelMetrics[, c('test', 'filename', 'line.number', 'NT', 'NFC')],
        lineLevelMetrics$test),
  function(df) df[, c('filename', 'line.number', 'NT', 'NFC')]
)

# ==================== Spectrum loading  ====================
get.spectrum.dir = function(sat.name) {
  paste0(spectrum.base.dir, sat.name, '/Result/', spectrum.subdir, '/line_result/test/')
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
  test           <- as.character(spec.df[['test']])
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

# ==================== Line-level Metrics Calculation  ====================
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

# ==================== FETCH ranking  ====================
get.FETCH.ranking = function(SAT.base.info, GLANCE_F, spectrum.full, lambda_T, jaccard_threshold) {
  rank.df = SAT.base.info %>%
    left_join(GLANCE_F, by = c('filename', 'line.number')) %>%
    mutate(NT = replace_na(NT, 0), NFC = replace_na(NFC, 0)) %>%
    left_join(spectrum.full, by = c('filename', 'line.number')) %>%
    mutate(spectrum.score = replace_na(spectrum.score, 0),
           max_jaccard    = replace_na(max_jaccard, 0),
           is_persistent  = max_jaccard >= jaccard_threshold,
           Persistent     = ifelse(is_persistent, 1, 0))
  rank.df$baseScore = rank.df$NFC * rank.df$NT
  rank.df$FDE = log1p(rank.df$baseScore) + lambda_T * log1p(rank.df$spectrum.score)

  rank.df = rank.df %>%
    group_by(filename) %>%
    arrange(desc(Persistent), desc(FDE), Priority, line.number, .by_group = TRUE) %>%
    mutate(rank = row_number()) %>% ungroup()
  rank.df = select(rank.df, filename, line.number, rank)
  return(rank.df)
}

# ====================  SAT × Release: Preload  ====================

preload_one_release = function(SAT.result.dir, SATname, rel,
                                line.ground.truth.split, GLANCE_F.split,
                                cur.spectrum.df) {
  if (SATname == 'PMD' || SATname == 'ErrorProne') {
    allSATresult = read.csv(paste0(SAT.result.dir, rel, '-line-lvl-result.txt'), quote = '')
    if (SATname != 'ErrorProne') {
      allSATresult$filename <- gsub('/', '_', allSATresult$filename)
    }
  } else {
    allSATresult = read.csv(paste0(SAT.result.dir, rel, '-result.csv'))
  }


  cur.df.file = line.ground.truth.split[[rel]]

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


  GLANCE_F = GLANCE_F.split[[rel]]
  spectrum.result = cur.spectrum.df[[rel]]

  return(list(
    rel          = rel,
    SAT.base.info = SAT.base.info,
    GLANCE_F     = GLANCE_F,
    cur.df.file  = cur.df.file,
    spectrum.full = spectrum.result
  ))
}

# ====================  SAT × Release × τ: Rank Re-calculation + 6 Metric Calculation  ====================

evaluate_one_release_one_tau = function(preload, SATname, lambda_T, jaccard_threshold) {
  rel          = preload$rel
  SAT.base.info = preload$SAT.base.info
  GLANCE_F     = preload$GLANCE_F
  cur.df.file  = preload$cur.df.file
  spectrum.full = preload$spectrum.full

  # 与 τ 相关的部分: is_persistent + arrange + rank + 6 metric
  rank.df = SAT.base.info %>%
    left_join(GLANCE_F, by = c('filename', 'line.number')) %>%
    mutate(NT = replace_na(NT, 0), NFC = replace_na(NFC, 0)) %>%
    left_join(spectrum.full, by = c('filename', 'line.number')) %>%
    mutate(spectrum.score = replace_na(spectrum.score, 0),
           max_jaccard    = replace_na(max_jaccard, 0),
           is_persistent  = max_jaccard >= jaccard_threshold,
           Persistent     = ifelse(is_persistent, 1, 0))
  rank.df$baseScore = rank.df$NFC * rank.df$NT
  rank.df$FDE = log1p(rank.df$baseScore) + lambda_T * log1p(rank.df$spectrum.score)

  fetch.rank = rank.df %>%
    group_by(filename) %>%
    arrange(desc(Persistent), desc(FDE), Priority, line.number, .by_group = TRUE) %>%
    mutate(rank = row_number()) %>% ungroup()
  fetch.rank = select(fetch.rank, filename, line.number, rank)

  fetch.eval = get.line.metrics.result(fetch.rank, cur.df.file) %>%
    mutate(test = rel)

  return(fetch.eval)
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
  list(name = 'PMD',         dir = PMD.result.dir),
  list(name = 'CheckStyle',  dir = CheckStyle.result.dir),
  list(name = 'ErrorProne',  dir = ErrorProne.result.dir),
  list(name = 'Spotbugs',    dir = Spotbugs.result.dir),
  list(name = 'Codacy',      dir = codacy.result.dir),
  list(name = 'Codeql',      dir = codeql.result.dir),
  list(name = 'Sonarqube',   dir = sonarqube.result.dir)
)


tau.grid = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
default.tau = 0.7

METRICS  <- c('IFA', 'FPavg', 'FPA', 'Top1', 'Top3', 'Top5')
flip.metrics = c('IFA', 'FPavg')  

# ==================== Main Process ====================
cat('\n\n========== RQ3-A Overall Aggregation (Table-5-aligned, per-τ FETCH re-run) ==========\n\n')
cat(sprintf('[Configuration] τ grid = %s, default τ = %.1f, %d SATs × %d releases × %d τ = %d FETCH re-runs\n\n',
            paste(sprintf('%.1f', tau.grid), collapse = ', '),
            default.tau,
            length(sat.tools), length(all_eval_releases), length(tau.grid),
            length(sat.tools) * length(all_eval_releases) * length(tau.grid)))


all.tool.tau.release.df = NULL


for (tool in sat.tools) {
  cat(sprintf('\n----- 处理 %s -----\n', tool$name))


  spec.dir        = get.spectrum.dir(tool$name)
  cur.spectrum.df = load.spectrum.result(spec.dir,
                                         use.mapping = TRUE,
                                         SATname_for_mapping = tool$name)

  cur.spectrum.split <- split(cur.spectrum.df, cur.spectrum.df$test)

  cat('  [Preload] Preprocess 14 releases...\n')
  preloads = lapply(all_eval_releases, function(rel) {
    preload_one_release(tool$dir, tool$name, rel,
                         line.ground.truth.split, GLANCE_F.split,
                         cur.spectrum.split)
  })
  names(preloads) = all_eval_releases

  for (tau in tau.grid) {
    cat(sprintf('  ----- τ = %.1f -----\n', tau))

    fetch_result.df = NULL
    for (rel in all_eval_releases) {
      lambda_T = get_lambda_for_release(tool$name, 'Tarantula', rel)
      fetch.eval = evaluate_one_release_one_tau(preloads[[rel]], tool$name,
                                                  lambda_T, tau)
      fetch_result.df = rbind(fetch_result.df, fetch.eval)
      cat(sprintf('    [%s | τ=%.1f] finished %s\n', tool$name, tau, rel))
    }

    cur.release.df = fetch_result.df %>%
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
      mutate(technique = 'FETCH', SAT = tool$name, tool = tool$name, tau = tau) %>%
      mutate(project = sapply(test, test_to_project))

    all.tool.tau.release.df = rbind(all.tool.tau.release.df, cur.release.df)
  }
}

cat(sprintf('\n[Pre-aggregation] Total rows = %d (should be 7 × 14 × 10 = 980)\n',
            nrow(all.tool.tau.release.df)))

# ==================== Stage 2: project-level mean  ====================
project.df = all.tool.tau.release.df %>%
  filter(!is.na(project)) %>%
  summarise(
    IFA   = mean(IFA,   na.rm = TRUE),
    FPavg = mean(FPavg, na.rm = TRUE),
    FPA   = mean(FPA,   na.rm = TRUE),
    Top1  = mean(Top1,  na.rm = TRUE),
    Top3  = mean(Top3,  na.rm = TRUE),
    Top5  = mean(Top5,  na.rm = TRUE),
    .by = c(project, SAT, tau)
  )


# ==================== Stage 3: Global mean over 60 cells  ====================
overall.df = project.df %>%
  filter(if_all(c(IFA, FPavg, FPA, Top1, Top3, Top5), ~ !is.na(.x) & !is.nan(.x))) %>%
  group_by(tau) %>%
  summarise(across(all_of(METRICS), ~ mean(.x, na.rm = TRUE)),
            .groups = 'drop') %>%
  arrange(tau)

# ==================== Stage 4: Normalization  ====================
norm_one = function(x, flip) {
  rng = max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(0.5, length(x)))
  z = (x - min(x, na.rm = TRUE)) / rng
  if (flip) z = 1 - z
  return(z)
}

overall.norm.df = overall.df
for (m in METRICS) {
  overall.norm.df[[paste0(m, '_norm')]] = norm_one(overall.norm.df[[m]],
                                                    flip = (m %in% flip.metrics))
}

out.df = overall.norm.df %>%
  select(tau, IFA_norm, FPavg_norm, FPA_norm, Top1_norm, Top3_norm, Top5_norm)

for (col in names(out.df)) {
  if (col == 'tau') {
    out.df$tau = round(out.df$tau, 3)
  } else {
    out.df[[col]] = round(out.df[[col]], 3)
  }
}

norm.out = paste0(RQ3.base.dir, 'RQ3A_overall_norm.csv')
write.csv(out.df, file = norm.out, row.names = FALSE)

cat('\n===== Output =====\n')
cat(sprintf('  [CSV] norm overall:   %s  (10 τ × 7 columns: tau + 6 _norm metrics, main output)\n', norm.out))

# ==================== Console Print  ====================
fmt_row = function(tau, row) {
  sprintf('%.1f     %5.3f   %5.3f   %5.3f   %5.3f   %5.3f   %5.3f',
          tau, row$IFA_norm, row$FPavg_norm, row$FPA_norm,
          row$Top1_norm, row$Top3_norm, row$Top5_norm)
}

cat('\n----- τ=0.7 (default) -----\n')
row.07 = out.df %>% filter(tau == 0.7)
if (nrow(row.07) > 0) {
  cat('tau=0.7   IFA   FPavg   FPA   Top1   Top3   Top5\n')
  cat('------   ----   -----   ----   ----   ----   ----\n')
  cat(fmt_row(0.7, row.07), '\n')
}

cat('\n----- All 10 τ Normalization Results -----\n')
cat('tau    IFA   FPavg   FPA   Top1   Top3   Top5\n')
cat('----   ----   -----   ----   ----   ----   ----\n')
for (i in seq_len(nrow(out.df))) {
  cat(fmt_row(out.df$tau[i], out.df[i, ]), '\n')
}

cat('\n========== RQ3-A Overall Aggregation Completed ==========\n')
