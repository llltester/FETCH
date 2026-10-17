# RQ2 v3: Ablation Study of FETCH (Contribution Analysis) 
#
# 4 ranking variants (per tool x per release):
#   V1  Current-only      : (-CCE,                Priority, LineNumber)        SAT priority + current-version code characteristics
#   V2  Current-only+EP   : (-EP, -CCE,           Priority, LineNumber)        + persistence (binary)
#   V3  Current-only+HDE  : (-FDE,                Priority, LineNumber)        + historical/temporal token-level suspiciousness (via FDE)
#   V4  FETCH (full)      : (-EP, -FDE,           Priority, LineNumber)        complete fusion
#
# CCE      = NFC * NT                    (current-version code characteristics)
# FDE      = log1p(CCE) + lambda_T * log1p(specScore)   (complete temporal evidence)
# EP       = is_persistent = (max_jaccard >= 0.7)        (tau = 0.7, same as RQ1)
#

library(tidyverse)

# ==================== Path Configuration ====================
BASE = '../'

SAT_DIR = paste0(BASE, 'SATs_initial_scan_result/')

PMD.result.dir       = paste0(SAT_DIR, 'PMD/')
CheckStyle.result.dir = paste0(SAT_DIR, 'CheckStyle/')
ErrorProne.result.dir = paste0(SAT_DIR, 'Errorprone/')      
Spotbugs.result.dir  = paste0(SAT_DIR, 'Spotbugs/')
codacy.result.dir    = paste0(SAT_DIR, 'codacy/')
codeql.result.dir    = paste0(SAT_DIR, 'codeql/')
sonarqube.result.dir = paste0(SAT_DIR, 'sonarqube/')

# Spectrum (FETCH) - only Tarantula for RQ2
spectrum.base.dir    = paste0(BASE, 'SATs_result/')
spectrum.subdir      = 'TarantulaWithoutCA'

Mapping.dir = paste0(BASE, 'datasets/map-warning-lines-only/')

# GLANCE metrics (CCE = NFC * NT)
CEandNFCdir = paste0(BASE, 'Baseline/GLANCE_and_LineDP/result/Glance_MD_full_threshold/line_result/test/')

# lambda_T (same source as RQ1)
lambda_file = paste0(BASE, 'Auto_lambda_Per_release_Results.txt')

# Output directory (script's own folder: FETCH/RQ2/)
RQ2.save.dir = './'
if (!dir.exists(RQ2.save.dir)) {
  dir.create(RQ2.save.dir, recursive = TRUE)
}

# v2 -> v3 pairs (same as RQ1, used for lambda lookup)
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

# ==================== Project -> Releases mapping ====================
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

# ==================== Load lambda_T ====================
lambda_lines = readLines(lambda_file)
per_release_lambda = list()
current_sat  = NULL
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
    pair       = sub('^    ([^:]+): ([0-9.]+)$', '\\1', line)
    lambda_val = as.numeric(sub('^    [^:]+: ([0-9.]+)$', '\\1', line))
    key        = paste0(current_sat, '_', current_spec)
    if (is.null(per_release_lambda[[key]])) per_release_lambda[[key]] = list()
    per_release_lambda[[key]][[pair]] = lambda_val
  }
}

cat(sprintf('[Lambda source] %s | loaded %d (SAT, Spectrum) groups\n',
            basename(lambda_file), length(per_release_lambda)))

get_lambda_for_release = function(sat_name, spectrum_name, v3_release) {
  key = paste0(sat_name, '_', spectrum_name)
  if (is.null(per_release_lambda[[key]])) return(0.5)
  for (pair in v2_v3_pairs) {
    if (pair$v3 == v3_release) {
      pair_key = paste0(pair$v2, '->', pair$v3)
      val      = per_release_lambda[[key]][[pair_key]]
      if (is.null(val)) return(0.5)
      return(val)
    }
  }
  return(0.5)
}

# ==================== Ground Truth ====================
# (Reuse RQ1's DeepLineDP files as ground truth source; contains line.level.ground.truth)
prediction_dir = paste0(BASE, 'Baseline/DeepLineDP/output/prediction/DeepLineDP/within-release/')
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

# ==================== Spectrum loaders ====================
get.spectrum.dir = function(sat.name) {
  paste0(spectrum.base.dir, sat.name, '/Result/', spectrum.subdir, '/line_result/test/')
}

load.spectrum.result = function(spectrum.dir, use.mapping = FALSE, SATname_for_mapping = NULL) {
  all_files = list.files(spectrum.dir)
  spec.df   = NULL
  for (f in all_files) {
    releasename <- sub('-result\\.csv$', '', f)
    df <- read.csv(paste0(spectrum.dir, f), stringsAsFactors = FALSE)
    df$test <- releasename
    spec.df <- rbind(spec.df, df)
  }
  colnames(spec.df)[2] <- 'spectrum.score'
  temp          <- strsplit(spec.df[[1]], ':', fixed = TRUE)
  filename      <- sapply(temp, function(x) paste(x[-length(x)], collapse = ':'))
  line.number   <- as.integer(sapply(temp, function(x) x[length(x)]))
  spectrum.score <- as.numeric(spec.df[['spectrum.score']])
  max_jaccard   <- as.numeric(spec.df[['max_jaccard']])
  test          <- spec.df[['test']]
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

# ==================== Metric computation  ====================
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

# ==================== Main driver: 4 variants of RQ2  ====================
get.RQ2.result = function(all_eval_releases, SAT.result.dir, SATname,
                          line.ground.truth, lineLevelMetrics,
                          spectrum.result.df,
                          jaccard_threshold = 0.70) {

  # ===== Stage 1: per-variant file-level result containers =====
  V1.result.df = NULL  # Current-only            : (-CCE,              Priority, LineNumber)
  V2.result.df = NULL  # Current-only+EP         : (-EP, -CCE,         Priority, LineNumber)
  V3.result.df = NULL  # Current-only+HDE        : (-FDE,              Priority, LineNumber)
  V4.result.df = NULL  # FETCH (full)            : (-EP, -FDE,         Priority, LineNumber)

  for (rel in all_eval_releases) {
    # ---- Load SAT result ----
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

    # ---- Prepare GLANCE / spectrum for this release ----
    GLANCE_F = select(lineLevelMetrics, test, filename, line.number, NT, NFC) %>%
      filter(test == rel) %>% select(filename, line.number, NT, NFC)

    spectrum.result = spectrum.result.df %>% filter(test == rel)

    # Recompute is_persistent from jaccard_threshold 
    spectrum.result$is_persistent = spectrum.result$max_jaccard >= jaccard_threshold

    # ---- Single master join: SAT.base.info + GLANCE + spectrum ----
    lambda_T = get_lambda_for_release(SATname, 'Tarantula', rel)

    master = SAT.base.info %>%
      left_join(GLANCE_F, by = c('filename', 'line.number')) %>%
      left_join(spectrum.result, by = c('filename', 'line.number')) %>%
      mutate(NT = replace_na(NT, 0),
             NFC = replace_na(NFC, 0),
             baseScore = NFC * NT,
             spectrum.score = replace_na(spectrum.score, 0),
             max_jaccard = replace_na(max_jaccard, 0),
             is_persistent = replace_na(is_persistent, FALSE),
             Persistent = ifelse(is_persistent, 1, 0),
             specScore.log = log1p(spectrum.score),
             FDE = log1p(baseScore) + lambda_T * log1p(spectrum.score))

    # ============ V1: Current-only  (-CCE, Priority, LineNumber) ============
    V1.rank = master %>%
      group_by(filename) %>%
      arrange(-baseScore, Priority, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    V1.rank = select(V1.rank, filename, line.number, rank)
    V1.eval = get.line.metrics.result(V1.rank, cur.df.file) %>% mutate(test = rel)

    # ============ V2: Current-only+EP  (-EP, -CCE, Priority, LineNumber) ============
    V2.rank = master %>%
      group_by(filename) %>%
      arrange(desc(Persistent), -baseScore, Priority, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    V2.rank = select(V2.rank, filename, line.number, rank)
    V2.eval = get.line.metrics.result(V2.rank, cur.df.file) %>% mutate(test = rel)

    # ============ V3: Current-only+HDE  (-FDE, Priority, LineNumber) ============
    V3.rank = master %>%
      group_by(filename) %>%
      arrange(-FDE, Priority, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    V3.rank = select(V3.rank, filename, line.number, rank)
    V3.eval = get.line.metrics.result(V3.rank, cur.df.file) %>% mutate(test = rel)

    # ============ V4: FETCH  (-EP, -FDE, Priority, LineNumber) ============
    V4.rank = master %>%
      group_by(filename) %>%
      arrange(desc(Persistent), -FDE, Priority, line.number, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>% ungroup()
    V4.rank = select(V4.rank, filename, line.number, rank)
    V4.eval = get.line.metrics.result(V4.rank, cur.df.file) %>% mutate(test = rel)

    # ============ Stage 1 accumulation ============
    V1.result.df = rbind(V1.result.df, V1.eval)
    V2.result.df = rbind(V2.result.df, V2.eval)
    V3.result.df = rbind(V3.result.df, V3.eval)
    V4.result.df = rbind(V4.result.df, V4.eval)

    cat(sprintf('  [%s] finished %s\n', SATname, rel))
  }

  # ============ Stage 1 -> release-level aggregation  ============
  sum_V1 = V1.result.df %>%
    summarise(IFA = median(ifa.list), FPA = median(fpa.list),
              top1 = mean(top1.list), top3 = mean(top3.list),
              top5 = mean(top5.list), FPavg = median(FPavg.list),
              .by = test) %>%
    mutate(technique = 'Current-only')

  sum_V2 = V2.result.df %>%
    summarise(IFA = median(ifa.list), FPA = median(fpa.list),
              top1 = mean(top1.list), top3 = mean(top3.list),
              top5 = mean(top5.list), FPavg = median(FPavg.list),
              .by = test) %>%
    mutate(technique = 'Current+EP')

  sum_V3 = V3.result.df %>%
    summarise(IFA = median(ifa.list), FPA = median(fpa.list),
              top1 = mean(top1.list), top3 = mean(top3.list),
              top5 = mean(top5.list), FPavg = median(FPavg.list),
              .by = test) %>%
    mutate(technique = 'Current+HDE')

  sum_V4 = V4.result.df %>%
    summarise(IFA = median(ifa.list), FPA = median(fpa.list),
              top1 = mean(top1.list), top3 = mean(top3.list),
              top5 = mean(top5.list), FPavg = median(FPavg.list),
              .by = test) %>%
    mutate(technique = 'FETCH')

  all.release.df = rbind(sum_V1, sum_V2, sum_V3, sum_V4)
  return(all.release.df)
}

# ==================== Test releases ====================
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
  list(name = 'PMD',         dir = PMD.result.dir,        threshold = 0.70),
  list(name = 'CheckStyle',  dir = CheckStyle.result.dir,  threshold = 0.70),
  list(name = 'ErrorProne',  dir = ErrorProne.result.dir, threshold = 0.70),
  list(name = 'Spotbugs',    dir = Spotbugs.result.dir,    threshold = 0.70),
  list(name = 'Codacy',      dir = codacy.result.dir,      threshold = 0.70),
  list(name = 'Codeql',      dir = codeql.result.dir,      threshold = 0.70),
  list(name = 'Sonarqube',   dir = sonarqube.result.dir,   threshold = 0.70)
)

variant.order = c('Current-only', 'Current+EP', 'Current+HDE', 'FETCH')

# ==================== Run ====================
cat("\n\n========== RQ2 v3: Ablation Study of FETCH (4 Variants, Plan A) ==========\n\n")

all.tools.release.df = NULL

for (tool in sat.tools) {
  cat(sprintf('----- Processing %s -----\n', tool$name))

  # Load Tarantula spectrum once per tool (identical to RQ2_ablation.R)
  spec.dir         = get.spectrum.dir(tool$name)
  cur.spectrum.df  = load.spectrum.result(spec.dir,
                                          use.mapping = TRUE,
                                          SATname_for_mapping = tool$name)

  cur.release.df = get.RQ2.result(
    all_eval_releases   = all_eval_releases,
    SAT.result.dir      = tool$dir,
    SATname             = tool$name,
    line.ground.truth   = line.ground.truth,
    lineLevelMetrics    = lineLevelMetrics,
    spectrum.result.df  = cur.spectrum.df,
    jaccard_threshold   = tool$threshold
  )

  cur.release.df = cur.release.df %>% mutate(tool = tool$name)
  all.tools.release.df = rbind(all.tools.release.df, cur.release.df)
}

# Inject project column from release name (test -> project)
all.tools.release.df = all.tools.release.df %>%
  mutate(project = sapply(test, test_to_project))

cat('\nProject distribution after Stage 1 (release-level rows per (tool, project)):\n')
print(table(all.tools.release.df$tool, all.tools.release.df$project, useNA = 'ifany'))

# ==================== Aggregation ====================

project_results <- all.tools.release.df %>%
  summarise(
    IFA   = mean(IFA,   na.rm = TRUE),
    FPavg = mean(FPavg, na.rm = TRUE),
    FPA   = mean(FPA,   na.rm = TRUE),
    top1  = mean(top1,  na.rm = TRUE),
    top3  = mean(top3,  na.rm = TRUE),
    top5  = mean(top5,  na.rm = TRUE),
    .by = c(tool, project, technique)
  )

cat(sprintf('\n  project-level: %d rows\n', nrow(project_results)))

agg_mean = project_results %>%
  summarise(
    IFA   = mean(IFA,   na.rm = TRUE),
    FPavg = mean(FPavg, na.rm = TRUE),
    FPA   = mean(FPA,   na.rm = TRUE),
    top1  = mean(top1,  na.rm = TRUE),
    top3  = mean(top3,  na.rm = TRUE),
    top5  = mean(top5,  na.rm = TRUE),
    .by = c(tool, technique)
  )

cat(sprintf('  tool-level:    %d rows\n', nrow(agg_mean)))

# Display order: Current-only -> Current+EP -> Current+HDE -> FETCH
variant.display.order = variant.order

to.csv = function(agg.df, label) {
  out = agg.df %>%
    rename(Tool = tool,
           `Feature combination` = technique,
           `Top-1` = top1, `Top-3` = top3, `Top-5` = top5) %>%
    mutate(Tool = factor(Tool,
                         levels = c('PMD', 'CheckStyle', 'ErrorProne',
                                    'Spotbugs', 'Codacy', 'Codeql',
                                    'Sonarqube')),
           `Feature combination` = factor(`Feature combination`,
                                          levels = variant.display.order)) %>%
    arrange(Tool, `Feature combination`) %>%
    select(Tool, `Feature combination`, IFA, FPavg, FPA, `Top-1`, `Top-3`, `Top-5`)
  return(out)
}

mean.csv = to.csv(agg_mean, 'mean')

# ==================== Write CSV ====================
write.csv(mean.csv,
          file = paste0(RQ2.save.dir, 'RQ2_overall_six_metric.csv'),
          row.names = FALSE)

cat(sprintf('\n[RQ2 v3] CSV written:\n'))
cat(sprintf('  - %sRQ2_overall_six_metric.csv    (%d rows)\n',
            RQ2.save.dir, nrow(mean.csv)))

# ==================== Paper Table: Wide FPA + Top-1 (mean) ====================
# Layout requested by the user:
#                  FPA                              Top-1
#   Tools  Current-only  +EP  +HDE  FETCH  Current-only  +EP  +HDE  FETCH
#   PMD
#   ...
#   Sonarqube
#
# Wide format:  7 rows (one per tool) x 9 columns
#     1 x Tools + 4 x FPA variants + 4 x Top-1 variants

variant_order_short = c('Current-only', '+EP', '+HDE', 'FETCH')

fpa_top1_wide = agg_mean %>%
  rename(Tool = tool,
         `Feature combination` = technique,
         TOP1 = top1) %>%
  mutate(Tool = factor(Tool,
                       levels = c('PMD', 'CheckStyle', 'ErrorProne',
                                  'Spotbugs', 'Codacy', 'Codeql',
                                  'Sonarqube')),
         `Feature combination` = factor(`Feature combination`,
                                        levels = variant.display.order,
                                        labels = variant_order_short)) %>%
  arrange(Tool, `Feature combination`)

# Pivot FPA: tools as rows, variants as columns
fpa_wide = fpa_top1_wide %>%
  select(Tool, `Feature combination`, FPA) %>%
  pivot_wider(names_from   = `Feature combination`,
              values_from  = FPA,
              names_prefix = 'FPA.')

# Pivot Top-1: tools as rows, variants as columns
top1_wide = fpa_top1_wide %>%
  select(Tool, `Feature combination`, TOP1) %>%
  pivot_wider(names_from   = `Feature combination`,
              values_from  = TOP1,
              names_prefix = 'Top1.')

# Final wide table:  Tools | FPA.x4 | Top1.x4
fpa_top1_mean_wide = fpa_wide %>%
  inner_join(top1_wide, by = 'Tool') %>%
  arrange(Tool)

write.csv(fpa_top1_mean_wide,
          file = paste0(RQ2.save.dir, 'RQ2_FPA_Top1.csv'),
          row.names = FALSE)

cat(sprintf('  - %sRQ2_FPA_Top1.csv  (%d rows x %d cols)\n',
            RQ2.save.dir, nrow(fpa_top1_mean_wide),
            ncol(fpa_top1_mean_wide)))
cat('\n========== RQ2 v3 experiment complete ==========\n')
