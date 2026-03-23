
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Main descriptive results
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Calculate main descriptive results...")


## For handling project structure / relative paths ##
suppressMessages(require("rprojroot"))
sourceDir <- find_root(is_git_root)
source(file.path(sourceDir, "aux", "aux-functions.R"))
source(file.path(sourceDir, "aux", "aux-plot-aesthetics.R"))

args = commandArgs(trailingOnly=TRUE)
RUN_LIVE <- interactive()

if (RUN_LIVE) {
  basepath <- set_live_basepath() # Path in full path to git repo if running individual files live...
  inputpath <- paste0(basepath, "data/merged/combined-all-exp-trials.rds")
  outputdir <- paste0(basepath, "output/stats/")
}

output_message <- load_in_libraries()
plot_library_status <- load_in_plotting_libraries()
load_in_plot_aesthetics()


if (length(args)>1) {
  inputpath <- args[1]
  outputdir <- args[2]
} else {
  print("Not enough input arguments")
}

setwd(outputdir)


df.orig.raw <- readRDS(inputpath)

## Checking total N per experiment
cross_experiment_subject_counts <- df.orig.raw %>% distinct(SubjectID, ExpNum, excl.subj) %>%
  count(ExpNum, excl.subj) %>%
  pivot_wider(names_from = excl.subj, 
              values_from = n, 
              values_fill = 0) %>%
  rename(retain = `0`, exclude = `1`) %>%
  mutate(total_recruited = retain + exclude) %>%
  bind_rows(
    summarise(., across(where(is.numeric), sum),
              ExpNum = "TOTAL")
  )
if (RUN_LIVE) { cross_experiment_subject_counts} 

df.orig <- df.orig.raw %>% filter(excl.subj == 0) %>% filter(excl.trial == 0) %>% filter(RT < 50000)
if (RUN_LIVE) { nrow(df.orig) }
rm(df.orig.raw)

if (RUN_LIVE) { df.orig %>% filter(ExpNum == "S1-load-M2") %>% distinct(loadtime, SubjectID) %>% count(loadtime) }
if (RUN_LIVE) { df.orig %>% distinct(global.argid, ExpNum, TrialType) %>% count(ExpNum, TrialType) }


if (RUN_LIVE) {
  # nrow(df.orig)
  table(df.orig$ExpNum)
}

df.M1.cross <- df.orig %>% filter(ExpNum == "M1-cross")
df.M2.bal <- df.orig %>% filter(ExpNum == "M2-bal")
df.M3.prime <- df.orig %>% filter(ExpNum == "M3-prime")
df.S1.load <- df.orig %>% filter(ExpNum == "S1-load-M2")
rm(df.orig)
###########################################
###########################################

###########################################
## Descriptive Statistics
output_desc_stats <- function(df, exp_name, output_file, 
                                  trial_type_levels = c("Valid Filler", "Polysemy", "Homonymy", "Invalid Filler"),
                                  run_live = RUN_LIVE) {
  
  # Reorder TrialType factor
  df_ordered <- df %>%
    mutate(TrialType = factor(TrialType, levels = trial_type_levels))
  
  # Generate descriptive statistics
  descriptives <- df_ordered %>% 
    group_by(TrialType) %>% 
    dplyr::summarise(
      N_trials = n(),
      N_subjects = n_distinct(SubjectID),
      ArgResponse = mean(Response),
      ArgResponse_SE = sd(Response) / sqrt(n()),
      ConcStrength_mean = mean(stim.ConcTruth),
      ConcStrength_SD = sd(stim.ConcTruth),
      Premise1_mean = mean(stim.premiseJudgment_P1),
      Premise1_SD = sd(stim.premiseJudgment_P1),
      Premise2_mean = mean(stim.premiseJudgment_P2),
      Premise2_SD = sd(stim.premiseJudgment_P2),
      RT_mean = mean(RT, na.rm = TRUE),
      RT_SD = sd(RT, na.rm = TRUE)
    )
  
  # Save to file
  print_or_save(
    descriptives,
    file = output_file,
    header = paste("###", exp_name, "- Descriptive Statistics by Trial Type")
  )
  
  return(descriptives)
}


if (!RUN_LIVE) {
  descriptives_file <- paste0(outputdir, "M1-M2-M3-S1-descriptive_stats.txt")
  file.create(descriptives_file)
}


M1_descriptives <- output_desc_stats(df.M1.cross, "Exp 1 (Fully Crossed)", descriptives_file)
M2_descriptives <- output_desc_stats(df.M2.bal, "Exp 2 (Balanced Conclusions)", descriptives_file)

M3_descriptives <- output_desc_stats(df.M3.prime, "Exp 3 (Semantic Relatedness)", descriptives_file,
                                     trial_type_levels = c("Valid Filler", "Related", "Unrelated", "Invalid Filler")
                                     )

S1_descriptives <- output_desc_stats(df.S1.load, "SI Exp 1 (Cognitive Load)", descriptives_file)




S1_descriptives_750ms <- output_desc_stats(df.S1.load %>% filter(loadtime == "750ms"), "SI Exp 1 (Cognitive Load)", descriptives_file)
S1_descriptives_1500ms <- output_desc_stats(df.S1.load %>% filter(loadtime == "1500ms"), "SI Exp 1 (Cognitive Load)", descriptives_file)
S1_descriptives_3dig <- output_desc_stats(df.S1.load %>% filter(loadtime == "ThreeDig"), "SI Exp 1 (Cognitive Load)", descriptives_file)
S1_descriptives_5dig <- output_desc_stats(df.S1.load %>% filter(loadtime == "FiveDig"), "SI Exp 1 (Cognitive Load)", descriptives_file)

three.dig.df <- df.S1.load %>% filter(loadtime == "ThreeDig")
five.dig.df <- df.S1.load %>% filter(loadtime == "FiveDig")

three.dig.df$digitMatch <- as.numeric(as.factor(three.dig.df$digitMatch)) - 2
five.dig.df$digitMatch <- as.numeric(as.factor(five.dig.df$digitMatch)) - 2

# t.test(three.dig.df$digitMatch, five.dig.df$digitMatch)

if (RUN_LIVE) { mean(three.dig.df$digitMatch) }
if (RUN_LIVE) { mean(five.dig.df$digitMatch) }

threeDig.bysubj <- three.dig.df %>% group_by(SubjectID) %>% dplyr::summarise(DigMatchMean = mean(digitMatch), DigSD = sd(digitMatch))
fiveDig.bysubj <- five.dig.df %>% group_by(SubjectID) %>% dplyr::summarise(DigMatchMean = mean(digitMatch))

if (RUN_LIVE) { sd(threeDig.bysubj$DigMatchMean) }
if (RUN_LIVE) { sd(fiveDig.bysubj$DigMatchMean) }

if (RUN_LIVE) { t.test(threeDig.bysubj$DigMatchMean, fiveDig.bysubj$DigMatchMean) } 

S1_descriptives_5dig.correct <- output_desc_stats(df.S1.load %>% filter(loadtime == "FiveDig") %>% filter(digitMatch == "1"), "SI Exp 1 (Cognitive Load)", descriptives_file)



### RTs
# 
M1.rts <- df.M1.cross %>%
  mutate(TrialType = factor(TrialType, levels = c("Valid Filler", "Polysemy", "Homonymy", "Invalid Filler"))) %>%
  group_by(TrialType, Response) %>%
  dplyr::summarise(
    N_trials = n(),
    N_subjects = n_distinct(SubjectID),
    RT_mean = mean(RT, na.rm = TRUE),
    RT_SD = sd(RT, na.rm = TRUE)
  )
if (RUN_LIVE) { M1.rts }


M1.subject.rts <- df.M1.cross %>% group_by(SubjectID) %>% dplyr::summarize(meanRT = mean(RT, na.rm = TRUE))



# 
# 
M2.rts <- df.M2.bal %>%
  mutate(TrialType = factor(TrialType, levels = c("Valid Filler", "Polysemy", "Homonymy", "Invalid Filler"))) %>%
  group_by(TrialType, Response) %>%
  dplyr::summarise(
    N_trials = n(),
    N_subjects = n_distinct(SubjectID),
    RT_mean = mean(RT, na.rm = TRUE),
    RT_SD = sd(RT, na.rm = TRUE)
  )
if (RUN_LIVE) { M2.rts }


S1.rts <- df.S1.load %>%
  mutate(TrialType = factor(TrialType, levels = c("Valid Filler", "Polysemy", "Homonymy", "Invalid Filler"))) %>%
  filter(loadtype == "timing") %>%
  group_by(loadtime, TrialType, Response) %>%
  dplyr::summarise(
    N_trials = n(),
    N_subjects = n_distinct(SubjectID),
    RT_mean = mean(RT, na.rm = TRUE),
    RT_SD = sd(RT, na.rm = TRUE)
  )
if (RUN_LIVE) { S1.rts }

