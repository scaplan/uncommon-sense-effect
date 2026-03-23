
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Derive stimulus-level results based on the norming studies
###########################################################
###########################################################

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Derive stimulus-level results based on the norming studies...")


## For handling project structure / relative paths ##
suppressMessages(require("rprojroot"))
sourceDir <- find_root(is_git_root)
source(file.path(sourceDir, "aux", "aux-functions.R"))

args = commandArgs(trailingOnly=TRUE)
RUN_LIVE <- interactive()

if (RUN_LIVE) {
  basepath <- set_live_basepath() # Path in full path to git repo if running individual files live...
  inputpath <- paste(basepath, "data/merged/combined-all-exp-trials-nostim.rds", sep="")
  outputpathdata <- paste(basepath, "data/merged/combined-all-exp-trials.rds", sep="")
  outputpathtext <- paste(basepath, "data/merged/combined-all-exp-trials.csv", sep="")
}

output_message <- load_in_libraries()


if (length(args)>1) {
  inputpath <- args[1]
  outputbase <- args[2]
  outputpathdata <- paste0(outputbase, ".rds")
  outputpathtext <- paste0(outputbase, ".csv")
} else {
  print("Not enough input arguments")
}

df.orig.raw <- readRDS(inputpath)

df.orig.raw <- df.orig.raw %>%
  mutate(global.argid = dense_rank(match(FullArgument, unique(FullArgument)))) %>%
  relocate(global.argid, .after = ResponseType)
df.full.withstim <- df.orig.raw

df.orig <- df.orig.raw %>% filter(excl.subj == 0) %>% filter(excl.trial == 0)


if (RUN_LIVE) {
  # nrow(df.orig.raw)
  nrow(df.orig)
  table(df.orig$ExpNum)
}




df.N1.cross <- df.orig %>% filter(ExpNum == "N1-c-M1")
df.N2.cross <- df.orig %>% filter(ExpNum == "N2-p-M1")
if (RUN_LIVE) {
  nrow(df.N1.cross)
  nrow(df.N2.cross)
}

df.N3.bal <- df.orig %>% filter(ExpNum == "N3-c-M2")
df.N4.bal <- df.orig %>% filter(ExpNum == "N4-p-M2")
if (RUN_LIVE) {
  nrow(df.N3.bal)
  nrow(df.N4.bal)
}

df.N5.prime <- df.orig %>% filter(ExpNum == "N5-c-M3")
df.N6.prime <- df.orig %>% filter(ExpNum == "N6-p-M3")
if (RUN_LIVE) {
  nrow(df.N5.prime)
  nrow(df.N6.prime)
}



# df.N4.bal.stims <- df.N4.bal %>% select(premise1,premise2,conclusion,FullArgument) %>% unique()
# df.N4.bal.stims




append_stimlevel_to_specified_exps <- function(input.df.main,
                                               input.df.stim,
                                               joinCol,
                                               ExpsToUpdate,
                                               StimColsToUpdate) {
  df.full.withstim <- input.df.main %>%
    left_join(input.df.stim, by = joinCol, suffix = c("", ".new")) %>%
    mutate(
      across(
        all_of(StimColsToUpdate),
        ~ if_else(ExpNum %in% ExpsToUpdate,
                  coalesce(get(paste0(cur_column(), ".new")), .),
                  .)
      )
    ) %>%
    select(-ends_with(".new"))
  
  return(df.full.withstim)
}

taulate_stim_conclusion_numbers <- function(input.df) {
  df.conc.stats <- input.df %>%
    group_by(conclusion) %>%
    dplyr::summarize(stim.ConcTruth = mean(Response),
                     stim.ConcRT = mean(RT),
                     stim.Conc.N = n())
  return(df.conc.stats)
}

taulate_stim_P1_numbers <- function(input.df) {
  df.P1.stats <- input.df %>%
    filter(ResponseType == "answer-P1") %>%
    group_by(premise1) %>%
    dplyr::summarize(stim.premiseJudgment_P1 = mean(Response),
                     stim.premiseRT_P1 = mean(RT),
                     stim.N_P1 = n())
  return(df.P1.stats)
}

taulate_stim_P2_numbers <- function(input.df) {
  df.P1.stats <- input.df %>%
    filter(ResponseType == "answer-P2") %>%
    group_by(premise2) %>%
    dplyr::summarize(stim.premiseJudgment_P2 = mean(Response),
                     stim.premiseRT_P2 = mean(RT),
                     stim.N_P2 = n())
  return(df.P1.stats)
}




stim_conc_col_names <- c("stim.ConcTruth", "stim.ConcRT", "stim.Conc.N")
stim_P1_col_names <- c("stim.premiseJudgment_P1", "stim.premiseRT_P1", "stim.N_P1")
stim_P2_col_names <- c("stim.premiseJudgment_P2", "stim.premiseRT_P2", "stim.N_P2")

df.conc.M1 <- taulate_stim_conclusion_numbers(df.N1.cross)
df.full.withstim <- append_stimlevel_to_specified_exps(df.full.withstim, df.conc.M1, "conclusion",
                                                       c("M1-cross", "N1-c-M1", "N2-p-M1"),
                                                       stim_conc_col_names)

df.conc.M2 <- taulate_stim_conclusion_numbers(df.N3.bal)
df.full.withstim <- append_stimlevel_to_specified_exps(df.full.withstim, df.conc.M2, "conclusion",
                                                       c("M2-bal", "N3-c-M2", "N4-p-M2", "S1-load-M2"),
                                                       stim_conc_col_names)

df.conc.M3 <- taulate_stim_conclusion_numbers(df.N5.prime)
df.full.withstim <- append_stimlevel_to_specified_exps(df.full.withstim, df.conc.M3, "conclusion",
                                                       c("M3-prime", "N5-c-M3", "N6-p-M3"),
                                                       stim_conc_col_names)



df.P1.M1 <- taulate_stim_P1_numbers(df.N2.cross)
df.P2.M1 <- taulate_stim_P2_numbers(df.N2.cross)
df.full.withstim <- append_stimlevel_to_specified_exps(df.full.withstim, df.P1.M1, "premise1",
                                                       c("M1-cross", "N1-c-M1", "N2-p-M1"),
                                                       stim_P1_col_names)
df.full.withstim <- append_stimlevel_to_specified_exps(df.full.withstim, df.P2.M1, "premise2",
                                                       c("M1-cross", "N1-c-M1", "N2-p-M1"),
                                                       stim_P2_col_names)

df.P1.M2 <- taulate_stim_P1_numbers(df.N4.bal)
df.P2.M2 <- taulate_stim_P2_numbers(df.N4.bal)
df.full.withstim <- append_stimlevel_to_specified_exps(df.full.withstim, df.P1.M2, "premise1",
                                                       c("M2-bal", "N3-c-M2", "N4-p-M2", "S1-load-M2"),
                                                       stim_P1_col_names)
df.full.withstim <- append_stimlevel_to_specified_exps(df.full.withstim, df.P2.M2, "premise2",
                                                       c("M2-bal", "N3-c-M2", "N4-p-M2", "S1-load-M2"),
                                                       stim_P2_col_names)


df.P1.M3 <- taulate_stim_P1_numbers(df.N6.prime)
df.P2.M3 <- taulate_stim_P2_numbers(df.N6.prime)
df.full.withstim <- append_stimlevel_to_specified_exps(df.full.withstim, df.P1.M3, "premise1",
                                                       c("M3-prime", "N5-c-M3", "N6-p-M3"),
                                                       stim_P1_col_names)
df.full.withstim <- append_stimlevel_to_specified_exps(df.full.withstim, df.P2.M3, "premise2",
                                                       c("M3-prime", "N5-c-M3", "N6-p-M3"),
                                                       stim_P2_col_names)



# check2 <- df.full.withstim %>% select(ExpNum, premise1,premise2,conclusion,FullArgument, stim.premiseJudgment_P1) %>% unique()
# sum(is.na(check2$stim.premiseJudgment_P1))
# 
# 
# check2 %>%
#   group_by(ExpNum) %>%
#   summarise(
#     na_rows = sum(if_any(everything(), is.na)),
#     .groups = "drop"
#   )




write.csv(df.full.withstim, outputpathtext, row.names = F, quote=F)
saveRDS(df.full.withstim, outputpathdata)




