
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Generate / print LaTeX argument table
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Generate / print LaTeX argument tables...")


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

M1.stimtable <- df.M1.cross %>%
  distinct(
    FullArgument,
    TargetWord,
    premise1,
    premise2,
    conclusion,
    TrialType,
    Syllogism
  ) %>%
  rename(
    `Middle Term` = TargetWord,
    Condition     = TrialType,
    `Premise 1`    = premise1,
    `Premise 2`    = premise2,
    Conclusion    = conclusion
  ) %>% mutate(
    Condition = factor(
      Condition,
      levels = c(
        "Polysemy",
        "Homonymy",
        "Valid Filler",
        "Invalid Filler"
      )
    )
  ) %>%
  arrange(
    Condition,
    Syllogism,
    `Middle Term`
  ) %>%
  select(
    `Middle Term`,
    Condition,
    Syllogism,
    `Premise 1`,
    `Premise 2`,
    Conclusion
  )


M2.stimtable <- df.M2.bal %>%
  distinct(
    FullArgument,
    TargetWord,
    premise1,
    premise2,
    conclusion,
    TrialType,
    Syllogism
  ) %>%
  rename(
    `Middle Term` = TargetWord,
    Condition     = TrialType,
    `Premise 1`    = premise1,
    `Premise 2`    = premise2,
    Conclusion    = conclusion
  ) %>% mutate(
    Condition = factor(
      Condition,
      levels = c(
        "Polysemy",
        "Homonymy",
        "Valid Filler",
        "Invalid Filler"
      )
    )
  ) %>%
  arrange(
    Condition,
    Syllogism,
    `Middle Term`
  ) %>%
  select(
    `Middle Term`,
    Condition,
    Syllogism,
    `Premise 1`,
    `Premise 2`,
    Conclusion
  )



M3.stimtable <- df.M3.prime %>%
  distinct(
    FullArgument,
    TargetWord,
    premise1,
    premise2,
    conclusion,
    TrialType,
    Syllogism
  ) %>%
  rename(
    `Middle Term` = TargetWord,
    Condition     = TrialType,
    `Premise 1`    = premise1,
    `Premise 2`    = premise2,
    Conclusion    = conclusion
  ) %>% mutate(
    Condition = factor(
      Condition,
      levels = c(
        "Related",
        "Unrelated",
        "Valid Filler",
        "Invalid Filler"
      )
    )
  ) %>%
  arrange(
    Condition,
    Syllogism,
    `Middle Term`
  ) %>%
  select(
    `Middle Term`,
    Condition,
    Syllogism,
    `Premise 1`,
    `Premise 2`,
    Conclusion
  )


M1.stim_argument <- M1.stimtable %>%
  distinct(`Middle Term`, Condition, Syllogism, `Premise 1`, `Premise 2`, Conclusion) %>%
  mutate(
    Argument = str_c(`Premise 1`, `Premise 2`, Conclusion, sep = "\n")
  ) %>%
  arrange(Condition, Syllogism, `Middle Term`) %>%
  select(`Middle Term`, Condition, Syllogism, Argument) %>%
  mutate(
    Argument = kableExtra::linebreak(Argument)  
  ) %>%
  as.data.frame()

M1.table.tex <- kable(
  M1.stim_argument,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  escape = FALSE,
  linesep = "",  
  col.names = c("Middle Term", "Condition", "Syllogism", "Argument"),
  caption = "Experimental materials (one row per argument)"
) %>%
  kable_styling(latex_options = "repeat_header") %>%
  column_spec(4, width = "10cm")

writeLines(M1.table.tex, "M1-arguments-table.tex")





M2.stim_argument <- M2.stimtable %>%
  distinct(`Middle Term`, Condition, Syllogism, `Premise 1`, `Premise 2`, Conclusion) %>%
  mutate(
    Argument = str_c(`Premise 1`, `Premise 2`, Conclusion, sep = "\n")
  ) %>%
  arrange(Condition, Syllogism, `Middle Term`) %>%
  select(`Middle Term`, Condition, Syllogism, Argument) %>%
  mutate(
    Argument = kableExtra::linebreak(Argument)  
  ) %>%
  as.data.frame()

M2.table.tex <- kable(
  M2.stim_argument,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  escape = FALSE,
  linesep = "",   
  col.names = c("Middle Term", "Condition", "Syllogism", "Argument"),
  caption = "Experimental materials (one row per argument)"
) %>%
  kable_styling(latex_options = "repeat_header") %>%
  column_spec(4, width = "10cm")

writeLines(M2.table.tex, "M2-arguments-table.tex")




M3.stim_argument <- M3.stimtable %>%
  distinct(`Middle Term`, Condition, Syllogism, `Premise 1`, `Premise 2`, Conclusion) %>%
  mutate(
    Argument = str_c(`Premise 1`, `Premise 2`, Conclusion, sep = "\n")
  ) %>%
  arrange(Condition, Syllogism, `Middle Term`) %>%
  select(`Middle Term`, Condition, Syllogism, Argument) %>%
  mutate(
    Argument = kableExtra::linebreak(Argument)  
  ) %>%
  mutate(
    `Middle Term` = gsub("_", "\n", `Middle Term`),
    `Middle Term` = kableExtra::linebreak(`Middle Term`)
  )%>%
  as.data.frame()

M3.table.tex <- kable(
  M3.stim_argument,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  escape = FALSE,
  linesep = "",  
  col.names = c("Middle Term", "Condition", "Syllogism", "Argument"),
  caption = "Experimental materials (one row per argument)"
) %>%
  kable_styling(latex_options = "repeat_header") %>%
  column_spec(4, width = "10cm")

writeLines(M3.table.tex, "M3-arguments-table.tex")
