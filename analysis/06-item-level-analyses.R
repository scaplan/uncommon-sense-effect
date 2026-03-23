
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Stimulus/item-level boost
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Item-level analysis...")


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
  outputdir_stats <- paste0(basepath, "output/stats/")
  outputdir_plots <- paste0(basepath, "output/plots/")
}

output_message <- load_in_libraries()
plot_library_status <- load_in_plotting_libraries()
load_in_plot_aesthetics()


if (length(args)>2) {
  inputpath <- args[1]
  outputdir_stats <- args[2]
  outputdir_plots <- args[3]
} else {
  print("Not enough input arguments")
}

setwd(outputdir_plots)


df.orig.raw <- readRDS(inputpath)
df.orig <- df.orig.raw %>% filter(excl.subj == 0) %>% filter(excl.trial == 0)
if (RUN_LIVE) { nrow(df.orig) }
rm(df.orig.raw)

if (RUN_LIVE) {
  # nrow(df.orig)
  table(df.orig$ExpNum)
} else {
    boost_file <- paste0(outputdir_stats, "Item-level-boost-numbers.txt")
    file.create(boost_file)
}



analyze_item_boost <- function(df, trial_types, ref_level, out_file, currexp) {
  item.boost <- tally_item_level_boost(df) %>% 
    filter(TrialType %in% trial_types) %>% 
    droplevels()
  item.boost$TrialType <- relevel(item.boost$TrialType, ref = ref_level)
  
  summary_stats <- item.boost %>% 
    group_by(TrialType) %>% 
    dplyr::summarize(Arg = mean(mean),Conc = mean(SoloConc), Boost = mean(Boost))
  
  model <- lm(Boost ~ TrialType, data = item.boost)
  
  
  print_or_save(
    summary_stats,
    file = out_file, header = paste("\n--- Group-level boost summary ---\n", currexp)
  )
  print_or_save(
    item.boost,
    file = out_file, header = paste("\n--- Item-level boost summary ---\n", currexp)
  )
  print_or_save(
    summary(model),
    file = out_file, header = paste("\n###", currexp,
                                      "Item-level polysemy boost (simple lm)")
  )
  return(model)
}




###################################################################################
###################################################################################

df.M1.cross <- df.orig %>% filter(ExpNum == "M1-cross")
df.M2.bal <- df.orig %>% filter(ExpNum == "M2-bal")
df.M3.prime <- df.orig %>% filter(ExpNum == "M3-prime")
df.S1.load <- df.orig %>% filter(ExpNum == "S1-load-M2")

df.M1.byWordSummary <- df.M1.cross %>%
  group_by(TargetWord, TrialType, ExpNum) %>%
  summarise(ArgResponse = mean(Response),
            n_Arg = n(),
            se_Arg = sd(Response)/sqrt(n()),
            ConcAvg = mean(stim.ConcTruth),
            boost = ArgResponse - ConcAvg
  ) %>% ungroup()

df.M2.byWordSummary <- df.M2.bal %>%
  group_by(TargetWord, TrialType, ExpNum) %>%
  summarise(ArgResponse = mean(Response),
            n_Arg = n(),
            se_Arg = sd(Response)/sqrt(n()),
            ConcAvg = mean(stim.ConcTruth),
            boost = ArgResponse - ConcAvg
  ) %>% ungroup()

df.M3.byWordSummary <- df.M3.prime %>%
  group_by(TargetWord, TrialType, ExpNum) %>%
  summarise(ArgResponse = mean(Response),
            n_Arg = n(),
            se_Arg = sd(Response)/sqrt(n()),
            ConcAvg = mean(stim.ConcTruth),
            boost = ArgResponse - ConcAvg
  ) %>% ungroup()

df.byWord.M1.M2 <- bind_rows(df.M1.byWordSummary, df.M2.byWordSummary)

df.byWord.M1.M2.P <- df.byWord.M1.M2 %>%
  filter(TrialType == "Polysemy") %>%
  mutate(changeDir = case_when(boost > 0 ~ "grow",
                               TRUE ~ "drop"))


if (RUN_LIVE) { df.M1.byWordSummary %>% group_by(TrialType) %>% dplyr::summarise(mean = mean(boost), n = n(), sd = sd(boost)) }
if (RUN_LIVE) { df.M2.byWordSummary %>% group_by(TrialType) %>% dplyr::summarise(mean = mean(boost), n = n(), sd = sd(boost)) }
if (RUN_LIVE) { df.M3.byWordSummary %>% group_by(TrialType) %>% dplyr::summarise(mean = mean(boost), n = n(), sd = sd(boost)) }


dodge_width <- 0.2

pl.dumbbell <- df.byWord.M1.M2.P %>%
  arrange(ConcAvg) %>%
  mutate(
    TargetWord = factor(TargetWord, levels = unique(TargetWord)),
    dodge_offset = ifelse(ExpNum == "M1-cross", -dodge_width/2, dodge_width/2),
    TargetWord_num = as.numeric(TargetWord) + dodge_offset,
    arrow_x = ifelse(changeDir == "grow", ArgResponse - 0.01, ArgResponse + 0.01)
  ) %>%
  ggplot(aes(y = TargetWord_num, yend = TargetWord_num)) +
  geom_segment(aes(x = ConcAvg, xend = ArgResponse, color = changeDir),
               linewidth = 2.5) +
  geom_segment(aes(x = arrow_x, xend = ArgResponse, color = ExpNum),
               linewidth = 3.5,
               arrow = arrow(length = unit(0.2, "inches"))) +
  scale_color_manual(
    breaks = c("M1-cross", "M2-bal", "grow", "drop"),
    values = c("M1-cross" = exp1_color, "M2-bal" = exp2_color,
               "grow" = grow_color, "drop" = drop_color),
    labels = c(
      "M1-cross" = "Exp 1",
      "M2-bal" = "Exp 2",
      "drop" = "Null effect",
      "grow" = "Arg. boost (USE)"
    )) + 
  scale_y_continuous(
    breaks = function(x) seq_len(length(unique(df.byWord.M1.M2.P$TargetWord))),
    labels = function(x) levels(factor(df.byWord.M1.M2.P$TargetWord, 
                                       levels = unique(arrange(df.byWord.M1.M2.P, ConcAvg)$TargetWord)))
  ) +
  single_pane_theme_withLegend(c(0.23, 0.89)) +
  theme(legend.background = element_rect(color = "black", linewidth = 0.5, fill = "white"),
        legend.box.background = element_rect(color = "black", linewidth = 0.5),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  labs(x = "Mean Polysemy Argument Judgement\n(vs. Isolated Conclusion)", y = "Middle Term", color = 'Experiment / Change') +
  xlim(0, 1.0) + 
  theme(axis.text.y = element_text(margin = margin(r = 5)))
if (RUN_LIVE) { pl.dumbbell }
output.name.pl <- paste(outputdir_plots, "Item-level-Boost.png", sep="")
ggsave(plot = pl.dumbbell, output.name.pl,
       width = 10, height = 10, units = "in")



##################
##### Item-level check
model.boost.M1 <- analyze_item_boost(df.M1.cross, c("Homonymy", "Polysemy"), "Homonymy", boost_file, "M1 (Crossed)")
model.boost.M2 <- analyze_item_boost(df.M2.bal, c("Homonymy", "Polysemy"), "Homonymy", boost_file, "M2 (Balanced)")
model.boost.M3 <- analyze_item_boost(df.M3.prime, c("Unrelated", "Related"), "Unrelated", boost_file, "M3 (Prime/Related)")

model.boost.M1.M2.together <- analyze_item_boost(df.orig %>% filter(ExpNum %in% c("M1-cross", "M2-bal")), c("Homonymy", "Polysemy"), "Homonymy", boost_file, "M1-M2-together")


## For S1 keeping Target words separate per load condition
df.S1.load.for_boost <- df.S1.load %>% 
  mutate(TargetWord = paste0(TargetWord, "_", loadtime))

model.boost.S1 <- analyze_item_boost(df.S1.load.for_boost, c("Homonymy", "Polysemy"), "Homonymy", boost_file, "S1 (Load)")


