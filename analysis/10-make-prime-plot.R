
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Main prime null effect plot (Exp 3)
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Main prime null effect plot (Exp 3)...")


## For handling project structure / relative paths ##
suppressMessages(require("rprojroot"))
sourceDir <- find_root(is_git_root)
source(file.path(sourceDir, "aux", "aux-functions.R"))
source(file.path(sourceDir, "aux", "aux-plot-aesthetics.R"))

args = commandArgs(trailingOnly=TRUE)
RUN_LIVE <- interactive()

if (RUN_LIVE) {
  basepath <- set_live_basepath()
  inputpath <- paste(basepath, "data/merged/combined-all-exp-trials.rds", sep="")
  outputdir <- paste(basepath, "output/plots/", sep="")
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



df.orig.raw <- readRDS(inputpath)
df.orig <- df.orig.raw %>% filter(excl.subj == 0) %>% filter(excl.trial == 0)
df.M1.cross <- df.orig %>% filter(ExpNum == "M1-cross")
df.M2.bal <- df.orig %>% filter(ExpNum == "M2-bal")
df.M3.prime <- df.orig %>% filter(ExpNum == "M3-prime")



tally_item_level_boost <- function(input.df) {
  by.item <- input.df %>%
    group_by(TargetWord, TrialType) %>%
    dplyr::summarise(mean = mean(Response),
                     P1 = mean(stim.premiseJudgment_P2),
                     P2 = mean(stim.premiseJudgment_P1),
                     SoloConc = mean(stim.ConcTruth),
                     Boost = mean - SoloConc,
                     .groups = "drop") %>%
    ungroup()
  return(by.item)
}


M1.item.all <- tally_item_level_boost(df.M1.cross)
polysemy_boost.M1 <- M1.item.all %>% group_by(TrialType) %>%
  summarise(mean_boost = mean(Boost)) %>%
  filter(TrialType == "Polysemy") %>%
  pull(mean_boost)

M2.item.all <- tally_item_level_boost(df.M2.bal)
polysemy_boost.M2 <- M2.item.all %>% group_by(TrialType) %>%
  summarise(mean_boost = mean(Boost)) %>%
  filter(TrialType == "Polysemy") %>%
  pull(mean_boost)


M3.item.all <- tally_item_level_boost(df.M3.prime)
M3.item.all$TrialType <- factor(M3.item.all$TrialType, levels = c("Valid Filler", "Related", "Unrelated", "Invalid Filler"))
M3.item.all.noFiller <- M3.item.all %>% filter(TrialType %in% c("Related", "Unrelated")) %>% droplevels()
M3.item.all.noFiller$TrialType <- factor(M3.item.all.noFiller$TrialType, levels = c("Related", "Unrelated"))



set.seed(123)

p <- ggplot(M3.item.all.noFiller, aes(x = TrialType, y = Boost, fill=TrialType)) +
  geom_point(aes(color = TrialType), position = position_jitter(width = 0.15), size = 2, alpha = 0.8) +
  geom_hline(yintercept = polysemy_boost.M1, linetype = "dashed", color = exp1_color, linewidth = 2) +
  geom_hline(yintercept = polysemy_boost.M2, linetype = "dashed", color = exp2_color, linewidth = 2) +
  annotate("text", y = polysemy_boost.M1+0.03, x = 1.5, label = paste0("Mean Polysemy Boost (Exp 1) = ", round(polysemy_boost.M1, 3)), color = exp1_color, size = 8) +
  annotate("text", y = polysemy_boost.M2+0.03, x = 1.5, label = paste0("Mean Polysemy Boost (Exp 2) = ", round(polysemy_boost.M2, 3)), color = exp2_color, size = 8) +
  stat_summary(aes(y = Boost, color = TrialType), fun = median, geom = "crossbar", width = 0.7, fatten = 2.8) + 
  geom_boxplot(width = 0.35, aes(color = TrialType), alpha = 0.3, outlier.shape = NA) +
  scale_fill_manual(values = c("Related" = grow_color,
                               "Unrelated" = drop_color)) + 
  scale_color_manual(values = c("Related" = grow_color,
                                "Unrelated" = drop_color))+ single_pane_theme() +
  ylab("Boost in Argument Judgements\n(vs. Conclusion in Isolation)") + coord_cartesian(ylim = c(-.4, 0.4)) +
  xlab("Semantic Association") +
  theme(axis.title.y = element_text(margin = margin(r = 15)))
if (RUN_LIVE) { print(p) }
ggsave(plot = p, paste0(outputdir, "M3-prime-no-boost.png"),
       width = 8, height = 8, units = "in")


if (RUN_LIVE) { M3.item.all.noFiller %>% group_by(TrialType) %>% summarise(boost = mean(Boost), n = n()) }


