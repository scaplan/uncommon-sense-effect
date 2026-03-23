
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Make forest plot with odds ratio for polysemy effect
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Make forest plot with odds ratio for polysemy effect...")

## For handling project structure / relative paths ##
suppressMessages(require("rprojroot"))
sourceDir <- find_root(is_git_root)
source(file.path(sourceDir, "aux", "aux-functions.R"))
source(file.path(sourceDir, "aux", "aux-plot-aesthetics.R"))

args = commandArgs(trailingOnly=TRUE)
RUN_LIVE <- interactive()

if (RUN_LIVE) {
  basepath <- set_live_basepath() # Path in full path to git repo if running individual files live...
  inputpath <- paste0(basepath, "output/stats/fitted-models.rds")
  outputdir <- paste0(basepath, "output/plots/")
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
models <- readRDS(inputpath)

M1.model <- models$M1.PH.randSlope.nointeraction
M2.model <- models$M2.PH.randInt.full
### Add in S1 load study models for SI odds ratio (forest) plot
S1.model.3d <- models$S1.PH.randSlope.ThreeDig.full
S1.model.5d <- models$S1.PH.randSlope.FiveDig.full
S1.model.750ms <- models$S1.PH.randSlope.750ms.full
S1.model.1500ms <- models$S1.PH.randSlope.1500ms.full

forest_data <- data.frame(
  Experiment = c("Exp 1", "Exp 2"),
  OR = c(
    exp(fixef(M1.model)["TrialTypePolysemy"]),
    exp(fixef(M2.model)["TrialTypePolysemy"])
  ),
  Lower = c(
    exp(confint(M1.model, method = "Wald")["TrialTypePolysemy", 1]),
    exp(confint(M2.model, method = "Wald")["TrialTypePolysemy", 1])
  ),
  Upper = c(
    exp(confint(M1.model, method = "Wald")["TrialTypePolysemy", 2]),
    exp(confint(M2.model, method = "Wald")["TrialTypePolysemy", 2])
  )
)

forest_data$Experiment <- factor(forest_data$Experiment, 
                                 levels = c("Exp 2", "Exp 1"))

forest.plot <- ggplot(forest_data, aes(x = OR, y = Experiment)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, color = Experiment), 
                 height = 0.15, linewidth = 2.5) +
  geom_point(aes(color = Experiment), size = 16, shape = 18) +
  scale_color_manual(values = c("Exp 1" = exp1_color, 
                                "Exp 2" = exp2_color)) +
  labs(
    x = "Odds Ratio (Polysemy vs. Homonymy)",
    y = NULL #,
    # title = "Uncommon Sense Effect Across Experiments"
  ) +
  scale_x_continuous(
    limits = c(0, 7.5),
    breaks = seq(0, 7, 1)
  ) +
  single_pane_theme()
if (RUN_LIVE) { forest.plot }
output.name.pl <- paste(outputdir, "Exp-1-2-Forest-Plot.png", sep="")
ggsave(plot = forest.plot, output.name.pl,
       width = 8, height = 8, units = "in")



forest_data_S1 <- data.frame(
  Experiment = c("Three-digit", "Five-digit", "1500ms time", "750ms time"),
  OR = c(
    exp(fixef(S1.model.3d)["TrialTypePolysemy"]),
    exp(fixef(S1.model.5d)["TrialTypePolysemy"]),
    exp(fixef(S1.model.1500ms)["TrialTypePolysemy"]),
    exp(fixef(S1.model.750ms)["TrialTypePolysemy"])
  ),
  Lower = c(
    exp(confint(S1.model.3d, method = "Wald")["TrialTypePolysemy", 1]),
    exp(confint(S1.model.5d, method = "Wald")["TrialTypePolysemy", 1]),
    exp(confint(S1.model.1500ms, method = "Wald")["TrialTypePolysemy", 1]),
    exp(confint(S1.model.750ms, method = "Wald")["TrialTypePolysemy", 1])
  ),
  Upper = c(
    exp(confint(S1.model.3d, method = "Wald")["TrialTypePolysemy", 2]),
    exp(confint(S1.model.5d, method = "Wald")["TrialTypePolysemy", 2]),
    exp(confint(S1.model.1500ms, method = "Wald")["TrialTypePolysemy", 2]),
    exp(confint(S1.model.750ms, method = "Wald")["TrialTypePolysemy", 2])
  )
)

forest_data_S1$Experiment <- factor(forest_data_S1$Experiment, 
                                 levels = c("Three-digit", "Five-digit", "1500ms time", "750ms time"))



# forest.plot.S1 <- ggplot(forest_data_S1, aes(x = OR, y = Experiment)) +
#   geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 2) +
#   geom_errorbarh(aes(xmin = Lower, xmax = Upper, color = Experiment), 
#                  height = 0.15, linewidth = 2.5) +
#   geom_point(aes(color = Experiment), size = 16, shape = 18) +
#   scale_y_discrete(limits = rev(levels(forest_data_S1$Experiment))) +
#   scale_color_manual(values = c(
#     "Three-digit" = "#01665E",
#     "Five-digit"  = "#80CDC1",
#     "1500ms time" = "#BF812D",
#     "750ms time"  = "#DFC27D"
#   )) + 
#   labs(
#     x = "Odds Ratio (Polysemy vs. Homonymy)",
#     y = NULL #,
#     # title = "Uncommon Sense Effect Across Experiments"
#   ) +
#   scale_x_continuous(
#     limits = c(0, 6.0),
#     breaks = seq(0, 6, 1)
#   ) +
#   single_pane_theme()
# if (RUN_LIVE) { forest.plot.S1 }
# output.name.pl <- paste(outputdir, "S1-load-separate-Forest-Plot.png", sep="")
# ggsave(plot = forest.plot.S1, output.name.pl,
#        width = 12, height = 6, units = "in")
# 


