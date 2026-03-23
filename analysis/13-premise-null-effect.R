
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  No effect of premise plausibility
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Null effect of trial number...")


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


df.orig.raw <- readRDS(inputpath)
df.orig <- df.orig.raw %>% filter(excl.subj == 0) %>% filter(excl.trial == 0)
df.M1.M2 <- df.orig %>% filter(ExpNum %in% c("M1-cross","M2-bal"))


by.arg <- df.M1.M2 %>% group_by(global.argid, ExpNum, TrialType, MiddleTerm1, FullArgument) %>% 
  dplyr::summarise(P1.strength = mean(stim.premiseJudgment_P1),
                   P2.strength = mean(stim.premiseJudgment_P2),
                   P.both.strength = mean((stim.premiseJudgment_P1 + stim.premiseJudgment_P2) / 2),
                   C.strength = mean(stim.ConcTruth),
                   arg.strength = mean(Response))


by.arg.PH <- by.arg %>% filter(TrialType %in% c("Polysemy", "Homonymy"))
by.arg.PH$ExpNum <- factor(by.arg.PH$ExpNum,
                           levels = c("M1-cross", "M2-bal"),
                           labels = c("Exp 1", "Exp 2"))


by.arg.PH %>% group_by(ExpNum, TrialType) %>% 
  dplyr::summarise(P1 = mean(P1.strength),
                   P1.sd = sd(P1.strength),
                   P2 = mean(P2.strength),
                   P2.sd = sd(P2.strength),
                   Pboth = mean(P.both.strength),
                   Pboth.sd = sd(P.both.strength),
                   Arg = mean(arg.strength),
                   n = n())

t.test(P1.strength ~ TrialType,
       data = by.arg.PH,
       subset = ExpNum == "Exp 1")

t.test(P1.strength ~ TrialType,
       data = by.arg.PH,
       subset = ExpNum == "Exp 2")

t.test(P2.strength ~ TrialType,
       data = by.arg.PH,
       subset = ExpNum == "Exp 1")

t.test(P2.strength ~ TrialType,
       data = by.arg.PH,
       subset = ExpNum == "Exp 2")

t.test(P.both.strength ~ TrialType,
       data = by.arg.PH,
       subset = ExpNum == "Exp 1")

t.test(P.both.strength ~ TrialType,
       data = by.arg.PH,
       subset = ExpNum == "Exp 2")




cor.test(by.arg.PH$P1.strength, by.arg.PH$arg.strength)
cor.test(by.arg.PH$P2.strength, by.arg.PH$arg.strength)
cor.test(by.arg.PH$P.both.strength, by.arg.PH$arg.strength)



pl.P1 <- ggplot(by.arg.PH, aes(x = P1.strength, y = arg.strength, color = ExpNum, shape = ExpNum)) +
  geom_point(size = 4) +
  geom_smooth(aes(color = NULL, shape = NULL), method = "lm", se = TRUE,
              color = "red", linewidth = 2) +
  labs(
    x = "P1 Plausibility",
    y = "Argument Response",
  ) +
  scale_color_manual(values = c("Exp 1" = exp1_color,
                                "Exp 2" = exp2_color),
                     name = "Experiment"
  ) + 
  scale_shape_manual(values = c("Exp 1" = 16,
                                "Exp 2" = 17),
                     name = "Experiment"
  ) +
  single_pane_theme_withLegend(c(0.72,0.90)) +
  theme(axis.title.y = element_text(margin = margin(r = 15)),
        legend.box.background = element_rect(color = "black", linewidth = 0.5))

pl.P2 <- ggplot(by.arg.PH, aes(x = P2.strength, y = arg.strength, color = ExpNum, shape = ExpNum)) +
  geom_point(size = 4) +
  geom_smooth(aes(color = NULL, shape = NULL), method = "lm", se = TRUE,
              color = "red", linewidth = 2) +
  labs(
    x = "P2 Plausibility",
    y = "Argument Response",
  ) +
  scale_color_manual(values = c("Exp 1" = exp1_color,
                                "Exp 2" = exp2_color),
                     name = "Experiment"
  ) + 
  scale_shape_manual(values = c("Exp 1" = 16,
                                "Exp 2" = 17),
                     name = "Experiment"
  ) +
  single_pane_theme() +
  theme(axis.title.y = element_text(margin = margin(r = 15)))

pl.P.both <- ggplot(by.arg.PH, aes(x = P.both.strength, y = arg.strength, color = ExpNum, shape = ExpNum)) +
  geom_point(size = 4) +
  geom_smooth(aes(color = NULL, shape = NULL), method = "lm", se = TRUE,
              color = "red", linewidth = 2) +
  labs(
    x = "Average Premise Plausibility",
    y = "Argument Response",
  ) +
  scale_color_manual(values = c("Exp 1" = exp1_color,
                                "Exp 2" = exp2_color),
                     name = "Experiment"
  ) + 
  scale_shape_manual(values = c("Exp 1" = 16,
                                "Exp 2" = 17),
                     name = "Experiment"
  ) +
  single_pane_theme() +
  theme(axis.title.y = element_text(margin = margin(r = 15)))



axisTextSizeBig <- 36

p.P1.grid <- ggdraw(pl.P1) + draw_label("(A)", x = 0.18, y = 0.95, hjust = 0, vjust = 1, size = axisTextSizeBig)
p.P2.grid <- ggdraw(pl.P2) + draw_label("(B)", x = 0.18, y = 0.95, hjust = 0, vjust = 1, size = axisTextSizeBig)
p.P.both.grid <- ggdraw(pl.P.both) + draw_label("(C)", x = 0.18, y = 0.95, hjust = 0, vjust = 1, size = axisTextSizeBig)

combined_plot <- plot_grid(p.P1.grid, p.P2.grid, p.P.both.grid, nrow = 1)
if (RUN_LIVE) { combined_plot }
ggsave(plot = combined_plot,
       filename="Premise-Null-Effect.png",
       width = 24, height = 8, units = "in") 

