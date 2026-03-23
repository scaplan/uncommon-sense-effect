
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Main plots (raw P and H)
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Main plots (raw P and H)...")

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
df.S1.load <- df.orig %>% filter(ExpNum == "S1-load-M2")


#### M1


M1.all.binom.prop <- summary_binomial_data_group(df.M1.cross, Response, TrialType, method = "wilson")
M1.PH.binom.prop <- M1.all.binom.prop %>% filter(TrialType %in% c("Polysemy", "Homonymy"))
M1.PH.binom.prop$TrialType <- factor(M1.PH.binom.prop$TrialType, levels = c("Polysemy", "Homonymy"))


M2.all.binom.prop <- summary_binomial_data_group(df.M2.bal, Response, TrialType, method = "wilson")
M2.PH.binom.prop <- M2.all.binom.prop %>% filter(TrialType %in% c("Polysemy", "Homonymy"))
M2.PH.binom.prop$TrialType <- factor(M2.PH.binom.prop$TrialType, levels = c("Polysemy", "Homonymy"))




M1.PH.gap <- M1.PH.binom.prop %>% summarise(diff = prop[TrialType == "Polysemy"] - prop[TrialType == "Homonymy"]) %>%  pull(diff)
M1.PH.annotation <- bquote(.(round(M1.PH.gap, 3))^"***")
M1.P.level <- M1.PH.binom.prop %>% summarise(plevel = ci_high[TrialType == "Polysemy"]) %>%  pull(plevel)

M2.PH.gap <- M2.PH.binom.prop %>% summarise(diff = prop[TrialType == "Polysemy"] - prop[TrialType == "Homonymy"]) %>%  pull(diff)
M2.PH.annotation <- bquote(.(round(M2.PH.gap, 3))^"***")
M2.P.level <- M2.PH.binom.prop %>% summarise(plevel = ci_high[TrialType == "Polysemy"]) %>%  pull(plevel)



pl.response.trialbinom.M1 <-  ggplot(M1.PH.binom.prop, aes(TrialType, prop, fill=TrialType, color=TrialType)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.4, linewidth = 5) +
  geom_point(color = "black", size = 5) +
  coord_cartesian(ylim = c(0.2, 0.8)) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
  # geom_signif(
  #   comparisons = list(c("Polysemy", "Homonymy")),
  #   annotations = deparse(M1.PH.annotation), 
  #   textsize = 10, y_position = M1.P.level + 0.02,
  #   parse = TRUE, color = "black", size = 1
  # ) + 
  labs(y="Prop. Valid Argument Judgements", x = NULL) +
  single_pane_theme() + two_set_fill() + two_set_color() +
  theme(axis.title.y = element_text(margin = margin(r = 15)))
if (RUN_LIVE) { pl.response.trialbinom.M1 }
ggsave(plot = pl.response.trialbinom.M1, paste0(outputdir, "M1-binom-result.png"),
       width = 8, height = 8, units = "in")




pl.response.trialbinom.M2 <-  ggplot(M2.PH.binom.prop, aes(TrialType, prop, fill=TrialType, color=TrialType)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.4, linewidth = 5) +
  geom_point(color = "black", size = 5) +
  coord_cartesian(ylim = c(0.2, 0.8)) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
  # geom_signif(
  #   comparisons = list(c("Polysemy", "Homonymy")),
  #   annotations = deparse(M2.PH.annotation), 
  #   textsize = 10, y_position = M2.P.level + 0.02,
  #   parse = TRUE, color = "black", size = 1
  # ) + 
  labs(y="Prop. Valid Argument Judgements", x = NULL) +
  single_pane_theme() + two_set_fill() + two_set_color() +
  theme(axis.title.y = element_text(margin = margin(r = 15)))
if (RUN_LIVE) { pl.response.trialbinom.M2 }
ggsave(plot = pl.response.trialbinom.M2, paste0(outputdir, "M2-binom-result.png"),
       width = 8, height = 8, units = "in")




df.S1.3dig <- df.S1.load %>% filter(loadtime == "ThreeDig")
df.S1.5dig <- df.S1.load %>% filter(loadtime == "FiveDig")
df.S1.750ms <- df.S1.load %>% filter(loadtime == "750ms")
df.S1.1500ms <- df.S1.load %>% filter(loadtime == "1500ms")


S1.all.binom.prop <- summary_binomial_data_group(df.S1.load, Response, TrialType, method = "wilson")
S1.PH.binom.prop <- S1.all.binom.prop %>% filter(TrialType %in% c("Polysemy", "Homonymy"))
S1.PH.binom.prop$TrialType <- factor(S1.PH.binom.prop$TrialType, levels = c("Polysemy", "Homonymy"))

S1.3dig.all.binom.prop <- summary_binomial_data_group(df.S1.3dig, Response, TrialType, method = "wilson")
S1.3dig.PH.binom.prop <- S1.3dig.all.binom.prop %>% filter(TrialType %in% c("Polysemy", "Homonymy"))
S1.3dig.PH.binom.prop$TrialType <- factor(S1.3dig.PH.binom.prop$TrialType, levels = c("Polysemy", "Homonymy"))

S1.5dig.all.binom.prop <- summary_binomial_data_group(df.S1.5dig, Response, TrialType, method = "wilson")
S1.5dig.PH.binom.prop <- S1.5dig.all.binom.prop %>% filter(TrialType %in% c("Polysemy", "Homonymy"))
S1.5dig.PH.binom.prop$TrialType <- factor(S1.5dig.PH.binom.prop$TrialType, levels = c("Polysemy", "Homonymy"))

S1.750ms.all.binom.prop <- summary_binomial_data_group(df.S1.750ms, Response, TrialType, method = "wilson")
S1.750ms.PH.binom.prop <- S1.750ms.all.binom.prop %>% filter(TrialType %in% c("Polysemy", "Homonymy"))
S1.750ms.PH.binom.prop$TrialType <- factor(S1.750ms.PH.binom.prop$TrialType, levels = c("Polysemy", "Homonymy"))

S1.1500ms.all.binom.prop <- summary_binomial_data_group(df.S1.1500ms, Response, TrialType, method = "wilson")
S1.1500ms.PH.binom.prop <- S1.1500ms.all.binom.prop %>% filter(TrialType %in% c("Polysemy", "Homonymy"))
S1.1500ms.PH.binom.prop$TrialType <- factor(S1.1500ms.PH.binom.prop$TrialType, levels = c("Polysemy", "Homonymy"))

pl.response.trialbinom.S1 <-  ggplot(S1.PH.binom.prop, aes(TrialType, prop, fill=TrialType, color=TrialType)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.4, linewidth = 5) +
  geom_point(color = "black", size = 5) +
  coord_cartesian(ylim = c(0.2, 0.8)) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
  labs(y="Prop. Valid Argument Judgements", x = NULL) +
  single_pane_theme() + two_set_fill() + two_set_color() +
  theme(axis.title.y = element_text(margin = margin(r = 15)))
if (RUN_LIVE) { pl.response.trialbinom.S1 }
ggsave(plot = pl.response.trialbinom.S1, paste0(outputdir, "S1-binom-result.png"),
       width = 8, height = 8, units = "in")

pl.response.trialbinom.S1.3dig <-  ggplot(S1.3dig.PH.binom.prop, aes(TrialType, prop, fill=TrialType, color=TrialType)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.4, linewidth = 5) +
  geom_point(color = "black", size = 5) +
  coord_cartesian(ylim = c(0.3, 0.7)) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
  labs(y="Prop. Valid Argument Judgements", x = NULL) +
  single_pane_theme() + two_set_fill() + two_set_color() +
  annotate("label", x = 1.5, y = 0.68, label = "Three-digit",
           fill = NA,
           color = "black",
           label.size = 1,
           size=11 ) + 
  theme(axis.title.y = element_text(margin = margin(r = 15)),
        axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 2)))
if (RUN_LIVE) { pl.response.trialbinom.S1.3dig }
ggsave(plot = pl.response.trialbinom.S1.3dig, paste0(outputdir, "S1-3dig-binom-result.png"),
       width = 4, height = 8, units = "in")

pl.response.trialbinom.S1.5dig <-  ggplot(S1.5dig.PH.binom.prop, aes(TrialType, prop, fill=TrialType, color=TrialType)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.4, linewidth = 5) +
  geom_point(color = "black", size = 5) +
  coord_cartesian(ylim = c(0.3, 0.7)) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
  labs(y = NULL, x = NULL) +
  single_pane_theme() + two_set_fill() + two_set_color() + 
  annotate("label", x = 1.5, y = 0.68, label = "Five-digit",
           fill = NA,
           color = "black",
           label.size = 1,
           size=11 ) + 
  theme(axis.title.y = element_text(margin = margin(r = 15)),
        axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 2)))
if (RUN_LIVE) { pl.response.trialbinom.S1.5dig }
ggsave(plot = pl.response.trialbinom.S1.5dig, paste0(outputdir, "S1-5dig-binom-result.png"),
       width = 4, height = 8, units = "in")

pl.response.trialbinom.S1.750ms <-  ggplot(S1.750ms.PH.binom.prop, aes(TrialType, prop, fill=TrialType, color=TrialType)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.4, linewidth = 5) +
  geom_point(color = "black", size = 5) +
  coord_cartesian(ylim = c(0.3, 0.7)) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
  labs(y = NULL, x = NULL) +
  single_pane_theme() + two_set_fill() + two_set_color() +
  annotate("label", x = 1.5, y = 0.68, label = "750ms time",
           fill = NA,
           color = "black",
           label.size = 1,
           size=11 ) + 
  theme(axis.title.y = element_text(margin = margin(r = 15)),
        axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 2)))
if (RUN_LIVE) { pl.response.trialbinom.S1.750ms }
ggsave(plot = pl.response.trialbinom.S1.750ms, paste0(outputdir, "S1-750ms-binom-result.png"),
       width = 4, height = 8, units = "in")

pl.response.trialbinom.S1.1500ms <-  ggplot(S1.1500ms.PH.binom.prop, aes(TrialType, prop, fill=TrialType, color=TrialType)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.4, linewidth = 5) +
  geom_point(color = "black", size = 5) +
  coord_cartesian(ylim = c(0.3, 0.7)) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
  labs(y = NULL, x = NULL) +
  single_pane_theme() + two_set_fill() + two_set_color() +
  annotate("label", x = 1.5, y = 0.68, label = "1500ms time",
           fill = NA,
           color = "black",
           label.size = 1,
           size=11 ) + 
  theme(axis.title.y = element_text(margin = margin(r = 15)),
        axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 2)))
if (RUN_LIVE) { pl.response.trialbinom.S1.1500ms }
ggsave(plot = pl.response.trialbinom.S1.1500ms, paste0(outputdir, "S1-1500ms-binom-result.png"),
       width = 4, height = 8, units = "in")


combined_plot <- plot_grid(pl.response.trialbinom.S1.3dig, pl.response.trialbinom.S1.5dig, pl.response.trialbinom.S1.1500ms, pl.response.trialbinom.S1.750ms, nrow = 1)
if (RUN_LIVE) { combined_plot }
ggsave(plot = combined_plot,
       filename = paste0(outputdir, "S1-binom-by-condition.png"),
       width = 16, height = 8, units = "in") 







# # 
# # ## Make simple M1 and M2 plots for the paper
# # 
# M1.by.subject.byTypeSummary <- summary_group_by_sc(df.M1.cross, "Response", "TrialType", "SubjectID")
# M1.by.subject.byTypeSummary$TrialType <- factor(M1.by.subject.byTypeSummary$TrialType, levels=c('Valid Filler', 'Polysemy', 'Homonymy', 'Invalid Filler'))
# M2.by.subject.byTypeSummary <- summary_group_by_sc(df.M2.bal, "Response", "TrialType", "SubjectID")
# M2.by.subject.byTypeSummary$TrialType <- factor(M2.by.subject.byTypeSummary$TrialType, levels=c('Valid Filler', 'Polysemy', 'Homonymy', 'Invalid Filler'))
# M3.by.subject.byTypeSummary <- summary_group_by_sc(df.M3.prime, "Response", "TrialType", "SubjectID")
# M3.by.subject.byTypeSummary$TrialType <- factor(M3.by.subject.byTypeSummary$TrialType, levels=c('Valid Filler', 'Related', 'Unrelated', 'Invalid Filler'))
# 
# 
# 
# 
# 
# #### M2 Conclusion and Premise plausibility distro
# 
# M1.byArg.normed <- df.M1.cross %>% group_by(FullArgument, TrialType) %>% dplyr::summarise(concTruth = mean(stim.ConcTruth),
#                                                                                              P1 = mean(stim.premiseJudgment_P1),
#                                                                                              P2 = mean(stim.premiseJudgment_P2),
#                                                                                           premiseTruth = mean((stim.premiseJudgment_P1 + stim.premiseJudgment_P2)/2))
# M2.byArg.normed <- df.M2.bal %>% group_by(FullArgument, TrialType) %>% dplyr::summarise(concTruth = mean(stim.ConcTruth),
#                                                                                           P1 = mean(stim.premiseJudgment_P1),
#                                                                                           P2 = mean(stim.premiseJudgment_P2),
#                                                                                           premiseTruth = mean((stim.premiseJudgment_P1 + stim.premiseJudgment_P2)/2))
# M3.byArg.normed <- df.M3.prime %>% group_by(FullArgument, TrialType) %>% dplyr::summarise(concTruth = mean(stim.ConcTruth),
#                                                                                           P1 = mean(stim.premiseJudgment_P1),
#                                                                                           P2 = mean(stim.premiseJudgment_P2),
#                                                                                           premiseTruth = mean((stim.premiseJudgment_P1 + stim.premiseJudgment_P2)/2))
# 
# M1.byType.concTruth <- summary_group_by_sc(M1.byArg.normed, "concTruth", "TrialType")
# M1.byType.premTruth <- summary_group_by_sc(M1.byArg.normed, "premiseTruth", "TrialType")
# M2.byType.concTruth <- summary_group_by_sc(M2.byArg.normed, "concTruth", "TrialType")
# M2.byType.premTruth <- summary_group_by_sc(M2.byArg.normed, "premiseTruth", "TrialType")
# M3.byType.concTruth <- summary_group_by_sc(M3.byArg.normed, "concTruth", "TrialType")
# M3.byType.premTruth <- summary_group_by_sc(M3.byArg.normed, "premiseTruth", "TrialType")
# 
# 
# 
# M1.byArg.normed$TrialType <- factor(M1.byArg.normed$TrialType, levels=c('Valid Filler', 'Polysemy', 'Homonymy', 'Invalid Filler'))
# M2.byArg.normed$TrialType <- factor(M2.byArg.normed$TrialType, levels=c('Valid Filler', 'Polysemy', 'Homonymy', 'Invalid Filler'))
# M3.byArg.normed$TrialType <- factor(M3.byArg.normed$TrialType, levels=c('Valid Filler', 'Polysemy', 'Homonymy', 'Invalid Filler'))
# 
# 
# pl.conc.by.arg.M1 <- ggplot(M1.byArg.normed, aes(y = TrialType, x = concTruth, fill = TrialType, color = TrialType)) +
#   geom_point(
#     position = position_jitter(height = 0.05, width = 0.05, seed = 42),
#     alpha = 0.5, size = 3.5, shape = 16) +
#   stat_summary(
#     fun.data = mean_cl_boot, geom = "errorbar",
#     width = 0.4, linewidth = 5,  # thickness to match main figure
#     position = position_nudge(y = -0.25)) +
#   stat_summary(
#     fun = mean, geom = "point", color = "black", size = 5,
#     position = position_nudge(y = -0.25)) +
#   coord_cartesian(xlim = c(0, 1.0)) + scale_y_discrete(limits = rev) + 
#   labs(x = "Mean Conclusion Plausibility per Argument", y = NULL) +
#   single_pane_theme() + four_set_fill() + four_set_color() 
# if (RUN_LIVE) { pl.conc.by.arg.M1 }
# ggsave(plot = pl.conc.by.arg.M1, paste0(outputdir, "M1-Conc-Norming.png"),
#        width = 16, height = 8, units = "in")
# 
# pl.conc.by.arg.M1 <- ggplot(M1.byArg.normed, aes(y = TrialType, x = premiseTruth, fill = TrialType, color = TrialType)) +
#   geom_point(
#     position = position_jitter(height = 0.05, width = 0.05, seed = 42),
#     alpha = 0.5, size = 3.5, shape = 16) +
#   stat_summary(
#     fun.data = mean_cl_boot, geom = "errorbar",
#     width = 0.4, linewidth = 5,  # thickness to match main figure
#     position = position_nudge(y = -0.25)) +
#   stat_summary(
#     fun = mean, geom = "point", color = "black", size = 5,
#     position = position_nudge(y = -0.25)) +
#   coord_cartesian(xlim = c(0, 1.0)) + scale_y_discrete(limits = rev) + 
#   labs(x = "Mean Conclusion Plausibility per Argument", y = NULL) +
#   single_pane_theme() + four_set_fill() + four_set_color() 
# if (RUN_LIVE) { pl.conc.by.arg.M1 }
# ggsave(plot = pl.conc.by.arg.M1, paste0(outputdir, "M1-Prem-Norming.png"),
#        width = 16, height = 8, units = "in")
# 
# 
# 
# pl.conc.by.arg.M2 <- ggplot(M2.byArg.normed, aes(y = TrialType, x = concTruth, fill = TrialType, color = TrialType)) +
#   geom_point(
#     position = position_jitter(height = 0.05, width = 0.05, seed = 42),
#     alpha = 0.5, size = 3.5, shape = 16) +
#   stat_summary(
#     fun.data = mean_cl_boot, geom = "errorbar",
#     width = 0.4, linewidth = 5,  # thickness to match main figure
#     position = position_nudge(y = -0.25)) +
#   stat_summary(
#     fun = mean, geom = "point", color = "black", size = 5,
#     position = position_nudge(y = -0.25)) +
#   coord_cartesian(xlim = c(0, 1.0)) + scale_y_discrete(limits = rev) + 
#   labs(x = "Mean Conclusion Plausibility per Argument", y = NULL) +
#   single_pane_theme() + four_set_fill() + four_set_color() 
# if (RUN_LIVE) { pl.conc.by.arg.M2 }
# ggsave(plot = pl.conc.by.arg.M2, paste0(outputdir, "M2-Conc-Norming.png"),
#        width = 16, height = 8, units = "in")
# 
# pl.prem.by.arg.M2 <- ggplot(M2.byArg.normed, aes(y = TrialType, x = premiseTruth, fill = TrialType, color = TrialType)) +
#   geom_point(
#     position = position_jitter(height = 0.05, width = 0.05, seed = 42),
#     alpha = 0.5, size = 3.5, shape = 16) +
#   stat_summary(
#     fun.data = mean_cl_boot, geom = "errorbar",
#     width = 0.4, linewidth = 5,  # thickness to match main figure
#     position = position_nudge(y = -0.25)) +
#   stat_summary(
#     fun = mean, geom = "point", color = "black", size = 5,
#     position = position_nudge(y = -0.25)) +
#   coord_cartesian(xlim = c(0, 1.0)) + scale_y_discrete(limits = rev) + 
#   labs(x = "Mean Conclusion Plausibility per Argument", y = NULL) +
#   single_pane_theme() + four_set_fill() + four_set_color() 
# if (RUN_LIVE) { pl.prem.by.arg.M2 }
# ggsave(plot = pl.prem.by.arg.M2, paste0(outputdir, "M2-Prem-Norming.png"),
#        width = 16, height = 8, units = "in")
# 
# 
# 
# 
# 
# 
# pl.response.by.subject.M1 <- ggplot(M1.by.subject.byTypeSummary, aes(y = TrialType, x = mean_prop, fill = TrialType, color = TrialType)) +
#   geom_point(
#     position = position_jitter(height = 0.05, width = 0.05, seed = 42),
#     alpha = 0.5, size = 3.5, shape = 16) +
#   stat_summary(
#     fun.data = mean_cl_boot, geom = "errorbar",
#     width = 0.4, linewidth = 5,  # thickness to match main figure
#     position = position_nudge(y = -0.25)) +
#   stat_summary(
#     fun = mean, geom = "point", color = "black", size = 5,
#     position = position_nudge(y = -0.25)) +
#   coord_cartesian(xlim = c(0, 1.0)) + scale_y_discrete(limits = rev) + 
#   labs(x = "Mean Argument Judgement per Participant", y = NULL) +
#   single_pane_theme() + four_set_fill() + four_set_color() 
# if (RUN_LIVE) { pl.response.by.subject.M1 }
# ggsave(plot = pl.response.by.subject.M1, paste0(outputdir, "M1-raw-subject-result.png"),
#        width = 16, height = 8, units = "in")
# 
# pl.response.by.subject.M2 <- ggplot(M2.by.subject.byTypeSummary, aes(y = TrialType, x = mean_prop, fill = TrialType, color = TrialType)) +
#   geom_point(
#     position = position_jitter(height = 0.05, width = 0.05, seed = 42),
#     alpha = 0.5, size = 3.5, shape = 16) +
#   stat_summary(
#     fun.data = mean_cl_boot, geom = "errorbar",
#     width = 0.4, linewidth = 5,  # thickness to match main figure
#     position = position_nudge(y = -0.25)) +
#   stat_summary(
#     fun = mean, geom = "point", color = "black", size = 5,
#     position = position_nudge(y = -0.25)) +
#   coord_cartesian(xlim = c(0, 1.0)) + scale_y_discrete(limits = rev) + 
#   labs(x = "Mean Argument Judgement per Participant", y = NULL) +
#   single_pane_theme() + four_set_fill() + four_set_color() 
# if (RUN_LIVE) { pl.response.by.subject.M2 }
# ggsave(plot = pl.response.by.subject.M2, paste0(outputdir, "M2-raw-subject-result.png"),
#        width = 16, height = 8, units = "in")
# 
# pl.response.by.subject.M3 <- ggplot(M3.by.subject.byTypeSummary, aes(y = TrialType, x = mean_prop, fill = TrialType, color = TrialType)) +
#   geom_point(
#     position = position_jitter(height = 0.05, width = 0.05, seed = 42),
#     alpha = 0.5, size = 3.5, shape = 16) +
#   stat_summary(
#     fun.data = mean_cl_boot, geom = "errorbar",
#     width = 0.4, linewidth = 5,  # thickness to match main figure
#     position = position_nudge(y = -0.25)) +
#   stat_summary(
#     fun = mean, geom = "point", color = "black", size = 5,
#     position = position_nudge(y = -0.25)) +
#   coord_cartesian(xlim = c(0, 1.0)) + scale_y_discrete(limits = rev) + 
#   labs(x = "Mean Argument Judgement per Participant", y = NULL) +
#   single_pane_theme() + # four_set_fill() + four_set_color() 
#   scale_fill_manual(values = c("Valid Filler" = valid_filler_color,
#                                "Related" = grow_color,
#                                "Unrelated" = drop_color,
#                                "Invalid Filler" = invalid_filler_color)) + 
#   scale_color_manual(values = c("Valid Filler" = valid_filler_color,
#                                 "Related" = grow_color,
#                                 "Unrelated" = drop_color,
#                                 "Invalid Filler" = invalid_filler_color))
# if (RUN_LIVE) { pl.response.by.subject.M3 }
# ggsave(plot = pl.response.by.subject.M3, paste0(outputdir, "M3-raw-subject-result.png"),
#        width = 16, height = 8, units = "in")

