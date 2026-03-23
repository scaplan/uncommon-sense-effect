
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Subject-level analysis (everyone displays the USE)
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Subject-level analysis (everyone displays the USE)...")


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
if (RUN_LIVE) { nrow(df.orig) }
rm(df.orig.raw)

if (RUN_LIVE) {
  # nrow(df.orig)
  table(df.orig$ExpNum)
}

df.M1.cross <- df.orig %>% filter(ExpNum == "M1-cross")
df.M2.bal <- df.orig %>% filter(ExpNum == "M2-bal")
df.M3.prime <- df.orig %>% filter(ExpNum == "M3-prime")


if (RUN_LIVE) {
  df.orig %>% 
    group_by(ExpNum) %>% 
    summarise(n_subjects = n_distinct(SubjectID))
}



jitter_boost <- function(x, amount = 0.03,
                         min = 0, max = 1,
                         seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  x_jit <- x + rnorm(length(x), mean = 0, sd = amount)
  
  return(pmin(max, pmax(min, x_jit)))
}

# Make P vs. H triangle plot
df.OneTwoBoth <- df.orig %>% filter(ExpNum %in% c("M1-cross", "M2-bal")) %>%
  mutate(GlobalSubjNum = paste(as.character(ExpNum), as.character(SubjectID), sep = "-"))

by.subj <- df.OneTwoBoth %>% group_by(GlobalSubjNum, TrialType, ExpNum) %>%
  summarise(mean = mean(Response)) %>%
  pivot_wider(names_from = TrialType, values_from = mean, names_prefix = "total_") %>%
  mutate(P.boost = total_Polysemy - total_Homonymy)

set.seed(123)

by.subj$total_Polysemy_jitter <- jitter_boost(by.subj$total_Polysemy)
by.subj$total_Homonymy_jitter <- jitter_boost(by.subj$total_Homonymy)



# With significance test
if (RUN_LIVE) { cor.test(by.subj$total_Polysemy, by.subj$total_Homonymy) }

by.subj.M1 <- by.subj %>% filter(ExpNum == "M1-cross")
by.subj.M2 <- by.subj %>% filter(ExpNum == "M2-bal")

if (RUN_LIVE) { cor.test(by.subj.M1$total_Polysemy, by.subj.M1$total_Homonymy) }
if (RUN_LIVE) { cor.test(by.subj.M2$total_Polysemy, by.subj.M2$total_Homonymy) }



boost_region  <- function(x) x
p.boost.plot.scatter <- ggplot(by.subj, aes(x = total_Homonymy_jitter, y = total_Polysemy_jitter, color = ExpNum)) +
  geom_ribbon(stat = 'function', fun = boost_region,
              mapping = aes(ymin = after_stat(y), ymax = Inf),
              fill = 'lightgreen', alpha = 0.2,
              inherit.aes = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 2) +  # Line x = y
  geom_point(size = 4) +
  # single_pane_theme_withLegend(c(0.50,0.1)) +
  single_pane_theme_withLegend(c(0.18, 0.92)) +
  theme(legend.background = element_rect(color = "black", linewidth = 0.5, fill = scales::alpha("lightgreen", 0.2)),
        legend.box.background = element_rect(color = "black", linewidth = 0.5),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  xlim(0,1) + ylim(0,1) +
  scale_color_manual(
    values = c("M1-cross" = exp1_color, "M2-bal" = exp2_color),
    labels = c(
      "M1-cross" = "Exp 1",
      "M2-bal" = "Exp 2"
    ),
    name = "Experiment"
  ) +
  labs(y="Mean Polysemy Argument Judgment", x = "\n Mean Homonymy Argument Judgment") +
  theme(axis.text.y = element_text(margin = margin(r = 5)))
if (RUN_LIVE) { p.boost.plot.scatter }
output.name.pl <- paste(outputdir, "Subject-level-Boost.png", sep="")
ggsave(plot = p.boost.plot.scatter, output.name.pl,
       width = 10, height = 10, units = "in")



###################################################################################
#### 2. Confirm unimodality of subject responses (no secret non-polysemy havers) ##
# 
# # For what proportion of subjects were the P scores higher than H scores?
# Adjusting for conclusion strength (and raw)

group_level_conc_adjustment <- function(df, type1, type2) {
  df %>% 
    filter(TrialType %in% c(type1, type2)) %>% 
    group_by(TrialType) %>%
    summarise(concStrength = mean(stim.ConcTruth)) %>%
    pivot_wider(names_from = TrialType, values_from = concStrength) %>%
    {.[[type1]] - .[[type2]]}
}

calc_subject_level_USE <- function(df, type1, type2, conc_adj, 
                               prefix = "total_", 
                               diff_name = "boost") {
  df %>% 
    group_by(SubjectID, TrialType) %>%
    summarise(mean = mean(Response), .groups = "drop") %>%
    pivot_wider(names_from = TrialType, 
                values_from = mean, 
                names_prefix = prefix) %>%
    mutate(
      "{diff_name}.raw" := .data[[paste0(prefix, type1)]] - .data[[paste0(prefix, type2)]],
      "{diff_name}.adjusted" := .data[[paste0(prefix, type1)]] - .data[[paste0(prefix, type2)]] - conc_adj
    ) %>%
    arrange(desc(.data[[paste0(diff_name, ".raw")]]))
}


M1.conc.adjustment <- group_level_conc_adjustment(df.M1.cross, "Polysemy", "Homonymy")
M2.conc.adjustment <- group_level_conc_adjustment(df.M2.bal, "Polysemy", "Homonymy")
M3.conc.adjustment <- group_level_conc_adjustment(df.M3.prime, "Related", "Unrelated")

M1.by.subj <- calc_subject_level_USE(df.M1.cross, "Polysemy", "Homonymy", 
                                 M1.conc.adjustment, diff_name = "P.boost")
M2.by.subj <- calc_subject_level_USE(df.M2.bal, "Polysemy", "Homonymy", 
                                 M2.conc.adjustment, diff_name = "P.boost")
M3.by.subj <- calc_subject_level_USE(df.M3.prime, "Related", "Unrelated", 
                                 M3.conc.adjustment, diff_name = "R.boost")

M1.descriptive <- paste("N =", nrow(M1.by.subj), 
                        ", positive boost:", round(nrow(M1.by.subj %>% filter(P.boost.raw > 0.0)) / nrow(M1.by.subj), 2),
                        ", adjusted boost:", round(nrow(M1.by.subj %>% filter(P.boost.adjusted > 0.0)) / nrow(M1.by.subj), 2))

M2.descriptive <- paste("N =", nrow(M2.by.subj), 
                        ", positive boost:", round(nrow(M2.by.subj %>% filter(P.boost.raw > 0.0)) / nrow(M2.by.subj), 2),
                        ", adjusted boost:", round(nrow(M2.by.subj %>% filter(P.boost.adjusted > 0.0)) / nrow(M2.by.subj), 2))

M3.descriptive <- paste("N =", nrow(M3.by.subj), 
                        ", positive boost:", round(nrow(M3.by.subj %>% filter(R.boost.raw > 0.0)) / nrow(M3.by.subj), 2),
                        ", adjusted boost:", round(nrow(M3.by.subj %>% filter(R.boost.adjusted > 0.0)) / nrow(M3.by.subj), 2))



M1.mc <- Mclust(M1.by.subj$P.boost.raw, G = 1:5)
M2.mc <- Mclust(M2.by.subj$P.boost.raw, G = 1:5)
M3.mc <- Mclust(M3.by.subj$R.boost.raw, G = 1:5)
M1.mc.adj <- Mclust(M1.by.subj$P.boost.adjusted, G = 1:5)
M2.mc.adj <- Mclust(M2.by.subj$P.boost.adjusted, G = 1:5)
M3.mc.adj <- Mclust(M3.by.subj$R.boost.adjusted, G = 1:5)

summary(M1.mc)
summary(M2.mc)
summary(M3.mc)
summary(M3.mc.adj)

M1.mc$parameters$mean
M2.mc$parameters$mean
M3.mc$parameters$mean
M1.mc.adj$parameters$mean
M2.mc.adj$parameters$mean
M3.mc.adj$parameters$mean



M3.by.subj$cluster <- M3.mc.adj$classification

# Summary by cluster
M3.by.subj %>% 
  group_by(cluster) %>% 
  summarise(
    n = n(),
    mean = mean(R.boost.adjusted),
    sd = sd(R.boost.adjusted),
    range = paste(round(min(R.boost.adjusted), 2), "to", round(max(R.boost.adjusted), 2))
  )


