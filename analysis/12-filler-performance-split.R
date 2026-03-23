
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Analysis after splitting on filler-performance
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Analysis after splitting on filler-performance...")


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

setwd(outputdir)

df.orig.raw <- readRDS(inputpath)
df.orig <- df.orig.raw %>% filter(excl.subj == 0) %>% filter(excl.trial == 0)
if (RUN_LIVE) { nrow(df.orig) }

if (RUN_LIVE) {
  # nrow(df.orig)
  table(df.orig$ExpNum)
}


df.M1.cross <- df.orig %>% filter(ExpNum == "M1-cross")
df.M2.bal <- df.orig %>% filter(ExpNum == "M2-bal")
df.M3.prime <- df.orig %>% filter(ExpNum == "M3-prime")
df.S1.load <- df.orig %>% filter(ExpNum == "S1-load-M2")




calculate_filler_performance <- function(df, ceiling_threshold = 85, good_threshold = 70) {
  df %>%
    filter(TrialType %in% c("Valid Filler", "Invalid Filler")) %>%
    mutate(ResponseWasCorrect.numeric = as.numeric(ResponseWasCorrect) - 1) %>%
    group_by(SubjectID) %>%
    summarise(Filler.Perf.Num = mean(ResponseWasCorrect.numeric) * 100) %>%
    mutate(Filler.Performance = case_when(
      Filler.Perf.Num >= ceiling_threshold ~ "Ceiling",
      Filler.Perf.Num >= good_threshold & Filler.Perf.Num < ceiling_threshold ~ "Good",
      Filler.Perf.Num < good_threshold ~ "Bad",
      TRUE ~ NA_character_
    ))
}

df.M1.by.subj.filler <- calculate_filler_performance(df.M1.cross)
df.M2.by.subj.filler <- calculate_filler_performance(df.M2.bal)
df.M3.by.subj.filler <- calculate_filler_performance(df.M3.prime)
df.S1.by.subj.filler <- calculate_filler_performance(df.S1.load)





if (RUN_LIVE) {
  df.M1.by.subj.filler %>% group_by(Filler.Performance) %>% dplyr::summarise(n = n())
}
if (RUN_LIVE) {
  df.M2.by.subj.filler %>% group_by(Filler.Performance) %>% dplyr::summarise(n = n())
}
if (RUN_LIVE) {
  df.M3.by.subj.filler %>% group_by(Filler.Performance) %>% dplyr::summarise(n = n())
}
if (RUN_LIVE) {
  df.S1.by.subj.filler %>% group_by(Filler.Performance) %>% dplyr::summarise(n = n())
}



df.M1.cross <- df.M1.cross %>% left_join(df.M1.by.subj.filler, by = "SubjectID")
df.M2.bal <- df.M2.bal %>% left_join(df.M2.by.subj.filler, by = "SubjectID")
df.M3.prime <- df.M3.prime %>% left_join(df.M3.by.subj.filler, by = "SubjectID")
df.S1.load <- df.S1.load %>% left_join(df.S1.by.subj.filler, by = "SubjectID")




df.M1.cross <- df.M1.cross %>% 
  mutate(target_syll_pair = paste(TargetWord, SyllogismSimple, sep = "_"))


# Finding subset of P and H stims with balanced conclusion strength
df.p.h.stims <- df.M1.cross %>% select(c(target_syll_pair, TrialType, SyllogismSimple, stim.ConcTruth)) %>%
  filter(TrialType %in% c("Homonymy", "Polysemy")) %>% distinct()


if (RUN_LIVE) { 
  # Initial imbalance of P and H conc_strength
  df.p.h.stims %>% 
    group_by(TrialType) %>%
    summarise(ConcStrength = mean(stim.ConcTruth))
}

poly_df <- df.p.h.stims %>% filter(TrialType == "Polysemy")
homo_df <- df.p.h.stims %>% filter(TrialType == "Homonymy")
n_poly <- nrow(poly_df)
n_homo <- nrow(homo_df)
k <- 20  # number of items per group in subset
c_poly <- poly_df$stim.ConcTruth
c_homo <- homo_df$stim.ConcTruth
best <- list(diff = Inf, poly_idx = NULL, homo_idx = NULL)
iters <- 250000  # number of random sample combinations
set.seed(123)

for (it in seq_len(iters)) {
  xp <- sample(n_poly, k)
  yh <- sample(n_homo, k)
  diff <- abs(mean(c_poly[xp]) - mean(c_homo[yh]))
  
  if (diff < best$diff) {
    best <- list(diff = diff, poly_idx = xp, homo_idx = yh)
  }
}

best_subset <- bind_rows(
  poly_df[best$poly_idx, ],
  homo_df[best$homo_idx, ]
)

best_subset %>%
  group_by(TrialType) %>%
  summarise(
    n = n(),
    mean_prop_c = mean(stim.ConcTruth),
    .groups = "drop"
  )

# Add these as a new column
df.M1.cross <- df.M1.cross %>%
  mutate(in_balanced_PH_subset = case_when(
    TrialType %in% c("Valid Filler", "Invalid Filler") ~ TRUE,
    TrialType %in% c("Polysemy", "Homonymy") & target_syll_pair %in% best_subset$target_syll_pair ~ TRUE,
    TrialType %in% c("Polysemy", "Homonymy") & !(target_syll_pair %in% best_subset$target_syll_pair) ~ FALSE,
    TRUE ~ NA  # else condition...
  ))





df.M1.byperf.group <- df.M1.cross %>% group_by(Filler.Performance, TrialType) %>% summarise(mean = mean(Response),
                                                                                            solo_conc = mean(stim.ConcTruth),
                                                                                            sd = sd(Response),
                                                                                            n = n(),
                                                                                            se = sd / sqrt(n),
                                                                                            ci_lower = mean - 1.96 * se,
                                                                                            ci_upper = mean + 1.96 * se,
                                                                                            .groups = "drop")
df.M1.byperf.group.matched <- df.M1.cross %>% filter(in_balanced_PH_subset == TRUE) %>% group_by(Filler.Performance, TrialType) %>% summarise(mean = mean(Response),
                                                                                                                                              solo_conc = mean(stim.ConcTruth),
                                                                                                                                              sd = sd(Response),
                                                                                                                                              n = n(),
                                                                                                                                              se = sd / sqrt(n),
                                                                                                                                              ci_lower = mean - 1.96 * se,
                                                                                                                                              ci_upper = mean + 1.96 * se,
                                                                                                                                              .groups = "drop")
df.M2.byperf.group <- df.M2.bal %>% group_by(Filler.Performance, TrialType) %>% group_by(Filler.Performance, TrialType) %>% summarise(mean = mean(Response),
                                                                                                                                      solo_conc = mean(stim.ConcTruth),
                                                                                                                                      sd = sd(Response),
                                                                                                                                      n = n(),
                                                                                                                                      se = sd / sqrt(n),
                                                                                                                                      ci_lower = mean - 1.96 * se,
                                                                                                                                      ci_upper = mean + 1.96 * se,
                                                                                                                                      .groups = "drop")
df.M3.byperf.group <- df.M3.prime %>% group_by(Filler.Performance, TrialType) %>% group_by(Filler.Performance, TrialType) %>% summarise(mean = mean(Response),
                                                                                                                                        solo_conc = mean(stim.ConcTruth),
                                                                                                                                        sd = sd(Response),
                                                                                                                                        n = n(),
                                                                                                                                        se = sd / sqrt(n),
                                                                                                                                        ci_lower = mean - 1.96 * se,
                                                                                                                                        ci_upper = mean + 1.96 * se,
                                                                                                                                        .groups = "drop")
df.S1.byperf.group <- df.S1.load %>% group_by(Filler.Performance, TrialType) %>% group_by(Filler.Performance, TrialType) %>% summarise(mean = mean(Response),
                                                                                                                                       solo_conc = mean(stim.ConcTruth),
                                                                                                                                       sd = sd(Response),
                                                                                                                                       n = n(),
                                                                                                                                       se = sd / sqrt(n),
                                                                                                                                       ci_lower = mean - 1.96 * se,
                                                                                                                                       ci_upper = mean + 1.96 * se,
                                                                                                                                       .groups = "drop")
df.S1.byperf.group.eachload <- df.S1.load %>% group_by(Filler.Performance, loadtime, TrialType) %>% group_by(Filler.Performance, TrialType) %>% summarise(mean = mean(Response),
                                                                                                                                                          solo_conc = mean(stim.ConcTruth),
                                                                                                                                                          sd = sd(Response),
                                                                                                                                                          n = n(),
                                                                                                                                                          se = sd / sqrt(n),
                                                                                                                                                          ci_lower = mean - 1.96 * se,
                                                                                                                                                          ci_upper = mean + 1.96 * se,
                                                                                                                                                          .groups = "drop")



df.M1.byperf.group %>%
  filter(TrialType %in% c("Polysemy", "Homonymy")) %>%
  select(Filler.Performance, TrialType, mean) %>%
  pivot_wider(names_from = TrialType, values_from = mean) %>%
  mutate(Polysemy_minus_Homonymy = Polysemy - Homonymy) %>%
  mutate(Filler.Performance = factor(Filler.Performance, levels = c("Ceiling", "Good", "Bad"))) %>%
  arrange(Filler.Performance)
df.M1.byperf.group.matched %>%
  filter(TrialType %in% c("Polysemy", "Homonymy")) %>%
  select(Filler.Performance, TrialType, mean) %>%
  pivot_wider(names_from = TrialType, values_from = mean) %>%
  mutate(Polysemy_minus_Homonymy = Polysemy - Homonymy) %>%
  mutate(Filler.Performance = factor(Filler.Performance, levels = c("Ceiling", "Good", "Bad"))) %>%
  arrange(Filler.Performance)
df.M2.byperf.group %>%
  filter(TrialType %in% c("Polysemy", "Homonymy")) %>%
  select(Filler.Performance, TrialType, mean) %>%
  pivot_wider(names_from = TrialType, values_from = mean) %>%
  mutate(Polysemy_minus_Homonymy = Polysemy - Homonymy) %>%
  mutate(Filler.Performance = factor(Filler.Performance, levels = c("Ceiling", "Good", "Bad"))) %>%
  arrange(Filler.Performance)
df.M3.byperf.group %>%
  filter(TrialType %in% c("Related", "Unrelated")) %>%
  select(Filler.Performance, TrialType, mean) %>%
  pivot_wider(names_from = TrialType, values_from = mean) %>%
  mutate(Related_minus_Unrelated = Related - Unrelated) %>%
  mutate(Filler.Performance = factor(Filler.Performance, levels = c("Ceiling", "Good", "Bad"))) %>%
  arrange(Filler.Performance)
df.S1.byperf.group %>%
  filter(TrialType %in% c("Polysemy", "Homonymy")) %>%
  select(Filler.Performance, TrialType, mean) %>%
  pivot_wider(names_from = TrialType, values_from = mean) %>%
  mutate(Polysemy_minus_Homonymy = Polysemy - Homonymy) %>%
  mutate(Filler.Performance = factor(Filler.Performance, levels = c("Ceiling", "Good", "Bad"))) %>%
  arrange(Filler.Performance)
df.S1.byperf.group.eachload %>%
  filter(TrialType %in% c("Polysemy", "Homonymy")) %>%
  select(Filler.Performance, TrialType, mean) %>%
  pivot_wider(names_from = TrialType, values_from = mean) %>%
  mutate(Polysemy_minus_Homonymy = Polysemy - Homonymy) %>%
  mutate(Filler.Performance = factor(Filler.Performance, levels = c("Ceiling", "Good", "Bad"))) %>%
  arrange(Filler.Performance)





# Calculate USE (difference) at participant level
df.M1.participant.USE <- df.M1.cross %>%
  filter(TrialType %in% c("Polysemy", "Homonymy")) %>%
  group_by(SubjectID, Filler.Performance, TrialType) %>%
  summarise(mean_response = mean(Response), .groups = "drop") %>%
  pivot_wider(names_from = TrialType, values_from = mean_response) %>%
  mutate(USE = Polysemy - Homonymy)

# Now compute group stats with CIs
df.M1.USE.byperf <- df.M1.participant.USE %>%
  group_by(Filler.Performance) %>%
  summarise(
    mean_USE = mean(USE),
    sd_USE = sd(USE),
    n = n(),
    se = sd_USE / sqrt(n),
    ci_lower = mean_USE - 1.96 * se,
    ci_upper = mean_USE + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(Filler.Performance = factor(Filler.Performance, levels = c("Ceiling", "Good", "Bad"))) %>%
  arrange(Filler.Performance)


# Same for Exp 2
df.M2.participant.USE <- df.M2.bal %>%
  filter(TrialType %in% c("Polysemy", "Homonymy")) %>%
  group_by(SubjectID, Filler.Performance, TrialType) %>%
  summarise(mean_response = mean(Response), .groups = "drop") %>%
  pivot_wider(names_from = TrialType, values_from = mean_response) %>%
  mutate(USE = Polysemy - Homonymy)

df.M2.USE.byperf <- df.M2.participant.USE %>%
  group_by(Filler.Performance) %>%
  summarise(
    mean_USE = mean(USE),
    sd_USE = sd(USE),
    n = n(),
    se = sd_USE / sqrt(n),
    ci_lower = mean_USE - 1.96 * se,
    ci_upper = mean_USE + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(Filler.Performance = factor(Filler.Performance, levels = c("Ceiling", "Good", "Bad"))) %>%
  arrange(Filler.Performance)



df.M1.USE.byperf$Experiment <- "Exp 1"
df.M2.USE.byperf$Experiment <- "Exp 2"
df.USE.combined <- rbind(df.M1.USE.byperf, df.M2.USE.byperf)



filler.performance.plot <- ggplot(df.USE.combined, aes(x = Filler.Performance, y = mean_USE, 
                            color = Experiment, group = Experiment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 2) +
  geom_point(position = position_dodge(0.3), size = 16, shape = 18) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.15, linewidth = 2.5,
                position = position_dodge(0.3)) +
  facet_wrap(~ Experiment) +
  labs(y = "Uncommon Sense Effect\n(Polysemy - Homonymy Acceptance Rate)", 
       x = "Filler Performance") +
  scale_color_manual(values = c("Exp 1" = exp1_color, 
                                "Exp 2" = exp2_color)) +
  scale_y_continuous(limits = c(-0.25, 0.55),
                     breaks = seq(-0.2, 0.6, 0.1)) +
  single_pane_theme() + 
  theme(axis.title.y = element_text(margin = margin(r = 15)),
        strip.text = element_text(size = 24))  
if (RUN_LIVE) { filler.performance.plot }
ggsave(plot = filler.performance.plot, paste0(outputdir, "Filler-performance-USE-plot.png"),
       width = 14, height = 7, units = "in")


