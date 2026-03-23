
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  No effect of trial number
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Null effect of trial number... (plus minor darii / datisi checking)...")


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
df.M1.cross <- df.orig %>% filter(ExpNum == "M1-cross")
df.M2.bal <- df.orig %>% filter(ExpNum == "M2-bal")


df.by.item.num <- df.M1.M2 %>% 
  filter(TrialType %in% c("Polysemy", "Homonymy")) %>% 
  group_by(ExpNum, ItemNum, TrialType) %>% 
  summarise(Response = mean(Response), .groups = 'drop')

window_size <- 8

df.by.item.num <- df.by.item.num %>%
  arrange(ExpNum, TrialType, ItemNum) %>%
  group_by(ExpNum, TrialType) %>%
  mutate(
    rolling_mean = rollapply(Response, width = window_size, 
                             FUN = function(x) mean(x, na.rm = TRUE), 
                             fill = NA, align = "right", partial = TRUE),
    rolling_sd = rollapply(Response, width = window_size, 
                           FUN = function(x) sd(x, na.rm = TRUE), 
                           fill = NA, align = "right", partial = TRUE),
    n_obs = rollapply(Response, width = window_size,
                      FUN = function(x) sum(!is.na(x)),
                      fill = NA, align = "right", partial = TRUE)
  ) %>%
  ungroup()

df.by.item.num$rolling_sem <- df.by.item.num$rolling_sd / sqrt(df.by.item.num$n_obs)
df.by.item.num$ci_upper <- df.by.item.num$rolling_mean + 1.96 * df.by.item.num$rolling_sem
df.by.item.num$ci_lower <- df.by.item.num$rolling_mean - 1.96 * df.by.item.num$rolling_sem

df.by.item.num.M1.plotting <- df.by.item.num %>% filter(ExpNum == "M1-cross") %>% droplevels()
df.by.item.num.M1.plotting$TrialType <- factor(df.by.item.num.M1.plotting$TrialType, levels = c("Polysemy", "Homonymy"))


pl.rolling.mean <- ggplot(df.by.item.num.M1.plotting, aes(x = ItemNum, color = TrialType, fill = TrialType)) +
  geom_point(aes(y = rolling_mean), size = 4) +
  geom_errorbar(aes(y = rolling_mean, ymin = ci_lower, ymax = ci_upper), 
                width = 1.5, alpha = 0.8, linewidth = 1) +
  single_pane_theme_withLegend(c(0.88, 0.9)) +
  two_set_fill() + two_set_color() +
  coord_cartesian(xlim = c(5, 56),
                  ylim = c(0.1, 0.9)) + 
  scale_x_continuous(breaks = seq(10, 60, 10)) + 
  scale_y_continuous(breaks = seq(0.0, 1.0, 0.2)) + 
  labs(y = "Prop. Valid Argument Judgments", x = "Trial Number", 
       color = "Middle Term", fill = "Middle Term") +
  theme(legend.box.background = element_rect(color = "black", linewidth = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(r = 15)))
if (RUN_LIVE) { pl.rolling.mean }
ggsave(plot = pl.rolling.mean, paste0(outputdir, "Item-Number-Null-Effect.png"),
       width = 12, height = 8, units = "in")







###
## RTs and Darii vs. Datisi
df.orig.clean <- df.orig %>% filter(RT < 30000)
rts <- df.orig.clean %>% filter(ExpNum %in% c("M1-cross","M2-bal", "M3-prime")) %>% group_by(ExpNum, TrialType, Response) %>% dplyr::summarise(rt = mean(RT), na.rm = TRUE)

rt.accept.reject.diff <- rts %>%
  select(ExpNum, TrialType, Response, rt) %>%
  mutate(
    Response = recode(Response,
                      `1` = "Accept",
                      `0` = "Reject")
  ) %>%
  pivot_wider(
    names_from = Response,
    values_from = rt
  ) %>%
  mutate(
    rt_diff = Accept - Reject
  )
if (RUN_LIVE) {  rt.accept.reject.diff }




df.M1.justP <- df.orig.clean %>% filter(ExpNum == "M1-cross") %>% filter(TrialType == "Polysemy")
df.M1.justH <- df.orig.clean %>% filter(ExpNum == "M1-cross") %>% filter(TrialType == "Homonymy")

if (RUN_LIVE) { t.test(df.M1.justP$RT, df.M1.justH$RT) }
# sd(df.M1.justP$RT)
# sd(df.M1.justH$RT)


df.M2.justP <- df.orig.clean %>% filter(ExpNum == "M2-bal") %>% filter(TrialType == "Polysemy")
df.M2.justH <- df.orig.clean %>% filter(ExpNum == "M2-bal") %>% filter(TrialType == "Homonymy")

if (RUN_LIVE) {  t.test(df.M2.justP$RT, df.M2.justH$RT) }
# sd(df.M2.justP$RT)
# sd(df.M2.justH$RT)



df_relevel <- df.M1.cross %>%
  filter(TrialType == "Polysemy") %>%
  filter(SyllogismSimple %in% c("Darii", "Datisi")) %>%
  mutate(SyllogismSimple = relevel(factor(SyllogismSimple), ref = "Datisi"))

model <- glm(Response ~ SyllogismSimple, 
             data = df_relevel,
             family = binomial)
if (RUN_LIVE) { summary(model) }


df_relevel <- df.M2.bal %>%
  filter(TrialType == "Polysemy") %>%
  filter(SyllogismSimple %in% c("Darii", "Datisi")) %>%
  mutate(SyllogismSimple = relevel(factor(SyllogismSimple), ref = "Datisi"))

model <- glm(Response ~ SyllogismSimple, 
             data = df_relevel,
             family = binomial)
if (RUN_LIVE) { summary(model) }


# 
# 
# just.P.datisi <- df.M1.justP %>% filter(SyllogismSimple == "Datisi")
# just.P.darii <- df.M1.justP %>% filter(SyllogismSimple == "Darii")
# 
# just.P.bySyll.Summary <- summary_group_by_sc(df.M1.justP, "Response", "SyllogismSimple")
# just.P.bySyll.Summary <- just.P.bySyll.Summary %>% filter(SyllogismSimple %in% c("Datisi", "Darii")) %>% droplevels()
# 
# p <- ggplot(just.P.bySyll.Summary, aes(x=SyllogismSimple, y=mean_prop, fill=SyllogismSimple)) + 
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=mean_prop-se_prop, ymax=mean_prop+se_prop),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) + single_pane_theme() +
#   ylab("Syllogism Form (Just Polysemy Arguments)") +
#   xlab("Mean Validity Judgment") +
#   single_pane_theme()
# if (RUN_LIVE) { p }  
# ggsave(plot = p, "Exp7-syll-order-no-effect.png",
#        width = 12, height = 12, units = "in")
# 
# 
# 
# 
# just.P.datisi <- df.M2.justP %>% filter(SyllogismSimple == "Datisi")
# just.P.darii <- df.M2.justP %>% filter(SyllogismSimple == "Darii")
# 
# just.P.bySyll.Summary <- summary_group_by_sc(df.M2.justP, "Response", "SyllogismSimple")
# just.P.bySyll.Summary <- just.P.bySyll.Summary %>% filter(SyllogismSimple %in% c("Datisi", "Darii")) %>% droplevels()
# 
# p <- ggplot(just.P.bySyll.Summary, aes(x=SyllogismSimple, y=mean_prop, fill=SyllogismSimple)) + 
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=mean_prop-se_prop, ymax=mean_prop+se_prop),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) + single_pane_theme() +
#   ylab("Syllogism Form (Just Polysemy Arguments)") +
#   xlab("Mean Validity Judgment") +
#   single_pane_theme()
# if (RUN_LIVE) { p }  



