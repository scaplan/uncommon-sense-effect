
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Fit primary regression models (separate from plotting
##  or reporting model comparisons)
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Fit primary regression models...")


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


models_to_save <- list()

df.orig.raw <- readRDS(inputpath)
df.orig <- df.orig.raw %>% filter(excl.subj == 0) %>% filter(excl.trial == 0)
nrow(df.orig)
rm(df.orig.raw)

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
## Shared prep and helpers
center_scale <- function(input.df) {
  input.df$RT <- scale(input.df$RT, center = TRUE, scale = TRUE)
  input.df$ItemNum <- scale(input.df$ItemNum, center = TRUE, scale = TRUE)
  input.df$stim.ConcTruth <- scale(input.df$stim.ConcTruth, center = TRUE, scale = TRUE)
  input.df$stim.premiseJudgment_P1 <- scale(input.df$stim.premiseJudgment_P1, center = TRUE, scale = TRUE)
  input.df$stim.premiseJudgment_P2 <- scale(input.df$stim.premiseJudgment_P2, center = TRUE, scale = TRUE)
  return(input.df)
}


prepare_trial_data <- function(df, trial_types, ref_level, sum_coded_factor = NULL) {
  df_out <- df %>% 
    filter(TrialType %in% trial_types) %>% 
    droplevels() %>% 
    center_scale()  # Scales all continuous vars
  
  # TrialType: always treatment/dummy coded
  df_out$TrialType <- relevel(df_out$TrialType, ref = ref_level)
  
  # Optional sum-coded factor (SyllogismSimple or loadtime)
  if (!is.null(sum_coded_factor) && sum_coded_factor %in% names(df_out)) {
    df_out[[sum_coded_factor]] <- as.factor(df_out[[sum_coded_factor]])
    contrasts(df_out[[sum_coded_factor]]) <- contr.sum(nlevels(df_out[[sum_coded_factor]]))
  }
  
  return(df_out)
}


fit_our_standard_full_glmer_model <- function(formula_as_str, data) {
  return(
    glmer(
      formula_as_str,
      data = data,
      family = binomial(link = 'logit'),
      control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa"))
    )
  )
}


random_terms_spec_intercept_only <- c("(1 | SubjectID)",
                       "(1 | TargetWord)")

random_terms_spec_slopes <- c("(1 + TrialType | SubjectID)",
                       "(1 | TargetWord)")

possible_random_terms <- list(
  randInt = random_terms_spec_intercept_only, 
  randSlope = random_terms_spec_slopes
  )

build_formula_str <- function(fixed_terms, random_terms) {
  paste("Response ~ 1 +", paste(c(fixed_terms, random_terms), collapse = " + "))
}




announce_progress <- function(currexp) {
  cat("\n=====================================\n")
  cat(paste0("     Fitting ", currexp, " models...\n"))
  cat("=====================================\n")
}
###########################################
###########################################






###########################################
## Regression fitting for Main Experiment 1 (Fully Crossed)
ExpCode <- "M1"
announce_progress(ExpCode)

df.M1.cross.PH <- prepare_trial_data(df.M1.cross, c("Homonymy", "Polysemy"), "Homonymy", sum_coded_factor = "SyllogismSimple")
if (RUN_LIVE) { levels(df.M1.cross.PH$SyllogismSimple) }
df.M1.cross.VI <- prepare_trial_data(df.M1.cross, c("Invalid Filler", "Valid Filler"), "Invalid Filler", sum_coded_factor = "SyllogismSimple")


fixed.spec.M1.PH <- c(
  "TrialType * SyllogismSimple",
  "RT",
  "ItemNum",
  "stim.ConcTruth",
  "stim.premiseJudgment_P1",
  "stim.premiseJudgment_P2"
)

## Note that since conclusion strength is (definitionally) confounded with filler
## status here. We're not including the conclusion strength in the model
fixed.spec.M1.VI <- c(
  "TrialType * SyllogismSimple",
  "RT",
  "ItemNum",
  "stim.premiseJudgment_P1",
  "stim.premiseJudgment_P2"
)


M1.comparisons <- list(
  VI = list(data = df.M1.cross.VI, fixed = fixed.spec.M1.VI),
  PH = list(data = df.M1.cross.PH, fixed = fixed.spec.M1.PH)
)


for (rand_name in names(possible_random_terms)) {
  random_term <- possible_random_terms[[rand_name]]
  
  for (comp_name in names(M1.comparisons)) {
    curr_comp <- M1.comparisons[[comp_name]]
    
    # Current model specification (full formula)
    full.spec <- build_formula_str(curr_comp$fixed, random_term)
  
    # Construct and fit models
    model.full <- fit_our_standard_full_glmer_model(full.spec, curr_comp$data) # formula, data
    model.nointeraction <- drop_interaction(model.full, "TrialType", "SyllogismSimple")
    model.notrialtype <- drop_main_effect(model.nointeraction, "TrialType")
    
    # Save (comparisons later)
    models_to_save[[paste(ExpCode, comp_name, rand_name, "full", sep = ".")]] <- model.full
    models_to_save[[paste(ExpCode, comp_name, rand_name, "nointeraction", sep = ".")]] <- model.nointeraction
    models_to_save[[paste(ExpCode, comp_name, rand_name, "notrialtype", sep = ".")]] <- model.notrialtype
    
  }
}

# Sanity checking
if (RUN_LIVE) { anova(models_to_save$M1.PH.randSlope.full, models_to_save$M1.PH.randSlope.nointeraction) }
if (RUN_LIVE) { summary(models_to_save$M1.PH.randSlope.nointeraction) }
###########################################
###########################################





###########################################
## Regression fitting for Main Experiment 2
ExpCode <- "M2"
announce_progress(ExpCode)

df.M2.bal.PH <- prepare_trial_data(df.M2.bal, c("Homonymy", "Polysemy"), "Homonymy")
df.M2.bal.VI <- prepare_trial_data(df.M2.bal, c("Invalid Filler", "Valid Filler"), "Invalid Filler")

## IMPORTANT: *don't* include an effect of syllogism in the M2 results.
## A few reasons for this.
## First, we established in M1 that there was no interaction between syllogism form
## and the Uncommon Sense Effect (unlike lexical properties of the arguments...
## it does not substantially affect participant judgement)
## Second, the design of balancing conclusion strength (which *does* affect judgements
## of argument validity), means that P and H items are matched for conclusion strength 
## overall (at the group level), not within each of the four syllogism forms.
##

fixed.spec.M2.PH <- c(
  "TrialType",
  "RT",
  "ItemNum",
  "stim.ConcTruth",
  "stim.premiseJudgment_P1",
  "stim.premiseJudgment_P2"
)

fixed.spec.M2.VI <- c(
  "TrialType",
  "RT",
  "ItemNum",
  "stim.premiseJudgment_P1",
  "stim.premiseJudgment_P2"
)

M2.comparisons <- list(
  VI = list(data = df.M2.bal.VI, fixed = fixed.spec.M2.VI),
  PH = list(data = df.M2.bal.PH, fixed = fixed.spec.M2.PH)
)


### Finding same results with the more complicated random slopes model, but given the smaller size could report
### the simpler intercept only (doesn't improve model fit notable to justify the increased model complexity)

for (rand_name in names(possible_random_terms)) {
  random_term <- possible_random_terms[[rand_name]]
  
  for (comp_name in names(M2.comparisons)) {
    curr_comp <- M2.comparisons[[comp_name]]
    
    # Current model specification (full formula)
    full.spec <- build_formula_str(curr_comp$fixed, random_term)
    
    model.full <- fit_our_standard_full_glmer_model(full.spec, curr_comp$data) # formula, data
    model.notrialtype <- drop_main_effect(model.full, "TrialType")
    
    # Save (comparisons later)
    models_to_save[[paste(ExpCode, comp_name, rand_name, "full", sep = ".")]] <- model.full
    models_to_save[[paste(ExpCode, comp_name, rand_name, "notrialtype", sep = ".")]] <- model.notrialtype
    
  }
}

if (RUN_LIVE) { anova(models_to_save$M2.PH.randInt.full, models_to_save$M2.PH.randInt.notrialtype) }
if (RUN_LIVE) { summary(models_to_save$M2.PH.randInt.full) }
if (RUN_LIVE) { anova(models_to_save$M2.PH.randSlope.full, models_to_save$M2.PH.randInt.full) }
###########################################
###########################################




###########################################
## Regression fitting for Main Experiment 3
ExpCode <- "M3"
announce_progress(ExpCode)


df.M3.prime.RU <- prepare_trial_data(df.M3.prime, c("Unrelated", "Related"), "Unrelated")
df.M3.prime.VI <- prepare_trial_data(df.M3.prime, c("Invalid Filler", "Valid Filler"), "Invalid Filler")
df.M3.prime.RI <- prepare_trial_data(df.M3.prime, c("Invalid Filler", "Related"), "Invalid Filler")



fixed.spec.M3.RU <- c(
  "TrialType",
  "RT",
  "ItemNum",
  "stim.ConcTruth",
  "stim.premiseJudgment_P1",
  "stim.premiseJudgment_P2"
)

fixed.spec.M3.VI <- c(
  "TrialType",
  "RT",
  "ItemNum",
  "stim.premiseJudgment_P1",
  "stim.premiseJudgment_P2"
)

fixed.spec.M3.RI <- c(
  "TrialType",
  "RT",
  "ItemNum",
  "stim.ConcTruth",
  "stim.premiseJudgment_P1",
  "stim.premiseJudgment_P2"
)


M3.comparisons <- list(
  VI = list(data = df.M3.prime.VI, fixed = fixed.spec.M3.VI),
  RU = list(data = df.M3.prime.RU, fixed = fixed.spec.M3.RU),
  RI = list(data = df.M3.prime.RI, fixed = fixed.spec.M3.RI)
)

## Random slope model has singular fit issues so should stick with the intercept only one here

for (rand_name in names(possible_random_terms)) {
  random_term <- possible_random_terms[[rand_name]]
  
  for (comp_name in names(M3.comparisons)) {
    curr_comp <- M3.comparisons[[comp_name]]
    
    # Current model specification (full formula)
    full.spec <- build_formula_str(curr_comp$fixed, random_term)
    
    # Construct and fit models
    model.full <- fit_our_standard_full_glmer_model(full.spec, curr_comp$data) # formula, data
    model.notrialtype <- drop_main_effect(model.full, "TrialType")
    
    # Save (comparisons later)
    models_to_save[[paste(ExpCode, comp_name, rand_name, "full", sep = ".")]] <- model.full
    models_to_save[[paste(ExpCode, comp_name, rand_name, "notrialtype", sep = ".")]] <- model.notrialtype
    
  }
}


if (RUN_LIVE) { anova(models_to_save$M3.RU.randInt.full, models_to_save$M3.RU.randInt.notrialtype) }
if (RUN_LIVE) { summary(models_to_save$M3.RU.randInt.full) }
if (RUN_LIVE) { summary(models_to_save$M3.RI.randInt.full) }
if (RUN_LIVE) { anova(models_to_save$M3.RU.randSlope.full, models_to_save$M3.RU.randInt.full) }
###########################################
###########################################





###########################################
## Regression fitting for Supplemental Experiment 1 (Load)
ExpCode <- "S1"
announce_progress(ExpCode)

df.S1.load.PH <- prepare_trial_data(df.S1.load, c("Homonymy", "Polysemy"), "Homonymy", sum_coded_factor = "loadtime")
if (RUN_LIVE) { levels(df.S1.load.PH$loadtime) }
df.S1.load.VI <- prepare_trial_data(df.S1.load, c("Invalid Filler", "Valid Filler"), "Invalid Filler", sum_coded_factor = "loadtime")


# Fixed effects for COMBINED model (with loadtime interaction)
fixed.spec.S1.PH.combined <- c(
  "TrialType * loadtime",
  "RT",
  "ItemNum",
  "stim.ConcTruth",
  "stim.premiseJudgment_P1",
  "stim.premiseJudgment_P2"
)
fixed.spec.S1.VI.combined <- c(
  "TrialType * loadtime",
  "RT",
  "ItemNum",
  "stim.premiseJudgment_P1",
  "stim.premiseJudgment_P2"
)

# Fixed effects for SEPARATE models (no loadtime)
fixed.spec.S1.PH.separate <- c(
  "TrialType",
  "RT",
  "ItemNum",
  "stim.ConcTruth",
  "stim.premiseJudgment_P1",
  "stim.premiseJudgment_P2"
)
fixed.spec.S1.VI.separate <- c(
  "TrialType",
  "RT",
  "ItemNum",
  "stim.premiseJudgment_P1",
  "stim.premiseJudgment_P2"
)


S1.comparisons.combined <- list(
  VI = list(data = df.S1.load.VI, fixed = fixed.spec.S1.VI.combined),
  PH = list(data = df.S1.load.PH, fixed = fixed.spec.S1.PH.combined)
)

for (rand_name in names(possible_random_terms)) {
  random_term <- possible_random_terms[[rand_name]]
  
  for (comp_name in names(S1.comparisons.combined)) {
    curr_comp <- S1.comparisons.combined[[comp_name]]
    
    full.spec <- build_formula_str(curr_comp$fixed, random_term)
    
    model.full <- fit_our_standard_full_glmer_model(full.spec, curr_comp$data) # formula, data
    model.nointeraction <- drop_interaction(model.full, "TrialType", "loadtime")
    model.notrialtype <- drop_main_effect(model.nointeraction, "TrialType")
    
    models_to_save[[paste(ExpCode, comp_name, rand_name, "combined", "full", sep = ".")]] <- model.full
    models_to_save[[paste(ExpCode, comp_name, rand_name, "combined", "nointeraction", sep = ".")]] <- model.nointeraction
    models_to_save[[paste(ExpCode, comp_name, rand_name, "combined", "notrialtype", sep = ".")]] <- model.notrialtype
  }
}


## We see a significant interaction (on the Filler trials) between TrialType and load condition
## Thus we'll test all four conditions separately 
load_conditions <- c("750ms", "1500ms", "FiveDig", "ThreeDig")

for (load_cond in load_conditions) {
  # Prepare data for this load condition only
  df.S1.PH.curr_load <- prepare_trial_data(
    df.S1.load %>% filter(loadtime == load_cond),
    c("Homonymy", "Polysemy"), "Homonymy"
  )
  df.S1.VI.curr_load <- prepare_trial_data(
    df.S1.load %>% filter(loadtime == load_cond),
    c("Invalid Filler", "Valid Filler"), "Invalid Filler"
  )
  
  S1.comparisons.curr_load <- list(
    VI = list(data = df.S1.VI.curr_load, fixed = fixed.spec.S1.VI.separate),
    PH = list(data = df.S1.PH.curr_load, fixed = fixed.spec.S1.PH.separate)
  )
  
  for (rand_name in names(possible_random_terms)) {
    random_term <- possible_random_terms[[rand_name]]
    
    for (comp_name in names(S1.comparisons.curr_load)) {
      curr_comp <- S1.comparisons.curr_load[[comp_name]]
      
      full.spec <- build_formula_str(curr_comp$fixed, random_term)
      
      model.full <- fit_our_standard_full_glmer_model(full.spec, curr_comp$data) # formula, data
      model.notrialtype <- drop_main_effect(model.full, "TrialType")
      
      # Save with load condition in the name
      models_to_save[[paste(ExpCode, comp_name, rand_name, load_cond, "full", sep = ".")]] <- model.full
      models_to_save[[paste(ExpCode, comp_name, rand_name, load_cond, "notrialtype", sep = ".")]] <- model.notrialtype
    }
  }
}

## Confirmed. Participants are completing the experiment above chance across all conditions
## Though some load manipulations are more taxing than others. We expect that this should
## manifest in a comparable change in magnitude of the USE on the P vs. H trials
## *if* underspecified representations are active automatically / by default processes
## and that's what drives USE

###########################################
###########################################



cat("\n========================================\n")
cat("All models fitted! Total:", length(models_to_save), "\n")
cat("Saving to:", paste0(outputdir, "fitted-models.rds"), "\n")
cat("========================================\n")
saveRDS(models_to_save, paste0(outputdir, "fitted-models.rds"))


