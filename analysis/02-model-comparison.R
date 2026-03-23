
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Main model comparison
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Model comparison and main reported stats...")


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
models <- readRDS(inputpath)

if (RUN_LIVE) {
  names(models)
}

prep_regression_table_for_display <- function(model, effect_names) {
  # Get model summary with confidence intervals
  model_fits <- summ(model, confint = TRUE, digits = 4)
  odds_ratios <- computeOddsRatios(model)
  
  # Extract coefficient table
  display_table <- cleanUpDecimals(as.data.frame(model_fits$coeftable))
  
  # Add clean names and reorganize
  display_table$`Fixed effect` <- effect_names
  display_table <- display_table[c(ncol(display_table), 1:(ncol(display_table)-1))]
  rownames(display_table) <- NULL
  display_table$`Odds Ratio` <- odds_ratios$`Odds Ratio`
  
  return(display_table)
}


get_lrt_as_prose_string <- function(anova_obj, digits = 2) {
  row <- anova_obj[nrow(anova_obj), ]
  
  chisq <- round(row$Chisq, digits)
  deg_free <- row$Df
  p <- row$`Pr(>Chisq)`
  
  p_str <- if (is.na(p)) {
    "\\textit{p} = NA"
  } else if (p < .001) {
    "\\textit{p} < .001"
  } else {
    paste0("\\textit{p} = ", formatC(p, digits = 3, format = "f"))
  }
  
  paste0("$\\chi^2(", deg_free, ") = ", chisq, ", ", p_str, "$")
}


print_lrt_both <- function(lrt_expr, file, comparison_desc) {
  # Evaluate the LRT expression once in the parent environment
  lrt_result <- eval(substitute(lrt_expr), envir = parent.frame())
  
  # Print raw anova output
  print_or_save(lrt_result, 
                file = file, 
                header = paste("###", currexp_name, "Nested Models:", comparison_desc))
  
  # Print prose version
  prose_string <- get_lrt_as_prose_string(lrt_result)
  print_or_save(prose_string, 
                file = file, 
                header = paste("###", currexp_name, "LRT prose for paper:", comparison_desc))
  
  # Return the result invisibly
  invisible(lrt_result)
}

###########################################
###########################################





###########################################
## Main Experiment 1
currexp <- "M1"
currexp_name <- "Exp 1 (Fully Crossed)"
compare_VI <- "Valid vs. Invalid Fillers"
compare_PH <- "Polysemy vs. Homonymy"

if (!RUN_LIVE) {
  M1_output_file <- paste0(outputdir, "M1_model_comparisons.txt")
  file.create(M1_output_file)
}

M1.models <- models[startsWith(names(models), currexp)]

if (RUN_LIVE) {
  names(M1.models)
}



print_lrt_both(
  anova(M1.models$M1.VI.randSlope.full, M1.models$M1.VI.randInt.full),
  file = M1_output_file, comparison_desc = "Full with random slopes or intercepts (Valid / Invalid Fillers)"
  )

## The random slope model is a better fit than the random-intercept model so we'll stick with that
## Then confirming that the full model is better than the one without trial-type (or the interaction term)
print_lrt_both(
  anova(M1.models$M1.VI.randSlope.full, M1.models$M1.VI.randSlope.nointeraction),
  file = M1_output_file, comparison_desc = "Full vs. no interaction with syllogism (Valid / Invalid Fillers)"
)
print_lrt_both(
  anova(M1.models$M1.VI.randSlope.full, M1.models$M1.VI.randSlope.notrialtype),
  file = M1_output_file, comparison_desc = "Full vs no trial type (Valid / Invalid Fillers)"
)

## So the best fitting VI model is the full one (with random slopes)
model.VI.best.M1 <- M1.models$M1.VI.randSlope.full
print_or_save(
  summary(model.VI.best.M1),
  file = M1_output_file, header = paste("###", currexp, "Summary Table:", "Best fitting model (VI)", compare_VI))




### Then do the same procedure but for PH trials

print_lrt_both(
  anova(M1.models$M1.PH.randSlope.full, M1.models$M1.PH.randInt.full),
  file = M1_output_file, comparison_desc = "Full with random slopes or intercepts (Polysemy / Homonymy)"
)
## The random slope model is a better fit than the random-intercept model so we'll stick with that

print_lrt_both(
  anova(M1.models$M1.PH.randSlope.full, M1.models$M1.PH.randSlope.nointeraction),
  file = M1_output_file, comparison_desc = "Full vs. no interaction with syllogism (Polysemy / Homonymy)"
)
## The interaction term actually doesn't improve fit on PH trials

print_lrt_both(
  anova(M1.models$M1.PH.randSlope.nointeraction, M1.models$M1.PH.randSlope.notrialtype),
  file = M1_output_file, comparison_desc = "No interaction vs no trial type (Polysemy / Homonymy)"
)
## But there's a strong effect of Trial type as main effect


## and no effect of trial number
no.item.number <- drop_main_effect(M1.models$M1.PH.randSlope.nointeraction, "ItemNum")
print_lrt_both(
  anova(M1.models$M1.PH.randSlope.nointeraction, no.item.number),
  file = M1_output_file, comparison_desc = "No interaction vs no item number (Polysemy / Homonymy)"
)

with.item.PH.interaction <- add_interaction(M1.models$M1.PH.randSlope.nointeraction, "TrialType", "ItemNum")
summary(with.item.PH.interaction)
print_lrt_both(
  anova(M1.models$M1.PH.randSlope.nointeraction, with.item.PH.interaction),
  file = M1_output_file, comparison_desc = "No interaction vs no item number (Polysemy / Homonymy)"
)


## and no effect of premise plausibility (or for P2 it's in the negative direction)
no.P1 <- drop_main_effect(M1.models$M1.PH.randSlope.nointeraction, "stim.premiseJudgment_P1")
no.P2 <- drop_main_effect(M1.models$M1.PH.randSlope.nointeraction, "stim.premiseJudgment_P2")
print_lrt_both(
  anova(M1.models$M1.PH.randSlope.nointeraction, no.P1),
  file = M1_output_file, comparison_desc = "Main model vs no P1 plausibility (Polysemy / Homonymy)"
)
print_lrt_both(
  anova(M1.models$M1.PH.randSlope.nointeraction, no.P2),
  file = M1_output_file, comparison_desc = "Main model vs no P2 plausibility (Polysemy / Homonymy)"
)


model.PH.best.M1 <- M1.models$M1.PH.randSlope.nointeraction
print_or_save(
  summary(model.PH.best.M1),
  file = M1_output_file, header = paste("###", currexp, "Summary Table:", "Best fitting model (PH)", compare_VI))

##################
##### Display Prep
fixed_effects_names <- c("(Intercept)", "Polysemy", "Darii", "Datisi", "Dimatis",
                         "RT", "Trial Number",
                         "Conclusion Strength", "Premise1 Strength", "Premise2 Strength")
display_table_M1 <- prep_regression_table_for_display(model.PH.best.M1, fixed_effects_names)

print_or_save(
  xtable(display_table_M1), file = M1_output_file,
  header = paste("###", currexp, "Best fitting model display summary for paper:",
                 "xtable", compare_PH)
)
if (RUN_LIVE) { htmlTable(display_table_M1) }


strip_index_columns <- function(df) {
  df[, nzchar(names(df)), drop = FALSE]
}


fixed_effects_names_VI <- c("(Intercept)", "Valid Filler", "Darii", "Datisi", "Dimatis",
                         "RT", "Trial Number",
                         "Premise1 Strength", "Premise2 Strength",
                         "Valid Filler x Darii", "Valid Filler x Datisi", "Valid Filler x Dimatis")
display_table_M1_VI <- prep_regression_table_for_display(model.VI.best.M1, fixed_effects_names_VI)
print_or_save(
  xtable(display_table_M1_VI), file = M1_output_file,
  header = paste("###", currexp, "Best fitting model display summary for paper (Valid / Invalid fillers):",
                 "xtable", compare_PH)
)
if (RUN_LIVE) { htmlTable(display_table_M1_VI) }

###########################################
###########################################




## Now repeat the procedure for M2 and M3


###########################################
## Main Experiment 2
currexp <- "M2"
currexp_name <- "Exp 2 (Balanced conclusion strength)"
compare_VI <- "Valid vs. Invalid Fillers"
compare_PH <- "Polysemy vs. Homonymy"

if (!RUN_LIVE) {
  M2_output_file <- paste0(outputdir, "M2_model_comparisons.txt")
  file.create(M2_output_file)
}

M2.models <- models[startsWith(names(models), currexp)]

if (RUN_LIVE) {
  names(M2.models)
}




print_lrt_both(
  anova(M2.models$M2.VI.randSlope.full, M2.models$M2.VI.randInt.full),
  file = M2_output_file, comparison_desc = "Full with random slopes or intercepts (Valid / Invalid Fillers)"
)

## The random slope model is a better fit than the random-intercept model so we'll stick with that
## Then confirming that the full model is better than the one without trial-type
print_lrt_both(
  anova(M2.models$M2.VI.randSlope.full, M2.models$M2.VI.randSlope.notrialtype),
  file = M2_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
)


## So the best fitting VI model is the full one (with random slopes)
model.VI.best.M2 <- M2.models$M2.VI.randSlope.full
print_or_save(
  summary(model.VI.best.M2),
  file = M2_output_file, header = paste("###", currexp, "Summary Table:", "Best fitting model (VI)", compare_VI))






## Now for the critical PH trials

print_lrt_both(
  anova(M2.models$M2.PH.randSlope.full, M2.models$M2.PH.randInt.full),
  file = M2_output_file, comparison_desc = "Full with random slopes or intercepts (Polysemy / Homonymy)"
)

## The random slope model is NOT a better fit than the random-intercept model so we'll simplify to the intercept-only one

## Then confirming that the full model is better than the one without trial-type 
print_lrt_both(
  anova(M2.models$M2.PH.randInt.full, M2.models$M2.PH.randInt.notrialtype),
  file = M2_output_file, comparison_desc = "Full vs. no trial type (Polysemy / Homonymy)"
)


## So the best fitting VI model is the full one (with random intercepts)
model.PH.best.M2 <- M2.models$M2.PH.randInt.full
print_or_save(
  summary(model.PH.best.M2),
  file = M2_output_file, header = paste("###", currexp, "Summary Table:", "Best fitting model (PH)", compare_VI))
## And there's a strong effect of Trial type as main effect


## and no effect of trial number
no.item.number <- drop_main_effect(M2.models$M2.PH.randInt.full, "ItemNum")
print_lrt_both(
  anova(M2.models$M2.PH.randInt.full, no.item.number),
  file = M1_output_file, comparison_desc = "no item number (Polysemy / Homonymy)"
)


with.item.PH.interaction <- add_interaction(M2.models$M2.PH.randInt.full, "TrialType", "ItemNum")
summary(with.item.PH.interaction)
print_lrt_both(
  anova(M2.models$M2.PH.randInt.full, with.item.PH.interaction),
  file = M2_output_file, comparison_desc = "Testing item-number Trial-Type interaction (Polysemy / Homonymy)"
)


## and no effect of premise plausibility (or for P2 it's in the negative direction)
no.P1 <- drop_main_effect(M2.models$M2.PH.randInt.full, "stim.premiseJudgment_P1")
no.P2 <- drop_main_effect(M2.models$M2.PH.randInt.full, "stim.premiseJudgment_P2")
print_lrt_both(
  anova(M2.models$M2.PH.randInt.full, no.P1),
  file = M2_output_file, comparison_desc = "Main model vs no P1 plausibility (Polysemy / Homonymy)"
)
print_lrt_both(
  anova(M2.models$M2.PH.randInt.full, no.P2),
  file = M2_output_file, comparison_desc = "Main model vs no P2 plausibility (Polysemy / Homonymy)"
)




##################
##### Display Prep
fixed_effects_names <- c("(Intercept)", "Polysemy", "RT", "Trial Number", "Conclusion Strength", "Premise1 Strength", "Premise2 Strength")
display_table_M2 <- prep_regression_table_for_display(model.PH.best.M2, fixed_effects_names)
print_or_save(
  xtable(display_table_M2), file = M2_output_file,
  header = paste("###", currexp, "Best fitting model display summary for paper:",
                 "xtable", compare_PH)
)
if (RUN_LIVE) { htmlTable(display_table_M2) }




fixed_effects_names_VI <- c("(Intercept)", "Valid Filler",
                            "RT", "Trial Number",
                            "Premise1 Strength", "Premise2 Strength")
display_table_M2_VI <- prep_regression_table_for_display(model.VI.best.M2, fixed_effects_names_VI)
print_or_save(
  xtable(display_table_M2_VI), file = M2_output_file,
  header = paste("###", currexp, "Best fitting model display summary for paper (Valid / Invalid fillers):",
                 "xtable", compare_VI)
)
if (RUN_LIVE) { htmlTable(display_table_M2_VI) }
###########################################
###########################################






###########################################
## Regression fitting for Main Experiment 3
currexp <- "M3"
currexp_name <- "Exp 3 (Semantic Relatedness)"
compare_RU <- "Related vs. Unrelated"
if (!RUN_LIVE) {
  M3_output_file <- paste0(outputdir, "M3_model_comparisons.txt")
  file.create(M3_output_file)
}

## Random slope model has singular fit issues so sticking with the intercept only one here

M3.models <- models[startsWith(names(models), currexp)]

if (RUN_LIVE) {
  names(M3.models)
}



## Note that since conclusion strength is (definitionally) confounded with filler
## status here. We're not including the conclusion strength in the model


print_lrt_both(
  anova(M3.models$M3.VI.randSlope.full, M3.models$M3.VI.randInt.full),
  file = M3_output_file, comparison_desc = "Full with random slopes or intercepts (Valid / Invalid Fillers)"
)

## The random slope model is a better fit than the random-intercept model so we'll stick with that
## Then confirming that the full model is better than the one without trial-type
print_lrt_both(
  anova(M3.models$M3.VI.randSlope.full, M3.models$M3.VI.randSlope.notrialtype),
  file = M3_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
)


## So the best fitting VI model is the full one (with random slopes)
model.VI.best.M3 <- M3.models$M3.VI.randSlope.full
print_or_save(
  summary(model.VI.best.M3),
  file = M3_output_file, header = paste("###", currexp, "Summary Table:", "Best fitting model (VI)", compare_VI))



print_lrt_both(
  anova(M3.models$M3.RU.randInt.full, M3.models$M3.RU.randSlope.full),
  file = M3_output_file, comparison_desc = "Full with random slopes or intercepts (Related / Unrelated)"
)
# summary(M3.models$M3.RU.randSlope.full)
## Random slope model not a significantly better fit. Also the fact that slope model shows singular fit issues
## So we proceed with the random-intercept model



## Then checking if the full model is better than the one without trial-type 
print_lrt_both(
  anova(M3.models$M3.RU.randInt.full, M3.models$M3.RU.randInt.notrialtype),
  file = M2_output_file, comparison_desc = "Full vs. no trial type (Related / Unrelated)"
)


## So the best fitting RU model is the full one (with random intercepts)
## Although this is likely due to the correlation with conclusion strength (and it doesn't actually lead to argument acceptance)
model.RU.best.M3 <- M3.models$M3.RU.randInt.full
print_or_save(
  summary(model.RU.best.M3),
  file = M3_output_file, header = paste("###", currexp, "Summary Table:", "Best fitting model (PH)", compare_VI))

# 
# with.interaction <- update(
#   model.RU.best.M3,
#   as.formula(paste(". ~ . +", "TrialType*stim.ConcTruth"))
# )
# summary(with.interaction)
# 
# print_lrt_both(
#   anova(model.RU.best.M3, with.interaction),
#   file = M3_output_file, comparison_desc = "Testing"
# )




##################
##### Display Prep
fixed_effects_names <- c("(Intercept)", "Semantically Related", "RT", "Trial Number", "Conclusion Strength", "Premise1 Strength", "Premise2 Strength")
display_table_M3 <- prep_regression_table_for_display(model.RU.best.M3, fixed_effects_names)

print_or_save(
  xtable(display_table_M3), file = M3_output_file,
  header = paste("###", currexp, "Best fitting model display summary for paper:",
                 "xtable", compare_RU)
)
if (RUN_LIVE) { htmlTable(display_table_M3) }



fixed_effects_names_VI <- c("(Intercept)", "Valid Filler",
                            "RT", "Trial Number",
                            "Premise1 Strength", "Premise2 Strength")
display_table_M3_VI <- prep_regression_table_for_display(model.VI.best.M3, fixed_effects_names_VI)
print_or_save(
  xtable(display_table_M3_VI), file = M3_output_file,
  header = paste("###", currexp, "Best fitting model display summary for paper (Valid / Invalid fillers):",
                 "xtable", compare_VI)
)
if (RUN_LIVE) { htmlTable(display_table_M2_VI) }
###########################################
###########################################







###########################################
## Regression fitting for Supplemental Experiment 1 (Load)
currexp <- "S1"
currexp_name <- "SI Exp 1 (Cognitive Load)"
compare_PH <- "Polysemy vs. Homonymy"
if (!RUN_LIVE) {
  S1_output_file <- paste0(outputdir, "S1_model_comparisons.txt")
  file.create(S1_output_file)
}

S1.models <- models[startsWith(names(models), currexp)]


if (RUN_LIVE) {
  names(S1.models)
}



## The random slope model is a better fit than the random-intercept model so we'll stick with that
print_lrt_both(
  anova(S1.models$S1.VI.randSlope.combined.full, S1.models$S1.VI.randInt.combined.full),
  file = S1_output_file, comparison_desc = "Full with random slopes or intercepts (Valid / Invalid Fillers)"
)

## Then confirming that the full model is better than the one without trial-type
print_lrt_both(
  anova(S1.models$S1.VI.randSlope.combined.full, S1.models$S1.VI.randSlope.combined.notrialtype),
  file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
)

print_lrt_both(
  anova(S1.models$S1.VI.randSlope.combined.full, S1.models$S1.VI.randSlope.combined.nointeraction),
  file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
)


## So the best fitting VI model is the full one (with random slopes)
model.VI.best.S1 <- S1.models$S1.VI.randSlope.combined.full
print_or_save(
  summary(model.VI.best.S1),
  file = S1_output_file, header = paste("###", currexp, "Summary Table:", "Best fitting model (VI)", compare_VI))




### Same thing but for PH



## The random slope model is a better fit than the random-intercept model so we'll stick with that
print_lrt_both(
  anova(S1.models$S1.PH.randSlope.combined.full, S1.models$S1.PH.randInt.combined.full),
  file = S1_output_file, comparison_desc = "Full with random slopes or intercepts (Polysemy / Homonymy)"
)

## Then confirming that the full model is better than the one without trial-type
print_lrt_both(
  anova(S1.models$S1.PH.randSlope.combined.full, S1.models$S1.PH.randSlope.combined.notrialtype),
  file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
)

print_lrt_both(
  anova(S1.models$S1.PH.randSlope.combined.full, S1.models$S1.PH.randSlope.combined.nointeraction),
  file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
)

print_lrt_both(
  anova(S1.models$S1.PH.randSlope.combined.nointeraction, S1.models$S1.PH.randSlope.combined.notrialtype),
  file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
)


## So the best fitting VI model is the full one (with random intercepts) and no interaction between Polysemy and load condition
model.PH.best.S1 <- S1.models$S1.PH.randSlope.combined.nointeraction
print_or_save(
  summary(model.PH.best.S1),
  file = S1_output_file, header = paste("###", currexp, "Summary Table:", "Best fitting model (PH)", compare_VI))
## And there's a strong effect of Trial type as main effect



# 
# ## We see a significant interaction between TrialType and load condition
# ## Thus we'll test all four conditions separately 
# print_lrt_both(
#   anova(S1.models$S1.VI.randSlope.1500ms.full, S1.models$S1.VI.randSlope.1500ms.notrialtype),
#   file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
# )
# print_lrt_both(
#   anova(S1.models$S1.VI.randSlope.750ms.full, S1.models$S1.VI.randSlope.750ms.notrialtype),
#   file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
# )
# print_lrt_both(
#   anova(S1.models$S1.VI.randSlope.ThreeDig.full, S1.models$S1.VI.randSlope.ThreeDig.notrialtype),
#   file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
# )
# print_lrt_both(
#   anova(S1.models$S1.VI.randSlope.FiveDig.full, S1.models$S1.VI.randSlope.FiveDig.notrialtype),
#   file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
# )
# 
# 
# ## Confirmed. Participants are completing the experiment above chance across all conditions
# ## Though some load manipulations are more taxing than others. We expect that this should
# ## manifest in a comparable change in magnitude of the USE on the P vs. H trials
# ## *if* underspecified representations are active automatically / by default processes
# ## and that's what drives USE
# 
# print_lrt_both(
#   anova(S1.models$S1.PH.randSlope.1500ms.full, S1.models$S1.PH.randSlope.1500ms.notrialtype),
#   file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
# )
# print_lrt_both(
#   anova(S1.models$S1.PH.randSlope.750ms.full, S1.models$S1.PH.randSlope.750ms.notrialtype),
#   file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
# )
# print_lrt_both(
#   anova(S1.models$S1.PH.randSlope.ThreeDig.full, S1.models$S1.PH.randSlope.ThreeDig.notrialtype),
#   file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
# )
# print_lrt_both(
#   anova(S1.models$S1.PH.randSlope.FiveDig.full, S1.models$S1.PH.randSlope.FiveDig.notrialtype),
#   file = S1_output_file, comparison_desc = "Full vs. no trial type (Valid / Invalid Fillers)"
# )



##################
##### Display Prep
fixed_effects_names <- c("(Intercept)", "Polysemy", "1500ms Load", "750ms Load", "Five-Digit Load", "RT", "Trial Number", "Conclusion Strength", "Premise1 Strength", "Premise2 Strength")
display_table_S1 <- prep_regression_table_for_display(model.PH.best.S1, fixed_effects_names)

print_or_save(
  xtable(display_table_S1), file = S1_output_file,
  header = paste("###", currexp, "Best fitting model display summary for paper:",
                 "xtable", compare_PH)
)
if (RUN_LIVE) { htmlTable(display_table_S1) }

###########################################
###########################################



