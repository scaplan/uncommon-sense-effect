
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Bind individual experiment files into single trial-level results
###########################################################
###########################################################

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Bind individual experiment files into single trial-level results...")


## For handling project structure / relative paths ##
suppressMessages(require("rprojroot"))
sourceDir <- find_root(is_git_root)
source(file.path(sourceDir, "aux", "aux-functions.R"))

args = commandArgs(trailingOnly=TRUE)
RUN_LIVE <- interactive()

if (RUN_LIVE) {
  basepath <- set_live_basepath() # Path in full path to git repo if running individual files live...
  outputpath <- paste(basepath, "data/merged/combined-all-exp-trials-nostim.rds", sep="")
  expstring <- "4-RevampedConclusions,502-FullStudy-main,501-FullStudy-loaded,6-JustPremises,7-FullyCrossedJSC,8-FullyCrossedJSP,9-FullyCrossedAll,10-PremisePrimes,11-ConclusionPrimes,12-FullPrimes"
  # expstring <- "7-FullyCrossedJSC,8-FullyCrossedJSP,9-FullyCrossedAll"
}

output_message <- load_in_libraries()


if (length(args)>2) {
  basepath <- args[1]
  expstring <- args[2]
  outputpath <- args[3]
} else {
  print("Not enough input arguments")
}



reorder_columns <- function(input.df) {
  goal.cols <- c("ExpNum", "SubjectID", "SubjTrial", "ItemNum",
                 "ItemNumQuartile", "Response", "ResponseType", "RT",
                 "TargetWord", "MiddleTerm1", "MiddleTerm2", "TrialType",
                 "ExpectedResponse", "ResponseWasCorrect", "Syllogism", "SyllogismSimple", 
                 "ListNum", "premise1", "premise2", "conclusion",
                 "MiddleSyllogismPairing", "PrimePairRelationType", "excl.subj", "excl.trial", 
                 "loadunload", "loadtype", "loadtime", "digitMatch", 
                 "FullArgument", "stim.ConcTruth", "stim.ConcRT", "stim.Conc.N",
                 "stim.premiseJudgment_P1", "stim.premiseRT_P1", "stim.N_P1", "stim.premiseJudgment_P2", "stim.premiseRT_P2", "stim.N_P2"
  )
  
  in_df_not_goal <- setdiff(names(input.df), goal.cols)
  in_goal_not_df <- setdiff(goal.cols, names(input.df))
  
  # cat("Extra:\n")
  # print(in_df_not_goal)
  # cat("\nMissing:\n")
  # print(in_goal_not_df)
  
  input.df <- input.df %>%
    select(all_of(goal.cols))
}

assign_column_types <- function(input.df) {
  input.df$SubjTrial <- as.character(input.df$SubjTrial)
  input.df$SubjectID <- as.factor(input.df$SubjectID)
  input.df$ExpNum <- as.factor(input.df$ExpNum)
  input.df$ItemNum <- as.numeric(input.df$ItemNum)
  input.df$ItemNumQuartile <- as.factor(input.df$ItemNumQuartile)
  input.df$Response <- as.numeric(input.df$Response)
  input.df$TargetWord <- as.factor(input.df$TargetWord)
  input.df$TrialType <- as.factor(input.df$TrialType)
  input.df$Syllogism <- as.factor(input.df$Syllogism)
  input.df$SyllogismSimple <- as.factor(input.df$SyllogismSimple)
  input.df$MiddleSyllogismPairing <- as.factor(input.df$MiddleSyllogismPairing)
  input.df$ListNum <- as.factor(input.df$ListNum)
  input.df$premise1 <- as.character(input.df$premise1)
  input.df$premise2 <- as.character(input.df$premise2)
  input.df$conclusion <- as.character(input.df$conclusion)
  input.df$digitMatch <- as.factor(input.df$digitMatch)
  input.df$RT <- as.numeric(input.df$RT)
  input.df$ExpectedResponse <- as.factor(input.df$ExpectedResponse)
  input.df$ResponseWasCorrect <- as.factor(input.df$ResponseWasCorrect)
  input.df$excl.subj <- as.factor(input.df$excl.subj)
  input.df$excl.trial <- as.factor(input.df$excl.trial)
  input.df$loadunload <- as.factor(input.df$loadunload)
  input.df$loadtype <- as.factor(input.df$loadtype)
  input.df$loadtime <- as.factor(input.df$loadtime)
  input.df$stim.ConcTruth <- as.numeric(input.df$stim.ConcTruth)
  input.df$stim.ConcRT <- as.numeric(input.df$stim.ConcRT)
  input.df$stim.Conc.N <- as.numeric(input.df$stim.Conc.N)
  input.df$stim.premiseJudgment_P1 <- as.numeric(input.df$stim.premiseJudgment_P1)
  input.df$stim.premiseRT_P1 <- as.numeric(input.df$stim.premiseRT_P1)
  input.df$stim.N_P1 <- as.numeric(input.df$stim.N_P1)
  input.df$stim.premiseJudgment_P2 <- as.numeric(input.df$stim.premiseJudgment_P2)
  input.df$stim.premiseRT_P2 <- as.numeric(input.df$stim.premiseRT_P2)
  input.df$stim.N_P2 <- as.numeric(input.df$stim.N_P2)
  input.df$ResponseType <- as.factor(input.df$ResponseType)
  input.df$PrimePairRelationType <- as.factor(input.df$PrimePairRelationType)
  input.df$MiddleTerm1 <- as.character(input.df$MiddleTerm1)
  input.df$MiddleTerm2 <- as.character(input.df$MiddleTerm2)
  input.df$FullArgument <- as.character(input.df$FullArgument)
  
  return(input.df)
}



add_item_quartile_column <- function(input.df) {
  maxItemNum <- max(input.df$ItemNum)
  input.df <- input.df %>%
    mutate(ItemNumQuartile = case_when(ItemNum <= round(maxItemNum * 0.25) ~ 'First',
                                       ItemNum <= round(maxItemNum * 0.50) ~ 'Second',
                                       ItemNum <= round(maxItemNum * 0.75) ~ 'Third',
                                       TRUE ~ 'Fourth')) %>%
    relocate(ItemNumQuartile, .after = ItemNum)
  input.df$ItemNumQuartile <- factor(input.df$ItemNumQuartile,
                                     levels=c('First', 'Second', 'Third', 'Fourth'))
  return(input.df)
}

annotate_response_type <- function(input.df, RespString) {
  if ("PremiseNum" %in% colnames(input.df)) {
    reformatted.df <- input.df %>%
      mutate(ResponseType = PremiseNum) %>%
      select(-c(PremiseNum))
  } else {
    reformatted.df <- input.df %>%
      mutate(ResponseType = RespString)
  }
  return(reformatted.df)
}

clean_whitespace <- function(input.df) {
  df.cleaned.whitespace <- input.df %>%
    mutate(premise1 = str_trim(premise1)) %>%
    mutate(premise2 = str_trim(premise2)) %>%
    mutate(conclusion = str_trim(conclusion))
  return(df.cleaned.whitespace)
}

add_no_load_cols <- function(input.df) {
  reformatted.df <- input.df %>%
    mutate(loadunload = "unloaded") %>%
    mutate(loadtype = "NA") %>%
    mutate(loadtime = "NA") %>%
    mutate(digitMatch = "NA")
  return(reformatted.df)
}

add_stim_level_placeholders <-  function(input.df) {
  reformatted.df <- input.df %>%
    mutate(stim.ConcTruth = "TBD") %>%
    mutate(stim.ConcRT = "TBD") %>%
    mutate(stim.Conc.N = "TBD") %>%
    mutate(stim.premiseJudgment_P1 = "TBD") %>%
    mutate(stim.premiseRT_P1 = "TBD") %>%
    mutate(stim.N_P1 = "TBD") %>%
    mutate(stim.premiseJudgment_P2 = "TBD") %>%
    mutate(stim.premiseRT_P2 = "TBD") %>%
    mutate(stim.N_P2 = "TBD")
  return(reformatted.df)
}

load_single_file <- function(name) {
  curr_exp_full_path <- paste(basepath, "data/raw/", name, "/proc/",
                              name, "-singleRowPerTrial-labelexclude.csv", sep="")
  df.curr.exp <- read.csv(curr_exp_full_path, stringsAsFactors = T,header=TRUE, sep = ",")
  return(add_item_quartile_column(df.curr.exp))
}


standardize_M1_cross <- function(input.df, RespString) {
  reformatted.df <- add_no_load_cols(input.df)
  reformatted.df <- add_stim_level_placeholders(reformatted.df)
  reformatted.df <- annotate_response_type(reformatted.df, RespString)
  reformatted.df <- clean_whitespace(reformatted.df)
  
  reformatted.df <- reformatted.df %>%
    mutate(PrimePairRelationType = "NA") %>%
    mutate(MiddleTerm1 = TargetWord) %>%
    mutate(MiddleTerm2 = TargetWord) %>%
    mutate(FullArgument = paste(premise1, premise2, conclusion, sep = " -- "))
  
  return(reformatted.df)
}


standardize_M3_prime <- function(input.df, RespString) {
  reformatted.df <- add_no_load_cols(input.df)
  reformatted.df <- add_stim_level_placeholders(reformatted.df)
  reformatted.df <- annotate_response_type(reformatted.df, RespString)
  reformatted.df <- clean_whitespace(reformatted.df)
  
  reformatted.df$ListNum <- reformatted.df$OrderNum
  reformatted.df <- subset(reformatted.df, select = -OrderNum)
  
  reformatted.df <- reformatted.df %>%
    mutate(TargetWord = paste(MiddleTerm1, MiddleTerm2, sep = "_")) %>%
    rename_at('relationtype', ~ 'PrimePairRelationType') %>%
    mutate(MiddleSyllogismPairing = "NA") %>%
    mutate(FullArgument = paste(premise1, premise2, conclusion, sep = " -- "))
  
  return(reformatted.df)
}



standardize_M2_bal <- function(input.df, RespString) {
  reformatted.df <- add_no_load_cols(input.df)
  reformatted.df <- add_stim_level_placeholders(reformatted.df)
  reformatted.df <- annotate_response_type(reformatted.df, RespString)
  reformatted.df <- clean_whitespace(reformatted.df)
  
  columns_only_from_full_arg_version <- c("ConcTruth", "ConcRT",
                                          "FullStudyLoadType", "categoryMistake",
                                          "NominalType", "AC1", "AC2",
                                          "dominancePercent", "abstractType")
  reformatted.df <- reformatted.df %>%
    select(-any_of(columns_only_from_full_arg_version))
  
  reformatted.df <- reformatted.df %>%
    mutate(PrimePairRelationType = "NA") %>%
    mutate(MiddleSyllogismPairing = "NA") %>%
    mutate(MiddleTerm1 = TargetWord) %>%
    mutate(MiddleTerm2 = TargetWord) %>%
    mutate(FullArgument = paste(premise1, premise2, conclusion, sep = " -- "))
  
  return(reformatted.df)
}


extract_argument_lookup_df <- function(intput.df.with.fullarg) {
  intput.df.with.fullarg.whitespace <- clean_whitespace(intput.df.with.fullarg)
  argument_lookup <- intput.df.with.fullarg.whitespace %>%
    select(
      premise1, premise2, conclusion,
      Syllogism, TrialType,
      SyllogismSimple, TargetWord
    ) %>%
    mutate(FullArgument = paste(premise1, premise2, conclusion, sep = " -- ")) %>%
    pivot_longer(
      cols = c(premise1, premise2),
      names_to = "premise_position",
      values_to = "premise"
    ) %>%
    unique()
  
  return(argument_lookup)
}


standardize_N4_bal_premise <- function(input.df) {
  reformatted.df <- add_no_load_cols(input.df)
  reformatted.df <- add_stim_level_placeholders(reformatted.df)
  reformatted.df <- reformatted.df %>% rename(PremiseNum = premisenum)
  reformatted.df <- annotate_response_type(reformatted.df, "")
  
  reformatted.df <- reformatted.df %>%
    mutate(ResponseType = recode(ResponseType, "P1" = "answer-P1")) %>%
    mutate(ResponseType = recode(ResponseType, "P2" = "answer-P2")) %>%
    mutate(PrimePairRelationType = "NA") %>%
    mutate(MiddleSyllogismPairing = "NA") %>%
    mutate(MiddleTerm1 = TargetWord) %>%
    mutate(MiddleTerm2 = TargetWord) %>%
    mutate(premise1 = str_split(FullArgument, "\\s*--\\s*", simplify = TRUE)[, 1],
            premise2 = str_split(FullArgument, "\\s*--\\s*", simplify = TRUE)[, 2]) %>%
    select(-c(premise, premise_position))
  
  reformatted.df <- clean_whitespace(reformatted.df)
  
  return(reformatted.df)
}


standardize_S1_load <- function(input.df, RespString) {
  # reformatted.df <- add_no_load_cols(input.df)
  reformatted.df <- add_stim_level_placeholders(input.df)
  reformatted.df <- annotate_response_type(reformatted.df, RespString)
  reformatted.df <- clean_whitespace(reformatted.df)
  
  reformatted.df <- reformatted.df %>%
    mutate(loadunload = "loaded") %>%
    mutate(loadtype = ifelse(FullStudyLoadType %in% c("750", "1500"), "timing", "digits")) %>%
    mutate(loadtime = ifelse(loadtype == "timing", paste0(as.character(FullStudyLoadType), 'ms'), as.character(FullStudyLoadType)))
  
  columns_only_from_full_arg_version <- c("ConcTruth", "ConcRT", "RandDig",
                                          "FullStudyLoadType", "categoryMistake",
                                          "NominalType", "AC1", "AC2",
                                          "dominancePercent", "abstractType")
  reformatted.df <- reformatted.df %>%
    select(-any_of(columns_only_from_full_arg_version))
  
  reformatted.df <- reformatted.df %>%
    mutate(PrimePairRelationType = "NA") %>%
    mutate(MiddleSyllogismPairing = "NA") %>%
    mutate(MiddleTerm1 = TargetWord) %>%
    mutate(MiddleTerm2 = TargetWord) %>%
    mutate(FullArgument = paste(premise1, premise2, conclusion, sep = " -- "))
  
  return(reformatted.df)
}



# currexp <- "6-JustPremises"
# df.N4.bal.stims <- df.curr.exp %>% select(premise) %>% unique()
# df.N4.bal.stims

currexp <- "501-FullStudy-loaded"


# parse string back into list in order to handle one at a time
exp_list <- as.list(str_split(expstring, ',')[[1]])
df_list <- list()
argument.lookup.df <- data.frame()
for (currexp in exp_list) {
  df.curr.exp <- load_single_file(currexp)
  curr.exp.num <- as.character(df.curr.exp[1,]$ExpNum)
  
  if (curr.exp.num == "M1-cross") {
    df.curr.exp <- standardize_M1_cross(df.curr.exp, "FullArgument") 
  } else if (curr.exp.num == "N1-c-M1") {
    df.curr.exp <- standardize_M1_cross(df.curr.exp, "SoloConclusion")
  } else if (curr.exp.num == "N2-p-M1") {
    df.curr.exp <- standardize_M1_cross(df.curr.exp, "") 
  } else if (curr.exp.num == "M2-bal") {
    df.curr.exp <- standardize_M2_bal(df.curr.exp, "FullArgument") 
  } else if (curr.exp.num == "N3-c-M2") {
    argument.lookup.df <- extract_argument_lookup_df(df.curr.exp)
    df.curr.exp <- standardize_M2_bal(df.curr.exp, "SoloConclusion")
  } else if (curr.exp.num == "N4-p-M2") {
    if (nrow(argument.lookup.df) == 0) {
      stop("For kludgy design reasons you need to read in N3-c-M2 before N4-p-M2!")
    }
    
    # Fix defendents typo
    df.curr.exp <- df.curr.exp %>%
      mutate(premise = str_replace_all(premise, "defendents", "defendants")) %>%
      mutate(premise = str_trim(premise))
    
    df.curr.exp.enriched <- df.curr.exp %>%
      select(-c(TrialType)) %>%
      left_join(
        argument.lookup.df,
        by = "premise",
        relationship = "many-to-many"
      )
    # check <- standardize_N4_bal_premise(df.curr.exp.enriched)
    # check2 <- check %>% select(premise1,premise2,conclusion,FullArgument) %>% unique()
    # check3 <- check %>%
    #   reorder_columns() %>%
    #   assign_column_types()
    
    df.curr.exp <- standardize_N4_bal_premise(df.curr.exp.enriched)
  } else if (curr.exp.num == "M3-prime") {
    df.curr.exp <- standardize_M3_prime(df.curr.exp, "FullArgument")
  } else if (curr.exp.num == "N5-c-M3") {
    df.curr.exp <- standardize_M3_prime(df.curr.exp, "SoloConclusion")
  } else if (curr.exp.num == "N6-p-M3") {
    df.curr.exp <- standardize_M3_prime(df.curr.exp, "")
  } else if (curr.exp.num == "S1-load-M2") {
    df.curr.exp <- standardize_S1_load(df.curr.exp, "FullArgument")
  } else {
    stop("Experiment input string not handled!")
  }
  
  df_list[[length(df_list) + 1]] <- df.curr.exp %>%
    reorder_columns() %>%
    assign_column_types()
}


ExpOrdering <- c("M1-cross", "M2-bal", "M3-prime",
                 "N1-c-M1", "N2-p-M1",
                 "N3-c-M2", "N4-p-M2",
                 "N5-c-M3", "N6-p-M3",
                 "S1-load-M2")

combined_df <- bind_rows(df_list)
combined_df <- combined_df %>%
  mutate(ExpNum = factor(ExpNum, levels = ExpOrdering)) %>%
  arrange(ExpNum)

## Fix 'college' darii / datisi typo
## And remove erroneous "Some gestures are waves" ... "Some waves are ripples" case
combined_df <- combined_df %>%
  mutate(Syllogism = if_else(
    FullArgument == "All colleges grant degrees -- Some colleges are historic buildings -- Some historic buildings grant degrees",
    "Datisi",
      Syllogism
    )) %>%
  mutate(SyllogismSimple = if_else(
    FullArgument == "All colleges grant degrees -- Some colleges are historic buildings -- Some historic buildings grant degrees",
    "Datisi",
    SyllogismSimple
  )) %>%
  filter(FullArgument != "Some gestures are waves -- Some waves are ripples -- Some ripples are gestures")




# write.csv(combined_df, outputpathplain, row.names = F, quote=F)
saveRDS(combined_df, outputpath)


