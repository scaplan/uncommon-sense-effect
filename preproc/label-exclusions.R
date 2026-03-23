
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Add column(s) for exclusion labels
###########################################################
###########################################################

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Add column(s) for exclusion labels...")


## For handling project structure / relative paths ##
suppressMessages(require("rprojroot"))
sourceDir <- find_root(is_git_root)
source(file.path(sourceDir, "aux", "aux-functions.R"))

args = commandArgs(trailingOnly=TRUE)
RUN_LIVE <- interactive()

if (RUN_LIVE) {
  currMachine <- Sys.info()[['nodename']]
  if (currMachine == 'scaplanoffice.local') {
    basepath <- "/Users/scaplan/Dropbox/CS_accounts/penn_CS_account/polysemy/uncommon-sense-effect/"
  } else if  (currMachine == 'MiniNazz') {
    basepath <- "/home/scaplan/Dropbox/CS_accounts/penn_CS_account/polysemy/uncommon-sense-effect/"
  } else {
    basepath <- "/Users/spcaplan/Dropbox/CS_accounts/penn_CS_account/polysemy/uncommon-sense-effect/"
  }
  setwd(basepath)
}

output_message <- load_in_libraries()

if (length(args)>2) {
  basepath <- args[1]
  inputfile <- args[2]
  outputfile <- args[3]
} else {
  print("Not enough input arguments")
}

if (RUN_LIVE) {
  sourceDir <- paste(basepath, "data/raw/6-JustPremises/proc/", sep = "")
  # sourceDir <- paste(basepath, "data/raw/9-FullyCrossedAll/proc/", sep = "")
  
  # inputfile <- "1-TruthNorming-singleRowPerTrial.csv"
  # outputfile <- "1-TruthNorming-singleRowPerTrial-labelexclude.csv"
  inputfile <- "6-JustPremises-singleRowPerTrial.csv"
  outputfile <- "6-JustPremises-singleRowPerTrial-labelexclude.csv"
  setwd(sourceDir)
}


df.results <- read.csv(inputfile, stringsAsFactors = T,header=TRUE, sep = ",") # Read in the target words for each instance


apply_label <- function(input.df) {
  df.with.dropsub <- input.df %>%
    filter(dropsub == 1) %>% 
    pull(SubjectID)
  return(df.with.dropsub)
}

merge_bad_boys <- function(input.df.baddies) {
  if (length(input.df.baddies) < 1) {
    stop("Raise error: Can't call merge_bad_boys() with an empty list")
  } else {
    drop.list <- append(input.df.baddies[[1]], input.df.baddies[[2]])
  }
  
  if (length(input.df.baddies) > 1) {
    for(i in 2:length(input.df.baddies)){
      drop.list <- append(drop.list, input.df.baddies[[i]])
    }
  }
  
  drop.list <- sapply(list(drop.list), unique)
  return(drop.list)
  
}


find_subj_accept_invalid <- function(input.df, cutoff) {
  by.subj.accept.invalid <- input.df %>%
    filter(TrialType == "Invalid Filler") %>%
    group_by(SubjectID) %>%
    summarise(false.accept.rate = mean(Response)) %>%
    mutate(dropsub = case_when(false.accept.rate > cutoff ~ 1,
                               TRUE ~ 0)
    )
  return(by.subj.accept.invalid)
}

find_subj_sneeky_speedsters <- function(input.df, lower.bound, upper.bound) {
  by.subj.rts <- input.df %>%
    group_by(SubjectID) %>%
    summarise(meanRT = mean(RT)) %>%
    mutate(dropsub = case_when(meanRT > upper.bound ~ 1,
                               meanRT < lower.bound ~ 1,
                               TRUE ~ 0)
    )
  return(by.subj.rts)
}

find_subj_nonresp <- function(input.df, cutoff) {
  by.subj.nonresp <- input.df %>%
    group_by(SubjectID) %>%
    summarise(nonresponse.rate = mean(NonResponse)) %>%
    mutate(dropsub = case_when(nonresponse.rate >= cutoff ~ 1,
                               TRUE ~ 0))
  return(by.subj.nonresp)
}



apply_exclusion_labels <- function(df, drop.list, drop_nonresponse_trials = TRUE) {
  with.labels <- df %>%
      mutate(
        excl.subj = as.integer(SubjectID %in% drop.list),
        excl.trial = case_when(
          excl.subj == 1 ~ 1,
          drop_nonresponse_trials & NonResponse == 1 ~ 1,
          TRUE ~ 0
        )
      ) %>%
      select(-NonResponse)
  return(with.labels)
}



df.no.empties <- df.results %>% filter(NonResponse == 0)
CurrExp <- as.character(df.no.empties[1,]$ExpNum)
meanRT <- mean(df.no.empties$RT)
sdRT <- sd(df.no.empties$RT)
triple_SD_RT <- sdRT * 3
double_SD_RT <- sdRT * 2
RT_subj_lower_bound <- meanRT - triple_SD_RT
RT_subj_upper_bound <- meanRT + triple_SD_RT
RT_subj_lower_bound_2SD <- meanRT - double_SD_RT
RT_subj_upper_bound_2SD <- meanRT + double_SD_RT



### Can convert most of the subject-level calculations into single calls.
### Just deciding what to do with that is registration specific
by.subject.rts.SD3 <- find_subj_sneeky_speedsters(df.no.empties, RT_subj_lower_bound, RT_subj_upper_bound)
by.subject.rts.SD2 <- find_subj_sneeky_speedsters(df.no.empties, RT_subj_lower_bound_2SD, RT_subj_upper_bound_2SD)
by.subject.false.accept.50 <- find_subj_accept_invalid(df.no.empties, 0.5)
by.subject.false.accept.70 <- find_subj_accept_invalid(df.no.empties, 0.7)
by.subject.nonresponse.rate.10 <- find_subj_nonresp(df.results, 0.1)

bad.boys.outlier.RTs.SD3 <- apply_label(by.subject.rts.SD3)
bad.boys.outlier.RTs.SD2 <- apply_label(by.subject.rts.SD2)
bad.boys.false.accept.50 <- apply_label(by.subject.false.accept.50) 
bad.boys.false.accept.70 <- apply_label(by.subject.false.accept.70) 
bad.boys.nonresponse.10 <- apply_label(by.subject.nonresponse.rate.10)


if (CurrExp == "Pilot-1") {
  # (1) We will exclude participants whose average reaction time across trials
  # is more than three standard deviations (in either direction) from the average reaction time.
  # (2) We will exclude participants who accept more than 50% of the false filler conclusions. 
  # (3) We didn't mention this ahead of time, but no way around excluding non-responses...
  drop.list <- merge_bad_boys(list(bad.boys.false.accept.50, bad.boys.outlier.RTs.SD3))
}

if (CurrExp == "Pilot-2") {
# (1) We will exclude participants whose average reaction time across trials
# is more than three standard deviations (in either direction) from the average reaction time.
# (2) We will exclude participants who endorse as valid more than 50% of the invalid filler conclusions.
# (3) We will exclude individual trials where participants do not provide a response
# (4) We will exclude participants if we exclude 10% or more of their total trials as a result of non-responses.
  drop.list <- merge_bad_boys(list(bad.boys.false.accept.50, bad.boys.outlier.RTs.SD3, bad.boys.nonresponse.10))
}

if (CurrExp %in% c("Pilot-3", "N3-c-M2")) {
  # (1) We will exclude participants whose average reaction time across trials is more than three standard deviations (in either direction) from the average reaction time
  # (2) We will exclude individual trials where participants do not provide a response
  # (3) We will exclude participants if we exclude 10% or more of their total trials as a result of non-responses. 
  drop.list <- merge_bad_boys(list(bad.boys.outlier.RTs.SD3, bad.boys.nonresponse.10))
}

if (CurrExp %in% c("M2-bal", "N4-p-M2", "N2-p-M1", "S1-load-M2")) {
  # (1) We will exclude participants whose average reaction time across trials is more than two standard deviations (in either direction) from the average reaction time
  # (2) We will exclude individual trials where participants do not provide a response
  # (3) We will exclude participants if we exclude 10% or more of their total trials as a result of non-responses. 
  drop.list <- merge_bad_boys(list(bad.boys.outlier.RTs.SD2, bad.boys.nonresponse.10))
}

if (CurrExp == "N1-c-M1") {
  # We will use two exclusion criteria to filter participants.
  # (1) We will exclude participants whose average reaction time across trials is more than two standard deviations (in either direction) from the average reaction time.
  # (2) We will exclude participants who accept more than 70% of the false filler conclusions. 
  drop.list <- merge_bad_boys(list(bad.boys.false.accept.70, bad.boys.outlier.RTs.SD2, by.subject.nonresponse.rate.10))
}

if (CurrExp == "M1-cross") {
  # We will use four different exclusion criteria.
  # (1) We will exclude participants whose average reaction time across trials is more than two standard deviations (in either direction) from the average reaction time.
  # (2) We will exclude participants who accept more than 50% of the false filler arguments.
  # (3) We will exclude individual trials where participants do not provide a response.
  # (4) We will exclude participants if we exclude 10% or more of their total trials as a result of non-responses. 
  
  # Deviating to 70% rather than 50%
  drop.list <- merge_bad_boys(list(bad.boys.false.accept.70, bad.boys.outlier.RTs.SD2, bad.boys.nonresponse.10))
}


if (CurrExp %in% c("N6-p-M3", "N5-c-M3") ) {
  # We will exclude individual trials for which a participant does not provide a response.

  # We will use three exclusion criteria to filter participants.
  # (1) We will exclude participants whose average reaction time across trials is more than two standard deviations (in either direction) from the average reaction time
  # (2) We will exclude participants who accept more than 30% of the false filler conclusions (# Deviating from 30% to 50%)
  #  (3) We will exclude participants if we need to drop 10% or more of their total trials due to non-response.

  if (CurrExp == "N5-c-M3") {
    drop.list <- merge_bad_boys(list(by.subject.false.accept.50, bad.boys.outlier.RTs.SD2, bad.boys.nonresponse.10))
  } else {
    drop.list <- merge_bad_boys(list(bad.boys.outlier.RTs.SD2, bad.boys.nonresponse.10))
  }
  
}

if (CurrExp %in% c("M3-prime") ) {
  # We will use four different exclusion criteria.
  
  # (1) We will exclude participants whose average reaction time across trials is more than two standard deviations (in either direction) from the average reaction time.
  # (2) We will exclude participants who accept more than 50% of the false filler arguments. (# deviating from 50% to 70%)
  # (3) We will exclude individual trials where participants do not provide a response.
  # (4) We will exclude participants if we exclude 10% or more of their total trials as a result of non-responses. 
  
  drop.list <- merge_bad_boys(list(bad.boys.false.accept.70, bad.boys.outlier.RTs.SD2, bad.boys.nonresponse.10))
}


df.results.marked.exclusions <- apply_exclusion_labels(df.results, drop.list)


if (RUN_LIVE) {
  # How many trials / subjects do we end up dropping?
  df.results.marked.exclusions %>% group_by(excl.trial) %>%
    summarise(n = n())
}

write.csv(df.results.marked.exclusions, outputfile,
          row.names = F, quote=F)

