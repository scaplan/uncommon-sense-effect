
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Pre-processing ibex output files into standard single-trial-per-row format
###########################################################
###########################################################

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Pre-processing ibex output files into standard single-trial-per-row format...")


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


if (length(args)>3) {
  basepath <- args[1]
  inputfile <- args[2]
  outputfile <- args[3]
  currexp <- args[4]
} else {
  print("Not enough input arguments")
}


if (RUN_LIVE) {
  sourceDir <- paste(basepath, "data/raw/5-FullStudy-main/proc/", sep = "")
  sourceDir <- paste(basepath, "data/raw/5-FullStudy-loaded/proc/", sep = "")
  inputfile <- "ibex-merged-output.csv"
  outputfile <- "LiveRun-singleRowPerTrial.csv"
  currexp <- "S1-load-M2"
  setwd(sourceDir)
}


set_ng_colnames <- function(ng_data, currexp) {
  
  base_cols <- c("RawTime", "IPHash", "Ibex", "ItemNum",
                 "InnerElemNum", "Label", "LatinSq", "ElemType",
                 "ElemNum", "Parameter", "Value", "TimeStamp")
  
  extra_cols <- NULL
  
  if (currexp %in% c('N6-p-M3',  'N5-c-M3', 'M3-prime') ) {
    extra_cols <- c("MiddleTerm1", "MiddleTerm2", "relationtype", "TrialType",
                    "Syllogism", "ListNum", "OrderNum", "premise1",
                    "premise2", "conclusion", "Comments")
    
  } else if (currexp %in% c("N2-p-M1", "N1-c-M1", "M1-cross")) {
    extra_cols <- c("TargetWord", "TrialType", "Syllogism", "MiddleSyllogismPairing",
                    "ListNum", "premise1", "premise2", "conclusion", "Comments"
    )
  } else {
    if (ncol(ng_data) == 16) {
      extra_cols <- c("premise", "premisenum", "ListNum", "Comments")
    } else if (ncol(ng_data) == 17) {
      extra_cols <- c("TargetWord", "TrialType", "Syllogism", "ListNum",
                      "Comments")
    } else if (ncol(ng_data) == 20) {
      extra_cols <- c("TargetWord", "TrialType", "Syllogism", "ListNum",
                      "premise1", "premise2", "conclusion", "Comments")
    } else if (ncol(ng_data) == 21) {
      extra_cols <- c("TargetWord", "TrialType", "Syllogism", "ListNum",
                      "premise1", "premise2", "conclusion", "loadtime",
                      "Comments")
    } else if (ncol(ng_data) == 27) {
      extra_cols <- c("TargetWord", "TrialType", "Syllogism", "ListNum",
                      "premise1", "premise2", "conclusion", "loadtime",
                      "NominalType", "AC1" ,"AC2", "dominancePercent",
                      "abstractType", "categoryMistake", "Comments")
    } else if (ncol(ng_data) == 31) {
      extra_cols <- c("TargetWord", "TrialType", "Syllogism", "ListNum",
                      "premise1", "premise2", "conclusion", "loadtime",
                      "NominalType", "AC1" ,"AC2", "dominancePercent",
                      "abstractType", "categoryMistake", "ConcTruth", "ConcRT",
                      "RandDig", "FullStudyLoadType", "Comments")
    } else {
      stop("This data has a different number of columns than I'm expecting / able to handle!",
           "Unexpected number of columns: ", ncol(ng_data),
           "\ncurrexp = ", currexp)
    }
  }
  
  # Confirm num columns matches
  stopifnot(
    ncol(ng_data) == length(base_cols) + length(extra_cols)
    )
  
  names(ng_data) <- c(base_cols, extra_cols)
  return(ng_data)
  
}


ng_data <- read.csv(inputfile, stringsAsFactors = T,header=FALSE, sep = ",") # Read in the target words for each instance
ng_data <- set_ng_colnames(ng_data, currexp)


# Remove irrelevant columns
ng_data <- ng_data %>% 
  select(-Comments) %>% 
  select(-LatinSq) %>%
  select(-RawTime) %>% 
  select(-Ibex) %>%
  select(-InnerElemNum)


# Find first row with experimental data and use that to calculate the instruction offset
## e.g. Renumber the items to start at 1 instead of 5
if (currexp %in% c("M2-bal", "S1-load-M2")) {
  offsets <- ng_data %>%
    filter(grepl("experimental-trial", Label)) %>%  # Filter rows where Label contains "experimental-trial"
    group_by(FullStudyLoadType) %>%                # Group by FullStudyLoadType
    slice_min(order_by = row_number(), n = 1) %>%  # Select the first row in each group
    ungroup() %>%                                  # Remove grouping
    mutate(instruction_offset = ItemNum - 1) %>%
    select(FullStudyLoadType, instruction_offset)             # Select only the columns of interest
  
  ng_data <- ng_data %>%
    left_join(offsets, by = "FullStudyLoadType") %>%  # Join to add instruction_offset
    mutate(ItemNum = ItemNum - instruction_offset) %>%  # Subtract instruction_offset
    select(-instruction_offset)                     # Remove instruction_offset column if no longer needed
  
} else {
  first_real_trial_row <- ng_data[which(ng_data$Label=='experimental-trial', arr.ind=TRUE)[1],]
  instruction_offset <- first_real_trial_row$ItemNum - 1
  ng_data$ItemNum <- ng_data$ItemNum - instruction_offset
}


# Remove digit-enter empty rows (for M2-bal)
ng_data <- ng_data %>% filter(Parameter != "First") %>% filter(Parameter != "Print")
# Just keep the experimental trials....
ng_data <- ng_data %>% filter(Label %in% c("experimental-trial") )


# Remove instruction rows
ng_data <- ng_data %>% filter(ItemNum > 0)

### Good here
# table(ng_data$FullStudyLoadType)

## Confirm no repeat takers...
rows_per <- ng_data %>% group_by(IPHash) %>% summarise(n = n())
median_rows <- median(rows_per$n)
repeat_takers <- rows_per %>% filter(n > median_rows) %>% pull(IPHash)

if (RUN_LIVE) {
  ## Confirm no repeat takers...
  rows_per
}

# row counting doesn't work for the digit match load study (since digit trials are extra)
# So skipping that filter just in that case
if (currexp != "S1-load-M2") {
  ng_data <- ng_data %>%
    filter(!(IPHash %in% repeat_takers))
  
}


# Convert IPhash into sequential subject ID
# Then make new column with Subject+Trial value
# (Which we can use to widen based on)
# Finally, post SubjectID and SubjTrial first (on the left)
grouped_data <- ng_data %>%
                group_by(IPHash) %>%
                mutate(SubjectID = cur_group_id()) %>%
                mutate(SubjTrial = paste(SubjectID, ItemNum, sep="_")) %>%
                mutate(ExpNum = currexp) %>%
                relocate(SubjTrial) %>%
                relocate(SubjectID, .after = SubjTrial) %>%
                relocate(ExpNum, .after = SubjectID) %>%
                ungroup()


# Drop IPhash since we no longer need it
grouped_data <- grouped_data %>% select(-IPHash)
# Group by SubjTrial before we widen
grouped_data <- grouped_data %>% group_by(SubjTrial)

## Need to get rid of the non-numeric "never" values in the
## timestamp field from non-responses
grouped_data$TimeStamp <- as.character(grouped_data$TimeStamp) 
grouped_data <- grouped_data %>%  
  mutate(TimeStamp = case_when(TimeStamp == 'Never' ~ "0",
                       TRUE ~ TimeStamp))
grouped_data$TimeStamp <- as.numeric(grouped_data$TimeStamp)


########
########
# Separately pull out digit-response 
# total-RT
# Merge in as new columns (or NA where appropriate)

# ElemNum == dig-input
# Value == their answer (e.g. 98305)


## Aha, this shouldn't me M2 anymore.... it's not 
if (currexp == "S1-load-M2") {
  digit_trials_only <- grouped_data %>% ungroup() %>% filter(ElemNum == "dig-input") %>%
    select(SubjTrial, ExpNum, Value, RandDig, FullStudyLoadType) %>%
    mutate(digitMatch = ifelse(as.character(Value) == as.character(RandDig), 1, 0)) %>%
    select(SubjTrial, digitMatch)

  ## Join back in and then filter(ElemNum != "dig-input") 
  grouped_data <- grouped_data %>%
    full_join(digit_trials_only, by = "SubjTrial", relationship = "many-to-many") %>%
    relocate(digitMatch, .after = TimeStamp)
  
  grouped_data <- grouped_data %>% filter(ElemNum != "dig-input")
} else {
  grouped_data <- grouped_data %>% mutate(digitMatch = "NA")
}


#### Any duplicate rows?
grouped_data_unique <- grouped_data %>%
  distinct()

if (RUN_LIVE) {
  print(nrow(grouped_data))
  print(nrow(grouped_data_unique))
}

grouped_data_unique_start_end_only <- grouped_data_unique %>% filter(Value %in% c("Start", "End"))

# df_only_RTs <- pivot_wider(data = grouped_data_unique,
df_only_RTs <- pivot_wider(data = grouped_data_unique_start_end_only,
                           id_cols = c("SubjTrial"),
                           names_from = Value,
                           values_from = c("TimeStamp")) %>%
  mutate(
    Start = as.numeric(Start),
    End = as.numeric(End),
    RT = End - Start
  )

df_only_RTs <- subset(df_only_RTs, select = c(SubjTrial, RT))

grouped_data_plus_RTs <- merge(x=grouped_data_unique,y=df_only_RTs,by="SubjTrial")

## just for the 7-p and 8-p experiment data need to widen from ElemNum to Response-P1 and Response-P2
if (currexp %in% c('N2-p-M1', 'N6-p-M3')) {
  grouped_data_plus_RTs_cleaned <- grouped_data_plus_RTs %>%
    select(-TimeStamp) %>%
    # filter(ElemNum == "answer") %>%
    filter(ElemNum %in% c("answer", "answer-P1", "answer-P2")) %>%
    select(-ElemType, -Parameter, -Label) %>%
    rename_at('ElemNum', ~'PremiseNum')
} else {
  grouped_data_plus_RTs_cleaned <- grouped_data_plus_RTs %>%
    select(-TimeStamp) %>%
    filter(ElemNum == "answer") %>%
    select(-ElemType, -Parameter, -Label, -ElemNum)
}


# Sort SubjTrial in natural order
mixedrank = function(x) order(gtools::mixedorder(x))
grouped_data_plus_RTs_cleaned <- grouped_data_plus_RTs_cleaned %>% arrange(mixedrank(SubjTrial))

grouped_data_plus_RTs_cleaned <- grouped_data_plus_RTs_cleaned %>%  
            mutate(Value = case_when(grepl("False", Value) ~ 0,
                                     grepl("True",  Value) ~ 1,
                                     grepl("No", Value) ~ 0,
                                     grepl("Yes", Value) ~ 1,)) %>%
            rename_at('Value', ~'Response')

if (currexp != 'N4-p-M2') {
  grouped_data_plus_RTs_cleaned$SyllogismSimple <- gsub("\\*", '', grouped_data_plus_RTs_cleaned$Syllogism)
}



# Mark the response for no-selection trials to be "-99" so there are no NAs in the data
grouped_data_plus_RTs_cleaned <- grouped_data_plus_RTs_cleaned %>%
  mutate(NonResponse = case_when(is.na(Response) ~ 1,
                                 TRUE ~ 0)) %>%
  mutate(Response = case_when(NonResponse == 1 ~ -99,
                              NonResponse == 0 ~ Response)
  )

if (RUN_LIVE) {
  # Confirming that the item numbering is set up correctly
  grouped_data_plus_RTs_cleaned %>% distinct(ItemNum)
}
if (RUN_LIVE) {
  # Confirming that Response/NonResponse is fully handled
  grouped_data_plus_RTs_cleaned %>% distinct(Response)
  grouped_data_plus_RTs_cleaned %>% distinct(NonResponse)
}


## There's an operation in label-exlcusions.R which relies on the column
## "TrialType" existing so add here if missing
if (!has_name(grouped_data_plus_RTs_cleaned, "TrialType")) {
  grouped_data_plus_RTs_cleaned <- grouped_data_plus_RTs_cleaned %>%
    mutate(TrialType = "NA") %>%
    relocate(TrialType, .after = Response)
}
        

## Add column indicating ExpectedResponse
# That's 1 for the Valid Fillers and 0 for everything else
if (currexp == 'N4-p-M2') {
  df <- grouped_data_plus_RTs_cleaned %>%  
    mutate(ExpectedResponse = 1) %>%
    mutate(ResponseWasCorrect = case_when(Response == ExpectedResponse ~ 1,
                                          Response != ExpectedResponse ~ 0))
} else {
  df <- grouped_data_plus_RTs_cleaned %>%  
    mutate(ExpectedResponse = case_when(TrialType == 'Homonymy' ~ 0,
                                        TrialType == 'Polysemy' ~ 0,
                                        TrialType == 'Invalid Filler' ~ 0,
                                        TrialType == 'Related' ~ 0,
                                        TrialType == 'Unrelated' ~ 0,
                                        TrialType == 'Valid Filler' ~ 1)) %>%
    mutate(ResponseWasCorrect = case_when(Response == ExpectedResponse ~ 1,
                                          Response != ExpectedResponse ~ 0))
}


write.csv(df, outputfile, row.names = F, quote=F)

