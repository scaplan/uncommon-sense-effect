#!/bin/bash

##  Spencer Caplan 
##  CUNY Graduate Center 


############
## This is a control script to convert the raw experimental data into a 
## standard format in which there is one row per trial
##
## Individual behavior can be (en/dis)abled via these top flags
############
############


############
## Flags
REMOVE_CSV_HEADER_COMMENTS=false
MERGE_TRIAL_LINES=false
CONSOLIDATE_PILOT_DATA=false
MERGE_ALL_EXPS=true
############
############


#########################
#########################
## Load source and set exp list
#########################
#########################
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOME_DIR="$SCRIPT_DIR/.."
AUX_BASE_PATH="$HOME_DIR/aux"
DATA_BASE_PATH="$HOME_DIR/data"

cd $AUX_BASE_PATH
source "./trimheader.sh"
source "./condmkdir.sh"

cd $DATA_BASE_PATH
echo `pwd`


PILOT_LIST=("1-TruthNorming" "2-ArgumentNorming" "3-LoadNorming")
MAIN_LIST=("4-RevampedConclusions" "502-FullStudy-main" "501-FullStudy-loaded" "6-JustPremises" \
		   "7-FullyCrossedJSC" "8-FullyCrossedJSP" "9-FullyCrossedAll" "10-PremisePrimes" \
		   "11-ConclusionPrimes" "12-FullPrimes")
EXP_LIST=( "${PILOT_LIST[@]}" "${MAIN_LIST[@]}" )

MAIN_EXP_AS_STRING=$(printf '%s,' "${MAIN_LIST[@]// /|}")
MAIN_EXP_AS_STRING=$(echo ${MAIN_EXP_AS_STRING%?}) # remove the trailing comma that gets added above

MEGA_COMBINED_DATA_PATH="${DATA_BASE_PATH}/merged/combined-all-exp-trials"
# BY_STIM_DATA_PATH="${DATA_BASE_PATH}/by-stimulus-results.csv"



### Mapping from orig file naming to paper conventions
#   "M" for "Main" studies
#   "N" for "Norming" studies
#   "S" for SI load version
###
declare -a EXP_NUMBERING=(
	# Main text
	["9-FullyCrossedAll"]="M1-cross"
	["502-FullStudy-main"]="M2-bal"
	["12-FullPrimes"]="M3-prime"

	# Norming and SI studies
	["7-FullyCrossedJSC"]="N1-c-M1"
	["8-FullyCrossedJSP"]="N2-p-M1"
	["4-RevampedConclusions"]="N3-c-M2"
	["6-JustPremises"]="N4-p-M2"
	["11-ConclusionPrimes"]="N5-c-M3"
	["10-PremisePrimes"]="N6-p-M3"
	["501-FullStudy-loaded"]="S1-load-M2"

	# Pilot studies
	["1-TruthNorming"]="Pilot-1"
	["2-ArgumentNorming"]="Pilot-2"
	["3-LoadNorming"]="Pilot-3"
)
echo ${EXP_NUMBERING[*]}




#########################
#########################
## Iterate over and reformat individual exps
#########################
#########################
for WHICH_EXP in "${EXP_LIST[@]}"
do
	printf "Pre-processing ${WHICH_EXP}..."
	CURR_EXP_BASE_PATH="${DATA_BASE_PATH}/raw/${WHICH_EXP}"
	CURR_EXP_NUM="${EXP_NUMBERING[${WHICH_EXP}]}"
	echo "${CURR_EXP_NUM}"

	RAW_DIR=$(mkdir_if_needed "${CURR_EXP_BASE_PATH}/orig/")
	REFORMATED_DIR=$(mkdir_if_needed "${CURR_EXP_BASE_PATH}/proc/")

	CURR_EXP_COMBINED_FILE_RAW="${REFORMATED_DIR}ibex-merged-output.csv"
	
	if [ "$REMOVE_CSV_HEADER_COMMENTS" = true ] ; then
		NO_HEADER_DIR=$(mkdir_if_needed "${RAW_DIR}noheaders/")

		echo "Removing ibex comment lines..."
		remove_header_comments "$RAW_DIR" "$NO_HEADER_DIR" "$REFORMATED_DIR"   # "Remove raw CSV header comment lines..."
		echo ""
		# Combine the different condition data from $WHICH_EXP into a single file, then remove no_header directory for clean up afterwards
		combine_conditions_into_single_file "$NO_HEADER_DIR" "$CURR_EXP_COMBINED_FILE_RAW" "$CURR_EXP_NUM"
		rm -r "${NO_HEADER_DIR}" # cleaning up here
		echo ""
	fi

	CURR_EXP_COMBINED_FILE_TRIALMERGE="${REFORMATED_DIR}${WHICH_EXP}-singleRowPerTrial.csv"
	CURR_EXP_COMBINED_FILE_WITHEXCLUSIONS="${REFORMATED_DIR}${WHICH_EXP}-singleRowPerTrial-labelexclude.csv"

	if [ "$MERGE_TRIAL_LINES" = true ] ; then
		echo "################################################"
		echo "Converting data into one-row-per-trial format..."
		echo "################################################"

		# Convert into one-row-per-trial
		Rscript "${HOME_DIR}/preproc/"ibex-merge.R \
			"${HOME_DIR}/" \
			"${CURR_EXP_COMBINED_FILE_RAW}" \
			"${CURR_EXP_COMBINED_FILE_TRIALMERGE}" \
			"${CURR_EXP_NUM}" 
		
		Rscript "${HOME_DIR}/preproc/"label-exclusions.R \
			"${HOME_DIR}/" \
			"${CURR_EXP_COMBINED_FILE_TRIALMERGE}" \
			"${CURR_EXP_COMBINED_FILE_WITHEXCLUSIONS}"

		# Clean up temp files
		rm "${CURR_EXP_COMBINED_FILE_TRIALMERGE}"
		rm "${CURR_EXP_COMBINED_FILE_RAW}"
		
	fi

done


#########################
#########################
## Extract / save pilot data
#########################
#########################
if [ "$CONSOLIDATE_PILOT_DATA" = true ] ; then
	for WHICH_EXP in "${PILOT_LIST[@]}"
	do
		printf "Saving pilot study data ${WHICH_EXP}..."
		CURR_EXP_BASE_PATH="${DATA_BASE_PATH}/raw/${WHICH_EXP}"
		REFORMATED_DIR=$(mkdir_if_needed "${CURR_EXP_BASE_PATH}/proc/")
		CURR_EXP_NUM="${EXP_NUMBERING[${WHICH_EXP}]}"
		echo "${CURR_EXP_NUM}"

		CURR_EXP_COMBINED_FILE_WITHEXCLUSIONS="${REFORMATED_DIR}${WHICH_EXP}-singleRowPerTrial-labelexclude.csv"

		DESTINTATION_FILE_PILOT="${DATA_BASE_PATH}/merged/pilot-studies/${CURR_EXP_NUM}-singleRowPerTrial.csv"
		echo ${DESTINTATION_FILE_PILOT}
		cp "${CURR_EXP_COMBINED_FILE_WITHEXCLUSIONS}" "${DESTINTATION_FILE_PILOT}"
	done
fi


#########################
#########################
## Combine all exp data into single mega file
#########################
#########################
if [ "$MERGE_ALL_EXPS" = true ] ; then
	echo "Merging ${MAIN_EXP_AS_STRING} into a single data file..."
	Rscript "${HOME_DIR}/preproc/"conjoin-exps.R "${HOME_DIR}/" "${MAIN_EXP_AS_STRING}" "${MEGA_COMBINED_DATA_PATH}-nostim.rds"
	Rscript "${HOME_DIR}/preproc/"append-stimulus-level-properties.R "${MEGA_COMBINED_DATA_PATH}-nostim.rds" "${MEGA_COMBINED_DATA_PATH}" && rm "${MEGA_COMBINED_DATA_PATH}-nostim.rds"

fi


echo "Done pre-processing."
