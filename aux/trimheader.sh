#!/bin/bash

##  Spencer Caplan 
##  CUNY Graduate Center 


append_commas_to_malformed_csv () {
	ORIG_FILE="${1}"
	FIXED_FILE="${2}"

	awk '
	BEGIN{FS=OFS=","} # This sets the field seperators to commas

	NR==FNR {				# first run
	if(maxnf<NF)			# find the biggest NF
		maxnf=NF
	next
	}

	NF<maxnf {
		for(i=NF+1;i<=maxnf;i++)
			$i=""
		}1
	' "${ORIG_FILE}" "${ORIG_FILE}" > "${FIXED_FILE}"
}



remove_header_comments () {
	RAW_SOURCE_DIR="${1}"
	NO_HEADER_DIR="${2}"
	REFORMATED_DEST_DIR="${3}"


	for filetoparse in "${RAW_DIR}/"* ;
	do
		if [ -f "${filetoparse}" ]; then 
			# echo "Trim-header: ${filetoparse}"

			# See more about bash string manipulations here:
			# https://linuxgazette.net/18/bash.html
			CURR_FILENAME_NO_PATH="${filetoparse##*/}"
			CURR_FILENAME_NO_PATH_NO_EXT="${CURR_FILENAME_NO_PATH%.*}"
			RAW_NO_HEADER_FILENAME="${NO_HEADER_DIR}${CURR_FILENAME_NO_PATH_NO_EXT}-noheader.csv"
			RAW_NO_HEADER_FIXEDCSV_FILENAME="${NO_HEADER_DIR}/${CURR_FILENAME_NO_PATH_NO_EXT}-noheader-addeddelim.csv"

			echo "${CURR_FILENAME_NO_PATH}"
			# clearing away old "noheader" files if they already exist
			touch "${RAW_NO_HEADER_FILENAME}" && rm "${RAW_NO_HEADER_FILENAME}"
			touch "${RAW_NO_HEADER_FIXEDCSV_FILENAME}" && rm "${RAW_NO_HEADER_FIXEDCSV_FILENAME}"

			# need the || rhs to handle the bizarre no-newline issue 
			# https://stackoverflow.com/questions/12916352/shell-script-read-missing-last-line
			cat "${filetoparse}" | while read line || [ -n "$line" ]; 
			do
				# Here we'll remove lines that start with "#"
				firstchar=${line:0:1}
				if [[ ${firstchar} == "#" ]] ; 
				then
					# skipping over headers and printing the skipped lines for transparency
					echo "${line}" > /dev/null # remove the /dev/null here to see which lines are getting cut
				else
					# keeping content lines
					echo "${line}" >> "${RAW_NO_HEADER_FILENAME}"
				fi
			done

			# Append extra commas if needed since the default output from ibex is malformed
			# (i.e. contains potentially different number of commas per row)
			append_commas_to_malformed_csv "${RAW_NO_HEADER_FILENAME}" "${RAW_NO_HEADER_FIXEDCSV_FILENAME}"

			if [ -f "${RAW_NO_HEADER_FILENAME}" ]; then 
				rm "${RAW_NO_HEADER_FILENAME}"
			fi
			
		fi
	done
	
}


combine_conditions_into_single_file() {

	NO_HEADER_DIR="${1}"
	COMBINED_FILE="${2}"
	CURR_EXP_NUM="${3}" # just for printing

	COMBINED_FILENAME_NO_PATH="${COMBINED_FILE##*/}"
	echo "Merging ${CURR_EXP_NUM} data into single file: ${COMBINED_FILENAME_NO_PATH}"

	FIRST_FILE="${NO_HEADER_DIR}/"`ls "${NO_HEADER_DIR}" | head -n 1`
	HEADER_STRING=`cat "${FIRST_FILE}" | head -n 1`
	touch "${COMBINED_FILE}" && rm "${COMBINED_FILE}"
	echo "${HEADER_STRING}" > "${COMBINED_FILE}"

	for noheaderfile in "${NO_HEADER_DIR}/"* ;
	do
		# echo $filename
		tail -n +2 "${noheaderfile}" >> "${COMBINED_FILE}"
	done

}


