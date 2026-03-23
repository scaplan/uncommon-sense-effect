#!/bin/bash

##  Spencer Caplan 
##  CUNY Graduate Center 


#########################
#########################
## Define helper function (check if dir exists, and mkdir if it doesn't)
#########################
#########################
mkdir_if_needed () {
	DIR_TO_CHECK="${1}"
	if [ ! -d "${DIR_TO_CHECK}" ]; then
		# echo "${DIR_TO_CHECK} does not exist, so creating now..."
		mkdir "${DIR_TO_CHECK}"
	fi
	echo "${DIR_TO_CHECK}"
}

