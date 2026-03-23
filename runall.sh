#!/bin/bash

##  Spencer Caplan 
##  CUNY Graduate Center 


############
## Flags
PRE_PROCESSING=true
MAIN_ANALYSIS=true
CP_PAPER_FIGS=true
############
############

PRE_PROCESS_SOURCE="./aux/extracttrialsfromraw.sh"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DATA_FILE="$SCRIPT_DIR/data/merged/combined-all-exp-trials.rds"
DATA_FILE_TEXT="$SCRIPT_DIR/data/merged/combined-all-exp-trials.csv"
MODELS_FILE="$SCRIPT_DIR/output/stats/fitted-models.rds"
NUMS_DIR="${SCRIPT_DIR}/output/stats/"
PLOT_DIR="${SCRIPT_DIR}/output/plots/"
PAPER_FIG_DIR="${SCRIPT_DIR}/paperfigs/"

if [ "$PRE_PROCESSING" = true ] ; then
	${PRE_PROCESS_SOURCE}
fi


if [ "$MAIN_ANALYSIS" = true ] ; then
	Rscript "${SCRIPT_DIR}/analysis/01-fit-primary-regressions.R" "${DATA_FILE}" "${NUMS_DIR}"
	Rscript "${SCRIPT_DIR}/analysis/02-model-comparison.R" "${MODELS_FILE}" "${NUMS_DIR}"
	Rscript "${SCRIPT_DIR}/analysis/03a-output-descriptive-stats.R" "${DATA_FILE}" "${NUMS_DIR}"
	Rscript "${SCRIPT_DIR}/analysis/03b-output-argument-tables.R" "${DATA_FILE}" "${NUMS_DIR}"
	Rscript "${SCRIPT_DIR}/analysis/04-make-odds-ratio-plot.R" "${MODELS_FILE}" "${PLOT_DIR}"
	Rscript "${SCRIPT_DIR}/analysis/05-subject-level-analyses.R" "${DATA_FILE}" "${PLOT_DIR}"
	Rscript "${SCRIPT_DIR}/analysis/06-item-level-analyses.R" "${DATA_FILE}" "${NUMS_DIR}" "${PLOT_DIR}"
	Rscript "${SCRIPT_DIR}/analysis/07-make-multilevel-subject-item-plot.R" \
					"${PLOT_DIR}Subject-level-Boost.png" \
					"${PLOT_DIR}Item-level-Boost.png" \
					"${PLOT_DIR}USE-across-subjects-and-items.png"
	Rscript "${SCRIPT_DIR}/analysis/08-individual-main-study-plots.R" "${DATA_FILE}" "${PLOT_DIR}"
	Rscript "${SCRIPT_DIR}/analysis/09-make-combined-main-study-plots.R" \
					"${PLOT_DIR}Exp-1-2-Forest-Plot.png" \
					"${PLOT_DIR}M1-binom-result.png" \
					"${PLOT_DIR}M2-binom-result.png" \
					"${PLOT_DIR}M1-M2-Main-Result.png"
	Rscript "${SCRIPT_DIR}/analysis/10-make-prime-plot.R" "${DATA_FILE}" "${PLOT_DIR}"
	Rscript "${SCRIPT_DIR}/analysis/11-ordering-null-effect.R" "${DATA_FILE}" "${PLOT_DIR}"
	Rscript "${SCRIPT_DIR}/analysis/12-filler-performance-split.R" "${DATA_FILE}" "${PLOT_DIR}"
	Rscript "${SCRIPT_DIR}/analysis/13-premise-null-effect.R" "${DATA_FILE}" "${PLOT_DIR}"

fi

if [ "$CP_PAPER_FIGS" = true ] ; then
	echo "CP'ing over final paper figs..."
	cp "${PLOT_DIR}M1-M2-Main-Result.png" "${PAPER_FIG_DIR}Fig3-M1-M2-Main-Result.png"
	cp "${PLOT_DIR}USE-across-subjects-and-items.png" "${PAPER_FIG_DIR}Fig4-USE-across-subjects-and-items.png"
	cp "${PLOT_DIR}M3-prime-no-boost.png" "${PAPER_FIG_DIR}Fig6-M3-prime-no-boost.png"

	cp "${PLOT_DIR}S1-binom-by-condition.png" "${PAPER_FIG_DIR}SI/S1-binom-by-condition.png"
	cp "${PLOT_DIR}Premise-Null-Effect.png" "${PAPER_FIG_DIR}SI/S2-Premise-Null-Effect.png"
	cp "${PLOT_DIR}Item-Number-Null-Effect.png" "${PAPER_FIG_DIR}SI/S3-Item-Number-Null-Effect.png"
	cp "${PLOT_DIR}Filler-performance-USE-plot.png" "${PAPER_FIG_DIR}SI/S4-Filler-performance-USE-plot.png"
	
fi


echo "Done all."
