
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Make three-level USE plot
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Make three-level USE plot (experiments, subjects, items)...")

## For handling project structure / relative paths ##
suppressMessages(require("rprojroot"))
sourceDir <- find_root(is_git_root)
source(file.path(sourceDir, "aux", "aux-functions.R"))
source(file.path(sourceDir, "aux", "aux-plot-aesthetics.R"))

args = commandArgs(trailingOnly=TRUE)
RUN_LIVE <- interactive()

if (RUN_LIVE) {
  basepath <- set_live_basepath() # Path in full path to git repo if running individual files live...
  inputpath_SUBJ <- paste0(basepath, "output/plots/Subject-level-Boost.png")
  inputpath_ITEMS <- paste0(basepath, "output/plots/Item-level-Boost.png")
  output_multi <- paste0(basepath, "output/plots/USE-across-subjects-and-items.png")
}

output_message <- load_in_libraries()
plot_library_status <- load_in_plotting_libraries()
load_in_plot_aesthetics()


if (length(args)>2) {
  inputpath_SUBJ <- args[1]
  inputpath_ITEMS <- args[2]
  output_multi <- args[3]
} else {
  print("Not enough input arguments")
}


##################################
## 0. Reading in base plots
axisTextSizeBig <- 36

p.SUBJ <- ggdraw() + draw_image(inputpath_SUBJ) + draw_label("(A)", x = 0.85, y = 0.22, hjust = 0, vjust = 1, size = axisTextSizeBig)
p.ITEMS <- ggdraw() + draw_image(inputpath_ITEMS) + draw_label("(B)", x = 0.85, y = 0.22, hjust = 0, vjust = 1, size = axisTextSizeBig)

combined_plot <- plot_grid(p.SUBJ, p.ITEMS, nrow = 1)
if (RUN_LIVE) { combined_plot }
ggsave(plot = combined_plot,
       filename=output_multi,
       width = 16, height = 8, units = "in") 
##################################
##################################


