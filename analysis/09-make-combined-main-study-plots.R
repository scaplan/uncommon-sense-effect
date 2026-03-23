
###########################################################
###########################################################
##  Author: Spencer Caplan
##  CUNY Graduate Center
##
##  Make combined M1/M2 study plot
###########################################################
###########################################################

## if you run into issues running locally NOT from the runall.sh script
## you may try manually calling setwd() to change to "uncommon-sense-effect/analysis" from your 
## working environment

rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
invisible(gc()) # free up memory and report the memory usage.
cat("Make combined M1/M2 study plot...")

## For handling project structure / relative paths ##
suppressMessages(require("rprojroot"))
sourceDir <- find_root(is_git_root)
source(file.path(sourceDir, "aux", "aux-functions.R"))
source(file.path(sourceDir, "aux", "aux-plot-aesthetics.R"))

args = commandArgs(trailingOnly=TRUE)
RUN_LIVE <- interactive()

if (RUN_LIVE) {
  basepath <- set_live_basepath() # Path in full path to git repo if running individual files live...
  inputpath_OR <- paste0(basepath, "output/plots/Exp-1-2-Forest-Plot.png")
  inputpath_M1 <- paste0(basepath, "output/plots/M1-binom-result.png")
  inputpath_M2 <- paste0(basepath, "output/plots/M2-binom-result.png")
  output_multi <- paste0(basepath, "output/plots/M1-M2-Main-Result.png")
}

output_message <- load_in_libraries()
plot_library_status <- load_in_plotting_libraries()
load_in_plot_aesthetics()


if (length(args)>3) {
  inputpath_OR <- args[1]
  inputpath_M1 <- args[2]
  inputpath_M2 <- args[3]
  output_multi <- args[4]
} else {
  print("Not enough input arguments")
}


##################################
## 0. Reading in base plots
axisTextSizeBig <- 36


p.M1 <- ggdraw() + draw_image(inputpath_M1) + draw_label("(A)", x = 0.85, y = 0.93, hjust = 0, vjust = 1, size = axisTextSizeBig)
p.M2 <- ggdraw() + draw_image(inputpath_M2) + draw_label("(B)", x = 0.85, y = 0.93, hjust = 0, vjust = 1, size = axisTextSizeBig)
p.OR <- ggdraw() + draw_image(inputpath_OR) + draw_label("(C)", x = 0.85, y = 0.93, hjust = 0, vjust = 1, size = axisTextSizeBig)

combined_plot <- plot_grid(p.M1, p.M2, p.OR, nrow = 1)
if (RUN_LIVE) { combined_plot }
ggsave(plot = combined_plot,
       filename=output_multi,
       width = 24, height = 8, units = "in") 
##################################
##################################


