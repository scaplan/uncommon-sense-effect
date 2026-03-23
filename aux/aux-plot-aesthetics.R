
#############################
#############################
#############################

# frequire <- require
# require <- function(...) suppressPackageStartupMessages(frequire(...))


## Helper function for loading / installing required packages
ensure_package <- function(pkg) {
  if (require(pkg, character.only = TRUE)) {
    # print(paste(pkg, "is loaded correctly"))
  } else {
    
    # Try to install the package
    message(paste("Trying to install", pkg))
    install.packages(pkg, dependencies = TRUE)
    
    # Try loading the package again
    if (require(pkg, character.only = TRUE)) {
      message(paste(pkg, "installed and loaded"))
    } else {
      stop(paste("Could not install", pkg))
    }
    
  }
}



load_in_plotting_libraries <- function(x) {
  
  # Could in future split these up into plotting vs. non-plotting libraries
  # Or more modular division
  
  # gplots
  # data.table
  # multimode
  # 
  
  packages <- c("dplyr",
                "tidyr",
                "stringr",
                "ggplot2",
                "Hmisc",
                "scales",
                "multimode",
                "Matrix",
                "tibble",
                "lme4",
                "reshape2",
                "tidyverse",
                "cowplot",
                "ggpubr",
                "ggrepel",
                "xtable",
                "broom",
                "ggbeeswarm",
                "data.table",
                "gplots",
                "diptest",
                "zoo",
                "jtools",
                "htmlTable",
                "ggdist")
  
  # Loop over each package and call ensure_package
  for (pkg in packages) {
    ensure_package(pkg)
  }
  
  return("load_in_libraries correctly")
  # print("load_in_libraries correctly")
}


#############################
### Plot Theme Aesthetics ###
#############################

load_in_plot_aesthetics <- function(x) {
  
  
  # Avoid scientific notation
  options(scipen = 999)
  
  
  # If errorbars overlap use position_dodge to shift them horizontally
  pd <<- position_dodge(0.2)
  
  
  axisTextSize <<- 24
  axisTextSizeSmall <<- 16
  lineSize <<- 2
  dotSize <<- 4
  density_plot_ymax <<- 0.3
  
  valid_filler_color <<- "#009E73"
  polysemy_color <<- "#56B4E9"
  homonomy_color <<- "#a9a9a9"
  invalid_filler_color <<- "#D55E00"
  
  exp1_color <<- "grey31"
  exp2_color <<- "orange3"
  exp3_color <<- "#d2352c"
  
  
  unloaded_color <<- "#CC79A7"
  loaded_color <<- "#d2352c"
  
  drop_color <<- "#A03B2A"
  grow_color <<- "#2F6B1C"
    
  
  
  four_set_fill <<- function() {
    scale_fill_manual(values = c("Valid Filler" = valid_filler_color,
                                 "Polysemy" = polysemy_color,
                                 "Homonymy" = homonomy_color,
                                 "Invalid Filler" = invalid_filler_color))
  }
  
  two_set_fill <<- function() {
    scale_fill_manual(values = c("Polysemy" = polysemy_color,
                                 "Homonymy" = homonomy_color))
  }
  
  four_set_color <<- function() {
    scale_color_manual(values = c("Valid Filler" = valid_filler_color,
                                 "Polysemy" = polysemy_color,
                                 "Homonymy" = homonomy_color,
                                 "Invalid Filler" = invalid_filler_color))
  }
  
  two_set_color <<- function() {
    scale_color_manual(values = c("Polysemy" = polysemy_color,
                                 "Homonymy" = homonomy_color))
  }
  
  response_geom_col_design <<- function(pl) {
    pl <- pl + geom_col() + 
      geom_hline(yintercept=0.5, linetype='dotted', col = 'black' ) +
      ylim(0,1.0)
    return(pl)
  }
  
  
  single_pane_theme <<- function() { 
    theme_minimal() %+replace% 
      theme(legend.position="none",
            legend.title = element_text(size=axisTextSize,face="bold"),
            legend.text=element_text(size=axisTextSize,face="bold"),
            plot.title = element_text(hjust = 0.5,size=axisTextSize,face="bold"),
            axis.text=element_text(size=axisTextSize,face="bold"),
            axis.text.x=element_text(size=axisTextSize,face="bold"),
            axis.text.y=element_text(size=axisTextSize,face="bold"),
            axis.title.x = element_text(size=axisTextSize,face="bold"),
            axis.title.y = element_text(size=axisTextSize,face="bold", angle = 90),
            plot.background = element_rect(fill = "white"),
            axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
      )
  }
  
  single_pane_theme_withLegend <<- function(legend_position) { 
    base <- theme_minimal() %+replace% 
        theme(legend.title = element_text(size=axisTextSize,face="bold"),
              legend.text=element_text(size=axisTextSize,face="bold"),
              plot.title = element_text(hjust = 0.5,size=axisTextSize,face="bold"),
              axis.text=element_text(size=axisTextSize,face="bold"),
              axis.text.x=element_text(size=axisTextSize,face="bold"),
              axis.text.y=element_text(size=axisTextSize,face="bold"),
              axis.title.x = element_text(size=axisTextSize,face="bold"),
              axis.title.y = element_text(size=axisTextSize,face="bold", angle = 90),
              plot.background = element_rect(fill = "white"),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
        )
    
    if (is.numeric(legend_position)) {
      base %+replace%
        theme(
          legend.position = "inside",
          legend.position.inside = legend_position
        )
    } else {
      base %+replace%
        theme(
          legend.position = legend_position
        )
    }

  }
  
  
  ### Move coloring (two-color, four-color), fill / color into function here
  ### so they can be called in one line in the main script and also changed globally
  
  FormSense_theme <<- function () { 
    theme_minimal() %+replace% 
      theme(# legend.position = c(.15, .15),
        legend.title = element_text(size=axisTextSize,face="bold"),
        legend.text=element_text(size=axisTextSize,face="plain"),
        plot.title = element_text(hjust = 0.5,size=axisTextSize,face="bold"),
        axis.text=element_text(size=axisTextSize,face="bold"),
        axis.text.y=element_text(size=axisTextSize,face="bold"),
        axis.text.x = element_text(size=axisTextSize,angle = 40,colour="grey20",hjust=1,face="plain"),
        # axis.title.x = element_text(size=axisTextSize,angle=0,hjust=.5,vjust=1.0,face="bold"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=axisTextSize,face="bold", angle = 90, hjust=.5,vjust=.5),
        axis.line = element_line(colour = "black"), panel.border = element_blank()
      )
  }

  
  rename_word_nums <<- function(df, namelist) {
    for (index in 0:4) {
      df$word_num[df$word_num == index] <- namelist[index+1]
    }
    return(df)
  }
  
  
}
