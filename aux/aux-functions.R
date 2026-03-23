
#############################
#############################
#############################

frequire <- require
require <- function(...) suppressPackageStartupMessages(frequire(...))


## Helper function for loading / installing required packages
ensure_package <- function(pkg) {
  if (suppressPackageStartupMessages(
    require(pkg, character.only = TRUE)
  )) {
    return(invisible(TRUE))
  }
  
  message("Trying to install ", pkg)
  install.packages(pkg, dependencies = TRUE)
  
  if (!suppressPackageStartupMessages(
    require(pkg, character.only = TRUE)
  )) {
    stop("Could not install ", pkg)
  }
  
  invisible(TRUE)
}






load_in_libraries <- function(x) {
  
  # Could in future split these up into plotting vs. non-plotting libraries
  # Or more modular division
  
  # gplots
  # data.table
  # multimode
  # diptest
  
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
                "rlang",
                "mclust",
                "kableExtra")
  
  # Loop over each package and call ensure_package
  for (pkg in packages) {
    ensure_package(pkg)
  }
  
  return("load_in_libraries correctly")
  # print("load_in_libraries correctly")
}




set_live_basepath <- function(basepath = NULL) {
  if (is.null(basepath)) {
    currMachine <- Sys.info()[['nodename']]
    if (currMachine %in% c('scaplanoffice.local', 'MiniNazz')) {
      basepath <- "/Users/scaplan/Dropbox/CS_accounts/penn_CS_account/polysemy/uncommon-sense-effect/"
    } else if  (currMachine %in% c('spcaplanMacbook', 'Spencers-MacBook-Air.local')) {
      basepath <- "/Users/spcaplan/Dropbox/CS_accounts/penn_CS_account/polysemy/uncommon-sense-effect/"
    } else {
      stop("Unknown machine '", currMachine, "'. ",
           "Please supply basepath explicitly:\n",
           "  set_live_basepath(\"/path/to/project/\")",
           call. = FALSE
           )
    }
  }
  
  if (!dir.exists(basepath)) {
    stop("Basepath does not exist: ", basepath, call. = FALSE)
  }
  
  setwd(basepath)
  return(basepath)
}



#############################
#############################
#############################

summary_group_by_sc <- function(orig.df, value, grouping1, grouping2) {
  
  if (missing(grouping2)) {
    summary.df <- orig.df %>%
      group_by(get(grouping1)) %>%
      summarise(mean_prop = mean(get(value)), na.rm = TRUE,
                sd_prop = sd(get(value)),
                n_prop = n(),
                se_prop = sd(get(value))/sqrt(n())
      ) %>% ungroup()
    colnames(summary.df)[1] <- grouping1
  } else {
    summary.df <- orig.df %>%
      group_by(get(grouping1), get(grouping2)) %>%
      summarise(mean_prop = mean(get(value)), na.rm = TRUE,
                sd_prop = sd(get(value)),
                n_prop = n(),
                se_prop = sd(get(value))/sqrt(n())
      ) %>% ungroup()
    colnames(summary.df)[1] <- grouping1
    colnames(summary.df)[2] <- grouping2
  }
  
  return(summary.df)
}


summary_group_by_sc_boost <- function(orig.df, value, offset, grouping1, grouping2) {
  
  if (missing(grouping2)) {
    summary.df <- orig.df %>%
      group_by(get(grouping1)) %>%
      summarise(mean_prop = mean(get(value) - mean(get(offset))), na.rm = TRUE,
                sd_prop = sd(get(value)),
                n_prop = n(),
                se_prop = sd(get(value))/sqrt(n())
      ) %>% ungroup()
    colnames(summary.df)[1] <- grouping1
  } else {
    summary.df <- orig.df %>%
      group_by(get(grouping1), get(grouping2)) %>%
      # summarise(mean_prop = mean(get(value)) - (mean(get(offset))/100), na.rm = TRUE,
      summarise(mean_prop = mean(get(value)) - mean(get(offset)), na.rm = TRUE,
                sd_prop = sd(get(value)),
                n_prop = n(),
                se_prop = sd(get(value))/sqrt(n())
      ) %>% ungroup()
    colnames(summary.df)[1] <- grouping1
    colnames(summary.df)[2] <- grouping2
  }
  
  return(summary.df)
}


summary_group_by_sc_boost_single_group <- function(orig.df, value, offset, grouping1) {
  
  summary.df <- orig.df %>%
      group_by(get(grouping1)) %>%
      summarise(mean_prop = mean(get(value)) - (mean(get(offset))/100), na.rm = TRUE,
                sd_prop = sd(get(value)),
                n_prop = n(),
                se_prop = sd(get(value))/sqrt(n())
      ) %>% ungroup()
    colnames(summary.df)[1] <- grouping1
  
  return(summary.df)
}

summary_group_three_by_sc <- function(orig.df, value, grouping1, grouping2, grouping3) {
  
    summary.df <- orig.df %>%
      group_by(get(grouping1), get(grouping2), get(grouping3)) %>%
      summarise(mean_prop = mean(get(value)), na.rm = TRUE,
                sd_prop = sd(get(value)),
                n_prop = n(),
                se_prop = sd(get(value))/sqrt(n())
      ) %>% ungroup()
    colnames(summary.df)[1] <- grouping1
    colnames(summary.df)[2] <- grouping2
    colnames(summary.df)[3] <- grouping3
  
  return(summary.df)
}


summary_binomial_data_group <- function(
    df,
    response,
    group,
    conf_level = 0.95,
    method = c("wilson", "bootstrap"),
    B = 5000
) {
  response <- rlang::enquo(response)
  group <- rlang::enquo(group)
  method <- match.arg(method)
  
  df %>%
    dplyr::group_by(!!group) %>%
    dplyr::summarise(
      n     = dplyr::n(),
      yeses = sum(!!response, na.rm = TRUE),
      prop  = mean(!!response, na.rm = TRUE),
      values = list(!!response),
      .groups = "drop"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ci = list(
        if (method == "wilson") {
          binom::binom.confint(
            x = yeses,
            n = n,
            conf.level = conf_level,
            methods = "wilson"
          )
        } else {
          boot_props <- replicate(
            B,
            mean(sample(values, replace = TRUE), na.rm = TRUE)
          )
          alpha <- (1 - conf_level) / 2
          tibble::tibble(
            lower = quantile(boot_props, alpha),
            upper = quantile(boot_props, 1 - alpha)
          )
        }
      ),
      ci_low  = ci$lower[1],
      ci_high = ci$upper[1]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-values, -ci)
}



# function to remove leading zeros for p values
cleanP <- function(X1)gsub("0\\.","\\.", X1)

cleanUpDecimals <- function(inputFrame) {
  names(inputFrame)[names(inputFrame) == 'z val.'] <- 'z'
  inputFrame %>% mutate_at(vars(-p), list(~round(., 2))) %>%
    mutate_at(vars(p), list(~round(., 3))) %>%
    mutate_at(vars(p), list(~cleanP(.))) %>%
    unite("CI", "2.5%", "97.5%", sep = ", ") %>% # Put 2.5% and 97.5% in brackets combined with odds ratio
    unite("Coefficient", "Est.", "CI", sep = " [") -> inputFrame
  inputFrame$Coefficient <- paste(inputFrame$Coefficient, ']', sep="")
  inputFrame$p[inputFrame$p=='0'] <- '< .001'
  inputFrame$p[inputFrame$p=='.001'] <- '< .001'
  return(inputFrame)
}


computeOddsRatios <- function(inputModel) {
  model_withOddsRatio_Fits <- summ(inputModel, confint = TRUE, digits = 4, exp=TRUE)
  inputFrame <- as.data.frame(model_withOddsRatio_Fits$coeftable)
  names(inputFrame)[names(inputFrame) == 'exp(Est.)'] <- 'Odds Ratio'
  inputFrame %>% mutate_at(vars(-p), list(~round(., 2))) %>%
    unite("CI", "2.5%", "97.5%", sep = ", ") %>% # Put 2.5% and 97.5% in brackets combined with odds ratio
    unite("Odds Ratio", "Odds Ratio", "CI", sep = " [") -> inputFrame
  inputFrame$`Odds Ratio` <- paste(inputFrame$`Odds Ratio`, ']', sep="")
  return(inputFrame)
}


add_interaction <- function(model, fac1, fac2) {
  interaction_term <- paste(fac1, fac2, sep = ":")
  
  update(
    model,
    as.formula(paste(". ~ . +", interaction_term))
  )
}

drop_interaction <- function(model, fac1, fac2) {
  interaction_term <- paste(fac1, fac2, sep = ":")
  
  update(
    model,
    as.formula(paste(". ~ . -", interaction_term))
  )
}

drop_main_effect <- function(model, term) {
  update(
    model,
    as.formula(paste(". ~ . -", term))
  )
}


tally_item_level_boost <- function(input.df) {
  by.item <- input.df %>%
    group_by(TargetWord, TrialType) %>%
    dplyr::summarise(mean = mean(Response),
                     P1 = mean(stim.premiseJudgment_P2),
                     P2 = mean(stim.premiseJudgment_P1),
                     SoloConc = mean(stim.ConcTruth),
                     Boost = mean - SoloConc,
                     .groups = "drop") %>%
    ungroup()
  return(by.item)
}


print_or_save <- function(expr,
                          file,
                          header = NULL,
                          runlive = get("RUN_LIVE",
                                        envir = parent.frame(),
                                        inherits = TRUE)) {
  
  expr <- enquo(expr)
  
  if (runlive) {
    
    if (!is.null(header)) {
      cat(header, "\n")
    }
    
    val <- eval_tidy(expr)
    
    if (is.character(val)) {
      cat(val, "\n")
    } else {
      print(val)
    }
    
  } else {
    
    capture.output(
      {
        withr::local_options(
          list(
            width = 10000,
            pillar.width = Inf,
            pillar.min_chars = Inf,
            tibble.print_max = Inf,
            tibble.print_min = Inf
          )
        )
        
        if (!is.null(header)) {
          cat("\n\n", header, "\n", sep = "")
        }
        
        val <- eval_tidy(expr)
        
        if (is.character(val)) {
          cat(val, "\n")
        } else {
          suppressWarnings(print(val, width = 10000))
        }
      },
      file = file,
      append = TRUE
    )
  }
}


#############################
#############################
#############################