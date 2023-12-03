#' readers
#'
#' @description Read in program from xlsx file
#'
#' @return Program as dataframe
#'
#' @noRd

library(readxl)

read_program <- function(x) {

  df <- read_excel(x)

  df <- df[,5:ncol(df)] %>% filter(if_any(everything(), ~ !is.na(.))) %>%
    filter_all(all_vars(!grepl("^Day\\s\\d", as.character(.)))) %>%
    filter_all(all_vars(!grepl("Intensity", as.character(.))))

  df_list <- lapply(seq(1, ncol(df), 8), function(i) df[, i:min(i+7, ncol(df))])

  # Bind all data frames together
  df_combined <- bind_rows(df_list) %>%
    mutate(
      `Week 1` = case_when(
        is.na(`Week 1`) & is.na(`Week 2`) ~ `Week 3`,
        is.na(`Week 1`) & is.na(`Week 3`) ~ `Week 2`,
        TRUE ~ `Week 1`))

  df_combined <- df_combined[,1:7]

  names(df_combined) <- c(
    'Exercise',
    'Sets',
    'Reps',
    'Intensity',
    'Load',
    'RPE',
    'Notes')

  df_combined

}
