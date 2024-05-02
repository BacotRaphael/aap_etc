#' add_wgss
#'
#' @param .dataset Input dataset at individual level
#' @param wgss_seeing the name of the variable that indicates the difficulty experienced for seeing
#' @param wgss_hearing the name of the variable that indicates the difficulty experienced for hearing
#' @param wgss_walking the name of the variable that indicates the difficulty experienced for walking
#' @param wgss_remembering the name of the variable that indicates the difficulty experienced for remembering
#' @param wgss_selfcare the name of the variable that indicates the difficulty experienced for selfcare
#' @param wgss_communicating the name of the variable that indicates the difficulty experienced for communicating
#' @param value_no_difficulty the value that indicates no difficulty
#' @param value_some_difficulty the value that indicates some difficulty
#' @param value_a_lot_of_difficulty the value that indicates a lot of difficulty
#' @param value_cannot_do_it the value that indicates that the person cannot do it at all
#' @param value_dnk the value that indicates that the person does not know. If not present in the survey, set to NULL
#' @param value_pnta the value that indicates that the person prefered not to answer. If not present in the survey, set to NULL
#' @param value_na the value that indicates that the indicator is NA
#' @param threshold indicates the threshold value starting which the individual will be considered as "having a difficulty". By default set to 'a_lot_of_difficulty', can be set to 'some_difficulty'

#' @return a dataset with 7 binary columns created, wgss_any_difficulty reporting individual that experience a difficulty in at least one of the 6 dimensions, and wgss_..._diff for each of the 6 dimensions

add_wgss <- function(.dataset = mli_indv,
                     # col_uuid_hh = "loop_uuid",
                     # col_uuid_ind = "X_uuid",
                     wgss_seeing = "wgss_seeing",
                     wgss_hearing = "wgss_hearing",
                     wgss_walking = "wgss_walking",
                     wgss_remembering = "wgss_remembering",
                     wgss_selfcare = "wgss_selfcare", 
                     wgss_communicating = "wgss_communicating",
                     value_no_difficulty = "no_difficulty",
                     value_some_difficulty = "some_difficulty",
                     value_a_lot_of_difficulty = "a_lot_of_difficulty",
                     value_cannot_do_it = "cannot_do_at_all",
                     value_dnk = "dont_know",
                     value_pnta = "refused_to_answer",
                     value_na = "",
                     threshold = "a_lot_of_difficulty",
                     keep_names = F,
                     overwrite = T
                     ){
  
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }
  
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  
  all.col <- c(wgss_seeing, 
               wgss_hearing, 
               wgss_walking, 
               wgss_remembering, 
               wgss_selfcare, 
               wgss_communicating)
  
  if (!all(all.col %in% colnames(.dataset))) {
    stop(paste0("Some of the columns are not in the dataset: '", all.col[!all.col %in% colnames(.dataset)], "'"))
  }
  
  # checking that function has not be run already once with or without renaming of column names
  col.diff.data <- paste0(all.col, "_diff")
  col.diff.standard <- c("wgss_seeing_diff", "wgss_hearing_diff", "wgss_walking_diff", "wgss_remembering_diff", "wgss_selfcare_diff", "wgss_communicating_diff")
  
  if (all(col.diff.data %in% colnames(.dataset)) | all(col.diff.standard %in% colnames(.dataset))) {
    if (!overwrite) stop("The function has already been run on this dataset. Please remove the columns ending with '_diff' before running the function again.")
    if (overwrite) {
      print(paste0("The function has already been run on this dataset. The column wgss_any_difficulty and wgss_..._diff for all dimensions will be replaced with new values."))
      .dataset <- .dataset %>% select(-any_of(c("wgss_any_difficulty", col.diff.data, col.diff.standard)))
    }
  }
  
  col.values <- c("value_no_difficulty"=value_no_difficulty, 
                  "value_some_difficulty"=value_some_difficulty, 
                  "value_a_lot_of_difficulty"=value_a_lot_of_difficulty, 
                  "value_cannot_do_it"=value_cannot_do_it, 
                  "value_dnk"=value_dnk, 
                  "value_pnta"=value_pnta, 
                  "value_na"=value_na)
  
  ## extract names of objects in the vector above:
  name.col.value <- names(col.values)
  check.unique.dimension <- .dataset %>% select(all_of(all.col)) %>% lapply(unique)
  check.unique <- check.unique.dimension %>% unlist() %>% unique
  check.val <- col.values %in% check.unique
  
  if (sum(check.val)<length(check.unique)) {
    stop(paste0("Some values are not in the list of possible values\nPossible values are: '", names(col.values)[!check.val],"' set as '", col.values[!check.val], "' in the value argument of the function."))
  }
  
  ## seting difficulty values depending on chosen threshold
  if (!threshold %in% c("a_lot_of_difficulty")) stop(paste0("The threshold value is not authorized. Please enter either a_lot_of_difficulty or some_difficulty"))
  if (threshold=="a_lot_of_difficulty") val.difficulty <- c(value_a_lot_of_difficulty, value_cannot_do_it)
  if (threshold=="some_difficulty") val.difficulty <- c(value_some_difficulty, value_a_lot_of_difficulty, value_cannot_do_it)
  val.na <- c(value_dnk, value_pnta, value_na)
  val.no.difficulty <- col.values[!col.values %in% c(val.difficulty, val.na)] %>% unname

  .dataset <- .dataset %>%
    mutate(across(all_of(all.col), ~ case_when(. %in% val.difficulty ~ 1, 
                                               . %in% val.no.difficulty ~ 0,
                                               . %in% val.na ~ NA_real_),
                  .names = "{.col}_diff"),
           wgss_any_difficulty = case_when(rowSums(across(all_of(col.diff.data), ~ .), na.rm=T) > 0 ~ 1, 
                                           rowSums(across(all_of(col.diff.data), ~ .), na.rm=T) == 0 ~ 0,
                                           rowSums(across(all_of(col.diff.data), ~ is.na(.))) == 6 ~ NA_real_))
  
  ## Check what to do with NAs
  ## Finish running on other countries on Thursday
  
  if (!keep_names) {
    .dataset <- .dataset %>% 
      rename_with(~col.diff.standard, all_of(col.diff.data))
      }
  return(.dataset)    
}





