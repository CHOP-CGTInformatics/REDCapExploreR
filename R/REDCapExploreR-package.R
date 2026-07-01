#' @keywords internal
#' @aliases REDCapExploreR-package
#' @importFrom cli cli_abort cli_fmt cli_progress_bar cli_progress_done
#' cli_progress_update cli_text cli_vec cli_warn qty symbol
#' @importFrom dplyr %>% across arrange bind_rows case_when count distinct filter group_by
#' if_any if_else n n_distinct
#' left_join mutate pull recode relocate rename right_join row_number rowwise
#' select slice summarise transmute ungroup bind_cols first
#' @importFrom purrr map map2 map_chr map_int map_lgl pluck pmap_chr pmap
#' flatten_chr map2_chr reduce
#' @importFrom REDCapR redcap_arm_export redcap_event_instruments redcap_instruments
#' redcap_event_read redcap_instrument_repeating redcap_metadata_read
#' redcap_read_oneshot sanitize_token
#' @importFrom REDCapTidieR read_redcap extract_tibbles
#' @importFrom rlang .data !!! abort as_closure caller_arg caller_env catch_cnd
#' check_installed cnd_muffle current_call current_env enexpr enquo env_poke
#' eval_tidy get_env global_env is_atomic is_bare_formula is_bare_list quo_name
#' is_installed new_environment quo_get_expr try_fetch zap as_label sym syms expr
#' :=
#' @importFrom stats IQR quantile
#' @importFrom stringr str_detect str_replace str_replace_all str_squish str_trunc
#' str_trim str_ends
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tidyselect all_of any_of ends_with eval_select everything
#' starts_with
#' @importFrom utils read.csv
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
