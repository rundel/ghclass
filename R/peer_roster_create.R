##---------------------------------------------------------
# Naming principles
#
# rev denotes reviewer
# aut denotes author
# _review indicates repositories that are specific for review files
#
##---------------------------------------------------------


#' Create peer review roster
#'
#' `peer_roster_create` creates data frame of random assignments of author files to
#' reviewers. By default, the output is saved to a `.csv` file in the current working
#' directory that incorporates the current date and random seed as part of the file name.
#'
#' @param n_rev Numeric. Number of reviews per user. Must be larger than zero and smaller
#' than the number of users.
#' @param user Character. A vector of GitHub user names.
#' @param seed Numeric. Random seed for assignment, if not set a random seed will be chosen.
#' @param write_csv Logical. Whether the roster data frame should be written to a `.csv` file
#' in the current working directory, defaults to TRUE.
#' @param dir Character. Directory where the peer review roster will be written if `write_csv = TRUE`.
#'
#' @examples
#' \dontrun{
#' peer_roster_create(
#'   3,
#'   c("anya-ghclass", "bruno-ghclass",
#'     "celine-ghclass", "diego-ghclass"),
#'   dir = "hw2/"
#' )
#' }
#'
#' @family peer review functions
#'
#' @export
#'
peer_roster_create = function(n_rev, user, seed = NULL, write_csv = TRUE, dir = NULL) {
  arg_is_chr(user)
  arg_is_chr(dir, allow_null = TRUE)
  arg_is_pos_int(n_rev)
  arg_is_pos_int_scalar(seed, allow_null = TRUE) #fails now

  if (!(length(user) > 1)) {
    cli_stop("{.field user} must contain more than one user name.")
  }

  if (!(n_rev < length(user))) {
    cli_stop("{.field n_rev} must be smaller than the number of users in {.field user}.")
  }

  if (write_csv) {
    if (is.null(dir)) {
      cli_stop("No directory specified in {.field dir}.")
    } else if (!dir.exists(dir)) {
      cli_stop("Directory {.val {dir}} does not exist.")
    }
  }

  if (is.null(seed)) {
    seed = sample.int(1e+06, 1L)
    cli_warn("No seed was specified. Using randomly sampled seed {.val {seed}}")
  }

  withr::with_seed(seed, {
    j = sample(2:length(user), n_rev)

    # Randomizing user names to avoid clustering
    user_random = paste0("aut", sample(1:length(user), length(user)))
  })

  res = purrr::map(j, ~ latin_square(.x, length(user)))


  df_sort = tibble::tibble(user = user,
                           user_random = user_random)[order(as.numeric(sub("[aA-zZ]+", "", user_random))),]

  df_tmp = purrr::set_names(
    if (length(user) > 2) {
      out = purrr::map_dfc(res, ~ df_sort[['user_random']][.x])
    } else {
      # if length(user) == 2, will always res = c(2, 1)
      out = tibble::tibble(rev(user_random))
    },
    paste0("rev", 1:n_rev)
  )

  res_df = tibble::as_tibble(cbind(df_sort, df_tmp))
  attr(res_df, "seed") <- seed
  attr(res_df, "username") <- user
  attributes(res_df)

  if (write_csv) {
    fname = glue::glue("roster_seed{seed}.csv")
    readr::write_csv(res_df, glue::glue("{dir}/{fname}"))
    cli::cli_alert_success("Saved file {.val {fname}} to {.val {dir}}.")
  }
  res_df
}
