#' @rdname repo_file
#'
#' @param recurse Logical. Should the listing recurse into subdirectories. Default `TRUE`.
#'
#' @export
#'
repo_tree = function(repo, path = ".", branch = NULL, recurse = TRUE) {
  arg_is_chr_scalar(repo, path)
  arg_is_chr_scalar(branch, allow_null = TRUE)
  arg_is_lgl_scalar(recurse)

  res = purrr::safely(github_api_repo_tree)(repo, branch)

  if (failed(res)) {
    status = error(res)[["headers"]][["status"]]

    cli_stop(
      "Failed to retrieve tree for repo {.val {format_repo(repo, branch)}}.",
      " ({.val {status}})"
    )
  }

  if (isTRUE(result(res)[["truncated"]]))
    cli_warn("File listing for repo {.val {repo}} was truncated by the GitHub API.")

  files = purrr::map_chr(result(res)[["tree"]], "path")

  path = as.character(fs::path_norm(path))
  if (path != ".") {
    files = files[startsWith(files, paste0(path, "/"))]
    if (length(files) == 0)
      cli_stop("Failed to retrieve path {.val {path}} in repo {.val {repo}}.")
  }

  rel = if (path == ".") files else substring(files, nchar(path) + 2)

  if (!recurse) {
    keep = !grepl("/", rel, fixed = TRUE)
    files = files[keep]
    rel = rel[keep]
  }

  parent = as.character(fs::path_dir(rel))
  ids = c(".", rel)

  d = tibble::tibble(
    id = ids,
    children = purrr::map(ids, function(id) rel[parent == id]),
    label = c(
      format_repo(repo, branch, if (path == ".") NULL else path),
      as.character(fs::path_file(rel))
    )
  )

  cat(cli::tree(d, root = "."), sep = "\n")

  invisible(files)
}
