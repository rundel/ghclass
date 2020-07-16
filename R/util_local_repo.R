is_git_repo = function(path) {
  arg_is_chr_scalar(path)

  res = purrr::safely(gert::git_open)(path)
  succeeded(res)
}

# If we are given a single repo directory check if it is a repo or a directory of repos
repo_dir_helper = function(repo_dir) {

  exists = fs::dir_exists(repo_dir)
  if(!all(exists))
    cli_stop("Unable to locate the repo{?s}: {.val {repo_dir[!exists]}}.")

  if (length(repo_dir) == 1 & !is_git_repo(repo_dir[1])) {
    dir = fs::dir_ls(repo_dir, type="directory")
  } else {
    dir = repo_dir
  }

  fs::path_real(dir)
}
