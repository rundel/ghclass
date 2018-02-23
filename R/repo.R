#' @export
#'
check_repos = function(repos)
{
  exists = function(owner, repo)
  {
    gh("GET /repos/:owner/:repo", owner=owner, repo=repo, .token=get_github_token())
    TRUE
  }

  purrr::map2_lgl(
    get_repo_owner(repos), get_repo_name(repos),
    purrr::possibly(exists, FALSE)
  )
}

#' @export
#'
fix_repo_name = function(repos)
{
  repos = stringr::str_replace_all(repos, " ", "_")
  stringr::str_replace_all(repos, "[^A-Za-z0-9_.-]+","-")
}

#' @export
#'
create_individual_repo = function(org, user,
                                  prefix="", suffix="", verbose=TRUE,
                                  auto_init=FALSE, gitignore_template="R") {
  if (prefix == "" & suffix == "")
    stop("Either a prefix or a suffix must be specified")

  org_users = get_members(org)

  purrr::walk(
    user,
    function(user) {
      repo_name = fix_repo_name(paste0(prefix, user, suffix))

      if (check_repos(repo_name)) {
        warning("Repo ", org,"/",repo_name, " already exists", call. = FALSE, noBreaks. = TRUE)
        return(invisible(NULL))
      }

      if (verbose)
        message("Creating repo ", org, "/", repo_name, " ...", sep="")

      try({
        gh("POST /orgs/:org/repos",
           org = org,
           name=repo_name, private=TRUE,
           auto_init=auto_init,
           gitignore_template=gitignore_template,
           .token=get_github_token())
      })

      try({
        gh("PUT /repos/:owner/:repo/collaborators/:username",
           owner = org, repo = repo_name, username = user,
           permission="push",
           .token=get_github_token())
      })
    }
  )
}


#' @export
#'
create_team_repo = function(org, team, prefix="", suffix="", verbose=TRUE)
{
  org_teams = get_teams(org)

  if (is.character(team)) {
    team = merge(
      tibble::data_frame(name = team), org_teams,
      by = "name", all.x = TRUE
    )
  }

  stopifnot(is.data.frame(team) & all( c("name","id") %in% names(team)))

  missing_ids = is.na(team[["id"]])
  if (any(missing_ids))
    stop("Unable to locate team(s): ", paste(team[["name"]][missing_ids], collapse=", "), call. = FALSE)

  purrr::pwalk(
    team,
    function(name, id) {
      repo_name = fix_repo_name( paste0(prefix, name, suffix) )
      print(repo_name)


      if (verbose)
        message("Creating repo ", org, "/", repo_name, " ...", sep="")

      res = purrr::safely(function() {
        gh("POST /orgs/:org/repos",
           org = org,
           name=repo_name, private=TRUE, team_id=id,
           auto_init=TRUE, gitignore_template="R",
           .token=get_github_token())

        gh("PUT /teams/:id/repos/:org/:repo",
           id = id, org = org, repo = repo_name,
           permission="push",
           .token=get_github_token())
      })()

      check_result(res, sprintf("Failed to create team repo %s.", repo_name), verbose)
    }
  )
}









#' @export
mirror_repo = function(source_repo, target_repos, verbose=TRUE)
{
  stopifnot(length(source_repo) == 1)
  stopifnot(length(target_repos) >= 1)

  stopifnot(check_repos(source_repo))
  stopifnot(all(check_repos(target_repos)))

  git = require_git()

  cur_dir = getwd()
  setwd(tempdir())
  on.exit({setwd(cur_dir)})

  if (verbose)
    cat("Cloning source repo (", source_repo, ") ...\n", sep = "")

  system(paste0(git, " clone --bare ", get_repo_url(source_repo)), intern = FALSE,
         wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

  repo_dir = dir(pattern = "\\.git")
  stopifnot(length(repo_dir) == 1)
  setwd(repo_dir)

  purrr::walk(
    target_repos,
    function(repo) {

      if (verbose)
        cat("Mirroring ", source_repo, " to ", repo,"...\n", sep="")

      try({
        system(paste0(git, " push --mirror ", get_repo_url(repo)), intern = FALSE,
               wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
      })
    }
  )

  if (verbose)
    cat("Cleaning up ...\n")

  unlink(file.path("..",repo_dir), recursive = TRUE)
}


get_commit = function(repo, ref="HEAD") {
  stopifnot(length(repo)==1)

  name = get_repo_name(repo)
  owner = get_repo_owner(repo)

  gh("GET /repos/:owner/:repo/commits/:ref",
     owner = owner, repo = name, ref = ref,
     .token=get_github_token())
}

#' @export
branch_repo = function(repos, branch, verbose=TRUE)
{
  purrr::walk2(
    repos, branch,
    function(repo, branch) {

      tryCatch({
        if (!check_repos(repo))
          stop(repo, "does not exist.")

        name = get_repo_name(repo)
        owner = get_repo_owner(repo)

        head = get_commit(repo)

        gh("POST /repos/:owner/:repo/git/refs",
           owner = owner, repo = name,
           ref = paste0("refs/heads/",branch),
           sha = head$sha,
           .token=get_github_token())
      }, error = function(e) {
        warning("Failed to create ", repo, "@", branch, " branch. (", e$content$message, ")", call. = FALSE)
      })
    }
  )
}


#' @export
style_repo = function(repo, files=c("*.R","*.Rmd"), branch="styler", git = require_git(), verbose=TRUE)
{
  stopifnot(styler_available())
  stopifnot(length(repo) >= 1)

  dir = file.path(tempdir(),"styler")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  on.exit({
    unlink(file.path(dir), recursive = TRUE)
  })

  purrr::walk2(
    repo, branch,
    function(repo, branch) {

      branch_repo(repo, branch, verbose = FALSE)
      path = clone_repo(repo, local_path = dir, branch = branch)

      file_paths = unlist(purrr::map(files, ~ fs::dir_ls(path, recursive = TRUE, glob = .x)), use.names = FALSE)

      cur_dir = getwd()
      setwd(path)

      on.exit({
        setwd(cur_dir)
      })

      writeLines(
        c("Results of running styler:", utils::capture.output( styler::style_file(file_paths) )),
        "commit_msg"
      )

      system(paste0(git, " add ", paste0(file_paths, collapse=" ")),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      system(paste0(git, " commit -F commit_msg"),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      system(paste0(git, " push"),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
  )
}
