#' @export
#'
check_repos = function(repos)
{
  exists = function(owner, repo)
  {
    gh("GET /repos/:owner/:repo", owner=owner, repo=repo, .token=get_github_token())
    TRUE
  }

  map2_lgl(
    get_repo_owner(repos), get_repo_name(repos),
    possibly(exists, FALSE)
  )
}

#' @export
#'
fix_repo_name = function(repos)
{
  repos %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^A-Za-z0-9_.-]+","-")
}

#' @export
#'
create_individual_repos = function(org, users,
                                   prefix="", suffix="", verbose=TRUE,
                                   auto_init=FALSE, gitignore_template="R") {
  if (prefix == "" & suffix == "")
    stop("Either a prefix or a suffix must be specified")

  org_users = get_members(org)

  walk(
    users,
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
create_team_repos = function(org, teams, prefix="", suffix="", verbose=TRUE)
{
  org_teams = get_teams(org)

  if (missing(teams)) {
    teams = org_teams
  } else {
    teams = left_join(
      data.frame(name = teams, stringsAsFactors = FALSE),
      org_teams,
      by = "name")
  }

  missing_ids = is.na(teams$id)
  if (any(missing_ids))
    stop("Unable to locate team(s): ", paste(teams$name[missing_ids], collapse=", "))

  for(i in seq_len(nrow(teams)))
  {
    team = teams$name[i]
    id = teams$id[i]

    repo_name = paste0(prefix, team, suffix) %>% fix_repo_name()

    if (verbose)
      message("Creating repo ", org, "/", repo_name, " ...", sep="")

    try({
      gh("POST /orgs/:org/repos",
         org = org,
         name=repo_name, private=TRUE, team_id=id,
         auto_init=TRUE, gitignore_template="R",
         .token=get_github_token())
    })

    try({
      gh("PUT /teams/:id/repos/:org/:repo",
         id = id, org = org, repo = repo_name,
         permission="push",
         .token=get_github_token())
    })
  }
}

#' @export
get_file = function(repo, file, branch="master")
{
  repo_name  = get_repo_name(repo)
  repo_owner = get_repo_owner(repo)

  gh("GET /repos/:owner/:repo/contents/:path",
     owner = repo_owner, repo = repo_name, path=file,
     ref = branch,
     .token=get_github_token(), .limit=get_github_api_limit())
}

#' @export
check_files = function(repos, files, branch = "master")
{
  file_exists = function(repo, file, branch)
  {
    get_file(repo,file,branch)
    TRUE
  }

  pmap_lgl(list(repos, files, branch), possibly(file_exists,FALSE))
}

#' @export
add_files = function(repos, message, files, branch = "master", preserve_path=FALSE, verbose=TRUE)
{
  stopifnot(all(file.exists(files)))
  stopifnot(all(check_repos(repos)))

  repo_name  = get_repo_name(repos)
  repo_owner = get_repo_owner(repos)

  walk(repos, function(repo) {

    name = get_repo_name(repo)
    owner = get_repo_owner(repo)

    if (verbose)
      cat("Adding files to", repo, "...\n")

    walk(files, function(file) {

      gh_path = file
      if (!preserve_path)
        gh_path = basename(file)

      content = base64enc::base64encode(file)

      tryCatch({
        if (check_files(repo, gh_path, branch)) {
          gh_file = get_file(repo, gh_path, branch)
          gh("PUT /repos/:owner/:repo/contents/:path",
             owner = owner, repo = name, path=gh_path,
             message = message, content = content, branch = branch,
             sha = gh_file$sha,
              .token=get_github_token())
        } else {
          gh("PUT /repos/:owner/:repo/contents/:path",
             owner = owner, repo = name, path=gh_path,
             message = message, content = content, branch = branch,
             .token=get_github_token())
        }
      }, error = function(e) {
          message("Adding ", file, " to ", repo, " failed.")
          if (verbose)
            print(e)
      })
    })
  })
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

  for(repo in target_repos)
  {
    if (verbose)
      cat("Mirroring ", source_repo, " to ", repo,"...\n", sep="")

    try({
      system(paste0(git, " push --mirror ", get_repo_url(repo)), intern = FALSE,
             wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    })
  }

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
  walk2(
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
style_repo = function(repos, files=c("*.R","*.Rmd"), branch="styler", git = require_git(), verbose=TRUE)
{
  stopifnot(styler_available())
  stopifnot(length(repos) >= 1)

  dir = file.path(tempdir(),"styler")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  on.exit({
    unlink(file.path(dir), recursive = TRUE)
  })

  walk2(
    repos, branch,
    function(repo, branch) {

      branch_repo(repo, branch, verbose = FALSE)
      path = clone_repos(repo, local_path = dir, branch = branch)

      file_paths = unlist(map(files, ~ fs::dir_ls(path, recursive = TRUE, glob = .x)), use.names = FALSE)

      cur_dir = getwd()
      setwd(path)

      on.exit({
        setwd(cur_dir)
      })

      writeLines(
        c("Results of running styler:", capture.output( styler::style_file(file_paths) )),
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
