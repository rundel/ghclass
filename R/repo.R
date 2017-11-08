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
create_team_repos = function(org, teams = get_teams(org), prefix="", suffix="", verbose=TRUE, delay=0.2)
{
  if (prefix == "" & suffix == "")
    stop("Either a prefix or a suffix must be specified")

  if (is.character(teams))
  {
    org_teams = get_teams(org)
    teams = org_teams[ teams %in% names(org_teams) ]
  }

  for(team in names(teams))
  {
    repo_name = paste0(prefix, team, suffix)

    if (verbose)
      cat("Creating ", repo_name, " for ",team," ...\n",sep="")

    try({
      gh("POST /orgs/:org/repos",
         org = org,
         name=repo_name, private=TRUE, team_id=teams[team],
         auto_init=TRUE, gitignore_template="R",
         .token=get_github_token())
    })

    Sys.sleep(delay)

    try({
      gh("PUT /teams/:id/repos/:org/:repo",
         id = teams[team], org = org, repo = repo_name,
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

  system(paste0(git, " clone --bare ", repo_url(source_repo)), intern = FALSE,
         wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

  repo_dir = dir(pattern = "\\.git")
  stopifnot(length(repo_dir) == 1)
  setwd(repo_dir)

  for(repo in target_repos)
  {
    if (verbose)
      cat("Mirroring ", source_repo, " to ", repo,"...\n", sep="")

    try({
      system(paste0(git, " push --mirror ", repo_url(repo)), intern = FALSE,
             wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    })
  }

  if (verbose)
    cat("Cleaning up ...\n")

  unlink(file.path("..",repo_dir), recursive = TRUE)
}


