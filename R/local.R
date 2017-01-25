grab_repos = function(org, pattern, local_path)
{
  # org = args[1]
  # pattern = args[2]
  # local_path = args[3]
  #
  # progress = FALSE
  # if (length(args) == 4)
  #   progress = as.logical(args[4])
  #
  # repos = gh("GET /orgs/:org/repos", org = org, .token=token, .limit=1000)
  # repo_names = sapply(repos, function(x) x$name)
  #
  # selected_repos = str_detect(repo_names,pattern) %>% repo_names[.]
  #
  #
  # for(repo in selected_repos)
  # {
  #   cat("Grabbing", repo, "...\n")
  #   org_url = paste0('git@github.com:',org,'/',repo,'.git')
  #
  #   d = file.path(local_path,repo)
  #   dir.create(d, recursive=TRUE, showWarnings=FALSE)
  #
  #   local_repo = clone(org_url, d, progress=progress)
  # }
}

compile_rmds = function(local_path)
{
  # local_path = args[1]
  #
  # stopifnot(dir.exists(local_path))
  #
  # repos = list.dirs(path = local_path, full.names = TRUE, recursive = FALSE)
  #
  # prev_dir = getwd()
  #
  # for(repo in repos)
  # {
  #   cat("Knitting in", repo, "...\n")
  #
  #   setwd(repo)
  #
  #   try({
  #     rmds = list.files("./", pattern="[Rr]md$")
  #     lapply(rmds, render, quiet=TRUE)
  #   })
  #
  #   setwd(prev_dir)
  # }
}

update_repos = function(local_path)
{
  # local_path = args[1]
  #
  # repos = list.dirs(path = local_path, full.names = TRUE, recursive=FALSE)
  #
  # for(repo in repos)
  # {
  #   cat("Updating", repo, "... (")
  #   pull(repo = repository(repo))
  #   cat(")\n")
  # }
}
