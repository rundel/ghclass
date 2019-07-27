## Trees API
github_api_branch_get = function(repo, branch = "master") {
  gh::gh(
    "GET /repos/:owner/:repo/branches/:branch",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    branch = branch,
    .token = github_get_token()
  )
}

keep_blobs = function(x, path = NULL) {
  arg_is_chr(path, allow_null = TRUE)

  if (!is.null(path)) {
    purrr::keep(x, purrr::map(x, "path") %in% path)
  } else {
    purrr::keep(x,
                purrr::map(x, "type") == "blob" &
                  purrr::map(x, "path") != ".gitignore")
  }
}

github_api_get_blob = function(repo, file_sha) {
  gh::gh(
    "GET /repos/:owner/:repo/git/blobs/:file_sha",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    file_sha = file_sha
  )
}

github_api_post_blob = function(repo, content) {
  gh::gh(
    "POST /repos/:owner/:repo/git/blobs",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    content = content[['content']],
    encoding = content[['encoding']]
  )
}

github_api_post_commit = function(repo, tree, parents, message = "Creating new tree") {
  gh::gh(
    "POST /repos/:owner/:repo/git/commits",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    message = message,
    parents = parents,
    tree = tree,
    parents = parents
  )
}

github_api_post_tree = function(repo, tree) {
  gh::gh(
    "POST /repos/:owner/:repo/git/trees",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    tree = tree
  )
}

github_api_patch_ref = function(repo,
                                sha,
                                branch = "master",
                                force = TRUE) {
  gh::gh(
    "PATCH /repos/:owner/:repo/git/:ref",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ref = glue::glue("refs/heads/{branch}"),
    sha = sha,
    force = force
  )
}

repo_move_file = function(source_repo,
                          target_repo,
                          source_path = NULL,
                          target_path = NULL,
                          source_repo_folder = NULL,
                          target_repo_folder = NULL,
                          branch = "master",
                          message = "Adding new files") {
  arg_is_chr_scalar(org, branch)
  arg_is_chr_scalar(source_repo_folder,
                    target_repo_folder,
                    message,
                    allow_null = TRUE)
  arg_is_chr(source_repo, target_repo)
  arg_is_chr(source_path, target_path, allow_null = TRUE)

  ## Get commits on target repo
  target_root = get_lastcommit_sha(target_repo)

  # Rething whether this is necessary.
  # Can I get the folder from the recursively grabbed branch?
  if (!is.null(source_repo_folder)) {
    source_files = repo_files(source_repo, branch)
    source_tree_sha = source_files$sha[files$path == source_repo_folder]
  } else {
    source_res = purrr::safely(github_api_branch_get)(source_repo, branch)
    status_msg(source_res,
               fail = "Failed to retrieve branch {usethis::ui_value(branch)} for repository {usethis::ui_value(source_repo)}.")
    source_tree_sha = source_res$result$commit$commit$tree$sha
  }

  source_tree_all = github_api_repo_get_tree(source_repo, sha = source_tree_sha)

  source_tree = keep_blobs(source_tree_all$tree, source_path)

  if (!is.null(target_repo_folder)) {
    target_files = repo_files(target_repo, branch)
    target_tree_sha = target_files$sha[files$path == target_repo_folder]
  } else {
    target_res = purrr::safely(github_api_branch_get)(target_repo, branch)
    status_msg(target_res,
               fail = "Failed to retrieve branch {usethis::ui_value(branch)} for repository {usethis::ui_value(target_repo)}.")
    target_tree_sha = target_res$result$commit$commit$tree$sha
  }

  target_tree_all = github_api_repo_get_tree(target_repo, sha = target_tree_sha)

  target_tree = keep_blobs(target_tree_all$tree, target_path)

  df_source = purrr::map_dfr(source_tree, ~.)
  df_source$from = "source"
  df_target = purrr::map_dfr(target_tree, ~.)
  df_target$from = "target"
  df_all = rbind(df_source, df_target)

  tree_content = purrr::map(seq_len(nrow(df_all)),
                            function(x) {
                              content = github_api_get_blob(ifelse(df_all[x, 'from'] == "source", source_repo, target_repo),
                                                            df_all[x, 'sha'])
                              github_api_post_blob(target_repo, content)

                              list(
                                path = ifelse(
                                  is.null(target_repo_folder),
                                  as.character(df_all[x, 'path']),
                                  glue::glue("{target_repo_folder}/{as.character(df_all[x, 'path'])}")
                                ),
                                mode = as.character(df_all[x, 'mode']),
                                type = as.character(df_all[x, 'type']),
                                sha = as.character(df_all[x, 'sha'])
                              )
                            })



  # tree_content = purrr::map(tree,
  #                           function(x) {
  #                             content = github_api_get_blob(source_repo, x['sha'])
  #                             github_api_post_blob(target_repo, content)
  #
  #                             list(
  #                               path = ifelse(
  #                                 is.null(target_repo_folder),
  #                                 x[['path']],
  #                                 glue::glue("{target_repo_folder}/{x[['path']]}")
  #                               ),
  #                               mode = x[['mode']],
  #                               type = x[['type']],
  #                               sha = x[['sha']]
  #                             )
  #                           })

  new_tree = github_api_post_tree(target_repo, tree_content)

  # Leaving parent blank will nuke commit history
  new_commit = github_api_post_commit(target_repo,
                                      new_tree$sha,
                                      parents = list(target_root))

  out = github_api_patch_ref(target_repo,
                             new_commit$sha,
                             branch = branch,
                             force = TRUE)

}
