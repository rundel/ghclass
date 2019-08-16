# The following two functions (peer_anonymize_file and remove_author_rmd) are prob not needed any longer if we decide not to include author as a YAML parameter

# If we keep this function, it should just strip the author field from YAML
peer_anonymize_file = function(path) {
  remove_author_rmd(path)
}

remove_author_rmd = function(input) {
  sub(
    '\\nauthor: \\"[aA-zZ]+ ([aA-zZ]+[ \\.]+)?[aA-zZ]+\"',
    '\\nauthor: \\"Student x"',
    input
  )
}


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

repo_file_move = function(source_repo,
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
  target_root = get_lastcommit_sha(target_repo) #apicall

  # Rething whether this is necessary.
  # Can I get the folder from the recursively grabbed branch?
  if (!is.null(source_repo_folder)) {
    source_files = repo_files(source_repo, branch) #apicall
    source_tree_sha = source_files$sha[source_files$path == source_repo_folder]
  } else {
    source_res = purrr::safely(github_api_branch_get)(source_repo, branch)
    status_msg(source_res,
               fail = "Failed to retrieve branch {usethis::ui_value(branch)} for repository {usethis::ui_value(source_repo)}.")
    source_tree_sha1 = source_res$result$commit$commit$tree$sha
  }

  source_tree_all = github_api_repo_tree(source_repo, sha = source_tree_sha)
  source_tree_all1 = github_api_repo_tree(source_repo, sha = source_tree_sha1)

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

  target_tree_all = github_api_repo_tree(target_repo, sha = target_tree_sha)

  target_tree = keep_blobs(target_tree_all$tree, target_path)

  df_source = purrr::map_dfr(source_tree, ~ .)
  df_source$from = "source"
  df_target = purrr::map_dfr(target_tree, ~ .)
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
                                  glue::glue(
                                    "{target_repo_folder}/{as.character(df_all[x, 'path'])}"
                                  )
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

# https://github.community/t5/How-to-use-Git-and-GitHub/Adding-a-folder-from-one-repo-to-another/td-p/5425
# Not yet vectorized over path, need to specify path

peer_assign_clone = function(org,
                             roster,
                             path,
                             local_path,
                             prefix = "",
                             suffix = "",
                             temp_folder = "assign") {
  arg_is_chr(local_path, path)
  arg_is_chr_scalar(prefix, suffix, prefix_rev, suffix_rev, temp_folder)

  prefix_rev = format_rev(prefix, suffix)$prefix_rev
  suffix_rev = format_rev(prefix, suffix)$suffix_rev

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  # 1. Temporary working directory & creating temp folder & change wd
  setwd(local_path)
  folder = processx_run("mkdir", c(temp_folder), wd = local_path)
  processx_check(folder)
  temp_wd = fs::path(local_path, temp_folder)
  setwd(temp_wd)

  repo_a = unique(rdf[['repo_a']])
  purrr::walk(repo_a,
              function(x) {
                # 2. Clone all author repos
                res = purrr::safely(org_repo_clone)(x, temp_wd, verbose = FALSE)
                status_msg(res,
                           fail = glue::glue("Failed to clone {usethis::ui_value(x)}."))

                # 3. Remove remote
                setwd(fs::path(temp_wd, get_repo_name(x)))
                rmremote = processx_run("git", c("remote", "rm", "origin"))
                processx_check(rmremote)

                # 4. Make new folder & move files into it
                folder = unique(rdf$author_random[rdf$repo_a == x])

                # Create subfolder, filter for subfolder
                if (!is.null(path)) {
                  system(glue::glue("mkdir {folder} && mv {path} {folder}"))
                  processx_run("git", c("add", "."))
                  processx_run("git", c("commit", "-m'Moving files to author folder'"))
                  filter = processx_run("git",
                                        c(
                                          "filter-branch",
                                          "--subdirectory-filter",
                                          folder,
                                          "--",
                                          "--all"
                                        ))
                }

                # Suppress warnings - this moves all files in repo
                system(glue::glue("mkdir {folder} && mv * {folder}"))
                getwd()

                # 5. Add to git repo
                addfiles = processx_run("git", c("add", "."))
                processx_check(addfiles)

                # 6. commit
                commit = processx_run("git", c("commit", "-m'Add files to folder'"))
                processx_check(commit)

                ### grab destination reviewer repos
                repo_r_rev = unique(rdf$repo_r_rev[rdf$repo_a == x])
                purrr::walk(repo_r_rev,
                            function(y) {
                              # 7. clone reviewer repos
                              res_rev = purrr::safely(org_repo_clone)(y, temp_wd, verbose = FALSE)
                              status_msg(res_rev,
                                         fail = glue::glue("Failed to clone {usethis::ui_value(y)}."))
                              repo_rev_wd = fs::path(temp_wd, get_repo_name(y))
                              setwd(repo_rev_wd)

                              # 8. add remote
                              remoteadd = processx_run("git",
                                                       c(
                                                         "remote",
                                                         "add",
                                                         "modified-source",
                                                         glue::glue("../../{temp_folder}/{get_repo_name(x)}")
                                                       ))

                              # 9. pull remote
                              remotepull = processx_run("git",
                                                        c(
                                                          "pull",
                                                          "modified-source",
                                                          "master",
                                                          "--allow-unrelated-histories"
                                                        ))

                              # 10. remove remote
                              rmremote2 = processx_run("git", c("remote", "rm", "modified-source"))
                              processx_check(rmremote2)

                              # 11. Remove git history
                              getwd()
                              rmhist = processx_run("rm", c("-rf", ".git"))
                              newinit = processx_run("git", c("init"))
                              newadd = processx_run("git", c("add", "."))
                              newcommit = processx_run("git", c("commit", "-m'Adding author files'"))

                              # 12. Add remote
                              newremoteadd = processx_run("git", c(
                                "remote",
                                "add",
                                "origin",
                                glue::glue(
                                  "https://github.com/{get_repo_owner(y)}/{get_repo_name(y)}"
                                )
                              ))

                              # 13. Pull upstream changes
                              pulltarget = processx_run("git",
                                                        c(
                                                          "pull",
                                                          "origin",
                                                          "master",
                                                          "--allow-unrelated-histories"
                                                        ))

                              # 14. push changes
                              getwd()
                              processx_run("git", c("status"))
                              pushtarget = processx_run("git", c("push", "-u", "origin", "master"))
                            })
              })
}

peer_return_clone = function(org,
                             roster,
                             path,
                             local_path,
                             prefix = "",
                             suffix = "",
                             temp_folder = "return",
                             double_blind = FALSE) {

  arg_is_chr(path, local_path)
  arg_is_chr_scalar(prefix, suffix, prefix_rev, suffix_rev, temp_folder)

  prefix_rev = format_rev(prefix, suffix)$prefix_rev
  suffix_rev = format_rev(prefix, suffix)$suffix_rev

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  # 1. Temporary working directory & creating temp folder & change wd
  setwd(local_path)
  folder = processx_run("mkdir", c(temp_folder), wd = local_path)
  processx_check(folder)
  temp_wd = fs::path(local_path, temp_folder)
  setwd(temp_wd)

  purrr::walk2(rdf[['repo_r_rev']], rdf[['repo_a']],
               function(r, a) {

                 # 2. Clone review repos
                 res = purrr::safely(org_repo_clone)(r, temp_wd, verbose = FALSE)
                 status_msg(res,
                            fail = glue::glue("Failed to clone {usethis::ui_value(r)}."))

                 # 3. Remove remote
                 setwd(fs::path(temp_wd, get_repo_name(r)))
                 rmremote = processx_run("git", c("remote", "rm", "origin"))
                 processx_check(rmremote)

                 # 4. Make new folder, filter & move files into it
                 if (!double_blind) {
                   target_folder = rdf$reviewer[rdf$repo_r_rev == r]
                 } else {
                   target_folder = rdf$reviewer_no[rdf$repo_r_rev == r & rdf$repo_a == a]
                 }

                 filter = processx_run("git",
                                       c(
                                         "filter-branch",
                                         "--subdirectory-filter",
                                         rdf$author_random[rdf$repo_r_rev == r & rdf$repo_a == a],
                                         "--",
                                         "--all"
                                       ))

                 getwd()
                 system(glue::glue("mkdir {target_folder} && mv * {target_folder}"))

                 # 5. Add to git repo
                 addfiles = processx_run("git", c("add", "."))
                 processx_check(addfiles)

                 # 6. commit
                 commit = processx_run("git", c("commit", "-m'Add files to folder'"))
                 processx_check(commit)

                 # 7. Remove git history
                 getwd()
                 rmhist = processx_run("rm", c("-rf", ".git"))
                 newinit = processx_run("git", c("init"))
                 newadd = processx_run("git", c("add", "."))
                 newcommit = processx_run("git", c("commit", "-m'Adding Reviewer files'"))

                 # 8. grab author repo
                 res_aut = purrr::safely(org_repo_clone)(a, temp_wd, verbose = FALSE)
                 status_msg(res_aut,
                            fail = glue::glue("Failed to clone {usethis::ui_value(a)}."))
                 repo_aut_wd = fs::path(temp_wd, get_repo_name(a))
                 setwd(repo_aut_wd)

                 ## Copy original file
                 system(glue::glue("mkdir {target_folder} && cp {path} {target_folder}"))
                 processx_run("git", c("add", "."))
                 commit = processx_run("git", c("commit", "-m'Place original file'"))

                 # 9. add remote
                 remoteadd = processx_run("git",
                                          c(
                                            "remote",
                                            "add",
                                            "modified-source",
                                            glue::glue("../../{temp_folder}/{get_repo_name(r)}")
                                          ))
                 processx_run("git", c("remote", "-v"))

                 # 10. pull remote
                 remotepull = processx_run("git",
                                           c(
                                             "pull",
                                             "modified-source",
                                             "master",
                                             "--allow-unrelated-histories"
                                           ))

                 # 11. remove remote
                 rmremote2 = processx_run("git", c("remote", "rm", "modified-source"))
                 processx_check(rmremote2)



                 # 12. Add remote
                 # newremoteadd = processx_run("git", c(
                 #   "remote",
                 #   "add",
                 #   "origin",
                 #   glue::glue(
                 #     "https://github.com/{get_repo_owner(a)}/{get_repo_name(a)}"
                 #   )
                 # ))


                 # 13. Pull upstream changes
                 pulltarget = processx_run("git",
                                           c(
                                             "pull",
                                             "origin",
                                             "master",
                                             "--allow-unrelated-histories"
                                           ))

                 # 14. push changes
                 getwd()
                 processx_run("git", c("status"))
                 pushtarget = processx_run("git", c("push", "-u", "origin", "master"))
               })

}


org_repo_clone = function(repo, local_wd, verbose = FALSE) {
  arg_is_chr_scalar(repo, local_wd)

  res = processx::run(
    require_git(),
    args  = c("clone", get_repo_url(repo)),
    error_on_status = FALSE,
    echo = verbose,
    echo_cmd = verbose,
    wd = local_wd
  )

  err_msg = res[["stderr"]]
  err_msg = sub("fatal: ", "", err_msg)
  err_msg = sub("^\\s|\\s$", "", err_msg)
  err_msg = censor_token(err_msg, suffix = "@")

  if (res[["status"]] != 0)
    stop(err_msg)

  res

}


processx_check = function(res) {
  if (res[['stderr']] != "")
    usethis::ui_oops("Command did not run")
}

processx_run = function(cmd,
                        args,
                        wd = NULL,
                        verbose = TRUE) {
  if (!is.null(wd)) {
    processx::run(
      cmd,
      args,
      error_on_status = FALSE,
      echo = verbose,
      echo_cmd = verbose,
      wd = wd
    )
  } else {
    processx::run(
      cmd,
      args,
      error_on_status = FALSE,
      echo = verbose,
      echo_cmd = verbose
    )
  }
}

processx_pwd = function(verbose = TRUE) {
  processx::run(
    "pwd",
    error_on_status = FALSE,
    echo = verbose,
    echo_cmd = verbose
  )
}

get_lastcommit_sha = function(repo, path = NULL) {
  arg_is_chr_scalar(repo)
  arg_is_chr(path, allow_null = TRUE)

  if (!is.null(path)) {
    purrr::map_dfr(path,
                   function(path) {
                     sub = get_commits(repo = repo, path = path)
                     sub = sub[order(sub$date, decreasing = TRUE), ]
                     tibble::tibble(sha = as.character(sub[1, 'sha']),
                                    path = path)
                   })
  } else {
    purrr::map_chr(repo,
                   function(.x) {
                     sub = get_commits(repo = repo)
                     sub = sub[order(sub$date, decreasing = TRUE), ]
                     as.character(sub[1, 'sha'])
                   })
  }
}
