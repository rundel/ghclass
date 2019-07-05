library(dplyr)

googlesheets::gs_auth(cache = FALSE)
s = googlesheets::gs_title("ghclass test accounts")
d = googlesheets::gs_read(s, verbose = FALSE)

users = d$github_username
pats = d$github_pat %>% setNames(users)

stopifnot(Sys.getenv("GHCLASS_PAT") != "")

enc = sodium::data_encrypt(
  serialize(pats, NULL),
  sodium::sha256(charToRaw(Sys.getenv("GHCLASS_PAT")))
)

saveRDS(enc, here::here("credentials/example_users.enc.rds"))


