## Tools for managing github class organization accounts

* Step 1 - validating student provided accounts

```
# Provide a csv file with a column containing the account names
./check_accounts.R example/teams.csv github
```

* Step 2 - invite students

```
./invite_accounts.R class_org example/teams.csv github
```