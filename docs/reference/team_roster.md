# Add team assignments to a roster

`team_roster()` adds a team (or repo) name column to a roster data frame
by randomly grouping members into teams of a target `size`. Teams may be
formed within groups (e.g. lab sections) via `by`, and names are built
from a [glue](https://glue.tidyverse.org/reference/glue.html) template
that can reference any column of `roster` together with the generated
`team_id`. When `by` is supplied and no `name` is given, the grouping
column(s) are folded into the default name so team names stay unique
across groups.

## Usage

``` r
team_roster(
  roster,
  size,
  name = NULL,
  by = NULL,
  col = "team",
  pad = TRUE,
  shuffle = TRUE,
  seed = NULL
)
```

## Arguments

- roster:

  Data frame. Course roster with one row per student.

- size:

  Integer. Target (maximum) team size. Within each group members are
  split into `ceiling(n / size)` teams of as-equal-as-possible size. An
  error is thrown if the roster (or any group defined by `by`) has fewer
  than `size` members.

- name:

  Character. A glue template for the team / repo name, used as-is if
  supplied. May reference any column of `roster` as well as `team_id`
  (the within-group team number). If `NULL` (default) the template is
  `"team{team_id}"`, or `"{group}-team{team_id}"` when `by` is set.

- by:

  Character. Optional column name(s) in `roster` to form teams within
  (e.g. a lab section). If `NULL` (default) teams are formed across the
  whole roster.

- col:

  Character. Name of the output column to add. Default `"team"`.

- pad:

  Logical or integer. Zero-padding applied to `team_id` before it is
  substituted into `name`. `TRUE` (default) pads to a consistent width
  based on the largest team number, `FALSE` leaves it as an integer, or
  supply an integer for a fixed width.

- shuffle:

  Logical. Should members be randomly assigned to teams? Default `TRUE`.

- seed:

  Integer. Optional random seed for reproducible assignments.

## Value

The `roster` data frame with `team_id` and `col` columns added.

## Examples

``` r
if (FALSE) { # \dontrun{
roster = readr::read_csv(system.file("roster.csv", package = "ghclass"))

# Teams of 4 across the whole roster -> "team01", "team02", ...
team_roster(roster, size = 4)

# Teams of 4 within each lab section, group folded into the default name
team_roster(roster, size = 4, by = "section", seed = 20250901)

# Custom name template (used exactly as written)
team_roster(
  roster, size = 4, by = "section",
  name = "hw1_lab{section}_team{team_id}", seed = 20250901
)
} # }
```
