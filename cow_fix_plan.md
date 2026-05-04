# Fix plan: cow column bugs across Munro and Corbett blog posts

This plan describes all the changes needed to fix the cow-related bugs in the two blog posts and their supporting scripts. It is intended to be executed by Sonnet in order, and each step includes the exact file, the problem, and the fix.

## Context

There are two blog posts and two scraping scripts involved:

- **Munro post**: `post/2025-08-31-munro-tidy-tuesday/index.en.qmd`
- **Munro scrape**: `post/2025-08-31-munro-tidy-tuesday/scraping script.R`
- **Corbett post**: `post/2025-09-25-corbetts/corbetts.qmd`
- **Corbett scrape**: `post/2025-09-25-corbetts/corbett_scraping.R`
- **Patch file (to delete)**: `post/patch_cow_column.R`

Both scraping scripts already include `cow` in `extract_flags_time_distance()`, so a fresh scrape will produce CSVs with a native `cow` column. The patch file is therefore redundant.

## Step 1: Re-run the Munro scrape

Run `post/2025-08-31-munro-tidy-tuesday/scraping script.R` in R. This will regenerate `walkhighlands.csv` with the `cow` column included natively. This takes a while because of the polite delays between HTTP requests (~0.2-0.6s per page, ~282 Munros plus rating/ascent pages).

No code changes are needed to the scraping script itself. It already scrapes `cow` correctly.

## Step 2: Re-run the Corbett scrape

Run `post/2025-09-25-corbetts/corbett_scraping.R` in R. This will regenerate `walkhighlands_corbetts.csv` with the `cow` column included natively. Same timing considerations as Step 1 (~220 Corbetts).

**Important:** The Corbett scraping script writes output to the current working directory (`write_csv(walkhighlands_corbetts, file = "walkhighlands_corbetts.csv")` at line 251). Make sure R's working directory is set to `post/2025-09-25-corbetts/` before running, or change line 251 to use the full path. Alternatively, after running, copy the output CSV to `post/2025-09-25-corbetts/walkhighlands_corbetts.csv`.

## Step 3: Fix the Munro QMD cow plot (Bug 1 — wrong column name)

**File:** `post/2025-08-31-munro-tidy-tuesday/index.en.qmd`  
**Line:** 944  
**Problem:** `filter(munros_coords, cows == TRUE)` — the column is called `cow`, not `cows`.  
**Fix:** Change `cows` to `cow`.

Before:
```r
  geom_jitter(data = filter(munros_coords, cows == TRUE),
```

After:
```r
  geom_jitter(data = filter(munros_coords, cow == TRUE),
```

## Step 4: Fix the Corbett QMD cow plot (Bug 2 — wrong data object AND wrong column name)

**File:** `post/2025-09-25-corbetts/corbetts.qmd`  
**Lines:** 907-909  
**Problem:** Two errors on this line. It references `munros_coords` (which does not exist in the Corbett environment) and uses `cows` (the column is `cow`).  
**Fix:** Change `munros_coords` to `corbett_coords` and `cows` to `cow`.

Before:
```r
  geom_jitter(data = filter(munros_coords, cows == TRUE),
             aes(x = longitude,
                 y = latitude,
                 colour = cow,
```

After:
```r
  geom_jitter(data = filter(corbett_coords, cow == TRUE),
             aes(x = longitude,
                 y = latitude,
                 colour = cow,
```

## Step 5: Remove the defensive cow fallback from the Munro QMD

**File:** `post/2025-08-31-munro-tidy-tuesday/index.en.qmd`  
**Lines:** 225-228  
**Problem:** This code exists as a safety net for when `walkhighlands.csv` did not have a `cow` column. After re-scraping (Step 1), the column will always be present, so this code is unnecessary and masks potential data issues.

Delete these lines:
```r
# Ensure cow column exists and has no NAs.
# The column is absent if walkhighlands.csv pre-dates patch_cow_column.R.
if (!"cow" %in% names(munro_dat)) munro_dat$cow <- FALSE
munro_dat <- munro_dat |> mutate(cow = ifelse(is.na(cow), FALSE, cow))
```

## Step 6: Remove the defensive cow fallback from the Corbett QMD

**File:** `post/2025-09-25-corbetts/corbetts.qmd`  
**Lines:** 176-179  
**Problem:** Same as Step 5 but for the Corbett post.

Delete these lines:
```r
# Ensure cow column exists and has no NAs.
# The column is absent if walkhighlands_corbetts.csv pre-dates patch_cow_column.R.
if (!"cow" %in% names(corbett_dat)) corbett_dat$cow <- FALSE
corbett_dat <- corbett_dat |> mutate(cow = ifelse(is.na(cow), FALSE, cow))
```

## Step 7: Delete the patch file

**File:** `post/patch_cow_column.R`  
**Action:** Delete this file entirely. It is no longer needed since both scraping scripts produce the `cow` column natively.

## Step 8: Verify the Corbett QMD cow section text says "toggle the graph"

**File:** `post/2025-09-25-corbetts/corbetts.qmd`  
**Line:** 901  
**Note:** The text reads "You can toggle the graph" which is a leftover fragment that does not make sense in context (it is a static plotly map, not a toggleable chart). Consider removing this sentence or replacing it with something more appropriate. This is a minor editorial note, not a bug.

## Step 9: Re-render both QMDs

After all changes are made:

1. Open `post/2025-08-31-munro-tidy-tuesday/index.en.qmd` in RStudio and render it. Confirm the cow map appears correctly under "Other features > Cows".
2. Open `post/2025-09-25-corbetts/corbetts.qmd` in RStudio and render it. Confirm the cow map appears correctly under "Cows".

## Summary of changes

| File | Action |
|---|---|
| `scraping script.R` | Re-run (no code changes) |
| `corbett_scraping.R` | Re-run (no code changes) |
| `index.en.qmd` line 944 | `cows` → `cow` |
| `index.en.qmd` lines 225-228 | Delete defensive fallback |
| `corbetts.qmd` line 907 | `munros_coords` → `corbett_coords`, `cows` → `cow` |
| `corbetts.qmd` lines 176-179 | Delete defensive fallback |
| `corbetts.qmd` line 901 | Remove "You can toggle the graph" (editorial) |
| `patch_cow_column.R` | Delete file |
