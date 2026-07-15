# Stop-Out Dashboard

An R Shiny app for identifying and filtering stop-out students at Haywood Community College. It pulls live data from the IERG data warehouse covering the past 10 academic years and lets staff slice the results by program, term, graduation status, and student type.

## What is a "stop-out"?

A stop-out is a student who enrolled in a program at some point in the past 10 years but whose last active term was not the current term — and who has not graduated from all of their programs. These are students who started but did not finish and are not currently enrolled.

## How it works

**`cleaning.r`** runs automatically when the app launches. It:

1. Connects to the IERG data warehouse via `ccdwr`.
2. Determines the current term and calculates a 10-year lookback window.
3. Pulls course enrollment records (`Student_Acad_Cred`), program enrollment records (`Student_Programs_V9_Data`), graduation credentials (`Acad_Credentials`), and program metadata (`ACAD_PROGRAMS__USER_Current`). Program enrollment records include both the program code and its full program name, which is more descriptive than the broader major area.
4. Builds a per-student-per-program summary that flags:
   - Whether the student graduated from that specific program.
   - Whether they graduated from any program or any major area.
   - Whether their last term in that program was also their last term at the college overall.
5. Excludes any student who has graduated from all of their programs (those are completers, not stop-outs).

**`app.r`** renders the dashboard with the processed data.

## Filters

| Filter | Description |
|---|---|
| Major Area | Broad discipline area (e.g., Business, Health Sciences) |
| Program Code | Specific program (cascades from Major Area selection) |
| Academic Year | Filters by the academic year of the student's last term (cascades to Term ID) |
| Term ID | Specific term of the student's last enrollment |
| Graduated (Program Level) | Whether the student graduated from that specific program |
| Graduated from Any Program | Whether the student ever graduated from any program at HCC |
| Graduated from Any Major | Whether the student ever graduated from any program in that major area |
| Graduated from All Majors | Whether the student graduated from every program they held in that major area |
| Remove CCP Students | Excludes students who were ever enrolled as CCP (College & Career Promise) |
| Remove HS Students | Excludes students who were ever flagged as high school students |
| Primary Programs Only | Limits to records where the program was ever the student's primary program |

Selecting an Academic Year automatically narrows the Term ID choices. Selecting a Major Area automatically narrows the Program Code choices. The **Clear All Filters** button resets everything.

## Summary cards

Four value boxes at the top update in real time as filters change:

- **Stop-Out Records** — total rows in the filtered dataset (one row per student per program)
- **Unique Students** — count of distinct student IDs
- **CCP Students** — unique students who were ever enrolled as CCP
- **High School Students** — unique students who were ever flagged as high school students

## Data table

The table shows one record per student per program for the student's last active term, including the Program Code, its full Program name, and the broader Major Area. Columns are color-coded for quick scanning: CCP, High School, Primary Program, and all graduation flags are highlighted when `TRUE`.

## Download

The **Download CSV** button exports the filtered records joined with student PII (name, HCC email, secondary email, phone number) from the `Demographics_V5_Data` view. The file is named `stop_out_report_<date>.csv`.

## Requirements

- R with the following packages: `shiny`, `bslib`, `bsicons`, `tidyverse`, `DT`, `ccdwr`, `janitor`
- Network access to the HCC IERG data warehouse
- `ccdwr` configured with valid warehouse credentials (`getCfg()`)

## Running the app

```r
shiny::runApp("stop_outs_shiny_app")
```

Or open `app.r` in RStudio and click **Run App**. `cleaning.r` will execute automatically on startup — allow a moment for the warehouse queries to complete before the UI loads.
