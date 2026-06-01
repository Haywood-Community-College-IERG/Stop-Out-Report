# Libraries
library(tidyverse)
library(ccdwr)
library(janitor)

# Notes
# Get current then calculate the academic year 10 years ago and filter on that. DONE
# Then get the term start 10 years ago DONE
# and end date from last term end date. DONE
# Get last term they were here and check against Credentials to see if they graduated out
#terms then get terms you want make variable then filter on student acad cred

# Configuration to connect to the database
cfg <- getCfg()

# Current Academic Term of report pull
current_term <- getColleagueData(
  "Term_CU_Current_or_Last",
  schema = "dw_dim"
) |>
  clean_names() |>
  collect()

# Current Academic year and start/end date for data filtering
current_term_end_date <- current_term |>
  pull(term_end_date)
current_ay <- current_term |>
  pull(academic_year)
current_ay_start_date <- current_term |>
  pull(academic_year_start_date)
current_ay_end_date <- current_term |>
  pull(academic_year_end_date)
current_term_id <- current_term |>
  pull(term_id)


# Academic year start and end date for 10 years ago for data filtering
ten_years_ago_ay_start_date <- local(current_ay_start_date) - years(10)
ten_years_ago_ay_end_date <- local(current_ay_end_date) - years(10)

# All term data for set range
terms <- getColleagueData(
  "Term_CU",
  schema = "dw_dim"
) |>
  clean_names() |>
  filter(
    academic_year_start_date >= local(ten_years_ago_ay_start_date),
    academic_year_end_date <= local(current_ay_end_date)
  ) |>
  select(
    term_id,
    term_type,
    academic_year,
    term_start_date,
    term_end_date,
    academic_year_start_date,
    academic_year_end_date
  ) |>
  collect() |>
  filter(term_id != current_term_id)

ten_years_ago_term_start_date <- terms |>
  filter(academic_year_start_date == ten_years_ago_ay_start_date) |>
  pull(term_start_date) |>
  min()
ten_years_ago_term_end_date <- terms |>
  filter(academic_year_end_date == ten_years_ago_ay_end_date) |>
  pull(term_end_date) |>
  max()


program_info <- getColleagueData(
  "ACAD_PROGRAMS__USER_Current",
  schema = "history"
) |>
  rename(
    program_code = "USER.ACPG.PROGRAMS.ID",
    major_area = "USER.ACPG.MAJOR_AREA",
    current = "CurrentFlag"
  ) |>
  select(program_code, major_area, current) |>
  filter(current == "Y") |>
  collect()

# Student record of courses taken in the set date range for report (Past 10 Years)
student_acad_cred <- getColleagueData(
  "Student_Acad_Cred"
) |>
  clean_names() |>
  filter(
    stc_status %in% c("A", 'W', 'N'),
    stc_term %in% local(terms$term_id),
    stc_acad_level == "CU"
  ) |>
  collect() |>
  left_join(
    terms |> select(term_id, term_start_date, term_end_date),
    by = c("stc_term" = "term_id")
  ) |>
  select(stc_person_id, stc_term, term_start_date, term_end_date) |>
  distinct()


# Student record of graduation credentials for report (Past 10 Years)
credentials <- getColleagueData(
  "Acad_Credentials"
) |>
  clean_names() |>
  filter(acad_institutions_id == '0019844') |>
  collect() |>
  select(acad_person_id, acad_term) |>
  distinct()

# Students who have taken courses in the past 10 years, but have not graduated yet.
# THIS WILL REMOVE STUDENTS WHO GRADUATED WITH ANYTHING AND DOES NOT ACCOUNT FOR THOSE THAT GRADUATE AND COME BACK.
# students <- student_acad_cred |>
#   anti_join(credentials, by = c("stc_person_id" = "acad_person_id", "stc_term" = "acad_term"))

# Student record of programs for report (Past 10 Years)
student_programs <- getColleagueData(
  "Student_Programs_V9_Data",
  schema = "local"
) |>
  filter(`Term ID` %in% local(terms$term_id)) |>
  clean_names() |>
  collect() |>
  select(
    campus_id,
    term_id,
    program_code,
    program,
    department_code,
    department,
    graduated,
    is_primary_program,
    is_ccp_student,
    is_high_school
  )

programs <- student_programs |>
  left_join(program_info, by = "program_code") |>
  select(
    campus_id,
    term_id,
    program_code,
    major_area,
    graduated,
    is_primary_program,
    is_ccp_student,
    is_high_school
  )

# Final data frame of students who have taken courses in the past 10 years, but have not graduated yet.
students_with_programs <- student_acad_cred |>
  left_join(
    programs,
    by = c("stc_person_id" = "campus_id", "stc_term" = "term_id")
  )

students <- student_acad_cred |>
  group_by(stc_person_id) |>
  summarize(
    true_last_end_date = max(term_end_date, na.rm = TRUE),
    .groups = "drop"
  )

student_program_summary <- students_with_programs |>
  group_by(
    stc_person_id,
    program_code,
    major_area
  ) |>
  summarize(
    first_start_date = min(term_start_date, na.rm = TRUE),
    last_end_date = max(term_end_date, na.rm = TRUE),
    graduated = any(graduated == "Yes"),
    ever_primary = any(is_primary_program == "Y"),
    ever_ccp = any(is_ccp_student == "Y"),
    ever_high_school = any(is_high_school == "Y"),
    .groups = "drop"
  ) |>
  filter(last_end_date != current_term_end_date) |>
  left_join(
    terms |> select(term_id, academic_year, term_end_date),
    by = c("last_end_date" = "term_end_date")
  ) |>
  left_join(
    students,
    by = c("stc_person_id")
  ) |>
  filter(true_last_end_date == last_end_date)

student_summary <- student_program_summary |>
  group_by(stc_person_id) |>
  summarize(
    graduated_from_any_program = any(graduated == TRUE),
    graduated_from_all_program = all(graduated == TRUE),
    .groups = "drop"
  )


major_area_summary <- student_program_summary |>
  group_by(stc_person_id, major_area) |>
  summarize(
    graduated_from_any_major = any(graduated == TRUE),
    graduated_from_all_major = all(graduated == TRUE),
    .groups = "drop"
  )

df <- student_program_summary |>
  left_join(student_summary, by = "stc_person_id") |>
  left_join(major_area_summary, by = c("stc_person_id", "major_area")) |>
  filter(graduated_from_all_program == FALSE)


student_pii <- getColleagueData(
  "Demographics_V5_Data",
  schema = "local"
) |>
  clean_names() |>
  select(
    campus_id,
    first_name,
    last_name,
    campus_email_address,
    secondary_email_address,
    personal_phone_number
  ) |>
  collect()
