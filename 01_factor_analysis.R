# COOP CUSTOMER SATISFACTION | FACTOR ANALYSIS
# Academic group project
# Authors: Valentina Monticelli, Lisa Rigo, Mario Spanò, Raffaella Trucchi,
#          Giovanni Zecchini
#
# The original Excel datasets are not distributed with this repository.
# The analysis cannot be reproduced without the underlying survey data.

# 1. Packages ---------------------------------------------------------------

library(readxl)
library(psych)
library(GPArotation)

# 2. Data import and checks -------------------------------------------------

# Expected input: data/coop_factor.xlsx (not included in the public repository).
# The input retains the original questionnaire column names.
factor_data_path <- file.path("data", "coop_factor.xlsx")

if (!file.exists(factor_data_path)) {
  stop("Input file not available: data/coop_factor.xlsx")
}

survey_data <- read_excel(factor_data_path)

# The 20 customer-satisfaction items were stored in columns 25–44.
satisfaction_items <- as.data.frame(survey_data[, 25:44])

stopifnot(ncol(satisfaction_items) == 20L)
stopifnot(all(vapply(satisfaction_items, is.numeric, logical(1))))

missing_values <- sum(is.na(satisfaction_items))
print(paste("Missing satisfaction responses:", missing_values))

# The original source script reported no missing values for these items.
if (missing_values > 0L) {
  stop("Missing values detected. Review the data before factor analysis.")
}

# 3. Exploratory factor analysis --------------------------------------------

# Compare alternative numbers of factors using minimum-residual extraction
# and oblimin rotation. Selection of five factors was based on interpretation
# and the group's assessment of the resulting scales, not an automated rule.

factor_solutions <- lapply(c(5L, 6L, 7L, 8L), function(k) {
  fa(satisfaction_items, nfactors = k, rotate = "oblimin", fm = "minres")
})
names(factor_solutions) <- paste0("factors_", c(5L, 6L, 7L, 8L))

# Print each solution's loadings. The cutoff changes display only.
for (solution_name in names(factor_solutions)) {
  cat("\n----", solution_name, "----\n")
  print(factor_solutions[[solution_name]], cut = 0.2975, digits = 3)
}

selected_initial <- factor_solutions[["factors_5"]]

# 4. Reliability of the interpreted dimensions -----------------------------

# Item groups reflect the interpretation documented in the original script.
# They are specified by questionnaire variable names rather than positions.

items_by_dimension <- list(
  products_and_store_layout = c(
    "Q25_CS_Quality", "Q26_CS_Availability", "Q27_CS_Assortment",
    "Q28_CS_Ready_to_eat", "Q39_CS_Shelves_org", "Q40_CS_Aisle_mark"
  ),
  staff = c(
    "Q29_CS_Pers_competence", "Q30_CS_Pers_kindness",
    "Q31_CS_Pers_presence"
  ),
  membership_program = c(
    "Q42_CS_Promotions_dur", "Q43_CS_Promotions_prod",
    "Q44_CS_Member_benefits"
  ),
  accessibility = c(
    "Q34_CS_Payment", "Q35_CS_Parking", "Q36_CS_Karts",
    "Q37_CS_Arch_barriers", "Q41_CS_Cleanliness", "Q38_CS_Opening"
  ),
  checkout = c("Q32_CS_N_counters", "Q33_CS_Self_service")
)

all_item_names <- unique(unlist(items_by_dimension))
missing_columns <- setdiff(all_item_names, names(satisfaction_items))
if (length(missing_columns) > 0L) {
  stop("Missing questionnaire variables: ", paste(missing_columns, collapse = ", "))
}

# Cronbach's alpha for the five proposed dimensions.
reliability_initial <- lapply(items_by_dimension, function(item_names) {
  psych::alpha(satisfaction_items[, item_names, drop = FALSE])
})

for (dimension in names(reliability_initial)) {
  cat("\nCronbach's alpha -", dimension, ":",
      round(reliability_initial[[dimension]]$total$raw_alpha, 3), "\n")
}

# The original analysis indicated a higher alpha for the staff dimension
# after excluding Q31_CS_Pers_presence (reported change: 0.79 to 0.84).

items_by_dimension$staff <- c("Q29_CS_Pers_competence", "Q30_CS_Pers_kindness")

# 5. Final five-factor solution ---------------------------------------------

satisfaction_items_final <- satisfaction_items[
  , setdiff(names(satisfaction_items), "Q31_CS_Pers_presence"), drop = FALSE
]

selected_final <- fa(
  satisfaction_items_final,
  nfactors = 5,
  rotate = "oblimin",
  fm = "minres"
)

cat("\n---- Final five-factor solution ----\n")
print(selected_final, cut = 0.2975, digits = 3)

reliability_final <- lapply(items_by_dimension, function(item_names) {
  psych::alpha(satisfaction_items_final[, item_names, drop = FALSE])
})

cat("\n---- Final scale reliability ----\n")
for (dimension in names(reliability_final)) {
  cat(dimension, ":",
      round(reliability_final[[dimension]]$total$raw_alpha, 3), "\n")
}

# Reported group-project interpretation: five customer-experience dimensions,
# comprising product/store layout, staff, membership, accessibility and checkout.
# Refer to the presentation for the full interpretation and conclusions.
