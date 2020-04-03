## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ExpertChoice)

## -----------------------------------------------------------------------------
library(ExpertChoice)

## -----------------------------------------------------------------------------
attri55 <- list(
  maker = c("0", "1", "2", "3", "4"),
  technical = c("0", "1", "2", "3", "4"),
  category_rarity = c("0", "1", "2", "3", "4"),
  size = c("0", "1", "2", "3", "4"),
  age = c("0", "1", "2", "3", "4")
)

## -----------------------------------------------------------------------------
attri4521 <- list(
  maker = c("0", "1", "2", "3"),
  technical = c("0", "1", "2", "3"),
  category_rarity = c("0", "1", "2", "3"),
  size = c("0", "1", "2", "3"),
  age = c("0", "1", "2", "3"),
  provenance = c("0", "1")
)

## -----------------------------------------------------------------------------
ff55 <- full_factorial(attri55)

## -----------------------------------------------------------------------------
rbind(head(ff55, 5), tail(ff55, 5))

## -----------------------------------------------------------------------------
contrasts(ff55$maker)

## -----------------------------------------------------------------------------
aff55 <- augment_levels(ff55)

## -----------------------------------------------------------------------------
aff55[sample(nrow(aff55), 10), ]

## ----load-packages, message=FALSE---------------------------------------------
#library(AlgDesign)
library(DoE.base)
# library(DoE.MIParray)

## -----------------------------------------------------------------------------
# Design: DF: 17, 32 OA (Resolution II), 64 OA (Resolution III)
nlevels <- unlist(purrr::map(ff55, function(x){length(levels(x))}))
oa_feasible(25, nlevels, strength = 2)

## -----------------------------------------------------------------------------
fractional_factorial_55_25 <- oa.design(nlevels = nlevels, columns = "min34")

## -----------------------------------------------------------------------------
# Not run because it requires time as well as some setting up if this is your first time.
# See DoE.MIParray for more details.
#fractional_factorial_55_25 <- gurobi_MIParray(25, nlevels)

## -----------------------------------------------------------------------------
head(fractional_factorial_55_25, 10)

## ----warning=FALSE------------------------------------------------------------
colnames(fractional_factorial_55_25) <- colnames(ff55)
fractional_f55_25 <- search_design(ff55, fractional_factorial_55_25)

## -----------------------------------------------------------------------------
# Check to see if the searched attribute exists on the fractional_f4521_64 object.
attributes(fractional_f55_25)$searched

## -----------------------------------------------------------------------------
DoE.base::GWLP(fractional_f55_25)

## -----------------------------------------------------------------------------
# Test for main effects
main_effects <- fractional_factorial_efficiency(~ maker + technical + category_rarity + size + age, fractional_f55_25)

## -----------------------------------------------------------------------------
names(main_effects)

## -----------------------------------------------------------------------------
# Test for main effects and interactions described in note.
#main_plus_interacts <- fractional_factorial_efficiency(~ maker * technical + category_rarity + size + age, fractional_f55_25)

## -----------------------------------------------------------------------------
dce_modulo <- modulo_method(
  fractional_f55_25,
  list(c(1, 1, 1, 1, 1), c(3, 3, 3, 3, 3))
)

## -----------------------------------------------------------------------------
checking_overshadow <- check_overshadow(dce_modulo)
# The matrix of indixes indicate that for row 1, 7, 13, 19 and 25 there are Pareto dominate solutions.
checking_overshadow

## -----------------------------------------------------------------------------
dce_modulo_efficacy <- dce_efficiency(aff55, dce_modulo)

## -----------------------------------------------------------------------------
question_table_f55 <- construct_question_frame(aff55, dce_modulo)

## -----------------------------------------------------------------------------
levels(question_table_f55$maker) <- c("common (bottom 50% of makers)", "known to specialists (50% to 65% of makers)", "recognised (65% to 80%)", "famous (80% to 90%)", "celebrated top 10%")
levels(question_table_f55$technical) <- c("below average (below 50% of craftmanship)", "good (50% to 65%)", "meritorious (65% to 80%)", "distinguished (80% to 90%)", "exquisite (top 10%)")
levels(question_table_f55$category_rarity) <- c("common (bottom 20%)", "uncommon (20% to 40%)", "rare (40% to 60%)", "very rare (60% to 80%)", "exceptional (top 20%)")
levels(question_table_f55$size) <- c("petite: under 125g", "small: between 126g and 275g", "medium: between 276g and 600g", "large: between 601g and 1200g", "extra large: exceeds 1200g")
levels(question_table_f55$age) <- c("1951-present", "1951-present", "1951-present", "1951-present", "before 1800")
# View(question_table_f4521)

## -----------------------------------------------------------------------------
question_table_f55

## -----------------------------------------------------------------------------
#Step 0
# Described in Theory
attri3261 <- list(
  starter = c("1", "2", "3"),
  main = c("1", "2", "3", "4", "5", "6"),
  dessert = c("1", "2", "3")
)
# Step 1
ff_examp <- full_factorial(attri3261)
# Step 2
aff_examp <- augment_levels(ff_examp)
#write.csv(ff_examp, "example.csv")
# Step 3
nlevels <- unlist(purrr::map(ff_examp, function(x){length(levels(x))}))
#oa_feasible(36, nlevels, strength = 3)

fractional_factorial_3261_18 <- oa.design(nlevels = nlevels, columns = "min34")
# The fractional_factorial design is generated using the DoE.MIParray package.
# The following is the command to run this generation.
# The result is saved in the package.


# Step 4
# Confirming that this is an efficient design.
colnames(fractional_factorial_3261_18) <- colnames(ff_examp)
fractional_factorial_3261_18 <- search_design(ff_examp, fractional_factorial_3261_18)

# Step 5.
# This table is reported as Table
# Confirm that this design supports all interactions.
row1_main_effects <- fractional_factorial_efficiency(~ starter + main + dessert, fractional_factorial_3261_18)

# Step 6.
# Two different card options
# Option 1
dce_modulo_examp1 <- modulo_method(
  fractional_factorial_3261_18,
  list(c(1, 0, 1), c(0,1,0))
)

# Option 2.
dce_modulo_examp2 <- modulo_method(
  fractional_factorial_3261_18,
  list(c(1, 0, 1), c(1,3,1), c(0,5,0))
)

# Step 7
# This experiment uses categorical data (not ordinal) hence there can be no pareto dominate solution.
# Each category is merely a choice.


# Step 8.
# Compare the efficiencies.
dce_efficency_menu_example1 <- dce_efficiency(aff_examp, dce_modulo_examp1)
dce_efficency_menu_example2 <- dce_efficiency(aff_examp, dce_modulo_examp2)
# Option 2 is much more efficient so let's use that version!

# Step 9
# Construct the question table
menu_question_table <- construct_question_frame(aff_examp, dce_modulo_examp2)

# Finally augment the question table. See Table 1 in the Theoretical Vignette.
levels(menu_question_table$starter) <- c("Tomato Soup", "Duck Rillettes", "Seafood Chowder")
levels(menu_question_table$main) <- c("Roast Pheasant", "Pan Fried Hake", "Pork Belly", "Mushroom Risotto", "Sirloin Steak", "Vegetable Bake")
levels(menu_question_table$dessert) <- c("Sticky Toffee Pudding", "Chocolate & Hazelnut Brownie", "Cheesecake")
#View(menu_question_table)

## -----------------------------------------------------------------------------
menu_question_table

## -----------------------------------------------------------------------------
# Step 0
atttravel <- list(
  airfaire = c("0", "1"),
  travel_time = c("0", "1", "2")
)
# Step 1
travel2131 <- full_factorial(atttravel)
# Step 2
aff_travel2131 <- augment_levels(travel2131)
# Step 3.
# The full factorial is already so small that selecting a fraction of it would be silly.
# Therefore re-use the full factorial as the fractional factorial.

# Step 4.
# Confirming that this is an efficient design.
fractional_travel2131 <- search_design(travel2131, travel2131)

# Step 5.
# Confirm that this design supports all interactions.
full_factorial_efficiacy <- fractional_factorial_efficiency(~ (airfaire + travel_time)^2, fractional_travel2131)

# Step 6 & Step 7.
# Street gives two examples of choice sets.
travel_choice_set1 <- list(c("00", "11", "02"), c("10", "02", "12"))
class(travel_choice_set1) <- c(class(travel_choice_set1), "choice_set")

travel_example <- dce_efficiency(aff_travel2131, travel_choice_set1)
# Note, if you want to rearrange the columns of the lamda matrix so that they are the same as Street use the following:
# lamda_street_cols  <- matrix(c(travel_example$Lamda$mat[,1],
#                                travel_example$Lamda$mat[,3],
#                                travel_example$Lamda$mat[,5],
#                                travel_example$Lamda$mat[,2],
#                                travel_example$Lamda$mat[,4],
#                                travel_example$Lamda$mat[,6]), ncol = 6)
# lamda_street_paper <- matrix(c(lamda_street_cols[1,],
#                                lamda_street_cols[3,],
#                                lamda_street_cols[5,],
#                                lamda_street_cols[2,],
#                                lamda_street_cols[4,],
#                                lamda_street_cols[6,]), ncol = 6)
# Street gives a second arrangement:
travel_choice_set2 <- list(c("00", "11", "02"), c("10", "01", "12"))
class(travel_choice_set2) <- c(class(travel_choice_set2), "choice_set")
# This version is 100% efficient.
travel_example2 <- dce_efficiency(aff_travel2131, travel_choice_set2)

# Step 8
travel_questions <- construct_question_frame(aff_travel2131, travel_choice_set2, randomise_choice_sets = FALSE)
levels(travel_questions$airfaire) <- c("$350", "$650")
levels(travel_questions$travel_time) <- c("4 hours", "5 hours", "6 hours")
#View(travel_questions)

