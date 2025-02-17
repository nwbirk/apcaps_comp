# DXA TANITA Permutation test
# Per the suggestions of one reviewer
# Data processing will be the same, 
# Model fitting also generally the same
# But outcome values will randomly be permuted

library(readstata13)
library(tidyverse)
library(mice)
library(caret)
library(ranger)
library(glmnet)
library(Metrics)
library(blandr)
library(gridExtra)


# Import data
wave3 <- read.dta13('~/Downloads/3FU_for share_Nick.dta')
wave3_plus <- read.dta13('~/Downloads/extract_forNick_070721.dta')

wave3 <- left_join(wave3, wave3_plus, by = 'ha1id')

# DXA Variables
summary(wave3$ha1q34_10al1l4_fat1) #   L1-L4 fat Reading1 (g)
summary(wave3$ha1q34_10bl1l4_lean1) #   L1-L4 lean Reading1 (g)
summary(wave3$ha1q34_10cl1l4_mass1) #   L1-L4 mass Reading1 (g)
summary(wave3$ha1q34_10dl1l4_pcent1) #   L1-L4 % body fat Reading1 (g)
summary(wave3$ha1q34_11al2l4_fat1) #   L2-L4 fat Reading1 (g)
summary(wave3$ha1q34_11bl2l4_lean1)
summary(wave3$ha1q34_11cl2l4_mass1) # MAX here seems like a data entry error. 
summary(wave3$ha1q34_11dl2l4_pcent1)
summary(wave3$ha1q34_12al1l4_fat1) #   L1-L4 fat Reading2 (g)
summary(wave3$ha1q34_12bl1l4_lean1)
summary(wave3$ha1q34_12cl1l4_mass1)
summary(wave3$ha1q34_12dl1l4_pcent1)
summary(wave3$ha1q34_13al2l4_fat1) #   L2-L4 fat Reading2 (g)
summary(wave3$ha1q34_13bl2l4_lean1)
summary(wave3$ha1q34_13cl2l4_mass1)
summary(wave3$ha1q34_13dl2l4_pcent1)

summary(wave3$ha1q34_4alarm_fat) #   Left arm fat (g)
summary(wave3$ha1q34_4blarm_lean) #   Left arm lean (g)
summary(wave3$ha1q34_4clarm_mass) #   Left arm mass (g)
summary(wave3$ha1q34_4dlarm_pcent_fat) #   Left arm % body fat
summary(wave3$ha1q34_5ararm_fat)
summary(wave3$ha1q34_5brarm_lean)
summary(wave3$ha1q34_5crarm_mass)
summary(wave3$ha1q34_5drarm_pcent_fat)
summary(wave3$ha1q34_6atrunk_fat)
summary(wave3$ha1q34_6btrunk_lean)
summary(wave3$ha1q34_6ctrunk_mass)
summary(wave3$ha1q34_6dtrunk_pcent_fat)
summary(wave3$ha1q34_7alleg_fat)
summary(wave3$ha1q34_7blleg_lean)
summary(wave3$ha1q34_7clleg_mass) # I suspect typos here?
summary(wave3$ha1q34_7dlleg_pcent_fat)
summary(wave3$ha1q34_8arleg_fat)
summary(wave3$ha1q34_8brleg_lean)
summary(wave3$ha1q34_8crleg_mass)
summary(wave3$ha1q34_8drleg_pcent_fat)
summary(wave3$ha1q34_9atotal_fat)
summary(wave3$ha1q34_9btotal_lean)
summary(wave3$ha1q34_9ctotal_mass)
summary(wave3$ha1q34_9dtotal_pcent_fat)

# 3718 people have data for both variables (TANITA and DXA)
table(is.na(wave3$ha1q37_10i_seg_la_pcent), is.na(wave3$ha1q34_10al1l4_fat1))


# will build on this paper:
# https://www.researchgate.net/publication/255959062_Development_and_validation_of_anthropometric_prediction_equations_for_estimation_of_lean_body_mass_and_appendicular_lean_soft_tissue_in_Indian_men_and_women
# "DXA-measured LBM and ALST estimates as dependent variables"
# (LBM = lean body mass and ALST = appendicular lean soft tissue)
# Their unit was kg, so total mass it seems
# ALST is used for sarcopenia definition, approximates skeletal mass


# TANITA Variables
summary(wave3$ha1q37_10i_seg_la_pcent) #   Segmental analysis: Left Arm: Fat percentage(%)
summary(wave3$ha1q37_10ii_seg_la_mass) #   Segmental analysis: Left Arm: Fat mass Kg
summary(wave3$ha1q37_10iii_seg_la_free_mass) #   Segmental analysis: Left Arm: Fat free mass Kg
summary(wave3$ha1q37_10iv_seg_la_muscle) #   Segmental analysis: Left Arm: Pred muscle mass Kg
summary(wave3$ha1q37_11i_seg_tr_pcent) #   Segmental analysis: Trunk: Fat percentage(%)
summary(wave3$ha1q37_11ii_seg_tr_mass) #   Segmental analysis: Trunk: Fat mass Kg
summary(wave3$ha1q37_11iii_seg_tr_free_mass) #   Segmental analysis: Trunk: Fat free mass Kg
summary(wave3$ha1q37_11iv_seg_tr_muscle) #   Segmental analysis: Trunk: Pred muscle mass Kg
summary(wave3$ha1q37_2_weight)
summary(wave3$ha1q37_3_bmi)
summary(wave3$ha1q37_4a_bmr_kj)
summary(wave3$ha1q37_4b_bmr_kcal)
summary(wave3$ha1q37_5i_tbf_pcent) #   Total Body Fat: Fat percentage(%)
summary(wave3$ha1q37_5ii_tbf_mass) #   Total Body Fat: Fat mass Kg
summary(wave3$ha1q37_5iii_tbf_free_mass) #   Total Body Fat: Fat free mass Kg
summary(wave3$ha1q37_5iv_tbf_water) #   Total Body Fat: Total body water Kg
summary(wave3$ha1q37_6i_imp_whole_body)
summary(wave3$ha1q37_6ii_imp_rleg)
summary(wave3$ha1q37_6iii_imp_lleg)
summary(wave3$ha1q37_6iv_imp_rarm)
summary(wave3$ha1q37_6v_imp_larm)
summary(wave3$ha1q37_7i_seg_rl_pcent) #   Segmental analysis: Right Leg: Fat percentage(%)
summary(wave3$ha1q37_7ii_seg_rl_mass) #   Segmental analysis: Right Leg: Fat mass Kg
summary(wave3$ha1q37_7iii_seg_rl_free_mass) #   Segmental analysis: Right Leg: Fat free mass Kg
summary(wave3$ha1q37_7iv_seg_rl_muscle) #   Segmental analysis: Right Leg: Pred muscle mass Kg
summary(wave3$ha1q37_8i_seg_ll_pcent)
summary(wave3$ha1q37_8ii_seg_ll_mass)
summary(wave3$ha1q37_8iii_seg_ll_free_mass)
summary(wave3$ha1q37_8iv_seg_ll_muscle)
summary(wave3$ha1q37_9i_seg_ra_pcent)
summary(wave3$ha1q37_9ii_seg_ra_mass)
summary(wave3$ha1q37_9iii_seg_ra_free_mass)
summary(wave3$ha1q37_9iv_seg_ra_muscle)

# Circumferences
summary(wave3$ha1q25_10acalf_circ1)
summary(wave3$ha1q25_10bcalf_circ2)
summary(wave3$ha1q25_11ahead_circ1)
summary(wave3$ha1q25_11bhead_circ2)
summary(wave3$ha1q25_11iachest_i_circ1) #   Chest circumference at end of inspiration1(mm)
summary(wave3$ha1q25_11ibchest_i_circ2)
summary(wave3$ha1q25_11iiachest_e_circ1) #   Chest circumference at end of expiration(mm)
summary(wave3$ha1q25_11iibchest_e_circ2)
summary(wave3$ha1q25_7awaist_circ1)
summary(wave3$ha1q25_7bwaist_circ2)
summary(wave3$ha1q25_8ahip_circ1)
summary(wave3$ha1q25_8bhip_circ2)
summary(wave3$ha1q25_9aarm_circ1)
summary(wave3$ha1q25_9barm_circ2)

# Skin Folds
summary(wave3$ha1q25_12atricep1)
summary(wave3$ha1q25_12btricep2)
summary(wave3$ha1q25_12ctricep3)
summary(wave3$ha1q25_13abicep1)
summary(wave3$ha1q25_13bbicep2)
summary(wave3$ha1q25_13cbicep3)
summary(wave3$ha1q25_14asubscap1)
summary(wave3$ha1q25_14bsubscap2)
summary(wave3$ha1q25_14csubscap3)
summary(wave3$ha1q25_15asupra1)
summary(wave3$ha1q25_15bsupra2)
summary(wave3$ha1q25_15csupra3)
summary(wave3$ha1q25_16acalf1)
summary(wave3$ha1q25_16bcalf2)
summary(wave3$ha1q25_16ccalf3)

# Grip strength
summary(wave3$ha1q25_18hand1_strgthr)
summary(wave3$ha1q25_18hand2_strgthr)
summary(wave3$ha1q25_18hand3_strgthr)
summary(wave3$ha1q25_18hand4_strgthr)
summary(wave3$ha1q25_19hand1_strgthl)
summary(wave3$ha1q25_19hand2_strgthl) # Some VERY high values here? 1-3, but not 4.
summary(wave3$ha1q25_19hand3_strgthl)
summary(wave3$ha1q25_19hand4_strgthl)
table(wave3$ha1q25_20dom_hand) # 1 = right, 2 = left

# Standing height
summary(wave3$ha1q25_3astand_height1)
summary(wave3$ha1q25_3bstand_height2)


# Next steps are as follows:

# First, split into training and testing

# To split the data, should filter out the records with NO data
# As these people are not part of the study
table(is.na(wave3$ha1q5_5sex)) # 7460 NA, 6928 have data

# Also exclude people younger than 18? 
# AND in this case, exclude NA from the variables we are using

# THIS IS THE SLIGHTLY MODIFIED SCRIPT WHERE WE USE AVERAGE OF 2 L1-L4
# CORRECT NOW TO AVOID ISSUES LATER
wave3 <- wave3 %>% mutate(ha1q34_12dl1l4_pcent1 = (ha1q34_10dl1l4_pcent1 + ha1q34_12dl1l4_pcent1)/2)

our_variables <- c('ha1q34_9atotal_fat', 'ha1q34_9btotal_lean', 'ha1q34_9dtotal_pcent_fat',
                   'ha1q34_6dtrunk_pcent_fat', 
                   'ha1q34_12dl1l4_pcent1',
                   'ha1q34_4alarm_fat', 'ha1q34_4blarm_lean', 'ha1q34_5ararm_fat', 'ha1q34_5brarm_lean',
                   'ha1q34_7alleg_fat', 'ha1q34_7blleg_lean', 'ha1q34_8arleg_fat', 'ha1q34_8brleg_lean',
                   
                   # Predictors
                   'ha1q37_10i_seg_la_pcent', 
                   'ha1q37_10ii_seg_la_mass', 'ha1q37_10iii_seg_la_free_mass', 'ha1q37_10iv_seg_la_muscle', 
                   'ha1q37_11i_seg_tr_pcent', 'ha1q37_11ii_seg_tr_mass', 'ha1q37_11iii_seg_tr_free_mass', 
                   'ha1q37_11iv_seg_tr_muscle', 'ha1q37_2_weight', 'ha1q37_3_bmi', 'ha1q37_4a_bmr_kj', 
                   'ha1q37_4b_bmr_kcal', 'ha1q37_5i_tbf_pcent', 'ha1q37_5ii_tbf_mass', 
                   'ha1q37_5iii_tbf_free_mass', 'ha1q37_5iv_tbf_water', 'ha1q37_6i_imp_whole_body', 
                   'ha1q37_6ii_imp_rleg', 'ha1q37_6iii_imp_lleg', 'ha1q37_6iv_imp_rarm', 
                   'ha1q37_6v_imp_larm', 'ha1q37_7i_seg_rl_pcent', 'ha1q37_7ii_seg_rl_mass', 
                   'ha1q37_7iii_seg_rl_free_mass', 'ha1q37_7iv_seg_rl_muscle', 'ha1q37_8i_seg_ll_pcent', 
                   'ha1q37_8ii_seg_ll_mass', 'ha1q37_8iii_seg_ll_free_mass', 'ha1q37_8iv_seg_ll_muscle', 
                   'ha1q37_9i_seg_ra_pcent', 'ha1q37_9ii_seg_ra_mass', 'ha1q37_9iii_seg_ra_free_mass', 
                   'ha1q37_9iv_seg_ra_muscle', 'ha1q25_10acalf_circ1', 'ha1q25_10bcalf_circ2', 
                   'ha1q25_11ahead_circ1', 'ha1q25_11bhead_circ2', 'ha1q25_11iachest_i_circ1', 
                   'ha1q25_11ibchest_i_circ2', 'ha1q25_11iiachest_e_circ1', 'ha1q25_11iibchest_e_circ2', 
                   'ha1q25_7awaist_circ1', 'ha1q25_7bwaist_circ2', 'ha1q25_8ahip_circ1', 
                   'ha1q25_8bhip_circ2', 'ha1q25_9aarm_circ1', 'ha1q25_9barm_circ2', 'ha1q25_12atricep1',
                   'ha1q25_12btricep2', 'ha1q25_12ctricep3', 'ha1q25_13abicep1', 'ha1q25_13bbicep2', 
                   'ha1q25_13cbicep3', 'ha1q25_14asubscap1', 'ha1q25_14bsubscap2', 'ha1q25_14csubscap3', 
                   'ha1q25_15asupra1', 'ha1q25_15bsupra2', 'ha1q25_15csupra3', 'ha1q25_16acalf1', 
                   'ha1q25_16bcalf2', 'ha1q25_16ccalf3', 'ha1q25_18hand1_strgthr', 'ha1q25_18hand2_strgthr', 
                   'ha1q25_18hand3_strgthr', 'ha1q25_18hand4_strgthr', 'ha1q25_19hand1_strgthl', 
                   'ha1q25_19hand2_strgthl', 'ha1q25_19hand3_strgthl', 'ha1q25_19hand4_strgthl', 'ha1q25_20dom_hand',
                   'ha1q25_3astand_height1', 'ha1q25_3bstand_height2')

wave3 <- wave3 %>% filter(across(c(ha1q5_5sex, our_variables), ~ !is.na(.)))
wave3 <- wave3 %>% filter(ha1dv_age >= 18)


# Filter out for data quality issues
wave3 <- wave3 %>% filter(!(ha1q25_11ahead_circ1 < 450 | ha1q25_11bhead_circ2 < 450))
wave3 <- wave3 %>% filter(ha1q25_18hand1_strgthr < 100 & 
                            ha1q25_18hand2_strgthr < 100 &
                            ha1q25_18hand3_strgthr < 100 &
                            ha1q25_18hand4_strgthr < 100 &
                            ha1q25_19hand1_strgthl < 100 &
                            ha1q25_19hand2_strgthl < 100 &
                            ha1q25_19hand3_strgthl < 100 &
                            ha1q25_19hand4_strgthl < 100)

wave3 <- wave3 %>% filter(ha1q37_5i_tbf_pcent < 50 & ha1q34_9dtotal_pcent_fat < 50)

wave3 <- wave3 %>% filter((abs(ha1q37_2_weight - ha1q37_5ii_tbf_mass - ha1q37_5iii_tbf_free_mass) < 1))

# muscle cannot be greater than fat free mass
wave3 <- wave3 %>% filter(ha1q37_11iv_seg_tr_muscle <= ha1q37_11iii_seg_tr_free_mass)
wave3 <- wave3 %>% filter(ha1q37_10iv_seg_la_muscle <= ha1q37_10iii_seg_la_free_mass)
wave3 <- wave3 %>% filter(ha1q37_9iv_seg_ra_muscle <= ha1q37_9iii_seg_ra_free_mass)
wave3 <- wave3 %>% filter(ha1q37_8iv_seg_ll_muscle <= ha1q37_8iii_seg_ll_free_mass)
wave3 <- wave3 %>% filter(ha1q37_7iv_seg_rl_muscle <= ha1q37_7iii_seg_rl_free_mass)

# component sum cannot be very different from total
wave3 <- wave3 %>% filter(abs(ha1q37_5ii_tbf_mass - ha1q37_7ii_seg_rl_mass - ha1q37_8ii_seg_ll_mass - ha1q37_9ii_seg_ra_mass - ha1q37_10ii_seg_la_mass - ha1q37_11ii_seg_tr_mass) < 1)

wave3 <- wave3 %>% filter(abs(ha1q37_5iii_tbf_free_mass - ha1q37_7iii_seg_rl_free_mass - ha1q37_8iii_seg_ll_free_mass - ha1q37_9iii_seg_ra_free_mass - ha1q37_10iii_seg_la_free_mass - ha1q37_11iii_seg_tr_free_mass) < 1)

wave3 <- wave3 %>% filter(abs(ha1q34_9atotal_fat - ha1q34_8arleg_fat - ha1q34_7alleg_fat - ha1q34_6atrunk_fat - ha1q34_5ararm_fat - ha1q34_4alarm_fat) < 2000)

wave3 <- wave3 %>% filter(abs(ha1q34_9btotal_lean - ha1q34_8brleg_lean - ha1q34_7blleg_lean - ha1q34_6btrunk_lean - ha1q34_5brarm_lean - ha1q34_4blarm_lean) < 5000)

wave3 <- wave3 %>% filter(abs(ha1q34_9ctotal_mass - ha1q34_8crleg_mass - ha1q34_7clleg_mass - ha1q34_6ctrunk_mass - ha1q34_5crarm_mass - ha1q34_4clarm_mass) < 7000)

# body cannot be less than 10% water
wave3 <- wave3 %>% filter(ha1q37_5iv_tbf_water > 10)

# some people have highly different left and right limb values
wave3 <- wave3 %>% filter(abs(wave3$ha1q37_7iv_seg_rl_muscle - wave3$ha1q37_8iv_seg_ll_muscle) < 5)

# extreme impedance values
wave3 <- wave3 %>% filter(ha1q37_6i_imp_whole_body > 100)
wave3 <- wave3 %>% filter(ha1q37_6v_imp_larm < 1000)
wave3 <- wave3 %>% filter(ha1q37_6iv_imp_rarm < 1000) # no arms less than 100
# no legs less than 100 or above 1000

# Difference in weight between scale and Tanita OR DXA
# Cannot be more than 3kg
wave3 <- wave3 %>% mutate(newweight = (ha1q25_1aweight1 + ha1q25_1bweight2)/2) %>% 
  filter(abs(newweight - ha1q34_9ctotal_mass/1000) < 3 & abs(newweight - ha1q37_2_weight) < 3)


# SPLIT DATA

set.seed(92)

# Let's try an 80/20 split
train_ids <- sample(1:nrow(wave3), size = round(0.8*nrow(wave3)))

dxa_train <- wave3[train_ids,]
dxa_test <- wave3[-train_ids,]

# add 'ha1q5_5sex' and age

dxa_train <- dxa_train %>% select(all_of(our_variables), 'ha1q5_5sex', 'ha1dv_age')
dxa_test <- dxa_test %>% select(all_of(our_variables), 'ha1q5_5sex', 'ha1dv_age')

# Separate into men and women

dxa_train_male <- dxa_train %>% filter(ha1q5_5sex == 1)
dxa_test_male <- dxa_test %>% filter(ha1q5_5sex == 1)

dxa_train_female <- dxa_train %>% filter(ha1q5_5sex == 2)
dxa_test_female <- dxa_test %>% filter(ha1q5_5sex ==2)

# Generate function to do this for all datasets
avg_measures <- function(data){
  data <- data %>% 
    mutate(calf_circ = (ha1q25_10acalf_circ1 + ha1q25_10bcalf_circ2)/2,
           head_circ = (ha1q25_11ahead_circ1 +ha1q25_11bhead_circ2)/2,
           chest_circ = (ha1q25_11iachest_i_circ1 + ha1q25_11ibchest_i_circ2)/2,
           cheste_circ = (ha1q25_11iiachest_e_circ1 + ha1q25_11iibchest_e_circ2)/2,
           waist_circ = (ha1q25_7awaist_circ1 + ha1q25_7bwaist_circ2)/2,
           hip_circ = (ha1q25_8ahip_circ1 + ha1q25_8bhip_circ2)/2,
           arm_circ = (ha1q25_9aarm_circ1 + ha1q25_9barm_circ2)/2, # remove?
           tri_fold = (ha1q25_12atricep1 + ha1q25_12btricep2 + ha1q25_12ctricep3)/3,
           bi_fold = (ha1q25_13abicep1 + ha1q25_13bbicep2 + ha1q25_13cbicep3)/3,
           sub_fold = (ha1q25_14asubscap1 + ha1q25_14bsubscap2 + ha1q25_14csubscap3)/3,
           sup_fold = (ha1q25_15asupra1 + ha1q25_15bsupra2 + ha1q25_15csupra3)/3,
           calf2_fold = (ha1q25_16acalf1 + ha1q25_16bcalf2 + ha1q25_16ccalf3)/3,
           rhand_strength = (ha1q25_18hand1_strgthr + ha1q25_18hand2_strgthr + ha1q25_18hand3_strgthr + ha1q25_18hand4_strgthr)/4,
           lhand_strength = (ha1q25_19hand1_strgthl + ha1q25_19hand2_strgthl + ha1q25_19hand3_strgthl + ha1q25_19hand4_strgthl)/4,
           height = (ha1q25_3astand_height1 + ha1q25_3bstand_height2)/2,
           
           # ALSO GENERATE APPENDICULAR VALUES HERE
           appendic_fat = ha1q34_4alarm_fat + ha1q34_5ararm_fat + ha1q34_7alleg_fat + ha1q34_8arleg_fat,
           appendic_lean = ha1q34_4blarm_lean + ha1q34_5brarm_lean + ha1q34_7blleg_lean + ha1q34_8brleg_lean) %>%
    select(-c('ha1q25_10acalf_circ1', 'ha1q25_10bcalf_circ2', 
              'ha1q25_11ahead_circ1', 'ha1q25_11bhead_circ2', 'ha1q25_11iachest_i_circ1', 
              'ha1q25_11ibchest_i_circ2', 'ha1q25_11iiachest_e_circ1', 'ha1q25_11iibchest_e_circ2', 
              'ha1q25_7awaist_circ1', 'ha1q25_7bwaist_circ2', 'ha1q25_8ahip_circ1', 
              'ha1q25_8bhip_circ2', 'ha1q25_9aarm_circ1', 'ha1q25_9barm_circ2', 'ha1q25_12atricep1',
              'ha1q25_12btricep2', 'ha1q25_12ctricep3', 'ha1q25_13abicep1', 'ha1q25_13bbicep2', 
              'ha1q25_13cbicep3', 'ha1q25_14asubscap1', 'ha1q25_14bsubscap2', 'ha1q25_14csubscap3', 
              'ha1q25_15asupra1', 'ha1q25_15bsupra2', 'ha1q25_15csupra3', 'ha1q25_16acalf1', 
              'ha1q25_16bcalf2', 'ha1q25_16ccalf3', 'ha1q25_18hand1_strgthr', 'ha1q25_18hand2_strgthr', 
              'ha1q25_18hand3_strgthr', 'ha1q25_18hand4_strgthr', 'ha1q25_19hand1_strgthl', 
              'ha1q25_19hand2_strgthl', 'ha1q25_19hand3_strgthl', 'ha1q25_19hand4_strgthl',
              'ha1q25_3astand_height1', 'ha1q25_3bstand_height2', 'ha1q34_4alarm_fat', 'ha1q34_4blarm_lean', 'ha1q34_5ararm_fat', 'ha1q34_5brarm_lean',
              'ha1q34_7alleg_fat', 'ha1q34_7blleg_lean', 'ha1q34_8arleg_fat', 'ha1q34_8brleg_lean'))
  data$dom_grip <- ifelse(data$ha1q25_20dom_hand == 1, data$rhand_strength, data$lhand_strength)
  data$nondom_grip <- ifelse(data$ha1q25_20dom_hand == 2, data$rhand_strength, data$lhand_strength)
  data <- data %>% select(-c('ha1q25_20dom_hand', 'rhand_strength', 'lhand_strength', 'chest_circ'))
  
}

# MALES
dxa_train_male <- avg_measures(dxa_train_male)
dxa_test_male <- avg_measures(dxa_test_male)
# FEMALES
dxa_train_female <- avg_measures(dxa_train_female)
dxa_test_female <- avg_measures(dxa_test_female)


# Now, feature generation
# Make new function for interactions, ratios, etc.

interacts <- function(data){
  data <- data  %>% select(-c(ha1q37_4a_bmr_kj, ha1q37_4b_bmr_kcal)) %>%
    mutate(waist_hip = waist_circ/hip_circ,
           waist_height = waist_circ/height,
           chest_waist = cheste_circ/waist_circ,
           calf_height = calf_circ/height,
           impmod_whole = (height^2)/ha1q37_6i_imp_whole_body,
           impmod_rleg = (height^2)/ha1q37_6ii_imp_rleg,
           impmod_lleg = (height^2)/ha1q37_6iii_imp_lleg,
           impmod_rarm = (height^2)/ha1q37_6iv_imp_rarm,
           impmod_larm = (height^2)/ha1q37_6v_imp_larm,
           fmi = ha1q37_5ii_tbf_mass/((height/1000)^2),
           lmi = ha1q37_5iii_tbf_free_mass/((height/1000)^2),
           bsi = 1000*waist_circ*(ha1q37_2_weight^(-2/3))*((height/1000)^(5/6)),
           log_skin = log(bi_fold + tri_fold + sub_fold + sup_fold),
           age_grip = ha1dv_age*dom_grip
    )
  
}


# MALES
dxa_train_male <- interacts(dxa_train_male)
dxa_test_male <- interacts(dxa_test_male)
# FEMALES
dxa_train_female <- interacts(dxa_train_female)
dxa_test_female <- interacts(dxa_test_female)


# Can add CAMA here as well, slightly different formula for male and females
# So easier to use separate functions
dxa_train_male$cama <- ((dxa_train_male$arm_circ/10 - (pi*dxa_train_male$tri_fold/10))^2/(4*pi)) - 10
dxa_test_male$cama <- ((dxa_test_male$arm_circ/10 - (pi*dxa_test_male$tri_fold/10))^2/(4*pi)) - 10

dxa_train_female$cama <- ((dxa_train_female$arm_circ/10 - (pi*dxa_train_female$tri_fold/10))^2/(4*pi)) - 6.5
dxa_test_female$cama <- ((dxa_test_female$arm_circ/10 - (pi*dxa_test_female$tri_fold/10))^2/(4*pi)) - 6.5


# Finally, implement machine learning models!

###############################
# MODEL BUILDING
###############################

# General function to assess performance in train and test set
train_test_perf <- function(predictions_train, predictions_test, outcome, sex){
  if(sex == 'male'){
    a <- rmse(dxa_train_male[,outcome], predictions_train)
    b <- mae(dxa_train_male[,outcome], predictions_train)
    c <- mape(dxa_train_male[,outcome], predictions_train)
    d <- cor(dxa_train_male[,outcome], predictions_train)^2
    e <- rmse(dxa_test_male[,outcome], predictions_test)
    f <- mae(dxa_test_male[,outcome], predictions_test)
    g <- mape(dxa_test_male[,outcome], predictions_test)
    h <- cor(dxa_test_male[,outcome], predictions_test)^2
    
    results <- c(a,b,c,d,e,f, g, h)
    names(results) <- c('RMSE_train', 'MAE_train', 'MAPE_train', 'R2_train', 'RMSE_test', 'MAE_test', 'MAPE_test', 'R2_test')
    return(results)
    
  }
  else if(sex == 'female'){
    a <- rmse(dxa_train_female[,outcome], predictions_train)
    b <- mae(dxa_train_female[,outcome], predictions_train)
    c <- mape(dxa_train_female[,outcome], predictions_train)
    d <- cor(dxa_train_female[,outcome], predictions_train)^2
    e <- rmse(dxa_test_female[,outcome], predictions_test)
    f <- mae(dxa_test_female[,outcome], predictions_test)
    g <- mape(dxa_test_female[,outcome], predictions_test)
    h <- cor(dxa_test_female[,outcome], predictions_test)^2
    
    results <- c(a,b,c,d,e,f, g, h)
    names(results) <- c('RMSE_train', 'MAE_train', 'MAPE_train', 'R2_train', 'RMSE_test', 'MAE_test', 'MAPE_test', 'R2_test')
    return(results)
  }
}

train_test_perf_age <- function(predictions_train, predictions_test, outcome, sex, age ){
  if(sex == 'male'){
    if(age == "<40"){
      train_set = dxa_train_male[dxa_train_male$ha1dv_age < 40, outcome]
      test_set = dxa_test_male[dxa_test_male$ha1dv_age < 40, outcome]
      
      predictions_train = predictions_train[dxa_train_male$ha1dv_age < 40]
      predictions_test = predictions_test[dxa_test_male$ha1dv_age < 40]
      
      a <- rmse(train_set, predictions_train)
      b <- mae(train_set, predictions_train)
      c <- mape(train_set, predictions_train)
      d <- cor(train_set, predictions_train)^2
      e <- rmse(test_set, predictions_test)
      f <- mae(test_set, predictions_test)
      g <- mape(test_set, predictions_test)
      h <- cor(test_set, predictions_test)^2
    } else if (age == "40+"){
      train_set = dxa_train_male[dxa_train_male$ha1dv_age >= 40, outcome]
      test_set = dxa_test_male[dxa_test_male$ha1dv_age >= 40, outcome]
      
      predictions_train = predictions_train[dxa_train_male$ha1dv_age >= 40]
      predictions_test = predictions_test[dxa_test_male$ha1dv_age >= 40]
      
      a <- rmse(train_set, predictions_train)
      b <- mae(train_set, predictions_train)
      c <- mape(train_set, predictions_train)
      d <- cor(train_set, predictions_train)^2
      e <- rmse(test_set, predictions_test)
      f <- mae(test_set, predictions_test)
      g <- mape(test_set, predictions_test)
      h <- cor(test_set, predictions_test)^2
    }
    
    results <- c(a,b,c,d,e,f, g, h)
    names(results) <- c('RMSE_train', 'MAE_train', 'MAPE_train', 'R2_train', 'RMSE_test', 'MAE_test', 'MAPE_test', 'R2_test')
    return(results)
    
  }
  else if(sex == 'female'){
    if (age == "<40"){
      train_set = dxa_train_female[dxa_train_female$ha1dv_age < 40, outcome]
      test_set = dxa_test_female[dxa_test_female$ha1dv_age < 40, outcome]
      
      predictions_train = predictions_train[dxa_train_female$ha1dv_age < 40]
      predictions_test = predictions_test[dxa_test_female$ha1dv_age < 40]
      
      a <- rmse(train_set, predictions_train)
      b <- mae(train_set, predictions_train)
      c <- mape(train_set, predictions_train)
      d <- cor(train_set, predictions_train)^2
      e <- rmse(test_set, predictions_test)
      f <- mae(test_set, predictions_test)
      g <- mape(test_set, predictions_test)
      h <- cor(test_set, predictions_test)^2
    } else if (age == "40+"){
      train_set = dxa_train_female[dxa_train_female$ha1dv_age >= 40, outcome]
      test_set = dxa_test_female[dxa_test_female$ha1dv_age >= 40, outcome]
      
      predictions_train = predictions_train[dxa_train_female$ha1dv_age >= 40]
      predictions_test = predictions_test[dxa_test_female$ha1dv_age >= 40]
      
      a <- rmse(train_set, predictions_train)
      b <- mae(train_set, predictions_train)
      c <- mape(train_set, predictions_train)
      d <- cor(train_set, predictions_train)^2
      e <- rmse(test_set, predictions_test)
      f <- mae(test_set, predictions_test)
      g <- mape(test_set, predictions_test)
      h <- cor(test_set, predictions_test)^2
    }
    
    results <- c(a,b,c,d,e,f, g, h)
    names(results) <- c('RMSE_train', 'MAE_train', 'MAPE_train', 'R2_train', 'RMSE_test', 'MAE_test', 'MAPE_test', 'R2_test')
    return(results)
  }
}


# LASSO

# Function to save model
make_lasso <- function(outcome_var, sex){
  if(sex == 'male'){
    trainmat <- dxa_train_male %>% select(colnames(dxa_train_male)[c(6:50, 53:69)]) %>% as.matrix()
    lasso1cv <- cv.glmnet(trainmat, dxa_train_male[,outcome_var])
    lam_min1 <- lasso1cv$lambda.min
    lasso1 <- glmnet(trainmat, dxa_train_male[,outcome_var], lambda = lam_min1)
    
    return(lasso1)
  }
  else if(sex == 'female'){
    trainmat <- dxa_train_female %>% select(colnames(dxa_train_female)[c(6:50, 53:69)]) %>% as.matrix()
    lasso1cv <- cv.glmnet(trainmat, dxa_train_female[,outcome_var])
    lam_min1 <- lasso1cv$lambda.min
    lasso1 <- glmnet(trainmat, dxa_train_female[,outcome_var], lambda = lam_min1)
    
    return(lasso1)
  }
}

# RANDOM FOREST

# Function to save model
make_rf <- function(outcome_var, sex){
  if(sex == 'male'){
    form <- as.formula(paste(outcome_var, '~ . -ha1q34_6dtrunk_pcent_fat - ha1q34_12dl1l4_pcent1 - ha1q34_9atotal_fat - ha1q34_9btotal_lean - ha1q34_9dtotal_pcent_fat - appendic_fat - appendic_lean'))
    
    rf1 <- caret::train(form,  
                        dxa_train_male, 
                        tuneLength = 5,
                        method = "ranger",
                        trControl = trainControl(method = "cv", 
                                                 number = 10, 
                                                 verboseIter = FALSE))
    rf1mtry <- rf1$bestTune$mtry
    rf1r <- ranger(form,
                   dxa_train_male, 
                   mtry = rf1mtry, 
                   importance = 'impurity')
    return(rf1r)
  }
  else if(sex == 'female'){
    form <- as.formula(paste(outcome_var, '~ .  -ha1q34_6dtrunk_pcent_fat - ha1q34_12dl1l4_pcent1 - ha1q34_9atotal_fat - ha1q34_9btotal_lean - ha1q34_9dtotal_pcent_fat - appendic_fat - appendic_lean'))
    
    rf1 <- caret::train(form,  
                        dxa_train_female, 
                        tuneLength = 5,
                        method = "ranger",
                        trControl = trainControl(method = "cv", 
                                                 number = 10, 
                                                 verboseIter = FALSE))
    rf1mtry <- rf1$bestTune$mtry
    rf1r <- ranger(form,
                   dxa_train_female, 
                   mtry = rf1mtry, 
                   importance = 'impurity')
    return(rf1r)
  }
}


# XGBOOST

# Function to save model
make_xgb <- function(outcome_var, sex){
  if(sex == 'male'){
    form <- as.formula(paste(outcome_var, '~ .  -ha1q34_6dtrunk_pcent_fat - ha1q34_12dl1l4_pcent1 - ha1q34_9atotal_fat - ha1q34_9btotal_lean - ha1q34_9dtotal_pcent_fat - appendic_fat - appendic_lean'))
    
    xg1 <- caret::train(form,
                        dxa_train_male,
                        method = 'xgbTree',
                        verbosity = 0,
                        #objective = "reg:squarederror",
                        trControl = trainControl(method = "cv", 
                                                 number = 10, 
                                                 verboseIter = FALSE))
    
    return(xg1)
  }
  else if(sex == 'female'){
    form <- as.formula(paste(outcome_var, '~ .  -ha1q34_6dtrunk_pcent_fat - ha1q34_12dl1l4_pcent1 - ha1q34_9atotal_fat - ha1q34_9btotal_lean - ha1q34_9dtotal_pcent_fat - appendic_fat - appendic_lean'))
    
    xg1 <- caret::train(form,
                        dxa_train_female,
                        method = 'xgbTree',
                        verbosity = 0,
                        #objective = "reg:squarederror",
                        trControl = trainControl(method = "cv", 
                                                 number = 10, 
                                                 verboseIter = FALSE))
    
    return(xg1)
  }
}

# Use function to get performance from elastic net models since it takes extra steps
predict_enet <- function(model, dataset){
  return(as.numeric((dataset %>% select(colnames(dataset)[c(6:50, 53:69)]) %>% as.matrix() %*% model$beta) + model$a0))
}


############################
# FIT MODELS AND GET RESULTS
############################

# Outcome = Total body fat mass

# Because of the order processing has been done,
# Might want to do something where we concatenate the outcome for training and test,
# Then shuffle and reallocate

# Within each iteration, start by doing this, 
# Then fit the models and go through to getting the results table
# resultstab = resultstab + resultstab_temp
# Also consider setting seed for each of these separately so they can be run 
# One at a time instead of needing to run for all 6 outcomes at once



set.seed(88)

totalfat_perm = data.frame(RMSE_train	= numeric(8),
                           MAE_train	= numeric(8),
                           MAPE_train	= numeric(8),
                           R2_train	= numeric(8),
                           RMSE_test	= numeric(8),
                           MAE_test	= numeric(8),
                           MAPE_test	= numeric(8),
                           R2_test	= numeric(8))

for(i in 1:25){
  # Have backup copies of dataset from before permutation
  dxa_train_male_save = dxa_train_male
  dxa_test_male_save = dxa_test_male
  dxa_train_female_save = dxa_train_female
  dxa_test_female_save = dxa_test_female
  
  # Randomly sample order for males
  new_order = sample(1:(length(c(dxa_train_male[,'ha1q34_9atotal_fat'], dxa_test_male[,'ha1q34_9atotal_fat']))),
                     length(c(dxa_train_male[,'ha1q34_9atotal_fat'], dxa_test_male[,'ha1q34_9atotal_fat'])))
  
  
  out_vec = c(dxa_train_male[,'ha1q34_9atotal_fat'], dxa_test_male[,'ha1q34_9atotal_fat'])
  out_vec = out_vec[new_order]
  
  dxa_train_male[,'ha1q34_9atotal_fat']  = out_vec[1:nrow(dxa_train_male)]
  dxa_test_male[,'ha1q34_9atotal_fat']  = out_vec[(nrow(dxa_train_male)+1):length(out_vec)]
  
  # Randomly sample order for females
  new_order = sample(1:(length(c(dxa_train_female[,'ha1q34_9atotal_fat'], dxa_test_female[,'ha1q34_9atotal_fat']))),
                     length(c(dxa_train_female[,'ha1q34_9atotal_fat'], dxa_test_female[,'ha1q34_9atotal_fat'])))
  
  
  out_vec = c(dxa_train_female[,'ha1q34_9atotal_fat'], dxa_test_female[,'ha1q34_9atotal_fat'])
  out_vec = out_vec[new_order]
  
  dxa_train_female[,'ha1q34_9atotal_fat']  = out_vec[1:nrow(dxa_train_female)]
  dxa_test_female[,'ha1q34_9atotal_fat']  = out_vec[(nrow(dxa_train_female)+1):length(out_vec)]
  
  
  # Models
  tot_fat_male_lasso <- make_lasso('ha1q34_9atotal_fat', 'male')
  tot_fat_male_rf <- make_rf('ha1q34_9atotal_fat', 'male')
  tot_fat_male_xgb <- make_xgb('ha1q34_9atotal_fat', 'male')
  tot_fat_female_lasso <- make_lasso('ha1q34_9atotal_fat', 'female')
  tot_fat_female_rf <- make_rf('ha1q34_9atotal_fat', 'female')
  tot_fat_female_xgb <- make_xgb('ha1q34_9atotal_fat', 'female')
  
  # Performance
  tfm1 <- train_test_perf(1000*dxa_train_male[,'ha1q37_5ii_tbf_mass'], 
                          1000*dxa_test_male[,'ha1q37_5ii_tbf_mass'], 
                          'ha1q34_9atotal_fat', 'male')
  tfm2 <- train_test_perf(predict_enet(tot_fat_male_lasso, dxa_train_male), 
                          predict_enet(tot_fat_male_lasso, dxa_test_male),
                          'ha1q34_9atotal_fat', 'male')
  tfm3 <- train_test_perf(predict(tot_fat_male_rf, dxa_train_male)$predictions, 
                          predict(tot_fat_male_rf, dxa_test_male)$predictions,
                          'ha1q34_9atotal_fat', 'male')
  tfm4 <- train_test_perf(predict(tot_fat_male_xgb, dxa_train_male), 
                          predict(tot_fat_male_xgb, dxa_test_male),
                          'ha1q34_9atotal_fat', 'male')
  
  
  tfm5 <- train_test_perf(1000*dxa_train_female[,'ha1q37_5ii_tbf_mass'], 
                          1000*dxa_test_female[,'ha1q37_5ii_tbf_mass'], 
                          'ha1q34_9atotal_fat', 'female')
  tfm6 <- train_test_perf(predict_enet(tot_fat_female_lasso, dxa_train_female), 
                          predict_enet(tot_fat_female_lasso, dxa_test_female),
                          'ha1q34_9atotal_fat', 'female')
  tfm7 <- train_test_perf(predict(tot_fat_female_rf, dxa_train_female)$predictions, 
                          predict(tot_fat_female_rf, dxa_test_female)$predictions,
                          'ha1q34_9atotal_fat', 'female')
  tfm8 <- train_test_perf(predict(tot_fat_female_xgb, dxa_train_female), 
                          predict(tot_fat_female_xgb, dxa_test_female),
                          'ha1q34_9atotal_fat', 'female')
  
  totalfatresults <- rbind(tfm1, tfm2, tfm3, tfm4, tfm5, tfm6, tfm7, tfm8)
  
  totalfat_perm = totalfat_perm + totalfatresults
  
  # revert datasets to unpermuted version
  dxa_train_male = dxa_train_male_save
  dxa_test_male = dxa_test_male_save
  dxa_train_female = dxa_train_female_save
  dxa_test_female = dxa_test_female_save
  
  print(i)
}

totalfatresults = totalfat_perm / 25



# Outcome = Total body lean mass, ha1q34_9btotal_lean

set.seed(88)

totallean_perm = data.frame(RMSE_train	= numeric(8),
                           MAE_train	= numeric(8),
                           MAPE_train	= numeric(8),
                           R2_train	= numeric(8),
                           RMSE_test	= numeric(8),
                           MAE_test	= numeric(8),
                           MAPE_test	= numeric(8),
                           R2_test	= numeric(8))

for(i in 1:100){
  # Have backup copies of dataset from before permutation
  dxa_train_male_save = dxa_train_male
  dxa_test_male_save = dxa_test_male
  dxa_train_female_save = dxa_train_female
  dxa_test_female_save = dxa_test_female
  
  # Randomly sample order for males
  new_order = sample(1:(length(c(dxa_train_male[,'ha1q34_9btotal_lean'], dxa_test_male[,'ha1q34_9btotal_lean']))),
                     length(c(dxa_train_male[,'ha1q34_9btotal_lean'], dxa_test_male[,'ha1q34_9btotal_lean'])))
  
  
  out_vec = c(dxa_train_male[,'ha1q34_9btotal_lean'], dxa_test_male[,'ha1q34_9btotal_lean'])
  out_vec = out_vec[new_order]
  
  dxa_train_male[,'ha1q34_9btotal_lean']  = out_vec[1:nrow(dxa_train_male)]
  dxa_test_male[,'ha1q34_9btotal_lean']  = out_vec[(nrow(dxa_train_male)+1):length(out_vec)]
  
  # Randomly sample order for females
  new_order = sample(1:(length(c(dxa_train_female[,'ha1q34_9btotal_lean'], dxa_test_female[,'ha1q34_9btotal_lean']))),
                     length(c(dxa_train_female[,'ha1q34_9btotal_lean'], dxa_test_female[,'ha1q34_9btotal_lean'])))
  
  
  out_vec = c(dxa_train_female[,'ha1q34_9btotal_lean'], dxa_test_female[,'ha1q34_9btotal_lean'])
  out_vec = out_vec[new_order]
  
  dxa_train_female[,'ha1q34_9btotal_lean']  = out_vec[1:nrow(dxa_train_female)]
  dxa_test_female[,'ha1q34_9btotal_lean']  = out_vec[(nrow(dxa_train_female)+1):length(out_vec)]
  
  
  # Models
  tot_lean_male_lasso <- make_lasso('ha1q34_9btotal_lean', 'male')
  tot_lean_male_rf <- make_rf('ha1q34_9btotal_lean', 'male')
  tot_lean_male_xgb <- make_xgb('ha1q34_9btotal_lean', 'male')
  tot_lean_female_lasso <- make_lasso('ha1q34_9btotal_lean', 'female')
  tot_lean_female_rf <- make_rf('ha1q34_9btotal_lean', 'female')
  tot_lean_female_xgb <- make_xgb('ha1q34_9btotal_lean', 'female')
  
  # Performance
  tlm1 <- train_test_perf(1000*dxa_train_male[,'ha1q37_5iii_tbf_free_mass'], 
                          1000*dxa_test_male[,'ha1q37_5iii_tbf_free_mass'], 
                          'ha1q34_9btotal_lean', 'male')
  tlm2 <- train_test_perf(predict_enet(tot_lean_male_lasso, dxa_train_male), 
                          predict_enet(tot_lean_male_lasso, dxa_test_male),
                          'ha1q34_9btotal_lean', 'male')
  tlm3 <- train_test_perf(predict(tot_lean_male_rf, dxa_train_male)$predictions, 
                          predict(tot_lean_male_rf, dxa_test_male)$predictions,
                          'ha1q34_9btotal_lean', 'male')
  tlm4 <- train_test_perf(predict(tot_lean_male_xgb, dxa_train_male), 
                          predict(tot_lean_male_xgb, dxa_test_male),
                          'ha1q34_9btotal_lean', 'male')
  
  
  tlm5 <- train_test_perf(1000*dxa_train_female[,'ha1q37_5iii_tbf_free_mass'], 
                          1000*dxa_test_female[,'ha1q37_5iii_tbf_free_mass'], 
                          'ha1q34_9btotal_lean', 'female')
  tlm6 <- train_test_perf(predict_enet(tot_lean_female_lasso, dxa_train_female), 
                          predict_enet(tot_lean_female_lasso, dxa_test_female),
                          'ha1q34_9btotal_lean', 'female')
  tlm7 <- train_test_perf(predict(tot_lean_female_rf, dxa_train_female)$predictions, 
                          predict(tot_lean_female_rf, dxa_test_female)$predictions,
                          'ha1q34_9btotal_lean', 'female')
  tlm8 <- train_test_perf(predict(tot_lean_female_xgb, dxa_train_female), 
                          predict(tot_lean_female_xgb, dxa_test_female),
                          'ha1q34_9btotal_lean', 'female')
  
  
  totalleanresults <- rbind(tlm1, tlm2, tlm3, tlm4, tlm5, 
                            tlm6, tlm7, tlm8)
  
  
  totallean_perm = totallean_perm + totalleanresults
  
  # revert datasets to unpermuted version
  dxa_train_male = dxa_train_male_save
  dxa_test_male = dxa_test_male_save
  dxa_train_female = dxa_train_female_save
  dxa_test_female = dxa_test_female_save
  
}

totalleanresults = totallean_perm/100



# Outcome = total fat %, ha1q34_9dtotal_pcent_fat


set.seed(88)

totalfatp_perm = data.frame(RMSE_train	= numeric(8),
                            MAE_train	= numeric(8),
                            MAPE_train	= numeric(8),
                            R2_train	= numeric(8),
                            RMSE_test	= numeric(8),
                            MAE_test	= numeric(8),
                            MAPE_test	= numeric(8),
                            R2_test	= numeric(8))

for(i in 1:100){
  # Have backup copies of dataset from before permutation
  dxa_train_male_save = dxa_train_male
  dxa_test_male_save = dxa_test_male
  dxa_train_female_save = dxa_train_female
  dxa_test_female_save = dxa_test_female
  
  # Randomly sample order for males
  new_order = sample(1:(length(c(dxa_train_male[,'ha1q34_9dtotal_pcent_fat'], dxa_test_male[,'ha1q34_9dtotal_pcent_fat']))),
                     length(c(dxa_train_male[,'ha1q34_9dtotal_pcent_fat'], dxa_test_male[,'ha1q34_9dtotal_pcent_fat'])))
  
  
  out_vec = c(dxa_train_male[,'ha1q34_9dtotal_pcent_fat'], dxa_test_male[,'ha1q34_9dtotal_pcent_fat'])
  out_vec = out_vec[new_order]
  
  dxa_train_male[,'ha1q34_9dtotal_pcent_fat']  = out_vec[1:nrow(dxa_train_male)]
  dxa_test_male[,'ha1q34_9dtotal_pcent_fat']  = out_vec[(nrow(dxa_train_male)+1):length(out_vec)]
  
  # Randomly sample order for females
  new_order = sample(1:(length(c(dxa_train_female[,'ha1q34_9dtotal_pcent_fat'], dxa_test_female[,'ha1q34_9dtotal_pcent_fat']))),
                     length(c(dxa_train_female[,'ha1q34_9dtotal_pcent_fat'], dxa_test_female[,'ha1q34_9dtotal_pcent_fat'])))
  
  
  out_vec = c(dxa_train_female[,'ha1q34_9dtotal_pcent_fat'], dxa_test_female[,'ha1q34_9dtotal_pcent_fat'])
  out_vec = out_vec[new_order]
  
  dxa_train_female[,'ha1q34_9dtotal_pcent_fat']  = out_vec[1:nrow(dxa_train_female)]
  dxa_test_female[,'ha1q34_9dtotal_pcent_fat']  = out_vec[(nrow(dxa_train_female)+1):length(out_vec)]
  
  
  
  # Models
  tot_p_male_lasso <- make_lasso('ha1q34_9dtotal_pcent_fat', 'male')
  tot_p_male_rf <- make_rf('ha1q34_9dtotal_pcent_fat', 'male')
  tot_p_male_xgb <- make_xgb('ha1q34_9dtotal_pcent_fat', 'male')
  tot_p_female_lasso <- make_lasso('ha1q34_9dtotal_pcent_fat', 'female')
  tot_p_female_rf <- make_rf('ha1q34_9dtotal_pcent_fat', 'female')
  tot_p_female_xgb <- make_xgb('ha1q34_9dtotal_pcent_fat', 'female')
  
  # Performance
  tfp1 <- train_test_perf(dxa_train_male[,'ha1q37_5i_tbf_pcent'], 
                          dxa_test_male[,'ha1q37_5i_tbf_pcent'], 
                          'ha1q34_9dtotal_pcent_fat', 'male')
  tfp2 <- train_test_perf(predict_enet(tot_p_male_lasso, dxa_train_male), 
                          predict_enet(tot_p_male_lasso, dxa_test_male),
                          'ha1q34_9dtotal_pcent_fat', 'male')
  tfp3 <- train_test_perf(predict(tot_p_male_rf, dxa_train_male)$predictions, 
                          predict(tot_p_male_rf, dxa_test_male)$predictions,
                          'ha1q34_9dtotal_pcent_fat', 'male')
  tfp4 <- train_test_perf(predict(tot_p_male_xgb, dxa_train_male), 
                          predict(tot_p_male_xgb, dxa_test_male),
                          'ha1q34_9dtotal_pcent_fat', 'male')
  
  
  tfp5 <- train_test_perf(dxa_train_female[,'ha1q37_5i_tbf_pcent'], 
                          dxa_test_female[,'ha1q37_5i_tbf_pcent'], 
                          'ha1q34_9dtotal_pcent_fat', 'female')
  tfp6 <- train_test_perf(predict_enet(tot_p_female_lasso, dxa_train_female), 
                          predict_enet(tot_p_female_lasso, dxa_test_female),
                          'ha1q34_9dtotal_pcent_fat', 'female')
  tfp7 <- train_test_perf(predict(tot_p_female_rf, dxa_train_female)$predictions, 
                          predict(tot_p_female_rf, dxa_test_female)$predictions,
                          'ha1q34_9dtotal_pcent_fat', 'female')
  tfp8 <- train_test_perf(predict(tot_p_female_xgb, dxa_train_female), 
                          predict(tot_p_female_xgb, dxa_test_female),
                          'ha1q34_9dtotal_pcent_fat', 'female')
  
  totalfatpresults <- rbind(tfp1, tfp2, tfp3, tfp4, tfp5, tfp6, tfp7, tfp8)
  
  
  
  totalfatp_perm = totalfatp_perm + totalfatpresults
  
  # revert datasets to unpermuted version
  dxa_train_male = dxa_train_male_save
  dxa_test_male = dxa_test_male_save
  dxa_train_female = dxa_train_female_save
  dxa_test_female = dxa_test_female_save
  
}

totalfatpresults=totalfatp_perm/100



# Outcome = Trunk percentage fat mass, ha1q34_6dtrunk_pcent_fat

set.seed(88)

trunkfat_perm = data.frame(RMSE_train	= numeric(8),
                            MAE_train	= numeric(8),
                            MAPE_train	= numeric(8),
                            R2_train	= numeric(8),
                            RMSE_test	= numeric(8),
                            MAE_test	= numeric(8),
                            MAPE_test	= numeric(8),
                            R2_test	= numeric(8))

for(i in 1:100){
  # Have backup copies of dataset from before permutation
  dxa_train_male_save = dxa_train_male
  dxa_test_male_save = dxa_test_male
  dxa_train_female_save = dxa_train_female
  dxa_test_female_save = dxa_test_female
  
  # Randomly sample order for males
  new_order = sample(1:(length(c(dxa_train_male[,'ha1q34_6dtrunk_pcent_fat'], dxa_test_male[,'ha1q34_6dtrunk_pcent_fat']))),
                     length(c(dxa_train_male[,'ha1q34_6dtrunk_pcent_fat'], dxa_test_male[,'ha1q34_6dtrunk_pcent_fat'])))
  
  
  out_vec = c(dxa_train_male[,'ha1q34_6dtrunk_pcent_fat'], dxa_test_male[,'ha1q34_6dtrunk_pcent_fat'])
  out_vec = out_vec[new_order]
  
  dxa_train_male[,'ha1q34_6dtrunk_pcent_fat']  = out_vec[1:nrow(dxa_train_male)]
  dxa_test_male[,'ha1q34_6dtrunk_pcent_fat']  = out_vec[(nrow(dxa_train_male)+1):length(out_vec)]
  
  # Randomly sample order for females
  new_order = sample(1:(length(c(dxa_train_female[,'ha1q34_6dtrunk_pcent_fat'], dxa_test_female[,'ha1q34_6dtrunk_pcent_fat']))),
                     length(c(dxa_train_female[,'ha1q34_6dtrunk_pcent_fat'], dxa_test_female[,'ha1q34_6dtrunk_pcent_fat'])))
  
  
  out_vec = c(dxa_train_female[,'ha1q34_6dtrunk_pcent_fat'], dxa_test_female[,'ha1q34_6dtrunk_pcent_fat'])
  out_vec = out_vec[new_order]
  
  dxa_train_female[,'ha1q34_6dtrunk_pcent_fat']  = out_vec[1:nrow(dxa_train_female)]
  dxa_test_female[,'ha1q34_6dtrunk_pcent_fat']  = out_vec[(nrow(dxa_train_female)+1):length(out_vec)]
  
  
  
  
  # Models
  trunk_fat_male_lasso <- make_lasso('ha1q34_6dtrunk_pcent_fat', 'male')
  trunk_fat_male_rf <- make_rf('ha1q34_6dtrunk_pcent_fat', 'male')
  trunk_fat_male_xgb <- make_xgb('ha1q34_6dtrunk_pcent_fat', 'male')
  trunk_fat_female_lasso <- make_lasso('ha1q34_6dtrunk_pcent_fat', 'female')
  trunk_fat_female_rf <- make_rf('ha1q34_6dtrunk_pcent_fat', 'female')
  trunk_fat_female_xgb <- make_xgb('ha1q34_6dtrunk_pcent_fat', 'female')
  
  # Performance
  trp1 <- train_test_perf(dxa_train_male[,'ha1q37_11i_seg_tr_pcent'], 
                          dxa_test_male[,'ha1q37_11i_seg_tr_pcent'], 
                          'ha1q34_6dtrunk_pcent_fat', 'male')
  trp2 <- train_test_perf(predict_enet(trunk_fat_male_lasso, dxa_train_male), 
                          predict_enet(trunk_fat_male_lasso, dxa_test_male),
                          'ha1q34_6dtrunk_pcent_fat', 'male')
  trp3 <- train_test_perf(predict(trunk_fat_male_rf, dxa_train_male)$predictions, 
                          predict(trunk_fat_male_rf, dxa_test_male)$predictions,
                          'ha1q34_6dtrunk_pcent_fat', 'male')
  trp4 <- train_test_perf(predict(trunk_fat_male_xgb, dxa_train_male), 
                          predict(trunk_fat_male_xgb, dxa_test_male),
                          'ha1q34_6dtrunk_pcent_fat', 'male')
  
  
  trp5 <- train_test_perf(dxa_train_female[,'ha1q37_11i_seg_tr_pcent'], 
                          dxa_test_female[,'ha1q37_11i_seg_tr_pcent'], 
                          'ha1q34_6dtrunk_pcent_fat', 'female')
  trp6 <- train_test_perf(predict_enet(trunk_fat_female_lasso, dxa_train_female), 
                          predict_enet(trunk_fat_female_lasso, dxa_test_female),
                          'ha1q34_6dtrunk_pcent_fat', 'female')
  trp7 <- train_test_perf(predict(trunk_fat_female_rf, dxa_train_female)$predictions, 
                          predict(trunk_fat_female_rf, dxa_test_female)$predictions,
                          'ha1q34_6dtrunk_pcent_fat', 'female')
  trp8 <- train_test_perf(predict(trunk_fat_female_xgb, dxa_train_female), 
                          predict(trunk_fat_female_xgb, dxa_test_female),
                          'ha1q34_6dtrunk_pcent_fat', 'female')
  
  trunkpercent <- rbind(trp1, trp2, trp3, trp4, trp5, trp6, trp7, trp8)
  
  
  trunkfat_perm = trunkfat_perm + trunkpercent
  
  # revert datasets to unpermuted version
  dxa_train_male = dxa_train_male_save
  dxa_test_male = dxa_test_male_save
  dxa_train_female = dxa_train_female_save
  dxa_test_female = dxa_test_female_save
  
}

trunkpercent = trunkfat_perm/100



# Outcome = L1-L4 trunk fat percentage, ha1q34_12dl1l4_pcent1


set.seed(88)

l1l4_perm = data.frame(RMSE_train	= numeric(8),
                           MAE_train	= numeric(8),
                           MAPE_train	= numeric(8),
                           R2_train	= numeric(8),
                           RMSE_test	= numeric(8),
                           MAE_test	= numeric(8),
                           MAPE_test	= numeric(8),
                           R2_test	= numeric(8))

for(i in 1:100){
  # Have backup copies of dataset from before permutation
  dxa_train_male_save = dxa_train_male
  dxa_test_male_save = dxa_test_male
  dxa_train_female_save = dxa_train_female
  dxa_test_female_save = dxa_test_female
  
  # Randomly sample order for males
  new_order = sample(1:(length(c(dxa_train_male[,'ha1q34_12dl1l4_pcent1'], dxa_test_male[,'ha1q34_12dl1l4_pcent1']))),
                     length(c(dxa_train_male[,'ha1q34_12dl1l4_pcent1'], dxa_test_male[,'ha1q34_12dl1l4_pcent1'])))
  
  
  out_vec = c(dxa_train_male[,'ha1q34_12dl1l4_pcent1'], dxa_test_male[,'ha1q34_12dl1l4_pcent1'])
  out_vec = out_vec[new_order]
  
  dxa_train_male[,'ha1q34_12dl1l4_pcent1']  = out_vec[1:nrow(dxa_train_male)]
  dxa_test_male[,'ha1q34_12dl1l4_pcent1']  = out_vec[(nrow(dxa_train_male)+1):length(out_vec)]
  
  # Randomly sample order for females
  new_order = sample(1:(length(c(dxa_train_female[,'ha1q34_12dl1l4_pcent1'], dxa_test_female[,'ha1q34_12dl1l4_pcent1']))),
                     length(c(dxa_train_female[,'ha1q34_12dl1l4_pcent1'], dxa_test_female[,'ha1q34_12dl1l4_pcent1'])))
  
  
  out_vec = c(dxa_train_female[,'ha1q34_12dl1l4_pcent1'], dxa_test_female[,'ha1q34_12dl1l4_pcent1'])
  out_vec = out_vec[new_order]
  
  dxa_train_female[,'ha1q34_12dl1l4_pcent1']  = out_vec[1:nrow(dxa_train_female)]
  dxa_test_female[,'ha1q34_12dl1l4_pcent1']  = out_vec[(nrow(dxa_train_female)+1):length(out_vec)]
  
  
  
  
  
  # Models
  l1_fat_male_lasso <- make_lasso('ha1q34_12dl1l4_pcent1', 'male')
  l1_fat_male_rf <- make_rf('ha1q34_12dl1l4_pcent1', 'male')
  l1_fat_male_xgb <- make_xgb('ha1q34_12dl1l4_pcent1', 'male')
  l1_fat_female_lasso <- make_lasso('ha1q34_12dl1l4_pcent1', 'female')
  l1_fat_female_rf <- make_rf('ha1q34_12dl1l4_pcent1', 'female')
  l1_fat_female_xgb <- make_xgb('ha1q34_12dl1l4_pcent1', 'female')
  
  # Performance
  # Trunk used as closest comparison from Tanita
  l11 <- train_test_perf(dxa_train_male[,'ha1q37_11i_seg_tr_pcent'], 
                         dxa_test_male[,'ha1q37_11i_seg_tr_pcent'], 
                         'ha1q34_12dl1l4_pcent1', 'male')
  l12 <- train_test_perf(predict_enet(l1_fat_male_lasso, dxa_train_male), 
                         predict_enet(l1_fat_male_lasso, dxa_test_male),
                         'ha1q34_12dl1l4_pcent1', 'male')
  l13 <- train_test_perf(predict(l1_fat_male_rf, dxa_train_male)$predictions, 
                         predict(l1_fat_male_rf, dxa_test_male)$predictions,
                         'ha1q34_12dl1l4_pcent1', 'male')
  l14 <- train_test_perf(predict(l1_fat_male_xgb, dxa_train_male), 
                         predict(l1_fat_male_xgb, dxa_test_male),
                         'ha1q34_12dl1l4_pcent1', 'male')
  
  
  l15 <- train_test_perf(dxa_train_female[,'ha1q37_11i_seg_tr_pcent'], 
                         dxa_test_female[,'ha1q37_11i_seg_tr_pcent'], 
                         'ha1q34_12dl1l4_pcent1', 'female')
  l16 <- train_test_perf(predict_enet(l1_fat_female_lasso, dxa_train_female), 
                         predict_enet(l1_fat_female_lasso, dxa_test_female),
                         'ha1q34_12dl1l4_pcent1', 'female')
  l17 <- train_test_perf(predict(l1_fat_female_rf, dxa_train_female)$predictions, 
                         predict(l1_fat_female_rf, dxa_test_female)$predictions,
                         'ha1q34_12dl1l4_pcent1', 'female')
  l18 <- train_test_perf(predict(l1_fat_female_xgb, dxa_train_female), 
                         predict(l1_fat_female_xgb, dxa_test_female),
                         'ha1q34_12dl1l4_pcent1', 'female')
  
  l1results <- rbind(l11, l12, l13, l14, l15, l16, l17, l18)
  
  l1l4_perm = l1l4_perm + l1results
  
  
  # revert datasets to unpermuted version
  dxa_train_male = dxa_train_male_save
  dxa_test_male = dxa_test_male_save
  dxa_train_female = dxa_train_female_save
  dxa_test_female = dxa_test_female_save
  
}

l1results = l1l4_perm/100


# Outcome = appendicular lean mass, appendic_lean


set.seed(88)

app_perm = data.frame(RMSE_train	= numeric(8),
                       MAE_train	= numeric(8),
                       MAPE_train	= numeric(8),
                       R2_train	= numeric(8),
                       RMSE_test	= numeric(8),
                       MAE_test	= numeric(8),
                       MAPE_test	= numeric(8),
                       R2_test	= numeric(8))

for(i in 1:100){
  # Have backup copies of dataset from before permutation
  dxa_train_male_save = dxa_train_male
  dxa_test_male_save = dxa_test_male
  dxa_train_female_save = dxa_train_female
  dxa_test_female_save = dxa_test_female
  
  # Randomly sample order for males
  new_order = sample(1:(length(c(dxa_train_male[,'ha1q37_7iii_seg_rl_free_mass'], dxa_test_male[,'ha1q37_7iii_seg_rl_free_mass']))),
                     length(c(dxa_train_male[,'ha1q37_7iii_seg_rl_free_mass'], dxa_test_male[,'ha1q37_7iii_seg_rl_free_mass'])))
  
  
  out_vec = c(dxa_train_male[,'ha1q37_7iii_seg_rl_free_mass'], dxa_test_male[,'ha1q37_7iii_seg_rl_free_mass'])
  out_vec = out_vec[new_order]
  
  dxa_train_male[,'ha1q37_7iii_seg_rl_free_mass']  = out_vec[1:nrow(dxa_train_male)]
  dxa_test_male[,'ha1q37_7iii_seg_rl_free_mass']  = out_vec[(nrow(dxa_train_male)+1):length(out_vec)]
  
  # Randomly sample order for females
  new_order = sample(1:(length(c(dxa_train_female[,'ha1q37_7iii_seg_rl_free_mass'], dxa_test_female[,'ha1q37_7iii_seg_rl_free_mass']))),
                     length(c(dxa_train_female[,'ha1q37_7iii_seg_rl_free_mass'], dxa_test_female[,'ha1q37_7iii_seg_rl_free_mass'])))
  
  
  out_vec = c(dxa_train_female[,'ha1q37_7iii_seg_rl_free_mass'], dxa_test_female[,'ha1q37_7iii_seg_rl_free_mass'])
  out_vec = out_vec[new_order]
  
  dxa_train_female[,'ha1q37_7iii_seg_rl_free_mass']  = out_vec[1:nrow(dxa_train_female)]
  dxa_test_female[,'ha1q37_7iii_seg_rl_free_mass']  = out_vec[(nrow(dxa_train_female)+1):length(out_vec)]
  
  
  
  
  # Models
  app_lean_male_lasso <- make_lasso('appendic_lean', 'male')
  app_lean_male_rf <- make_rf('appendic_lean', 'male')
  app_lean_male_xgb <- make_xgb('appendic_lean', 'male')
  app_lean_female_lasso <- make_lasso('appendic_lean', 'female')
  app_lean_female_rf <- make_rf('appendic_lean', 'female')
  app_lean_female_xgb <- make_xgb('appendic_lean', 'female')
  
  # Performance
  ap1 <- train_test_perf(1000*dxa_train_male[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_train_male[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_train_male[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_train_male[,'ha1q37_10iii_seg_la_free_mass'], 
                         1000*dxa_test_male[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_male[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_male[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_male[,'ha1q37_10iii_seg_la_free_mass'], 
                         'appendic_lean', 'male')
  ap2 <- train_test_perf(predict_enet(app_lean_male_lasso, dxa_train_male), 
                         predict_enet(app_lean_male_lasso, dxa_test_male),
                         'appendic_lean', 'male')
  ap3 <- train_test_perf(predict(app_lean_male_rf, dxa_train_male)$predictions, 
                         predict(app_lean_male_rf, dxa_test_male)$predictions,
                         'appendic_lean', 'male')
  ap4 <- train_test_perf(predict(app_lean_male_xgb, dxa_train_male), 
                         predict(app_lean_male_xgb, dxa_test_male),
                         'appendic_lean', 'male')
  
  
  ap5 <- train_test_perf(1000*dxa_train_female[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_train_female[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_train_female[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_train_female[,'ha1q37_10iii_seg_la_free_mass'], 
                         1000*dxa_test_female[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_female[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_female[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_female[,'ha1q37_10iii_seg_la_free_mass'], 
                         'appendic_lean', 'female')
  ap6 <- train_test_perf(predict_enet(app_lean_female_lasso, dxa_train_female), 
                         predict_enet(app_lean_female_lasso, dxa_test_female),
                         'appendic_lean', 'female')
  ap7 <- train_test_perf(predict(app_lean_female_rf, dxa_train_female)$predictions, 
                         predict(app_lean_female_rf, dxa_test_female)$predictions,
                         'appendic_lean', 'female')
  ap8 <- train_test_perf(predict(app_lean_female_xgb, dxa_train_female), 
                         predict(app_lean_female_xgb, dxa_test_female),
                         'appendic_lean', 'female')
  
  
  apresults <- rbind(ap1, ap2, ap3, ap4, ap5,
                     ap6, ap7, ap8)
  
  
  app_perm = app_perm + apresults
  
  
  # revert datasets to unpermuted version
  dxa_train_male = dxa_train_male_save
  dxa_test_male = dxa_test_male_save
  dxa_train_female = dxa_train_female_save
  dxa_test_female = dxa_test_female_save
  
}

apresults = app_perm/100


# Process results tables to make more legible
# Convert grams units to kg
# Convert percents to percents (XX.X% not 0.XXX%)
# round decimals to sensible number for each

totalfatresults[,1] <- round(totalfatresults[,1]/1000, 2)
totalfatresults[,2] <- round(totalfatresults[,2]/1000, 3)
totalfatresults[,3] <- round(totalfatresults[,3]*100, 2)
totalfatresults[,4] <- round(totalfatresults[,4], 3)
totalfatresults[,5] <- round(totalfatresults[,5]/1000, 2)
totalfatresults[,6] <- round(totalfatresults[,6]/1000, 3)
totalfatresults[,7] <- round(totalfatresults[,7]*100, 2)
totalfatresults[,8] <- round(totalfatresults[,8], 3)


totalleanresults[,1] <- round(totalleanresults[,1]/1000, 2)
totalleanresults[,2] <- round(totalleanresults[,2]/1000, 3)
totalleanresults[,3] <- round(totalleanresults[,3]*100, 2)
totalleanresults[,4] <- round(totalleanresults[,4], 3)
totalleanresults[,5] <- round(totalleanresults[,5]/1000, 2)
totalleanresults[,6] <- round(totalleanresults[,6]/1000, 3)
totalleanresults[,7] <- round(totalleanresults[,7]*100, 2)
totalleanresults[,8] <- round(totalleanresults[,8], 3)

apresults[,1] <- round(apresults[,1]/1000, 2)
apresults[,2] <- round(apresults[,2]/1000, 3)
apresults[,3] <- round(apresults[,3]*100, 2)
apresults[,4] <- round(apresults[,4], 3)
apresults[,5] <- round(apresults[,5]/1000, 2)
apresults[,6] <- round(apresults[,6]/1000, 3)
apresults[,7] <- round(apresults[,7]*100, 2)
apresults[,8] <- round(apresults[,8], 3)


totalfatpresults[,1] <- round(totalfatpresults[,1], 3)
totalfatpresults[,2] <- round(totalfatpresults[,2], 3)
totalfatpresults[,3] <- round(totalfatpresults[,3]*100, 2)
totalfatpresults[,4] <- round(totalfatpresults[,4], 3)
totalfatpresults[,5] <- round(totalfatpresults[,5], 3)
totalfatpresults[,6] <- round(totalfatpresults[,6], 3)
totalfatpresults[,7] <- round(totalfatpresults[,7]*100, 2)
totalfatpresults[,8] <- round(totalfatpresults[,8], 3)

trunkpercent[,1] <- round(trunkpercent[,1], 3)
trunkpercent[,2] <- round(trunkpercent[,2], 3)
trunkpercent[,3] <- round(trunkpercent[,3]*100, 2)
trunkpercent[,4] <- round(trunkpercent[,4], 3)
trunkpercent[,5] <- round(trunkpercent[,5], 3)
trunkpercent[,6] <- round(trunkpercent[,6], 3)
trunkpercent[,7] <- round(trunkpercent[,7]*100, 2)
trunkpercent[,8] <- round(trunkpercent[,8], 3)

l1results[,1] <- round(l1results[,1], 3)
l1results[,2] <- round(l1results[,2], 3)
l1results[,3] <- round(l1results[,3]*100, 2)
l1results[,4] <- round(l1results[,4], 3)
l1results[,5] <- round(l1results[,5], 3)
l1results[,6] <- round(l1results[,6], 3)
l1results[,7] <- round(l1results[,7]*100, 2)
l1results[,8] <- round(l1results[,8], 3)


# EXPORT RESULTS
write.csv(totalfatresults, 'totalfatmasslessints_perm.csv')
write.csv(totalleanresults, 'totalleanmasslessints_perm.csv')
write.csv(totalfatpresults, 'totalfatpercentlessints_perm.csv')
write.csv(trunkpercent, 'trunkfatpercentlessints_perm.csv')
write.csv(l1results, 'l1l4fatpercentlessints_perm.csv')
write.csv(apresults, 'appleanmasslessints_perm.csv')





#########################
# Get a better understanding of what results look like 
# of such an analysis under different data generating mechanisms
set.seed(99)
errs = rnorm(1000)
X1 = rnorm(1000)
X2 = rnorm(1000)
Y = 0.005*X1 + 0.008*X2 + errs # weak but real relationship

fakedat = data.frame(X1 = X1,
           X2 = X2,
           Y = Y)

train <- fakedat[1:800,]
test <- fakedat[801:1000,]

realmod <- lm(Y ~ X1 + X2, data = train)
test$preds = predict(realmod, test)
mean(abs(test$preds - test$Y))


# Now reshuffle
neworder = sample(1:1000, 1000)
fakedat$Y = fakedat$Y[neworder]

train <- fakedat[1:800,]
test <- fakedat[801:1000,]

realmod <- lm(Y ~ X1 + X2, data = train)
test$preds = predict(realmod, test)
mean(abs(test$preds - test$Y))
summary(realmod)
# MAE of fake data often smaller than real data
# But coef estimates quite different


# Get a better understanding
set.seed(99)
errs = rnorm(1000)
X1 = rnorm(1000)
X2 = rnorm(1000)
Y = 0.5*X1 + 0.8*X2 + errs # real and strong relationship

fakedat = data.frame(X1 = X1,
                     X2 = X2,
                     Y = Y)

train <- fakedat[1:800,]
test <- fakedat[801:1000,]

realmod <- lm(Y ~ X1 + X2, data = train)
test$preds = predict(realmod, test)
mean(abs(test$preds - test$Y))


# Now reshuffle
neworder = sample(1:1000, 1000)
fakedat$Y = fakedat$Y[neworder]

train <- fakedat[1:800,]
test <- fakedat[801:1000,]

realmod <- lm(Y ~ X1 + X2, data = train)
test$preds = predict(realmod, test)
mean(abs(test$preds - test$Y))
# Error often much larger in the fake data

