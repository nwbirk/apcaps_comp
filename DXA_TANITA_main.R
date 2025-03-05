# DXA TANITA NO WINDSOR
# Also includes updated data quality checks



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

# Get age info
table(dxa_train_male$ha1dv_age < 40)
table(dxa_train_female$ha1dv_age < 40)
table(dxa_test_male$ha1dv_age < 40)
table(dxa_test_female$ha1dv_age < 40)

# Outcome = Total body fat mass

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

# plots
p1 = blandr.draw(predict_enet(tot_fat_male_lasso, dxa_test_male), 
                 dxa_test_male[,'ha1q34_9atotal_fat'], lowest_y_axis = -10000, highest_y_axis = 10000,
                 plotTitle = "Total fat mass (g), male, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p2 = blandr.draw(predict_enet(tot_fat_female_lasso, dxa_test_female), 
                 dxa_test_female[,'ha1q34_9atotal_fat'], lowest_y_axis = -10000, highest_y_axis = 10000,
                 plotTitle = "Total fat mass (g), female, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")


p3 = blandr.draw(1000*dxa_test_male[,'ha1q37_5ii_tbf_mass'], 
                 dxa_test_male[,'ha1q34_9atotal_fat'], lowest_y_axis = -10000, highest_y_axis = 10000,
                 plotTitle = "Total fat mass (g), male, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p4 = blandr.draw(1000*dxa_test_female[,'ha1q37_5ii_tbf_mass'], 
                 dxa_test_female[,'ha1q34_9atotal_fat'], lowest_y_axis = -10000, highest_y_axis = 10000,
                 plotTitle = "Total fat mass (g), female, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")

# Display / save
jpeg("totalfatmale.jpg", width = 660, height = "411")
grid.arrange(p1, p3, ncol = 2)
dev.off()
jpeg("totalfatfemale.jpg", width = 660, height = "411")
grid.arrange(p2, p4, ncol = 2)
dev.off()


# Extra info - separated by age

# Performance
tfm1a <- train_test_perf_age(1000*dxa_train_male[,'ha1q37_5ii_tbf_mass'], 
                        1000*dxa_test_male[,'ha1q37_5ii_tbf_mass'], 
                        'ha1q34_9atotal_fat', 'male', age = "<40")
tfm2a <- train_test_perf_age(predict_enet(tot_fat_male_lasso, dxa_train_male), 
                        predict_enet(tot_fat_male_lasso, dxa_test_male),
                        'ha1q34_9atotal_fat', 'male', age = "<40")
tfm3a <- train_test_perf_age(predict(tot_fat_male_rf, dxa_train_male)$predictions, 
                        predict(tot_fat_male_rf, dxa_test_male)$predictions,
                        'ha1q34_9atotal_fat', 'male', age = "<40")
tfm4a <- train_test_perf_age(predict(tot_fat_male_xgb, dxa_train_male), 
                        predict(tot_fat_male_xgb, dxa_test_male),
                        'ha1q34_9atotal_fat', 'male', age = "<40")


tfm5a <- train_test_perf_age(1000*dxa_train_female[,'ha1q37_5ii_tbf_mass'], 
                        1000*dxa_test_female[,'ha1q37_5ii_tbf_mass'], 
                        'ha1q34_9atotal_fat', 'female', age = "<40")
tfm6a <- train_test_perf_age(predict_enet(tot_fat_female_lasso, dxa_train_female), 
                        predict_enet(tot_fat_female_lasso, dxa_test_female),
                        'ha1q34_9atotal_fat', 'female', age = "<40")
tfm7a <- train_test_perf_age(predict(tot_fat_female_rf, dxa_train_female)$predictions, 
                        predict(tot_fat_female_rf, dxa_test_female)$predictions,
                        'ha1q34_9atotal_fat', 'female', age = "<40")
tfm8a <- train_test_perf_age(predict(tot_fat_female_xgb, dxa_train_female), 
                        predict(tot_fat_female_xgb, dxa_test_female),
                        'ha1q34_9atotal_fat', 'female', age = "<40")


tfm1b <- train_test_perf_age(1000*dxa_train_male[,'ha1q37_5ii_tbf_mass'], 
                         1000*dxa_test_male[,'ha1q37_5ii_tbf_mass'], 
                         'ha1q34_9atotal_fat', 'male', age = "40+")
tfm2b <- train_test_perf_age(predict_enet(tot_fat_male_lasso, dxa_train_male), 
                         predict_enet(tot_fat_male_lasso, dxa_test_male),
                         'ha1q34_9atotal_fat', 'male', age = "40+")
tfm3b <- train_test_perf_age(predict(tot_fat_male_rf, dxa_train_male)$predictions, 
                         predict(tot_fat_male_rf, dxa_test_male)$predictions,
                         'ha1q34_9atotal_fat', 'male', age = "40+")
tfm4b <- train_test_perf_age(predict(tot_fat_male_xgb, dxa_train_male), 
                         predict(tot_fat_male_xgb, dxa_test_male),
                         'ha1q34_9atotal_fat', 'male', age = "40+")


tfm5b <- train_test_perf_age(1000*dxa_train_female[,'ha1q37_5ii_tbf_mass'], 
                         1000*dxa_test_female[,'ha1q37_5ii_tbf_mass'], 
                         'ha1q34_9atotal_fat', 'female', age = "40+")
tfm6b <- train_test_perf_age(predict_enet(tot_fat_female_lasso, dxa_train_female), 
                         predict_enet(tot_fat_female_lasso, dxa_test_female),
                         'ha1q34_9atotal_fat', 'female', age = "40+")
tfm7b <- train_test_perf_age(predict(tot_fat_female_rf, dxa_train_female)$predictions, 
                         predict(tot_fat_female_rf, dxa_test_female)$predictions,
                         'ha1q34_9atotal_fat', 'female', age = "40+")
tfm8b <- train_test_perf_age(predict(tot_fat_female_xgb, dxa_train_female), 
                         predict(tot_fat_female_xgb, dxa_test_female),
                         'ha1q34_9atotal_fat', 'female', age = "40+")


totalfatresults_u40 <- rbind(tfm1a, tfm2a, tfm3a, tfm4a, tfm5a, tfm6a, tfm7a, tfm8a)
totalfatresults_o40 <- rbind(tfm1b, tfm2b, tfm3b, tfm4b, tfm5b, tfm6b, tfm7b, tfm8b)


# Outcome = Total body lean mass, ha1q34_9btotal_lean

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

# Bharati also did paired t-test to compare differences?
t.test(predict_enet(tot_lean_male_lasso, dxa_test_male), dxa_test_male$ha1q34_9btotal_lean, paired = TRUE)
t.test(predict_enet(tot_lean_female_lasso, dxa_test_female), dxa_test_female$ha1q34_9btotal_lean, paired = TRUE)

# plots
p1 = blandr.draw(predict_enet(tot_lean_male_lasso, dxa_test_male), 
                 dxa_test_male[,'ha1q34_9btotal_lean'],
                 plotTitle = "Total lean mass (g), male, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p2 = blandr.draw(predict_enet(tot_lean_female_lasso, dxa_test_female), 
                 dxa_test_female[,'ha1q34_9btotal_lean'],
                 plotTitle = "Total lean mass (g), female, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")


p3 = blandr.draw(1000*dxa_test_male[,'ha1q37_5iii_tbf_free_mass'], 
                 dxa_test_male[,'ha1q34_9btotal_lean'],
                 plotTitle = "Total lean mass (g), male, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p4 = blandr.draw(1000*dxa_test_female[,'ha1q37_5iii_tbf_free_mass'], 
                 dxa_test_female[,'ha1q34_9btotal_lean'],
                 plotTitle = "Total lean mass (g), female, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")


# Display 
jpeg("totalleanmale.jpg", width = 660, height = "411")
grid.arrange(p1, p3, ncol = 2)
dev.off()

jpeg("totalleanfemale.jpg", width = 660, height = "411")
grid.arrange(p2, p4, ncol = 2)
dev.off()

# Further, can include comparison to Bharati's equations here (Table 2)
tlm9 <- train_test_perf((as.numeric(10.385 - (0.005*dxa_train_male$ha1dv_age) + (0.103*dxa_train_male$height/10)+ (0.680*dxa_train_male$ha1q37_2_weight) + (0.288*dxa_train_male$arm_circ/10) + (0.130*dxa_train_male$calf_circ/10) - (0.183*dxa_train_male$hip_circ/10) - (5.278*dxa_train_male$log_skin))*1000), 
                        (as.numeric(10.385 - (0.005*dxa_test_male$ha1dv_age) + (0.103*dxa_test_male$height/10)+ (0.680*dxa_test_male$ha1q37_2_weight) + (0.288*dxa_test_male$arm_circ/10) + (0.130*dxa_test_male$calf_circ/10) - (0.183*dxa_test_male$hip_circ/10) - (5.278*dxa_test_male$log_skin))*1000),
                        'ha1q34_9btotal_lean', 'male')

tlm10 <- train_test_perf(as.numeric(10.632 - (0.009*dxa_train_female$ha1dv_age) + (0.102*dxa_train_female$height/10)+ (0.592*dxa_train_female$ha1q37_2_weight) + (0.055*dxa_train_female$arm_circ/10) + (0.043*dxa_train_female$calf_circ/10) - (0.158*dxa_train_female$hip_circ/10) - (3.174*dxa_train_female$log_skin))*1000, 
                         as.numeric(10.632 - (0.009*dxa_test_female$ha1dv_age) + (0.102*dxa_test_female$height/10)+ (0.592*dxa_test_female$ha1q37_2_weight) + (0.055*dxa_test_female$arm_circ/10) + (0.043*dxa_test_female$calf_circ/10) - (0.158*dxa_test_female$hip_circ/10) - (3.174*dxa_test_female$log_skin))*1000,
                         'ha1q34_9btotal_lean', 'female')

totalleanresults <- rbind(tlm1, tlm2, tlm3, tlm4, tlm5, 
                          tlm6, tlm7, tlm8, tlm9, tlm10)

blandr.draw(as.numeric(10.385 - (0.005*dxa_test_male$ha1dv_age) + (0.103*dxa_test_male$height/10)+ (0.680*dxa_test_male$ha1q37_2_weight) + (0.288*dxa_test_male$arm_circ/10) + (0.130*dxa_test_male$calf_circ/10) - (0.183*dxa_test_male$hip_circ/10) - (5.278*dxa_test_male$log_skin))*1000, 
            dxa_test_male[,'ha1q34_9btotal_lean'],
            plotTitle = "Total lean mass (g) predictions, Bharati, male", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")

blandr.draw(as.numeric(10.632 - (0.009*dxa_test_female$ha1dv_age) + (0.102*dxa_test_female$height/10)+ (0.592*dxa_test_female$ha1q37_2_weight) + (0.055*dxa_test_female$arm_circ/10) + (0.043*dxa_test_female$calf_circ/10) - (0.158*dxa_test_female$hip_circ/10) - (3.174*dxa_test_female$log_skin))*1000, 
            dxa_test_female[,'ha1q34_9btotal_lean'],
            plotTitle = "Total lean mass (g) predictions, Bharati, female", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")



# Age-stratified
tlm1a <- train_test_perf_age(1000*dxa_train_male[,'ha1q37_5iii_tbf_free_mass'], 
                             1000*dxa_test_male[,'ha1q37_5iii_tbf_free_mass'], 
                             'ha1q34_9btotal_lean', 'male', age = "<40")
tlm2a <- train_test_perf_age(predict_enet(tot_lean_male_lasso, dxa_train_male), 
                             predict_enet(tot_lean_male_lasso, dxa_test_male),
                             'ha1q34_9btotal_lean', 'male', age = "<40")
tlm3a <- train_test_perf_age(predict(tot_lean_male_rf, dxa_train_male)$predictions, 
                             predict(tot_lean_male_rf, dxa_test_male)$predictions,
                             'ha1q34_9btotal_lean', 'male', age = "<40")
tlm4a <- train_test_perf_age(predict(tot_lean_male_xgb, dxa_train_male), 
                             predict(tot_lean_male_xgb, dxa_test_male),
                             'ha1q34_9btotal_lean', 'male', age = "<40")

tlm5a <- train_test_perf_age(1000*dxa_train_female[,'ha1q37_5iii_tbf_free_mass'], 
                             1000*dxa_test_female[,'ha1q37_5iii_tbf_free_mass'], 
                             'ha1q34_9btotal_lean', 'female', age = "<40")
tlm6a <- train_test_perf_age(predict_enet(tot_lean_female_lasso, dxa_train_female), 
                             predict_enet(tot_lean_female_lasso, dxa_test_female),
                             'ha1q34_9btotal_lean', 'female', age = "<40")
tlm7a <- train_test_perf_age(predict(tot_lean_female_rf, dxa_train_female)$predictions, 
                             predict(tot_lean_female_rf, dxa_test_female)$predictions,
                             'ha1q34_9btotal_lean', 'female', age = "<40")
tlm8a <- train_test_perf_age(predict(tot_lean_female_xgb, dxa_train_female), 
                             predict(tot_lean_female_xgb, dxa_test_female),
                             'ha1q34_9btotal_lean', 'female', age = "<40")

tlm1b <- train_test_perf_age(1000*dxa_train_male[,'ha1q37_5iii_tbf_free_mass'], 
                             1000*dxa_test_male[,'ha1q37_5iii_tbf_free_mass'], 
                             'ha1q34_9btotal_lean', 'male', age = "40+")
tlm2b <- train_test_perf_age(predict_enet(tot_lean_male_lasso, dxa_train_male), 
                             predict_enet(tot_lean_male_lasso, dxa_test_male),
                             'ha1q34_9btotal_lean', 'male', age = "40+")
tlm3b <- train_test_perf_age(predict(tot_lean_male_rf, dxa_train_male)$predictions, 
                             predict(tot_lean_male_rf, dxa_test_male)$predictions,
                             'ha1q34_9btotal_lean', 'male', age = "40+")
tlm4b <- train_test_perf_age(predict(tot_lean_male_xgb, dxa_train_male), 
                             predict(tot_lean_male_xgb, dxa_test_male),
                             'ha1q34_9btotal_lean', 'male', age = "40+")

tlm5b <- train_test_perf_age(1000*dxa_train_female[,'ha1q37_5iii_tbf_free_mass'], 
                             1000*dxa_test_female[,'ha1q37_5iii_tbf_free_mass'], 
                             'ha1q34_9btotal_lean', 'female', age = "40+")
tlm6b <- train_test_perf_age(predict_enet(tot_lean_female_lasso, dxa_train_female), 
                             predict_enet(tot_lean_female_lasso, dxa_test_female),
                             'ha1q34_9btotal_lean', 'female', age = "40+")
tlm7b <- train_test_perf_age(predict(tot_lean_female_rf, dxa_train_female)$predictions, 
                             predict(tot_lean_female_rf, dxa_test_female)$predictions,
                             'ha1q34_9btotal_lean', 'female', age = "40+")
tlm8b <- train_test_perf_age(predict(tot_lean_female_xgb, dxa_train_female), 
                             predict(tot_lean_female_xgb, dxa_test_female),
                             'ha1q34_9btotal_lean', 'female', age = "40+")


totalleanresults_u40 <- rbind(tlm1a, tlm2a, tlm3a, tlm4a, tlm5a, 
                              tlm6a, tlm7a, tlm8a)
totalleanresults_o40 <- rbind(tlm1b, tlm2b, tlm3b, tlm4b, tlm5b, 
                              tlm6b, tlm7b, tlm8b)


# Outcome = total fat %, ha1q34_9dtotal_pcent_fat


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

# plots
p1 = blandr.draw(predict_enet(tot_p_male_lasso, dxa_test_male), 
                 dxa_test_male[,'ha1q34_9dtotal_pcent_fat'],
                 plotTitle = "Total fat %, male, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p2 = blandr.draw(predict_enet(tot_p_female_lasso, dxa_test_female), 
                 dxa_test_female[,'ha1q34_9dtotal_pcent_fat'],
                 plotTitle = "Total fat %, female, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")


p3 = blandr.draw(dxa_test_male[,'ha1q37_5i_tbf_pcent'], 
                 dxa_test_male[,'ha1q34_9dtotal_pcent_fat'],
                 plotTitle = "Total fat %, male, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p4 = blandr.draw(dxa_test_female[,'ha1q37_5i_tbf_pcent'], 
                 dxa_test_female[,'ha1q34_9dtotal_pcent_fat'],
                 plotTitle = "Total fat %, female, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")


# Display 
jpeg("totalfatpmale.jpg", width = 660, height = "411")
grid.arrange(p1, p3, ncol = 2)
dev.off()

jpeg("totalfatpfemale.jpg", width = 660, height = "411")
grid.arrange(p2, p4, ncol = 2)
dev.off()

# Age-stratified
# Performance
tfp1a <- train_test_perf_age(dxa_train_male[,'ha1q37_5i_tbf_pcent'], 
                        dxa_test_male[,'ha1q37_5i_tbf_pcent'], 
                        'ha1q34_9dtotal_pcent_fat', 'male',  age = "<40")
tfp2a <- train_test_perf_age(predict_enet(tot_p_male_lasso, dxa_train_male), 
                        predict_enet(tot_p_male_lasso, dxa_test_male),
                        'ha1q34_9dtotal_pcent_fat', 'male',  age = "<40")
tfp3a <- train_test_perf_age(predict(tot_p_male_rf, dxa_train_male)$predictions, 
                        predict(tot_p_male_rf, dxa_test_male)$predictions,
                        'ha1q34_9dtotal_pcent_fat', 'male', age = "<40")
tfp4a <- train_test_perf_age(predict(tot_p_male_xgb, dxa_train_male), 
                        predict(tot_p_male_xgb, dxa_test_male),
                        'ha1q34_9dtotal_pcent_fat', 'male', age = "<40")


tfp5a <- train_test_perf_age(dxa_train_female[,'ha1q37_5i_tbf_pcent'], 
                        dxa_test_female[,'ha1q37_5i_tbf_pcent'], 
                        'ha1q34_9dtotal_pcent_fat', 'female', age = "<40")
tfp6a <- train_test_perf_age(predict_enet(tot_p_female_lasso, dxa_train_female), 
                        predict_enet(tot_p_female_lasso, dxa_test_female),
                        'ha1q34_9dtotal_pcent_fat', 'female', age = "<40")
tfp7a <- train_test_perf_age(predict(tot_p_female_rf, dxa_train_female)$predictions, 
                        predict(tot_p_female_rf, dxa_test_female)$predictions,
                        'ha1q34_9dtotal_pcent_fat', 'female', age = "<40")
tfp8a <- train_test_perf_age(predict(tot_p_female_xgb, dxa_train_female), 
                        predict(tot_p_female_xgb, dxa_test_female),
                        'ha1q34_9dtotal_pcent_fat', 'female', age = "<40")

totalfatpresults_u40 <- rbind(tfp1a, tfp2a, tfp3a, tfp4a, tfp5a, tfp6a, tfp7a, tfp8a)

tfp1b <- train_test_perf_age(dxa_train_male[,'ha1q37_5i_tbf_pcent'], 
                             dxa_test_male[,'ha1q37_5i_tbf_pcent'], 
                             'ha1q34_9dtotal_pcent_fat', 'male',  age = "40+")
tfp2b <- train_test_perf_age(predict_enet(tot_p_male_lasso, dxa_train_male), 
                             predict_enet(tot_p_male_lasso, dxa_test_male),
                             'ha1q34_9dtotal_pcent_fat', 'male',  age = "40+")
tfp3b <- train_test_perf_age(predict(tot_p_male_rf, dxa_train_male)$predictions, 
                             predict(tot_p_male_rf, dxa_test_male)$predictions,
                             'ha1q34_9dtotal_pcent_fat', 'male', age = "40+")
tfp4b <- train_test_perf_age(predict(tot_p_male_xgb, dxa_train_male), 
                             predict(tot_p_male_xgb, dxa_test_male),
                             'ha1q34_9dtotal_pcent_fat', 'male', age = "40+")


tfp5b <- train_test_perf_age(dxa_train_female[,'ha1q37_5i_tbf_pcent'], 
                             dxa_test_female[,'ha1q37_5i_tbf_pcent'], 
                             'ha1q34_9dtotal_pcent_fat', 'female', age = "40+")
tfp6b <- train_test_perf_age(predict_enet(tot_p_female_lasso, dxa_train_female), 
                             predict_enet(tot_p_female_lasso, dxa_test_female),
                             'ha1q34_9dtotal_pcent_fat', 'female', age = "40+")
tfp7b <- train_test_perf_age(predict(tot_p_female_rf, dxa_train_female)$predictions, 
                             predict(tot_p_female_rf, dxa_test_female)$predictions,
                             'ha1q34_9dtotal_pcent_fat', 'female', age = "40+")
tfp8b <- train_test_perf_age(predict(tot_p_female_xgb, dxa_train_female), 
                             predict(tot_p_female_xgb, dxa_test_female),
                             'ha1q34_9dtotal_pcent_fat', 'female', age = "40+")

totalfatpresults_o40 <- rbind(tfp1b, tfp2b, tfp3b, tfp4b, tfp5b, tfp6b, tfp7b, tfp8b)

# Outcome = Trunk percentage fat mass, ha1q34_6dtrunk_pcent_fat

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

# Plots
p1 = blandr.draw(predict_enet(trunk_fat_male_lasso, dxa_test_male), 
                 dxa_test_male[,'ha1q34_6dtrunk_pcent_fat'],
                 plotTitle = "Trunk fat %, male, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p2 = blandr.draw(predict_enet(trunk_fat_female_lasso, dxa_test_female), 
                 dxa_test_female[,'ha1q34_6dtrunk_pcent_fat'],
                 plotTitle = "Trunk fat %, female, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")


p3 = blandr.draw(dxa_test_male[,'ha1q37_11i_seg_tr_pcent'], 
                 dxa_test_male[,'ha1q34_6dtrunk_pcent_fat'],
                 plotTitle = "Trunk fat %, male, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p4 = blandr.draw(dxa_test_female[,'ha1q37_11i_seg_tr_pcent'], 
                 dxa_test_female[,'ha1q34_6dtrunk_pcent_fat'],
                 plotTitle = "Trunk fat %, female, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")

trunkpercent <- rbind(trp1, trp2, trp3, trp4, trp5, trp6, trp7, trp8)


# Display 
jpeg("trunkfatmale.jpg", width = 660, height = "411")
grid.arrange(p1, p3, ncol = 2)
dev.off()

jpeg("trunkfatfemale.jpg", width = 660, height = "411")
grid.arrange(p2, p4, ncol = 2)
dev.off()



# Age stratified
trp1a <- train_test_perf_age(dxa_train_male[,'ha1q37_11i_seg_tr_pcent'], 
                        dxa_test_male[,'ha1q37_11i_seg_tr_pcent'], 
                        'ha1q34_6dtrunk_pcent_fat', 'male', age = "<40")
trp2a <- train_test_perf_age(predict_enet(trunk_fat_male_lasso, dxa_train_male), 
                        predict_enet(trunk_fat_male_lasso, dxa_test_male),
                        'ha1q34_6dtrunk_pcent_fat', 'male', age = "<40")
trp3a <- train_test_perf_age(predict(trunk_fat_male_rf, dxa_train_male)$predictions, 
                        predict(trunk_fat_male_rf, dxa_test_male)$predictions,
                        'ha1q34_6dtrunk_pcent_fat', 'male', age = "<40")
trp4a <- train_test_perf_age(predict(trunk_fat_male_xgb, dxa_train_male), 
                        predict(trunk_fat_male_xgb, dxa_test_male),
                        'ha1q34_6dtrunk_pcent_fat', 'male', age = "<40")


trp5a <- train_test_perf_age(dxa_train_female[,'ha1q37_11i_seg_tr_pcent'], 
                        dxa_test_female[,'ha1q37_11i_seg_tr_pcent'], 
                        'ha1q34_6dtrunk_pcent_fat', 'female', age = "<40")
trp6a <- train_test_perf_age(predict_enet(trunk_fat_female_lasso, dxa_train_female), 
                        predict_enet(trunk_fat_female_lasso, dxa_test_female),
                        'ha1q34_6dtrunk_pcent_fat', 'female', age = "<40")
trp7a <- train_test_perf_age(predict(trunk_fat_female_rf, dxa_train_female)$predictions, 
                        predict(trunk_fat_female_rf, dxa_test_female)$predictions,
                        'ha1q34_6dtrunk_pcent_fat', 'female', age = "<40")
trp8a <- train_test_perf_age(predict(trunk_fat_female_xgb, dxa_train_female), 
                        predict(trunk_fat_female_xgb, dxa_test_female),
                        'ha1q34_6dtrunk_pcent_fat', 'female', age = "<40")

trunkpercent_u40 <- rbind(trp1a, trp2a, trp3a, trp4a, trp5a, trp6a, trp7a, trp8a)

trp1b <- train_test_perf_age(dxa_train_male[,'ha1q37_11i_seg_tr_pcent'], 
                             dxa_test_male[,'ha1q37_11i_seg_tr_pcent'], 
                             'ha1q34_6dtrunk_pcent_fat', 'male', age = "40+")
trp2b <- train_test_perf_age(predict_enet(trunk_fat_male_lasso, dxa_train_male), 
                             predict_enet(trunk_fat_male_lasso, dxa_test_male),
                             'ha1q34_6dtrunk_pcent_fat', 'male', age = "40+")
trp3b <- train_test_perf_age(predict(trunk_fat_male_rf, dxa_train_male)$predictions, 
                             predict(trunk_fat_male_rf, dxa_test_male)$predictions,
                             'ha1q34_6dtrunk_pcent_fat', 'male', age = "40+")
trp4b <- train_test_perf_age(predict(trunk_fat_male_xgb, dxa_train_male), 
                             predict(trunk_fat_male_xgb, dxa_test_male),
                             'ha1q34_6dtrunk_pcent_fat', 'male', age = "40+")


trp5b <- train_test_perf_age(dxa_train_female[,'ha1q37_11i_seg_tr_pcent'], 
                             dxa_test_female[,'ha1q37_11i_seg_tr_pcent'], 
                             'ha1q34_6dtrunk_pcent_fat', 'female', age = "40+")
trp6b <- train_test_perf_age(predict_enet(trunk_fat_female_lasso, dxa_train_female), 
                             predict_enet(trunk_fat_female_lasso, dxa_test_female),
                             'ha1q34_6dtrunk_pcent_fat', 'female', age = "40+")
trp7b <- train_test_perf_age(predict(trunk_fat_female_rf, dxa_train_female)$predictions, 
                             predict(trunk_fat_female_rf, dxa_test_female)$predictions,
                             'ha1q34_6dtrunk_pcent_fat', 'female', age = "40+")
trp8b <- train_test_perf_age(predict(trunk_fat_female_xgb, dxa_train_female), 
                             predict(trunk_fat_female_xgb, dxa_test_female),
                             'ha1q34_6dtrunk_pcent_fat', 'female', age = "40+")

trunkpercent_o40 <- rbind(trp1b, trp2b, trp3b, trp4b, trp5b, trp6b, trp7b, trp8b)


# Outcome = L1-L4 trunk fat percentage, ha1q34_12dl1l4_pcent1

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

# Plots
p1 = blandr.draw(predict_enet(l1_fat_male_lasso, dxa_test_male), 
                 dxa_test_male[,'ha1q34_12dl1l4_pcent1'],
                 plotTitle = "L1-L4 fat %, male, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p2 = blandr.draw(predict_enet(l1_fat_female_lasso, dxa_test_female), 
                 dxa_test_female[,'ha1q34_12dl1l4_pcent1'],
                 plotTitle = "L1-L4 fat %, female, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")


p3 = blandr.draw(dxa_test_male[,'ha1q37_11i_seg_tr_pcent'],
                 dxa_test_male[,'ha1q34_12dl1l4_pcent1'],
                 plotTitle = "L1-L4 fat %, male, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p4 = blandr.draw(dxa_test_female[,'ha1q37_11i_seg_tr_pcent'],
                 dxa_test_female[,'ha1q34_12dl1l4_pcent1'],
                 plotTitle = "L1-L4 fat %, female, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")


# Display 
jpeg("l1fatmale.jpg", width = 660, height = "411")
grid.arrange(p1, p3, ncol = 2)
dev.off()

jpeg("l1fatfemale.jpg", width = 660, height = "411")
grid.arrange(p2, p4, ncol = 2)
dev.off()


# Stratified by age 
l11a <- train_test_perf_age(dxa_train_male[,'ha1q37_11i_seg_tr_pcent'], 
                       dxa_test_male[,'ha1q37_11i_seg_tr_pcent'], 
                       'ha1q34_12dl1l4_pcent1', 'male', age = "<40")
l12a <- train_test_perf_age(predict_enet(l1_fat_male_lasso, dxa_train_male), 
                       predict_enet(l1_fat_male_lasso, dxa_test_male),
                       'ha1q34_12dl1l4_pcent1', 'male', age = "<40")
l13a <- train_test_perf_age(predict(l1_fat_male_rf, dxa_train_male)$predictions, 
                       predict(l1_fat_male_rf, dxa_test_male)$predictions,
                       'ha1q34_12dl1l4_pcent1', 'male', age = "<40")
l14a <- train_test_perf_age(predict(l1_fat_male_xgb, dxa_train_male), 
                       predict(l1_fat_male_xgb, dxa_test_male),
                       'ha1q34_12dl1l4_pcent1', 'male', age = "<40")


l15a <- train_test_perf_age(dxa_train_female[,'ha1q37_11i_seg_tr_pcent'], 
                       dxa_test_female[,'ha1q37_11i_seg_tr_pcent'], 
                       'ha1q34_12dl1l4_pcent1', 'female', age = "<40")
l16a <- train_test_perf_age(predict_enet(l1_fat_female_lasso, dxa_train_female), 
                       predict_enet(l1_fat_female_lasso, dxa_test_female),
                       'ha1q34_12dl1l4_pcent1', 'female', age = "<40")
l17a <- train_test_perf_age(predict(l1_fat_female_rf, dxa_train_female)$predictions, 
                       predict(l1_fat_female_rf, dxa_test_female)$predictions,
                       'ha1q34_12dl1l4_pcent1', 'female', age = "<40")
l18a <- train_test_perf_age(predict(l1_fat_female_xgb, dxa_train_female), 
                       predict(l1_fat_female_xgb, dxa_test_female),
                       'ha1q34_12dl1l4_pcent1', 'female', age = "<40")

l1results_u40 <- rbind(l11a, l12a, l13a, l14a, l15a, l16a, l17a, l18a)


l11b <- train_test_perf_age(dxa_train_male[,'ha1q37_11i_seg_tr_pcent'], 
                            dxa_test_male[,'ha1q37_11i_seg_tr_pcent'], 
                            'ha1q34_12dl1l4_pcent1', 'male', age = "40+")
l12b <- train_test_perf_age(predict_enet(l1_fat_male_lasso, dxa_train_male), 
                            predict_enet(l1_fat_male_lasso, dxa_test_male),
                            'ha1q34_12dl1l4_pcent1', 'male', age = "40+")
l13b <- train_test_perf_age(predict(l1_fat_male_rf, dxa_train_male)$predictions, 
                            predict(l1_fat_male_rf, dxa_test_male)$predictions,
                            'ha1q34_12dl1l4_pcent1', 'male', age = "40+")
l14b <- train_test_perf_age(predict(l1_fat_male_xgb, dxa_train_male), 
                            predict(l1_fat_male_xgb, dxa_test_male),
                            'ha1q34_12dl1l4_pcent1', 'male', age = "40+")


l15b <- train_test_perf_age(dxa_train_female[,'ha1q37_11i_seg_tr_pcent'], 
                            dxa_test_female[,'ha1q37_11i_seg_tr_pcent'], 
                            'ha1q34_12dl1l4_pcent1', 'female', age = "40+")
l16b <- train_test_perf_age(predict_enet(l1_fat_female_lasso, dxa_train_female), 
                            predict_enet(l1_fat_female_lasso, dxa_test_female),
                            'ha1q34_12dl1l4_pcent1', 'female', age = "40+")
l17b <- train_test_perf_age(predict(l1_fat_female_rf, dxa_train_female)$predictions, 
                            predict(l1_fat_female_rf, dxa_test_female)$predictions,
                            'ha1q34_12dl1l4_pcent1', 'female', age = "40+")
l18b <- train_test_perf_age(predict(l1_fat_female_xgb, dxa_train_female), 
                            predict(l1_fat_female_xgb, dxa_test_female),
                            'ha1q34_12dl1l4_pcent1', 'female', age = "40+")

l1results_o40 <- rbind(l11b, l12b, l13b, l14b, l15b, l16b, l17b, l18b)


# Outcome = appendicular lean mass, appendic_lean


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

# Can also use Bharati's equations
ap9 <- train_test_perf((as.numeric(-0.996 - (0.023*dxa_train_male$ha1dv_age) + (0.090*dxa_train_male$height/10)+ (0.274*dxa_train_male$ha1q37_2_weight) + (0.143*dxa_train_male$arm_circ/10) + (0.223*dxa_train_male$calf_circ/10) - (0.104*dxa_train_male$hip_circ/10) - (3.163*dxa_train_male$log_skin))*1000), 
                       (as.numeric(-0.996 - (0.023*dxa_test_male$ha1dv_age) + (0.090*dxa_test_male$height/10)+ (0.274*dxa_test_male$ha1q37_2_weight) + (0.143*dxa_test_male$arm_circ/10) + (0.223*dxa_test_male$calf_circ/10) - (0.104*dxa_test_male$hip_circ/10) - (3.163*dxa_test_male$log_skin))*1000),
                       'appendic_lean', 'male')

ap10 <- train_test_perf(as.numeric(1.609 - (0.021*dxa_train_female$ha1dv_age) + (0.070*dxa_train_female$height/10)+ (0.250*dxa_train_female$ha1q37_2_weight) + (0.027*dxa_train_female$arm_circ/10) + (0.098*dxa_train_female$calf_circ/10) - (0.085*dxa_train_female$hip_circ/10) - (1.821*dxa_train_female$log_skin))*1000, 
                        as.numeric(1.609 - (0.021*dxa_test_female$ha1dv_age) + (0.070*dxa_test_female$height/10)+ (0.250*dxa_test_female$ha1q37_2_weight) + (0.027*dxa_test_female$arm_circ/10) + (0.098*dxa_test_female$calf_circ/10) - (0.085*dxa_test_female$hip_circ/10) - (1.821*dxa_test_female$log_skin))*1000,
                        'appendic_lean', 'female')

apresults <- rbind(ap1, ap2, ap3, ap4, ap5,
                   ap6, ap7, ap8, ap9, ap10)

# Can also include plots to explore performance for each model-outcome-sex pair.


# Plots
p1 = blandr.draw(predict_enet(app_lean_male_lasso, dxa_test_male), 
                 dxa_test_male[,'appendic_lean'],
                 plotTitle = "Appendicular lean mass (g), male, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p2 = blandr.draw(predict_enet(app_lean_female_lasso, dxa_test_female), 
                 dxa_test_female[,'appendic_lean'],
                 plotTitle = "Appendicular lean mass (g), female, Full LASSO", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")


p3 = blandr.draw(1000*dxa_test_male[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_male[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_male[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_male[,'ha1q37_10iii_seg_la_free_mass'], 
                 dxa_test_male[,'appendic_lean'],
                 plotTitle = "Appendicular lean mass (g), male, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")
p4 = blandr.draw(1000*dxa_test_female[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_female[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_female[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_female[,'ha1q37_10iii_seg_la_free_mass'], 
                 dxa_test_female[,'appendic_lean'],
                 plotTitle = "Appendicular lean mass (g), female, Tanita", ciShading = FALSE, ciDisplay = FALSE) + geom_smooth(method = "lm")

# Display 
jpeg("apfatmale.jpg", width = 660, height = "411")
grid.arrange(p1, p3, ncol = 2)
dev.off()

jpeg("apfatfemale.jpg", width = 660, height = "411")
grid.arrange(p2, p4, ncol = 2)
dev.off()


# Age stratified
ap1a <- train_test_perf_age(1000*dxa_train_male[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_train_male[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_train_male[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_train_male[,'ha1q37_10iii_seg_la_free_mass'], 
                       1000*dxa_test_male[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_male[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_male[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_male[,'ha1q37_10iii_seg_la_free_mass'], 
                       'appendic_lean', 'male', age = "<40")
ap2a <- train_test_perf_age(predict_enet(app_lean_male_lasso, dxa_train_male), 
                       predict_enet(app_lean_male_lasso, dxa_test_male),
                       'appendic_lean', 'male', age = "<40")
ap3a <- train_test_perf_age(predict(app_lean_male_rf, dxa_train_male)$predictions, 
                       predict(app_lean_male_rf, dxa_test_male)$predictions,
                       'appendic_lean', 'male', age = "<40")
ap4a <- train_test_perf_age(predict(app_lean_male_xgb, dxa_train_male), 
                       predict(app_lean_male_xgb, dxa_test_male),
                       'appendic_lean', 'male', age = "<40")


ap5a <- train_test_perf_age(1000*dxa_train_female[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_train_female[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_train_female[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_train_female[,'ha1q37_10iii_seg_la_free_mass'], 
                       1000*dxa_test_female[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_female[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_female[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_female[,'ha1q37_10iii_seg_la_free_mass'], 
                       'appendic_lean', 'female', age = "<40")
ap6a <- train_test_perf_age(predict_enet(app_lean_female_lasso, dxa_train_female), 
                       predict_enet(app_lean_female_lasso, dxa_test_female),
                       'appendic_lean', 'female', age = "<40")
ap7a <- train_test_perf_age(predict(app_lean_female_rf, dxa_train_female)$predictions, 
                       predict(app_lean_female_rf, dxa_test_female)$predictions,
                       'appendic_lean', 'female', age = "<40")
ap8a <- train_test_perf_age(predict(app_lean_female_xgb, dxa_train_female), 
                       predict(app_lean_female_xgb, dxa_test_female),
                       'appendic_lean', 'female', age = "<40")

apresults_u40 <- rbind(ap1a, ap2a, ap3a, ap4a, ap5a,
                   ap6a, ap7a, ap8a)


ap1b <- train_test_perf_age(1000*dxa_train_male[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_train_male[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_train_male[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_train_male[,'ha1q37_10iii_seg_la_free_mass'], 
                            1000*dxa_test_male[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_male[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_male[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_male[,'ha1q37_10iii_seg_la_free_mass'], 
                            'appendic_lean', 'male', age = "40+")
ap2b <- train_test_perf_age(predict_enet(app_lean_male_lasso, dxa_train_male), 
                            predict_enet(app_lean_male_lasso, dxa_test_male),
                            'appendic_lean', 'male', age = "40+")
ap3b <- train_test_perf_age(predict(app_lean_male_rf, dxa_train_male)$predictions, 
                            predict(app_lean_male_rf, dxa_test_male)$predictions,
                            'appendic_lean', 'male', age = "40+")
ap4b <- train_test_perf_age(predict(app_lean_male_xgb, dxa_train_male), 
                            predict(app_lean_male_xgb, dxa_test_male),
                            'appendic_lean', 'male', age = "40+")


ap5b <- train_test_perf_age(1000*dxa_train_female[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_train_female[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_train_female[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_train_female[,'ha1q37_10iii_seg_la_free_mass'], 
                            1000*dxa_test_female[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_female[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_female[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_female[,'ha1q37_10iii_seg_la_free_mass'], 
                            'appendic_lean', 'female', age = "40+")
ap6b <- train_test_perf_age(predict_enet(app_lean_female_lasso, dxa_train_female), 
                            predict_enet(app_lean_female_lasso, dxa_test_female),
                            'appendic_lean', 'female', age = "40+")
ap7b <- train_test_perf_age(predict(app_lean_female_rf, dxa_train_female)$predictions, 
                            predict(app_lean_female_rf, dxa_test_female)$predictions,
                            'appendic_lean', 'female', age = "40+")
ap8b <- train_test_perf_age(predict(app_lean_female_xgb, dxa_train_female), 
                            predict(app_lean_female_xgb, dxa_test_female),
                            'appendic_lean', 'female', age = "40+")

apresults_o40 <- rbind(ap1b, ap2b, ap3b, ap4b, ap5b,
                       ap6b, ap7b, ap8b)


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
write.csv(totalfatresults, 'totalfatmasslessints.csv')
write.csv(totalleanresults, 'totalleanmasslessints.csv')
write.csv(totalfatpresults, 'totalfatpercentlessints.csv')
write.csv(trunkpercent, 'trunkfatpercentlessints.csv')
write.csv(l1results, 'l1l4fatpercentlessints.csv')
write.csv(apresults, 'appleanmasslessints.csv')



# Further processing for age-stratified results
totalfatresults_u40[,1] <- round(totalfatresults_u40[,1]/1000, 2)
totalfatresults_u40[,2] <- round(totalfatresults_u40[,2]/1000, 3)
totalfatresults_u40[,3] <- round(totalfatresults_u40[,3]*100, 2)
totalfatresults_u40[,4] <- round(totalfatresults_u40[,4], 3)
totalfatresults_u40[,5] <- round(totalfatresults_u40[,5]/1000, 2)
totalfatresults_u40[,6] <- round(totalfatresults_u40[,6]/1000, 3)
totalfatresults_u40[,7] <- round(totalfatresults_u40[,7]*100, 2)
totalfatresults_u40[,8] <- round(totalfatresults_u40[,8], 3)

totalfatresults_o40[,1] <- round(totalfatresults_o40[,1]/1000, 2)
totalfatresults_o40[,2] <- round(totalfatresults_o40[,2]/1000, 3)
totalfatresults_o40[,3] <- round(totalfatresults_o40[,3]*100, 2)
totalfatresults_o40[,4] <- round(totalfatresults_o40[,4], 3)
totalfatresults_o40[,5] <- round(totalfatresults_o40[,5]/1000, 2)
totalfatresults_o40[,6] <- round(totalfatresults_o40[,6]/1000, 3)
totalfatresults_o40[,7] <- round(totalfatresults_o40[,7]*100, 2)
totalfatresults_o40[,8] <- round(totalfatresults_o40[,8], 3)



totalleanresults_u40[,1] <- round(totalleanresults_u40[,1]/1000, 2)
totalleanresults_u40[,2] <- round(totalleanresults_u40[,2]/1000, 3)
totalleanresults_u40[,3] <- round(totalleanresults_u40[,3]*100, 2)
totalleanresults_u40[,4] <- round(totalleanresults_u40[,4], 3)
totalleanresults_u40[,5] <- round(totalleanresults_u40[,5]/1000, 2)
totalleanresults_u40[,6] <- round(totalleanresults_u40[,6]/1000, 3)
totalleanresults_u40[,7] <- round(totalleanresults_u40[,7]*100, 2)
totalleanresults_u40[,8] <- round(totalleanresults_u40[,8], 3)

totalleanresults_o40[,1] <- round(totalleanresults_o40[,1]/1000, 2)
totalleanresults_o40[,2] <- round(totalleanresults_o40[,2]/1000, 3)
totalleanresults_o40[,3] <- round(totalleanresults_o40[,3]*100, 2)
totalleanresults_o40[,4] <- round(totalleanresults_o40[,4], 3)
totalleanresults_o40[,5] <- round(totalleanresults_o40[,5]/1000, 2)
totalleanresults_o40[,6] <- round(totalleanresults_o40[,6]/1000, 3)
totalleanresults_o40[,7] <- round(totalleanresults_o40[,7]*100, 2)
totalleanresults_o40[,8] <- round(totalleanresults_o40[,8], 3)


apresults_u40[,1] <- round(apresults_u40[,1]/1000, 2)
apresults_u40[,2] <- round(apresults_u40[,2]/1000, 3)
apresults_u40[,3] <- round(apresults_u40[,3]*100, 2)
apresults_u40[,4] <- round(apresults_u40[,4], 3)
apresults_u40[,5] <- round(apresults_u40[,5]/1000, 2)
apresults_u40[,6] <- round(apresults_u40[,6]/1000, 3)
apresults_u40[,7] <- round(apresults_u40[,7]*100, 2)
apresults_u40[,8] <- round(apresults_u40[,8], 3)

apresults_o40[,1] <- round(apresults_o40[,1]/1000, 2)
apresults_o40[,2] <- round(apresults_o40[,2]/1000, 3)
apresults_o40[,3] <- round(apresults_o40[,3]*100, 2)
apresults_o40[,4] <- round(apresults_o40[,4], 3)
apresults_o40[,5] <- round(apresults_o40[,5]/1000, 2)
apresults_o40[,6] <- round(apresults_o40[,6]/1000, 3)
apresults_o40[,7] <- round(apresults_o40[,7]*100, 2)
apresults_o40[,8] <- round(apresults_o40[,8], 3)


totalfatpresults_u40[,1] <- round(totalfatpresults_u40[,1], 3)
totalfatpresults_u40[,2] <- round(totalfatpresults_u40[,2], 3)
totalfatpresults_u40[,3] <- round(totalfatpresults_u40[,3]*100, 2)
totalfatpresults_u40[,4] <- round(totalfatpresults_u40[,4], 3)
totalfatpresults_u40[,5] <- round(totalfatpresults_u40[,5], 3)
totalfatpresults_u40[,6] <- round(totalfatpresults_u40[,6], 3)
totalfatpresults_u40[,7] <- round(totalfatpresults_u40[,7]*100, 2)
totalfatpresults_u40[,8] <- round(totalfatpresults_u40[,8], 3)

totalfatpresults_o40[,1] <- round(totalfatpresults_o40[,1], 3)
totalfatpresults_o40[,2] <- round(totalfatpresults_o40[,2], 3)
totalfatpresults_o40[,3] <- round(totalfatpresults_o40[,3]*100, 2)
totalfatpresults_o40[,4] <- round(totalfatpresults_o40[,4], 3)
totalfatpresults_o40[,5] <- round(totalfatpresults_o40[,5], 3)
totalfatpresults_o40[,6] <- round(totalfatpresults_o40[,6], 3)
totalfatpresults_o40[,7] <- round(totalfatpresults_o40[,7]*100, 2)
totalfatpresults_o40[,8] <- round(totalfatpresults_o40[,8], 3)

trunkpercent_u40[,1] <- round(trunkpercent_u40[,1], 3)
trunkpercent_u40[,2] <- round(trunkpercent_u40[,2], 3)
trunkpercent_u40[,3] <- round(trunkpercent_u40[,3]*100, 2)
trunkpercent_u40[,4] <- round(trunkpercent_u40[,4], 3)
trunkpercent_u40[,5] <- round(trunkpercent_u40[,5], 3)
trunkpercent_u40[,6] <- round(trunkpercent_u40[,6], 3)
trunkpercent_u40[,7] <- round(trunkpercent_u40[,7]*100, 2)
trunkpercent_u40[,8] <- round(trunkpercent_u40[,8], 3)

trunkpercent_o40[,1] <- round(trunkpercent_o40[,1], 3)
trunkpercent_o40[,2] <- round(trunkpercent_o40[,2], 3)
trunkpercent_o40[,3] <- round(trunkpercent_o40[,3]*100, 2)
trunkpercent_o40[,4] <- round(trunkpercent_o40[,4], 3)
trunkpercent_o40[,5] <- round(trunkpercent_o40[,5], 3)
trunkpercent_o40[,6] <- round(trunkpercent_o40[,6], 3)
trunkpercent_o40[,7] <- round(trunkpercent_o40[,7]*100, 2)
trunkpercent_o40[,8] <- round(trunkpercent_o40[,8], 3)

l1results_u40[,1] <- round(l1results_u40[,1], 3)
l1results_u40[,2] <- round(l1results_u40[,2], 3)
l1results_u40[,3] <- round(l1results_u40[,3]*100, 2)
l1results_u40[,4] <- round(l1results_u40[,4], 3)
l1results_u40[,5] <- round(l1results_u40[,5], 3)
l1results_u40[,6] <- round(l1results_u40[,6], 3)
l1results_u40[,7] <- round(l1results_u40[,7]*100, 2)
l1results_u40[,8] <- round(l1results_u40[,8], 3)

l1results_o40[,1] <- round(l1results_o40[,1], 3)
l1results_o40[,2] <- round(l1results_o40[,2], 3)
l1results_o40[,3] <- round(l1results_o40[,3]*100, 2)
l1results_o40[,4] <- round(l1results_o40[,4], 3)
l1results_o40[,5] <- round(l1results_o40[,5], 3)
l1results_o40[,6] <- round(l1results_o40[,6], 3)
l1results_o40[,7] <- round(l1results_o40[,7]*100, 2)
l1results_o40[,8] <- round(l1results_o40[,8], 3)



# EXPORT RESULTS
write.csv(totalfatresults_u40, 'totalfatmasslessints_u40.csv')
write.csv(totalfatresults_o40, 'totalfatmasslessints_o40.csv')
write.csv(totalleanresults_u40, 'totalleanmasslessints_u40.csv')
write.csv(totalleanresults_o40, 'totalleanmasslessints_o40.csv')
write.csv(totalfatpresults_u40, 'totalfatpercentlessints_u40.csv')
write.csv(totalfatpresults_o40, 'totalfatpercentlessints_o40.csv')
write.csv(trunkpercent_u40, 'trunkfatpercentlessints_u40.csv')
write.csv(trunkpercent_o40, 'trunkfatpercentlessints_o40.csv')
write.csv(l1results_u40, 'l1l4fatpercentlessints_u40.csv')
write.csv(l1results_o40, 'l1l4fatpercentlessints_o40.csv')
write.csv(apresults_u40, 'appleanmasslessints_u40.csv')
write.csv(apresults_o40, 'appleanmasslessints_o40.csv')


# Check LASSO Coefficients in aggregate
lasso_coefs <- t(data.frame(app_lean_female_lasso$beta[,1], 
                            app_lean_male_lasso$beta[,1],
                            l1_fat_female_lasso$beta[,1],
                            l1_fat_male_lasso$beta[,1],
                            tot_fat_female_lasso$beta[,1],
                            tot_fat_male_lasso$beta[,1],
                            tot_lean_female_lasso$beta[,1],
                            tot_lean_male_lasso$beta[,1],
                            tot_p_female_lasso$beta[,1],
                            tot_p_male_lasso$beta[,1],
                            trunk_fat_female_lasso$beta[,1],
                            trunk_fat_male_lasso$beta[,1]))


# Random Forest variable importance
rf_importance <- t(data.frame(importance(app_lean_female_rf),
                              importance(app_lean_male_rf),
                              importance(l1_fat_female_rf),
                              importance(l1_fat_male_rf),
                              importance(tot_fat_female_rf),
                              importance(tot_fat_male_rf),
                              importance(tot_lean_female_rf),
                              importance(tot_lean_male_rf),
                              importance(tot_p_female_rf),
                              importance(tot_p_male_rf),
                              importance(trunk_fat_female_rf),
                              importance(trunk_fat_male_rf)))


write.csv(lasso_coefs, 'lasso_coefs.csv')
write.csv(rf_importance, 'rf_importance.csv')

# Can also plot as follows
data.frame(varimp = tot_fat_male_rf$variable.importance/max(tot_fat_male_rf$variable.importance),
           varname = names(tot_fat_male_rf$variable.importance)) %>%
  arrange(-1*varimp) %>% head(10) %>%
  ggplot(aes(reorder(varname, varimp), varimp)) + geom_col() + coord_flip() +
  xlab('Variable name') + ylab('Relative importance')

# Perhaps can write a function to make this easier
plot_topn_rf <- function(rfmod, n, name){
  data.frame(varimp = rfmod$variable.importance/max(rfmod$variable.importance),
             varname = names(rfmod$variable.importance)) %>%
    arrange(-1*varimp) %>% head(n) %>%
    ggplot(aes(reorder(varname, varimp), varimp)) + geom_col() + coord_flip() +
    xlab('Variable name') + ylab('Relative importance') + ggtitle(name)
  
}

# Test with above plot to see if results same
plot_topn_rf(tot_fat_male_rf, 10, 'Male RF total body fat')
# Seems to work as intended!

plot_topn_rf(tot_fat_female_rf, 10, 'Female RF total body fat')

# Great - can compile into Word document to share


# Can make similar plots for LASSO models.

data.frame(stcoefs = as.matrix(coef(tot_fat_female_lasso))[-1, 1] * sapply(dxa_train_female[c(6:50, 53:69)], sd),
           coefnames = names(coef(tot_fat_female_lasso)[-1,1])) %>%
  arrange(-1*abs(stcoefs)) %>% head(10) %>%
  ggplot(aes(reorder(coefnames, abs(stcoefs)), abs(stcoefs))) + geom_col() + coord_flip() + 
  xlab('Variable name') + ylab('Absolute Standardized Coefficient Value')

# Make function to generalize above
plot_topn_lasso_F <- function(lassomod, n, name){
  data.frame(stcoefs = as.matrix(coef(lassomod))[-1, 1] * sapply(dxa_train_female[c(6:50, 53:69)], sd),
             coefnames = names(coef(lassomod)[-1,1])) %>%
    arrange(-1*abs(stcoefs)) %>% head(n) %>%
    ggplot(aes(reorder(coefnames, abs(stcoefs)), abs(stcoefs))) + geom_col() + coord_flip() + 
    xlab('Variable name') + ylab('Absolute Standardized Coefficient Value') +
    ggtitle(name)
  
}

# Need separate function for male and female
plot_topn_lasso_M <- function(lassomod, n, name){
  data.frame(stcoefs = as.matrix(coef(lassomod))[-1, 1] * sapply(dxa_train_male[c(6:50, 53:69)], sd),
             coefnames = names(coef(lassomod)[-1,1])) %>%
    arrange(-1*abs(stcoefs)) %>% head(n) %>%
    ggplot(aes(reorder(coefnames, abs(stcoefs)), abs(stcoefs))) + geom_col() + coord_flip() + 
    xlab('Variable name') + ylab('Absolute Standardized Coefficient Value') +
    ggtitle(name)
  
}

plot_topn_lasso_F(tot_fat_female_lasso, 10, 'LASSO Female total fat')
plot_topn_lasso_M(tot_fat_male_lasso, 10, 'LASSO Male total fat')

# Way to check how many variable coefficients are < tolerance value
sum(as.matrix(abs(coef(tot_fat_male_lasso))[-1, 1]) * sapply(dxa_train_male[c(6:50, 53:69)], sd) < 1)



# Now let's do for each of the outcomes!

# Total fat mass
plot_topn_lasso_F(tot_fat_female_lasso, 15, 'LASSO Female total fat')
plot_topn_lasso_M(tot_fat_male_lasso, 15, 'LASSO Male total fat')
plot_topn_rf(tot_fat_male_rf, 15, 'Male RF total body fat')
plot_topn_rf(tot_fat_female_rf, 15, 'Female RF total body fat')
sum(as.matrix(abs(coef(tot_fat_male_lasso))[-1, 1]) * sapply(dxa_train_male[c(6:50, 53:69)], sd) == 0)
sum(as.matrix(abs(coef(tot_fat_female_lasso))[-1, 1]) * sapply(dxa_train_female[c(6:50, 53:69)], sd) == 0)


# Total lean mass
plot_topn_lasso_F(tot_lean_female_lasso, 15, 'LASSO Female total lean')
plot_topn_lasso_M(tot_lean_male_lasso, 15, 'LASSO Male total lean')
plot_topn_rf(tot_lean_male_rf, 15, 'Male RF total body lean')
plot_topn_rf(tot_lean_female_rf, 15, 'Female RF total body lean')
sum(as.matrix(abs(coef(tot_lean_male_lasso))[-1, 1]) * sapply(dxa_train_male[c(6:50, 53:69)], sd) == 0)
sum(as.matrix(abs(coef(tot_lean_female_lasso))[-1, 1]) * sapply(dxa_train_female[c(6:50, 53:69)], sd) == 0)


# Total fat %
plot_topn_lasso_F(tot_p_female_lasso, 15, 'LASSO Female total fat %')
plot_topn_lasso_M(tot_p_male_lasso, 15, 'LASSO Male total fat %')
plot_topn_rf(tot_p_male_rf, 15, 'Male RF total body fat %')
plot_topn_rf(tot_p_female_rf, 15, 'Female RF total body %')
sum(as.matrix(abs(coef(tot_p_male_lasso))[-1, 1]) * sapply(dxa_train_male[c(6:50, 53:69)], sd) == 0)
sum(as.matrix(abs(coef(tot_p_female_lasso))[-1, 1]) * sapply(dxa_train_female[c(6:50, 53:69)], sd) == 0)


# Trunk fat %
plot_topn_lasso_F(trunk_fat_female_lasso, 15, 'LASSO Female trunk fat %')
plot_topn_lasso_M(trunk_fat_male_lasso, 15, 'LASSO Male trunk fat %')
plot_topn_rf(trunk_fat_male_rf, 15, 'Male RF trunk fat %')
plot_topn_rf(trunk_fat_female_rf, 15, 'Female RF trunk %')
sum(as.matrix(abs(coef(trunk_fat_male_lasso))[-1, 1]) * sapply(dxa_train_male[c(6:50, 53:69)], sd) == 0)
sum(as.matrix(abs(coef(trunk_fat_female_lasso))[-1, 1]) * sapply(dxa_train_female[c(6:50, 53:69)], sd) == 0)


# L1 - L4 fat %
plot_topn_lasso_F(l1_fat_female_lasso, 15, 'LASSO Female L1-L4 fat %')
plot_topn_lasso_M(l1_fat_male_lasso, 15, 'LASSO Male L1-L4 fat %')
plot_topn_rf(l1_fat_male_rf, 15, 'Male RF L1-L4 fat %')
plot_topn_rf(l1_fat_female_rf, 15, 'Female RF L1-L4 fat %')
sum(as.matrix(abs(coef(l1_fat_male_lasso))[-1, 1]) * sapply(dxa_train_male[c(6:50, 53:69)], sd) == 0)
sum(as.matrix(abs(coef(l1_fat_female_lasso))[-1, 1]) * sapply(dxa_train_female[c(6:50, 53:69)], sd) == 0)


# Appendicular lean mass
plot_topn_lasso_F(app_lean_female_lasso, 15, 'LASSO Female Appendicular lean mass')
plot_topn_lasso_M(app_lean_male_lasso, 15, 'LASSO Male Appendicular lean mass')
plot_topn_rf(app_lean_male_rf, 15, 'Male RF Appendicular lean mass')
plot_topn_rf(app_lean_female_rf, 15, 'Female RF Appendicular lean mass')
sum(as.matrix(abs(coef(app_lean_male_lasso))[-1, 1]) * sapply(dxa_train_male[c(6:50, 53:69)], sd) == 0)
sum(as.matrix(abs(coef(app_lean_female_lasso))[-1, 1]) * sapply(dxa_train_female[c(6:50, 53:69)], sd) == 0)




##########################
# STANDARD REGRESSION ADDED
##########################

# Per reviewer comments, we can also add a comparison to standard regression models
make_lm <- function(outcome_var, sex){
  if(sex == 'male'){
    trainmat <- dxa_train_male %>% select(colnames(dxa_train_male)[c(6:50, 53:69)])
    trainmat$outcome = dxa_train_male[,outcome_var]
    lmmod <- lm(outcome ~ ., data = trainmat)
    
    return(lmmod)
  }
  else if(sex == 'female'){
    trainmat <- dxa_train_female %>% select(colnames(dxa_train_male)[c(6:50, 53:69)]) 
    trainmat$outcome = dxa_train_female[,outcome_var]
    lmmod <- lm(outcome ~ ., data = trainmat)
    
    return(lmmod)
  }
}

# We can do all 6 outcomes here
total_fat_male_lm <- make_lm('ha1q34_9atotal_fat', 'male')
total_fat_female_lm <- make_lm('ha1q34_9atotal_fat', 'female')
totalfatlmm <- train_test_perf(predict(total_fat_male_lm, dxa_train_male), 
                         predict(total_fat_male_lm, dxa_test_male),
                         'ha1q34_9atotal_fat', 'male')
totalfatlmf <- train_test_perf(predict(total_fat_female_lm, dxa_train_female), 
                         predict(total_fat_female_lm, dxa_test_female),
                         'ha1q34_9atotal_fat', 'female')

total_lean_male_lm <- make_lm('ha1q34_9btotal_lean', 'male')
total_lean_female_lm <- make_lm('ha1q34_9btotal_lean', 'female')
totalleanlmm <- train_test_perf(predict(total_lean_male_lm, dxa_train_male), 
                               predict(total_lean_male_lm, dxa_test_male),
                               'ha1q34_9btotal_lean', 'male')
totalleanlmf <- train_test_perf(predict(total_lean_female_lm, dxa_train_female), 
                               predict(total_lean_female_lm, dxa_test_female),
                               'ha1q34_9btotal_lean', 'female')

total_fatp_male_lm <- make_lm('ha1q34_9dtotal_pcent_fat', 'male')
total_fatp_female_lm <- make_lm('ha1q34_9dtotal_pcent_fat', 'female')
totalfatplmm <- train_test_perf(predict(total_fatp_male_lm, dxa_train_male), 
                               predict(total_fatp_male_lm, dxa_test_male),
                               'ha1q34_9dtotal_pcent_fat', 'male')
totalfatplmf <- train_test_perf(predict(total_fatp_female_lm, dxa_train_female), 
                               predict(total_fatp_female_lm, dxa_test_female),
                               'ha1q34_9dtotal_pcent_fat', 'female')

trunk_fat_male_lm <- make_lm('ha1q34_6dtrunk_pcent_fat', 'male')
trunk_fatp_female_lm <- make_lm('ha1q34_6dtrunk_pcent_fat', 'female')
trunkfatplmm <- train_test_perf(predict(trunk_fat_male_lm, dxa_train_male), 
                                predict(trunk_fat_male_lm, dxa_test_male),
                                'ha1q34_6dtrunk_pcent_fat', 'male')
trunkfatplmf <- train_test_perf(predict(trunk_fatp_female_lm, dxa_train_female), 
                                predict(trunk_fatp_female_lm, dxa_test_female),
                                'ha1q34_6dtrunk_pcent_fat', 'female')


l1_fat_male_lm <- make_lm('ha1q34_12dl1l4_pcent1', 'male')
l1_fat_female_lm <- make_lm('ha1q34_12dl1l4_pcent1', 'female')
l1lmm <- train_test_perf(predict(l1_fat_male_lm, dxa_train_male), 
                       predict(l1_fat_male_lm, dxa_test_male),
                       'ha1q34_12dl1l4_pcent1', 'male')
l1lmf <- train_test_perf(predict(l1_fat_female_lm, dxa_train_female), 
                       predict(l1_fat_female_lm, dxa_test_female),
                       'ha1q34_12dl1l4_pcent1', 'female')


app_leanmale_lm <- make_lm('appendic_lean', 'male')
app_lean_female_lm <- make_lm('appendic_lean', 'female')
appleanlmm <- train_test_perf(predict(app_leanmale_lm, dxa_train_male), 
                         predict(app_leanmale_lm, dxa_test_male),
                         'appendic_lean', 'male')
appleanlmf <- train_test_perf(predict(app_lean_female_lm, dxa_train_female), 
                         predict(app_lean_female_lm, dxa_test_female),
                         'appendic_lean', 'female')


# Export
lm_results = rbind(totalfatlmm, totalfatlmf, totalleanlmm, totalleanlmf, totalfatplmm, totalfatplmf, trunkfatplmm, trunkfatplmf, l1lmm, l1lmf, appleanlmm, appleanlmf)
write.csv(lm_results, 'lm_results.csv')

##########################
# STATION SUBTRACTION ANALYSIS
##########################

# We can additionally use LASSO on only some stations at a time

# Idea is to do
# TANITA only
# Tanita + one of (Circumference/skinfold/grip strength)

# RESET SEED HERE FOR CONSISTENT RESULTS WITHIN SUB-SECTION
# That way, don't have to re-run entire script with RF etc.
set.seed(92)


# Function to save model
make_lasso2 <- function(outcome_var, sex, colnums){
  if(sex == 'male'){
    trainmat <- dxa_train_male %>% select(colnames(dxa_train_male)[colnums]) %>% as.matrix()
    lasso1cv <- cv.glmnet(trainmat, dxa_train_male[,outcome_var])
    lam_min1 <- lasso1cv$lambda.min
    lasso1 <- glmnet(trainmat, dxa_train_male[,outcome_var], lambda = lam_min1)
    
    return(lasso1)
  }
  else if(sex == 'female'){
    trainmat <- dxa_train_female %>% select(colnames(dxa_train_female)[colnums]) %>% as.matrix()
    lasso1cv <- cv.glmnet(trainmat, dxa_train_female[,outcome_var])
    lam_min1 <- lasso1cv$lambda.min
    lasso1 <- glmnet(trainmat, dxa_train_female[,outcome_var], lambda = lam_min1)
    
    return(lasso1)
  }
}

predict_enet2 <- function(model, dataset, colnums){
  return(as.numeric((dataset %>% select(colnames(dataset)[colnums]) %>% as.matrix() %*% model$beta) + model$a0))
}

# TANITA Only

tot_fat_male_tanita <- make_lasso2('ha1q34_9atotal_fat', 'male', c(6:38, 50, 59:65))
tot_fat_female_tanita <- make_lasso2('ha1q34_9atotal_fat', 'female', c(6:38, 50, 59:65))

tot_lean_male_tanita <- make_lasso2('ha1q34_9btotal_lean', 'male', c(6:38, 50, 59:65))
tot_lean_female_tanita <- make_lasso2('ha1q34_9btotal_lean', 'female', c(6:38, 50, 59:65))

tot_fat_p_male_tanita <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'male', c(6:38, 50, 59:65))
tot_fat_p_female_tanita <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'female', c(6:38, 50, 59:65))

trunk_fat_male_tanita <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'male', c(6:38, 50, 59:65))
trunk_fat_female_tanita <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'female', c(6:38, 50, 59:65))

l1_fat_male_tanita <- make_lasso2('ha1q34_12dl1l4_pcent1', 'male', c(6:38, 50, 59:65))
l1_fat_female_tanita <- make_lasso2('ha1q34_12dl1l4_pcent1', 'female', c(6:38, 50, 59:65))

app_lean_male_tanita <- make_lasso2('appendic_lean', 'male', c(6:38, 50, 59:65))
app_lean_female_tanita <- make_lasso2('appendic_lean', 'female', c(6:38, 50, 59:65))

# Save results
total_fat_m_tanita <- train_test_perf(predict_enet2(tot_fat_male_tanita, dxa_train_male, c(6:38, 50, 59:65)), 
                                      predict_enet2(tot_fat_male_tanita, dxa_test_male, c(6:38, 50, 59:65)),
                                      'ha1q34_9atotal_fat', 'male')
total_fat_f_tanita <- train_test_perf(predict_enet2(tot_fat_female_tanita, dxa_train_female, c(6:38, 50, 59:65)), 
                                      predict_enet2(tot_fat_female_tanita, dxa_test_female, c(6:38, 50, 59:65)),
                                      'ha1q34_9atotal_fat', 'female')

total_lean_m_tanita <- train_test_perf(predict_enet2(tot_lean_male_tanita, dxa_train_male, c(6:38, 50, 59:65)), 
                                       predict_enet2(tot_lean_male_tanita, dxa_test_male, c(6:38, 50, 59:65)),
                                       'ha1q34_9btotal_lean', 'male')
total_lean_f_tanita <- train_test_perf(predict_enet2(tot_lean_female_tanita, dxa_train_female, c(6:38, 50, 59:65)), 
                                       predict_enet2(tot_lean_female_tanita, dxa_test_female, c(6:38, 50, 59:65)),
                                       'ha1q34_9btotal_lean', 'female')


total_fat_p_m_tanita <- train_test_perf(predict_enet2(tot_fat_p_male_tanita, dxa_train_male, c(6:38, 50, 59:65)), 
                                        predict_enet2(tot_fat_p_male_tanita, dxa_test_male, c(6:38, 50, 59:65)),
                                        'ha1q34_9dtotal_pcent_fat', 'male')
total_fat_p_f_tanita <- train_test_perf(predict_enet2(tot_fat_p_female_tanita, dxa_train_female, c(6:38, 50, 59:65)), 
                                        predict_enet2(tot_fat_p_female_tanita, dxa_test_female, c(6:38, 50, 59:65)),
                                        'ha1q34_9dtotal_pcent_fat', 'female')

trunk_m_tanita <- train_test_perf(predict_enet2(trunk_fat_male_tanita, dxa_train_male, c(6:38, 50, 59:65)), 
                                  predict_enet2(trunk_fat_male_tanita, dxa_test_male, c(6:38, 50, 59:65)),
                                  'ha1q34_6dtrunk_pcent_fat', 'male')
trunk_f_tanita <- train_test_perf(predict_enet2(trunk_fat_female_tanita, dxa_train_female, c(6:38, 50, 59:65)), 
                                  predict_enet2(trunk_fat_female_tanita, dxa_test_female, c(6:38, 50, 59:65)),
                                  'ha1q34_6dtrunk_pcent_fat', 'female')


l1_m_tanita <- train_test_perf(predict_enet2(l1_fat_male_tanita, dxa_train_male, c(6:38, 50, 59:65)), 
                               predict_enet2(l1_fat_male_tanita, dxa_test_male, c(6:38, 50, 59:65)),
                               'ha1q34_12dl1l4_pcent1', 'male')
l1_f_tanita <- train_test_perf(predict_enet2(l1_fat_female_tanita, dxa_train_female, c(6:38, 50, 59:65)), 
                               predict_enet2(l1_fat_female_tanita, dxa_test_female, c(6:38, 50, 59:65)),
                               'ha1q34_12dl1l4_pcent1', 'female')


app_m_tanita <- train_test_perf(predict_enet2(app_lean_male_tanita, dxa_train_male, c(6:38, 50, 59:65)), 
                                predict_enet2(app_lean_male_tanita, dxa_test_male, c(6:38, 50, 59:65)),
                                'appendic_lean', 'male')
app_f_tanita <- train_test_perf(predict_enet2(app_lean_female_tanita, dxa_train_female, c(6:38, 50, 59:65)), 
                                predict_enet2(app_lean_female_tanita, dxa_test_female, c(6:38, 50, 59:65)),
                                'appendic_lean', 'female')


# Now, can do TANITA + Circumference

tot_fat_male_tanita_c <- make_lasso2('ha1q34_9atotal_fat', 'male', c(6:44, 50, 55:66))
tot_fat_female_tanita_c <- make_lasso2('ha1q34_9atotal_fat', 'female', c(6:44, 50, 55:66))

tot_lean_male_tanita_c <- make_lasso2('ha1q34_9btotal_lean', 'male', c(6:44, 50, 55:66))
tot_lean_female_tanita_c <- make_lasso2('ha1q34_9btotal_lean', 'female', c(6:44, 50, 55:66))

tot_fat_p_male_tanita_c <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'male', c(6:44, 50, 55:66))
tot_fat_p_female_tanita_c <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'female', c(6:44, 50, 55:66))

trunk_fat_male_tanita_c <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'male', c(6:44, 50, 55:66))
trunk_fat_female_tanita_c <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'female', c(6:44, 50, 55:66))

l1_fat_male_tanita_c <- make_lasso2('ha1q34_12dl1l4_pcent1', 'male', c(6:44, 50, 55:66))
l1_fat_female_tanita_c <- make_lasso2('ha1q34_12dl1l4_pcent1', 'female', c(6:44, 50, 55:66))

app_lean_male_tanita_c <- make_lasso2('appendic_lean', 'male', c(6:44, 50, 55:66))
app_lean_female_tanita_c <- make_lasso2('appendic_lean', 'female', c(6:44, 50, 55:66))

# Save results
total_fat_m_tanita_c <- train_test_perf(predict_enet2(tot_fat_male_tanita_c, dxa_train_male, c(6:44, 50, 55:66)), 
                                        predict_enet2(tot_fat_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)),
                                        'ha1q34_9atotal_fat', 'male')
total_fat_f_tanita_c <- train_test_perf(predict_enet2(tot_fat_female_tanita_c, dxa_train_female, c(6:44, 50, 55:66)), 
                                        predict_enet2(tot_fat_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)),
                                        'ha1q34_9atotal_fat', 'female')

total_lean_m_tanita_c <- train_test_perf(predict_enet2(tot_lean_male_tanita_c, dxa_train_male, c(6:44, 50, 55:66)), 
                                         predict_enet2(tot_lean_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)),
                                         'ha1q34_9btotal_lean', 'male')
total_lean_f_tanita_c <- train_test_perf(predict_enet2(tot_lean_female_tanita_c, dxa_train_female, c(6:44, 50, 55:66)), 
                                         predict_enet2(tot_lean_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)),
                                         'ha1q34_9btotal_lean', 'female')


total_fat_p_m_tanita_c <- train_test_perf(predict_enet2(tot_fat_p_male_tanita_c, dxa_train_male, c(6:44, 50, 55:66)), 
                                          predict_enet2(tot_fat_p_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)),
                                          'ha1q34_9dtotal_pcent_fat', 'male')
total_fat_p_f_tanita_c <- train_test_perf(predict_enet2(tot_fat_p_female_tanita_c, dxa_train_female, c(6:44, 50, 55:66)), 
                                          predict_enet2(tot_fat_p_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)),
                                          'ha1q34_9dtotal_pcent_fat', 'female')

trunk_m_tanita_c <- train_test_perf(predict_enet2(trunk_fat_male_tanita_c, dxa_train_male, c(6:44, 50, 55:66)), 
                                    predict_enet2(trunk_fat_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)),
                                    'ha1q34_6dtrunk_pcent_fat', 'male')
trunk_f_tanita_c <- train_test_perf(predict_enet2(trunk_fat_female_tanita_c, dxa_train_female, c(6:44, 50, 55:66)), 
                                    predict_enet2(trunk_fat_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)),
                                    'ha1q34_6dtrunk_pcent_fat', 'female')


l1_m_tanita_c <- train_test_perf(predict_enet2(l1_fat_male_tanita_c, dxa_train_male, c(6:44, 50, 55:66)), 
                                 predict_enet2(l1_fat_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)),
                                 'ha1q34_12dl1l4_pcent1', 'male')
l1_f_tanita_c <- train_test_perf(predict_enet2(l1_fat_female_tanita_c, dxa_train_female, c(6:44, 50, 55:66)), 
                                 predict_enet2(l1_fat_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)),
                                 'ha1q34_12dl1l4_pcent1', 'female')


app_m_tanita_c <- train_test_perf(predict_enet2(app_lean_male_tanita_c, dxa_train_male, c(6:44, 50, 55:66)), 
                                  predict_enet2(app_lean_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)),
                                  'appendic_lean', 'male')
app_f_tanita_c <- train_test_perf(predict_enet2(app_lean_female_tanita_c, dxa_train_female, c(6:44, 50, 55:66)), 
                                  predict_enet2(app_lean_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)),
                                  'appendic_lean', 'female')




# Now, can do TANITA + Skinfold

tot_fat_male_tanita_s <- make_lasso2('ha1q34_9atotal_fat', 'male', c(6:38, 45:50, 59:65, 67))
tot_fat_female_tanita_s <- make_lasso2('ha1q34_9atotal_fat', 'female', c(6:38, 45:50, 59:65, 67))

tot_lean_male_tanita_s <- make_lasso2('ha1q34_9btotal_lean', 'male', c(6:38, 45:50, 59:65, 67))
tot_lean_female_tanita_s <- make_lasso2('ha1q34_9btotal_lean', 'female', c(6:38, 45:50, 59:65, 67))

tot_fat_p_male_tanita_s <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'male', c(6:38, 45:50, 59:65, 67))
tot_fat_p_female_tanita_s <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'female', c(6:38, 45:50, 59:65, 67))

trunk_fat_male_tanita_s <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'male', c(6:38, 45:50, 59:65, 67))
trunk_fat_female_tanita_s <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'female', c(6:38, 45:50, 59:65, 67))

l1_fat_male_tanita_s <- make_lasso2('ha1q34_12dl1l4_pcent1', 'male', c(6:38, 45:50, 59:65, 67))
l1_fat_female_tanita_s <- make_lasso2('ha1q34_12dl1l4_pcent1', 'female', c(6:38, 45:50, 59:65, 67))

app_lean_male_tanita_s <- make_lasso2('appendic_lean', 'male', c(6:38, 45:50, 59:65, 67))
app_lean_female_tanita_s <- make_lasso2('appendic_lean', 'female', c(6:38, 45:50, 59:65, 67))

# Save results
total_fat_m_tanita_s <- train_test_perf(predict_enet2(tot_fat_male_tanita_s, dxa_train_male, c(6:38, 45:50, 59:65, 67)), 
                                        predict_enet2(tot_fat_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)),
                                        'ha1q34_9atotal_fat', 'male')
total_fat_f_tanita_s <- train_test_perf(predict_enet2(tot_fat_female_tanita_s, dxa_train_female, c(6:38, 45:50, 59:65, 67)), 
                                        predict_enet2(tot_fat_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)),
                                        'ha1q34_9atotal_fat', 'female')

total_lean_m_tanita_s <- train_test_perf(predict_enet2(tot_lean_male_tanita_s, dxa_train_male, c(6:38, 45:50, 59:65, 67)), 
                                         predict_enet2(tot_lean_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)),
                                         'ha1q34_9btotal_lean', 'male')
total_lean_f_tanita_s <- train_test_perf(predict_enet2(tot_lean_female_tanita_s, dxa_train_female, c(6:38, 45:50, 59:65, 67)), 
                                         predict_enet2(tot_lean_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)),
                                         'ha1q34_9btotal_lean', 'female')


total_fat_p_m_tanita_s <- train_test_perf(predict_enet2(tot_fat_p_male_tanita_s, dxa_train_male, c(6:38, 45:50, 59:65, 67)), 
                                          predict_enet2(tot_fat_p_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)),
                                          'ha1q34_9dtotal_pcent_fat', 'male')
total_fat_p_f_tanita_s <- train_test_perf(predict_enet2(tot_fat_p_female_tanita_s, dxa_train_female, c(6:38, 45:50, 59:65, 67)), 
                                          predict_enet2(tot_fat_p_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)),
                                          'ha1q34_9dtotal_pcent_fat', 'female')

trunk_m_tanita_s <- train_test_perf(predict_enet2(trunk_fat_male_tanita_s, dxa_train_male, c(6:38, 45:50, 59:65, 67)), 
                                    predict_enet2(trunk_fat_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)),
                                    'ha1q34_6dtrunk_pcent_fat', 'male')
trunk_f_tanita_s <- train_test_perf(predict_enet2(trunk_fat_female_tanita_s, dxa_train_female, c(6:38, 45:50, 59:65, 67)), 
                                    predict_enet2(trunk_fat_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)),
                                    'ha1q34_6dtrunk_pcent_fat', 'female')


l1_m_tanita_s <- train_test_perf(predict_enet2(l1_fat_male_tanita_s, dxa_train_male, c(6:38, 45:50, 59:65, 67)), 
                                 predict_enet2(l1_fat_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)),
                                 'ha1q34_12dl1l4_pcent1', 'male')
l1_f_tanita_s <- train_test_perf(predict_enet2(l1_fat_female_tanita_s, dxa_train_female, c(6:38, 45:50, 59:65, 67)), 
                                 predict_enet2(l1_fat_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)),
                                 'ha1q34_12dl1l4_pcent1', 'female')


app_m_tanita_s <- train_test_perf(predict_enet2(app_lean_male_tanita_s, dxa_train_male, c(6:38, 45:50, 59:65, 67)), 
                                  predict_enet2(app_lean_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)),
                                  'appendic_lean', 'male')
app_f_tanita_s <- train_test_perf(predict_enet2(app_lean_female_tanita_s, dxa_train_female, c(6:38, 45:50, 59:65, 67)), 
                                  predict_enet2(app_lean_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)),
                                  'appendic_lean', 'female')



# Now, can do TANITA + Grip Strength

tot_fat_male_tanita_g <- make_lasso2('ha1q34_9atotal_fat', 'male', c(6:38, 50, 53:54, 59:65, 68))
tot_fat_female_tanita_g <- make_lasso2('ha1q34_9atotal_fat', 'female', c(6:38, 50, 53:54, 59:65, 68))

tot_lean_male_tanita_g <- make_lasso2('ha1q34_9btotal_lean', 'male', c(6:38, 50, 53:54, 59:65, 68))
tot_lean_female_tanita_g <- make_lasso2('ha1q34_9btotal_lean', 'female', c(6:38, 50, 53:54, 59:65, 68))

tot_fat_p_male_tanita_g <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'male', c(6:38, 50, 53:54, 59:65, 68))
tot_fat_p_female_tanita_g <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'female', c(6:38, 50, 53:54, 59:65, 68))

trunk_fat_male_tanita_g <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'male', c(6:38, 50, 53:54, 59:65, 68))
trunk_fat_female_tanita_g <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'female', c(6:38, 50, 53:54, 59:65, 68))

l1_fat_male_tanita_g <- make_lasso2('ha1q34_12dl1l4_pcent1', 'male', c(6:38, 50, 53:54, 59:65, 68))
l1_fat_female_tanita_g <- make_lasso2('ha1q34_12dl1l4_pcent1', 'female', c(6:38, 50, 53:54, 59:65, 68))

app_lean_male_tanita_g <- make_lasso2('appendic_lean', 'male', c(6:38, 50, 53:54, 59:65, 68))
app_lean_female_tanita_g <- make_lasso2('appendic_lean', 'female', c(6:38, 50, 53:54, 59:65, 68))

# Save results
total_fat_m_tanita_g <- train_test_perf(predict_enet2(tot_fat_male_tanita_g, dxa_train_male, c(6:38, 50, 53:54, 59:65, 68)), 
                                        predict_enet2(tot_fat_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)),
                                        'ha1q34_9atotal_fat', 'male')
total_fat_f_tanita_g <- train_test_perf(predict_enet2(tot_fat_female_tanita_g, dxa_train_female, c(6:38, 50, 53:54, 59:65, 68)), 
                                        predict_enet2(tot_fat_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)),
                                        'ha1q34_9atotal_fat', 'female')

total_lean_m_tanita_g <- train_test_perf(predict_enet2(tot_lean_male_tanita_g, dxa_train_male, c(6:38, 50, 53:54, 59:65, 68)), 
                                         predict_enet2(tot_lean_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)),
                                         'ha1q34_9btotal_lean', 'male')
total_lean_f_tanita_g <- train_test_perf(predict_enet2(tot_lean_female_tanita_g, dxa_train_female, c(6:38, 50, 53:54, 59:65, 68)), 
                                         predict_enet2(tot_lean_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)),
                                         'ha1q34_9btotal_lean', 'female')


total_fat_p_m_tanita_g <- train_test_perf(predict_enet2(tot_fat_p_male_tanita_g, dxa_train_male, c(6:38, 50, 53:54, 59:65, 68)), 
                                          predict_enet2(tot_fat_p_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)),
                                          'ha1q34_9dtotal_pcent_fat', 'male')
total_fat_p_f_tanita_g <- train_test_perf(predict_enet2(tot_fat_p_female_tanita_g, dxa_train_female, c(6:38, 50, 53:54, 59:65, 68)), 
                                          predict_enet2(tot_fat_p_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)),
                                          'ha1q34_9dtotal_pcent_fat', 'female')

trunk_m_tanita_g <- train_test_perf(predict_enet2(trunk_fat_male_tanita_g, dxa_train_male, c(6:38, 50, 53:54, 59:65, 68)), 
                                    predict_enet2(trunk_fat_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)),
                                    'ha1q34_6dtrunk_pcent_fat', 'male')
trunk_f_tanita_g <- train_test_perf(predict_enet2(trunk_fat_female_tanita_g, dxa_train_female, c(6:38, 50, 53:54, 59:65, 68)), 
                                    predict_enet2(trunk_fat_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)),
                                    'ha1q34_6dtrunk_pcent_fat', 'female')


l1_m_tanita_g <- train_test_perf(predict_enet2(l1_fat_male_tanita_g, dxa_train_male, c(6:38, 50, 53:54, 59:65, 68)), 
                                 predict_enet2(l1_fat_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)),
                                 'ha1q34_12dl1l4_pcent1', 'male')
l1_f_tanita_g <- train_test_perf(predict_enet2(l1_fat_female_tanita_g, dxa_train_female, c(6:38, 50, 53:54, 59:65, 68)), 
                                 predict_enet2(l1_fat_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)),
                                 'ha1q34_12dl1l4_pcent1', 'female')


app_m_tanita_g <- train_test_perf(predict_enet2(app_lean_male_tanita_g, dxa_train_male, c(6:38, 50, 53:54, 59:65, 68)), 
                                  predict_enet2(app_lean_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)),
                                  'appendic_lean', 'male')
app_f_tanita_g <- train_test_perf(predict_enet2(app_lean_female_tanita_g, dxa_train_female, c(6:38, 50, 53:54, 59:65, 68)), 
                                  predict_enet2(app_lean_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)),
                                  'appendic_lean', 'female')





# Now, can do circumference ONLY (but do also include weight and bmi from tanita)

tot_fat_male_circ <- make_lasso2('ha1q34_9atotal_fat', 'male', c(14:15, 37:44, 50, 55:58, 66))
tot_fat_female_circ <- make_lasso2('ha1q34_9atotal_fat', 'female', c(14:15, 37:44, 50, 55:58, 66))

tot_lean_male_circ <- make_lasso2('ha1q34_9btotal_lean', 'male', c(14:15, 37:44, 50, 55:58, 66))
tot_lean_female_circ <- make_lasso2('ha1q34_9btotal_lean', 'female', c(14:15, 37:44, 50, 55:58, 66))

tot_fat_p_male_circ <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'male', c(14:15, 37:44, 50, 55:58, 66))
tot_fat_p_female_circ <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'female', c(14:15, 37:44, 50, 55:58, 66))

trunk_fat_male_circ <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'male', c(14:15, 37:44, 50, 55:58, 66))
trunk_fat_female_circ <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'female', c(14:15, 37:44, 50, 55:58, 66))

l1_fat_male_circ <- make_lasso2('ha1q34_12dl1l4_pcent1', 'male', c(14:15, 37:44, 50, 55:58, 66))
l1_fat_female_circ <- make_lasso2('ha1q34_12dl1l4_pcent1', 'female', c(14:15, 37:44, 50, 55:58, 66))

app_lean_male_circ <- make_lasso2('appendic_lean', 'male', c(14:15, 37:44, 50, 55:58, 66))
app_lean_female_circ <- make_lasso2('appendic_lean', 'female', c(14:15, 37:44, 50, 55:58, 66))

# Save results
total_fat_m_circ <- train_test_perf(predict_enet2(tot_fat_male_circ, dxa_train_male, c(14:15, 37:44, 50, 55:58, 66)), 
                                    predict_enet2(tot_fat_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)),
                                    'ha1q34_9atotal_fat', 'male')
total_fat_f_circ <- train_test_perf(predict_enet2(tot_fat_female_circ, dxa_train_female, c(14:15, 37:44, 50, 55:58, 66)), 
                                    predict_enet2(tot_fat_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)),
                                    'ha1q34_9atotal_fat', 'female')

total_lean_m_circ <- train_test_perf(predict_enet2(tot_lean_male_circ, dxa_train_male, c(14:15, 37:44, 50, 55:58, 66)), 
                                     predict_enet2(tot_lean_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)),
                                     'ha1q34_9btotal_lean', 'male')
total_lean_f_circ <- train_test_perf(predict_enet2(tot_lean_female_circ, dxa_train_female, c(14:15, 37:44, 50, 55:58, 66)), 
                                     predict_enet2(tot_lean_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)),
                                     'ha1q34_9btotal_lean', 'female')


total_fat_p_m_circ <- train_test_perf(predict_enet2(tot_fat_p_male_circ, dxa_train_male, c(14:15, 37:44, 50, 55:58, 66)), 
                                      predict_enet2(tot_fat_p_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)),
                                      'ha1q34_9dtotal_pcent_fat', 'male')
total_fat_p_f_circ <- train_test_perf(predict_enet2(tot_fat_p_female_circ, dxa_train_female, c(14:15, 37:44, 50, 55:58, 66)), 
                                      predict_enet2(tot_fat_p_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)),
                                      'ha1q34_9dtotal_pcent_fat', 'female')

trunk_m_circ <- train_test_perf(predict_enet2(trunk_fat_male_circ, dxa_train_male, c(14:15, 37:44, 50, 55:58, 66)), 
                                predict_enet2(trunk_fat_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)),
                                'ha1q34_6dtrunk_pcent_fat', 'male')
trunk_f_circ <- train_test_perf(predict_enet2(trunk_fat_female_circ, dxa_train_female, c(14:15, 37:44, 50, 55:58, 66)), 
                                predict_enet2(trunk_fat_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)),
                                'ha1q34_6dtrunk_pcent_fat', 'female')


l1_m_circ <- train_test_perf(predict_enet2(l1_fat_male_circ, dxa_train_male, c(14:15, 37:44, 50, 55:58, 66)), 
                             predict_enet2(l1_fat_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)),
                             'ha1q34_12dl1l4_pcent1', 'male')
l1_f_circ <- train_test_perf(predict_enet2(l1_fat_female_circ, dxa_train_female, c(14:15, 37:44, 50, 55:58, 66)), 
                             predict_enet2(l1_fat_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)),
                             'ha1q34_12dl1l4_pcent1', 'female')


app_m_circ <- train_test_perf(predict_enet2(app_lean_male_circ, dxa_train_male, c(14:15, 37:44, 50, 55:58, 66)), 
                              predict_enet2(app_lean_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)),
                              'appendic_lean', 'male')
app_f_circ <- train_test_perf(predict_enet2(app_lean_female_circ, dxa_train_female, c(14:15, 37:44, 50, 55:58, 66)), 
                              predict_enet2(app_lean_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)),
                              'appendic_lean', 'female')



# Now, can do circumference plus skinfold
# (CAMA Finally applies)

tot_fat_male_circskin <- make_lasso2('ha1q34_9atotal_fat', 'male', c(14:15, 37:50, 55:58, 66, 67, 69))
tot_fat_female_circskin <- make_lasso2('ha1q34_9atotal_fat', 'female', c(14:15, 37:50, 55:58, 66, 67, 69))

tot_lean_male_circskin <- make_lasso2('ha1q34_9btotal_lean', 'male', c(14:15, 37:50, 55:58, 66, 67, 69))
tot_lean_female_circskin <- make_lasso2('ha1q34_9btotal_lean', 'female', c(14:15, 37:50, 55:58, 66, 67, 69))

tot_fat_p_male_circskin <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'male', c(14:15, 37:50, 55:58, 66, 67, 69))
tot_fat_p_female_circskin <- make_lasso2('ha1q34_9dtotal_pcent_fat', 'female', c(14:15, 37:50, 55:58, 66, 67, 69))

trunk_fat_male_circskin <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'male', c(14:15, 37:50, 55:58, 66, 67, 69))
trunk_fat_female_circskin <- make_lasso2('ha1q34_6dtrunk_pcent_fat', 'female', c(14:15, 37:50, 55:58, 66, 67, 69))

l1_fat_male_circskin <- make_lasso2('ha1q34_12dl1l4_pcent1', 'male', c(14:15, 37:50, 55:58, 66, 67, 69))
l1_fat_female_circskin <- make_lasso2('ha1q34_12dl1l4_pcent1', 'female', c(14:15, 37:50, 55:58, 66, 67, 69))

app_lean_male_circskin <- make_lasso2('appendic_lean', 'male', c(14:15, 37:50, 55:58, 66, 67, 69))
app_lean_female_circskin <- make_lasso2('appendic_lean', 'female', c(14:15, 37:50, 55:58, 66, 67, 69))

# Save results
total_fat_m_circskin <- train_test_perf(predict_enet2(tot_fat_male_circskin, dxa_train_male, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                        predict_enet2(tot_fat_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                        'ha1q34_9atotal_fat', 'male')
total_fat_f_circskin <- train_test_perf(predict_enet2(tot_fat_female_circskin, dxa_train_female, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                        predict_enet2(tot_fat_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                        'ha1q34_9atotal_fat', 'female')

total_lean_m_circskin <- train_test_perf(predict_enet2(tot_lean_male_circskin, dxa_train_male, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                         predict_enet2(tot_lean_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                         'ha1q34_9btotal_lean', 'male')
total_lean_f_circskin <- train_test_perf(predict_enet2(tot_lean_female_circskin, dxa_train_female, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                         predict_enet2(tot_lean_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                         'ha1q34_9btotal_lean', 'female')


total_fat_p_m_circskin <- train_test_perf(predict_enet2(tot_fat_p_male_circskin, dxa_train_male, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                          predict_enet2(tot_fat_p_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                          'ha1q34_9dtotal_pcent_fat', 'male')
total_fat_p_f_circskin <- train_test_perf(predict_enet2(tot_fat_p_female_circskin, dxa_train_female, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                          predict_enet2(tot_fat_p_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                          'ha1q34_9dtotal_pcent_fat', 'female')

trunk_m_circskin <- train_test_perf(predict_enet2(trunk_fat_male_circskin, dxa_train_male, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                    predict_enet2(trunk_fat_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                    'ha1q34_6dtrunk_pcent_fat', 'male')
trunk_f_circskin <- train_test_perf(predict_enet2(trunk_fat_female_circskin, dxa_train_female, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                    predict_enet2(trunk_fat_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                    'ha1q34_6dtrunk_pcent_fat', 'female')


l1_m_circskin <- train_test_perf(predict_enet2(l1_fat_male_circskin, dxa_train_male, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                 predict_enet2(l1_fat_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                 'ha1q34_12dl1l4_pcent1', 'male')
l1_f_circskin <- train_test_perf(predict_enet2(l1_fat_female_circskin, dxa_train_female, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                 predict_enet2(l1_fat_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                 'ha1q34_12dl1l4_pcent1', 'female')


app_m_circskin <- train_test_perf(predict_enet2(app_lean_male_circskin, dxa_train_male, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                  predict_enet2(app_lean_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                  'appendic_lean', 'male')
app_f_circskin <- train_test_perf(predict_enet2(app_lean_female_circskin, dxa_train_female, c(14:15, 37:50, 55:58, 66, 67, 69)), 
                                  predict_enet2(app_lean_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)),
                                  'appendic_lean', 'female')


###########################
# DURNIN-W EQUATIONS
###########################

# This section should be run LAST
# As we create new variables that are NOT used in other models

# We can use the age-specific estimates
# Perhaps need a case_when statement 
# Remember, coefficients differ between man and woman

dxa_train_male <- dxa_train_male %>% mutate(log10_skin = log10(sup_fold + sub_fold + bi_fold + tri_fold),
                                            density = case_when(
                                              .$ha1dv_age < 20  ~ 1.1620 - 0.0630*log10_skin, 
                                              .$ha1dv_age >= 20 & .$ha1dv_age < 30  ~ 1.1631 - 0.0632*log10_skin,
                                              .$ha1dv_age >= 30 & .$ha1dv_age < 40  ~ 1.1422 - 0.0544*log10_skin,
                                              .$ha1dv_age >= 40 & .$ha1dv_age < 50  ~ 1.1620 - 0.0700*log10_skin,
                                              .$ha1dv_age >= 50 ~ 1.1715 - 0.0779*log10_skin,
                                              TRUE  ~ 0
                                            ),
                                            dw_pfat = (495/density) - 450,
                                            dw_mfat = (dw_pfat/100)*ha1q37_2_weight,
                                            goel_pfat = 42.42 + 7.04 + 0.003*ha1dv_age - 0.22*ha1q37_2_weight - 0.042*height + 0.42*tri_fold + 0.029*waist_circ) 


dxa_test_male <- dxa_test_male %>% mutate(log10_skin = log10(sup_fold + sub_fold + bi_fold + tri_fold),
                                          density = case_when(
                                            .$ha1dv_age < 20  ~ 1.1620 - 0.0630*log10_skin, 
                                            .$ha1dv_age >= 20 & .$ha1dv_age < 30  ~ 1.1631 - 0.0632*log10_skin,
                                            .$ha1dv_age >= 30 & .$ha1dv_age < 40  ~ 1.1422 - 0.0544*log10_skin,
                                            .$ha1dv_age >= 40 & .$ha1dv_age < 50  ~ 1.1620 - 0.0700*log10_skin,
                                            .$ha1dv_age >= 50 ~ 1.1715 - 0.0779*log10_skin,
                                            TRUE  ~ 0
                                          ),
                                          dw_pfat = (495/density) - 450,
                                          dw_mfat = (dw_pfat/100)*ha1q37_2_weight,
                                          goel_pfat = 42.42 + 7.04 + 0.003*ha1dv_age - 0.22*ha1q37_2_weight - 0.042*height + 0.42*tri_fold + 0.029*waist_circ) 


dxa_train_female <- dxa_train_female %>% mutate(log10_skin = log10(sup_fold + sub_fold + bi_fold + tri_fold),
                                                density = case_when(
                                                  .$ha1dv_age < 20  ~ 1.1549 - 0.0678*log10_skin, 
                                                  .$ha1dv_age >= 20 & .$ha1dv_age < 30  ~ 1.1599 - 0.0717*log10_skin,
                                                  .$ha1dv_age >= 30 & .$ha1dv_age < 40  ~ 1.1423 - 0.0632*log10_skin,
                                                  .$ha1dv_age >= 40 & .$ha1dv_age < 50  ~ 1.1333 - 0.0612*log10_skin,
                                                  .$ha1dv_age >= 50 ~ 1.1339 - 0.0645*log10_skin,
                                                  TRUE  ~ 0
                                                ),
                                                dw_pfat = (495/density) - 450,
                                                dw_mfat = (dw_pfat/100)*ha1q37_2_weight,
                                                goel_pfat = 42.42 + 7.04 + 7.04 + 0.003*ha1dv_age - 0.22*ha1q37_2_weight - 0.042*height + 0.42*tri_fold + 0.029*waist_circ) 
# Note their paper stated 7.4 is gender coefficient
# where male = 1 and female = 2?

dxa_test_female <- dxa_test_female %>% mutate(log10_skin = log10(sup_fold + sub_fold + bi_fold + tri_fold),
                                              density = case_when(
                                                .$ha1dv_age < 20  ~ 1.1549 - 0.0678*log10_skin, 
                                                .$ha1dv_age >= 20 & .$ha1dv_age < 30  ~ 1.1599 - 0.0717*log10_skin,
                                                .$ha1dv_age >= 30 & .$ha1dv_age < 40  ~ 1.1423 - 0.0632*log10_skin,
                                                .$ha1dv_age >= 40 & .$ha1dv_age < 50  ~ 1.1333 - 0.0612*log10_skin,
                                                .$ha1dv_age >= 50 ~ 1.1339 - 0.0645*log10_skin,
                                                TRUE  ~ 0
                                              ),
                                              dw_pfat = (495/density) - 450,
                                              dw_mfat = (dw_pfat/100)*ha1q37_2_weight,
                                              goel_pfat = 42.42 + 7.04 + 7.04 + 0.003*ha1dv_age - 0.22*ha1q37_2_weight - 0.042*height + 0.42*tri_fold + 0.029*waist_circ) 

# Performance - MAE
summary(abs(dxa_test_male$ha1q34_9dtotal_pcent_fat - dxa_test_male$dw_pfat))
summary(abs(dxa_test_male$ha1q34_9atotal_fat/1000 - dxa_test_male$dw_mfat))
#summary(abs(dxa_test_male$ha1q34_9dtotal_pcent_fat - dxa_test_male$goel_pfat))
# Goel equations perform horribly! Mean tricep skinfold in their dataset was 18.6 - 
# only 12 men in our training set had tricep 18.6 or larger (315 did not, mean in test set 9.4) 

summary(abs(dxa_test_female$ha1q34_9dtotal_pcent_fat - dxa_test_female$dw_pfat))
summary(abs(dxa_test_female$ha1q34_9atotal_fat/1000 - dxa_test_female$dw_mfat))
#summary(abs(dxa_test_female$ha1q34_9dtotal_pcent_fat - dxa_test_female$goel_pfat))


# Performance - MAPE 

mape(dxa_test_male$ha1q34_9dtotal_pcent_fat, dxa_test_male$dw_pfat)
mape(dxa_test_male$ha1q34_9atotal_fat/1000, dxa_test_male$dw_mfat)
#mape(dxa_test_male$ha1q34_9dtotal_pcent_fat, dxa_test_male$goel_pfat)

mape(dxa_test_female$ha1q34_9dtotal_pcent_fat, dxa_test_female$dw_pfat)
mape(dxa_test_female$ha1q34_9atotal_fat/1000, dxa_test_female$dw_mfat)
#mape(dxa_test_female$ha1q34_9dtotal_pcent_fat, dxa_test_female$goel_pfat)



# Custom outcome metric - % of predictions within 5% of actual
percent5 <- function(actual, predicted){
  mean(abs((actual - predicted)/actual) < 0.05)
}

# Total fat mass
percent5(dxa_test_male$ha1q34_9atotal_fat/1000, dxa_test_male$ha1q37_5ii_tbf_mass)
percent5(dxa_test_male$ha1q34_9atotal_fat/1000, dxa_test_male$dw_mfat)
percent5(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita, dxa_test_male, c(6:38, 50, 59:65)))
percent5(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)))
percent5(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_male$ha1q34_9atotal_fat, predict_enet(tot_fat_male_lasso, dxa_test_male))


percent5(dxa_test_female$ha1q34_9atotal_fat/1000, dxa_test_female$ha1q37_5ii_tbf_mass)
percent5(dxa_test_female$ha1q34_9atotal_fat/1000, dxa_test_female$dw_mfat)
percent5(dxa_test_female$ha1q34_9atotal_fat, predict_enet2(tot_fat_female_tanita, dxa_test_female, c(6:38, 50, 59:65)))
percent5(dxa_test_female$ha1q34_9atotal_fat, predict_enet2(tot_fat_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_female$ha1q34_9atotal_fat, predict_enet2(tot_fat_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)))
percent5(dxa_test_female$ha1q34_9atotal_fat, predict_enet2(tot_fat_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_female$ha1q34_9atotal_fat, predict_enet2(tot_fat_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_female$ha1q34_9atotal_fat, predict_enet2(tot_fat_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_female$ha1q34_9atotal_fat, predict_enet(tot_fat_female_lasso, dxa_test_female))

# Total lean mass

percent5(dxa_test_male$ha1q34_9btotal_lean/1000, dxa_test_male$ha1q37_5iii_tbf_free_mass)
percent5(dxa_test_male$ha1q34_9btotal_lean, (as.numeric(10.385 - (0.005*dxa_test_male$ha1dv_age) + (0.103*dxa_test_male$height/10)+ (0.680*dxa_test_male$ha1q37_2_weight) + (0.288*dxa_test_male$arm_circ/10) + (0.130*dxa_test_male$calf_circ/10) - (0.183*dxa_test_male$hip_circ/10) - (5.278*dxa_test_male$log_skin))*1000)) # bharati
percent5(dxa_test_male$ha1q34_9btotal_lean, predict_enet2(tot_lean_male_tanita, dxa_test_male, c(6:38, 50, 59:65)))
percent5(dxa_test_male$ha1q34_9btotal_lean, predict_enet2(tot_lean_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_male$ha1q34_9btotal_lean, predict_enet2(tot_lean_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)))
percent5(dxa_test_male$ha1q34_9btotal_lean, predict_enet2(tot_lean_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_male$ha1q34_9btotal_lean, predict_enet2(tot_lean_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_male$ha1q34_9btotal_lean, predict_enet2(tot_lean_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_male$ha1q34_9btotal_lean, predict_enet(tot_lean_male_lasso, dxa_test_male))


percent5(dxa_test_female$ha1q34_9btotal_lean/1000, dxa_test_female$ha1q37_5iii_tbf_free_mass)
percent5(dxa_test_female$ha1q34_9btotal_lean, as.numeric(10.632 - (0.009*dxa_test_female$ha1dv_age) + (0.102*dxa_test_female$height/10)+ (0.592*dxa_test_female$ha1q37_2_weight) + (0.055*dxa_test_female$arm_circ/10) + (0.043*dxa_test_female$calf_circ/10) - (0.158*dxa_test_female$hip_circ/10) - (3.174*dxa_test_female$log_skin))*1000) # bharati
percent5(dxa_test_female$ha1q34_9btotal_lean, predict_enet2(tot_lean_female_tanita, dxa_test_female, c(6:38, 50, 59:65)))
percent5(dxa_test_female$ha1q34_9btotal_lean, predict_enet2(tot_lean_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_female$ha1q34_9btotal_lean, predict_enet2(tot_lean_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)))
percent5(dxa_test_female$ha1q34_9btotal_lean, predict_enet2(tot_lean_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_female$ha1q34_9btotal_lean, predict_enet2(tot_lean_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_female$ha1q34_9btotal_lean, predict_enet2(tot_lean_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_female$ha1q34_9btotal_lean, predict_enet(tot_lean_female_lasso, dxa_test_female))



# Total fat percentage
percent5(dxa_test_male$ha1q34_9dtotal_pcent_fat, dxa_test_male$ha1q37_5i_tbf_pcent)
percent5(dxa_test_male$ha1q34_9dtotal_pcent_fat, dxa_test_male$dw_pfat)
percent5(dxa_test_male$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_male_tanita, dxa_test_male, c(6:38, 50, 59:65)))
percent5(dxa_test_male$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_male$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)))
percent5(dxa_test_male$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_male$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_male$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_male$ha1q34_9dtotal_pcent_fat, predict_enet(tot_p_male_lasso, dxa_test_male))


percent5(dxa_test_female$ha1q34_9dtotal_pcent_fat, dxa_test_female$ha1q37_5i_tbf_pcent)
percent5(dxa_test_female$ha1q34_9dtotal_pcent_fat, dxa_test_female$dw_pfat)
percent5(dxa_test_female$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_female_tanita, dxa_test_female, c(6:38, 50, 59:65)))
percent5(dxa_test_female$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_female$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)))
percent5(dxa_test_female$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_female$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_female$ha1q34_9dtotal_pcent_fat, predict_enet2(tot_fat_p_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_female$ha1q34_9dtotal_pcent_fat, predict_enet(tot_p_female_lasso, dxa_test_female))









# My thought - what about Spearman correlation?
# Though don't know that we will include either way
cor(dxa_test_male$ha1q34_9atotal_fat/1000, dxa_test_male$ha1q37_5ii_tbf_mass, method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat/1000, dxa_test_male$dw_mfat, method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet(tot_fat_male_lasso, dxa_test_male), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita, dxa_test_male, c(6:38, 50, 59:65)), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)), method = 'spearman')


# Trunk fat percentage
percent5(dxa_test_male$ha1q34_6dtrunk_pcent_fat, dxa_test_male$ha1q37_11i_seg_tr_pcent)
#percent5(dxa_test_male$ha1q34_6dtrunk_pcent_fat, dxa_test_male$dw_pfat)
percent5(dxa_test_male$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_male_tanita, dxa_test_male, c(6:38, 50, 59:65)))
percent5(dxa_test_male$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_male$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)))
percent5(dxa_test_male$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_male$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_male$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_male$ha1q34_6dtrunk_pcent_fat, predict_enet(trunk_fat_male_lasso, dxa_test_male))


percent5(dxa_test_female$ha1q34_6dtrunk_pcent_fat, dxa_test_female$ha1q37_11i_seg_tr_pcent)
#percent5(dxa_test_female$ha1q34_6dtrunk_pcent_fat, dxa_test_female$dw_pfat)
percent5(dxa_test_female$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_female_tanita, dxa_test_female, c(6:38, 50, 59:65)))
percent5(dxa_test_female$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_female$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)))
percent5(dxa_test_female$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_female$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_female$ha1q34_6dtrunk_pcent_fat, predict_enet2(trunk_fat_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_female$ha1q34_6dtrunk_pcent_fat, predict_enet(trunk_fat_female_lasso, dxa_test_female))



# L1-L4 fat percentage
percent5(dxa_test_male$ha1q34_12dl1l4_pcent1, dxa_test_male$ha1q37_11i_seg_tr_pcent)
#percent5(dxa_test_male$ha1q34_6dtrunk_pcent_fat, dxa_test_male$dw_pfat)
percent5(dxa_test_male$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_male_tanita, dxa_test_male, c(6:38, 50, 59:65)))
percent5(dxa_test_male$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_male$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)))
percent5(dxa_test_male$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_male$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_male$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_male$ha1q34_12dl1l4_pcent1, predict_enet(l1_fat_male_lasso, dxa_test_male))


percent5(dxa_test_female$ha1q34_12dl1l4_pcent1, dxa_test_female$ha1q37_11i_seg_tr_pcent)
#percent5(dxa_test_female$ha1q34_6dtrunk_pcent_fat, dxa_test_female$dw_pfat)
percent5(dxa_test_female$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_female_tanita, dxa_test_female, c(6:38, 50, 59:65)))
percent5(dxa_test_female$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_female$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)))
percent5(dxa_test_female$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_female$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_female$ha1q34_12dl1l4_pcent1, predict_enet2(l1_fat_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_female$ha1q34_12dl1l4_pcent1, predict_enet(l1_fat_female_lasso, dxa_test_female))


# Total appendicular lean mass

percent5(dxa_test_male$appendic_lean, 1000*dxa_test_male[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_male[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_male[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_male[,'ha1q37_10iii_seg_la_free_mass'])
percent5(dxa_test_male$appendic_lean, (as.numeric(-0.996 - (0.023*dxa_test_male$ha1dv_age) + (0.090*dxa_test_male$height/10)+ (0.274*dxa_test_male$ha1q37_2_weight) + (0.143*dxa_test_male$arm_circ/10) + (0.223*dxa_test_male$calf_circ/10) - (0.104*dxa_test_male$hip_circ/10) - (3.163*dxa_test_male$log_skin))*1000)) # bharati
percent5(dxa_test_male$appendic_lean, predict_enet2(app_lean_male_tanita, dxa_test_male, c(6:38, 50, 59:65)))
percent5(dxa_test_male$appendic_lean, predict_enet2(app_lean_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_male$appendic_lean, predict_enet2(app_lean_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)))
percent5(dxa_test_male$appendic_lean, predict_enet2(app_lean_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_male$appendic_lean, predict_enet2(app_lean_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_male$appendic_lean, predict_enet2(app_lean_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_male$appendic_lean, predict_enet(app_lean_male_lasso, dxa_test_male))


percent5(dxa_test_female$appendic_lean, 1000*dxa_test_female[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_female[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_female[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_female[,'ha1q37_10iii_seg_la_free_mass'])
percent5(dxa_test_female$appendic_lean, as.numeric(1.609 - (0.021*dxa_test_female$ha1dv_age) + (0.070*dxa_test_female$height/10)+ (0.250*dxa_test_female$ha1q37_2_weight) + (0.027*dxa_test_female$arm_circ/10) + (0.098*dxa_test_female$calf_circ/10) - (0.085*dxa_test_female$hip_circ/10) - (1.821*dxa_test_female$log_skin))*1000) # bharati
percent5(dxa_test_female$appendic_lean, predict_enet2(app_lean_female_tanita, dxa_test_female, c(6:38, 50, 59:65)))
percent5(dxa_test_female$appendic_lean, predict_enet2(app_lean_female_tanita_s, dxa_test_female, c(6:38, 45:50, 59:65, 67)))
percent5(dxa_test_female$appendic_lean, predict_enet2(app_lean_female_tanita_c, dxa_test_female, c(6:44, 50, 55:66)))
percent5(dxa_test_female$appendic_lean, predict_enet2(app_lean_female_tanita_g, dxa_test_female, c(6:38, 50, 53:54, 59:65, 68)))
percent5(dxa_test_female$appendic_lean, predict_enet2(app_lean_female_circ, dxa_test_female, c(14:15, 37:44, 50, 55:58, 66)))
percent5(dxa_test_female$appendic_lean, predict_enet2(app_lean_female_circskin, dxa_test_female, c(14:15, 37:50, 55:58, 66, 67, 69)))
percent5(dxa_test_female$appendic_lean, predict_enet(app_lean_female_lasso, dxa_test_female))







# My thought, what about Spearman correlation?
cor(dxa_test_male$ha1q34_9atotal_fat/1000, dxa_test_male$ha1q37_5ii_tbf_mass, method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat/1000, dxa_test_male$dw_mfat, method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet(tot_fat_male_lasso, dxa_test_male), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita, dxa_test_male, c(6:38, 50, 59:65)), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita_c, dxa_test_male, c(6:44, 50, 55:66)), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita_s, dxa_test_male, c(6:38, 45:50, 59:65, 67)), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_tanita_g, dxa_test_male, c(6:38, 50, 53:54, 59:65, 68)), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_circ, dxa_test_male, c(14:15, 37:44, 50, 55:58, 66)), method = 'spearman')
cor(dxa_test_male$ha1q34_9atotal_fat, predict_enet2(tot_fat_male_circskin, dxa_test_male, c(14:15, 37:50, 55:58, 66, 67, 69)), method = 'spearman')


##################################
# SUMMARY INFORMATION FOR TABLE 1
##################################

mean(dxa_train_female$ha1dv_age)
sd(dxa_train_female$ha1dv_age)
mean(dxa_test_female$ha1dv_age)
sd(dxa_test_female$ha1dv_age)
mean(dxa_train_male$ha1dv_age)
sd(dxa_train_male$ha1dv_age)
mean(dxa_test_male$ha1dv_age)
sd(dxa_test_male$ha1dv_age)

mean(dxa_train_female$height)
sd(dxa_train_female$height)
mean(dxa_test_female$height)
sd(dxa_test_female$height)
mean(dxa_train_male$height)
sd(dxa_train_male$height)
mean(dxa_test_male$height)
sd(dxa_test_male$height)

mean(dxa_train_female$ha1q37_2_weight)
sd(dxa_train_female$ha1q37_2_weight)
mean(dxa_test_female$ha1q37_2_weight)
sd(dxa_test_female$ha1q37_2_weight)
mean(dxa_train_male$ha1q37_2_weight)
sd(dxa_train_male$ha1q37_2_weight)
mean(dxa_test_male$ha1q37_2_weight)
sd(dxa_test_male$ha1q37_2_weight)

mean(dxa_train_female$ha1q37_3_bmi)
sd(dxa_train_female$ha1q37_3_bmi)
mean(dxa_test_female$ha1q37_3_bmi)
sd(dxa_test_female$ha1q37_3_bmi)
mean(dxa_train_male$ha1q37_3_bmi)
sd(dxa_train_male$ha1q37_3_bmi)
mean(dxa_test_male$ha1q37_3_bmi)
sd(dxa_test_male$ha1q37_3_bmi)

mean(dxa_train_female$ha1q34_9atotal_fat)
sd(dxa_train_female$ha1q34_9atotal_fat)
mean(dxa_test_female$ha1q34_9atotal_fat)
sd(dxa_test_female$ha1q34_9atotal_fat)
mean(dxa_train_male$ha1q34_9atotal_fat)
sd(dxa_train_male$ha1q34_9atotal_fat)
mean(dxa_test_male$ha1q34_9atotal_fat)
sd(dxa_test_male$ha1q34_9atotal_fat)

mean(dxa_train_female$ha1q34_9btotal_lean)
sd(dxa_train_female$ha1q34_9btotal_lean)
mean(dxa_test_female$ha1q34_9btotal_lean)
sd(dxa_test_female$ha1q34_9btotal_lean)
mean(dxa_train_male$ha1q34_9btotal_lean)
sd(dxa_train_male$ha1q34_9btotal_lean)
mean(dxa_test_male$ha1q34_9btotal_lean)
sd(dxa_test_male$ha1q34_9btotal_lean)

mean(dxa_train_female$ha1q34_9dtotal_pcent_fat)
sd(dxa_train_female$ha1q34_9dtotal_pcent_fat)
mean(dxa_test_female$ha1q34_9dtotal_pcent_fat)
sd(dxa_test_female$ha1q34_9dtotal_pcent_fat)
mean(dxa_train_male$ha1q34_9dtotal_pcent_fat)
sd(dxa_train_male$ha1q34_9dtotal_pcent_fat)
mean(dxa_test_male$ha1q34_9dtotal_pcent_fat)
sd(dxa_test_male$ha1q34_9dtotal_pcent_fat)

mean(dxa_train_female$ha1q34_6dtrunk_pcent_fat)
sd(dxa_train_female$ha1q34_6dtrunk_pcent_fat)
mean(dxa_test_female$ha1q34_6dtrunk_pcent_fat)
sd(dxa_test_female$ha1q34_6dtrunk_pcent_fat)
mean(dxa_train_male$ha1q34_6dtrunk_pcent_fat)
sd(dxa_train_male$ha1q34_6dtrunk_pcent_fat)
mean(dxa_test_male$ha1q34_6dtrunk_pcent_fat)
sd(dxa_test_male$ha1q34_6dtrunk_pcent_fat)

mean(dxa_train_female$ha1q34_12dl1l4_pcent1)
sd(dxa_train_female$ha1q34_12dl1l4_pcent1)
mean(dxa_test_female$ha1q34_12dl1l4_pcent1)
sd(dxa_test_female$ha1q34_12dl1l4_pcent1)
mean(dxa_train_male$ha1q34_12dl1l4_pcent1)
sd(dxa_train_male$ha1q34_12dl1l4_pcent1)
mean(dxa_test_male$ha1q34_12dl1l4_pcent1)
sd(dxa_test_male$ha1q34_12dl1l4_pcent1)

mean(dxa_train_female$appendic_lean)
sd(dxa_train_female$appendic_lean)
mean(dxa_test_female$appendic_lean)
sd(dxa_test_female$appendic_lean)
mean(dxa_train_male$appendic_lean)
sd(dxa_train_male$appendic_lean)
mean(dxa_test_male$appendic_lean)
sd(dxa_test_male$appendic_lean)



##################################
# UPDATE VISUALS FOR FIGURE 2
##################################

tot_fat_male_plot1 <- data.frame(predicted = predict_enet(tot_fat_male_lasso, dxa_test_male), 
           dxa = dxa_test_male[,'ha1q34_9atotal_fat'],
           method = rep('LASSO', nrow(dxa_test_male)))

tot_fat_male_plot2 <- data.frame(predicted = 1000*dxa_test_male[,'ha1q37_5ii_tbf_mass'], 
                                 dxa = dxa_test_male[,'ha1q34_9atotal_fat'],
                                 method = rep('Tanita', nrow(dxa_test_male)))

tot_fat_male_plot <- rbind(tot_fat_male_plot1, tot_fat_male_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(tot_fat_male_plot$Differences[tot_fat_male_plot$method == 'LASSO'])
sdmean <- sd(tot_fat_male_plot$Differences[tot_fat_male_plot$method == 'LASSO'])

p1 <- tot_fat_male_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-8000, 6000)) + 
  ggtitle('Total fat mass (g) male, LASSO')


diffmean <- mean(tot_fat_male_plot$Differences[tot_fat_male_plot$method == 'Tanita'])
sdmean <- sd(tot_fat_male_plot$Differences[tot_fat_male_plot$method == 'Tanita'])

p3 <- tot_fat_male_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-8000, 6000))+ 
  ggtitle('Total fat mass (g) male, Tanita')


tot_fat_female_plot1 <- data.frame(predicted = predict_enet(tot_fat_female_lasso, dxa_test_female), 
                                 dxa = dxa_test_female[,'ha1q34_9atotal_fat'],
                                 method = rep('LASSO', nrow(dxa_test_female)))

tot_fat_female_plot2 <- data.frame(predicted = 1000*dxa_test_female[,'ha1q37_5ii_tbf_mass'], 
                                 dxa = dxa_test_female[,'ha1q34_9atotal_fat'],
                                 method = rep('Tanita', nrow(dxa_test_female)))

tot_fat_female_plot <- rbind(tot_fat_female_plot1, tot_fat_female_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(tot_fat_female_plot$Differences[tot_fat_female_plot$method == 'LASSO'])
sdmean <- sd(tot_fat_female_plot$Differences[tot_fat_female_plot$method == 'LASSO'])

p2 <- tot_fat_female_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-8000, 7500)) + 
  ggtitle('Total fat mass (g) female, LASSO')


diffmean <- mean(tot_fat_female_plot$Differences[tot_fat_female_plot$method == 'Tanita'])
sdmean <- sd(tot_fat_female_plot$Differences[tot_fat_female_plot$method == 'Tanita'])

p4 <- tot_fat_female_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-8000, 7500))+ 
  ggtitle('Total fat mass (g) female, Tanita')

# Display 
grid.arrange(p1, p3, ncol = 2)
grid.arrange(p2, p4, ncol = 2)


# TOTAL LEAN

tot_lean_male_plot1 <- data.frame(predicted = predict_enet(tot_lean_male_lasso, dxa_test_male), 
                                 dxa = dxa_test_male[,'ha1q34_9btotal_lean'],
                                 method = rep('LASSO', nrow(dxa_test_male)))

tot_lean_male_plot2 <- data.frame(predicted = 1000*dxa_test_male[,'ha1q37_5iii_tbf_free_mass'], 
                                 dxa = dxa_test_male[,'ha1q34_9btotal_lean'],
                                 method = rep('Tanita', nrow(dxa_test_male)))

tot_lean_male_plot <- rbind(tot_lean_male_plot1, tot_lean_male_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(tot_lean_male_plot$Differences[tot_lean_male_plot$method == 'LASSO'])
sdmean <- sd(tot_lean_male_plot$Differences[tot_lean_male_plot$method == 'LASSO'])

p1 <- tot_lean_male_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-5000, 11000)) + 
  ggtitle('Total lean mass (g) male, LASSO')


diffmean <- mean(tot_lean_male_plot$Differences[tot_lean_male_plot$method == 'Tanita'])
sdmean <- sd(tot_lean_male_plot$Differences[tot_lean_male_plot$method == 'Tanita'])

p3 <- tot_lean_male_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-5000, 11000))+ 
  ggtitle('Total lean mass (g) male, Tanita')



tot_lean_female_plot1 <- data.frame(predicted = predict_enet(tot_lean_female_lasso, dxa_test_female), 
                                  dxa = dxa_test_female[,'ha1q34_9btotal_lean'],
                                  method = rep('LASSO', nrow(dxa_test_female)))

tot_lean_female_plot2 <- data.frame(predicted = 1000*dxa_test_female[,'ha1q37_5iii_tbf_free_mass'], 
                                  dxa = dxa_test_female[,'ha1q34_9btotal_lean'],
                                  method = rep('Tanita', nrow(dxa_test_female)))

tot_lean_female_plot <- rbind(tot_lean_female_plot1, tot_lean_female_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(tot_lean_female_plot$Differences[tot_lean_female_plot$method == 'LASSO'])
sdmean <- sd(tot_lean_female_plot$Differences[tot_lean_female_plot$method == 'LASSO'])

p2 <- tot_lean_female_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-6000, 10000)) + 
  ggtitle('Total lean mass (g) female, LASSO')


diffmean <- mean(tot_lean_female_plot$Differences[tot_lean_female_plot$method == 'Tanita'])
sdmean <- sd(tot_lean_female_plot$Differences[tot_lean_female_plot$method == 'Tanita'])

p4 <- tot_lean_female_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-6000, 10000))+ 
  ggtitle('Total lean mass (g) female, Tanita')

# Display 
grid.arrange(p1, p3, ncol = 2)
grid.arrange(p2, p4, ncol = 2)


# TOTAL FAT MASS PERCENTAGE

tot_fatp_male_plot1 <- data.frame(predicted = predict_enet(tot_p_male_lasso, dxa_test_male), 
                                 dxa = dxa_test_male[,'ha1q34_9dtotal_pcent_fat'],
                                 method = rep('LASSO', nrow(dxa_test_male)))

tot_fatp_male_plot2 <- data.frame(predicted = dxa_test_male[,'ha1q37_5i_tbf_pcent'], 
                                 dxa = dxa_test_male[,'ha1q34_9dtotal_pcent_fat'],
                                 method = rep('Tanita', nrow(dxa_test_male)))

tot_fatp_male_plot <- rbind(tot_fatp_male_plot1, tot_fatp_male_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(tot_fatp_male_plot$Differences[tot_fatp_male_plot$method == 'LASSO'])
sdmean <- sd(tot_fatp_male_plot$Differences[tot_fatp_male_plot$method == 'LASSO'])

p1 <- tot_fatp_male_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-13, 8)) + 
  ggtitle('Total fat percentage (%) male, LASSO')


diffmean <- mean(tot_fatp_male_plot$Differences[tot_fatp_male_plot$method == 'Tanita'])
sdmean <- sd(tot_fatp_male_plot$Differences[tot_fatp_male_plot$method == 'Tanita'])

p3 <- tot_fatp_male_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-13, 8))+ 
  ggtitle('Total fat percentage (%) male, Tanita')


tot_fatp_female_plot1 <- data.frame(predicted = predict_enet(tot_p_female_lasso, dxa_test_female), 
                                  dxa = dxa_test_female[,'ha1q34_9dtotal_pcent_fat'],
                                  method = rep('LASSO', nrow(dxa_test_female)))

tot_fatp_female_plot2 <- data.frame(predicted = dxa_test_female[,'ha1q37_5i_tbf_pcent'], 
                                  dxa = dxa_test_female[,'ha1q34_9dtotal_pcent_fat'],
                                  method = rep('Tanita', nrow(dxa_test_female)))

tot_fatp_female_plot <- rbind(tot_fatp_female_plot1, tot_fatp_female_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(tot_fatp_female_plot$Differences[tot_fatp_female_plot$method == 'LASSO'])
sdmean <- sd(tot_fatp_female_plot$Differences[tot_fatp_female_plot$method == 'LASSO'])

p2 <- tot_fatp_female_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-18, 12)) + 
  ggtitle('Total fat percentage (%) female, LASSO')


diffmean <- mean(tot_fatp_female_plot$Differences[tot_fatp_female_plot$method == 'Tanita'])
sdmean <- sd(tot_fatp_female_plot$Differences[tot_fatp_female_plot$method == 'Tanita'])

p4 <- tot_fatp_female_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-18, 12))+ 
  ggtitle('Total fat percentage (%) female, Tanita')


# Display 
grid.arrange(p1, p3, ncol = 2)
grid.arrange(p2, p4, ncol = 2)



# TRUNK FAT MASS PERCENTAGE

tr_fatp_male_plot1 <- data.frame(predicted = predict_enet(tot_p_male_lasso, dxa_test_male), 
                                  dxa = dxa_test_male[,'ha1q34_6dtrunk_pcent_fat'],
                                  method = rep('LASSO', nrow(dxa_test_male)))

tr_fatp_male_plot2 <- data.frame(predicted = dxa_test_male[,'ha1q37_11i_seg_tr_pcent'], 
                                  dxa = dxa_test_male[,'ha1q34_6dtrunk_pcent_fat'],
                                  method = rep('Tanita', nrow(dxa_test_male)))

tr_fatp_male_plot <- rbind(tr_fatp_male_plot1, tr_fatp_male_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(tr_fatp_male_plot$Differences[tr_fatp_male_plot$method == 'LASSO'])
sdmean <- sd(tr_fatp_male_plot$Differences[tr_fatp_male_plot$method == 'LASSO'])

p1 <- tr_fatp_male_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-14, 11)) + 
  ggtitle('Trunk fat percentage (%) male, LASSO')


diffmean <- mean(tr_fatp_male_plot$Differences[tr_fatp_male_plot$method == 'Tanita'])
sdmean <- sd(tr_fatp_male_plot$Differences[tr_fatp_male_plot$method == 'Tanita'])

p3 <- tr_fatp_male_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-14, 11))+ 
  ggtitle('Trunk fat percentage (%) male, Tanita')


tr_fatp_female_plot1 <- data.frame(predicted = predict_enet(tot_p_female_lasso, dxa_test_female), 
                                 dxa = dxa_test_female[,'ha1q34_6dtrunk_pcent_fat'],
                                 method = rep('LASSO', nrow(dxa_test_female)))

tr_fatp_female_plot2 <- data.frame(predicted = dxa_test_female[,'ha1q37_11i_seg_tr_pcent'], 
                                 dxa = dxa_test_female[,'ha1q34_6dtrunk_pcent_fat'],
                                 method = rep('Tanita', nrow(dxa_test_female)))

tr_fatp_female_plot <- rbind(tr_fatp_female_plot1, tr_fatp_female_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(tr_fatp_female_plot$Differences[tr_fatp_female_plot$method == 'LASSO'])
sdmean <- sd(tr_fatp_female_plot$Differences[tr_fatp_female_plot$method == 'LASSO'])

p2 <- tr_fatp_female_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-23, 16)) + 
  ggtitle('Trunk fat percentage (%) female, LASSO')


diffmean <- mean(tr_fatp_female_plot$Differences[tr_fatp_female_plot$method == 'Tanita'])
sdmean <- sd(tr_fatp_female_plot$Differences[tr_fatp_female_plot$method == 'Tanita'])

p4 <- tr_fatp_female_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-23, 16))+ 
  ggtitle('Trunk fat percentage (%) female, Tanita')


# Display 
grid.arrange(p1, p3, ncol = 2)
grid.arrange(p2, p4, ncol = 2)



# L1-L4 FAT MASS PERCENTAGE

l1_fatp_male_plot1 <- data.frame(predicted = predict_enet(l1_fat_male_lasso, dxa_test_male), 
                                 dxa = dxa_test_male[,'ha1q34_12dl1l4_pcent1'],
                                 method = rep('LASSO', nrow(dxa_test_male)))

l1_fatp_male_plot2 <- data.frame(predicted = dxa_test_male[,'ha1q37_11i_seg_tr_pcent'], 
                                 dxa = dxa_test_male[,'ha1q34_12dl1l4_pcent1'],
                                 method = rep('Tanita', nrow(dxa_test_male)))

l1_fatp_male_plot <- rbind(l1_fatp_male_plot1, l1_fatp_male_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(l1_fatp_male_plot$Differences[l1_fatp_male_plot$method == 'LASSO'])
sdmean <- sd(l1_fatp_male_plot$Differences[l1_fatp_male_plot$method == 'LASSO'])

p1 <- l1_fatp_male_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-19, 13)) + 
  ggtitle('L1-L4 fat percentage (%) male, LASSO')


diffmean <- mean(l1_fatp_male_plot$Differences[l1_fatp_male_plot$method == 'Tanita'])
sdmean <- sd(l1_fatp_male_plot$Differences[l1_fatp_male_plot$method == 'Tanita'])

p3 <- l1_fatp_male_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-19, 13))+ 
  ggtitle('L1-L4 fat percentage (%) male, Tanita')



l1_fatp_female_plot1 <- data.frame(predicted = predict_enet(l1_fat_female_lasso, dxa_test_female), 
                                 dxa = dxa_test_female[,'ha1q34_12dl1l4_pcent1'],
                                 method = rep('LASSO', nrow(dxa_test_female)))

l1_fatp_female_plot2 <- data.frame(predicted = dxa_test_female[,'ha1q37_11i_seg_tr_pcent'], 
                                 dxa = dxa_test_female[,'ha1q34_12dl1l4_pcent1'],
                                 method = rep('Tanita', nrow(dxa_test_female)))

l1_fatp_female_plot <- rbind(l1_fatp_female_plot1, l1_fatp_female_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(l1_fatp_female_plot$Differences[l1_fatp_female_plot$method == 'LASSO'])
sdmean <- sd(l1_fatp_female_plot$Differences[l1_fatp_female_plot$method == 'LASSO'])

p2 <- l1_fatp_female_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-20, 15)) + 
  ggtitle('L1-L4 fat percentage (%) female, LASSO')


diffmean <- mean(l1_fatp_female_plot$Differences[l1_fatp_female_plot$method == 'Tanita'])
sdmean <- sd(l1_fatp_female_plot$Differences[l1_fatp_female_plot$method == 'Tanita'])

p4 <- l1_fatp_female_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-20, 15))+ 
  ggtitle('L1-L4 fat percentage (%) female, Tanita')


# Display 
grid.arrange(p1, p3, ncol = 2)
grid.arrange(p2, p4, ncol = 2)


# APPENDICULAR LEAN MASS

app_lean_male_plot1 <- data.frame(predicted = predict_enet(app_lean_male_lasso, dxa_test_male), 
                                  dxa = dxa_test_male[,'appendic_lean'],
                                  method = rep('LASSO', nrow(dxa_test_male)))

app_lean_male_plot2 <- data.frame(predicted = 1000*dxa_test_male[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_male[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_male[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_male[,'ha1q37_10iii_seg_la_free_mass'], 
                                  dxa = dxa_test_male[,'appendic_lean'],
                                  method = rep('Tanita', nrow(dxa_test_male)))

app_lean_male_plot <- rbind(app_lean_male_plot1, app_lean_male_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(app_lean_male_plot$Differences[app_lean_male_plot$method == 'LASSO'])
sdmean <- sd(app_lean_male_plot$Differences[app_lean_male_plot$method == 'LASSO'])

p1 <- app_lean_male_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-4000, 8000)) + 
  ggtitle('Appendicular lean mass (g) male, LASSO')


diffmean <- mean(app_lean_male_plot$Differences[app_lean_male_plot$method == 'Tanita'])
sdmean <- sd(app_lean_male_plot$Differences[app_lean_male_plot$method == 'Tanita'])

p3 <- app_lean_male_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-4000, 8000))+ 
  ggtitle('Appendicular lean mass (g) male, Tanita')


app_lean_female_plot1 <- data.frame(predicted = predict_enet(app_lean_female_lasso, dxa_test_female), 
                                  dxa = dxa_test_female[,'appendic_lean'],
                                  method = rep('LASSO', nrow(dxa_test_female)))

app_lean_female_plot2 <- data.frame(predicted = 1000*dxa_test_female[,'ha1q37_7iii_seg_rl_free_mass'] + 1000*dxa_test_female[,'ha1q37_8iii_seg_ll_free_mass'] + 1000*dxa_test_female[,'ha1q37_9iii_seg_ra_free_mass'] + 1000*dxa_test_female[,'ha1q37_10iii_seg_la_free_mass'], 
                                  dxa = dxa_test_female[,'appendic_lean'],
                                  method = rep('Tanita', nrow(dxa_test_female)))

app_lean_female_plot <- rbind(app_lean_female_plot1, app_lean_female_plot2) %>% 
  mutate(Means = (predicted+dxa)/2, Differences = predicted - dxa) 

# easiest may be to plot separately then combine later
diffmean <- mean(app_lean_female_plot$Differences[app_lean_female_plot$method == 'LASSO'])
sdmean <- sd(app_lean_female_plot$Differences[app_lean_female_plot$method == 'LASSO'])

p2 <- app_lean_female_plot %>% filter(method == 'LASSO') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-3000, 6000)) + 
  ggtitle('Appendicular lean mass (g) female, LASSO')


diffmean <- mean(app_lean_female_plot$Differences[app_lean_female_plot$method == 'Tanita'])
sdmean <- sd(app_lean_female_plot$Differences[app_lean_female_plot$method == 'Tanita'])

p4 <- app_lean_female_plot %>% filter(method == 'Tanita') %>%
  ggplot(aes(Means, Differences)) + 
  geom_point() + 
  geom_hline(yintercept = diffmean) +
  geom_hline(yintercept = diffmean - 1.96*sdmean, linetype = 2) + 
  geom_hline(yintercept = diffmean + 1.96*sdmean, linetype = 2) + 
  ylim(c(-3000, 6000))+ 
  ggtitle('Appendicular lean mass (g) female, Tanita')


# Display 
grid.arrange(p1, p3, ncol = 2)
grid.arrange(p2, p4, ncol = 2)


##################################
# COMPILE SUPPLEMENT TABLES FOR PRINT
##################################

# TABLE S1 - Coefficients for each of the LASSO models
# That is -
# Tanita only, Tanita and circ, Tanita and grip, Tanita and skin
# Circ only, circ and skin, FULL model

# One idea is to create one table per model
# then merge by varnames column
# Dont forget to include the intercept value!

full_coefs <- data.frame(varnames = c('int', rownames(tot_fat_male_lasso$beta)),
           totfatmalefull = c(tot_fat_male_lasso$a0, as.vector(tot_fat_male_lasso$beta)), 
           totfatfemalefull = c(tot_fat_female_lasso$a0, as.vector(tot_fat_female_lasso$beta)),
           totleanmalefull = c(tot_lean_male_lasso$a0, as.vector(tot_lean_male_lasso$beta)), 
           totleanfemalefull = c(tot_lean_female_lasso$a0, as.vector(tot_lean_female_lasso$beta)),
           totfatpmalefull = c(tot_p_male_lasso$a0, as.vector(tot_p_male_lasso$beta)), 
           totfatpfemalefull = c(tot_p_female_lasso$a0, as.vector(tot_p_female_lasso$beta)),
           trunkfatmalefull = c(trunk_fat_male_lasso$a0, as.vector(trunk_fat_male_lasso$beta)), 
           trunkfatfemalefull = c(trunk_fat_female_lasso$a0, as.vector(trunk_fat_female_lasso$beta)),
           l1fatmalefull = c(l1_fat_male_lasso$a0, as.vector(l1_fat_male_lasso$beta)), 
           l1fatfemalefull = c(l1_fat_female_lasso$a0, as.vector(l1_fat_female_lasso$beta)),
           appleanmalefull = c(app_lean_male_lasso$a0, as.vector(app_lean_male_lasso$beta)), 
           appleanfemalefull = c(app_lean_female_lasso$a0, as.vector(app_lean_female_lasso$beta))
           )

tanita_only_coefs <- data.frame(varnames = c('int', rownames(tot_fat_male_tanita$beta)),
                                totfatmaletanita = c(tot_fat_male_tanita$a0, as.vector(tot_fat_male_tanita$beta)), 
                                totfatfemaletanita = c(tot_fat_female_tanita$a0, as.vector(tot_fat_female_tanita$beta)),
                                totleanmaletanita = c(tot_lean_male_tanita$a0, as.vector(tot_lean_male_tanita$beta)), 
                                totleanfemaletanita = c(tot_lean_female_tanita$a0, as.vector(tot_lean_female_tanita$beta)),
                                totfatpmaletanita = c(tot_fat_p_male_tanita$a0, as.vector(tot_fat_p_male_tanita$beta)), 
                                totfatpfemaletanita = c(tot_fat_p_female_tanita$a0, as.vector(tot_fat_p_female_tanita$beta)),
                                trunkfatmaletanita = c(trunk_fat_male_tanita$a0, as.vector(trunk_fat_male_tanita$beta)), 
                                trunkfatfemaletanita = c(trunk_fat_female_tanita$a0, as.vector(trunk_fat_female_tanita$beta)),
                                l1fatmaletanita = c(l1_fat_male_tanita$a0, as.vector(l1_fat_male_tanita$beta)), 
                                l1fatfemaletanita = c(l1_fat_female_tanita$a0, as.vector(l1_fat_female_tanita$beta)),
                                appleanmaletanita = c(app_lean_male_tanita$a0, as.vector(app_lean_male_tanita$beta)), 
                                appleanfemaletanita = c(app_lean_female_tanita$a0, as.vector(app_lean_female_tanita$beta))
)

tanita_c_coefs <- data.frame(varnames = c('int', rownames(tot_fat_male_tanita_c$beta)),
                                totfatmaletanitac = c(tot_fat_male_tanita_c$a0, as.vector(tot_fat_male_tanita_c$beta)), 
                                totfatfemaletanitac = c(tot_fat_female_tanita_c$a0, as.vector(tot_fat_female_tanita_c$beta)),
                                totleanmaletanitac = c(tot_lean_male_tanita_c$a0, as.vector(tot_lean_male_tanita_c$beta)), 
                                totleanfemaletanitac = c(tot_lean_female_tanita_c$a0, as.vector(tot_lean_female_tanita_c$beta)),
                                totfatpmaletanitac = c(tot_fat_p_male_tanita_c$a0, as.vector(tot_fat_p_male_tanita_c$beta)), 
                                totfatpfemaletanitac = c(tot_fat_p_female_tanita_c$a0, as.vector(tot_fat_p_female_tanita_c$beta)),
                                trunkfatmaletanitac = c(trunk_fat_male_tanita_c$a0, as.vector(trunk_fat_male_tanita_c$beta)), 
                                trunkfatfemaletanitac = c(trunk_fat_female_tanita_c$a0, as.vector(trunk_fat_female_tanita_c$beta)),
                                l1fatmaletanitac = c(l1_fat_male_tanita_c$a0, as.vector(l1_fat_male_tanita_c$beta)), 
                                l1fatfemaletanitac = c(l1_fat_female_tanita_c$a0, as.vector(l1_fat_female_tanita_c$beta)),
                                appleanmaletanitac = c(app_lean_male_tanita_c$a0, as.vector(app_lean_male_tanita_c$beta)), 
                                appleanfemaletanitac = c(app_lean_female_tanita_c$a0, as.vector(app_lean_female_tanita_c$beta))
)

tanita_s_coefs <- data.frame(varnames = c('int', rownames(tot_fat_male_tanita_s$beta)),
                             totfatmaletanitas = c(tot_fat_male_tanita_s$a0, as.vector(tot_fat_male_tanita_s$beta)), 
                             totfatfemaletanitas = c(tot_fat_female_tanita_s$a0, as.vector(tot_fat_female_tanita_s$beta)),
                             totleanmaletanitas = c(tot_lean_male_tanita_s$a0, as.vector(tot_lean_male_tanita_s$beta)), 
                             totleanfemaletanitas = c(tot_lean_female_tanita_s$a0, as.vector(tot_lean_female_tanita_s$beta)),
                             totfatpmaletanitas = c(tot_fat_p_male_tanita_s$a0, as.vector(tot_fat_p_male_tanita_s$beta)), 
                             totfatpfemaletanitas = c(tot_fat_p_female_tanita_s$a0, as.vector(tot_fat_p_female_tanita_s$beta)),
                             trunkfatmaletanitas = c(trunk_fat_male_tanita_s$a0, as.vector(trunk_fat_male_tanita_s$beta)), 
                             trunkfatfemaletanitas = c(trunk_fat_female_tanita_s$a0, as.vector(trunk_fat_female_tanita_s$beta)),
                             l1fatmaletanitas = c(l1_fat_male_tanita_s$a0, as.vector(l1_fat_male_tanita_s$beta)), 
                             l1fatfemaletanitas = c(l1_fat_female_tanita_s$a0, as.vector(l1_fat_female_tanita_s$beta)),
                             appleanmaletanitas = c(app_lean_male_tanita_s$a0, as.vector(app_lean_male_tanita_s$beta)), 
                             appleanfemaletanitas = c(app_lean_female_tanita_s$a0, as.vector(app_lean_female_tanita_s$beta))
)

tanita_g_coefs <- data.frame(varnames = c( 'int', rownames(tot_fat_male_tanita_g$beta)),
                             totfatmaletanitag = c(tot_fat_male_tanita_g$a0, as.vector(tot_fat_male_tanita_g$beta)), 
                             totfatfemaletanitag = c(tot_fat_female_tanita_g$a0, as.vector(tot_fat_female_tanita_g$beta)),
                             totleanmaletanitag = c(tot_lean_male_tanita_g$a0, as.vector(tot_lean_male_tanita_g$beta)), 
                             totleanfemaletanitag = c(tot_lean_female_tanita_g$a0, as.vector(tot_lean_female_tanita_g$beta)),
                             totfatpmaletanitag = c(tot_fat_p_male_tanita_g$a0, as.vector(tot_fat_p_male_tanita_g$beta)), 
                             totfatpfemaletanitag = c(tot_fat_p_female_tanita_g$a0, as.vector(tot_fat_p_female_tanita_g$beta)),
                             trunkfatmaletanitag = c(trunk_fat_male_tanita_g$a0, as.vector(trunk_fat_male_tanita_g$beta)), 
                             trunkfatfemaletanitag = c(trunk_fat_female_tanita_g$a0, as.vector(trunk_fat_female_tanita_g$beta)),
                             l1fatmaletanitag = c(l1_fat_male_tanita_g$a0, as.vector(l1_fat_male_tanita_g$beta)), 
                             l1fatfemaletanitag = c(l1_fat_female_tanita_g$a0, as.vector(l1_fat_female_tanita_g$beta)),
                             appleanmaletanitag = c(app_lean_male_tanita_g$a0, as.vector(app_lean_male_tanita_g$beta)), 
                             appleanfemaletanitag = c(app_lean_female_tanita_g$a0, as.vector(app_lean_female_tanita_g$beta))
)

circum_coefs <- data.frame(varnames = c('int', rownames(tot_fat_male_circ$beta)),
                             totfatmalecircum = c(tot_fat_male_circ$a0, as.vector(tot_fat_male_circ$beta)), 
                             totfatfemalecircum = c(tot_fat_female_circ$a0, as.vector(tot_fat_female_circ$beta)),
                             totleanmalecircum = c(tot_lean_male_circ$a0, as.vector(tot_lean_male_circ$beta)), 
                             totleanfemalecircum = c(tot_lean_female_circ$a0, as.vector(tot_lean_female_circ$beta)),
                             totfatpmalecircum = c(tot_fat_p_male_circ$a0, as.vector(tot_fat_p_male_circ$beta)), 
                             totfatpfemalecircum = c(tot_fat_p_female_circ$a0, as.vector(tot_fat_p_female_circ$beta)),
                             trunkfatmalecircum = c(trunk_fat_male_circ$a0, as.vector(trunk_fat_male_circ$beta)), 
                             trunkfatfemalecircum = c(trunk_fat_female_circ$a0, as.vector(trunk_fat_female_circ$beta)),
                             l1fatmalecircum = c(l1_fat_male_circ$a0, as.vector(l1_fat_male_circ$beta)), 
                             l1fatfemalecircum = c(l1_fat_female_circ$a0, as.vector(l1_fat_female_circ$beta)),
                             appleanmalecircum = c(app_lean_male_circ$a0, as.vector(app_lean_male_circ$beta)), 
                             appleanfemalecircum = c(app_lean_female_circ$a0, as.vector(app_lean_female_circ$beta))
)

circumskin_coefs <- data.frame(varnames = c('int', rownames(tot_fat_male_circskin$beta)),
                           totfatmalecircumskin = c(tot_fat_male_circskin$a0, as.vector(tot_fat_male_circskin$beta)), 
                           totfatfemalecircumskin = c(tot_fat_female_circskin$a0, as.vector(tot_fat_female_circskin$beta)),
                           totleanmalecircumskin = c(tot_lean_male_circskin$a0, as.vector(tot_lean_male_circskin$beta)), 
                           totleanfemalecircumskin = c(tot_lean_female_circskin$a0, as.vector(tot_lean_female_circskin$beta)),
                           totfatpmalecircumskin = c(tot_fat_p_male_circskin$a0, as.vector(tot_fat_p_male_circskin$beta)), 
                           totfatpfemalecircumskin = c(tot_fat_p_female_circskin$a0, as.vector(tot_fat_p_female_circskin$beta)),
                           trunkfatmalecircumskin = c(trunk_fat_male_circskin$a0, as.vector(trunk_fat_male_circskin$beta)), 
                           trunkfatfemalecircumskin = c(trunk_fat_female_circskin$a0, as.vector(trunk_fat_female_circskin$beta)),
                           l1fatmalecircumskin = c(l1_fat_male_circskin$a0, as.vector(l1_fat_male_circskin$beta)), 
                           l1fatfemalecircumskin = c(l1_fat_female_circskin$a0, as.vector(l1_fat_female_circskin$beta)),
                           appleanmalecircumskin = c(app_lean_male_circskin$a0, as.vector(app_lean_male_circskin$beta)), 
                           appleanfemalecircumskin = c(app_lean_female_circskin$a0, as.vector(app_lean_female_circskin$beta))
)

merged_coefs <- left_join(full_coefs, tanita_only_coefs, by = 'varnames')
merged_coefs <- left_join(merged_coefs, tanita_c_coefs, by = 'varnames')
merged_coefs <- left_join(merged_coefs, tanita_s_coefs, by = 'varnames')
merged_coefs <- left_join(merged_coefs, tanita_g_coefs, by = 'varnames')
merged_coefs <- left_join(merged_coefs, circum_coefs, by = 'varnames')
merged_coefs <- left_join(merged_coefs, circumskin_coefs, by = 'varnames')
write.csv(merged_coefs, 'merged_coefs.csv')


# Table S2 - MAE of LASSO for different sets of predictors (as above)
totfatmale <- rbind(total_fat_m_tanita,
                    total_fat_m_tanita_c,
                    total_fat_m_tanita_g,
                    total_fat_m_tanita_s,
                    total_fat_m_circ,
                    total_fat_m_circskin,
                    tfm2)
# only interested in saving MAE column
# think the 7 different models can become the columns
# and outcome-gender pairs can be rows
totfatmale[,6]

totfatfemale <- rbind(total_fat_f_tanita,
                    total_fat_f_tanita_c,
                    total_fat_f_tanita_g,
                    total_fat_f_tanita_s,
                    total_fat_f_circ,
                    total_fat_f_circskin,
                    tfm6)


totleanmale <- rbind(total_lean_m_tanita,
                       total_lean_m_tanita_c,
                       total_lean_m_tanita_g,
                       total_lean_m_tanita_s,
                       total_lean_m_circ,
                       total_lean_m_circskin,
                       tlm2)

totleanfemale <- rbind(total_lean_f_tanita,
                      total_lean_f_tanita_c,
                      total_lean_f_tanita_g,
                      total_lean_f_tanita_s,
                      total_lean_f_circ,
                      total_lean_f_circskin,
                      tlm6)

totfatpmale <- rbind(total_fat_p_m_tanita,
                    total_fat_p_m_tanita_c,
                    total_fat_p_m_tanita_g,
                    total_fat_p_m_tanita_s,
                    total_fat_p_m_circ,
                    total_fat_p_m_circskin,
                    tfp2)


totfatpfemale <- rbind(total_fat_p_f_tanita,
                      total_fat_p_f_tanita_c,
                      total_fat_p_f_tanita_g,
                      total_fat_p_f_tanita_s,
                      total_fat_p_f_circ,
                      total_fat_p_f_circskin,
                      tfp6)

trunkpmale <- rbind(trunk_m_tanita,
                     trunk_m_tanita_c,
                     trunk_m_tanita_g,
                     trunk_m_tanita_s,
                     trunk_m_circ,
                     trunk_m_circskin,
                     trp2)


trunkpfemale <- rbind(trunk_f_tanita,
                       trunk_f_tanita_c,
                       trunk_f_tanita_g,
                       trunk_f_tanita_s,
                       trunk_f_circ,
                       trunk_f_circskin,
                       trp6)

l1pmale <- rbind(l1_m_tanita,
                    l1_m_tanita_c,
                    l1_m_tanita_g,
                    l1_m_tanita_s,
                    l1_m_circ,
                    l1_m_circskin,
                    l12)


l1pfemale <- rbind(l1_f_tanita,
                      l1_f_tanita_c,
                      l1_f_tanita_g,
                      l1_f_tanita_s,
                      l1_f_circ,
                      l1_f_circskin,
                      l16)

appmale <- rbind(app_m_tanita,
                 app_m_tanita_c,
                 app_m_tanita_g,
                 app_m_tanita_s,
                 app_m_circ,
                 app_m_circskin,
                 ap2)


appfemale <- rbind(app_f_tanita,
                   app_f_tanita_c,
                   app_f_tanita_g,
                   app_f_tanita_s,
                   app_f_circ,
                   app_f_circskin,
                   ap6)

supp_table_lassos <- data.frame(total_fat_male = totfatmale[,6],
           total_fat_female = totfatfemale[,6],
           total_lean_male = totleanmale[,6],
           total_lean_female = totleanfemale[,6],
           total_pcent_male = totfatpmale[,6],
           total_pcent_female = totfatpfemale[,6],
           trunk_pcent_male = trunkpmale[,6],
           trunk_pcent_female = trunkpfemale[,6],
           l1_pcent_male = l1pmale[,6],
           l1_pcent_female = l1pfemale[,6],
           app_lean_male = appmale[,6],
           app_lean_female = appfemale[,6])

supp_table_lassos <- t(supp_table_lassos)
colnames(supp_table_lassos) <- c('Tanita only', 'Tanita and circ',
                                 'Tanita and grip', 'Tanita and skinfold',
                                 'Circ only', 'Circ and skinfold', 'Full')

write.csv(supp_table_lassos, 'supp_table_lassos.csv')

# TABLE S3 - Performance for extra outcomes
# Will be handled in separate script!


# TABLE S4 - 1st and 99th percentiles for training variables
# TO DO: Correct this error
# either needs new list of variables after processing (preferable)
# Or need to move earlier in the setup

print_ext <- function(var_name){
  if(!is.character(var_name)){
    stop("Please use characters for variable name")
  }
  min_ext_male <- quantile(dxa_train_male[,var_name], 0.00, na.rm = TRUE)
  lower_ext_male <- quantile(dxa_train_male[,var_name], 0.01, na.rm = TRUE)
  upper_ext_male <- quantile(dxa_train_male[,var_name], 0.99, na.rm = TRUE)
  max_ext_male <- quantile(dxa_train_male[,var_name], 1.00, na.rm = TRUE)
  min_ext_female <- quantile(dxa_train_female[,var_name], 0.00, na.rm = TRUE)
  lower_ext_female <- quantile(dxa_train_female[,var_name], 0.01, na.rm = TRUE)
  upper_ext_female <- quantile(dxa_train_female[,var_name], 0.99, na.rm = TRUE)
  max_ext_female <- quantile(dxa_train_female[,var_name], 1.00, na.rm = TRUE)
  return(c(min_ext_male, lower_ext_male, upper_ext_male, max_ext_male,
           min_ext_female, lower_ext_female, upper_ext_female, max_ext_female))
}

ext_vals <- sapply(colnames(dxa_train_male[c(6:36, 38:50, 53:54)]), FUN = print_ext)
ext_vals <- t(ext_vals)
colnames(ext_vals) <- c('male_min', 'male_1', 'male_99', 'male_max',
                        'female_min', 'female_1', 'female_99', 'female_max')
write.csv(ext_vals, 'dxa_train_percentiles.csv')

####################################
# COMPARE REMOVED OBSERVATIONS
####################################

# Reload the data from scratch
# We will need ALL observations to compare those who have been removed
# To those who remain
# Missing age+sex info people will NOT be considered / still removed
# Because considered that these people were not actually in study


# Import data
wave3 <- read.dta13('./3FU_for share_Nick.dta')
wave3_plus <- read.dta13('./extract_forNick_070721.dta')

wave3 <- left_join(wave3, wave3_plus, by = 'ha1id')

# Remove those younger than 18 or missing sex
wave3 <- wave3 %>% filter(ha1dv_age >= 18, !is.na(ha1q5_5sex))
# n = 6309

table(wave3$ha1q5_5sex)

wave3 <- wave3 %>% mutate(ha1q34_12dl1l4_pcent1 = (ha1q34_10dl1l4_pcent1 + ha1q34_12dl1l4_pcent1)/2)

# JUST include the DXA variables
outcome_variables <- c('ha1q34_9atotal_fat', 'ha1q34_9btotal_lean', 'ha1q34_9dtotal_pcent_fat',
                   'ha1q34_6dtrunk_pcent_fat', 
                   'ha1q34_12dl1l4_pcent1',
                   'ha1q34_4alarm_fat', 'ha1q34_4blarm_lean', 'ha1q34_5ararm_fat', 'ha1q34_5brarm_lean',
                   'ha1q34_7alleg_fat', 'ha1q34_7blleg_lean', 'ha1q34_8arleg_fat', 'ha1q34_8brleg_lean'
                   )

wave3$has_dxa <- complete.cases(wave3[,outcome_variables])
# number drops to 3437
table(wave3$has_dxa)
# 1776 men, 1661 women

# Let's compare age, sex, and BMI between these groups
table(wave3$ha1q5_5sex, wave3$has_dxa)
chisq.test(wave3$ha1q5_5sex, wave3$has_dxa) # p = 0.06

summary(wave3$ha1dv_age[wave3$has_dxa])
summary(wave3$ha1dv_age[!wave3$has_dxa])
# Those with DXA seem to be older.
# mean with DXA is 37.7, mean without DXA is 34.1
# median is even more extreme difference, 39.8 vs 27.8
t.test(wave3$ha1dv_age[wave3$has_dxa], wave3$ha1dv_age[!wave3$has_dxa])
# p < 0.0001


summary(wave3$ha1q37_3_bmi[wave3$has_dxa])
summary(wave3$ha1q37_3_bmi[!wave3$has_dxa])
# These look similar to me
t.test(wave3$ha1q37_3_bmi[wave3$has_dxa], wave3$ha1q37_3_bmi[!wave3$has_dxa])
# p value = 0.3535

# How about those excluded for data quality problems?
# thought - make the complete case dataset now
# BUT keep ID column
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

# save with different name for later ref
# < 18 yo already filtered out so this is fine
wave3_more <- wave3 %>% filter(ha1dv_age >= 18)


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



# Then run data quality filtering checks
# and make cross tables by ID %in% filtered TRUE/FALSE
table(wave3_more$ha1q5_5sex, wave3_more$ha1id %in% wave3$ha1id)
chisq.test(wave3_more$ha1q5_5sex, wave3_more$ha1id %in% wave3$ha1id)
# p = 0.797

summary(wave3_more$ha1dv_age[wave3_more$ha1id %in% wave3$ha1id])
summary(wave3_more$ha1dv_age[!wave3_more$ha1id %in% wave3$ha1id])
t.test(wave3_more$ha1dv_age[wave3_more$ha1id %in% wave3$ha1id], wave3_more$ha1dv_age[!wave3_more$ha1id %in% wave3$ha1id])
# p = 0.271
# mean 37.7 vs 38.8

summary(wave3_more$ha1q37_3_bmi[wave3_more$ha1id %in% wave3$ha1id])
summary(wave3_more$ha1q37_3_bmi[!wave3_more$ha1id %in% wave3$ha1id])
t.test(wave3_more$ha1q37_3_bmi[wave3_more$ha1id %in% wave3$ha1id], wave3_more$ha1q37_3_bmi[!wave3_more$ha1id %in% wave3$ha1id])
# p value 0.276
# mean 20.8 vs 21.1


