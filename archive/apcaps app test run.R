# Test run of app to use prediction equations

if(!require(shiny)) install.packages('shiny')
library(shiny)
if(!require(shinythemes)) install.packages('shinythemes')
library(shinythemes)
if(!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if(!require(tidyselect)) install.packages('tidyselect')
library(tidyselect)


# Will need to load in the model coefficients as well
merged_coefs <- read.csv('~/Downloads/merged_coefs.csv')
# Change NA values to 0 to avoid errors
merged_coefs[is.na(merged_coefs)] <- 0

# How to calculate will be
# Confirm inputs are in the correct units
# Then create all the interaction term needed
# Then multiply out to get predicted values

# Also load in 1st and 99th percentile values from training data
# Then generate warning message if values are beyond these limits
train_percentiles <- read.csv('~/Downloads/dxa_train_percentiles.csv')
colnames(train_percentiles)[1] <- 'var_name'
# Can have text triggered by ifelse statement
# Can be first variable in output set
# Print blank string unless otherwise noted
# Would be helpful if can display which variables - maybe can save indeces where extreme and then print
# Corresponding column names?

# Make a new variable name column that is easier to read
train_percentiles$var_name_clean <- c("Left Arm percent fat",
                                      "Left Arm fat mass",
                                      "Left Arm fat-free mass",
                                      "Left Arm muscle",
                                      "Trunk percent fat",
                                      "Trunk fat mass",
                                      "Trunk fat-free mass",
                                      "Trunk muscle",
                                      "Weight",
                                      "BMI",
                                      "Total body percent fat",
                                      "Total body fat mass",
                                      "Total body fat-free mass",
                                      "Total body water mass",
                                      "Whole body impedence",
                                      "Right leg impedence",
                                      "Left leg impedence",
                                      "Right arm impedence",
                                      "Left arm impedence",
                                      "Right Leg percent fat",
                                      "Right Leg fat mass",
                                      "Right Leg fat-free mass",
                                      "Right Leg muscle",
                                      "Left Leg percent fat",
                                      "Left Leg fat mass",
                                      "Left Leg fat-free mass",
                                      "Left Leg muscle",
                                      "Right Arm percent fat",
                                      "Right Arm fat mass",
                                      "Right Arm fat-free mass",
                                      "Right Arm muscle",
                                      "Age",
                                      "Calf circumference",
                                      "Head circumference",
                                      "Chest circumference",
                                      "Waist circumference",
                                      "Hip circumference",
                                      "Arm circumference",
                                      "Tricep skinfold",
                                      "Bicep skinfold",
                                      "Subscapular skinfold",
                                      "Suprailiac skinfold",
                                      "Calf skinfold",
                                      "Height",
                                      "Dominant grip strength",
                                      "Non dominant grip strength"
                                      )

# NEW VERSION:
# One input tab on side

ui <- fluidPage( 
  theme = shinythemes::shinytheme("darkly"),
  titlePanel("Body Composition Estimation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", label = "Gender",
                  choices = c('Male', 'Female')),
      numericInput('age', label = "Age (yrs)", value = 50
      ),
      numericInput('height', label = "Height (cm)", value = 150
      ),
      numericInput('weight', label = "Weight (kg)", value = 50
      ),
      numericInput('bmi', label = "BMI", value = 22
      ),
      numericInput('seg_la_pcent', label = "Left arm percent fat", value = 10
      ),
      numericInput('seg_la_mass', label = "Left arm fat mass (kg)", value = 1
      ),
      numericInput('seg_la_free_mass', label = "Left arm fat-free mass (kg)", value = 2
      ),
      numericInput('seg_la_muscle', label = "Left arm muscle mass (kg)", value = 2
      ),
      numericInput('seg_ra_pcent', label = "Right arm percent fat", value = 10
      ),
      numericInput('seg_ra_mass', label = "Right arm fat mass (kg)", value = 1
      ),
      numericInput('seg_ra_free_mass', label = "Right arm fat-free mass (kg)", value = 2
      ),
      numericInput('seg_ra_muscle', label = "Right arm muscle mass (kg)", value = 2
      ),
      numericInput('seg_ll_pcent', label = "Left leg percent fat", value = 10
      ),
      numericInput('seg_ll_mass', label = "Left leg fat mass (kg)", value = 1
      ),
      numericInput('seg_ll_free_mass', label = "Left leg fat-free mass (kg)", value = 5
      ),
      numericInput('seg_ll_muscle', label = "Left leg muscle mass (kg)", value = 5
      ),
      numericInput('seg_rl_pcent', label = "Right leg percent fat", value = 10
      ),
      numericInput('seg_rl_mass', label = "Right leg fat mass (kg)", value = 1
      ),
      numericInput('seg_rl_free_mass', label = "Right leg fat-free mass (kg)", value = 5
      ),
      numericInput('seg_rl_muscle', label = "Right leg muscle mass (kg)", value = 5
      ),
      numericInput('seg_tr_pcent', label = "Trunk percent fat", value = 10
      ),
      numericInput('seg_tr_mass', label = "Trunk fat mass (kg)", value = 5
      ),
      numericInput('seg_tr_free_mass', label = "Trunk fat-free mass (kg)", value = 20
      ),
      numericInput('seg_tr_muscle', label = "Trunk muscle mass (kg)", value = 20
      ),
      numericInput('tbf_pcent', label = 'Total body percent fat', value = 10
      ),
      numericInput('tbf_mass', label = 'Total body fat mass (kg)', value = 10
      ),
      numericInput('tbf_free_mass', label = 'Total body fat-free mass (kg)', value = 40
      ),
      numericInput('tbf_water', label = 'Total body water mass (kg)', value = 30
      ),
      numericInput('imp_whole_body', label = 'Total body impedance (Ohms)', value = 900
      ),
      numericInput('imp_rleg', label = 'Right leg impedance (Ohms)', value = 250
      ),
      numericInput('imp_lleg', label = 'Left leg impedance (Ohms)', value = 250
      ),
      numericInput('imp_rarm', label = 'Right arm impedance (Ohms)', value = 300
      ),
      numericInput('imp_larm', label = 'Left arm impedance (Ohms)', value = 300
      ),
      
      # CIRCUMFERENCES WILL GO HERE
      numericInput('calf_circ', label = 'Calf circumference (mm)', value = 300
      ),
      numericInput('head_circ', label = 'Head circumference (mm)', value = 550
      ),
      numericInput('cheste_circ', label = 'Expiratory chest circumference (mm)', value = 800
      ),
      numericInput('waist_circ', label = 'Waist circumference (mm)', value = 600
      ),
      numericInput('hip_circ', label = 'Hip circumference (mm)', value = 800
      ),
      numericInput('arm_circ', label = 'Mid-upper arm circumference (mm)', value = 300
      ),
      
      # SKINFOLDS WILL GO HERE
      numericInput('tri_fold', label = 'Tricep skinfold (mm)', value = 10
      ),
      numericInput('bi_fold', label = 'Bicep skinfold (mm)', value = 10
      ),
      numericInput('sub_fold', label = 'Sub skinfold (mm)', value = 10
      ),
      numericInput('sup_fold', label = 'Sup skinfold (mm)', value = 10
      ),
      numericInput('calf2_fold', label = 'Calf skinfold (mm)', value = 10
      ),
      
      
      # GRIP STRENGTH WILL GO HERE
      numericInput('dom_grip', label = 'Dominant grip strength (kg)', value = 10
      ),
      numericInput('nondom_grip', label = 'Non-dominant grip strength (kg)', value = 10
      )
    
      
      
    ),
  
    mainPanel(
      tabsetPanel(
        
        tabPanel('ALL STATIONS', 
                 mainPanel(("FAT MASS"), textOutput("fatMassTotalPredFull"),
                           ("LEAN MASS"), textOutput("leanMassTotalPredFull"),
                           ("FAT PERCENTAGE"), textOutput("fatPercTotalPredFull"),
                           ("TRUNK FAT PERCENTAGE"), textOutput("fatPercTrunkPredFull"),
                           ("L1-L4 FAT PERCENTAGE"), textOutput("fatPercL1PredFull"),
                           ("APPENDICULAR LEAN MASS"), textOutput("leanMassAppPredFull"),
                           ("Message"), textOutput("percentileMessage")
                           )
        ),
    
    
        tabPanel('TANITA ONLY', 
               mainPanel(
                         ("FAT MASS"), textOutput("fatMassTotalPredTAN"),
                         ("LEAN MASS"), textOutput("leanMassTotalPredTAN"),
                         ("FAT PERCENTAGE"), textOutput("fatPercTotalPredTAN"),
                         ("TRUNK FAT PERCENTAGE"), textOutput("fatPercTrunkPredTAN"),
                         ("L1-L4 FAT PERCENTAGE"), textOutput("fatPercL1PredTAN"),
                         ("APPENDICULAR LEAN MASS"), textOutput("leanMassAppPredTAN"),
                         ("Message"), textOutput("percentileMessage2"))
             ),
    
    
    
        tabPanel('TANITA + Circumference', 
               mainPanel(
                         ("FAT MASS"), textOutput("fatMassTotalPredTANCirc"),
                         ("LEAN MASS"), textOutput("leanMassTotalPredTANCirc"),
                         ("FAT PERCENTAGE"), textOutput("fatPercTotalPredTANCirc"),
                         ("TRUNK FAT PERCENTAGE"), textOutput("fatPercTrunkPredTANCirc"),
                         ("L1-L4 FAT PERCENTAGE"), textOutput("fatPercL1PredTANCirc"),
                         ("APPENDICULAR LEAN MASS"), textOutput("leanMassAppPredTANCirc"),
                         ("Message"), textOutput("percentileMessage3"))
             ),
        
        tabPanel('TANITA + Skinfolds', 
                 mainPanel(
                           ("FAT MASS"), textOutput("fatMassTotalPredTANSkin"),
                           ("LEAN MASS"), textOutput("leanMassTotalPredTANSkin"),
                           ("FAT PERCENTAGE"), textOutput("fatPercTotalPredTANSkin"),
                           ("TRUNK FAT PERCENTAGE"), textOutput("fatPercTrunkPredTANSkin"),
                           ("L1-L4 FAT PERCENTAGE"), textOutput("fatPercL1PredTANSkin"),
                           ("APPENDICULAR LEAN MASS"), textOutput("leanMassAppPredTANSkin"),
                           ("Message"), textOutput("percentileMessage4"))
        ),
        
        tabPanel('TANITA + Grip strength', 
                 mainPanel(
                           ("FAT MASS"), textOutput("fatMassTotalPredTANGrip"),
                           ("LEAN MASS"), textOutput("leanMassTotalPredTANGrip"),
                           ("FAT PERCENTAGE"), textOutput("fatPercTotalPredTANGrip"),
                           ("TRUNK FAT PERCENTAGE"), textOutput("fatPercTrunkPredTANGrip"),
                           ("L1-L4 FAT PERCENTAGE"), textOutput("fatPercL1PredTANGrip"),
                           ("APPENDICULAR LEAN MASS"), textOutput("leanMassAppPredTANGrip"),
                           ("Message"), textOutput("percentileMessage5"))
        ),
        
        tabPanel('Circumferences', 
                 mainPanel(
                           ("FAT MASS"), textOutput("fatMassTotalPredCirc"),
                           ("LEAN MASS"), textOutput("leanMassTotalPredCirc"),
                           ("FAT PERCENTAGE"), textOutput("fatPercTotalPredCirc"),
                           ("TRUNK FAT PERCENTAGE"), textOutput("fatPercTrunkPredCirc"),
                           ("L1-L4 FAT PERCENTAGE"), textOutput("fatPercL1PredCirc"),
                           ("APPENDICULAR LEAN MASS"), textOutput("leanMassAppPredCirc"),
                           ("Message"), textOutput("percentileMessage6"))
        ),
        
        tabPanel('Circumferences + Skinfolds', 
                 mainPanel(
                           ("FAT MASS"), textOutput("fatMassTotalPredCircSkin"),
                           ("LEAN MASS"), textOutput("leanMassTotalPredCircSkin"),
                           ("FAT PERCENTAGE"), textOutput("fatPercTotalPredCircSkin"),
                           ("TRUNK FAT PERCENTAGE"), textOutput("fatPercTrunkPredCircSkin"),
                           ("L1-L4 FAT PERCENTAGE"), textOutput("fatPercL1PredCircSkin"),
                           ("APPENDICULAR LEAN MASS"), textOutput("leanMassAppPredCircSkin"),
                           ("Message"), textOutput("percentileMessage7"))
        )
  )
)))



# Define server (computation of body comp values)
server <- function(input, output) {
  
  # Set reactive values for ALL variables
  # MIGHT NEED TO CHANGE THE UNITS ON SOME OF THESE
  sex_react <- reactive({
    input$sex
  })
  
  age_react <- reactive({
    input$age
  })
  
  height_react <- reactive({
    input$height*10
  })
  
  weight_react <- reactive({
    input$weight
  })
  
  bmi_react <- reactive({
    input$bmi
  })
  seg_la_pcent_react <- reactive({
    input$seg_la_pcent
  })
  seg_la_mass_react <- reactive({
    input$seg_la_mass
  })
  seg_la_free_mass_react <- reactive({
    input$seg_la_free_mass
  })
  seg_la_muscle_react <- reactive({
    input$seg_la_muscle
  })
  seg_ra_pcent_react <- reactive({
    input$seg_ra_pcent
  })
  seg_ra_mass_react <- reactive({
    input$seg_ra_mass
  })
  seg_ra_free_mass_react <- reactive({
    input$seg_ra_free_mass
  })
  seg_ra_muscle_react <- reactive({
    input$seg_ra_muscle
  })
  seg_ll_pcent_react <- reactive({
    input$seg_ll_pcent
  })
  seg_ll_mass_react <- reactive({
    input$seg_ll_mass
  })
  seg_ll_free_mass_react <- reactive({
    input$seg_ll_free_mass
  })
  seg_ll_muscle_react <- reactive({
    input$seg_ll_muscle
  })
  seg_rl_pcent_react <- reactive({
    input$seg_rl_pcent
  })
  seg_rl_mass_react <- reactive({
    input$seg_rl_mass
  })
  seg_rl_free_mass_react <- reactive({
    input$seg_rl_free_mass
  })
  seg_rl_muscle_react <- reactive({
    input$seg_rl_muscle
  })
  seg_tr_pcent_react <- reactive({
    input$seg_tr_pcent
  })
  seg_tr_mass_react <- reactive({
    input$seg_tr_mass
  })
  seg_tr_free_mass_react <- reactive({
    input$seg_tr_free_mass
  })
  seg_tr_muscle_react <- reactive({
    input$seg_tr_muscle
  })
  tbf_pcent_react <- reactive({
    input$tbf_pcent
  })
  tbf_mass_react <- reactive({
    input$tbf_mass
  })
  tbf_free_mass_react <- reactive({
    input$tbf_free_mass
  })
  tbf_water_react <- reactive({
    input$tbf_water
  })
  imp_whole_body_react <- reactive({
    input$imp_whole_body
  })
  imp_rleg_react <- reactive({
    input$imp_rleg
  })
  imp_lleg_react <- reactive({
    input$imp_lleg
  })
  imp_rarm_react <- reactive({
    input$imp_rarm
  })
  imp_larm_react <- reactive({
    input$imp_larm
  })
  
  calf_circ_react <- reactive({
    input$calf_circ
  })
  head_circ_react <- reactive({
    input$head_circ
  })
  cheste_circ_react <- reactive({
    input$cheste_circ
  })
  waist_circ_react <- reactive({
    input$waist_circ
  })
  hip_circ_react <- reactive({
    input$hip_circ
  })
  arm_circ_react <- reactive({
    input$arm_circ
  })
  tri_fold_react <- reactive({
    input$tri_fold
  })
  bi_fold_react <- reactive({
    input$bi_fold
  })
  sub_fold_react <- reactive({
    input$sub_fold
  })
  sup_fold_react <- reactive({
    input$sup_fold
  })
  calf2_fold_react <- reactive({
    input$calf2_fold
  })
  dom_grip_react <- reactive({
    input$dom_grip
  })
  nondom_grip_react <- reactive({
    input$nondom_grip
  })
  
  # Create imp_mod terms
  impmod_whole_react <- reactive({
    (input$height^2)/(input$imp_whole_body)
  })
  impmod_rleg_react <- reactive({
    (input$height^2)/(input$imp_rleg)
  })
  impmod_lleg_react <- reactive({
    (input$height^2)/(input$imp_lleg)
  })
  impmod_rarm_react <- reactive({
    (input$height^2)/(input$imp_rarm)
  })
  impmod_larm_react <- reactive({
    (input$height^2)/(input$imp_larm)
  })
  
  # Create circumference ratios
  waist_hip_react <- reactive({
    input$waist_circ/input$hip_circ
  })
  waist_height_react <- reactive({
    input$waist_circ/input$height
  })
  chest_waist_react <- reactive({
    input$cheste_circ/input$waist_circ
  })
  calf_height_react <- reactive({
    input$calf_circ/input$height
  })
  
  
  # CREATE OTHER INTERACTIONS - 
  # STILL NEEDS TO BE DONE
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # SOME OF THESE ARE PLACEHOLDERS 
  # AND NOT THE CORRECT FORMULA
  
  fmi_react <- reactive({
    input$tri_fold
  })
  
  lmi_react  <- reactive({
    input$bi_fold
  })
  
  bsi_react  <- reactive({
    input$sup_fold
  })
  
  log_skin_react <- reactive({
    log(input$tri_fold + input$bi_fold + input$sub_fold + input$sup_fold)
  })
  
  age_grip_react <- reactive({
    input$age*input$dom_grip
  })
  
  cama_react <- reactive({
    input$sub_fold
  })
  
  
  # Does this thing need to live here?
  percs_check <- reactive({
    list(seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                   seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                   tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                   imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                   seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                   seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(),
                   age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                   arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                   height_react(), dom_grip_react(), nondom_grip_react())
  })
  
  
  # Prepare outputs
  
  ##############
  # ALL STATIONS
  output$fatMassTotalPredFull = reactive({
    paste('Predicted fat mass is',
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatmalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                           seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                           tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                           imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                           seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                           seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                           age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                           arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                           height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                           impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                           lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatfemalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                           seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                           tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                           imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                           seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                           seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                           age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                           arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                           height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                           impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                           lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
                 
                 
                 
          ),
          'kg'
    )
  })
  
  output$leanMassTotalPredFull = reactive({
    paste('Predicted lean mass is',
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totleanmalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                         seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                         tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                         imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                         seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                         seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                         age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                         arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                         height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                         impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                         lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totleanfemalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                           seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                           tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                           imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                           seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                           seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                           age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                           arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                           height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                           impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                           lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
                 
                 
                 
          ),
          'kg'
    )
  })
  
  output$fatPercTotalPredFull = reactive({
    
    # Intro text
    paste("Predicted total fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatpmalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatpfemalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercTrunkPredFull = reactive({
    
    # Intro text
    paste("Predicted trunk fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$trunkfatmalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                           seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                           tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                           imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                           seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                           seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                           age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                           arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                           height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                           impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                           lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$trunkfatfemalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                             arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                             height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                             lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercL1PredFull = reactive({
    
    # Intro text
    paste("Predicted L1-L4 fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$l1fatmalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                        seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                        tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                        imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                        seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                        seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                        age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                        arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                        height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                        impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                        lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$l1fatfemalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$leanMassAppPredFull = reactive({
    
    # Intro text
    paste("Predicted appendicular lean mass is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$appleanmalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$appleanfemalefull*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
          )
          
          ,
          
          "kg"
    )
  })
  
  ##############
  # TANITA ONLY
  output$fatMassTotalPredTAN = reactive({
    paste('Predicted fat mass is',
    ifelse(sex_react() == 'Male',
           
           # Equation for males
           round(sum(merged_coefs$totfatmaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                      seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                      tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                      imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                      seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                      seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                      age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                      impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0))/1000, 1),
           
           # Equation for females
           round(sum(merged_coefs$totfatfemaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                        seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                        tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                        imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                        seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                        seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                        age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                        impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0))/1000, 1)
           
           
           
           ),
    'kg'
    )
  })
    
  output$leanMassTotalPredTAN = reactive({
    
    # Intro text
    paste("Predicted lean mass is",
    
    ifelse(sex_react() == 'Male',
           
           # Equation for males
           round(sum(merged_coefs$totleanmaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                  seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                  tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                  imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                  seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                  seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                  age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                  impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0))/1000, 1),
           
           # Equation for females
           round(sum(merged_coefs$totleanfemaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                          age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0))/1000, 1)
           
           )
  
  ,
    
  "kg"
  )
  })
  
  output$fatPercTotalPredTAN = reactive({
    
    # Intro text
    paste("Predicted total fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatpmaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatpfemaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                              seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                              tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                              imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                              seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                              seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                              age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                              impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercTrunkPredTAN = reactive({
    
    # Intro text
    paste("Predicted trunk fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$trunkfatmaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$trunkfatfemaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                              seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                              tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                              imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                              seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                              seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                              age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                              impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercL1PredTAN = reactive({
    
    # Intro text
    paste("Predicted L1-L4 fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$l1fatmaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$l1fatfemaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                               seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                               tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                               imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                               seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                               seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                               age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                               impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$leanMassAppPredTAN = reactive({
    
    # Intro text
    paste("Predicted appendicular lean mass is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$appleanmaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$appleanfemaletanita*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                              seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                              tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                              imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                              seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                              seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                              age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                              impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0))/1000, 1)
                 
          )
          
          ,
          
          "kg"
    )
  })
  
  
  ###############
  # TANITA + Circ
  output$fatMassTotalPredTANCirc = reactive({
    paste('Predicted fat mass is',
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatmaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), arm_circ_react(), 
                                                             0, 0, 0, 0, 0, height_react(), 0, 0, 
                                                             waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 
                                                             0, 0, 0, 0, 0, 0))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatfemaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                               seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                               tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                               imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                               seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                               seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                               age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), arm_circ_react(), 
                                                               0, 0, 0, 0, 0, height_react(), 0, 0, 
                                                               waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                               impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 
                                                               0, 0, 0, 0, 0, 0))/1000, 1)
                 ),
          'kg'
    )
  })
  
  output$leanMassTotalPredTANCirc = reactive({
    
    # Intro text
    paste("Predicted lean mass is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totleanmaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), arm_circ_react(), 
                                                0, 0, 0, 0, 0, height_react(), 0, 0, 
                                                waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 
                                                0, 0, 0, 0, 0, 0))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totleanfemaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                   seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                   tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                   imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                   seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                   seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                   age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), arm_circ_react(), 
                                                   0, 0, 0, 0, 0, height_react(), 0, 0, 
                                                   waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                   impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 
                                                   0, 0, 0, 0, 0, 0))/1000, 1)
                 
          ),
          
          "kg"
    )
  })
  
  output$fatPercTotalPredTANCirc = reactive({
    
    # Intro text
    paste("Predicted total fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatpmaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatpfemaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                              seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                              tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                              imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                              seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                              seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                              age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                              impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercTrunkPredTANCirc = reactive({
    
    # Intro text
    paste("Predicted trunk fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$trunkfatmaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$trunkfatfemaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                               seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                               tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                               imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                               seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                               seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                               age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                               impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercL1PredTANCirc = reactive({
    
    # Intro text
    paste("Predicted L1-L4 fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$l1fatmaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$l1fatfemaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0)), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$leanMassAppPredTANCirc = reactive({
    
    # Intro text
    paste("Predicted appendicular lean mass is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$appleanmaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$appleanfemaletanitac*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                              seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                              tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                              imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                              seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                              seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                              age_react(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, height_react(), 0, 0, 0, 0, 0, 0, impmod_whole_react(),
                                                              impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), 0, 0, 0, 0, 0, 0))/1000, 1)
                 
          )
          
          ,
          
          "kg"
    )
  })
  
  ##############
  # TANITA + SKIN
  output$fatMassTotalPredTANSkin = reactive({
    paste('Predicted fat mass is',
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatmaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                         seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                         tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                         imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                         seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                         seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                         age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                         arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                         height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                         impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                         lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatfemaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                           seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                           tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                           imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                           seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                           seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                           age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                           arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                           height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                           impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                           lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
                 
                 
                 
          ),
          'kg'
    )
  })
  
  output$leanMassTotalPredTANSkin = reactive({
    paste('Predicted lean mass is',
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totleanmaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totleanfemaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
                 
                 
                 
          ),
          'kg'
    )
  })
  
  output$fatPercTotalPredTANSkin = reactive({
    
    # Intro text
    paste("Predicted total fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatpmaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatpfemaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercTrunkPredTANSkin = reactive({
    
    # Intro text
    paste("Predicted trunk fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$trunkfatmaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                           seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                           tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                           imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                           seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                           seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                           age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                           arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                           height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                           impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                           lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$trunkfatfemaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                             arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                             height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                             lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercL1PredTANSkin = reactive({
    
    # Intro text
    paste("Predicted L1-L4 fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$l1fatmaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                        seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                        tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                        imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                        seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                        seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                        age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                        arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                        height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                        impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                        lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$l1fatfemaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$leanMassAppPredTANSkin = reactive({
    
    # Intro text
    paste("Predicted appendicular lean mass is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$appleanmaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$appleanfemaletanitas*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
          )
          
          ,
          
          "kg"
    )
  })
  
  ##############
  # TANITA + GRIP
  output$fatMassTotalPredTANGrip = reactive({
    paste('Predicted fat mass is',
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatmaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatfemaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                              seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                              tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                              imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                              seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                              seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                              age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                              arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                              height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                              impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                              lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
                 
                 
                 
          ),
          'kg'
    )
  })
  
  output$leanMassTotalPredTANGrip = reactive({
    paste('Predicted lean mass is',
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totleanmaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                             arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                             height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                             lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totleanfemaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                               seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                               tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                               imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                               seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                               seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                               age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                               arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                               height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                               impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                               lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
                 
                 
                 
          ),
          'kg'
    )
  })
  
  output$fatPercTotalPredTANGrip = reactive({
    
    # Intro text
    paste("Predicted total fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatpmaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                             arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                             height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                             lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatpfemaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                               seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                               tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                               imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                               seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                               seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                               age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                               arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                               height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                               impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                               lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercTrunkPredTANGrip = reactive({
    
    # Intro text
    paste("Predicted trunk fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$trunkfatmaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                              seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                              tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                              imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                              seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                              seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                              age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                              arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                              height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                              impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                              lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$trunkfatfemaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                                seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                                tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                                imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                                seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                                seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                                age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                                arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                                height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                                impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                                lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercL1PredTANGrip = reactive({
    
    # Intro text
    paste("Predicted L1-L4 fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$l1fatmaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                           seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                           tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                           imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                           seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                           seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                           age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                           arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                           height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                           impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                           lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$l1fatfemaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                             arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                             height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                             lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$leanMassAppPredTANGrip = reactive({
    
    # Intro text
    paste("Predicted appendicular lean mass is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$appleanmaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                             arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                             height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                             lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$appleanfemaletanitag*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                               seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                               tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                               imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                               seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                               seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                               age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                               arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                               height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                               impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                               lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
          )
          
          ,
          
          "kg"
    )
  })
  
  ##############
  # CIRCUMFERENCES
  output$fatMassTotalPredCirc = reactive({
    paste('Predicted fat mass is',
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatmalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                         seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                         tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                         imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                         seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                         seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                         age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                         arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                         height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                         impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                         lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatfemalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                           seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                           tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                           imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                           seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                           seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                           age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                           arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                           height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                           impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                           lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
                 
                 
                 
          ),
          'kg'
    )
  })
  
  output$leanMassTotalPredCirc = reactive({
    paste('Predicted lean mass is',
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totleanmalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totleanfemalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
                 
                 
                 
          ),
          'kg'
    )
  })
  
  output$fatPercTotalPredCirc = reactive({
    
    # Intro text
    paste("Predicted total fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatpmalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatpfemalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercTrunkPredCirc = reactive({
    
    # Intro text
    paste("Predicted trunk fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$trunkfatmalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                           seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                           tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                           imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                           seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                           seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                           age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                           arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                           height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                           impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                           lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$trunkfatfemalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                             arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                             height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                             lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercL1PredCirc = reactive({
    
    # Intro text
    paste("Predicted L1-L4 fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$l1fatmalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                        seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                        tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                        imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                        seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                        seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                        age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                        arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                        height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                        impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                        lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$l1fatfemalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$leanMassAppPredCirc = reactive({
    
    # Intro text
    paste("Predicted appendicular lean mass is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$appleanmalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$appleanfemalecircum*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
          )
          
          ,
          
          "kg"
    )
  })
  
  
  ##############
  # CIRCUMFERENCES + SKINFOLDS
  output$fatMassTotalPredCircSkin = reactive({
    paste('Predicted fat mass is',
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatmalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                           seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                           tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                           imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                           seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                           seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                           age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                           arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                           height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                           impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                           lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatfemalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                             arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                             height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                             lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
                 
                 
                 
          ),
          'kg'
    )
  })
  
  output$leanMassTotalPredCircSkin = reactive({
    paste('Predicted lean mass is',
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totleanmalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totleanfemalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                              seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                              tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                              imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                              seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                              seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                              age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                              arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                              height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                              impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                              lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
                 
                 
                 
          ),
          'kg'
    )
  })
  
  output$fatPercTotalPredCircSkin = reactive({
    
    # Intro text
    paste("Predicted total fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$totfatpmalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$totfatpfemalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                              seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                              tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                              imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                              seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                              seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                              age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                              arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                              height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                              impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                              lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercTrunkPredCircSkin = reactive({
    
    # Intro text
    paste("Predicted trunk fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$trunkfatmalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                             seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                             tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                             imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                             seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                             seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                             age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                             arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                             height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                             impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                             lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$trunkfatfemalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                               seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                               tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                               imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                               seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                               seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                               age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                               arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                               height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                               impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                               lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$fatPercL1PredCircSkin = reactive({
    
    # Intro text
    paste("Predicted L1-L4 fat percentage is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$l1fatmalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                          seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                          tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                          imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                          seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                          seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                          age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                          arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                          height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                          impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                          lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$l1fatfemalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react())), 1)
                 
          )
          
          ,
          
          "%"
    )
  })
  
  output$leanMassAppPredCircSkin = reactive({
    
    # Intro text
    paste("Predicted appendicular lean mass is",
          
          ifelse(sex_react() == 'Male',
                 
                 # Equation for males
                 round(sum(merged_coefs$appleanmalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                            seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                            tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                            imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                            seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                            seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                            age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                            arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                            height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                            impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                            lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1),
                 
                 # Equation for females
                 round(sum(merged_coefs$appleanfemalecircumskin*c(1, seg_la_pcent_react(), seg_la_mass_react(), seg_la_free_mass_react(), seg_la_muscle_react(), seg_tr_pcent_react(),
                                                              seg_tr_mass_react(), seg_tr_free_mass_react(), seg_tr_muscle_react(), weight_react(), bmi_react(),
                                                              tbf_pcent_react(), tbf_mass_react(), tbf_free_mass_react(), tbf_water_react(), imp_whole_body_react(),
                                                              imp_rleg_react(), imp_lleg_react(), imp_rarm_react(), imp_larm_react(), seg_rl_pcent_react(), seg_rl_mass_react(),
                                                              seg_rl_free_mass_react(), seg_rl_muscle_react(), seg_ll_pcent_react(), seg_ll_mass_react(), seg_ll_free_mass_react(),
                                                              seg_ll_muscle_react(), seg_ra_pcent_react(), seg_ra_mass_react(), seg_ra_free_mass_react(), seg_ra_muscle_react(), 0,
                                                              age_react(), calf_circ_react(), head_circ_react(), cheste_circ_react(), waist_circ_react(), hip_circ_react(), 
                                                              arm_circ_react(), tri_fold_react(), bi_fold_react(), sub_fold_react(), sup_fold_react(), calf2_fold_react(), 
                                                              height_react(), dom_grip_react(), nondom_grip_react(), waist_hip_react(), waist_height_react(), chest_waist_react(), calf_height_react(), impmod_whole_react(),
                                                              impmod_rleg_react(), impmod_lleg_react(), impmod_rarm_react(), impmod_larm_react(), fmi_react(), 
                                                              lmi_react(), bsi_react(), log_skin_react(), age_grip_react(), cama_react()))/1000, 1)
                 
          )
          
          ,
          
          "kg"
    )
  })
  
  
  #################################
  # Try to think of elegant way to include the percentile error messages
  # maybe create a vector of all the relevant reacts()
  # Then do something like 
  # WHERE X < percentiles$Y or X > percentiles$Z
  # Print percentiles$varname
  # Maybe somehow ignore 0's or NAs?
  # And have condition that if ALL false, then display nothing
  
  
  # Also consider adding some text about interpretation of results
  # Maybe better suited in UI section since it does not rely on any inputs.
  
  # Might need to create more readable list of variable names for printing purposes...
  # In meantime, print from train_percentiles$varname
  
  # Remember this is different for males and females.
  output$percentileMessage  = output$percentileMessage2 = output$percentileMessage3 = output$percentileMessage4 = output$percentileMessage5 = output$percentileMessage6 = output$percentileMessage7 = reactive({
  
    
    if(sex_react() == "Male"){
      ifelse(sum(!is.na(percs_check()) & ((percs_check() < train_percentiles$male_1) | (percs_check() > train_percentiles$male_99))) > 0,
             paste(c("The following stations have extreme values: ",
                    train_percentiles$var_name_clean[which(!is.na(percs_check()) & ((percs_check() < train_percentiles$male_1) | (percs_check() > train_percentiles$male_99)))]),
                    collapse = "\n"),
             "No stations were detected to have extreme values")
    }else{
      ifelse(sum(!is.na(percs_check()) & ((percs_check() < train_percentiles$female_1) | (percs_check() > train_percentiles$female_99))) > 0,
             paste(c("The following stations have extreme values: ",
                    train_percentiles$var_name_clean[which(!is.na(percs_check()) & ((percs_check() < train_percentiles$female_1) | (percs_check() > train_percentiles$female_99)))]),
                   collapse = "\n"),
             "No stations were detected to have extreme values")
    }
          
    
  })
  
# end of server  
}


shinyApp(ui = ui, server = server)
