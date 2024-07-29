# CGE Data Processing Script
#
# Script to process the data collected from CGE during Fall 2023 and Winter 2024 in the CGE
# (Control & Gambling Task with Eyetracking) study.


# STEP 1: SET YOUR WORKING DIRECTORY!
# # On PSH's computers...
# setwd('/Users/sokolhessner/Documents/gitrepos/cge/');
# # On Von's PC Laptop "tabletas"...
setwd('C:/Users/jvonm/Documents/GitHub/cge');


# STEP 2: then run from here on the same
config = config::get();

Sys.setenv(R_CONFIG_ACTIVE = 'tabletas');

et_processing_file_name = normalizePath(dir(pattern = glob2rx('cge_et_processing.R'), full.names = T, recursive = T));

# Run the Eye-Tracking Processing Script ###########
source(et_processing_file_name) # NOTE: This will take a long time!! 

# Prepare for the rest of the processing ###########
setwd(config$path$data$raw);

# List all the data files
rdmfn = dir(pattern = glob2rx('cgeRDM_*.csv'),full.names = T, recursive = T);
sspfn = dir(pattern = glob2rx('cgeSYMSPANbothReal_*.csv'), full.names = T, recursive = T);
ospfn = dir(pattern = glob2rx('cgeOSPANbothReal_*.csv'), full.names = T, recursive = T);
qualfn = dir(pattern = glob2rx('*Survey*.csv'), full.names = T, recursive = T);

# Identify the number of participants from the file listing
number_of_subjects = length(rdmfn);


### Qualtrics CGE Survey Processing ###
cat('Processing Survey data...\n')

raw_qualtrics_data = read.csv(qualfn[(length(qualfn))]); # Load the last Qualtrics file, assuming naming convention sorts the files so that last is most recent!

survey_colnames = c(
  'subjectID',
  'age',
  'gender',
  'ethnicity',
  'race',
  'education',
  'firstgen',
  'politicalorientation',
  'IUS_prospective',
  'IUS_inhibitory',
  'IUS',
  'NCS',
  'SNS_ability',
  'SNS_preference',
  'SNS',
  'PSS'
);

survey_data = array(data = NA, dim = c(number_of_subjects, length(survey_colnames)));
colnames(survey_data) <- survey_colnames;
survey_data = as.data.frame(survey_data)

raw_qualtrics_data$EI.1[15] = '007'; # replacing 'CGE007' with the numeric value
raw_qualtrics_data = raw_qualtrics_data[-3,]; # deleting an early test line

# Add attention check DG.4 - Need to have selected 3

# Make indices to identify which rows to keep!
ind_complete = raw_qualtrics_data$Finished == 1; # completed the survey
ind_nottest = raw_qualtrics_data$EI.1 < 900; # Subject IDs should be < 900

ind_overall = ind_complete & ind_nottest;

# Do the #s of Subjects match? 
cat(sprintf('Qualtrics data has %g participants; decision-making data has %g participants.\n', sum(ind_overall), number_of_subjects))

if (sum(ind_overall) != number_of_subjects) {
  warning('WARNING: The numbers of subjects from Qualtrics & behavioral data do not match!!')
} 

# Used the rows from EI.1 (participant number) to create subject IDs
survey_data$subjectID = as.numeric(raw_qualtrics_data$EI.1[ind_overall])
plot(survey_data$subjectID)

# Age of participants
survey_data$age = as.numeric(raw_qualtrics_data$DG.1[ind_overall])
plot(survey_data$age)

# Gender of participants (1 = Man; 2 = Woman; 3 = Non-binary; 4 = Genderqueer; 5 = Gender expansive; 6 = Two-spirited; 7 = 3rd Gender; 8 = Agender; 9 = Not sure; 10 = Other(text); 11 = Prefer not to say)


# Ethnicity of participants (1 = Hispanic/Latinx; 2 = Not Hispanic/Latinx; 3 = Prefer not to say)
survey_data$ethnicity = as.numeric(raw_qualtrics_data$DG.3[ind_overall])

# Race of participants (1 = American Indian/Alaska Native; 2 = Black/African-American; 3 = East Asian; 4 = Native Hawaiian/Pacific Islander; 5 = South Asian; 6 = White; 7 = Bi-/Multi-racial (text); 8 = Other (text); 9 = Prefer not to say)


# Education level of participants (1 = No school; 2 = to 8th grade; 3 = Some HS, no diploma; 4 = HS/GED; 5 = Trade school; 6 = AA/S; 7 = BA/S; 8 = MA/S; 9 = Professional degree; 10 = PhD)
survey_data$education = as.numeric(raw_qualtrics_data$DG.6[ind_overall])

# If participants are firstgen (1 = Yes; 2 = No; 3 = Unsure)
survey_data$firstgen = as.numeric(raw_qualtrics_data$DG.7[ind_overall])

# Political orientation of participants: Likert (1 = "Extremely conservative" to 9 = "Extremely liberal")
survey_data$politicalorientation = as.numeric(raw_qualtrics_data$DG.8[ind_overall])

# IUS-12 Scores (Intolerance for Uncertainty; Carleton et al., 2007): Total (12-60 & Subscales); Subscales (Prospective Anxiety: 1-7 & Inhibitory Anxiety: 8-12); Likert (1 = "Not at all characteristic of me" to 5 = "Entirely characteristic of me")
survey_data$IUS_prospective = as.numeric(raw_qualtrics_data$IUS.12_1[ind_overall]) + # Prospective Anxiety
                  as.numeric(raw_qualtrics_data$IUS.12_2[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_3[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_4[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_5[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_6[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_7[ind_overall]);

survey_data$IUS_inhibitory = as.numeric(raw_qualtrics_data$IUS.12_8[ind_overall]) + # Inhibitory Anxiety
                  as.numeric(raw_qualtrics_data$IUS.12_9[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_10[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_11[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_12[ind_overall]);

survey_data$IUS = as.numeric(raw_qualtrics_data$IUS.12_1[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_2[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_3[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_4[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_5[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_6[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_7[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_8[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_9[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_10[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_11[ind_overall]) +
                  as.numeric(raw_qualtrics_data$IUS.12_12[ind_overall]);

# NCS-18 Scores (Need for Cognition; Cacioppo et al., 1984): Total (18-450); Reverse score (3, 4, 5, 7, 8, 9, 12, 16, 17); Likert (1 = "Extremely uncharacteristic of me" to 5 = "Extremely characteristic of me")
NCS_3_R = 6 - as.numeric(raw_qualtrics_data$NCS.18_3[ind_overall]); # _R = reverse scoring the item(s)

NCS_4_R = 6 - as.numeric(raw_qualtrics_data$NCS.18_4[ind_overall]);

NCS_5_R = 6 - as.numeric(raw_qualtrics_data$NCS.18_5[ind_overall]);

NCS_7_R = 6 - as.numeric(raw_qualtrics_data$NCS.18_7[ind_overall]);

NCS_8_R = 6 - as.numeric(raw_qualtrics_data$NCS.18_8[ind_overall]);

NCS_9_R = 6 - as.numeric(raw_qualtrics_data$NCS.18_9[ind_overall]);

NCS_12_R = 6 - as.numeric(raw_qualtrics_data$NCS.18_12[ind_overall]);

NCS_16_R = 6 - as.numeric(raw_qualtrics_data$NCS.18_16[ind_overall]);

NCS_17_R = 6 - as.numeric(raw_qualtrics_data$NCS.18_17[ind_overall]);

survey_data$NCS = as.numeric(raw_qualtrics_data$NCS.18_1[ind_overall]) +
                  as.numeric(raw_qualtrics_data$NCS.18_2[ind_overall]) +
                  NCS_3_R +
                  NCS_4_R +
                  NCS_5_R +
                  as.numeric(raw_qualtrics_data$NCS.18_6[ind_overall]) +
                  NCS_7_R +
                  NCS_8_R +
                  NCS_9_R +
                  as.numeric(raw_qualtrics_data$NCS.18_10[ind_overall]) +
                  as.numeric(raw_qualtrics_data$NCS.18_11[ind_overall]) +
                  NCS_12_R +
                  as.numeric(raw_qualtrics_data$NCS.18_13[ind_overall]) +
                  as.numeric(raw_qualtrics_data$NCS.18_14[ind_overall]) +
                  as.numeric(raw_qualtrics_data$NCS.18_15[ind_overall]) +
                  NCS_16_R +
                  NCS_17_R +
                  as.numeric(raw_qualtrics_data$NCS.18_18[ind_overall]);

# SNS Scores (Subjective Numeracy; Fagerlin et al., 2007): Average (Total & Subscales); Reverse score (7); Subscales (Ability: 1-4 & Preference: 5-8); Ability Likert (1 = "Not at all good" to 6 = "Extremely good") & Prefernce Likert (5: 1 = Not at all helpful to 6 = Extremely helpful"; 6: 1 = "Always prefer words" to 6 = "Always prefer numbers"; 7: 1 = "Always prefer percentages" to 6 = "Always prefer words"; 8: 1 = "Never" to 6 = "Very often")
SNS_7_R = 7 - as.numeric(raw_qualtrics_data$SNS.7.R[ind_overall]);

SNS_tmpSum_ability = as.numeric(raw_qualtrics_data$SNS.1[ind_overall]) + # Ability
                     as.numeric(raw_qualtrics_data$SNS.2[ind_overall]) +
                     as.numeric(raw_qualtrics_data$SNS.3[ind_overall]) +
                     as.numeric(raw_qualtrics_data$SNS.4[ind_overall]);

survey_data$SNS_ability = SNS_tmpSum_ability/4

SNS_tmpSum_preference = as.numeric(raw_qualtrics_data$SNS.5[ind_overall]) + # Preference
                        as.numeric(raw_qualtrics_data$SNS.6[ind_overall]) +
                        SNS_7_R +
                        as.numeric(raw_qualtrics_data$SNS.8[ind_overall]);

survey_data$SNS_preference = SNS_tmpSum_preference/4

SNS_tmpSum = as.numeric(raw_qualtrics_data$SNS.1[ind_overall]) +
             as.numeric(raw_qualtrics_data$SNS.2[ind_overall]) +
             as.numeric(raw_qualtrics_data$SNS.3[ind_overall]) +
             as.numeric(raw_qualtrics_data$SNS.4[ind_overall]) +
             as.numeric(raw_qualtrics_data$SNS.5[ind_overall]) +
             as.numeric(raw_qualtrics_data$SNS.6[ind_overall]) +
             SNS_7_R +
             as.numeric(raw_qualtrics_data$SNS.8[ind_overall]);

survey_data$SNS = SNS_tmpSum/8

# PSS Scores (Perceived Stress; Cohen et al., 1983): Total (0 - 40); Reverse score (4, 5, 7, & 8); Likert (0 = "Never" to 4 = "Very Often")
PSS_4_R = 5 - as.numeric(raw_qualtrics_data$PSS.Matrix_4[ind_overall]);

PSS_5_R = 5 - as.numeric(raw_qualtrics_data$PSS.Matrix_5[ind_overall]);

PSS_7_R = 5 - as.numeric(raw_qualtrics_data$PSS.Matrix_7[ind_overall]);

PSS_8_R = 5 - as.numeric(raw_qualtrics_data$PSS.Matrix_8[ind_overall]);

survey_data$PSS = as.numeric(raw_qualtrics_data$PSS.Matrix_1[ind_overall]) +
                  as.numeric(raw_qualtrics_data$PSS.Matrix_2[ind_overall]) +
                  as.numeric(raw_qualtrics_data$PSS.Matrix_3[ind_overall]) +
                  PSS_4_R +
                  PSS_5_R +
                  as.numeric(raw_qualtrics_data$PSS.Matrix_6[ind_overall]) +
                  PSS_7_R +
                  PSS_8_R +
                  as.numeric(raw_qualtrics_data$PSS.Matrix_9[ind_overall]) +
                  as.numeric(raw_qualtrics_data$PSS.Matrix_10[ind_overall]); 

cat('Done.\n\n')

### Prepping for Subject-Level Task Data Loop ###

cat('Processing decision-making and working memory data... ')

# Store some basic information about size of the decision-making task
num_static_trials = 50;
num_dynamic_trials = 120;
number_of_dm_trials_per_person = num_static_trials + num_dynamic_trials; # static = 50, dynamic = 120

# Set up variables to hold decision-making data
column_names_dm = c(
  'trialnumber',
  'subjectnumber',
  'riskyopt1',
  'riskyopt2',
  'safe',
  'choice',
  'reactiontime',
  'outcome',
  'ischecktrial',
  'static0dynamic1',
  'easyP1difficultN1',
  'choiceP',
  'bestRho',
  'bestMu',
  'ospan',
  'symspan',
  'complexspan',
  'age',
  'gender',
  'ethnicity',
  'race',
  'education',
  'firstgen',
  'politicalorientation',
  'IUS_prospective',
  'IUS_inhibitory',
  'IUS',
  'NCS',
  'SNS_ability',
  'SNS_preference',
  'SNS',
  'PSS'
);

data_dm = array(data = NA, dim = c(0, length(column_names_dm)));
colnames(data_dm) <- column_names_dm

# Set up variables to hold working memory data
number_of_ospan_trials_per_person = 25;
number_of_sspan_trials_per_person = 14;

ospanExclude = c();
sspanExclude = c();
complexSpanExclude = as.data.frame(matrix(data=0, 
                                          nrow = number_of_subjects, 
                                          ncol=3, 
                                          dimnames=list(c(NULL), c("subjectnumber", "ospanExclude", "symspanExclude"))));
complexSpanExclude$subjectnumber = 1:number_of_subjects;

complexSpanScores = as.data.frame(matrix(data=NA, 
                                         nrow = number_of_subjects, 
                                         ncol=4, 
                                         dimnames=list(c(NULL), c("subjectnumber", "ospanScore", "symspanScore", "compositeSpanScore"))));
complexSpanScores$subjectnumber= 1:number_of_subjects

# Loop
for(s in 1:number_of_subjects){
  
  ### OSPAN DATA ### 
  
  ospantmpdata = read.csv(ospfn[s]);
  ospantmpdata$subid = as.integer(substr(ospfn[s],6,8));
  
  
  if (any(ospantmpdata$percentCorrectMath[is.finite(ospantmpdata$percentCorrectMath)]<85)){
    ospanExclude = c(ospanExclude,ospantmpdata$subid[1]);
    complexSpanExclude$ospanExclude[s] = 1;
  } else {
    correctIndospan = which(ospantmpdata$correctCount == ospantmpdata$setSize)
    complexSpanScores$ospanScore[s] = sum(ospantmpdata$correctCount[correctIndospan])/number_of_ospan_trials_per_person;
  }
  
  ### SYMSPAN DATA ###
  sspantmpdata = read.csv(sspfn[s]);
  sspantmpdata$subid = as.integer(substr(sspfn[s],6,8));
  
  if (any(sspantmpdata$percentCorrectSym[is.finite(sspantmpdata$percentCorrectSym)]<85)){
    sspanExclude = c(sspanExclude,sspantmpdata$subid[1]);
    complexSpanExclude$symspanExclude[s] = 1;
  } else {
    correctIndsymspan = which(sspantmpdata$squareCorrectCount == sspantmpdata$setSize)
    complexSpanScores$symspanScore[s] = sum(sspantmpdata$squareCorrectCount[correctIndsymspan])/number_of_sspan_trials_per_person;
  }
  
  
  ### COMPOSITE SPAN ###
  if ((complexSpanExclude$ospanExclude[s] == 0) & (complexSpanExclude$symspanExclude[s] == 0)){ # if both scores available
    complexSpanScores$compositeSpanScore[s] = mean(c((complexSpanScores$ospanScore[s]),(complexSpanScores$symspanScore[s]))); # average the two scores
  } else if ((complexSpanExclude$ospanExclude[s] == 1) & (complexSpanExclude$symspanExclude[s] == 0)){ # if only SymSpan
    complexSpanScores$compositeSpanScore[s] = complexSpanScores$symspanScore[s];
  } else if ((complexSpanExclude$ospanExclude[s] == 0) & (complexSpanExclude$symspanExclude[s] == 1)){ # if only OSpan
    complexSpanScores$compositeSpanScore[s] = complexSpanScores$ospanScore[s];
  }; # ... else, leave it NA
  
  
  
  ### RDM Data ###
  # Load in the data
  tmpdata = read.csv(rdmfn[s]); 
  
  # DECISION-MAKING DATA
  dm_data_to_add = array(data = NA, dim = c(number_of_dm_trials_per_person,length(column_names_dm)));

  dm_index_static = is.finite(tmpdata$staticTrials.thisTrialN);
  dm_index_dynamic = is.finite(tmpdata$dynamicTrials.thisTrialN);

  tmp_trialnum = c(tmpdata$staticTrials.thisTrialN[dm_index_static] + 1,
                   tmpdata$dynamicTrials.thisTrialN[dm_index_dynamic] + num_static_trials + 1);

  dm_data_to_add[,1] = tmp_trialnum; # trial number
  dm_data_to_add[,2] = s; # subject number

  tmp_riskyopt1 = c(tmpdata$riskyoption1[dm_index_static],
                    tmpdata$riskyoption1[dm_index_dynamic]);
  tmp_riskyopt2 = c(tmpdata$riskyoption2[dm_index_static],
                    tmpdata$riskyoption2[dm_index_dynamic]);
  tmp_safe = c(tmpdata$safeoption[dm_index_static],
               tmpdata$safeoption[dm_index_dynamic]);

  dm_data_to_add[,3:5] = cbind(tmp_riskyopt1,tmp_riskyopt2,tmp_safe) # dollar amounts

  dm_data_to_add[,6] = c(tmpdata$choices[dm_index_static],
                         tmpdata$choices[dm_index_dynamic]); # choices

  dm_data_to_add[,7] = c(tmpdata$realChoiceResp.rt[dm_index_static],
                         tmpdata$realChoiceResp.rt[dm_index_dynamic]); # RTs

  dm_data_to_add[,8] = c(tmpdata$outcomes[dm_index_static],
                         tmpdata$outcomes[dm_index_dynamic]); # outcomes

  dm_data_to_add[,9] = c(tmpdata$ischecktrial[dm_index_static],
                         array(data = 0, dim = c(1,num_dynamic_trials))); # is check trial

  dm_data_to_add[,10] = c(array(data = 0, dim = c(1,num_static_trials)),
                         array(data = 1, dim = c(1,num_dynamic_trials))); # static 0, dynamic 1

  dm_data_to_add[,11] = c(array(data = 0, dim = c(1,num_static_trials)),
                          tmpdata$easy0difficult1[dm_index_dynamic]*-2 + 1); # easy +1, difficult -1

  dm_data_to_add[,12] = c(array(data = NA, dim = c(1,num_static_trials)),
                          tmpdata$choiceP[dm_index_dynamic]); # choice probability on easy/diff dynamic trials

  dm_data_to_add[,13] = tmpdata$bestRho[is.finite(tmpdata$bestRho)];
  dm_data_to_add[,14] = tmpdata$bestMu[is.finite(tmpdata$bestMu)];
  
  dm_data_to_add[,15] = complexSpanScores$ospanScore[s];
  dm_data_to_add[,16] = complexSpanScores$symspanScore[s];
  dm_data_to_add[,17] = complexSpanScores$compositeSpanScore[s];
  
  dm_data_to_add[,18] = survey_data$age[s];
  
  dm_data_to_add[,19] = survey_data$gender[s];
  
  dm_data_to_add[,20] = survey_data$ethnicity[s];
  
  dm_data_to_add[,21] = survey_data$race[s];
  
  dm_data_to_add[,22] = survey_data$education[s];
  
  dm_data_to_add[,23] = survey_data$firstgen[s];
  
  dm_data_to_add[,24] = survey_data$politicalorientation[s];
  
  dm_data_to_add[,25] = survey_data$IUS_prospective[s];
  
  dm_data_to_add[,26] = survey_data$IUS_inhibitory[s];
  
  dm_data_to_add[,27] = survey_data$IUS[s];
  
  dm_data_to_add[,28] = survey_data$NCS[s];
  
  dm_data_to_add[,29] = survey_data$SNS_ability[s];
  
  dm_data_to_add[,30] = survey_data$SNS_preference[s];
  
  dm_data_to_add[,31] = survey_data$SNS[s];
  
  dm_data_to_add[,32] = survey_data$PSS[s];
  
  

  # Add this person's DM data to the total DM data.
  data_dm = rbind(data_dm,dm_data_to_add);
}

data_dm = cbind(data_dm, data_pupil[,3:ncol(data_pupil)]); # these should have the same # of rows! (number of trials x number of subjects long)

data_dm = as.data.frame(data_dm) # make it a data frame so it plays nice

cat('Done.\n')

# save out CSVs with the clean, compiled data!
cat('Saving out data... ')
setwd(config$path$data$processed);

write.csv(data_dm, file=sprintf('cge_processed_decisionmaking_data_%s.csv',format(Sys.Date(), format="%Y%m%d")),
          row.names = F);
write.csv(complexSpanScores, file=sprintf('cge_processed_complexspan_data_%s.csv',format(Sys.Date(), format="%Y%m%d")),
          row.names = F);
write.csv(survey_data, file=sprintf('cge_processed_survey_data_%s.csv',format(Sys.Date(), format="%Y%m%d")),
          row.names = F);

cat('Done.\n\nAll data has been processed.\n\n')
# all done!

