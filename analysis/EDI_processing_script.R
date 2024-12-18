# EDI Data Processing Script
#
# Script to process the data collected from EDI during Summer 2024 and Fall 2024 in the EDI
# (Effort, Decision-making and Interoception) study.


# STEP 1: SET YOUR WORKING DIRECTORY! ##############
# # On PSH's computers...
# setwd('/Users/sokolhessner/Documents/gitrepos/edi/');
# # On SF's computers...
setwd('/Users/sophie/Desktop/GitHub/edi/');
# # On Von's PC Laptop "tabletas"...
# setwd('C:/Users/jvonm/Documents/GitHub/cge');


# STEP 2: then run from here on the same ##############
config = config::get();

et_processing_file_name = normalizePath(dir(pattern = glob2rx('edi_et_processing.R'), full.names = T, recursive = T));

## Run the Eye-Tracking Processing Script ###########
# source(et_processing_file_name) # NOTE: This will take a long time!!

## Prepare for the rest of the processing ###########
setwd(config$path$data$raw);

### List all the data files ##############
rdmfn = dir(pattern = glob2rx('edi*RDM*.csv'),full.names = T, recursive = T);
sspfn = dir(pattern = glob2rx('ediSYMSPANbothReal_*.csv'), full.names = T, recursive = T);
ospfn = dir(pattern = glob2rx('ediOSPANbothReal_*.csv'), full.names = T, recursive = T);
qualfn = dir(pattern = glob2rx('Qualtrics*.csv'), full.names = T, recursive = T);
manfn = dir(pattern = glob2rx('EDI_Post_Study_Questionnaire_Quant_HandEnteredData_*.csv'), full.names = T, recursive = T);

### Identify the number of participants from the file listing ##############
subjectIDs = c();
for (rfn in 1:length(rdmfn)){
  subjectIDs[rfn] = as.numeric(substr(rdmfn[rfn],6,8))
}

number_of_subjects = length(subjectIDs);


## Qualtrics EDI Survey Processing #################
cat('Processing Survey data...\n')

raw_qualtrics_data = read.csv(qualfn[(length(qualfn))]); # Load the last Qualtrics file, assuming naming convention sorts the files so that last is most recent!
manually_entered_data = read.csv(manfn[(length(manfn))]);

man_colnames_to_retain = c("D1_A1",
                           "D1_B1",
                           "D1_B2",
                           "D1_B3_1",
                           "D1_B3_2",
                           "D1_B3_3",
                           "D1_B3_4",
                           "D1_B4",
                           "D1_B8",
                           "D2_1A",
                           "D2_1B",
                           "D2_4",
                           "D2_5",
                           "D2_7",
                           "D2_9",
                           "Sex_F1M0",
                           "Height",
                           "Lbs",
                           "Age")
# For question text, see ReadMe_QuestionText.xlsx on the drive at shlab/EDI/documents/

## Copy made Dec. 17, 2024 for Day questions:
# D1_A1	  In general, how stressed did you feel during today's experiment?
# D1_B1	  How motivated were you to earn as much money as possible during the monetary decision-making task?
# D1_B2	  In general, how easy or difficult was it to decide between the gambles and the guaranteed alternatives?
# D1_B3_1	How easy/difficult was it to make choices on ‘one much better’ trials, e.g., when the between the gamble and the guaranteed alternative felt pretty different to you?
# D1_B3_2	On ‘one much better’ trials, e.g., when the between the gamble and the guaranteed alternative felt pretty different to you, how much mental effort did you put into making your decision?
# D1_B3_3	How would you rate your decision-making experience on ‘similar’ trials, e.g., when the gambles and guaranteed alternatives felt similarly good to you?
# D1_B3_4	On ‘similar’ trials, e.g., when the gambles and guaranteed alternatives felt similarly good to you, how much mental effort did you put into making your decision?
# D1_B4	  In the monetary decision-making task, you did not receive any actual money. How strongly did this influence your motivation to do the task?
# D1_B8	  In general, when making tough decisions, how intensely do you experience the feeling of mental effort?
# D2_1A	  How easy or difficult was it to judge whether the tones were in sync with or delayed from your heartbeat
# D2_1B	  How easy or difficult was it to rate your confidence?
# D2_4	  On what percentage of the trials do you think you were correct (from chance, 50%, to perfect, 100%)?
# D2_5	  This question is about how closely your confidence ratings corresponded with your judgments. Let’s say a 0 indicates that your confidence rating had no correspondence to your performance (i.e. high confidence and low confidence trials had the same chance of being correct), and a 10 indicates that your confidence rating perfectly matched the chance of you being correct (high confidence trials had the highest chance of being correct; low confidence trials had the lowest). What number would you say represents how closely your confidence ratings corresponded with your performance?
# D2_7	  On average, how many days per week do you exercise?
# D2_9	  During a typical session of exercise for you, how challenged are your heart and lungs? (from 1 = not challenged to 7 = very challenged)


STAI_col_names = c("STAI1", "STAI2", "STAI3", "STAI4","STAI5", "STAI6", "STAI7", "STAI8","STAI9", "STAI10", "STAI11", "STAI12","STAI13", "STAI14", "STAI15", "STAI16","STAI17", "STAI18", "STAI19", "STAI20");
STAI_data <- manually_entered_data[, STAI_col_names]

#Coding Reverse Scored Items using multiple iterations of a 'for loop' for survey items 1, 3, 6, 7, 10, 14, 16, and 19.
reversed_STAI <- c(1, 3, 6, 7, 10, 13, 14, 16, 19)
forward_STAI <- c(2, 4, 5, 8, 9, 11, 12, 15, 17, 18, 20) # nice to do this programmatically (i.e. to the exclusion of the reverse-coded items)
tmp_STAI_score = rowSums(cbind(STAI_data[,forward_STAI], (3-STAI_data[,reversed_STAI])));

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
  'SNS_ability',
  'SNS_preference',
  'SNS',
  'STAIT',
  man_colnames_to_retain,
  'BMI'
);

survey_data = array(data = NA, dim = c(number_of_subjects, length(survey_colnames)));
colnames(survey_data) <- survey_colnames;
survey_data = as.data.frame(survey_data)

# raw_qualtrics_data$EI.1[15] = '007'; # replacing 'CGE007' with the numeric value
# raw_qualtrics_data = raw_qualtrics_data[-3,]; # deleting an early test line

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
survey_data$gender = as.numeric(raw_qualtrics_data$DG.2[ind_overall])

# Ethnicity of participants (1 = Hispanic/Latinx; 2 = Not Hispanic/Latinx; 3 = Prefer not to say)
survey_data$ethnicity = as.numeric(raw_qualtrics_data$DG.3[ind_overall])

# Race of participants (1 = American Indian/Alaska Native; 2 = Black/African-American; 3 = East Asian; 4 = Native Hawaiian/Pacific Islander; 5 = South Asian; 6 = White; 7 = Bi-/Multi-racial (text); 8 = Other (text); 9 = Prefer not to say)
survey_data$race = as.numeric(raw_qualtrics_data$DG.5[ind_overall])

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

survey_data$STAIT = tmp_STAI_score;

survey_data[,man_colnames_to_retain] = manually_entered_data[,man_colnames_to_retain] # day 1 questions

survey_data$Sex_F1M0 = as.numeric(survey_data$Sex_F1M0 == 'F')

survey_data$Lbs = as.numeric(survey_data$Lbs) # there's an "n/a" b/c of a participant who didn't want to provide their weight
survey_data$BMI = survey_data$Lbs/(survey_data$Height^2)*703
# Classic BMI "categories":
# < 18.5 = underweight (0: 24/12/17)
# 18.5-24.9 = healthy (36: 24/12/17)
# 25-29.9 = overweight (12: 24/12/17)
# 30+ = obese (4: 24/12/17)

### List all the Heartbeat Detection (HBD) data files ##############
setwd(config$path$data$processed);
hbdfn = dir(pattern = 'EDI_HBD_taskperformance_output.csv');
hbd_data = read.csv(hbdfn);
for (h_id in 1:dim(hbd_data)[1]){
  hbd_data$SubjectID[h_id] = as.numeric(substr(hbd_data$SubjectID[h_id],4,6))
}
hbd_data$SubjectID = as.numeric(hbd_data$SubjectID)

# Merge survey & heartbeat detection data into a single survey item
survey_data = merge(x = survey_data, y = hbd_data, all.x = T, by.x = 'subjectID', by.y = 'SubjectID')

cat('Done.\n\n')

## Prepping for Subject-Level Task Data Loop #################

cat('Processing decision-making and working memory data... ')

setwd(config$path$data$raw);

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
  colnames(survey_data)[-1]
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
  sub_id = subjectIDs[s]
  ### OSPAN DATA ###
  
  osp_ind = which(grepl(sprintf('edi%03i',sub_id),ospfn))
  if (length(osp_ind) == 1){
    
    ospantmpdata = read.csv(ospfn[osp_ind]);
    ospantmpdata$subid = as.integer(substr(ospfn[osp_ind],6,8));
    
    
    if (any(ospantmpdata$percentCorrectMath[is.finite(ospantmpdata$percentCorrectMath)]<85)){
      ospanExclude = c(ospanExclude,ospantmpdata$subid[1]);
      complexSpanExclude$ospanExclude[s] = 1;
    } else {
      correctIndospan = which(ospantmpdata$correctCount == ospantmpdata$setSize)
      complexSpanScores$ospanScore[s] = sum(ospantmpdata$correctCount[correctIndospan])/number_of_ospan_trials_per_person;
    }
  } else {
    ospanExclude = c(ospanExclude,sub_id);
    complexSpanExclude$ospanExclude[s] = 1;
  }
  
  ### SYMSPAN DATA ###
  ssp_ind = which(grepl(sprintf('edi%03i',sub_id),sspfn))
  
  if (length(ssp_ind) == 1){
    
    sspantmpdata = read.csv(sspfn[ssp_ind]);
    sspantmpdata$subid = as.integer(substr(sspfn[ssp_ind],6,8));
    
    if (any(sspantmpdata$percentCorrectSym[is.finite(sspantmpdata$percentCorrectSym)]<85)){
      sspanExclude = c(sspanExclude,sspantmpdata$subid[1]);
      complexSpanExclude$symspanExclude[s] = 1;
    } else {
      correctIndsymspan = which(sspantmpdata$squareCorrectCount == sspantmpdata$setSize)
      complexSpanScores$symspanScore[s] = sum(sspantmpdata$squareCorrectCount[correctIndsymspan])/number_of_sspan_trials_per_person;
    }
  } else {
    sspanExclude = c(sspanExclude,sub_id);
    complexSpanExclude$symspanExclude[s] = 1;
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

  dm_index_static = is.finite(tmpdata$checkTrial);
  dm_index_dynamic = is.finite(tmpdata$easy0difficult1);

  tmp_trialnum = c(tmpdata$trialNumber[dm_index_static],
                   tmpdata$trialNumber[dm_index_dynamic] + num_static_trials);

  dm_data_to_add[,1] = tmp_trialnum; # trial number
  dm_data_to_add[,2] = s; # subject number

  tmp_riskyopt1 = c(tmpdata$gainValue[dm_index_static],
                    tmpdata$gainValue[dm_index_dynamic]);
  tmp_riskyopt2 = c(tmpdata$lossValue[dm_index_static],
                    tmpdata$lossValue[dm_index_dynamic]);
  tmp_safe = c(tmpdata$safeValue[dm_index_static],
               tmpdata$safeValue[dm_index_dynamic]);

  dm_data_to_add[,3:5] = cbind(tmp_riskyopt1,tmp_riskyopt2,tmp_safe) # dollar amounts

  dm_data_to_add[,6] = c(tmpdata$choiceMade[dm_index_static],
                         tmpdata$choiceMade[dm_index_dynamic]); # choices

  dm_data_to_add[,7] = c(tmpdata$choiceEnd[dm_index_static]-tmpdata$choiceStart[dm_index_static],
                         tmpdata$choiceEnd[dm_index_dynamic]-tmpdata$choiceStart[dm_index_dynamic]); # RTs

  dm_data_to_add[,8] = c(tmpdata$outcomeValue[dm_index_static],
                         tmpdata$outcomeValue[dm_index_dynamic]); # outcomes

  dm_data_to_add[,9] = c(tmpdata$checkTrial[dm_index_static],
                         array(data = 0, dim = c(1,num_dynamic_trials))); # is check trial

  dm_data_to_add[,10] = c(array(data = 0, dim = c(1,num_static_trials)),
                         array(data = 1, dim = c(1,num_dynamic_trials))); # static 0, dynamic 1

  dm_data_to_add[,11] = c(array(data = 0, dim = c(1,num_static_trials)),
                          tmpdata$easy0difficult1[dm_index_dynamic]*-2 + 1); # easy +1, difficult -1

  dm_data_to_add[,12] = c(array(data = NA, dim = c(1,num_static_trials)),
                          tmpdata$choiceProbability[dm_index_dynamic]); # choice probability on easy/diff dynamic trials

  dm_data_to_add[,13] = tmpdata$bestRho[is.finite(tmpdata$bestRho)];
  dm_data_to_add[,14] = tmpdata$bestMu[is.finite(tmpdata$bestMu)];
  
  # Span scores
  dm_data_to_add[,15] = complexSpanScores$ospanScore[s];
  dm_data_to_add[,16] = complexSpanScores$symspanScore[s];
  dm_data_to_add[,17] = complexSpanScores$compositeSpanScore[s];
  
  # Survey data + Heartbeat Detection Data
  survey_data_to_add = drop(survey_data[s,2:ncol(survey_data)]);
  for (n in 18:length(column_names_dm)){
    dm_data_to_add[,n] = as.numeric(survey_data_to_add[n-17])
  }
  # Add this person's DM data to the total DM data.
  data_dm = rbind(data_dm,dm_data_to_add);
}

data_dm = cbind(data_dm, data_pupil[,3:ncol(data_pupil)]); # these should have the same # of rows! (number of trials x number of subjects long)

data_dm = as.data.frame(data_dm) # make it a data frame so it plays nice

cat('Done.\n')


# save out CSVs with the clean, compiled data!
cat('Saving out data... ')
setwd(config$path$data$processed);

write.csv(data_dm, file=sprintf('edi_processed_decisionmaking_data_%s.csv',format(Sys.Date(), format="%Y%m%d")),
          row.names = F);
write.csv(complexSpanScores, file=sprintf('edi_processed_complexspan_data_%s.csv',format(Sys.Date(), format="%Y%m%d")),
          row.names = F);
write.csv(survey_data, file=sprintf('edi_processed_survey_data_%s.csv',format(Sys.Date(), format="%Y%m%d")),
          row.names = F);

cat('Done.\n\nAll data has been processed.\n\n')
# all done!

