# EDI Data Analysis Script
#
# Script to analyze the data collected in-person for EDI during Fall '24 & Winter '25
# (Eye-Tracking, Decision Making, and Interoception) study.
#

rm(list=ls()); # Clear the workspace


# Setting Up ###################################

# STEP 1: Set the working directory
# On PSH's computers...
# setwd('/Users/sokolhessner/Documents/gitrepos/edi/');
# # On Von's PC Laptop "tabletas"...
# setwd('C:/Users/jvonm/Documents/GitHub/cge');
# # Von - May need just in case tabletas disappears again Sys.setenv(R_CONFIG_ACTIVE = 'tabletas')
# Sys.setenv(R_CONFIG_ACTIVE = 'tabletas')
# On SF's computers...
setwd('/Users/sophie/Desktop/GitHub/edi/');

#setwd('/Users/shlab/Documents/GitHub/cge')
#Sys.setenv(R_CONFIG_ACTIVE = 'default')


# STEP 2: then run from here on the same
config = config::get()

# Loading Data ########################################
setwd(config$path$data$processed)

# Load Decision-Making Data
fn = dir(pattern = glob2rx('edi_processed_decisionmaking*.csv'),full.names = T);
number_of_files = length(fn) # ASSUMES YOU WANT THE MOST-RECENT PROCESSED DATA
data_dm = read.csv(fn[number_of_files]) # decision-making data

# Load Working Memory Data
fn = dir(pattern = glob2rx('edi_processed_complexspan_data_*.csv'),full.names = T);
number_of_files = length(fn) # ASSUMES YOU WANT THE MOST-RECENT PROCESSED DATA
complexSpanScores = read.csv(fn[number_of_files]) # working memory data

# Load Qualtrics Survey Data
fn = dir(pattern = glob2rx('edi_processed_survey_data_*.csv'),full.names = T);
number_of_files = length(fn) # ASSUMES YOU WANT THE MOST-RECENT PROCESSED DATA
survey_data = read.csv(fn[number_of_files]) # working memory data

number_of_subjects = length(unique(data_dm$subjectnumber));

# Load Trial-wise Heartbeat Detection Task (Tone Task) data
setwd(config$path$data$raw)
fn = dir(pattern = glob2rx('ediTask_EDI*.mat'),full.names = T, recursive = T);
library(R.matlab)

# Want columns for...
# - subject ID
# - trial number
# - reaction time
# - choice
# - syncstate
# - rating

hbd_tonetask_trials = array(data = NA, dim = c(0,6))
for (s in 1:length(fn)){
  tmp_hbd_data = readMat(fn[s])
  data_to_add = cbind(
    rep(as.numeric(tmp_hbd_data$output[[2]]), 160), # subject number
    1:160, # trial number
    tmp_hbd_data$output[[3]], # reaction time
    tmp_hbd_data$output[[4]], # choice
    tmp_hbd_data$output[[1]][[4]], # sync state
    tmp_hbd_data$output[[5]]) # rating
  hbd_tonetask_trials = rbind(hbd_tonetask_trials, data_to_add)
}
hbdcolnames = c('subjectnumber',
                'trialnumber',
                'reactiontime',
                'choiceS1D0',
                'syncstateS1D0',
                'confrating')
hbd_tonetask_trials = as.data.frame(hbd_tonetask_trials);
colnames(hbd_tonetask_trials) <- hbdcolnames

# Isolate the columns I want to compare
hbd_tonetask_trials$choiceS1D0 <- as.numeric(hbd_tonetask_trials$choiceS1D0)
hbd_tonetask_trials$syncstateS1D0 <- as.numeric(hbd_tonetask_trials$syncstateS1D0)

# Make a column that displays 1 if the choice is correct, 0 if incorrect
hbd_tonetask_trials$correct = ifelse(hbd_tonetask_trials$choiceS1D0 == hbd_tonetask_trials$syncstateS1D0, 1, 0)

# Calculate the % correct by participant
percent_correct_by_subject <- aggregate(correct ~ subjectnumber, data = hbd_tonetask_trials, FUN = function(x) {
  sum(x) / length(x) * 100  # Percentage of correct responses
})
print(percent_correct_by_subject)

#create a histogram to show percent correct
hist(percent_correct_by_subject$correct, 
     breaks = 10,        # Number of bins
     col = "purple",    # Color of the bars
     border = "black",   # Border color for bars
     main = "Distribution of Percent Correct",  # Title of the plot
     xlab = "Percent Correct",  # Label for x-axis
     ylab = "Number of Subjects",  # Label for y-axis
     xlim = c(0, 100))  # Set the x-axis limits from 0 to 100

#printing summary stats for percent correct
mean_percent_correct <- mean(percent_correct_by_subject$correct)
median_percent_correct <- median(percent_correct_by_subject$correct)
sd_percent_correct <- sd(percent_correct_by_subject$correct)
range_percent_correct <- range(percent_correct_by_subject$correct)
cat("Descriptive Statistics for Percent Correct:\n")
cat("Mean:", mean_percent_correct, "\n") #Mean = 57.88
cat("Median:", median_percent_correct, "\n") #Median = 54.19
cat("Standard Deviation:", sd_percent_correct, "\n") #SD = 13.29
cat("Range:", range_percent_correct[1], "to", range_percent_correct[2], "\n") #Range = 29.49 to 96.73

# Data Quality Checks & Exclusions ############################################

# Exclude on the basis of DM task performance
# (using... check trials, RTs, choices)

## Decision-Making Data Check ################################################
### EXCLUSION: Check Trials ################################################
check_trial_failurerate = array(dim = c(number_of_subjects,1));

for (subj in 1:number_of_subjects){
  tmpdata = data_dm[data_dm$subjectnumber == subj,];
  check_trial_index = which(tmpdata$ischecktrial==1);
  correct_answers = (0.5 * tmpdata$riskyopt1[check_trial_index] +
                       0.5 * tmpdata$riskyopt2[check_trial_index]) > tmpdata$safe[check_trial_index];
  check_trial_failurerate[subj] = length(which(!tmpdata$choice[check_trial_index] == correct_answers))/length(check_trial_index);

  # Plot the choice data
  plot(tmpdata$riskyopt1[tmpdata$choice == 1],tmpdata$safe[tmpdata$choice == 1], col = 'green',
       xlab = 'Risky Gain $', ylab = 'Safe $', main = paste0('All Subjects; Subj ', subj),
       xlim = c(0,30), ylim = c(0,12))
  points(tmpdata$riskyopt1[tmpdata$choice == 0],tmpdata$safe[tmpdata$choice == 0], col = 'red')
}

check_trial_criterion = 0.2; # The maximum percent of check trials that can be missed
# (there were 10 check trials)
# chance is 0.5, perfect is 0.0.

keep_check_trial = check_trial_failurerate <= check_trial_criterion; # 2 did not meet criteria: 16, 45, 76 (4/5/24)

### EXCLUSION: RTs ########################################################

mean_rts = array(dim = c(number_of_subjects,1));

for (subj in 1:number_of_subjects){
  tmpdata = data_dm[data_dm$subjectnumber == subj,];

  mean_rts[subj] = mean(tmpdata$reactiontime, na.rm = T)
}

keep_dm_rt = mean_rts > 0.85; # drop no one

mean_rts[keep_dm_rt]
hist(mean_rts[keep_dm_rt]) # histogram of mean rts
mean(mean_rts[keep_dm_rt]) # mean rt 1.548 seconds (2/25/25)

## Interoception Data Check #################################################

has_hbd_data = is.finite(survey_data$dprime)
#computing summary statistics for dprime
summary(survey_data$dprime)


### EXCLUSION: Tone Task ####################################################

# Exclude based on... 
# - Number of missed trials: 20 or more. 
exclusion_hbd_tt_num_trial_missed = 20
# - Reaction time: 100ms or faster on 20 or more trials
exclusion_hbd_tt_rtlim = .1 # 100ms, in units of seconds
exclusion_hbd_tt_rtlim_num = 20

# Missed Trials
keep_hbd_tt_missedtrials = rep(TRUE, number_of_subjects)
keep_hbd_tt_missedtrials[which(survey_data$missedTrials >= exclusion_hbd_tt_num_trial_missed)] = FALSE

# Reaction Time
keep_hbd_tt_toofasttrials = rep(TRUE, number_of_subjects)
for (s in 1:number_of_subjects){
  if(any(hbd_tonetask_trials$subjectnumber == s)){
    tmp_hbd = hbd_tonetask_trials[hbd_tonetask_trials$subjectnumber == s,]
    num_hbd_tt_trials_too_fast = sum(tmp_hbd$reactiontime <= exclusion_hbd_tt_rtlim, na.rm = T)
    if(num_hbd_tt_trials_too_fast >= exclusion_hbd_tt_rtlim_num){
      keep_hbd_tt_toofasttrials[s] = FALSE
    }
  }
}

good_hbd_data = which(has_hbd_data & keep_hbd_tt_missedtrials & keep_hbd_tt_toofasttrials)

#printing the number of people who were kept and excluded on the HB Tone Task
num_kept_hbd_tt = sum(keep_hbd_tt_missedtrials & keep_hbd_tt_toofasttrials)
num_excluded_hbd_tt = number_of_subjects - num_kept_hbd_tt
# Print results
cat("Heartbeat Tone Task:\n")
cat("Kept:", num_kept_hbd_tt, "\n")
cat("Excluded:", num_excluded_hbd_tt, "\n")


### Survey Exclusions
# quick responses to surveys (still figuring out how that should be done)
# questionable pattern of responses to surveys (nail in the coffin)


#### Create clean data frames ####

keep_participants = which(keep_check_trial & keep_dm_rt);

# Create clean data frames for data!
clean_data_dm = data_dm[data_dm$subjectnumber %in% keep_participants,]
clean_data_complexspan = complexSpanScores[complexSpanScores$subjectnumber %in% keep_participants,]
clean_data_survey = survey_data[survey_data$subjectID %in% keep_participants,]
clean_hbd_data = hbd_tonetask_trials[hbd_tonetask_trials$subjectnumber %in% good_hbd_data,]

number_of_clean_subjects = length(keep_participants);
number_of_clean_subjects # 71 participants (Feb '25)

# Create a re-scaled version of trial number for use in subsequent analyses
clean_data_dm$trialnumberRS = clean_data_dm$trialnumber/max(clean_data_dm$trialnumber)

# Create a better-behaved version of dprime using a square-root transform
clean_data_dm$sqrtdprime = sqrt(clean_data_dm$dprime)
clean_data_survey$sqrtdprime = sqrt(clean_data_survey$dprime)
# hist(clean_data_survey$dprime)
# hist(sqrt(clean_data_survey$dprime))


# BASIC DATA ANALYSIS ############################################

# (Separately for DM & WM, & Qualtrics data)
# Descriptives & simple averages of task performance

## Survey Analysis ############################################
library(corrplot)

# DATA: age, ethnicity, education, firstgen, politicalorientation, IUS_probability, IUS_inihibitory, IUS, NCS, SNS_ability, SNS_preference, SNS, PSS
# ANALYSIS: min, max, mean, SD, median, variance
# GRAPHS: histograms, scatterplots, correlograms, pairwise correlations

# Age
mean(clean_data_survey$age, na.rm = T) # 20.0 years old (2/17/25)
range(clean_data_survey$age, na.rm = T) # 18-27 (2/17/25)
sd(clean_data_survey$age, na.rm = T) # 1.57 years (2/17/25)
hist(clean_data_survey$age)

# Race (1 = American Indian/Alaska Native; 2 = Black/African-American; 3 = East Asian; 4 = Native Hawaiian/Pacific Islander; 5 = South Asian; 6 = White; 7 = Bi-/Multi-racial (text); 8 = Other (text); 9 = Prefer not to say)
clean_data_survey$race # was never loaded for some reason
sum(clean_data_survey$race == 1, na.rm = T) # 0
sum(clean_data_survey$race == 2, na.rm = T) # 0
sum(clean_data_survey$race == 3, na.rm = T) # 1
sum(clean_data_survey$race == 4, na.rm = T) # 0
sum(clean_data_survey$race == 5, na.rm = T) # 1
sum(clean_data_survey$race == 6, na.rm = T) # 67
sum(clean_data_survey$race == 7, na.rm = T) # 1
sum(clean_data_survey$race == 8, na.rm = T) # 0
sum(clean_data_survey$race == 9, na.rm = T) # 1

# Making a pie chart for race
race_counts <- table(clean_data_survey$race, useNA = "no")
race_labels <- c(
  "1" = "American Indian/Alaska Native",
  "2" = "Black/African-American",
  "3" = "East Asian",
  "4" = "Native Hawaiian/Pacific Islander",
  "5" = "South Asian",
  "6" = "White",
  "7" = "Bi-/Multi-racial",
  "8" = "Other",
  "9" = "Prefer not to say"
)

# corresponding pie chart labels to column names
names(race_counts) <- race_labels[names(race_counts)]
# labeling and formatting the chart
pie(race_counts, main = "Demographic Distribution by Race", col = rainbow(length(race_counts)))


# Gender of participants (1 = Man; 2 = Woman; 3 = Non-binary; 4 = Genderqueer; 5 = Gender expansive; 6 = Two-spirited; 7 = 3rd Gender; 8 = Agender; 9 = Not sure; 10 = Other(text); 11 = Prefer not to say)
sum(clean_data_survey$gender == 1, na.rm = T) # 16
sum(clean_data_survey$gender == 2, na.rm = T) # 55
sum(clean_data_survey$gender == 3, na.rm = T) # 0
sum(clean_data_survey$gender == 4, na.rm = T) # 0
sum(clean_data_survey$gender == 5, na.rm = T) # 0
sum(clean_data_survey$gender == 6, na.rm = T) # 0
sum(clean_data_survey$gender == 7, na.rm = T) # 0
sum(clean_data_survey$gender == 8, na.rm = T) # 0
sum(clean_data_survey$gender == 9, na.rm = T) # 0
sum(clean_data_survey$gender == 10, na.rm = T) # 0
sum(clean_data_survey$gender == 11, na.rm = T) # 0


# Ethnicity of participants (1 = Hispanic/Latinx; 2 = Not Hispanic/Latinx; 3 = Prefer not to say)
sum(clean_data_survey$ethnicity == 1, na.rm = T) # 4
sum(clean_data_survey$ethnicity == 2, na.rm = T) # 65
sum(clean_data_survey$ethnicity == 3, na.rm = T) # 2

# Education (1 = No school; 2 = to 8th grade; 3 = Some HS, no diploma; 4 = HS/GED; 5 = Trade school; 6 = AA/S; 7 = BA/S; 8 = MA/S; 9 = Professional degree; 10 = PhD)
sum(clean_data_survey$education == 1, na.rm = T) # 2
sum(clean_data_survey$education == 2, na.rm = T) # 0
sum(clean_data_survey$education == 3, na.rm = T) # 0
sum(clean_data_survey$education == 4, na.rm = T) # 59
sum(clean_data_survey$education == 5, na.rm = T) # 1
sum(clean_data_survey$education == 6, na.rm = T) # 1
sum(clean_data_survey$education == 7, na.rm = T) # 8
sum(clean_data_survey$education == 8, na.rm = T) # 0
sum(clean_data_survey$education == 9, na.rm = T) # 0
sum(clean_data_survey$education == 10, na.rm = T) # 0


# Main Questionnaire Scores:

# IUS - might be negative motivation to exert effort
mean(clean_data_survey$IUS, na.rm = T) # M = 33.20
sd(clean_data_survey$IUS, na.rm = T) # SD = 9.25
median(clean_data_survey$IUS, na.rm = T) # 33
range(clean_data_survey$IUS, na.rm = T) # 15-57
# SNS
mean(clean_data_survey$SNS, na.rm = T) # M = 3.95
sd(clean_data_survey$SNS, na.rm = T) # SD = 0.97
median(clean_data_survey$SNS, na.rm = T) # 4
range(clean_data_survey$SNS, na.rm = T) # 2.1 6.0

colnames(survey_data)
#D1_B2 is difficulty rating for Day 1
#D2_1A is difficulty rating for Day 2

# TODO Flesh out the below corr. matrix! Include...
# - sqrtdprime
# - metacognitive ability (MRatioBayesian)
# - difficulty rating session 1 
# - difficulty rating session 2
# - count accuracy (CountAccuracyM)
# - BMI
# - Trait anxiety (STAIT)

# cor_matrix = cor(cbind(
#   clean_data_survey$sqrtdprime,
#   clean_data_survey$MRatioBayesian,  
#   clean_data_survey$DifficultySession1,  
#   clean_data_survey$DifficultySession2,  
#   clean_data_survey$CountAccuracyM,  
#   clean_data_survey$BMI,  
#   clean_data_survey$STAIT,
#   clean_data_survey$IUS,
#   clean_data_survey$SNS,
#   clean_data_complexspan$compositeSpanScore
# ), use = 'complete.obs');

cor_data = cbind(
  clean_data_survey[,c('sqrtdprime','MRatioBayesian',
                       'D1_B2','D2_1A','CountAccuracyM',
                       'BMI','STAIT','IUS','SNS')],
  clean_data_complexspan['compositeSpanScore']
);

cor_matrix_pearson = cor(cor_data, use = 'complete.obs', method = 'pearson');
cor_matrix_pearson_pvalues = cor.mtest(cor_data, use = 'complete.obs', method = 'pearson');

cor_matrix_spearman = cor(cor_data, use = 'complete.obs', method = 'spearman');
cor_matrix_spearman_pvalues = cor.mtest(cor_data, use = 'complete.obs', method = 'spearman');


corrplot(cor_matrix_pearson, type = 'lower', diag = F, p.mat = cor_matrix_pearson_pvalues$p, sig.level = 0.01, col = rev(COL2('RdBu',200)))
corrplot(cor_matrix_spearman, type = 'lower', diag = F, p.mat = cor_matrix_spearman_pvalues$p, sig.level = 0.01, col = rev(COL2('RdBu',200)))

plot(cor_data);

plot(cor_data[,c('sqrtdprime', 'SNS', 'compositeSpanScore')])


# These suggest the following might be worth visualizing & exploring more:
# - BMI and...
#   - Day 1 difficulty (pearson & Spearman; higher BMI = easier)
#   - Day 2 difficulty (pearson only; higher BMI = easier)
# - IUS & STAIT (higher IUS, higher trait anxiety)
# - Dprime and...
#   - SNS (higher numeracy = higher dprime)
#   - Composite span (higher WMC = higher dprime)
#   - Day 2 difficulty (more difficult = lower performance)

#### D-prime & ...
# Pairwise correlation tests (Pearson by default)
cor.test(clean_data_survey$sqrtdprime, clean_data_survey$D2_1A) # r(50) = -0.38, p = 0.006
cor.test(clean_data_survey$sqrtdprime, clean_data_complexspan$compositeSpanScore) # r(47) = 0.40, p = 0.004
cor.test(clean_data_survey$sqrtdprime, clean_data_survey$SNS) # r(50) = 0.42, p = 0.002

plot(clean_data_survey$sqrtdprime, clean_data_survey$D2_1A)
plot(clean_data_survey$sqrtdprime, clean_data_complexspan$compositeSpanScore)
cor.test(clean_data_survey$sqrtdprime, clean_data_survey$SNS) #r(50) = 0.417, p = 0.002
plot(clean_data_survey$sqrtdprime, clean_data_survey$SNS)

# Better interoceptors, in this study, also...
# - rate the interoception task easier
# - have higher WMC
# - and have higher subjective numeracy
#
# ... why?? Engagement? Comfort in the study? Achievement orientation? ????

# n.s.
cor.test(clean_data_survey$sqrtdprime, clean_data_survey$MRatioBayesian) 
cor.test(clean_data_survey$sqrtdprime, clean_data_survey$D1_B2) 
cor.test(clean_data_survey$sqrtdprime, clean_data_survey$CountAccuracyM) 
cor.test(clean_data_survey$sqrtdprime, clean_data_survey$BMI) 
cor.test(clean_data_survey$sqrtdprime, clean_data_survey$STAIT) 
cor.test(clean_data_survey$sqrtdprime, clean_data_survey$IUS) #p = .08, trending neg corr

#### IUS & ...
cor.test(clean_data_survey$IUS, clean_data_survey$STAIT) # r(66) = 0.52, p = 0.000006
plot(clean_data_survey$IUS, clean_data_survey$STAIT)

# High trait anxiety people are intolerant of uncertainty

# n.s.
cor.test(clean_data_survey$IUS, clean_data_survey$SNS) #not correlated, p = 0.74
cor.test(clean_data_survey$IUS, clean_data_complexspan$compositeSpanScore) #not correlated, p-value = 0.1765
cor.test(clean_data_survey$IUS, clean_data_complexspan$compositeSpanScore, method = "spearman") #p-value = 0.09205, trend toward slight negative corr

# SNS & ...
cor.test(clean_data_survey$SNS, clean_data_complexspan$compositeSpanScore) #not correlated, p-value = 0.2795

cor.test(clean_data_survey$MRatioBayesian, clean_data_survey$D1_B2) 
cor.test(clean_data_survey$MRatioBayesian, clean_data_survey$D2_1A) 
cor.test(clean_data_survey$MRatioBayesian, clean_data_survey$CountAccuracyM) 
cor.test(clean_data_survey$MRatioBayesian, clean_data_survey$STAIT) 

cor.test(clean_data_survey$D1_B2, clean_data_survey$D2_1A) 
cor.test(clean_data_survey$D1_B2, clean_data_survey$CountAccuracyM) 
cor.test(clean_data_survey$D1_B2, clean_data_survey$STAIT) 

cor.test(clean_data_survey$D2_1A, clean_data_survey$CountAccuracyM) 
cor.test(clean_data_survey$D2_1A, clean_data_survey$STAIT) 

cor.test(clean_data_survey$CountAccuracyM, clean_data_survey$STAIT) 

# D1_B1	refered to 'How motivated were you to earn as much money as possible during the monetary decision-making task?'
cor.test(clean_data_survey$sqrtdprime, clean_data_survey$D1_B1) # Trend p = 0.08
cor.test(clean_data_survey$SNS, clean_data_survey$D1_B1) # n.s.
cor.test(clean_data_complexspan$compositeSpanScore, clean_data_survey$D1_B1) # n.s.
# So higher motivation is weakly related to higher d-prime, but not SNS or WMC
#
# Day 1 motivation is unrelated to SNS report and WMC measurement (composite span)
# Oddly, Day 1 motivation is weakly related to dprime (measured day 2)

#testing mean confidence in day 2 with relevant variables
cor.test(clean_data_survey$MeanConfidence, clean_data_survey$sqrtdprime) #positively correlated p = 0.009
cor.test(clean_data_survey$MeanConfidence, clean_data_survey$CountAccuracyM) #n.s
cor.test(clean_data_survey$MeanConfidence, clean_data_survey$STAIT) #n.s
cor.test(clean_data_survey$MeanConfidence, clean_data_survey$pcorrect) #positively correlated p = 0.02
cor.test(clean_data_survey$MeanConfidence, clean_data_survey$MRatioBayesian) #n.s

#mean confidence is correlated with pcorrect and sqrtdprime, suggesting that
#better interoceptors in this study were more confident overall regardless of accuracy of this conf


# optional?
# - Mean Confidence (how much swagger do they have)
# - any other D1 or D2 questions to include? 

# Basic visualization & analysis of difficulty ratings for Day 2
hist(clean_data_survey$D2_1A, 
     xlim = c(0,8), 
     breaks = seq(from = 0.5, to = 7.5, by = 1), 
     main = '', xlab = 'Interoception Difficulty')


#creating overlayed histograms to show difference in self reported diff for D1 & D2
# Create histogram for Day 2 (D2_1A)
hist(clean_data_survey$D2_1A, 
     xlim = c(0, 8), 
     breaks = seq(from = 0.5, to = 7.5, by = 1), 
     col = rgb(1, 0, 0, 0.5),  # red bars with 50% transparency
     main = "Task Difficulty Ratings", 
     xlab = "Difficulty Ratings of Tasks", 
     ylab = "Frequency")

# Overlay histogram for Day 1 (D1_B1)
hist(clean_data_survey$D1_B1, 
     breaks = seq(from = 0.5, to = 7.5, by = 1), 
     col = rgb(0, 0, 1, 0.5),  # blue bars 50% transparency
     add = TRUE)  # Adds to the existing histogram


# TAKEAWAYS: - does this still hold true?
# Because of several inter-item correlations, it's not wise to include these in the same model
# and we should expect some of these to perform similarly (i.e. PSS & IUS may be similar).
# However, because they're unrelated to composite span, we can freely include them alongside
# composite working memory span (and related variables).

par(mfrow = c(1,2)) # Allowing graphs to plot 1 x 2

hist(clean_data_survey$IUS, ylab = '', xlab = 'Score', main = 'Intolerance of Uncertainty (IUS)')
abline(v = mean(clean_data_survey$IUS, na.rm = T), col = 'blue', lwd = 5)

hist(clean_data_survey$SNS, ylab = '', xlab = 'Score', main = 'Subjective Numerancy Scale (SNS)')
abline(v = mean(clean_data_survey$SNS, na.rm = T), col = 'green', lwd = 5)

par(mfrow = c(1,1)) # Returning graphs to plot 1 at a time

# Make binary categorical variables for clean_data_dm based on each of the major Surveys - Median splits

clean_data_dm$IUS_HighP1_LowN1 = (clean_data_dm$IUS >= median(clean_data_survey$IUS, na.rm = T))*2-1;
clean_data_dm$SNS_HighP1_LowN1 = (clean_data_dm$SNS >= median(clean_data_survey$SNS, na.rm = T))*2-1;


# DM task Analysis ############################################
# Analysis for static trials: Mean p(gamble), optimization
mean_pgamble = array(dim = c(number_of_clean_subjects,1));
static_mean_pgamble = array(dim = c(number_of_clean_subjects, 1));
dynamic_mean_pgamble = array(dim = c(number_of_clean_subjects, 1));
easy_mean_pgamble = array(dim = c(number_of_clean_subjects, 1));
diff_mean_pgamble = array(dim = c(number_of_clean_subjects, 1));
easyACC_mean_pgamble = array(dim = c(number_of_clean_subjects, 1));
easyREJ_mean_pgamble = array(dim = c(number_of_clean_subjects, 1));

for (subj in 1:number_of_clean_subjects){
  subj_id = keep_participants[subj];
  tmpdata = clean_data_dm[clean_data_dm$subjectnumber == subj_id,]; # defines this person's data
  
  mean_pgamble[subj] = mean(tmpdata$choice, na.rm = T);
  static_mean_pgamble[subj] = mean(tmpdata$choice[tmpdata$static0dynamic1 == 0], na.rm=T);
  dynamic_mean_pgamble[subj] = mean(tmpdata$choice[tmpdata$static0dynamic1 == 1], na.rm=T);
  easy_mean_pgamble[subj] = mean(tmpdata$choice[tmpdata$easyP1difficultN1 == 1], na.rm = T);
  diff_mean_pgamble[subj] = mean(tmpdata$choice[tmpdata$easyP1difficultN1 == -1], na.rm = T);
  easyACC_mean_pgamble[subj] = mean(tmpdata$choice[(tmpdata$easyP1difficultN1 == 1) & (tmpdata$choiceP > .5)], na.rm = T);
  easyREJ_mean_pgamble[subj] = mean(tmpdata$choice[(tmpdata$easyP1difficultN1 == 1) & (tmpdata$choiceP < .5)], na.rm = T);

  # Plot the choice data
  plot(tmpdata$riskyopt1[tmpdata$choice == 1],tmpdata$safe[tmpdata$choice == 1], col = 'green',
       xlab = 'Risky Gain $', ylab = 'Safe $', main = paste0('Kept Subjects; Subj ', subj_id),
       xlim = c(0,30), ylim = c(0,12))
  points(tmpdata$riskyopt1[tmpdata$choice == 0],tmpdata$safe[tmpdata$choice == 0], col = 'red')
}

# Create simple summary object with p(gamble) descriptives
column_names_rdm = c(
  'mean_pgamble',
  'static_mean_pgamble',
  'dynamic_mean_pgamble',
  'easy_mean_pgamble',
  'diff_mean_pgamble',
  'easyACC_mean_pgamble',
  'easyREJ_mean_pgamble'
);

data_rdm = array(data = NA, dim = c(0, length(column_names_rdm)));
colnames(data_rdm) <-column_names_rdm
data_rdm <- data.frame(mean_pgamble,static_mean_pgamble,dynamic_mean_pgamble,easy_mean_pgamble,diff_mean_pgamble,easyACC_mean_pgamble,easyREJ_mean_pgamble)

#Q:standard error, means, range for gambling behavior for particpants

#Define function
# sem <- function(x) sd(x)/sqrt(length(x));
# SEM function (FANCY)
sem <- function(x){
  if(!is.null(dim(drop(x)))){ # if x is 2-dimensional
    sem_value = array(dim = dim(x)[1]) # make SEM vector assuming ROWS
    for (row_num in 1:dim(x)[1]){
      sem_value[row_num] = sd(x[row_num,], na.rm = T)/sqrt(sum(!is.na(x[row_num,])))
    }
  } else { # if unidimensional
    sem_value = sd(x, na.rm = T)/sqrt(sum(!is.na(x)))
  }
  return(sem_value)
}


# Calculate M's and SEMs
mean(mean_pgamble) # 0.497775
mean(static_mean_pgamble) # 0.5089402
mean(dynamic_mean_pgamble) # 0.4930427
sem(mean_pgamble) # 0.01143445
sem(static_mean_pgamble) # 0.01492569
sem(dynamic_mean_pgamble) # 0.01449466

mean(easyACC_mean_pgamble) # 0.9343611
mean(easyREJ_mean_pgamble) # 0.06401623
sem(easyACC_mean_pgamble) # 0.009493229
sem(easyREJ_mean_pgamble) # 0.00588866

mean(diff_mean_pgamble) # 0.4863754
sem(diff_mean_pgamble) # 0.02614511

min(mean_pgamble) # 0.2647059
max(mean_pgamble) # 0.8235294

# Q: How did risk-taking compare across the different dynamic trial types?
# e.g. easy accept vs. easy reject vs. difficult
hist(easyACC_mean_pgamble,
     col = rgb(0,1,0,.5), breaks = seq(from = 0, to = 1, by = 0.05), xlim = c(0,1),
     xlab = 'Mean Probability of Risk-Taking', ylab = 'Frequency',
     main = 'Average Risk-Taking Behavior By Trial Type')
hist(easyREJ_mean_pgamble,
     col = rgb(1,0,0,.5), breaks = seq(from = 0, to = 1, by = 0.05), add = TRUE)
hist(diff_mean_pgamble,
     col = rgb(0,0,1,.5), breaks = seq(from = 0, to = 1, by = 0.05), add = TRUE)
# A: As expected red < blue < green (easy reject < difficult < easy accept), blue is more spread out


# Q: Can we collapse across easy accept & reject trials based on relative distance of CHOICES from 0.5?
plot(abs(easyACC_mean_pgamble-.5),abs(easyREJ_mean_pgamble-.5),xlim = c(0,.5), ylim = c(0,.5),
     xlab = 'Easy Accept', ylab = 'Easy Reject',
     main = 'Distance of Observed p(risky) from 0.5 (chance) for EASY trial subtypes',
     pch = 19, col = rgb(0,0,0,.2), cex = 2)
lines(x = c(0,1), y = c(0,1), col = 'blue')

# Statistically test relative difficulty observed in easy ACC vs. easy REJ
t.test(abs(easyACC_mean_pgamble-.5), abs(easyREJ_mean_pgamble-.5), paired = T) # p = 0.07617
wilcox.test(abs(easyACC_mean_pgamble-.5), abs(easyREJ_mean_pgamble-.5), paired = T) # p = 0.1106

# A: Yes, we can treat all easy trials as similarly easy (whether easy ACC or REJ), not sig different 2/17/25


## Optimization Function Creation ############################################

# Function to calculate choice probabilities
choice_probability <- function(parameters, choiceset) {
  # A function to calculate the probability of taking a risky option
  # using a prospect theory model.
  # Assumes parameters are [rho, mu] as used in S-H 2009, 2013, 2015, etc.
  # Assumes choiceset has columns riskyoption1, riskyoption2, and safeoption
  #
  # PSH & AR June 2022

  # extract  parameters
  rho = as.double(parameters[1]); # risk attitudes
  mu = as.double(parameters[2]); # choice consistency

  # Correct parameter bounds
  if(rho <= 0){
    rho = .Machine$double.eps;
  }

  if(mu < 0){
    mu = 0;
  }

  # calculate utility of the two options
  utility_risky_option = 0.5 * choiceset$riskyoption1^rho +
    0.5 * choiceset$riskyoption2^rho;
  utility_safe_option = choiceset$safeoption^rho;

  # normalize values using this term
  div <- max(choiceset[,1:3])^rho; # decorrelates rho & mu

  # calculate the probability of selecting the risky option
  p = 1/(1+exp(-mu/div*(utility_risky_option - utility_safe_option)));

  return(p)
}

# Likelihood function
negLLprospect_cge <- function(parameters,choiceset,choices) {
  # A negative log likelihood function for a prospect-theory estimation.
  # Assumes parameters are [rho, mu] as used in S-H 2009, 2013, 2015, etc.
  # Assumes choiceset has columns riskyoption1, riskyoption2, and safeoption
  # Assumes choices are binary/logical, with 1 = risky, 0 = safe.
  #
  # Peter Sokol-Hessner
  # July 2021

  choiceP = choice_probability(parameters, choiceset);

  likelihood = choices * choiceP + (1 - choices) * (1-choiceP);
  likelihood[likelihood == 0] = 0.000000000000001; # 1e-15, i.e. 14 zeros followed by a 1

  nll <- -sum(log(likelihood));
  return(nll)
}


## Optimization ############################################
eps = .Machine$double.eps;
lower_bounds = c(eps, 0); # R, M
upper_bounds = c(2,80);
number_of_parameters = length(lower_bounds);

# Create placeholders for parameters, errors, NLL (and anything else you want)
number_of_iterations = 200; # 100 or more
estimated_parameters = array(dim = c(number_of_clean_subjects,2));
estimated_parameter_errors = array(dim = c(number_of_clean_subjects,2));
NLLs = array(dim = c(number_of_clean_subjects,1));

clean_data_dm$all_choiceP = NA;

cat('Beginning Optimization\n')

for (subj in 1:number_of_clean_subjects){
  subj_id = keep_participants[subj];
  print(subj_id)

  tmpdata = clean_data_dm[(clean_data_dm$subjectnumber == subj_id) &
                            (clean_data_dm$static0dynamic1 == 0) &
                            is.finite(clean_data_dm$choice),]; # defines this person's data

  temp_parameters = array(dim = c(number_of_iterations,number_of_parameters));
  temp_hessians = array(dim = c(number_of_iterations,number_of_parameters,number_of_parameters));
  temp_NLLs = array(dim = c(number_of_iterations,1));

  choiceset = as.data.frame(cbind(tmpdata$riskyopt1, tmpdata$riskyopt2, tmpdata$safe));
  colnames(choiceset) <- c('riskyoption1', 'riskyoption2', 'safeoption');

  # tic() # start the timer

  for(iter in 1:number_of_iterations){
    # Randomly set initial values within supported values
    # using uniformly-distributed values. Many ways to do this!

    initial_values = runif(number_of_parameters, min = lower_bounds, max = upper_bounds)

    temp_output = optim(initial_values, negLLprospect_cge,
                        choiceset = choiceset,
                        choices = tmpdata$choice,
                        lower = lower_bounds,
                        upper = upper_bounds,
                        method = "L-BFGS-B",
                        hessian = T)

    # Store the output we need access to later
    temp_parameters[iter,] = temp_output$par; # parameter values
    temp_hessians[iter,,] = temp_output$hessian; # SEs
    temp_NLLs[iter,] = temp_output$value; # the NLLs
  }

  # Compare output; select the best one
  NLLs[subj] = min(temp_NLLs); # the best NLL for this person
  best_ind = which(temp_NLLs == NLLs[subj])[1]; # the index of that NLL

  estimated_parameters[subj,] = temp_parameters[best_ind,] # the parameters
  estimated_parameter_errors[subj,] = sqrt(diag(solve(temp_hessians[best_ind,,]))); # the SEs

  # Calculating all choice probabilities for this participant, given best-fit parameters
  all_choice_ind = (clean_data_dm$subjectnumber == subj_id) & is.finite(clean_data_dm$choice)
  tmpdata = clean_data_dm[all_choice_ind,]; # defines this person's data

  choiceset = as.data.frame(cbind(tmpdata$riskyopt1, tmpdata$riskyopt2, tmpdata$safe));
  colnames(choiceset) <- c('riskyoption1', 'riskyoption2', 'safeoption');

  clean_data_dm$all_choiceP[all_choice_ind] = choice_probability(temp_parameters[best_ind,],choiceset);
}


## Grid Search ############################################

### Q: Does optimized analysis match grid search analysis?###

n_rho_values = 200; # SET THIS TO THE DESIRED DEGREE OF FINENESS
n_mu_values = 201; # IBID

rho_values = seq(from = 0.3, to = 2.2, length.out = n_rho_values); # the range of fit-able values
mu_values = seq(from = 7, to = 80, length.out = n_mu_values);

best_rhos = array(dim = c(number_of_clean_subjects,1));
best_mus = array(dim = c(number_of_clean_subjects,1));

cat('Beginning Grid Search\n')

for (subj in 1:number_of_clean_subjects){
  subj_id = keep_participants[subj];
  print(subj_id)
  #
  tmpdata = clean_data_dm[(clean_data_dm$subjectnumber == subj_id) &
                            (clean_data_dm$static0dynamic1 == 0) &
                            is.finite(clean_data_dm$choice),]; # defines this person's data

  choiceset = as.data.frame(cbind(tmpdata$riskyopt1, tmpdata$riskyopt2, tmpdata$safe));
  colnames(choiceset) <- c('riskyoption1', 'riskyoption2', 'safeoption');

  grid_nll_values = array(dim = c(n_rho_values, n_mu_values));

  for(r in 1:n_rho_values){
    for(m in 1:n_mu_values){
      grid_nll_values[r,m] = negLLprospect_cge(c(rho_values[r],mu_values[m]), choiceset, tmpdata$choice)
    }
  }

  min_nll = min(grid_nll_values); # identify the single best value
  indexes = which(grid_nll_values == min_nll, arr.ind = T); # Get indices for that single best value

  best_rhos[subj] = rho_values[indexes[1]]; # what are the corresponding rho & mu values?
  best_mus[subj] = mu_values[indexes[2]];
}

# look at all best rho & mu per participant
grid_bestRho = array(dim = c(number_of_clean_subjects,1));
grid_bestMu = array(dim = c(number_of_clean_subjects,1));
for (subj in 1:number_of_clean_subjects){
  subj_id = keep_participants[subj];

  tmpdata = clean_data_dm[clean_data_dm$subjectnumber == subj_id,];

  grid_bestRho[subj] = rho_values[unique(tmpdata$bestRho)];
  grid_bestMu[subj] = mu_values[unique(tmpdata$bestMu)];
}

# First, check fresh grid search best Rho & Mu against experiment-executed grid search Rho & Mu
# (should be trivial and match!)

if (any((grid_bestRho - best_rhos) != 0)){
  print('RHO MISMATCH!')
}else{
  print('RHO Grid search values match (as expected)')
}

if (any((grid_bestMu - best_mus) != 0)){
  print('MU MISMATCH!')
}else{
  print('MU Grid search values match (as expected)')
}


# Grid search does *not* replicate... WHY NOT? 

# Then check estimated parameters vs. grid search parameters
plot(grid_bestRho,estimated_parameters[,1], main = 'RHO',
     xlab = 'Grid Search Estimate', ylab = 'Optimization estimate',
     xlim = c(0, 2), ylim = c(0, 2))
lines(c(0, 2), c(0, 2))

plot(grid_bestMu,estimated_parameters[,2], main = 'MU',
     xlab = 'Grid Search Estimate', ylab = 'Optimization estimate',
     xlim = c(0, 100), ylim = c(0, 100))
lines(c(0, 100), c(0, 100))

hist(grid_bestRho - estimated_parameters[,1], xlim = c(-2,2),
     breaks = seq(from = -2, to = 2, by = 0.02), main = 'Difference in Rho Estimates',
     ylab = 'Participants', xlab = 'Grid estimate - MLE estimate')
hist(grid_bestMu - estimated_parameters[,2], xlim = c(-100,100),
     breaks = seq(from = -100, to = 100, by = 0.5), main = 'Difference in Mu Estimates',
     ylab = 'Participants', xlab = 'Grid estimate - MLE estimate')
# This is supposed to look silly! Should cluster around 0
# ... and it does, as of 4/5/24!

t.test(grid_bestRho, estimated_parameters[,1], paired = T) # no sig. diff (rho)... 4/11/24 (t(84) = 1.7804, p = 0.07862)
t.test(grid_bestMu, estimated_parameters[,2], paired = T) # no sig. diff (mu)... 4/11/24 (t(84) = 1.8545, p = 0.06718)

cor.test(grid_bestRho, estimated_parameters[,1]) # both extremely highly correlated. r(83) = 0.9974685, p = 2.2e-16
cor.test(grid_bestMu, estimated_parameters[,2]) # r(83) = 0.9997898, p = 2.2e-16

# A: YES, grid-search values match optimized values very closely.

# Who are our subjects?
mean_params = colMeans(estimated_parameters);

hist(estimated_parameters[,1], xlim = c(0,2),
     breaks = seq(from = 0, to = 2, by = 0.2), main = '',
     ylab = 'Participants', xlab = 'Rho (risk attitudes)')
lines(x = c(1,1), y = c(0,50), col = 'black', lwd = 2, lty = 'dashed')
lines(x = c(mean_params[1], mean_params[1]), y = c(0,50), col = 'green', lwd = 5, lend = 'butt')
# Seems evenly spread around 1 (risk neutral)

hist(estimated_parameters[,2], xlim = c(0,upper_bounds[2]),
     breaks = seq(from = 0, to = upper_bounds[2], by = 8), main = '',
     ylab = 'Participants', xlab = 'Mu (consistency)')
lines(x = c(mean_params[2], mean_params[2]), y = c(0,50), col = 'orange', lwd = 5, lend = 'butt')
# Seems positively skewed. Around 20-30.

## Setup for Regressions ############################################

### Create Continuous difficulty metric ###
clean_data_dm$diff_cont = abs(abs(clean_data_dm$choiceP - 0.5)*2-1); # JUST for the easy/difficult dynamic trials
clean_data_dm$all_diff_cont = abs(abs(clean_data_dm$all_choiceP - 0.5)*2-1); # for ALL trials

clean_data_dm$prev_all_diff_cont = c(NA,clean_data_dm$all_diff_cont[1:(length(clean_data_dm$all_diff_cont)-1)]) # for ALL trials
clean_data_dm$prev_all_diff_cont[clean_data_dm$trialnumber == 1] = NA;

clean_data_dm$prev2_all_diff_cont = c(NA,clean_data_dm$prev_all_diff_cont[1:(length(clean_data_dm$prev_all_diff_cont)-1)]) # for ALL trials
clean_data_dm$prev2_all_diff_cont[clean_data_dm$trialnumber == 1] = NA;

clean_data_dm$prev3_all_diff_cont = c(NA,clean_data_dm$prev2_all_diff_cont[1:(length(clean_data_dm$prev2_all_diff_cont)-1)]) # for ALL trials
clean_data_dm$prev3_all_diff_cont[clean_data_dm$trialnumber == 1] = NA;

clean_data_dm$prev4_all_diff_cont = c(NA,clean_data_dm$prev3_all_diff_cont[1:(length(clean_data_dm$prev3_all_diff_cont)-1)]) # for ALL trials
clean_data_dm$prev4_all_diff_cont[clean_data_dm$trialnumber == 1] = NA;

clean_data_dm$prev5_all_diff_cont = c(NA,clean_data_dm$prev4_all_diff_cont[1:(length(clean_data_dm$prev4_all_diff_cont)-1)]) # for ALL trials
clean_data_dm$prev5_all_diff_cont[clean_data_dm$trialnumber == 1] = NA;

clean_data_dm$prev6_all_diff_cont = c(NA,clean_data_dm$prev5_all_diff_cont[1:(length(clean_data_dm$prev5_all_diff_cont)-1)]) # for ALL trials
clean_data_dm$prev6_all_diff_cont[clean_data_dm$trialnumber == 1] = NA;

clean_data_dm$prev7_all_diff_cont = c(NA,clean_data_dm$prev6_all_diff_cont[1:(length(clean_data_dm$prev6_all_diff_cont)-1)]) # for ALL trials
clean_data_dm$prev7_all_diff_cont[clean_data_dm$trialnumber == 1] = NA;

clean_data_dm$prev8_all_diff_cont = c(NA,clean_data_dm$prev7_all_diff_cont[1:(length(clean_data_dm$prev7_all_diff_cont)-1)]) # for ALL trials
clean_data_dm$prev8_all_diff_cont[clean_data_dm$trialnumber == 1] = NA;

# EASY = 0
# DIFFICULT = 1


## Basic RT analysis ############################################

# Q:DO RT differ across conditions? Reaction times for easy risky, easy safe, and hard (hard > easy (either))
#mean easy RT
mean_rt_easy = array(dim = c(number_of_clean_subjects, 1));
mean_rt_diff = array(dim = c(number_of_clean_subjects, 1));
median_rt_easy = array(dim = c(number_of_clean_subjects, 1));
median_rt_diff = array(dim = c(number_of_clean_subjects, 1));
mean_rt_easyACC = array(dim = c(number_of_clean_subjects, 1));
mean_rt_easyREJ = array(dim = c(number_of_clean_subjects, 1));
var_rt_easy = array(dim = c(number_of_clean_subjects, 1));
var_rt_diff = array(dim = c(number_of_clean_subjects, 1));
mean_rt_static = array(dim = c(number_of_clean_subjects, 1));
mean_rt_dynamic = array(dim = c(number_of_clean_subjects, 1));


for (subj in 1:number_of_clean_subjects){
  subj_id = keep_participants[subj];
  tmpdata = data_dm[data_dm$subjectnumber == subj_id,]; # needs to be clean data dm not data dm????

  mean_rt_static[subj] = mean(tmpdata$reactiontime[(tmpdata$static0dynamic1 == 0)],na.rm = T);
  mean_rt_dynamic[subj] = mean(tmpdata$reactiontime[(tmpdata$static0dynamic1 == 1)], na.rm = T);

  # Identify just EASY dynamic data
  tmpdataEasyDyn = tmpdata[tmpdata$easyP1difficultN1 == 1,];

  # RTs within easy dynamic data
  mean_rt_easy[subj] = mean(tmpdataEasyDyn$reactiontime, na.rm = T)
  median_rt_easy[subj] = median(tmpdataEasyDyn$reactiontime, na.rm = T)
  mean_rt_easyACC[subj] = mean(tmpdataEasyDyn$reactiontime[(tmpdataEasyDyn$choiceP > .5)], na.rm = T);
  mean_rt_easyREJ[subj] = mean(tmpdataEasyDyn$reactiontime[(tmpdataEasyDyn$choiceP < .5)], na.rm = T);
  var_rt_easy[subj] = var(tmpdataEasyDyn$reactiontime, na.rm = T);

  # Identify just DIFFICULT dynamic data
  tmpdataDiffDyn = tmpdata[tmpdata$easyP1difficultN1 == -1,];

  mean_rt_diff[subj] = mean(tmpdataDiffDyn$reactiontime, na.rm = T)
  median_rt_diff[subj] = median(tmpdataDiffDyn$reactiontime, na.rm = T)
  var_rt_diff[subj] = var(tmpdataDiffDyn$reactiontime, na.rm = T);

}


# RTs between easy & difficult
plot(mean_rt_diff, mean_rt_easy,
     main = 'Average RT By Trial Type', xlab = "Mean RT Difficult", ylab = "Mean RT Easy",
     xlim = c(0.75, 2.75), ylim = c(0.75, 2.75))
lines(c(0, 2.75), c(0, 2.75))
points(mean(mean_rt_diff), mean(mean_rt_easy), pch = 16, col = rgb(1, 0, 1, .75), cex = 2.5)

plot(median_rt_diff, median_rt_easy,
     main = 'Median RT By Trial Type', xlab = "Median RT Difficult", ylab = "Median RT Easy",
     xlim = c(0.75, 2.75), ylim = c(0.75, 2.75))
lines(c(0, 2.75), c(0, 2.75))
points(mean(median_rt_diff), mean(median_rt_easy), pch = 16, col = rgb(1, 0, 1, .75), cex = 2.5)

# test easy RTs vs. diff. RTs
rt_mean_test = t.test(mean_rt_easy,mean_rt_diff, paired = T) # VERY significant t(70)= -7.7118, p = 6.197e-11
mean_diff = mean(mean_rt_diff, na.rm = T) # 1.686421
sd_diff = sd(mean_rt_diff, na.rm = T) # 0.3432346
mean_easy = mean(mean_rt_easy, na.rm = T) # 1.447765
sd_easy = sd(mean_rt_easy, na.rm = T) # 0.223144
rt_mean_test


# Cohen's D
Diff = length(mean_rt_diff)
Easy = length(mean_rt_easy)

pooled_sd = sqrt(((Diff - 1) * sd_diff^2 + (Easy - 1) * sd_easy^2)/ (Diff + Easy +2))

cohen_d = (mean_diff - mean_easy) / pooled_sd
cohen_d # 0.7013779

se_easy = sd_easy/sqrt(length(mean_rt_easy))
se_diff = sd_diff/sqrt(length(mean_rt_diff))
means = c(mean_easy, mean_diff)
ses = c(se_easy, se_diff)

choiceDifficultyGraph = barplot(means, beside = T, col = c("blue", "red"),
                                ylim = c(0, 2), names.arg = c("Easy", "Difficult"),
                                ylab = "Average RTs in Seconds", main = "Choice Difficulty")

arrows(choiceDifficultyGraph, means,
       choiceDifficultyGraph, means + ses,
       angle = 90, code = 3, length = 0.1)
box(bty="l")

rt_median_test = t.test(median_rt_easy,median_rt_diff, paired = T) # VERY significant (t(70) = -7.2756, p = 3.918e-10)
rt_median_test



#A: yes, looks like on average rt of difficult decisions was slower than avg rt of easy decisions 5/25/25

# Test for the across-participant variances in RTs by trial type
rt_mean_vartest = var.test(mean_rt_easy,mean_rt_diff); # F(70) = 0.434, p = 0.0006091
rt_mean_vartest
var(mean_rt_easy)
var(mean_rt_diff)
rt_median_vartest = var.test(median_rt_easy,median_rt_diff); # F(70) = 0.367, p = 4.172e-05
rt_median_vartest
# A: RTs are more variable across people for diff. than easy trials

# differences between variance of RTs in conditions WITHIN person
var_test_within = t.test(var_rt_easy,var_rt_diff, paired = T); # t(70) = -3.978, p = 0.000167
var_test_within

# yl = ceiling(max(c(max(mean_rt_diff),max(mean_rt_easy)))*10)/10
# plot(mean_rt_easy, mean_rt_diff, xlab = 'Easy trials', ylab = 'Difficult trials',
#      main = 'Across-Participant Variances in RT',
#      sub = sprintf('variance test p = %.03f', var_test_within$p.value),
#      xlim = c(0,yl), ylim = c(0,yl))
# lines(c(0,.6), c(0,.6), col = 'black', lty = 'dashed', lwd = 1.5)
# 


yl = ceiling(max(c(max(var_rt_diff),max(var_rt_easy)))*10)/10
plot(var_rt_easy, var_rt_diff, xlab = 'Easy trials', ylab = 'Difficult trials',
     main = 'Within-Trial-Type Variances in RT',
     sub = sprintf('paired t-test p = %.03f', var_test_within$p.value),
     xlim = c(0,yl), ylim = c(0,yl))
lines(c(0,.6), c(0,.6), col = 'black', lty = 'dashed', lwd = 1.5)

# RTs on difficult trials are more variable WITHIN person than their RTs on easy trials


# per person basis of easy RTs vs diff. RTs??
#Q: are easy choices similarly fast across people & are difficult choices similarly slower across people?
for (subj in 1:number_of_clean_subjects){
  subj_id = keep_participants[subj];
  tmpdata = clean_data_dm[clean_data_dm$subjectnumber == subj_id,]; # identify this person's data

  # test their easy trials vs. their difficult trials
  diff_stat_result = t.test(tmpdata$reactiontime[tmpdata$easyP1difficultN1 == 1], tmpdata$reactiontime[tmpdata$easyP1difficultN1 == -1]);
  var_stat_results = var.test(tmpdata$reactiontime[tmpdata$easyP1difficultN1 == 1], tmpdata$reactiontime[tmpdata$easyP1difficultN1 == -1]);

  hist(tmpdata$reactiontime[tmpdata$easyP1difficultN1 == -1],
       breaks = seq(from = 0, to = 4, by = .25), col = rgb(1,0,0,0.6), xlim = c(0,4), ylim = c(0,30),
       main = sprintf('Subject %g: t-test p = %.03f; var test p = %.03f', subj_id, diff_stat_result$p.value, var_stat_results$p.value));
  hist(tmpdata$reactiontime[tmpdata$easyP1difficultN1 == 1],
       breaks = seq(from = 0, to = 4, by = .25), col = rgb(0,0,1,0.6), add = T)
}


# test easy ACC vs. easy REJ RTs (and plot against each other)
# Q: Can we treat easy ACC & REJ RTs as the same kind of thing?
t.test(mean_rt_easyACC,mean_rt_easyREJ, paired = T); # not sig. diff between easy types 4/25/23 (t(84) = 0.28963, p = 0.7728) # thesis
mean_acc = mean(mean_rt_easyACC, na.rm = T) # 1.45068
sd_acc = sd(mean_rt_easyACC, na.rm = T) # 0.2693126
mean_rej = mean(mean_rt_easyREJ, na.rm = T) # 1.444786
sd_rej =sd(mean_rt_easyREJ, na.rm = T) # 0.2115278

# Cohen's D
Acc = length(mean_rt_easyACC)
Rej = length(mean_rt_easyREJ)

pooled_sd = sqrt(((Acc - 1) * sd_acc^2 + (Rej - 1) * sd_rej^2)/ (Acc + Rej +2))

cohen_d = (mean_acc - mean_rej) / pooled_sd
cohen_d # 0.02462664

means = c(mean_acc, mean_rej)
se = c(sd_acc, sd_rej)

barplot(means, beside = TRUE, col = c("skyblue", "salmon"),
        ylim = c(0, max(means + se) + 1), names.arg = c("Group 1", "Group 2"),
        ylab = "Mean Values", main = "Barplot of Group Means with Error Bars")

plot(mean_rt_easyACC, mean_rt_easyREJ, main = 'Reaction Times on Easy Trials',
     xlab = 'Easy ACCEPT trials', ylab = 'Easy REJECT trials', xlim = c(0,4), ylim = c(0,4))
lines(c(0,4), c(0,4))

# A: yes they are very similar & not significantly different, what we want to see. CAN collapse easy REJ and easy ACC as "easy"


## SUBSEQUENT DIFFICULTY ANALYSIS ############################################

#Q:Does RT change depending on subsequent trial types? # Something Anna wanted to examine - Underpowered?
#mean RT subsequent #
diff_diff_mean_rt = array(dim = c(number_of_clean_subjects, 1));
easy_easy_mean_rt = array(dim = c(number_of_clean_subjects, 1));
easy_diff_mean_rt = array(dim = c(number_of_clean_subjects, 1));
diff_easy_mean_rt = array(dim = c(number_of_clean_subjects, 1));

for (subj in 1:number_of_clean_subjects){
  subj_id = keep_participants[subj]
  tmpdata = data_dm[data_dm$subjectnumber == subj_id,]
  easy_easy_mean_rt[subj] = mean(tmpdata$reactiontime[(tmpdata$easyP1difficultN1[52:170] == 1) &
                                                        (tmpdata$easyP1difficultN1[51:169] == 1)], na.rm = T);

  diff_diff_mean_rt[subj] = mean(tmpdata$reactiontime[(tmpdata$easyP1difficultN1[52:170] == -1) &
                                                        (tmpdata$easyP1difficultN1[51:169] == -1)], na.rm = T);

  diff_easy_mean_rt[subj] = mean(tmpdata$reactiontime[(tmpdata$easyP1difficultN1[52:170] == 1) &
                                                        (tmpdata$easyP1difficultN1[51:169] == -1)], na.rm = T);

  easy_diff_mean_rt[subj] = mean(tmpdata$reactiontime[(tmpdata$easyP1difficultN1[52:170] == -1) &
                                                        (tmpdata$easyP1difficultN1[51:169] == 1)], na.rm = T);
}

# does prev. trial type influence RT on the current trial
t.test(easy_easy_mean_rt, diff_easy_mean_rt, paired = T); # NOT for easy 5/26/25 (t(70) = 0.38431, p = 0.7019) # SF Thesis
EE_mean = mean(easy_easy_mean_rt, na.rm = T) # 1.611737
EE_sd = sd(easy_diff_mean_rt, na.rm = T) # 0.2656962
DE_mean = mean(diff_easy_mean_rt, na.rm = T) # 1.603603
DE_sd = sd(diff_easy_mean_rt, na.rm = T) # 0.2683643

# Cohen's D
EE = length(easy_easy_mean_rt)
DE = length(easy_diff_mean_rt)

pooled_sd = sqrt(((EE - 1) * EE_sd^2 + (DE - 1) * DE_sd^2)/ (EE + DE +2))

cohen_d = (EE_mean - DE_mean) / pooled_sd
cohen_d # 0.0142

means = c(EE_mean, DE_mean)
se = c(EE_sd, DE_sd)

barplot(means, beside = TRUE, col = c("skyblue", "salmon"),
        ylim = c(0, max(means + se) + 1), names.arg = c("Group 1", "Group 2"),
        ylab = "Mean Values", main = "Barplot of Group Means with Error Bars")

t.test(diff_diff_mean_rt, easy_diff_mean_rt, paired = T); # NOT for difficult 2/26/25 (t(70) = -0.2439, p = 0.808)
DD_mean = mean(diff_diff_mean_rt, na.rm = T) # 1.610927
DD_sd = sd(diff_diff_mean_rt, na.rm = T) # 0.2721176
ED_mean = mean(easy_diff_mean_rt, na.rm = T) # 1.59511
ED_sd = sd(easy_diff_mean_rt, na.rm = T) # 0.2656962

# Cohen's D
DD = length(diff_diff_mean_rt)
ED = length(easy_diff_mean_rt)

pooled_sd = sqrt(((DD - 1) * DD_sd^2 + (ED - 1) * ED_sd^2)/ (DD + ED +2))

cohen_d = (DD_mean - ED_mean) / pooled_sd
cohen_d # -0.01109

means = c(DD_mean, ED_mean)
se = c(DD_sd, ED_sd)

barplot(means, beside = TRUE, col = c("skyblue", "salmon"),
        ylim = c(0, max(means + se) + 1), names.arg = c("Group 1", "Group 2"),
        ylab = "Mean Values", main = "Barplot of Group Means with Error Bars")

# A: No, previous trial type does not seem to influence RT on current trial at this level

# Plot the current trial as a function of prev. trial type
plot(easy_easy_mean_rt, diff_easy_mean_rt, xlim = c(0.75,2.2), ylim = c(0.75,2.2),
     main = 'EASY TRIALS', xlab = 'Previous trial was EASY', ylab = 'Previous trial was DIFFICULT'); lines(c(0,3), c(0,3)); # NOT for easy
plot(easy_diff_mean_rt, diff_diff_mean_rt, xlim = c(0.75,2.2), ylim = c(0.75,2.2),
     main = 'DIFFICULT TRIALS', xlab = 'Previous trial was EASY', ylab = 'Previous trial was DIFFICULT'); lines(c(0,3), c(0,3)); # NOT for difficult

t.test(diff_diff_mean_rt, easy_easy_mean_rt, paired = T); # not sig diff 4/5/24 (t(84) = -0.075654, p = 0.9399) # thesis
#A: it looks like RT based upon subsequent trials is not sig different at this level

# Cohen's D
pooled_sd = sqrt(((DD - 1) * DD_sd^2 + (EE - 1) * EE_sd^2)/ (DD + EE +2))

cohen_d = (DD_mean - EE_mean) / pooled_sd
cohen_d # -0.003047055

means = c(DD_mean, EE_mean)
se = c(DD_sd, EE_sd)

barplot(means, beside = TRUE, col = c("skyblue", "salmon"),
        ylim = c(0, max(means + se) + 1), names.arg = c("Group 1", "Group 2"),
        ylab = "Mean Values", main = "Barplot of Group Means with Error Bars")


#Q: Does gambling behavior change based upon previous trial difficulty?
#mean p_gamble subsequent
diff_diff_mean_pgamble = array(dim = c(number_of_clean_subjects, 1));
easy_easy_mean_pgamble = array(dim = c(number_of_clean_subjects, 1));
easy_diff_mean_pgamble = array(dim = c(number_of_clean_subjects, 1));
diff_easy_mean_pgamble = array(dim = c(number_of_clean_subjects, 1));

for (subj in 1:number_of_clean_subjects){
  subj_id = keep_participants[subj]
  tmpdata = data_dm[data_dm$subjectnumber == subj_id,]
  diff_diff_mean_pgamble[subj] = mean(tmpdata$choice[(tmpdata$easyP1difficultN1[52:170] == -1) &
                                                       (tmpdata$easyP1difficultN1[51:169] == -1)], na.rm = T);

  easy_easy_mean_pgamble[subj] = mean(tmpdata$choice[(tmpdata$easyP1difficultN1[52:170] == 1) &
                                                       (tmpdata$easyP1difficultN1[51:169] == 1)], na.rm = T);


  easy_diff_mean_pgamble[subj] = mean(tmpdata$choice[(tmpdata$easyP1difficultN1[52:170] == 1) &
                                                       (tmpdata$easyP1difficultN1[51:169] == -1)], na.rm = T);


  diff_easy_mean_pgamble[subj] = mean(tmpdata$choice[(tmpdata$easyP1difficultN1[52:170] == -1) &
                                                       (tmpdata$easyP1difficultN1[51:169] == 1)], na.rm = T);
}

t.test(diff_diff_mean_pgamble, easy_diff_mean_pgamble, paired = T); # not sig diff 2/26/25 (t(70) = -1.2398, p = 0.2192)
t.test(diff_easy_mean_pgamble, easy_easy_mean_pgamble, paired = T); # not sig diff 2/26/25 (t(70) = -0.4075, p = 0.6849)
t.test(diff_diff_mean_pgamble, easy_easy_mean_pgamble, paired = T); # not sig diff 2/26/25 (t(70) = -0.6797, p = 0.4989)

#A: it looks like pgamble based upon subsequent trials is not significantly different, difficulty doesn't show effect on p gamble.
# - may be due to not having intermediate pgambles?


### WORKING MEMORY BASIC ANALYSIS ###

cat(sprintf('Out of a total of %i participants, we have O-Span scores for %i, Sym-Span scores for %i, and composite span scores for %i.\n',
            number_of_subjects,
            sum(is.finite(complexSpanScores$ospanScore)),
            sum(is.finite(complexSpanScores$symspanScore)),
            sum(is.finite(complexSpanScores$compositeSpanScore))))
# Out of a total of 73 participants, we have O-Span scores for 51, Sym-Span scores for 50, and composite span scores for 67.
cat(sprintf('%i participants have both scores, %i participants are missing only one score, and %i participants are missing both scores.\n',
            sum(is.finite(complexSpanScores$ospanScore) & is.finite(complexSpanScores$symspanScore)),
            sum(xor(is.finite(complexSpanScores$ospanScore),is.finite(complexSpanScores$symspanScore))),
            number_of_subjects-sum(is.finite(complexSpanScores$compositeSpanScore))))
# 34 participants have both scores, 33 participants are missing only one score, and 6 participants are missing both scores.
# Mean, Median, and Variance of ospan, symspan, and compositespan
mean_ospan = mean(complexSpanScores$ospanScore, na.rm = T)
mean_symspan = mean(complexSpanScores$symspanScore, na.rm = T)
mean_compositespan = mean(complexSpanScores$compositeSpanScore, na.rm = T)

sd_ospan = sd(complexSpanScores$ospanScore, na.rm = T)
sd_symspan = sd(complexSpanScores$symspanScore, na.rm = T)
sd_compositespan = sd(complexSpanScores$compositeSpanScore, na.rm = T)

median_ospan = median(complexSpanScores$ospanScore, na.rm = T)
median_symspan = median(complexSpanScores$symspanScore, na.rm = T)
median_compositespan = median(complexSpanScores$compositeSpanScore, na.rm = T)

variance_ospan = var(complexSpanScores$ospanScore, na.rm = T)
variance_symspan = var(complexSpanScores$symspanScore, na.rm = T)
variance_compositespan = var(complexSpanScores$compositeSpanScore, na.rm = T)

cat("Mean ospan:", mean_ospan, "\n")
cat("Mean symspan:", mean_symspan, "\n")
cat("Mean compositeSpan:", mean_compositespan, "\n")

cat("Median ospan:", median_ospan, "\n")
cat("Median symspan:", median_symspan, "\n")
cat("Median compositeSpan:", median_compositespan, "\n")

cat("Variance ospan:", variance_ospan, "\n")
cat("Variance symspan:", variance_symspan, "\n")
cat("Variance compositeSpan:", variance_compositespan, "\n")

max_compositespan = max(complexSpanScores$compositeSpanScore, na.rm = T)
cat("Max composite span score:", max_compositespan, "\n")

# Include in the processing - Correlation between OSpan and SymSpan
ospanScores = complexSpanScores$ospanScore
symspanScores = complexSpanScores$symspanScore
compositeSpanScores = complexSpanScores$compositeSpanScore
# clean_data_dm$wmc_cont = compositeSpanScores[keep_participants]
# SEE: clean_data_dm$complexspan or clean_data_dm$complexspan_demeaned

cor.test(ospanScores, symspanScores) # r(32) = 0.19877 , p = 0.2597 (as of 2/26/25)
var.test(ospanScores, symspanScores) # similar variance (F(50) = 0.90125, p = 0.7152 as of 2/26/25)
t.test(ospanScores, symspanScores, paired = T) # t(33) = -0.5948, p = 0.556 (as of 2/26/25)

# SUMMARY: O-Span & Sym-Span scores have a weak positive correlation, and a similar variance. 
# different from one another. They are NOT redundant.

plot(ospanScores, symspanScores,
     pch = 19, col = rgb(.5, .5, .5, .5),
     xlim = c(0, 1), ylim = c(0, 1), cex = 2.5,
     xlab = 'OSpan Scores', ylab = 'SymSpan Scores',
     main = 'Complex Span Scores')
lines(x = c(-1, 2), y = c(-1, 2)) # so line extends to edge

capacity_HighP1_lowN1 = (compositeSpanScores > median_compositespan)*2-1;

sum(capacity_HighP1_lowN1 == 1, na.rm = T) # 32
sum(capacity_HighP1_lowN1 == -1, na.rm = T) # 35

# Plot the distribution w/ the median value
hist(compositeSpanScores, breaks = 10, xlab = 'Composite Span Score', main = 'Distribution of Spans');
abline(v = median_compositespan, col = 'red', lwd = 5)


clean_data_dm$capacity_HighP1_lowN1 = NA;

for(s in 1:number_of_clean_subjects){
  subj_id = keep_participants[s];
  clean_data_dm$capacity_HighP1_lowN1[clean_data_dm$subjectnumber == subj_id] = capacity_HighP1_lowN1[s];
}

clean_data_dm$complexspan_demeaned = clean_data_dm$complexspan - mean_compositespan;

plot(sort(compositeSpanScores))
abline(h = median_compositespan, col = 'red', lwd = 2)

# Easy vs. Difficult in WMC - From thesis

rt_mean__Hcap_test = t.test(mean_rt_easy[capacity_HighP1_lowN1 == 1], mean_rt_diff[capacity_HighP1_lowN1 == 1], paired = T) #
rt_mean__Hcap_test
rt_median_Hcap_test = t.test(median_rt_easy[capacity_HighP1_lowN1 == 1],median_rt_diff[capacity_HighP1_lowN1 == 1], paired = T) #
rt_median_Hcap_test

rt_mean__Lcap_test = t.test(mean_rt_easy[capacity_HighP1_lowN1 == 1], mean_rt_diff[capacity_HighP1_lowN1 == 1], paired = T) #
rt_mean__Lcap_test
rt_median_Lcap_test = t.test(median_rt_easy[capacity_HighP1_lowN1 == 1],median_rt_diff[capacity_HighP1_lowN1 == 1], paired = T) #
rt_median_Lcap_test

# - modified from cgt
meanRT_capacity_High <- numeric(number_of_clean_subjects)
meanRT_capacity_Low <- numeric(number_of_clean_subjects)
meanRT_diff_capacity_High <- numeric(number_of_clean_subjects)
meanRT_diff_capacity_Low <- numeric(number_of_clean_subjects)
meanRT_easy_capacity_High <- numeric(number_of_clean_subjects)
meanRT_easy_capacity_Low <- numeric(number_of_clean_subjects)

for (subj in 1:number_of_clean_subjects) {
  subj_id <- keep_participants[subj]
  tmpdata <- clean_data_dm[clean_data_dm$subjectnumber == subj_id, ]
  meanRT_capacity_High[subj] <- mean(tmpdata$reactiontime[tmpdata$capacity_HighP1_lowN1 == 1], na.rm = TRUE)
  meanRT_capacity_Low[subj] <- mean(tmpdata$reactiontime[tmpdata$capacity_HighP1_lowN1 == -1], na.rm = TRUE)
  meanRT_diff_capacity_High[subj] <- mean(tmpdata$reactiontime[(tmpdata$capacity_HighP1_lowN1 == 1) & (tmpdata$easyP1difficultN1 == -1)], na.rm = TRUE)
  meanRT_easy_capacity_High[subj] <- mean(tmpdata$reactiontime[(tmpdata$capacity_HighP1_lowN1 == 1) & (tmpdata$easyP1difficultN1 == 1)], na.rm = TRUE)
  meanRT_diff_capacity_Low[subj] <- mean(tmpdata$reactiontime[(tmpdata$capacity_HighP1_lowN1 == -1) & (tmpdata$easyP1difficultN1 == -1)], na.rm = TRUE)
  meanRT_easy_capacity_Low[subj] <- mean(tmpdata$reactiontime[(tmpdata$capacity_HighP1_lowN1 == -1) & (tmpdata$easyP1difficultN1 == 1)], na.rm = TRUE)
}

mean(meanRT_capacity_Low, na.rm = T) # 1.631571
sd(meanRT_capacity_Low, na.rm = T) # 0.2828634

mean(meanRT_capacity_High, na.rm = T) # 1.599115
sd(meanRT_capacity_High, na.rm = T) # 0.240628

t.test(meanRT_capacity_High, meanRT_capacity_Low, na.rm = T) # t = -0.5596, df = 77.995, p-value = 0.5774 # thesis

mean_HE = mean((meanRT_easy_capacity_High), na.rm = T); # 1.445861
sd_HE = sd((meanRT_easy_capacity_High), na.rm = T); # 0.1942527
mean_HD = mean((meanRT_diff_capacity_High), na.rm = T); # 1.703286
sd_HD = sd((meanRT_diff_capacity_High), na.rm = T); # 0.3666631

mean_LE = mean((meanRT_easy_capacity_Low), na.rm = T); # 1.465639
sd_LE = sd((meanRT_easy_capacity_Low), na.rm = T); # 0.2483976
mean_LD = mean((meanRT_diff_capacity_Low), na.rm = T); # 1.690224
sd_LD =  sd((meanRT_diff_capacity_Low), na.rm = T); # 0.3242537


t.test(meanRT_easy_capacity_High, meanRT_diff_capacity_High, paired = T) # t = -6.6327, df = 40, p-value = 6.109e-08 # thesis

# Cohen's D
HE = length(meanRT_easy_capacity_High)
HD = length(meanRT_diff_capacity_High)

pooled_sd = sqrt(((HD - 1) * sd_HD^2 + (HE - 1) * sd_HE^2)/ (HD + HE +2))

cohen_d = (mean_HD - mean_HE) / pooled_sd
cohen_d # 0.8877452

t.test(meanRT_easy_capacity_Low, meanRT_diff_capacity_Low, paired = T) # t = -8.0487, df = 40, p-value = 6.791e-10

# Cohen's D
LE = length(meanRT_easy_capacity_Low)
LD = length(meanRT_diff_capacity_Low)

pooled_sd = sqrt(((LD - 1) * sd_LD^2 + (LE - 1) * sd_LE^2)/ (LD + LE +2))

cohen_d = (mean_LD - mean_LE) / pooled_sd
cohen_d # 0.7867809

se_HE = sd_HE/sqrt(length(meanRT_easy_capacity_High))
se_HD = sd_HD/sqrt(length(meanRT_diff_capacity_High))
se_LE = sd_LE/sqrt(length(meanRT_easy_capacity_Low))
se_LD = sd_LD/sqrt(length(meanRT_diff_capacity_Low))
means = c(mean_LE, mean_HE, mean_LD, mean_HD)
ses = c(se_LE, se_HE, se_LD, se_HD)

means_matrix = matrix(means, nrow=2, byrow=TRUE)
rownames(means_matrix) = c("Easy", "Difficult")
colnames(means_matrix) = c("Low WMC", "High WMC")

# Bar plot with error bars
WMCgraph = barplot(means_matrix, beside = TRUE, col = c("blue", "red"),
                   ylim = c(0, 2),
                   names.arg = colnames(means_matrix),
                   ylab = "Average RTs in Seconds",
                   main = "Choice Difficulty x WMC Group",
                   legend.text = rownames(means_matrix),
                   args.legend = list(x = "topright"))

arrows(WMCgraph, means_matrix,
       WMCgraph, means_matrix + ses,
       angle = 90, code = 3, length = 0.1)
box(bty="l")


# Defining Heartbeat Detection Task Categories
# Tone Task grouping
# Define based on whether their individual performance was significantly 
# (p = 0.05) better than chance or not. Better than chance = +1, not better than
# chance = -1.
thresh_pval = 0.05
clean_data_dm$interocept_sigP1_nsN1 = as.numeric(clean_data_dm$pbetterthanchance < thresh_pval) - 
  as.numeric(clean_data_dm$pbetterthanchance > thresh_pval)
sum(clean_data_dm$interocept_sigP1_nsN1 == 1, na.rm = T)/170 # 26 good interoceptors
sum(clean_data_dm$interocept_sigP1_nsN1 == -1, na.rm = T)/170 # 41 poor interoceptors

# In regressions, we can use...
# - pcorrect
# - dprime (might be better behaved)
# - interocept_sigP1_nsN1
#
# Can also do similar things for...
# - counting task performance
# - median split on tone task
# - metacognitive performance (median split?)

colnames (survey_data)
#summary statistics for Dprime
mean_value <- mean(survey_data$dprime, na.rm = TRUE) #Mean: 0.5109743
median_value <- median(survey_data$dprime, na.rm = TRUE) #Median: 0.2290681
sd_value <- sd(survey_data$dprime, na.rm = TRUE) #Standard Deviation: 0.8536081 
range_values <- range(survey_data$dprime, na.rm = TRUE)
range_value <- diff(range_values)

#summary stats for pcorrectbyhalf_1 and pcorrectbyhalf_2
mean_value <- mean(survey_data$pcorrectbyhalf_1, na.rm = TRUE)
mean_value <- mean(survey_data$pcorrectbyhalf_2, na.rm = TRUE)

#t-test to examine if there is a significant difference between pcorrect in the 1st v 2nd half of the Tone Task
t.test(survey_data$pcorrectbyhalf_1, survey_data$pcorrectbyhalf_2, 
       paired = TRUE, na.rm = TRUE) 
# t = -1.6123, df = 68, p-value = 0.1115. There is not a significant difference in performance between the halves. slight trend toward ppl being less accurate in half 2
cor.test(survey_data$pcorrectbyhalf_1, survey_data$pcorrectbyhalf_2)
cor.test(survey_data$pcorrectbyhalf_1, survey_data$pcorrectbyhalf_2, method = 'spearman')
# Highly correlated by either method

#summary stats for confWrong and confRight
mean_value <- mean(survey_data$confWrong, na.rm = TRUE) #mean confidence when wrong = 0.518
mean_value <- mean(survey_data$confRight, na.rm = TRUE) #mean confidence when right = 0.573
t.test(survey_data$confWrong, survey_data$confRight, 
       paired = TRUE, na.rm = TRUE) # t = -5.6531, df = 68, p-value = 3.396e-07
#participants were significantly less confident in their responses when they were wrong vs when they were right

#summary stats for metacognition dprime Bayesian, a measure of metacognition adjusted for ability/accuracy in the task.
mean_value <- mean(survey_data$metadprimeBayesian, na.rm = TRUE) #Mean: 0.354
median_value <- median(survey_data$metadprimeBayesian, na.rm = TRUE) #Median: 0.159
sd_value <- sd(survey_data$metadprimeBayesian, na.rm = TRUE) #Standard Deviation: 0.532

# examining correlations between HB Tone Task Variables
cor.test(survey_data$dprime, survey_data$metadprimeBayesian) #cor between dprime and metacognition dprime
cor.test(survey_data$dprime, survey_data$MRatioBayesian) # no cor between dprime and M-Ratio; separate measures!





# RT on trials regressions
library(lme4)
library(lmerTest)

clean_data_dm$sqrtRT = sqrt(clean_data_dm$reactiontime);
clean_data_dm$easy = as.double(clean_data_dm$easyP1difficultN1 == 1)
clean_data_dm$difficult = as.double(clean_data_dm$easyP1difficultN1 == -1)
# input shifted version of desired content
clean_data_dm$easyP1difficultN1_prev = c(0,clean_data_dm$easyP1difficultN1[1:(length(clean_data_dm$easyP1difficultN1)-1)])
# fix the few problematic trials (#1)
clean_data_dm$easyP1difficultN1_prev[clean_data_dm$trialnumber == 1] = 0;
# input shifted version of desired content
clean_data_dm$sqrtRT_prev = c(NA,clean_data_dm$sqrtRT[1:(length(clean_data_dm$sqrtRT)-1)])
# fix the few problematic trials (#1)
clean_data_dm$sqrtRT_prev[clean_data_dm$trialnumber == 1] = NA;


## RT and Interoception Regressions ############################################

# RT regression with good/bad interoceptor conditions
m0_intero = lmer(sqrtRT ~ 1 + interocept_sigP1_nsN1 + (1 | subjectnumber), data = clean_data_dm)
summary(m0_intero)
# No significant net effect of being a good interoceptor on RT

#linear regression to test dprime values and RTs
m0_dprime = lmer(sqrtRT ~ 1 + dprime + (1 | subjectnumber), data = clean_data_dm)
summary(m0_dprime) #dprime had no meaningful effect on RTs
# Similarly for d-prime (as expected), no direct relationship between interoceptive ability & RT

#linear regression of percent correct on RTs
m0_pcorrect = lmer(sqrtRT ~ 1 + pcorrect + (1 | subjectnumber), data = clean_data_dm)
summary(m0_pcorrect)
# same thing, no significant effect of pcorrect

# TAKEAWAY: Interoceptive ability has no simple & direct effect on decision speed.

# 
m1_currdiff_interoceptive_grp = lmer(sqrtRT ~ 1 + all_diff_cont * interocept_sigP1_nsN1 + (1 | subjectnumber), data = clean_data_dm)
summary(m1_currdiff_interoceptive_grp)
# Strong effect of interoception interacting with current difficulty. 
# --> Good interoceptors are much slower on difficult trials (vs. easy)
# Given betas, good interoceptors have steeper relationship between current 
# difficulty & RT (1.15 -> 1.29, sqrtrt) than bad interoceptors (1.20 -> 1.28)
#
# Is most of the action on EASY trials? 

m1_currdiff_interoceptive_dp = lmer(sqrtRT ~ 1 + all_diff_cont * dprime + (1 | subjectnumber), data = clean_data_dm)
summary(m1_currdiff_interoceptive_dp)
# Same findings

m1_easydiff_interoceptive_dp = lmer(sqrtRT ~ 1 + easyP1difficultN1 * dprime + (1 | subjectnumber), data = clean_data_dm)
summary(m1_easydiff_interoceptive_dp)
# Same findings w/ categorical difficulty

# TAKEAWAY: Better interoceptors seem to be more influenced by current trial
# difficulty 

#repeating these regressions with pcorrect instead of dprime 
#Continuous Difficulty × pcorrect
m1_currdiff_interoceptive_pc = lmer(sqrtRT ~ 1 + all_diff_cont * pcorrect + (1 | subjectnumber), data = clean_data_dm)
summary(m1_currdiff_interoceptive_pc)
#effect of trial difficulty on RT stronger for more accurate participants (higher pcorrect), p < 2e-16

#Categorical Difficulty × pcorrect
m1_easydiff_interoceptive_pc = lmer(sqrtRT ~ 1 + easyP1difficultN1 * pcorrect + (1 | subjectnumber), data = clean_data_dm)
summary(m1_easydiff_interoceptive_pc)
#same result

m1_easydiffsep_interoceptive_grp = lmer(sqrtRT ~ 1 + easy * interocept_sigP1_nsN1 + 
                                      difficult * interocept_sigP1_nsN1 + 
                                      (1 | subjectnumber), data = clean_data_dm)
summary(m1_easydiffsep_interoceptive_grp)
# While above regressions suggest that easy trials might be different (and not
# difficult)...
# this actually suggests the opposite - difficult is slower for good interoceptors
# than bad interoceptors (significantly; p = 4.7e-14) but there's no difference
# on easy trials (p = 0.47). 


# regression bringing in previous difficulty, continuous
m1_pdiff_curdiff_interoceptiveGrp = lmer(sqrtRT ~ 1 + 
                                        all_diff_cont * interocept_sigP1_nsN1 + 
                                        prev_all_diff_cont * interocept_sigP1_nsN1 +
                                        (1 | subjectnumber), 
                                      data = clean_data_dm)

# A*B = A + B + A:B
# A*B*C = A + B + C + A:B + A:C + B:C + A:B:C
# A*B + A*C = A + B + A:B + C + A:C

summary(m1_pdiff_curdiff_interoceptiveGrp)

# Main Effects
# current difficulty slowing RT, as seen prior. p < 2e-16
# interoceptive ability is significant, p = 0.02 (good interoceptors are faster)
# -no effect- of previous difficulty

# 2-way Interactions
# current difficulty and interoception p = 4.8e-15 -> good interoceptors have stronger curr. diff. effect
# previous difficulty and interoception, p = 3e-4 -> good interoceptors have weaker prev. diff. effect

#TAKEAWAYS
# good interoceptors are...
#   - faster overall.
#   - more affected by current difficulty
#   - less affected by previous difficulty


# Are these effects of difficulty significant within-group?
m1_pdiff_curdiff_interocep_GOODONLY = lmer(sqrtRT ~ 1 + all_diff_cont + prev_all_diff_cont +
                                        (1 | subjectnumber), 
                                      data = clean_data_dm[clean_data_dm$interocept_sigP1_nsN1 == 1,])
m1_pdiff_curdiff_interocep_BADONLY = lmer(sqrtRT ~ 1 + all_diff_cont + prev_all_diff_cont +
                                             (1 | subjectnumber), 
                                           data = clean_data_dm[clean_data_dm$interocept_sigP1_nsN1 == -1,])
summary(m1_pdiff_curdiff_interocep_GOODONLY)
summary(m1_pdiff_curdiff_interocep_BADONLY)

# Good interoceptors: current difficulty (prev. is not sig)
# Bad interoceptors: current difficult and prev. difficulty

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GOOD INTEROCEPTORS LOOK MOSTLY LIKE HIGH WMC PARTICIPANTS, CONTRARY TO HYPOTHESES ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Is interoception collinear with working memory capacity?
cor.test(clean_data_survey$dprime,clean_data_complexspan$compositeSpanScore)
cor.test(clean_data_survey$dprime,clean_data_complexspan$compositeSpanScore, method = 'spearman')

cor.test(clean_data_survey$pcorrect,clean_data_complexspan$compositeSpanScore)
cor.test(clean_data_survey$pcorrect,clean_data_complexspan$compositeSpanScore, method = 'spearman')

plot(sqrt(clean_data_survey$dprime),clean_data_complexspan$compositeSpanScore,
     xlab = 'Sqrt(D-prime)', ylab = 'Composite Span Score', col = rgb(0,0,0,.7), pch = 16, cex = 3)

# YES. Correlation is around 0.33-ish, and significant with p(correct) or dprime, spearman or pearson
# Good interoceptors have higher working memory capacity.

# If you calculate binary good/bad interoceptors and compare that to a median split of WMC
# you'll also get more matches than would be expected by chance (40/63). 


# Running complementary models w/ diff. specifications of interoceptive ability
m1_pdiff_curdiff_interoceptiveDprime = lmer(sqrtRT ~ 1 + 
                                        all_diff_cont * sqrtdprime + 
                                        prev_all_diff_cont * sqrtdprime +
                                        (1 | subjectnumber), 
                                      data = clean_data_dm)

m1_pdiff_curdiff_interoceptivePcorrect = lmer(sqrtRT ~ 1 + 
                                              all_diff_cont * pcorrect + 
                                              prev_all_diff_cont * pcorrect +
                                              (1 | subjectnumber), 
                                            data = clean_data_dm)
summary(m1_pdiff_curdiff_interoceptiveDprime)
summary(m1_pdiff_curdiff_interoceptivePcorrect)
# These find largely identical patterns; because p(correct) values range from 0.5 to 1,
# there is an offset effect with the all_diff_cont:pcorrect interaction term. The pcorrect
# regression implies a negative effect of current difficulty, but that's not how it works
# out once you add in the interaction term & its offset. 




#regression bringing in previous difficulty one trial back, categorical
m2_pdiff_curdiff_interoceptive <- lmer(sqrtRT ~ 1 +  
                                         all_diff_cont * interocept_sigP1_nsN1 * easyP1difficultN1_prev +  
                                         (1 | subjectnumber),  
                                       data = clean_data_dm)  

summary(m2_pdiff_curdiff_interoceptive)
#in this model, no significant interaction between interoception and prev difficulty


## Model 0: Current RT based on current easy difficult
m0_diffcat = lm(sqrtRT ~ 1 + easyP1difficultN1, data = clean_data_dm); # LM
summary(m0_diffcat) # difficulty predicts RT!

m0_diffcat_rfx = lmer(sqrtRT ~ 1 + easyP1difficultN1 + (1 | subjectnumber), data = clean_data_dm); # LMER
summary(m0_diffcat_rfx) # Same with RFX!

m0_diffcat_dynonly_rfx = lmer(sqrtRT ~ 1 + easyP1difficultN1 + (1 | subjectnumber),
                              data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]);
summary(m0_diffcat_dynonly_rfx) # Same with only dynamic trials

# Takeaway: In all cases, difficult is slower than easy! Use: m0_diffcat_rfx

# use continuous diff metric instead of easy/difficult
m0_diffcont = lm(sqrtRT ~ 1 + diff_cont , data = clean_data_dm);
summary(m0_diffcont) # matches categorical

m0_diffcont_rfx = lmer(sqrtRT ~ 1 + diff_cont + (1 | subjectnumber), data = clean_data_dm);
summary(m0_diffcont_rfx) # matches categorical

m0_diffcont_dynonly_rfx = lmer(sqrtRT ~ 1 + diff_cont + (1 | subjectnumber),
                               data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]);
summary(m0_diffcont_dynonly_rfx) # matches categorical

# TAKEAWAY: Nothing new here, matches categorical, as expected (diff_cont is practically categorical!)

m0_alldiffcont = lm(sqrtRT ~ 1 + all_diff_cont, data = clean_data_dm);
summary(m0_alldiffcont) # matches categorical

m0_alldiffcont_rfx = lmer(sqrtRT ~ 1 + all_diff_cont + (1 | subjectnumber), data = clean_data_dm);
summary(m0_alldiffcont_rfx) # matches categorical

# IN ALL CASES, PEOPLE ARE SLOWER ON DIFFICULT TRIALS THAN EASY TRIALS.
# UNAFFECTED BY CATEGORICAL/CONTINUOUS
# UNAFFECTED BY DYNAMIC ONLY VS. ALL TRIALS

# Which model should we use?
# It's between m0_diffcat_rfx and m0_alldiffcont_rfx
AIC(m0_diffcat_rfx) # -8603.169
AIC(m0_alldiffcont_rfx) # -8783.393 <- BETTER (more negative)

anova(m0_diffcat_rfx,m0_alldiffcont_rfx) # CONFIRMS that all_diff_cont outperforms easyp1difficultn1
# p < 2e-16 (it's reported as '0') for continuous as better than categorical ???

# xval_plot = seq(from = 0, to = 1, by = .1);
#
# predict_data_m0 = clean_data_dm[0,];
# predict_data_m0[1:20,] = NA;
# predict_data_m0$all_diff_cont[1:10] = xval_plot
# # predict_data_m0$all_diff_cont[11:20] = xval_plot
#
# predict_output_m0 = predict(m0_alldiffcont_rfx, newdata = predict_data_m0, type = 'response', re.form = NA)^2
#
# plot(x = xval_plot, y = predict_output_m0[1:20],
#      type = 'l', lwd = 5, col = 'purple',
#      main = 'Effect of current difficulty', xlab = 'Difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)')


# Plot the simple main effect of difficulty
xval_plot = seq(from = 0, to = 1, by = .1);
coef_vals = fixef(m0_alldiffcont_rfx)

plot(x = xval_plot, y = (coef_vals["(Intercept)"] + xval_plot*coef_vals["all_diff_cont"])^2,
     type = 'l', lwd = 5, col = 'purple',
     main = 'Effect of current difficulty', xlab = 'Difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)')

# # Alternative Approach using data points and abline function
# plot(x = clean_data_dm$all_diff_cont, y = clean_data_dm$sqrtRT)
# abline(reg = m0_alldiffcont, col = 'red') # regression must be an LM or GLM, not LMER or GLMER



# BIG TAKEAWAY:
# Across categorical and two kinds of continuous difficulty, difficult trials are slower.
#
# m0_alldiffcont_rfx is best (see AICs)




## Model 1: PREVIOUS DIFFICULTY: Create Shifted versions of difficulty for use in regressions


# Does previous difficulty influence subsequent RT?
# LMs
m1_diffcat_prev = lm(sqrtRT ~ 1 + easyP1difficultN1 + easyP1difficultN1_prev, data = clean_data_dm);
summary(m1_diffcat_prev) # slight effect, OPPOSITE to main pattern (p = 0.0466 as of 2/25/24)
#  (if prev. trial easy, slower on current trial .... OR
#   if prev. trial difficult, faster on current trial)

m1_diffcat_prev_intxn = lm(sqrtRT ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev, data = clean_data_dm);
summary(m1_diffcat_prev_intxn) # no interaction, same main effect as m1_diffcat_prev

# LMERs, i.e. RFX Versions
m1_diffcat_prev_rfx = lmer(sqrtRT ~ 1 + easyP1difficultN1 + easyP1difficultN1_prev +
                             (1 | subjectnumber), data = clean_data_dm);
summary(m1_diffcat_prev_rfx) # previous difficulty has strong effect (p = 0.02)
# Same direction as in m1_diffcat_prev

m1_diffcat_prev_intxn_rfx = lmer(sqrtRT ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev + (1 | subjectnumber), data = clean_data_dm);
summary(m1_diffcat_prev_intxn_rfx) # no interaction, same main effect.

# TAKEAWAY: Previous categorical difficulty has OPPOSITE effect on current RTs as current difficulty.
#    THIS IS DIFFERENT FROM CGT! CGT HAD NO MAIN EFFECT OF PREV. CATEGORICAL DIFFICULTY.

m1_prev_alldiffCont_intxn_rfx = lmer(sqrtRT ~ 1 +
                                       all_diff_cont * prev_all_diff_cont +
                                       (1 | subjectnumber), data = clean_data_dm);
summary(m1_prev_alldiffCont_intxn_rfx) # Previous CONTINUOUS difficulty is VERY significant (p = 1.2e-7), no interaction w/ current diff
# Sign is negative: the more difficult the prev. trial was, the faster people were on the current trial
# ... facilitory? "giving up"?

# CAREFUL! These are different models with different #s of datapoints in them.
# !!!!!    CANNOT DIRECTLY COMPARE AICs     !!!!!
# AIC(m1_prev_alldiffCont_intxn_rfx) # -6376.622 <-- BETTER (more negative)
# AIC(m1_diffcat_prev_intxn_rfx) # -6208.394


xval_plot = seq(from = 0, to = 1, length.out = 10)

predict_data_m1 = clean_data_dm[0,];
predict_data_m1[1:20,] = NA;
predict_data_m1$all_diff_cont[1:10] = xval_plot
predict_data_m1$all_diff_cont[11:20] = xval_plot
predict_data_m1$prev_all_diff_cont[1:10] = 0;
predict_data_m1$prev_all_diff_cont[11:20] = 1;

predict_output_m1 = predict(m1_prev_alldiffCont_intxn_rfx, newdata = predict_data_m1, type = 'response', re.form = NA)^2

# Plot it!
plot(x = xval_plot, y = predict_output_m1[1:10],
     type = 'l', lwd = 5, col = 'blue',
     main = 'Effect of current & previous difficulty', xlab = 'Current difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)',
     ylim = c(1.25, 2))
lines(x = xval_plot, y = predict_output_m1[11:20],
      lwd = 5, col = 'red')

# BLUE = previous trial easy
# RED = previous trial difficult

# How far back does the previous difficulty effect go? Let's look **4 trials back**
m1_prev_alldiffCont_back4_intxn_rfx = lmer(sqrtRT ~ 1 +
                                             all_diff_cont + prev_all_diff_cont + prev2_all_diff_cont + prev3_all_diff_cont + prev4_all_diff_cont +
                                             (1 | subjectnumber), data = clean_data_dm);
summary(m1_prev_alldiffCont_back4_intxn_rfx)
# All 4 are significant.

m1_prev_alldiffCont_back8_intxn_rfx = lmer(sqrtRT ~ 1 +
                                             all_diff_cont + prev_all_diff_cont + prev2_all_diff_cont + prev3_all_diff_cont + prev4_all_diff_cont +
                                             prev5_all_diff_cont + prev6_all_diff_cont + prev7_all_diff_cont + prev8_all_diff_cont +
                                             (1 | subjectnumber), data = clean_data_dm);
summary(m1_prev_alldiffCont_back8_intxn_rfx)
# Looks like it peters out ~7 trials back. LONG LASTING EFFECTS!

# Trying out each difficult trial back

only_finite_index =
  is.finite(
    clean_data_dm$all_diff_cont +
      clean_data_dm$prev_all_diff_cont +
      clean_data_dm$prev2_all_diff_cont +
      clean_data_dm$prev3_all_diff_cont +
      clean_data_dm$prev4_all_diff_cont +
      clean_data_dm$prev5_all_diff_cont +
      clean_data_dm$prev6_all_diff_cont +
      clean_data_dm$prev7_all_diff_cont +
      clean_data_dm$prev8_all_diff_cont)


# -1 trial back
m1_prev_alldiffCont_back1_intxn_rfx = lmer(sqrtRT ~ 1 +
                                             all_diff_cont + prev_all_diff_cont +
                                             (1 | subjectnumber), data = clean_data_dm[only_finite_index,]);
summary(m1_prev_alldiffCont_back1_intxn_rfx)

# -2 trials back
m1_prev_alldiffCont_back2_intxn_rfx = lmer(sqrtRT ~ 1 +
                                             all_diff_cont + prev_all_diff_cont + prev2_all_diff_cont +
                                             (1 | subjectnumber), data = clean_data_dm[only_finite_index,]);
summary(m1_prev_alldiffCont_back2_intxn_rfx)

# -3 trials back
m1_prev_alldiffCont_back3_intxn_rfx = lmer(sqrtRT ~ 1 +
                                             all_diff_cont + prev_all_diff_cont + prev2_all_diff_cont + prev3_all_diff_cont +
                                             (1 | subjectnumber), data = clean_data_dm[only_finite_index,]);
summary(m1_prev_alldiffCont_back3_intxn_rfx)

# -4 trials back
m1_prev_alldiffCont_back4_intxn_rfx = lmer(sqrtRT ~ 1 +
                                             all_diff_cont + prev_all_diff_cont + prev2_all_diff_cont + prev3_all_diff_cont + prev4_all_diff_cont +
                                             (1 | subjectnumber), data = clean_data_dm[only_finite_index,]);
summary(m1_prev_alldiffCont_back4_intxn_rfx)

# -5 trials back
m1_prev_alldiffCont_back5_intxn_rfx = lmer(sqrtRT ~ 1 +
                                             all_diff_cont + prev_all_diff_cont + prev2_all_diff_cont + prev3_all_diff_cont + prev4_all_diff_cont +
                                             prev5_all_diff_cont +
                                             (1 | subjectnumber), data = clean_data_dm[only_finite_index,]);
summary(m1_prev_alldiffCont_back5_intxn_rfx)

# -6 trials back
m1_prev_alldiffCont_back6_intxn_rfx = lmer(sqrtRT ~ 1 +
                                             all_diff_cont + prev_all_diff_cont + prev2_all_diff_cont + prev3_all_diff_cont + prev4_all_diff_cont +
                                             prev5_all_diff_cont + prev6_all_diff_cont +
                                             (1 | subjectnumber), data = clean_data_dm[only_finite_index,]);
summary(m1_prev_alldiffCont_back6_intxn_rfx)

# -7 trials back
m1_prev_alldiffCont_back7_intxn_rfx = lmer(sqrtRT ~ 1 +
                                             all_diff_cont + prev_all_diff_cont + prev2_all_diff_cont + prev3_all_diff_cont + prev4_all_diff_cont +
                                             prev5_all_diff_cont + prev6_all_diff_cont + prev7_all_diff_cont +
                                             (1 | subjectnumber), data = clean_data_dm[only_finite_index,]);
summary(m1_prev_alldiffCont_back7_intxn_rfx)

# -8 trials back
m1_prev_alldiffCont_back8_intxn_rfx = lmer(sqrtRT ~ 1 +
                                             all_diff_cont + prev_all_diff_cont + prev2_all_diff_cont + prev3_all_diff_cont + prev4_all_diff_cont +
                                             prev5_all_diff_cont + prev6_all_diff_cont + prev7_all_diff_cont + prev8_all_diff_cont +
                                             (1 | subjectnumber), data = clean_data_dm[only_finite_index,]);
summary(m1_prev_alldiffCont_back8_intxn_rfx)

back1 = AIC(m1_prev_alldiffCont_back1_intxn_rfx)
back2 = AIC(m1_prev_alldiffCont_back2_intxn_rfx)
back3 = AIC(m1_prev_alldiffCont_back3_intxn_rfx)
back4 = AIC(m1_prev_alldiffCont_back4_intxn_rfx)
back5 = AIC(m1_prev_alldiffCont_back5_intxn_rfx)
back6 = AIC(m1_prev_alldiffCont_back6_intxn_rfx)
back7 = AIC(m1_prev_alldiffCont_back7_intxn_rfx)
back8 = AIC(m1_prev_alldiffCont_back8_intxn_rfx)

diff_trajectory = clean_data_dm$prev_all_diff_cont*-2.5593e-2 + clean_data_dm$prev2_all_diff_cont*-1.323-2 + clean_data_dm$prev3_all_diff_cont*1.499e-2
plot(diff_trajectory[1:170], type = 'l')


# Define the AIC values
trialBack_AIC = c(back1, back2, back3, back4, back5, back6, back7, back8)

# Define the Previous Trial Difficulty labels
trials = c('1 trial', '2 trials', '3 trials', '4 trials', '5 trials', '6 trials', '7 trials', '8 trials')


# Determine the range for zooming in
max_aic = max(trialBack_AIC)
min_aic = min(trialBack_AIC)

# Create the bar plot
barplot(trialBack_AIC, names.arg = trials,
        col = rgb(0,0,1,.5), xlab = 'Previous Trial Difficulty', ylab = 'AIC',
        main = 'AIC Values for Multiple Previous Trial Difficulty',
        ylim = c(-8850, -9070),
        border = 'black', xpd = FALSE, las = 2)
box(bty="l")

# Create the bar plot
barplot(trialBack_AIC, names.arg = trials,
        col = rgb(0,0,1,.5), xlab = 'Previous Trial Difficulty', ylab = 'AIC',
        main = 'AIC Values for Multiple Previous Trial Difficulty',
        ylim = c(-8595, -8610),
        border = 'black', xpd = FALSE)
box(bty="l")


## Model 2: What role does high/low cognitive capacity have on CURRENT TRIAL EFFECTS
m2_capacityCatDiff_intxn_rfx = lmer(sqrtRT ~ 1 + easyP1difficultN1 * capacity_HighP1_lowN1 +
                                      (1 | subjectnumber), data = clean_data_dm);
summary(m2_capacityCatDiff_intxn_rfx) # Capacity interacts with current easy/difficult CATEGORICALLY
# Sign of interaction indicates...
#   HIGH capacity people have greater easy/difficult effect
#   LOW capacity people have smaller easy/difficult effect


m2_capacityContDiff_intxn_rfx = lmer(sqrtRT ~ 1 + all_diff_cont * capacity_HighP1_lowN1 +
                                       (1 | subjectnumber), data = clean_data_dm);
summary(m2_capacityContDiff_intxn_rfx)
# SAME PATTERN If you use continuous difficulty instead of categorical difficulty

# effect of difficulty is 0.118 for HIGH CAPACITY
# effect of difficulty is 0.100 for LOW CAPACITY


AIC(m2_capacityCatDiff_intxn_rfx) # AIC: -5874.978
AIC(m2_capacityContDiff_intxn_rfx) # AIC: -6026.223 (CONTINUOUS IS BETTER)


# Model 3: What role does high/low cognitive capacity have on CURRENT AND PREVIOUS TRIAL EFFECTS

m3_prev_capacityCat_intxn_rfx = lmer(sqrtRT ~ 1 +
                                       easyP1difficultN1 * easyP1difficultN1_prev * capacity_HighP1_lowN1 +
                                       (1 | subjectnumber), data = clean_data_dm);
summary(m3_prev_capacityCat_intxn_rfx)
# Current difficulty predicts higher RT
# Previous difficulty predicts lower RT
# Current difficulty predicts EVEN HIGHER RT for people with high capacity.

#Q: separate easy and difficult based upon experienced difficulty

m3_capacityCat_intxn_rfx = lmer(sqrtRT ~ 1 + easy * capacity_HighP1_lowN1 + difficult * capacity_HighP1_lowN1 +
                                  (1 | subjectnumber), data = clean_data_dm);
summary(m3_capacityCat_intxn_rfx)
# Very significant interaction between difficult and capacity, indicating that the above effect is almost
# entirely due to higher RTs for people with higher capacity on DIFFICULT trials specifically.


# Continuous difficulty (including previous) and categorical capacity
m3_prev_diffCont_capacityCat_intxn_rfx = lmer(sqrtRT ~ 1 +
                                                all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1 +
                                                (1 | subjectnumber), data = clean_data_dm);
summary(m3_prev_diffCont_capacityCat_intxn_rfx)
# Besides main effects of current and previous difficulty, NO INTERACTIONS between capacity and
# difficulty (current or previous).



# DON'T COMPARE m3_prev_capacityCat_intxn_rfx AND m3_prev_diffCont_capacityCat_intxn_rfx (HAVE DIFFERENT
# NUMBERS OF TRIALS B/C OF HOW PREVIOUS DIFFICULTY WAS CODED; USED 0'S AT OVERLAP POINTS INSTEAD OF
# NAs AS USED IN CONTINUOUS DIFFICULTY)
# AIC(m3_prev_capacityCat_intxn_rfx)
AIC(m3_prev_diffCont_capacityCat_intxn_rfx) # Be careful when reporting; has fewer datapoints b/c of NAs


# Plot this??
#
# MODEL OUTPUT
#                                                        Estimate Std. Error         df t value Pr(>|t|)
#   (Intercept)                                             1.222e+00  1.333e-02  6.778e+01  91.669  < 2e-16 ***
#   all_diff_cont                                           1.085e-01  6.697e-03  9.839e+03  16.204  < 2e-16 ***
#   prev_all_diff_cont                                     -3.647e-02  6.721e-03  9.840e+03  -5.426 5.91e-08 ***
#   capacity_HighP1_lowN1                                  -1.606e-02  1.333e-02  6.778e+01  -1.205    0.233
#   all_diff_cont:prev_all_diff_cont                        6.487e-03  1.014e-02  9.834e+03   0.640    0.522
#   all_diff_cont:capacity_HighP1_lowN1                     8.721e-03  6.697e-03  9.839e+03   1.302    0.193
#   prev_all_diff_cont:capacity_HighP1_lowN1                6.921e-03  6.721e-03  9.840e+03   1.030    0.303
#   all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1  4.349e-05  1.014e-02  9.834e+03   0.004    0.997
#
# Current difficulty = slower RTs (found previously)
# Prev. difficulty = faster RTs (found previously)
# Curr. & prev. difficulty do NOT interact to influence reaction time.
#
# Capacity = no net effect! [contrary to hypotheses]
#
# in CGT, Capacity interacted with CURRENT difficulty to potentiate slowing due to difficulty
# for high capacity people, and attenuate that effect for low capacity people. NOT SO IN CGE!
#
# in CGT, Capacity had trending interaction with previous difficulty to almost eliminate the effect of prev. difficulty
# for high capacity folks, but potentiate it for low capacity folks. NOT SO IN CGE!

xval_plot = seq(from = 0, to = 1, length.out = 10)
predict_data_m3_H = clean_data_dm[0,];
predict_data_m3_H[1:20,] = NA;
predict_data_m3_H$all_diff_cont[1:10] = xval_plot
predict_data_m3_H$all_diff_cont[11:20] = xval_plot
predict_data_m3_H$prev_all_diff_cont[1:10] = 0;
predict_data_m3_H$prev_all_diff_cont[11:20] = 1;
predict_data_m3_H$capacity_HighP1_lowN1 = 1;

predict_data_m3_L = predict_data_m3_H;
predict_data_m3_L$capacity_HighP1_lowN1 = -1;

predict_output_m3_H = predict(m3_prev_diffCont_capacityCat_intxn_rfx, newdata = predict_data_m3_H, type = 'response', re.form = NA)^2
predict_output_m3_L = predict(m3_prev_diffCont_capacityCat_intxn_rfx, newdata = predict_data_m3_L, type = 'response', re.form = NA)^2

#HIGH CAPACITY PLOT
# First plot PREV easy & CAPACITY high
plot(x = xval_plot, y = predict_output_m3_H[1:10],
     type = 'l', lwd = 5, col = 'blue',
     main = 'Effect of current & previous difficulty: HIGH CAP', xlab = 'Current difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)',
     ylim = c(1.25, 2))
# Second plot PREV diff & CAPACITY high
lines(x = xval_plot, y = predict_output_m3_H[11:20],
      lwd = 5, col = 'red')

#SEPERATE INTO TWO PLOTS (LOW CAPACITY BELOW)
# Third plot PREV easy & CAPACITY low
plot(x = xval_plot, y = predict_output_m3_L[1:10],
     type = 'l',lwd = 5, col = 'blue',
     main = 'Effect of current & previous difficulty: LOW CAP', xlab = 'Current difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)',
     ylim = c(1.25,2))
# Fourth (last) plot PREV diff & CAPACITY low
lines(x = xval_plot, y = predict_output_m3_L[11:20],
      lwd = 5, col = 'red')

# RED = previous trial easy
# BLUE = previous trial difficult


m3_prev_diffCont_capacityCat_intxn_HIGHONLYrfx = lmer(sqrtRT ~ 1 +
                                                        all_diff_cont * prev_all_diff_cont +
                                                        (1 | subjectnumber),
                                                      data = clean_data_dm[clean_data_dm$capacity_HighP1_lowN1 == 1,]);
summary(m3_prev_diffCont_capacityCat_intxn_HIGHONLYrfx)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)
#   (Intercept)                       1.206e+00  1.750e-02  3.289e+01  68.920  < 2e-16 ***
#   all_diff_cont                     1.172e-01  8.756e-03  4.845e+03  13.387  < 2e-16 ***
#   prev_all_diff_cont               -2.957e-02  8.816e-03  4.845e+03  -3.355 0.000801 ***
#   all_diff_cont:prev_all_diff_cont  6.550e-03  1.346e-02  4.843e+03   0.486 0.626637

# Contrary to CGT, there *IS* a significant effect of previous difficulty for high capacity folks.

m3_prev_diffCont_capacityCat_intxn_LOWONLYrfx = lmer(sqrtRT ~ 1 +
                                                       all_diff_cont * prev_all_diff_cont +
                                                       (1 | subjectnumber), data = clean_data_dm[clean_data_dm$capacity_HighP1_lowN1 == -1,]);
summary(m3_prev_diffCont_capacityCat_intxn_LOWONLYrfx)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)
#   (Intercept)                       1.238e+00  2.005e-02  3.495e+01  61.757  < 2e-16 ***
#   all_diff_cont                     9.980e-02  1.018e-02  4.995e+03   9.806  < 2e-16 ***
#   prev_all_diff_cont               -4.340e-02  1.018e-02  4.995e+03  -4.261 2.07e-05 ***
#   all_diff_cont:prev_all_diff_cont  6.464e-03  1.519e-02  4.992e+03   0.425     0.67

# The effect of previous difficulty is STRONGER in low capacity people (just not sig. so)

# THESE FINDINGS TOTALLY PARALLEL THE COMPLEX INTERACTIVE MODEL ABOVE AND BACK UP THE OBSERVATIONS MADE THERE.

for (s in 1:number_of_clean_subjects){
  subj_id = keep_participants[s];
  clean_data_dm$diff_cont[subj_id] = abs(abs(clean_data_dm$choiceP[subj_id] - 0.5)*2-1); # JUST for the easy/difficult dynamic trials
  clean_data_dm$all_diff_cont[subj_id] = abs(abs(clean_data_dm$all_choiceP[subj_id] - 0.5)*2-1); # for ALL trials
  clean_data_dm$capacity_HighP1_lowN1[clean_data_dm$subjectnumber == subj_id] = capacity_HighP1_lowN1[s];

  m3_prev_diffCont_capacityCat_intxn_HIGHONLYrfx = lmer(sqrtRT ~ 1 +
                                                          all_diff_cont * prev_all_diff_cont +
                                                          (1 | subjectnumber),
                                                        data = clean_data_dm[clean_data_dm$capacity_HighP1_lowN1 == 1,]);
  summary(m3_prev_diffCont_capacityCat_intxn_HIGHONLYrfx)

  m3_prev_diffCont_capacityCat_intxn_LOWONLYrfx = lmer(sqrtRT ~ 1 +
                                                         all_diff_cont * prev_all_diff_cont +
                                                         (1 | subjectnumber), data = clean_data_dm[clean_data_dm$capacity_HighP1_lowN1 == -1,]);
  summary(m3_prev_diffCont_capacityCat_intxn_LOWONLYrfx)
}

for(s in 1:number_of_clean_subjects){
  subj_id = keep_participants[s];
  clean_data_dm$capacity_HighP1_lowN1[clean_data_dm$subjectnumber == subj_id] = capacity_HighP1_lowN1[s];
}

# Examine best-fitting threshold values
possible_threshold_values = sort(unique(compositeSpanScores))+.000001;
possible_threshold_values = possible_threshold_values[1:(length(possible_threshold_values)-1)];

all_aic_values = array(data = NA, dim = length(possible_threshold_values));

clean_data_dm$capacity_HighP1_lowN1_temp = NA;

for(ind in 1:length(possible_threshold_values)){
  break_val = possible_threshold_values[ind];
  clean_data_dm$capacity_HighP1_lowN1_temp[clean_data_dm$complexspan > break_val] = 1;
  clean_data_dm$capacity_HighP1_lowN1_temp[clean_data_dm$complexspan < break_val] = -1;

  cat(sprintf('This many people are < break_val: %g\n',sum(compositeSpanScores<break_val, na.rm = T)))

  minimum_grp_count = 6; # What's the smallest group we will tolerate? 
  minimum_grp_percent = 0.15; # What's the smallest group we will tolerate? 
  if((sum(compositeSpanScores<break_val, na.rm = T) < minimum_grp_count) | 
     (sum(compositeSpanScores>break_val, na.rm = T) < minimum_grp_count) | 
     (sum(compositeSpanScores>break_val, na.rm = T)/sum(is.finite(compositeSpanScores)) < minimum_grp_percent) | 
     (sum(compositeSpanScores<break_val, na.rm = T)/sum(is.finite(compositeSpanScores)) < minimum_grp_percent)){
    next # don't use any categorizations that create a 'group' with just 1 person
  }

  m3_tmp = lmer(sqrtRT ~ 1 + all_diff_cont * capacity_HighP1_lowN1_temp +
                        prev_all_diff_cont * capacity_HighP1_lowN1_temp +
                  (1 | subjectnumber), data = clean_data_dm, REML = F);
  all_aic_values[ind] = AIC(m3_tmp)
}

best_aic = min(all_aic_values, na.rm = T);
best_threshold = possible_threshold_values[which(all_aic_values == best_aic)];

cat(sprintf('The best fitting model uses a CompositeSpan threshold value of %g.\n', best_threshold))
cat(sprintf('The best AIC obtained with the CompositeSpan was %0.2f\n', best_aic))

plot(sort(compositeSpanScores), xlab = 'Participants', ylab = 'Span')
abline(h = best_threshold, col = 'blue', lwd = 4)
abline(h = median_compositespan, col = 'red', lwd = 4, lty = 'dotted') # median value
legend('topleft', legend = c('Span','Best threshold','Median span'), col = c('black','blue', 'red'), lty = 1)

plot(x = possible_threshold_values, y = all_aic_values, type = 'l', col = 'green', xlab = 'Thresholds', ylab = 'AIC values (lower better)')
abline(v = median_compositespan, col = 'red', lwd = 4, lty = 'dotted')
abline(v = best_threshold, col = 'blue', lwd = 4)

# The best-fitting threshold (just slightly above the median value) is 0.6571439

break_val = best_threshold; # as of 2/25/24, this was 0.6571439

capacity_HighP1_lowN1_Best = (compositeSpanScores[keep_participants] > break_val)*2 - 1;

clean_data_dm$capacity_HighP1_lowN1_best[clean_data_dm$complexspan > break_val] = 1;
clean_data_dm$capacity_HighP1_lowN1_best[clean_data_dm$complexspan < break_val] = -1;

m3_best = lmer(sqrtRT ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best +
                       prev_all_diff_cont * capacity_HighP1_lowN1_temp +
                 (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best)
m3_best_summary = summary(m3_best);
m3_best_meanlik = exp(-m3_best_summary$logLik/nobs(m3_best));
cat(sprintf('The best mean likelihood obtained with the CompositeSpan was %0.4f\n', m3_best_meanlik))


m3_best_continuousWMC_nointxn = lmer(sqrtRT ~ 1 + all_diff_cont * complexspan_demeaned + prev_all_diff_cont * complexspan_demeaned +
                         (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_continuousWMC_nointxn) # THIS OUTPERFORMS THE FULLY-INTERACTIVE VERSION


m3_best_HighCap_only = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont +
                              (1 | subjectnumber), data = clean_data_dm[clean_data_dm$capacity_HighP1_lowN1_best == 1,], REML = F);
summary(m3_best_HighCap_only)

m3_best_LowCap_only = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont +
                             (1 | subjectnumber), data = clean_data_dm[clean_data_dm$capacity_HighP1_lowN1_best == -1,], REML = F);
summary(m3_best_LowCap_only)


# Re-do easy/difficult regression w/ this split
m3_capacityCat_intxn_rfx_bestcap = lmer(sqrtRT ~ 1 + easy * capacity_HighP1_lowN1_best +
                                          difficult * capacity_HighP1_lowN1_best +
                                          (1 | subjectnumber), data = clean_data_dm);
summary(m3_capacityCat_intxn_rfx_bestcap)
# With this "best split", BOTH easy and difficult are different by capacity?
# This is dynamic-ONLY (using easy/difficult as categorical)


m2_best_cap = lmer(sqrtRT ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best +
                     (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m2_best_cap)
xval_plot = seq(from = 0, to = 1, by = .1);
coef_vals = fixef(m2_best_cap)

plot(x = xval_plot, y = (coef_vals["(Intercept)"] + xval_plot*coef_vals["all_diff_cont"] +
                           1*xval_plot*coef_vals["all_diff_cont:capacity_HighP1_lowN1_best"] +
                           1*coef_vals["capacity_HighP1_lowN1_best"])^2,
     type = 'l', lwd = 5, col = 'purple4',
     main = 'Effect of current difficulty', xlab = 'Difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)')
lines(x = xval_plot, y = (coef_vals["(Intercept)"] + xval_plot*coef_vals["all_diff_cont"] +
                            -1*xval_plot*coef_vals["all_diff_cont:capacity_HighP1_lowN1_best"]+
                            -1*coef_vals["capacity_HighP1_lowN1_best"])^2,
      lwd = 5, col = 'purple1')



# xval_plot = seq(from = 0, to = 1, by = .1); # current difficulty (easy = 0, difficult = 1)
# prev_trial_diff = c(0,1); # easy = 0, difficult = 1
# capacity = c(1, -1); # HIGH = 1, low = -1
# coef_vals = fixef(m3_best)

xval_plot = seq(from = 0, to = 1, length.out = 10)
predict_data_m3_best_H = clean_data_dm[0,];
predict_data_m3_best_H[1:20,] = NA;
predict_data_m3_best_H$all_diff_cont[1:10] = xval_plot
predict_data_m3_best_H$all_diff_cont[11:20] = xval_plot
predict_data_m3_best_H$prev_all_diff_cont[1:10] = 0;
predict_data_m3_best_H$prev_all_diff_cont[11:20] = 1;
predict_data_m3_best_H$capacity_HighP1_lowN1_best = 1;

predict_data_m3_best_L = predict_data_m3_best_H;
predict_data_m3_best_L$capacity_HighP1_lowN1_best = -1;

predict_output_m3_best_H = predict(m3_best, newdata = predict_data_m3_best_H, type = 'response', re.form = NA)^2
predict_output_m3_best_L = predict(m3_best, newdata = predict_data_m3_best_L, type = 'response', re.form = NA)^2


#HIGH CAPACITY PLOT
# First plot PREV easy & CAPACITY high
plot(x = xval_plot, y = predict_output_m3_best_H[1:10],
     type = 'l', lwd = 5, col = 'blue',
     main = 'Effect of current & previous difficulty: HIGH CAP', xlab = 'Current difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)',
     ylim = c(1.25, 1.85))
# Second plot PREV diff & CAPACITY high
lines(x = xval_plot, y = predict_output_m3_best_H[11:20],
      lwd = 5, col = 'red')

#SEPERATE INTO TWO PLOTS (LOW CAPACITY BELOW)
# Third plot PREV easy & CAPACITY low
plot(x = xval_plot, y = predict_output_m3_best_L[1:10],
     type = 'l',lwd = 5, col = 'blue',
     main = 'Effect of current & previous difficulty: LOW CAP', xlab = 'Current difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)',
     ylim = c(1.25,1.85))
# Fourth (last) plot PREV diff & CAPACITY low
lines(x = xval_plot, y = predict_output_m3_best_L[11:20],
      lwd = 5, col = 'red')


# Are we better off using O-Span or Sym-Span (vs. Composite Span)?

# Examine best-fitting threshold values: OSPAN
possible_threshold_values_Ospan = sort(unique(ospanScores))+.000001;
possible_threshold_values_Ospan = possible_threshold_values_Ospan[1:(length(possible_threshold_values_Ospan)-1)];

all_aic_values = array(data = NA, dim = length(possible_threshold_values_Ospan));
all_meanLik_values = array(data = NA, dim = length(possible_threshold_values_Ospan));

clean_data_dm$capacity_HighP1_lowN1_Ospan_temp = NA;

for(ind in 1:length(possible_threshold_values_Ospan)){
  break_val = possible_threshold_values_Ospan[ind];
  clean_data_dm$capacity_HighP1_lowN1_Ospan_temp[clean_data_dm$ospan > break_val] = 1;
  clean_data_dm$capacity_HighP1_lowN1_Ospan_temp[clean_data_dm$ospan < break_val] = -1;

  cat(sprintf('This many people are < break_val: %g\n',sum(ospanScores<break_val, na.rm = T)))

  if((sum(ospanScores<break_val, na.rm = T) == 1) | (sum(ospanScores>break_val, na.rm = T) == 1)){
    next # don't use any categorizations that create a 'group' with just 1 person
  }

  m3_tmp = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_Ospan_temp +
                  (1 | subjectnumber), data = clean_data_dm, REML = F);
  m3_tmp_summ = summary(m3_tmp);

  all_aic_values[ind] = AIC(m3_tmp)
  all_meanLik_values[ind] = exp(-m3_tmp_summ$logLik/nobs(m3_tmp))
}

best_Ospan_aic = min(all_aic_values, na.rm = T);
best_Ospan_threshold = possible_threshold_values_Ospan[which(all_aic_values == best_Ospan_aic)];
best_Ospan_meanlik = all_meanLik_values[which(all_aic_values == best_Ospan_aic)];

cat(sprintf('The best fitting model uses an OSpan threshold value of %g.\n', best_Ospan_threshold))
cat(sprintf('The best AIC obtained with the O-Span was %0.2f\n', best_Ospan_aic))
cat(sprintf('The best mean likelihood obtained with the Ospan was %0.4f\n', best_Ospan_meanlik))

# Examine best-fitting threshold values: SYMSPAN
possible_threshold_values_Symspan = sort(unique(symspanScores))+.000001;
possible_threshold_values_Symspan = possible_threshold_values_Symspan[1:(length(possible_threshold_values_Symspan)-1)];

all_aic_values = array(data = NA, dim = length(possible_threshold_values_Symspan));
all_meanLik_values = array(data = NA, dim = length(possible_threshold_values_Symspan));

clean_data_dm$capacity_HighP1_lowN1_Symspan_temp = NA;

for(ind in 1:length(possible_threshold_values_Symspan)){
  break_val = possible_threshold_values_Symspan[ind];
  clean_data_dm$capacity_HighP1_lowN1_Symspan_temp[clean_data_dm$symspan > break_val] = 1;
  clean_data_dm$capacity_HighP1_lowN1_Symspan_temp[clean_data_dm$symspan < break_val] = -1;

  cat(sprintf('This many people are < break_val: %g\n',sum(symspanScores<break_val, na.rm = T)))

  if((sum(symspanScores<break_val, na.rm = T) == 1) | (sum(symspanScores>break_val, na.rm = T) == 1)){
    next # don't use any categorizations that create a 'group' with just 1 person
  }

  m3_tmp = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_Symspan_temp +
                  (1 | subjectnumber), data = clean_data_dm, REML = F);
  m3_tmp_summ = summary(m3_tmp);

  all_aic_values[ind] = AIC(m3_tmp)
  all_meanLik_values[ind] = exp(-m3_tmp_summ$logLik/nobs(m3_tmp))
}

best_Symspan_aic = min(all_aic_values, na.rm = T);
best_Symspan_threshold = possible_threshold_values_Symspan[which(all_aic_values == best_Symspan_aic)];
best_Symspan_meanlik = all_meanLik_values[which(all_aic_values == best_Symspan_aic)];

cat(sprintf('The best fitting model uses a SymSpan threshold value of %g.\n', best_Symspan_threshold))
cat(sprintf('The best AIC obtained with the SymSpan was %0.2f\n', best_Symspan_aic))
cat(sprintf('The best mean likelihood obtained with the SymSpan was %0.4f\n', best_Symspan_meanlik))


# Which model does best?
# Fraught question: different people & trials in these 3 models.
# (well, the Ospan & Symspan are different; composite span includes both of those, but other people too)
# Mean choice likelihood is best for SymSpan (0.7280), then Composite (0.7222), then OSpan (0.7195). Differences
# are slight, so probably prefer the model that fits more data, i.e. composite.


### Leave One Out Cross Validation ###

# xval_plot = seq(from = 0, to = 1, length.out = 10)
# predict_data_m3_H = clean_data_dm[0,];
# predict_data_m3_H[1:20,] = NA;
# predict_data_m3_H$all_diff_cont[1:10] = xval_plot
# predict_data_m3_H$all_diff_cont[11:20] = xval_plot
# predict_data_m3_H$prev_all_diff_cont[1:10] = 0;
# predict_data_m3_H$prev_all_diff_cont[11:20] = 1;
# predict_data_m3_H$capacity_HighP1_lowN1 = 1;
#
# predict_data_m3_L = predict_data_m3_H;
# predict_data_m3_L$capacity_HighP1_lowN1 = -1;
#
# predict_output_m3_H = predict(m3_prev_diffCont_capacityCat_intxn_rfx, newdata = predict_data_m3_H, type = 'response', re.form = NA)^2
# predict_output_m3_L = predict(m3_prev_diffCont_capacityCat_intxn_rfx, newdata = predict_data_m3_L, type = 'response', re.form = NA)^2
#
# #HIGH CAPACITY PLOT
# # First plot PREV easy & CAPACITY high
# plot(x = xval_plot, y = predict_output_m3_H[1:10],
#      type = 'l', lwd = 5, col = 'blue',
#      main = 'Effect of current & previous difficulty: HIGH CAP', xlab = 'Current difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)',
#      ylim = c(1.25, 2))
# # Second plot PREV diff & CAPACITY high
# lines(x = xval_plot, y = predict_output_m3_H[11:20],
#       lwd = 5, col = 'red')
#
# tmpdataDyn = tmpdata$trialnumber[tmpdata$static0dynamic1 == 1];
# tmpdataDynSamp = sample(tmpdataDyn) # use length?
#
# trainDynData = tmpdataDynSamp[1:108]
# testDynData = tmpdataDynSamp[109:120]

# All dynamic trials
for (subj in 1:number_of_clean_subjects){
  subj_id = keep_participants[subj];
  tmpdata = clean_data_dm[clean_data_dm$subjectnumber == subj_id,];

  tmpdataDyn = tmpdata[tmpdata$static0dynamic1 == 1,];
  tmpdataDynSamp = sample(tmpdataDyn$trialnumber)

  trainDynData = tmpdataDynSamp[1:108]
  testDynData = tmpdataDynSamp[109:120]

  # m1_trainDynData_intxn_rfx = lmer(sqrtRT ~ 1 +
  #                       trainDynData +
  #                       (1 | subjectnumber), data = tmpdata);
  # summary(m1_trainDynData_intxn_rfx)

}

# All easy trials
for (subj in 1:number_of_clean_subjects){
  subj_id = keep_participants[subj];
  tmpdata = clean_data_dm[clean_data_dm$subjectnumber == subj_id,];

  tmpdataEasy = tmpdata[tmpdata$easyP1difficultN1 == 1,];
  tmpdataEasySamp = sample(tmpdataEasy$trialnumber)

  trainEasyData = tmpdataEasySamp[1:54]
  testEasyData = tmpdataEasySamp[55:60]


}

# All difficult trials
for (subj in 1:number_of_clean_subjects){
  subj_id = keep_participants[subj];
  tmpdata = clean_data_dm[clean_data_dm$subjectnumber == subj_id,];

  tmpdataDiff = tmpdata[tmpdata$easyP1difficultN1 == -1,];
  tmpdataDiffSamp = sample(tmpdataDiff$trialnumber)

  trainDiffData = tmpdataDiffSamp[1:54]
  testDiffData = tmpdataDiffSamp[55:60]


}


### Testing the integration of Survey Data ###  - I think this is where it breaks

NCS_HighP1_LowN1 = (clean_data_dm$NCS >= median(clean_data_survey$NCS, na.rm = T))*2-1;
IUS_HighP1_LowN1 = (clean_data_dm$IUS >= median(clean_data_survey$IUS, na.rm = T))*2-1;
SNS_HighP1_LowN1 = (clean_data_dm$SNS >= median(clean_data_survey$SNS, na.rm = T))*2-1;
PSS_HighP1_LowN1 = (clean_data_dm$PSS >= median(clean_data_survey$PSS, na.rm = T))*2-1;

clean_data_dm$NCS_HighP1_LowN1 = (clean_data_dm$NCS >= median(clean_data_survey$NCS, na.rm = T))*2-1;
clean_data_dm$IUS_HighP1_LowN1 = (clean_data_dm$IUS >= median(clean_data_survey$IUS, na.rm = T))*2-1;
clean_data_dm$SNS_HighP1_LowN1 = (clean_data_dm$SNS >= median(clean_data_survey$SNS, na.rm = T))*2-1;
clean_data_dm$PSS_HighP1_LowN1 = (clean_data_dm$PSS >= median(clean_data_survey$PSS, na.rm = T))*2-1;


clean_data_dm$NCS_HighP1_LowN1

# Trying NCS as noted in thesis proposal
sum(NCS_HighP1_LowN1 == 1, na.rm = T) # 7480 - doesn't sound right
sum(NCS_HighP1_LowN1 == -1, na.rm = T) # 6800 - doesn't sound right





# NCS, IUS, SNS, PSS

m3_best_NCS = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best * NCS_HighP1_LowN1 +
                     (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_NCS)
# OLD: NCS x SPAN x current difficulty interaction
# New: not much; a 4-way interaction + a trend for current x capacity x NCS

m3_best_IUS = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best * IUS_HighP1_LowN1 +
                     (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_IUS)
# OLD:
# IUS x Span x current difficulty interaction
# IUS x current difficulty interaction
#
# NEW:
# current difficulty x IUS (negative)
#   -> effect of current difficulty on RTs is stronger for people low on IUS, and weaker for people high on IUS
# previous difficulty x IUS (negative)
#   -> effect of previous difficulty on RTs is stronger for people HIGH on IUS, and weaker for people low on IUS
# so low IUS -> strong effect of current difficulty, weak effect of prev.
# so high IUS -> weak effect of current difficulty, strong effect of prev.

m3_best_SNS = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best * SNS_HighP1_LowN1 +
                     (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_SNS)
# SNS main effect + (high numeracy = slower)
# previous difficulty x SNS interaction (high numeracy folks have strong prev. difficulty effect)

m3_best_PSS = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best * PSS_HighP1_LowN1 +
                     (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_PSS)
# ... nothing...

AIC(m3_best_NCS) # -8782.879
AIC(m3_best_IUS) # -8794.3 <- BEST AIC = IUS categorical
AIC(m3_best_SNS) # -8789.276
AIC(m3_best_PSS) # -8785.113

# OLD:
# NCS, IUS, and PSS all have the same interaction in their regressions - with current difficulty
# and working memory span. These interactions look like they qualify the 2-way interaction between
# current difficulty & working memory span (that's present in these regressions). THAT interaction
# best characterized as a difference in the slope of current difficulty between high and low
# capacity participants (high cap participants have a steeper slope with current difficulty). So
# these 3-way interactions (+ve for NCS, -ve for IUS & PSS) can be broadly interpreted to indicate
# that that increasing steepness with capacity is WEAKER for people low in need for cognition /
# high in intolerance of uncertainty / high in chronic stress (conversely, the opposite is true:
# Increasing steepness w/ increasing capacity is STRONGER in people high in need for cognition,
# low in intolerance of uncertainty / low in chronic stress).


m3_best_IUS_SNS = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best * IUS_HighP1_LowN1 +
                         all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best * SNS_HighP1_LowN1 +
                         (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_IUS_SNS) # AIC -6459

# We can run this regression, but... it's a lot. Many 2- & 3-way interactions.
# AIC *is* better (-6459.4)
#
# Brief summary:
# MAIN EFFECTS
# current difficulty +
# previous difficulty -
# SNS score +
#
# TWO WAY INTERACTIONS:
# current difficulty x capacity (weak, trend) +
# previous difficulty x capacity +
# current difficulty x IUS -
#
# THREE WAY INTERACTION:
# Current difficulty x capacity x IUS -
#

m3_best_IUS_SNS_simple = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                all_diff_cont * prev_all_diff_cont * IUS_HighP1_LowN1 +
                                all_diff_cont * prev_all_diff_cont * SNS_HighP1_LowN1 +
                                (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_IUS_SNS_simple) # AIC -6443; worse than mostly-interactive version

m3_best_IUS_SNS_full = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best * IUS_HighP1_LowN1 * SNS_HighP1_LowN1 +
                              (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_IUS_SNS_full) # AIC -6452; worse than mostly-interactive version


# m3_best_summary = summary(m3_best);
# m3_best_meanlik = exp(-m3_best_summary$logLik/nobs(m3_best));
# cat(sprintf('The best mean likelihood obtained with the CompositeSpan was %0.4f\n', m3_best_meanlik))

# m3_best
# sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber)
# AIC -8760.373



### Adding in TrialNumber (time) to RT regressions ############################################

# The best-fitting model behaviorally uses current diffculty, previous
# difficulty, and the best-fitting split between high and low capacity.
#
# The goal of these regressions is to establish the extent to which including
# trial number (as a stand-in for time) improves model fit and/or changes any
# of the main results in an important way.
#
# The motivation for this is twofold: 1) it might matter and we haven't tested
# it, and 2) it matters for pupillometry.

# m3_best
# sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber)
# AIC -8760.373

m3_best_trialNum_MEonly = lmer(sqrtRT ~ 1 + trialnumberRS + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                 (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_trialNum_MEonly)
# AIC -9347.1 (better)
# Main Effects of...
# - trial number (neg.; faster with increasing time in task)
# - current difficulty (pos.; slower with increasing curr. difficulty)
# - previous difficulty (MARGINAL; neg.; faster with increasing prev. difficulty)
#
# Interaction Effects of...
# - curr diff & Capacity (pos.; more of an effect for current difficulty if high cap)
# - prev diff & Capacity (pos.; less of the negative effect of prev. diff. if high cap)

# TAKEAWAY: the story remains the same if the simple main effect of trialnumber is included

m3_best_trialNum_MEonly_trialRFX = lmer(sqrtRT ~ 1 + trialnumberRS + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                 (1 + trialnumberRS | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_trialNum_MEonly_trialRFX)
# AIC -9741.4 (better)
# Main effects are same as above (trialnum, curr diff, prev diff)
#
# Interaction effects are weakened or eliminated (which figures, given RFX & indiv. diffs measures).
# - curr diff & Capacity (pos; p = 0.052)
# - curr diff & prev diff & Capacity (pos; p = 0.08)
#
# prev diff & Capacity is GONE (p = 0.68)

# Including trial number RFX might eliminate some individual differences terms.
# Unclear how to proceed.

m3_best_trialNum_full = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best * trialnumberRS +
                                       (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_trialNum_full) # FULL regression w/ all interactions
# SIGNIFICANT EFFECTS:
# all_diff_cont                                                              1.348e-01  1.282e-02  1.368e+04  10.512  < 2e-16 ***
# prev_all_diff_cont                                                        -3.813e-02  1.288e-02  1.368e+04  -2.959  0.00309 **
# capacity_HighP1_lowN1_best                                                -3.712e-02  1.371e-02  1.356e+02  -2.706  0.00768 **
# trialnumberRS                                                             -1.336e-01  1.218e-02  1.368e+04 -10.966  < 2e-16 ***
# prev_all_diff_cont:trialnumberRS                                           4.872e-02  2.110e-02  1.368e+04   2.309  0.02093 *
# capacity_HighP1_lowN1_best:trialnumberRS                                   4.915e-02  1.218e-02  1.368e+04   4.035  5.5e-05 ***


m3_best_trialNum_3WayIntxOnly = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                       all_diff_cont * prev_all_diff_cont * trialnumberRS +
                                       all_diff_cont * capacity_HighP1_lowN1_best * trialnumberRS +
                                       prev_all_diff_cont * capacity_HighP1_lowN1_best * trialnumberRS +
                                       (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_trialNum_3WayIntxOnly) # no 4-way interaction, otherwise identical

anova(m3_best_trialNum_full,m3_best_trialNum_3WayIntxOnly) # FULL IS NOT BETTER

# AIC -9455.5 (a little better)
# Main effects of...
# - curr diff (+), prev diff (-), capacity (-), and trial (-)
# Two Way Interactions of...
# - prev. diff & trial number (+) (less of a - prev. diff effect with increasing trial)
# - capacity & trial number (+) (faster with increasing trial # if low capacity)

m3_best_trialNum_3WayIntxOnly = lmer(sqrtRT ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best * trialnumberRS +
                                       prev_all_diff_cont * capacity_HighP1_lowN1_best * trialnumberRS +
                                       (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_trialNum_3WayIntxOnly) # no 4-way interaction

m3_best_trialNum_contWMC_3WayIntxOnly = lmer(sqrtRT ~ 1 + all_diff_cont * complexspan_demeaned * trialnumberRS +
                                       prev_all_diff_cont * complexspan_demeaned * trialnumberRS +
                                       (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_trialNum_contWMC_3WayIntxOnly)

AIC(m3_best_trialNum_3WayIntxOnly) # does better
AIC(m3_best_trialNum_contWMC_3WayIntxOnly)

m3_best_trialNum_2WayIntxOnly = lmer(sqrtRT ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best +
                                       all_diff_cont * trialnumberRS +
                                       prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                       prev_all_diff_cont * trialnumberRS +
                                       capacity_HighP1_lowN1_best * trialnumberRS +
                                       (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_trialNum_2WayIntxOnly)

m3_best_trialNum_contWMC_2WayIntxOnly = lmer(sqrtRT ~ 1 + all_diff_cont * complexspan_demeaned +
                                               all_diff_cont * trialnumberRS +
                                               prev_all_diff_cont * complexspan_demeaned +
                                               prev_all_diff_cont * trialnumberRS +
                                               complexspan_demeaned * trialnumberRS +
                                               (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_trialNum_contWMC_2WayIntxOnly) # no 3-way interaction

AIC(m3_best_trialNum_2WayIntxOnly) # better
AIC(m3_best_trialNum_contWMC_2WayIntxOnly)


AIC(m3_best_trialNum_3WayIntxOnly) # better
AIC(m3_best_trialNum_2WayIntxOnly)
anova(m3_best_trialNum_3WayIntxOnly, m3_best_trialNum_2WayIntxOnly)

anova(m3_best_trialNum_full, m3_best_trialNum_3WayIntxOnly, m3_best_trialNum_2WayIntxOnly)

# The best model has only 2-way interactions (models with 3 & 4-way interactions do
# not perform significantly better).

summary(m3_best_trialNum_2WayIntxOnly)



m3_best_trialNum_2WayIntxOnly_highCap = lmer(sqrtRT ~ 1 +
                                       all_diff_cont * trialnumberRS +
                                       prev_all_diff_cont * trialnumberRS +
                                       (1 | subjectnumber), data = clean_data_dm[clean_data_dm$capacity_HighP1_lowN1_best == 1,], REML = F);
m3_best_trialNum_2WayIntxOnly_lowCap = lmer(sqrtRT ~ 1 +
                                               all_diff_cont * trialnumberRS +
                                               prev_all_diff_cont * trialnumberRS +
                                               (1 | subjectnumber), data = clean_data_dm[clean_data_dm$capacity_HighP1_lowN1_best == -1,], REML = F);
summary(m3_best_trialNum_2WayIntxOnly_highCap)
#                                    Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)                         1.21667    0.02088   36.84603  58.271  < 2e-16 ***
# all_diff_cont                       0.13573    0.01349 4519.82565  10.059  < 2e-16 ***
# trialnumberRS                      -0.08487    0.01501 4518.81187  -5.652 1.68e-08 ***
# prev_all_diff_cont                 -0.03204    0.01353 4520.06777  -2.368   0.0179 *
# all_diff_cont:trialnumberRS         0.02277    0.02173 4518.68947   1.048   0.2948
# trialnumberRS:prev_all_diff_cont    0.04970    0.02177 4518.83446   2.283   0.0225 *
summary(m3_best_trialNum_2WayIntxOnly_lowCap)
#                                    Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)                         1.30134    0.01564   84.48435  83.186  < 2e-16 ***
# all_diff_cont                       0.11698    0.01033 9160.23098  11.326  < 2e-16 ***
# trialnumberRS                      -0.18815    0.01259 9155.27608 -14.940  < 2e-16 ***
# prev_all_diff_cont                 -0.06116    0.01036 9160.44232  -5.905 3.64e-09 ***
# all_diff_cont:trialnumberRS        -0.01230    0.01664 9155.07098  -0.740 0.459550
# trialnumberRS:prev_all_diff_cont    0.06458    0.01665 9155.11543   3.878 0.000106 ***

# including choice
m3_best_trialNum_2WayIntxOnly_choice = lmer(sqrtRT ~ 1 +
                                       all_diff_cont * capacity_HighP1_lowN1_best +
                                       all_diff_cont * trialnumberRS +
                                       all_diff_cont * choice +
                                       prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                       prev_all_diff_cont * trialnumberRS +
                                       prev_all_diff_cont * choice +
                                       capacity_HighP1_lowN1_best * trialnumberRS * choice +
                                       (1 | subjectnumber), data = clean_data_dm, REML = F);
summary(m3_best_trialNum_2WayIntxOnly_choice)
#                                                   Estimate Std. Error         df t value Pr(>|t|)

# SIGNIFICANT
# (Intercept)                                      1.248e+00  1.368e-02  1.361e+02  91.188  < 2e-16 ***
# all_diff_cont                                    1.475e-01  9.218e-03  1.368e+04  16.005  < 2e-16 ***
# capacity_HighP1_lowN1_best                      -3.446e-02  1.290e-02  1.077e+02  -2.671  0.00874 **
# trialnumberRS                                   -1.299e-01  1.125e-02  1.368e+04 -11.546  < 2e-16 ***
# choice                                           1.888e-02  7.360e-03  1.368e+04   2.565  0.01033 *
# prev_all_diff_cont                              -4.232e-02  9.079e-03  1.368e+04  -4.661 3.17e-06 ***
# all_diff_cont:capacity_HighP1_lowN1_best         1.795e-02  3.798e-03  1.368e+04   4.727 2.30e-06 ***
# all_diff_cont:choice                            -3.165e-02  7.286e-03  1.369e+04  -4.344 1.41e-05 ***
# capacity_HighP1_lowN1_best:prev_all_diff_cont    1.042e-02  3.799e-03  1.368e+04   2.742  0.00612 **
# trialnumberRS:prev_all_diff_cont                 5.918e-02  1.325e-02  1.367e+04   4.467 8.01e-06 ***
# capacity_HighP1_lowN1_best:trialnumberRS         4.924e-02  7.720e-03  1.368e+04   6.379 1.85e-10 ***
# capacity_HighP1_lowN1_best:choice               -1.873e-02  6.301e-03  1.368e+04  -2.973  0.00295 **

# NON-SIGNIFICANT
# all_diff_cont:trialnumberRS                     -3.937e-03  1.326e-02  1.367e+04  -0.297  0.76648
# choice:prev_all_diff_cont                       -1.063e-02  7.153e-03  1.367e+04  -1.487  0.13716
# trialnumberRS:choice                            -8.007e-03  1.110e-02  1.368e+04  -0.721  0.47085
# capacity_HighP1_lowN1_best:trialnumberRS:choice  1.256e-02  1.086e-02  1.368e+04   1.156  0.24761

anova(m3_best_trialNum_2WayIntxOnly,m3_best_trialNum_2WayIntxOnly_choice)
#                                      npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# m3_best_trialNum_2WayIntxOnly          12 -9458.5 -9368.2 4741.3  -9482.5
# m3_best_trialNum_2WayIntxOnly_choice   18 -9486.9 -9351.4 4761.5  -9522.9 40.382  6  3.832e-07 ***



### Individual Regressions ############################################

model_summaries = list()
lm_estimates = array(data = NA, dim = c(number_of_clean_subjects,4)); # 4 estimates
lm_pvalues = array(data = NA, dim = c(number_of_clean_subjects,4)); # 4 p-values

for (s in 1:number_of_clean_subjects){
  subj_id = keep_participants[s]
  tmpdata = clean_data_dm[clean_data_dm$subjectnumber == subj_id,];

  indiv_model = lm(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont,
                   data = tmpdata);

  model_summaries[[s]] = summary(indiv_model)
  lm_estimates[s,] = coef(indiv_model);
  lm_pvalues[s,] = summary(indiv_model)$coefficients[,4]
}

# Columns...
# 1 = intercept
# 2 = all_diff_cont
# 3 = prev_all_diff_cont
# 4 = all_diff_cont:prev_all_diff_cont

# p-values... expect low numbers for big effects
plot(lm_pvalues[,2], compositeSpanScores[keep_participants]) # current difficulty
# given big model, mostly expect negative relationship (higher span = lower/more sig. p-value)
# LOOKS LIKE THAT
plot(lm_pvalues[,3], compositeSpanScores[keep_participants]) # previous difficulty
# given big model, mostly expect positive relationship (higher span = higher/less sig. p-value)
# LOOKS LIKE THAT

# Estimates... expect high values for big effects
plot(lm_estimates[,2], compositeSpanScores[keep_participants]) # current difficulty
cor.test(lm_estimates[,2], compositeSpanScores[keep_participants]) # positive w/ p = 0.087 (trend)
# given big model, mostly expect positive relationship (higher span = larger estimate)
# LOOKS LIKE THAT
plot(lm_estimates[,3], compositeSpanScores[keep_participants]) # previous difficulty
cor.test(lm_estimates[,3], compositeSpanScores[keep_participants]) # POSITIVE*, w/ p = 0.033    *unexpected!
# given big model, mostly expect negative relationship (higher span = lower estimate)
# WITH A FEW EXCEPTIONS, LOOKS LIKE THAT?

# TAKEAWAY: Unsurprisingly, these individual-level estimates & p-values/etc are noisy.
# Without the stabilizing effect of a hierarchical approach, these are hard to read.
# Additionally the magnitude and significance of an estimate are two different things
# and this approach doesn't handle that gracefully.

# PROBABLY DON'T TAKE SERIOUSLY OR USE CENTRALLY



### Throwing In The Towel ############################################

### Explaining lower RTs (faster choices) after difficult trials:
# Are low-capacity participants "throwing in the towel" after difficult choices, or
# is their choice process *facilitated*, and thus better?

# APPROACH:
#   1. Use all_choice_P values (calculated from estimates of rho & mu from static trials) to...
#   2. Calculate the LIKELIHOOD of data on a per-trial basis in dynamic trials, and then...
#   3. Calculate the MEAN LIKELIHOOD of dynamic trials that follow a difficult trial
#      and compare it to the MEAN LIKELIHOOD of dynamic trials that follow an easy trial
#   4. Calculate the difference in MEAN LIKELIHOOD (easy - difficult), and...
#   5. Relate that difference to capacity group and/or continuous capacity.

# 1. is already done.

# 2. Calculate the likelihood of the choices.

clean_data_dm$all_choice_likelihood = clean_data_dm$choice * clean_data_dm$all_choiceP + (1-clean_data_dm$choice) * (1-clean_data_dm$all_choiceP);

# 3. Calculate the mean likelihood on dynamic trials after diff. vs. easy trials.

mean_choice_lik_prevEasy = array(data = NA, dim = c(number_of_clean_subjects,1));
mean_choice_lik_prevDiff = array(data = NA, dim = c(number_of_clean_subjects,1));

for (s in 1:number_of_clean_subjects){
  subj_id = keep_participants[s]
  tmpdata = clean_data_dm[clean_data_dm$subjectnumber == subj_id,];

  mean_choice_lik_prevEasy[s] = mean(tmpdata$all_choice_likelihood[tmpdata$easyP1difficultN1_prev == 1], na.rm = T); # na.rm b/c of missed trials
  mean_choice_lik_prevDiff[s] = mean(tmpdata$all_choice_likelihood[tmpdata$easyP1difficultN1_prev == -1], na.rm = T);

  # Use this code to run this test ONLY on current difficult trials
  # mean_choice_lik_prevEasy[s] = mean(tmpdata$all_choice_likelihood[(tmpdata$easyP1difficultN1_prev == 1) & (tmpdata$easyP1difficultN1 == -1)], na.rm = T); # na.rm b/c of missed trials
  # mean_choice_lik_prevDiff[s] = mean(tmpdata$all_choice_likelihood[(tmpdata$easyP1difficultN1_prev == -1) & (tmpdata$easyP1difficultN1 == -1)], na.rm = T);

  # Use this code to run this test ONLY on current easy trials
  # mean_choice_lik_prevEasy[s] = mean(tmpdata$all_choice_likelihood[(tmpdata$easyP1difficultN1_prev == 1) & (tmpdata$easyP1difficultN1 == 1)], na.rm = T); # na.rm b/c of missed trials
  # mean_choice_lik_prevDiff[s] = mean(tmpdata$all_choice_likelihood[(tmpdata$easyP1difficultN1_prev == -1) & (tmpdata$easyP1difficultN1 == 1)], na.rm = T);
}

t.test(mean_choice_lik_prevEasy, mean_choice_lik_prevDiff, paired = T) # p = 0.09, 2/25/24
wilcox.test(mean_choice_lik_prevEasy, mean_choice_lik_prevDiff, paired = T)
# mean choice likelihood is slightly HIGHER after difficult than after easy (0.697 vs. 0.685)

cor.test(mean_choice_lik_prevEasy, mean_choice_lik_prevDiff) # r(59) = 0.33, p = 0.009
cor.test(mean_choice_lik_prevEasy, mean_choice_lik_prevDiff, method = 'spearman')
# Correlated w/ each other, unsurprisingly.

plot(mean_choice_lik_prevEasy, mean_choice_lik_prevDiff)
lines(x = c(0, 1), y = c(0, 1))

# 4. Calculate the difference in mean choice likelihood as a function of prev. difficulty

mean_choice_lik_relative = mean_choice_lik_prevEasy - mean_choice_lik_prevDiff;
# Positive numbers = Likelihood after easy is HIGHER than after difficult.
# Negative numbers = likelihood after easy is LOWER than after difficult.
#
# We predict more positive numbers for low-capacity folks.

# 5. Relate that difference to capacity group and/or continuous capacity.

plot(mean_choice_lik_relative, compositeSpanScores[keep_participants])
cor.test(mean_choice_lik_relative, compositeSpanScores[keep_participants]) # n.s. (p = 0.27) on 2/25/24
cor.test(mean_choice_lik_relative, compositeSpanScores[keep_participants], method = 'spearman')
# Direction of the (non-sig.) correlation is negative, as expected
# Low capacity folks have a greater gap between their choice likelihood
# after easy vs. difficult (and easy is >> difficult).

t.test(mean_choice_lik_relative[capacity_HighP1_lowN1_Best == 1], mean_choice_lik_relative[capacity_HighP1_lowN1_Best == -1])
wilcox.test(mean_choice_lik_relative[capacity_HighP1_lowN1_Best == 1], mean_choice_lik_relative[capacity_HighP1_lowN1_Best == -1])
# n.s. p = 0.63 on 2/25/24
# High cap. mean difference = -0.017
# Low cap. mean difference = -0.011 (more positive)


# Try a regression-based approach to this whole question?
likmodel_catDiff_catCap = lmer(all_choice_likelihood ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev * capacity_HighP1_lowN1_best +
                                 (1 | subjectnumber), data = clean_data_dm)
summary(likmodel_catDiff_catCap)
# no signs of anything going on in previous difficulty

likmodel_contDiff_catCap = lmer(all_choice_likelihood ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                  (1 | subjectnumber), data = clean_data_dm)
summary(likmodel_contDiff_catCap)
# no signs of anything going on in previous difficulty


# TAKEAWAY:
# There are no strong signs in peoples' choices that after difficult trials, choice likelihood
# is relatively lower for low capacity folks than for high capacity folks. That *would* have
# been consistent with 'giving up'. The signs are in the expected direction, but no stats
# reach significance.
#
# Note that no stats are instead consistent with *facilitation* after difficult trials (which
# might predict more internally-consistent choices after difficult trials).

# This analysis likely suffers from too much breaking-into-parts (and any others going down this
# path would suffer more, e.g. is the likelihood lower, on difficult trials only, after difficult
# trials than after easy trials?)

# This analysis is ALSO fundamentally limited by our design - easy and difficult trials are
# DESIGNED with a particular choice likelihood. That's the literal definition of what makes an
# easy trial easy etc. In particular, difficult trials, where we might expect to see the most
# movement, are designed with likelihoods *right in the middle*, so biases to act in a
# particular way on those trials (e.g. always gamble; always safe) might be impossible to see
# as *all actions are equally (un)likely*. If anything, this counterintuitively suggests that
# EASY trials would be the place to observe unusual behavior, but we may have again hurt
# ourselves as easy trials are SO easy as to be obvious, and not requiring much cognitive
# work at all to decide about. In short, the way to have observed this might have been with
# trials that were in-between easy and difficult.



# Pupillometry Analyses #################

tmp_pupil_QA_metrics_fn = dir(pattern = glob2rx(sprintf('cge_pupil_QA_metrics*.csv',s)),full.names = T, recursive = T);
pupil_QA_metrics = read.csv(tmp_pupil_QA_metrics_fn[length(tmp_pupil_QA_metrics_fn)])
keep_subj_pupil = as.logical(pupil_QA_metrics$keep_subj_pupil[keep_participants]); # focusing on those individuals
# we are retaining for behavioral analysis, which do we ALSO retain for pupil analysis

## Characterizing Overall Pupillometric Variability Across Participants ##############################

mean_pupil_dilations = array(dim = c(number_of_clean_subjects)) # mean pupil dilation
sd_pupil_dilations = array(dim = c(number_of_clean_subjects)) # standard deviation
ci_pupil_dilations = array(dim = c(2,number_of_clean_subjects)) # 95% CI

for (s in keep_participants){
  s_index = which(keep_participants == s);

  cat(sprintf('Subject CGE%03i (%i of %i)\n', s, s_index, length(keep_participants)))

  # find their file...
  tmp_downsampled_fn = dir(pattern = glob2rx(sprintf('cge%03i_et_processed_downsampled*.RData',s)),full.names = T, recursive = T);
  # and load only the most recent downsampled data file
  load(tmp_downsampled_fn[length(tmp_downsampled_fn)])
  downsampled_et_data = as.data.frame(downsampled_et_data);

  mean_pupil_dilations[s_index] = mean(downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled, na.rm = T)
  sd_pupil_dilations[s_index] = sd(downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled, na.rm = T)
  ci_pupil_dilations[,s_index] = quantile(downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled, probs = c(0.025, 0.975), na.rm = T)
}

sort_order = order(mean_pupil_dilations)

pdf(sprintf('%s/plots/mean_pupil_diameter_SDbars.pdf',config$path$data$processed),
    width = 5, height = 5)
plot(mean_pupil_dilations[sort_order],
     xlab = 'Participants', ylab = 'Diameter (mm)', main = 'Mean Pupil Diameter by Subject (w/ S.D.)',
     pch = 20, ylim = c(2,7), xaxt = 'n')
segments(x0 = 1:number_of_clean_subjects, x1 = 1:number_of_clean_subjects,
         y0 = mean_pupil_dilations[sort_order] - sd_pupil_dilations[sort_order],
         y1 = mean_pupil_dilations[sort_order] + sd_pupil_dilations[sort_order])
dev.off()

pdf(sprintf('%s/plots/mean_pupil_diameter_95CIbars.pdf',config$path$data$processed),
    width = 5, height = 5)
plot(mean_pupil_dilations[sort_order],
     xlab = 'Participants', ylab = 'Diameter (mm)', main = 'Mean Pupil Diameter by Subject (w/ 95% CIs)',
     pch = 20, ylim = c(2,7), xaxt = 'n')
segments(x0 = 1:number_of_clean_subjects, x1 = 1:number_of_clean_subjects,
         y0 = ci_pupil_dilations[1,sort_order],
         y1 = ci_pupil_dilations[2,sort_order])
dev.off()

# There is in fact quite a bit of variation in mean pupil dilation, and as the
# above graphs make clear, people are at very different levels overall.
# Correcting for this might be unavoidable. There is also variability IN the
# variability (i.e. some people are highly variable, others much less so), and
# that may *also* need correction eventually via Z-scoring.


## Regression on Continuous Pupillometry ################

### Setup #########################

# This analysis will seek to create one *large* pupil dilation array.
# Columns will be different samples during the trial.
# Rows will be distinct trials, across all participants.
#
# Then regressions can be carried out on a per-point basis, yielding moment-by-
# moment regression results of how effects of e.g., current difficulty, previous
# difficulty, WMC, etc interact and change to alter pupil dilation over time.

frequency_of_resampled_points = 40 # bins of 25ms width, 40/second
point_increment = 1/frequency_of_resampled_points*1000 # in units of ms

xvals_window_1 = seq(from = 0, to = 1000, by = point_increment)
xvals_blank = seq(from = 1050, to = 1200, by = point_increment)
xvals_window_2 = seq(from = -500, to = 5000, by = point_increment)

xvals = c(
  xvals_window_1, # the choice option presentation window
  xvals_blank, # the BLANK
  xvals_window_2 # the dec-isi-otc-iti window
)

window_1_ind = 1:length(xvals_window_1)
window_2_ind = (length(xvals_window_1) + length(xvals_blank) + 1):(length(xvals_window_1) + length(xvals_blank) + length(xvals_window_2))

mega_pupil_array = array(data = NA, dim = c(0, length(xvals)))

cat('Creating mega_pupil_array for regression.\n')
### Pupil Array Creation #########################
for (s in keep_participants){
# for (s in 1:2){
  s_index = which(keep_participants == s);

  cat(sprintf('Subject CGE%03i (%i of %i): trial 000', s, s_index, length(keep_participants)))

  # Create NA-filled array to hold this one person's pupil trace data
  mini_pupil_array = array(data = NA, dim = c(170, length(xvals)))

  # find their file...
  tmp_downsampled_fn = dir(pattern = glob2rx(sprintf('cge%03i_et_processed_downsampled*.RData',s)),full.names = T, recursive = T);
  # and load only the most recent downsampled data file
  load(tmp_downsampled_fn[length(tmp_downsampled_fn)]) # loads downsampled_et_data and event_timestamps
  downsampled_et_data = as.data.frame(downsampled_et_data);

  number_of_trials = length(event_timestamps[,1]);
  for (t in 1:number_of_trials){
    cat(sprintf('\b\b\b%03i',t))

    if(!is.na(event_timestamps$decision_start[t])){
      # Do Window 1 (choice option presentation)
      tmp_time_points = event_timestamps$decision_start[t] + xvals_window_1;

      tmp_norm_pupil = approx(x = downsampled_et_data$time_data_downsampled, # use approx to get them
                              y = downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled,
                              xout = tmp_time_points)$y
      mini_pupil_array[t,window_1_ind] = tmp_norm_pupil # store in the mini array

      # Do Window 2 (choice option presentation)
      tmp_time_points = event_timestamps$decision_end[t] + xvals_window_2;

      tmp_norm_pupil = approx(x = downsampled_et_data$time_data_downsampled, # use approx to get them
                              y = downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled,
                              xout = tmp_time_points)$y
      mini_pupil_array[t,window_2_ind] = tmp_norm_pupil # store in the mini array
    }
  }
  mega_pupil_array = rbind(mega_pupil_array, mini_pupil_array);
  cat(sprintf('. Done.\n'))
}
cat('Mega pupil array created')

### Carry out regressions #########################
# We'll use clean_data_dm as the source of regressors.

clean_data_dm$choice_riskyP1_safeN1 = clean_data_dm$choice*2-1
clean_data_dm$riskywinP1_loseN1 = clean_data_dm$choice*(  # on trials where the risky option was chosen...
  1*(clean_data_dm$outcome == clean_data_dm$riskyopt1) -  # +1 when they 'won'
  1*(clean_data_dm$outcome == clean_data_dm$riskyopt2))   # -1 when they 'lost'
# Because the mean probability of winning was slightly *below* 0.5 (0.4896),
# it's technically possible to *predict* on a per trial basis that participants
# are more likely to lose than win, and when used as-is, this regressor
# is capturing a small slice of risk-taking activity in its non-zero mean
# value. Regressor must be DEMEANED to handle this slight bias.
clean_data_dm$riskywinP1_loseN1_DM = clean_data_dm$riskywinP1_loseN1 - mean(clean_data_dm$riskywinP1_loseN1, na.rm = T)

reg_formula_full = as.formula("mega_pupil_array[,timepoint] ~ 1 +
trialnumberRS + choice_riskyP1_safeN1 + riskywinP1_loseN1 +
all_diff_cont*capacity_HighP1_lowN1_best + all_diff_cont * trialnumberRS +
prev_all_diff_cont*capacity_HighP1_lowN1_best + prev_all_diff_cont * trialnumberRS +
(1 | subjectnumber)")

reg_all_terms = labels(terms(reg_formula_full)) # this contains the RFX terms, which we don't want
keep_reg_terms = !grepl("subjectnumber", reg_all_terms) # find the RFX term...
reg_all_terms = c('intercept',reg_all_terms[keep_reg_terms]) #... and remove it and add the intercept term

beta_vals = array(data = NA, dim = c(length(xvals),length(reg_all_terms)))
beta_vals = as.data.frame(beta_vals);
colnames(beta_vals) <- reg_all_terms;

p_vals = beta_vals

cat('Beginning regressions on mega_pupil_array\n')
cat(sprintf('000/%03i',length(xvals)))
for (timepoint in 1:length(xvals)){
  cat(sprintf('\b\b\b\b\b\b\b%03i/%03i',timepoint, length(xvals)))
  if (all(is.na(mega_pupil_array[,timepoint]))){
    next
  } else{
    tmp_model = lmer(reg_formula_full,data = clean_data_dm)
    tmp_summ = summary(tmp_model)
    beta_vals[timepoint,] = coef(tmp_summ)[,1]
    p_vals[timepoint,] = coef(tmp_summ)[,5]
  }
}

p_vals_reconfig = 1-p_vals;
p_vals_reconfig[(beta_vals < 0)&(!is.na(beta_vals))] = -p_vals_reconfig[(beta_vals < 0)&(!is.na(beta_vals))];
# +1 = significant, positive, corresponds w/ p = 0, pos. beta
# -1 = significant, negative, corresponds w/ p = 0, neg. beta

# plot(p_vals_reconfig$prev_all_diff_contXcapacity_HighP1_lowN1_best,
#      type = 'l', ylim = c(-1,1))
# abline(h = 0.95, lty = 'dashed')
# abline(h = -0.95, lty = 'dashed')

deconv_pval_threshold = 0.05 # the p-value at which to threshold the visualization

# turn a p-value threshold into the appropriate coloring
n_pval_levels = 1e8; # think of this as the resolution of p-value thresholding
n_deconv_pval_sig_levels = (n_pval_levels/2)*deconv_pval_threshold;

if(n_deconv_pval_sig_levels%%1!=0){
  warning('p-value selection does not result in a whole integer value')
}

color_palette_for_pval_reconfig = rev(c(
  colorRampPalette(c("#FFFF00","#FF0000"))(n_deconv_pval_sig_levels),
  rep("#000000",n_pval_levels-2*n_deconv_pval_sig_levels),
  colorRampPalette(c("#0000FF","#12fcca"))(n_deconv_pval_sig_levels)
))

par(mar=c(5,20,4,2))
image(as.matrix(p_vals_reconfig), col = color_palette_for_pval_reconfig, axes=F)
axis(2, at=seq(0,1,length=length(reg_all_terms)), labels = reg_all_terms, lwd=0, las=1)
axis(1, seq(from=0,to=1,length.out=25), labels = xvals[round(seq(from=1,to=length(xvals),length.out=25))])
abline(v = which(xvals == 0)[2]/length(xvals), col = 'magenta', lwd = 3, lty = 'dotted')
abline(v = which(xvals == 1000)[2]/length(xvals), col = 'purple', lwd = 3, lty = 'dashed')
abline(v = which(xvals == 2000)/length(xvals), col = 'purple', lwd = 3, lty = 'dashed')


par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(beta_vals$choice_riskyP1_safeN1 + beta_vals$riskywinP1_loseN1, ylim = c(-0.1,0.1));
lines(beta_vals$choice_riskyP1_safeN1 - beta_vals$riskywinP1_loseN1, lwd = 2)
lines(-beta_vals$choice_riskyP1_safeN1,col = 'red', lwd = 2)
abline(v = which(xvals == 0)[2], col = 'magenta', lwd = 3, lty = 'dotted')
abline(h = 0, col = 'black', lwd = 2)
abline(v = which(xvals == 1000)[2], col = 'purple', lwd = 3, lty = 'dashed')
abline(v = which(xvals == 2000), col = 'purple', lwd = 3, lty = 'dashed')
legend('topleft',legend = c('risky win', 'risky loss', 'safe'))

# Need to ...
#   1. decide on a better regression
#   2. Re-run said regression
#   3. Work on visualization of betas and/or p-values


## Plotting Downsampled Pupillometry ####################
### Per-Subject Plots ###########################

baseline_window_width = 500;
dec_isi_otc_iti_window_width = 5000; # ITIs that were 3s long end at 5s after dec; other ITIs were 3.5s long
pre_dec_window_width = 1500;

bin_increment = 50; # ensure bins increment by multiples of 25ms

number_of_normBins = 200 ; # number of points for the normalized decision window plot
normBins = seq(from = 1, to = 200, by = 1)

decision_start_bins = seq(from = -baseline_window_width, to = 3000, by = bin_increment);
decision_end_bins = seq(from = -3000, to = baseline_window_width, by = bin_increment);
dec_isi_otc_iti_bins = seq(from = -pre_dec_window_width, to = 5000, by = bin_increment);

dec_isi_otc_iti_array = array(data = NA, dim = c(170, length(dec_isi_otc_iti_bins)-1, number_of_clean_subjects)) # trials x bins x subjects # focusing on first for normalizing

# overall arrays
mean_decision_start_array = array(data = NA, dim = c(length(decision_start_bins)-1,number_of_clean_subjects))
mean_decision_end_array = array(data = NA, dim = c(length(decision_end_bins)-1,number_of_clean_subjects))
mean_dec_isi_otc_iti_array = array(data = NA, dim = c(length(dec_isi_otc_iti_bins)-1,number_of_clean_subjects))

# easy/difficult arrays # not doing yet for the normalized
# TRIAL-level
decision_start_EvD_array = array(data = NA, dim = c(60, length(decision_start_bins)-1, number_of_clean_subjects, 2)) # trials x bins x subjects x Easy/Difficult
decision_start_RvS_array = array(data = NA, dim = c(60, length(decision_start_bins)-1, number_of_clean_subjects, 2))
decision_start_EvD_RvS_array = array(data = NA, dim = c(60, length(decision_start_bins)-1, number_of_clean_subjects, 2, 2))
decision_start_prev_EvD_array = array(data = NA, dim = c(60, length(decision_start_bins)-1, number_of_clean_subjects, 2, 2))
decision_start_WMC_array = array(data = NA, dim = c(60, length(decision_start_bins)-1, number_of_clean_subjects, 2))
decision_start_EvD_WMC_array = array(data = NA, dim = c(60, length(decision_start_bins)-1, number_of_clean_subjects, 2, 2))
decision_start_prev_EvD_WMC_array = array(data = NA, dim = c(60, length(decision_start_bins)-1, number_of_clean_subjects, 2, 2, 2))
dec_isi_otc_iti_EvD_array = array(data = NA, dim = c(60, length(dec_isi_otc_iti_bins)-1, number_of_clean_subjects, 2)) # trials x bins x subjects x Easy/Difficult
dec_isi_otc_iti_RvS_array = array(data = NA, dim = c(60, length(dec_isi_otc_iti_bins)-1, number_of_clean_subjects, 2))
dec_isi_otc_iti_EvD_RvS_array = array(data = NA, dim = c(60, length(dec_isi_otc_iti_bins)-1, number_of_clean_subjects, 2, 2))
dec_isi_otc_iti_prev_EvD_array = array(data = NA, dim = c(60, length(dec_isi_otc_iti_bins)-1, number_of_clean_subjects, 2, 2))
dec_isi_otc_iti_WMC_array = array(data = NA, dim = c(60, length(dec_isi_otc_iti_bins)-1, number_of_clean_subjects, 2))
dec_isi_otc_iti_EvD_WMC_array = array(data = NA, dim = c(60, length(dec_isi_otc_iti_bins)-1, number_of_clean_subjects, 2, 2))
dec_isi_otc_iti_prev_EvD_WMC_array = array(data = NA, dim = c(60, length(dec_isi_otc_iti_bins)-1, number_of_clean_subjects, 2, 2, 2))
# There are 120 dynamic trials: 60 easy & 60 difficult
# There is one less trial for either a previous easy or difficult: This is for the 1st trial
# There is uneven amount of risky v. safe choices:
### People will choose based on their risk preferences
### While easy accept and reject are bimodally split, difficult choices are not because it is at the indifference point

# SUBJECT-level
mean_decision_start_EvD_array = array(data = NA, dim = c(length(decision_start_bins)-1,number_of_clean_subjects, 2)) # ... by 2 for easy/difficult
mean_decision_start_RvS_array = array(data = NA, dim = c(length(decision_start_bins)-1,number_of_clean_subjects, 2))
mean_decision_start_EvD_RvS_array = array(data = NA, dim = c(length(decision_start_bins)-1,number_of_clean_subjects, 2, 2))
mean_decision_start_prev_EvD_array = array(data = NA, dim = c(length(decision_start_bins)-1,number_of_clean_subjects, 2, 2))
mean_decision_start_WMC_array = array(data = NA, dim = c(length(decision_start_bins)-1,number_of_clean_subjects, 2))
mean_decision_start_EvD_WMC_array = array(data = NA, dim = c(length(decision_start_bins)-1,number_of_clean_subjects, 2, 2))
mean_decision_start_prev_EvD_WMC_array = array(data = NA, dim = c(length(decision_start_bins)-1,number_of_clean_subjects, 2, 2, 2))
mean_dec_isi_otc_iti_EvD_array = array(data = NA, dim = c(length(dec_isi_otc_iti_bins)-1,number_of_clean_subjects, 2)) # ... by 2 for easy/difficult
mean_dec_isi_otc_iti_RvS_array = array(data = NA, dim = c(length(dec_isi_otc_iti_bins)-1,number_of_clean_subjects, 2))
mean_dec_isi_otc_iti_EvD_RvS_array = array(data = NA, dim = c(length(dec_isi_otc_iti_bins)-1,number_of_clean_subjects, 2, 2))
mean_dec_isi_otc_iti_prev_EvD_array = array(data = NA, dim = c(length(dec_isi_otc_iti_bins)-1,number_of_clean_subjects, 2, 2))
mean_dec_isi_otc_iti_WMC_array = array(data = NA, dim = c(length(dec_isi_otc_iti_bins)-1,number_of_clean_subjects, 2))
mean_dec_isi_otc_iti_EvD_WMC_array = array(data = NA, dim = c(length(dec_isi_otc_iti_bins)-1,number_of_clean_subjects, 2, 2))
mean_dec_isi_otc_iti_prev_EvD_WMC_array = array(data = NA, dim = c(length(dec_isi_otc_iti_bins)-1,number_of_clean_subjects, 2, 2, 2))

# Normalized Bin Arrays
decision_norm_array = array(data = NA, dim = c(170, number_of_normBins, number_of_clean_subjects)) # all trials
decision_norm_EvD_array = array(data = NA, dim = c(60, number_of_normBins, number_of_clean_subjects, 2)) # trial-level (choice difficulty)
decision_norm_EvD_RvS_array = array(data = NA, dim = c(60, number_of_normBins, number_of_clean_subjects, 2, 2)) # trial-level (choice difficulty x choice made)
decision_norm_prev_EvD_array = array(data = NA, dim = c(60, number_of_normBins, number_of_clean_subjects, 2, 2)) # trial-level (choice difficulty x previous choice difficulty)
decision_norm_WMC_array = array(data = NA, dim = c(60, number_of_normBins, number_of_clean_subjects, 2)) # WMC
decision_norm_EvD_WMC_array = array(data = NA, dim = c(60, number_of_normBins, number_of_clean_subjects, 2, 2))
decision_norm_prev_EvD_WMC_array = array(data = NA, dim = c(60, number_of_normBins, number_of_clean_subjects, 2, 2, 2))
mean_decision_norm_array = array(data = NA, dim = c(number_of_normBins, number_of_clean_subjects)) # all trials
mean_decision_norm_EvD_array = array(data = NA, dim = c(number_of_normBins, number_of_clean_subjects, 2)) # subject-level (choice difficulty)
mean_decision_norm_EvD_RvS_array = array(data = NA, dim = c(number_of_normBins, number_of_clean_subjects, 2, 2)) # subject-level (choice difficulty x choice made)
mean_decision_norm_prev_EvD_array = array(data = NA, dim = c(number_of_normBins, number_of_clean_subjects, 2, 2)) # subject-level (choice difficulty x previous choice difficulty)
mean_decision_norm_WMC_array = array(data = NA, dim = c(number_of_normBins, number_of_clean_subjects, 2)) # WMC
mean_decision_norm_EvD_WMC_array = array(data = NA, dim = c(number_of_normBins, number_of_clean_subjects, 2, 2)) #
mean_decision_norm_prev_EvD_WMC_array = array(data = NA, dim = c(number_of_normBins, number_of_clean_subjects, 2, 2, 2)) #


for (s in keep_participants){
  s_index = which(keep_participants == s);

  cat(sprintf('Subject CGE%03i (%i of %i): trial 000', s, s_index, length(keep_participants)))

  tmpdata = clean_data_dm[clean_data_dm$subjectnumber == s,]; # defines this person's BEHAVIORAL data

  # Create NA-filled arrays to hold this one person's pupil trace data
  decision_start_array = array(data = NA, dim = c(170, length(decision_start_bins)-1)) # what does the -1 do again?
  decision_end_array = array(data = NA, dim = c(170, length(decision_end_bins)-1))

  # find their file...
  tmp_downsampled_fn = dir(pattern = glob2rx(sprintf('cge%03i_et_processed_downsampled*.RData',s)),full.names = T, recursive = T);
  # and load only the most recent downsampled data file
  load(tmp_downsampled_fn[length(tmp_downsampled_fn)]) # loads downsampled_et_data and event_timestamps
  downsampled_et_data = as.data.frame(downsampled_et_data);

  # Baseline correct all ET data to the MEAN PUPIL DIAMETER
  downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled = downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled -
    mean(downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled, na.rm = T);

  # Prep Subject-Level Decision Plot
  pdf(sprintf('%s/plots/cge%03i_downsampled_decision_plot.pdf',config$path$data$processed, s),
      width = 5, height = 8)

  par(mfrow = c(2,1)); # Set up the individual-level plot
  # Pre-decision | Decision Start
  # Decision End | ISI | Outcome | ITI
  plot(1, type = "n", xlab = "milliseconds", ylab = "demeaned pupil diameter (mm)", main = "Aligned to Decision Window Start", # I don't see the graph
       xlim = c(-baseline_window_width, 3000), ylim = c(-2, 2))
  abline(v = 0, lty = 'dashed')
  p1_coords = par('usr');
  # pre-dec window, up until 3000 ms into the 4000ms response window

  plot(1, type = "n", xlab = "milliseconds", ylab = "demeaned pupil diameter (mm)", main = "Aligned to Choice",
       xlim = c(-3000, baseline_window_width), ylim = c(-2, 2))
  abline(v = 0, lty = 'dotted')
  p2_coords = par('usr');
  # the last 3000ms of the 4000ms response window, ISI (1000), Otc (1000), and ITI (3000 or 3500ms)

  number_of_trials = length(event_timestamps[,1]);
  cum_easy_trial_num = 0; # for use in indexing; will increment
  cum_diff_trial_num = 0;

  for (t in 1:number_of_trials){
    cat(sprintf('\b\b\b%03i',t))
    # Pre-decision baseline
    indices = (downsampled_et_data$time_data_downsampled >= (event_timestamps$decision_start[t] - baseline_window_width)) &
      (downsampled_et_data$time_data_downsampled < event_timestamps$decision_start[t])
    pupil_tmp = downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled[indices];
    time_tmp = downsampled_et_data$time_data_downsampled[indices] - event_timestamps$decision_start[t];
    par(usr = p1_coords)
    par(mfg = c(1,1)); lines(x = time_tmp, y = pupil_tmp, col = rgb(0,0,0,.05), lwd = 3)

    # Put the mean values into the bins
    for (b in 1:(length(decision_start_bins)-1)){
      tmp_bin_mean = mean(pupil_tmp[(time_tmp >= decision_start_bins[b]) & (time_tmp < decision_start_bins[b+1])], na.rm = T);
      if (!is.na(tmp_bin_mean)){
        decision_start_array[t,b] = tmp_bin_mean;
      }
    }

    # Decision (mean)
    indices = (downsampled_et_data$time_data_downsampled >= event_timestamps$decision_start[t]) &
      (downsampled_et_data$time_data_downsampled < event_timestamps$decision_end[t]);
    pupil_tmp = downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled[indices];
    time_tmp = downsampled_et_data$time_data_downsampled[indices] - event_timestamps$decision_start[t];
    par(usr = p1_coords)
    par(mfg = c(1,1)); lines(x = time_tmp, y = pupil_tmp, col = rgb(0,0,0,.05), lwd = 3)

    # Put the mean values into the bins
    for (b in 1:(length(decision_start_bins)-1)){
      tmp_bin_mean = mean(pupil_tmp[(time_tmp >= decision_start_bins[b]) & (time_tmp < decision_start_bins[b+1])], na.rm = T);
      if (!is.na(tmp_bin_mean)){
        decision_start_array[t,b] = tmp_bin_mean;
      }
    }

    # Decision aligned to CHOICE (mean)
    indices = (downsampled_et_data$time_data_downsampled >= event_timestamps$decision_start[t]) &
      (downsampled_et_data$time_data_downsampled < event_timestamps$decision_end[t]);
    pupil_tmp = downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled[indices];
    time_tmp = downsampled_et_data$time_data_downsampled[indices] - event_timestamps$decision_end[t];
    par(usr = p2_coords)
    par(mfg = c(2,1)); lines(x = time_tmp, y = pupil_tmp, col = rgb(0,0,0,.05), lwd = 3)

    # Put the mean values into the bins
    for (b in 1:(length(decision_end_bins)-1)){
      tmp_bin_mean = mean(pupil_tmp[(time_tmp >= decision_end_bins[b]) & (time_tmp < decision_end_bins[b+1])], na.rm = T);
      if (!is.na(tmp_bin_mean)){
        decision_end_array[t,b] = tmp_bin_mean;
      }
    }


    # Post-decision aligned to CHOICE (mean)
    indices = (downsampled_et_data$time_data_downsampled >= event_timestamps$decision_end[t]) &
      (downsampled_et_data$time_data_downsampled < (event_timestamps$decision_end[t] + baseline_window_width));
    pupil_tmp = downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled[indices];
    time_tmp = downsampled_et_data$time_data_downsampled[indices] - event_timestamps$decision_end[t];
    par(usr = p2_coords)
    par(mfg = c(2,1)); lines(x = time_tmp, y = pupil_tmp, col = rgb(0,0,0,.05), lwd = 3)

    # Put the mean values into the bins
    for (b in 1:(length(decision_end_bins)-1)){
      tmp_bin_mean = mean(pupil_tmp[(time_tmp >= decision_end_bins[b]) & (time_tmp < decision_end_bins[b+1])], na.rm = T);
      if (!is.na(tmp_bin_mean)){
        decision_end_array[t,b] = tmp_bin_mean;
      }
    }

    # Decision Start-Aligned x EASY/DIFF
    indices = (downsampled_et_data$time_data_downsampled >= (event_timestamps$decision_start[t] - baseline_window_width)) &
      (downsampled_et_data$time_data_downsampled < event_timestamps$decision_end[t])
    pupil_tmp = downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled[indices];
    time_tmp = downsampled_et_data$time_data_downsampled[indices] - event_timestamps$decision_start[t];
    # par(usr = p1_coords)
    # par(mfg = c(1,1)); lines(x = time_tmp, y = pupil_tmp, col = rgb(0,0,0,.05), lwd = 3)

    # Put the mean values into the bins
    if(tmpdata$easyP1difficultN1[t] == 1) {
      cum_easy_trial_num = cum_easy_trial_num + 1;
      for (b in 1:(length(decision_start_bins)-1)){
        tmp_bin_mean = mean(pupil_tmp[(time_tmp >= decision_start_bins[b]) & (time_tmp < decision_start_bins[b+1])], na.rm = T);
        if (!is.na(tmp_bin_mean)){
          decision_start_EvD_array[cum_easy_trial_num, b, s_index, 1] = tmp_bin_mean; # trials x bins x subjects x Easy/Difficult
          if(tmpdata$choice[t] == 1) {
            decision_start_EvD_RvS_array[cum_easy_trial_num, b,s_index,1, 1] = tmp_bin_mean; # trials x bins x subjects x Easy x Risky
          }
          else if (tmpdata$choice[t] == 0) {
            decision_start_EvD_RvS_array[cum_easy_trial_num, b,s_index,1, 2] = tmp_bin_mean # trials x bins x subjects x Easy x Safe
          }
          if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
            if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
              decision_start_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
            }
            else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
              decision_start_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
            }
          }
          if(tmpdata$easyP1difficultN1_prev[t] == 1) {
            decision_start_prev_EvD_array[cum_easy_trial_num, b,s_index,1, 1] = tmp_bin_mean # trials x bins x subjects x Easy x Prev Easy
            if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
              if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
                decision_start_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 1, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
              }
              else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
                decision_start_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 1, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
              }
            }
          }
          else if (tmpdata$easyP1difficultN1_prev[t] == -1) {
            decision_start_prev_EvD_array[cum_easy_trial_num, b,s_index,1, 2] = tmp_bin_mean # trials x bins x subjects x Easy x Prev Diff
            if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
              if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
                decision_start_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 2, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
              }
              else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
                decision_start_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 2, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
              }
            }
          }
        }
      }
    }
    else if (tmpdata$easyP1difficultN1[t] == -1) {
      cum_diff_trial_num = cum_diff_trial_num + 1;
      for (b in 1:(length(decision_start_bins)-1)){
        tmp_bin_mean = mean(pupil_tmp[(time_tmp >= decision_start_bins[b]) & (time_tmp < decision_start_bins[b+1])], na.rm = T);
        if (!is.na(tmp_bin_mean)){
          decision_start_EvD_array[cum_diff_trial_num, b, s_index, 2] = tmp_bin_mean; # trials x bins x subjects x Easy/Difficult
          if(tmpdata$choice[t] == 1) {
            decision_start_EvD_RvS_array[cum_diff_trial_num, b,s_index,2, 1] = tmp_bin_mean; # trials x bins x subjects x Easy x Risky
          }
          else if (tmpdata$choice[t] == 0) {
            decision_start_EvD_RvS_array[cum_diff_trial_num, b,s_index,2, 2] = tmp_bin_mean # trials x bins x subjects x Easy x Safe
          }
          if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
            if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
              decision_start_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
            }
            else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
              decision_start_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
            }
          }
          if(tmpdata$easyP1difficultN1_prev[t] == 1) {
            decision_start_prev_EvD_array[cum_diff_trial_num, b,s_index,2, 1] = tmp_bin_mean # trials x bins x subjects x Easy x Prev Easy
            if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
              if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
                decision_start_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 1, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
              }
              else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
                decision_start_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 1, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
              }
            }
          }
          else if (tmpdata$easyP1difficultN1_prev[t] == -1) {
            decision_start_prev_EvD_array[cum_diff_trial_num, b,s_index,2, 2] = tmp_bin_mean # trials x bins x subjects x Easy x Prev Diff
            if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
              if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
                decision_start_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 2, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
              }
              else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
                decision_start_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 2, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
              }
            }
          }
        }
      }
    }

      # Dec/ISI/Otc/ITI
      indices = (downsampled_et_data$time_data_downsampled >= (event_timestamps$decision_end[t] - pre_dec_window_width)) &
        (downsampled_et_data$time_data_downsampled < (event_timestamps$decision_end[t] + dec_isi_otc_iti_window_width));
      pupil_tmp = downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled[indices];
      time_tmp = downsampled_et_data$time_data_downsampled[indices] - event_timestamps$decision_end[t];
      # par(mfg = c(2,1)); lines(x = time_tmp, y = pupil_tmp, col = rgb(0,0,0,.05), lwd = 3)

      # Put the mean values into the bins
      for (b in 1:(length(dec_isi_otc_iti_bins)-1)){
        tmp_bin_mean = mean(pupil_tmp[(time_tmp >= dec_isi_otc_iti_bins[b]) & (time_tmp < dec_isi_otc_iti_bins[b+1])], na.rm = T);
        if (!is.na(tmp_bin_mean)){
          dec_isi_otc_iti_array[t,b,s_index] = tmp_bin_mean;
        }
      }

      # Put the mean values into the bins
      if(tmpdata$easyP1difficultN1[t] == 1) {
        for (b in 1:(length(dec_isi_otc_iti_bins)-1)){
          tmp_bin_mean = mean(pupil_tmp[(time_tmp >= dec_isi_otc_iti_bins[b]) & (time_tmp < dec_isi_otc_iti_bins[b+1])], na.rm = T);
          if (!is.na(tmp_bin_mean)){
            dec_isi_otc_iti_EvD_array[cum_easy_trial_num, b, s_index, 1] = tmp_bin_mean; # trials x bins x subjects x Easy/Difficult
            if(tmpdata$choice[t] == 1) {
              dec_isi_otc_iti_EvD_RvS_array[cum_easy_trial_num, b,s_index,1, 1] = tmp_bin_mean; # trials x bins x subjects x Easy x Risky
            }
            else if (tmpdata$choice[t] == 0) {
              dec_isi_otc_iti_EvD_RvS_array[cum_easy_trial_num, b,s_index,1, 2] = tmp_bin_mean # trials x bins x subjects x Easy x Safe
            }
            if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
              if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
                dec_isi_otc_iti_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
              }
              else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
                dec_isi_otc_iti_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
              }
            }
            if(tmpdata$easyP1difficultN1_prev[t] == 1) {
              dec_isi_otc_iti_prev_EvD_array[cum_easy_trial_num, b,s_index,1, 1] = tmp_bin_mean # trials x bins x subjects x Easy x Prev Easy
              if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
                if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
                  dec_isi_otc_iti_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 1, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
                }
                else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
                  dec_isi_otc_iti_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 1, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
                }
              }
            }
            else if (tmpdata$easyP1difficultN1_prev[t] == -1) {
              dec_isi_otc_iti_prev_EvD_array[cum_easy_trial_num, b,s_index,1, 2] = tmp_bin_mean # trials x bins x subjects x Easy x Prev Diff
              if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
                if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
                  dec_isi_otc_iti_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 2, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
                }
                else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
                  dec_isi_otc_iti_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,1, 2, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
                }
              }
            }
          }
        }
      }
      else if (tmpdata$easyP1difficultN1[t] == -1) {
        for (b in 1:(length(dec_isi_otc_iti_bins)-1)){
          tmp_bin_mean = mean(pupil_tmp[(time_tmp >= dec_isi_otc_iti_bins[b]) & (time_tmp < dec_isi_otc_iti_bins[b+1])], na.rm = T);
          if (!is.na(tmp_bin_mean)){
            dec_isi_otc_iti_EvD_array[cum_diff_trial_num, b, s_index, 2] = tmp_bin_mean; # trials x bins x subjects x Easy/Difficult
            if(tmpdata$choice[t] == 1) {
              dec_isi_otc_iti_EvD_RvS_array[cum_diff_trial_num, b,s_index,2, 1] = tmp_bin_mean; # trials x bins x subjects x Easy x Risky
            }
            else if (tmpdata$choice[t] == 0) {
              dec_isi_otc_iti_EvD_RvS_array[cum_diff_trial_num, b,s_index,2, 2] = tmp_bin_mean # trials x bins x subjects x Easy x Safe
            }
            if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
              if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
                dec_isi_otc_iti_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
              }
              else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
                dec_isi_otc_iti_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
              }
            }
            if(tmpdata$easyP1difficultN1_prev[t] == 1) {
              dec_isi_otc_iti_prev_EvD_array[cum_diff_trial_num, b,s_index,2, 1] = tmp_bin_mean # trials x bins x subjects x Easy x Prev Easy
              if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
                if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
                  dec_isi_otc_iti_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 1, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
                }
                else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
                  dec_isi_otc_iti_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 1, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
                }
              }
            }
            else if(tmpdata$easyP1difficultN1_prev[t] == -1) {
              dec_isi_otc_iti_prev_EvD_array[cum_diff_trial_num, b,s_index,2, 2] = tmp_bin_mean # trials x bins x subjects x Easy x Prev Easy
              if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
                if(tmpdata$capacity_HighP1_lowN1_best[t] == -1) {
                  dec_isi_otc_iti_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 2, 1] = tmp_bin_mean # trials x bins x subjects x Easy x low WMC
                }
                else if (tmpdata$capacity_HighP1_lowN1_best[t] == 1) {
                  dec_isi_otc_iti_prev_EvD_WMC_array[cum_easy_trial_num, b,s_index,2, 2, 2] = tmp_bin_mean # trials x bins x subjects x Easy x high WMC
                }
              }
            }
          }
        }
      }

      # NORMALIZED Decision
      if(!is.na(event_timestamps$decision_start[t])){
        tmp_norm_time_points = seq(from = event_timestamps$decision_start[t], # set up the normalized time slices we want
                                   to = event_timestamps$decision_end[t],
                                   length.out = number_of_normBins)
        tmp_norm_pupil = approx(x = downsampled_et_data$time_data_downsampled, # use approx to get them
                                y = downsampled_et_data$pupil_data_extend_interp_smooth_mm_downsampled,
                                xout = tmp_norm_time_points)$y
        decision_norm_array[t,,s_index] = tmp_norm_pupil # store in the normalized array

        current_difficulty_index = abs((tmpdata$easyP1difficultN1[t]-1)/2)+1; # easy --> 1, difficult --> 2
        current_choice_index = abs(tmpdata$choice[t]-2); # risky --> 1, safe --> 2
        previous_difficulty_index = abs((tmpdata$easyP1difficultN1_prev[t]-1)/2)+1; # easy --> 1, difficult --> 2
        wmc_index = abs(((tmpdata$capacity_HighP1_lowN1_best[t]*-1)-1)/2)+1; # low WMC --> 1, high WMC --> 2

        if((tmpdata$static0dynamic1[t] == 1) & (!is.na(tmpdata$choice[t]))){
          if(tmpdata$easyP1difficultN1[t] == 1) { # CURRENT EASY
            cum_easyDiff_trial_num = cum_easy_trial_num
          }
          else if (tmpdata$easyP1difficultN1[t] == -1) { # CURRENT DIFFICULT
            cum_easyDiff_trial_num = cum_diff_trial_num
          }

          # Use trial-wise indices to place tmp_norm_pupil into the right spot
          decision_norm_EvD_array[cum_easyDiff_trial_num,,s_index,current_difficulty_index] = tmp_norm_pupil;# trials x bins x subjects x Easy
          decision_norm_EvD_RvS_array[cum_easyDiff_trial_num,,s_index,current_difficulty_index, current_choice_index] = tmp_norm_pupil; # trials x bins x subjects x Easy x Risky

          if(!is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
            decision_norm_EvD_WMC_array[cum_easyDiff_trial_num,,s_index,current_difficulty_index, wmc_index] = tmp_norm_pupil; # trials x bins x subjects x Easy x WMC
          }

          if(tmpdata$easyP1difficultN1_prev[t] != 0 & !is.na(tmpdata$capacity_HighP1_lowN1_best[t])){
            decision_norm_prev_EvD_array[cum_easyDiff_trial_num,,s_index,current_difficulty_index,previous_difficulty_index] = tmp_norm_pupil # trials x bins x subjects x Easy x Prev Easy
            decision_norm_prev_EvD_WMC_array[cum_easyDiff_trial_num,,s_index,current_difficulty_index,previous_difficulty_index,wmc_index] = tmp_norm_pupil # trials x bins x subjects x Easy x Prev Easy x WMC

          }
        }
      }
    } # end trial loop


    par(usr = p1_coords)
    par(mfg = c(1,1)); lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
                             y = colMeans(decision_start_array, na.rm = T), col = rgb(1,0,0), lwd = 3)

    par(usr = p2_coords)
    par(mfg = c(2,1)); lines(x = decision_end_bins[1:(length(decision_end_bins)-1)] + bin_increment/2,
                             y = colMeans(decision_end_array, na.rm = T), col = rgb(1,0,0), lwd = 3)

    dev.off() # complete the plot

    # all
    mean_decision_start_array[,s_index] = colMeans(decision_start_array, na.rm = T)
    mean_decision_end_array[,s_index] = colMeans(decision_end_array, na.rm = T)
    mean_dec_isi_otc_iti_array[,s_index] = colMeans(dec_isi_otc_iti_array[,,s_index], na.rm = T)

    # decision_start
    mean_decision_start_EvD_array[,s_index,1] = colMeans(decision_start_EvD_array[,,s_index,1], na.rm = T) # Current Difficulty
    mean_decision_start_EvD_array[,s_index,2] = colMeans(decision_start_EvD_array[,,s_index,2], na.rm = T)
      mean_decision_start_EvD_RvS_array[,s_index,1,1] = colMeans(decision_start_EvD_RvS_array[,,s_index,1,1], na.rm = T) # Current Difficulty x Choice Made
      mean_decision_start_EvD_RvS_array[,s_index,1,2] = colMeans(decision_start_EvD_RvS_array[,,s_index,1,2], na.rm = T)
      mean_decision_start_EvD_RvS_array[,s_index,2,1] = colMeans(decision_start_EvD_RvS_array[,,s_index,2,1], na.rm = T)
      mean_decision_start_EvD_RvS_array[,s_index,2,2] = colMeans(decision_start_EvD_RvS_array[,,s_index,2,2], na.rm = T)
        mean_decision_start_prev_EvD_array[,s_index,1,1] = colMeans(decision_start_prev_EvD_array[,,s_index,1,1], na.rm = T) # Previous Difficulty x Current Difficulty
        mean_decision_start_prev_EvD_array[,s_index,1,2] = colMeans(decision_start_prev_EvD_array[,,s_index,1,2], na.rm = T)
        mean_decision_start_prev_EvD_array[,s_index,2,1] = colMeans(decision_start_prev_EvD_array[,,s_index,2,1], na.rm = T)
        mean_decision_start_prev_EvD_array[,s_index,2,2] = colMeans(decision_start_prev_EvD_array[,,s_index,2,2], na.rm = T)
      mean_decision_start_EvD_WMC_array[,s_index,1,1] = colMeans(decision_start_EvD_WMC_array[,,s_index,1,1], na.rm = T) # Current Difficulty x WMC
      mean_decision_start_EvD_WMC_array[,s_index,1,2] = colMeans(decision_start_EvD_WMC_array[,,s_index,1,2], na.rm = T)
      mean_decision_start_EvD_WMC_array[,s_index,2,1] = colMeans(decision_start_EvD_WMC_array[,,s_index,2,1], na.rm = T)
      mean_decision_start_EvD_WMC_array[,s_index,2,2] = colMeans(decision_start_EvD_WMC_array[,,s_index,2,2], na.rm = T)
        mean_decision_start_prev_EvD_WMC_array[,s_index,1,1,1] = colMeans(decision_start_prev_EvD_WMC_array[,,s_index,1,1,1], na.rm = T) # Previous Difficulty x Current Difficulty x WMC
        mean_decision_start_prev_EvD_WMC_array[,s_index,1,1,2] = colMeans(decision_start_prev_EvD_WMC_array[,,s_index,1,1,2], na.rm = T)
        mean_decision_start_prev_EvD_WMC_array[,s_index,1,2,1] = colMeans(decision_start_prev_EvD_WMC_array[,,s_index,1,2,1], na.rm = T)
        mean_decision_start_prev_EvD_WMC_array[,s_index,1,2,2] = colMeans(decision_start_prev_EvD_WMC_array[,,s_index,1,2,2], na.rm = T)
        mean_decision_start_prev_EvD_WMC_array[,s_index,2,1,1] = colMeans(decision_start_prev_EvD_WMC_array[,,s_index,2,1,1], na.rm = T)
        mean_decision_start_prev_EvD_WMC_array[,s_index,2,1,2] = colMeans(decision_start_prev_EvD_WMC_array[,,s_index,2,1,2], na.rm = T)
        mean_decision_start_prev_EvD_WMC_array[,s_index,2,2,1] = colMeans(decision_start_prev_EvD_WMC_array[,,s_index,2,2,1], na.rm = T)
        mean_decision_start_prev_EvD_WMC_array[,s_index,2,2,2] = colMeans(decision_start_prev_EvD_WMC_array[,,s_index,2,2,2], na.rm = T)
    # dec_isi_otc_iti
    mean_dec_isi_otc_iti_EvD_array[,s_index,1] = colMeans(dec_isi_otc_iti_EvD_array[,,s_index,1], na.rm = T) # Current Difficulty
    mean_dec_isi_otc_iti_EvD_array[,s_index,2] = colMeans(dec_isi_otc_iti_EvD_array[,,s_index,2], na.rm = T)
      mean_dec_isi_otc_iti_EvD_RvS_array[,s_index,1,1] = colMeans(dec_isi_otc_iti_EvD_RvS_array[,,s_index,1,1], na.rm = T) # Current Difficulty x Choice Made
      mean_dec_isi_otc_iti_EvD_RvS_array[,s_index,1,2] = colMeans(dec_isi_otc_iti_EvD_RvS_array[,,s_index,1,2], na.rm = T)
      mean_dec_isi_otc_iti_EvD_RvS_array[,s_index,2,1] = colMeans(dec_isi_otc_iti_EvD_RvS_array[,,s_index,2,1], na.rm = T)
      mean_dec_isi_otc_iti_EvD_RvS_array[,s_index,2,2] = colMeans(dec_isi_otc_iti_EvD_RvS_array[,,s_index,2,2], na.rm = T)
        mean_dec_isi_otc_iti_prev_EvD_array[,s_index,1,1] = colMeans(dec_isi_otc_iti_prev_EvD_array[,,s_index,1,1], na.rm = T) # Previous Difficulty x Current Difficulty
        mean_dec_isi_otc_iti_prev_EvD_array[,s_index,1,2] = colMeans(dec_isi_otc_iti_prev_EvD_array[,,s_index,1,2], na.rm = T)
        mean_dec_isi_otc_iti_prev_EvD_array[,s_index,2,1] = colMeans(dec_isi_otc_iti_prev_EvD_array[,,s_index,2,1], na.rm = T)
        mean_dec_isi_otc_iti_prev_EvD_array[,s_index,2,2] = colMeans(dec_isi_otc_iti_prev_EvD_array[,,s_index,2,2], na.rm = T)
      mean_dec_isi_otc_iti_EvD_WMC_array[,s_index,1,1] = colMeans(dec_isi_otc_iti_EvD_WMC_array[,,s_index,1,1], na.rm = T) # Current Difficulty x WMC
      mean_dec_isi_otc_iti_EvD_WMC_array[,s_index,1,2] = colMeans(dec_isi_otc_iti_EvD_WMC_array[,,s_index,1,2], na.rm = T)
      mean_dec_isi_otc_iti_EvD_WMC_array[,s_index,2,1] = colMeans(dec_isi_otc_iti_EvD_WMC_array[,,s_index,2,1], na.rm = T)
      mean_dec_isi_otc_iti_EvD_WMC_array[,s_index,2,2] = colMeans(dec_isi_otc_iti_EvD_WMC_array[,,s_index,2,2], na.rm = T)
        mean_dec_isi_otc_iti_prev_EvD_WMC_array[,s_index,1,1,1] = colMeans(dec_isi_otc_iti_prev_EvD_WMC_array[,,s_index,1,1,1], na.rm = T) # Previous Difficulty x Current Difficulty x WMC
        mean_dec_isi_otc_iti_prev_EvD_WMC_array[,s_index,1,1,2] = colMeans(dec_isi_otc_iti_prev_EvD_WMC_array[,,s_index,1,1,2], na.rm = T)
        mean_dec_isi_otc_iti_prev_EvD_WMC_array[,s_index,1,2,1] = colMeans(dec_isi_otc_iti_prev_EvD_WMC_array[,,s_index,1,2,1], na.rm = T)
        mean_dec_isi_otc_iti_prev_EvD_WMC_array[,s_index,1,2,2] = colMeans(dec_isi_otc_iti_prev_EvD_WMC_array[,,s_index,1,2,2], na.rm = T)
        mean_dec_isi_otc_iti_prev_EvD_WMC_array[,s_index,2,1,1] = colMeans(dec_isi_otc_iti_prev_EvD_WMC_array[,,s_index,2,1,1], na.rm = T)
        mean_dec_isi_otc_iti_prev_EvD_WMC_array[,s_index,2,1,2] = colMeans(dec_isi_otc_iti_prev_EvD_WMC_array[,,s_index,2,1,2], na.rm = T)
        mean_dec_isi_otc_iti_prev_EvD_WMC_array[,s_index,2,2,1] = colMeans(dec_isi_otc_iti_prev_EvD_WMC_array[,,s_index,2,2,1], na.rm = T)
        mean_dec_isi_otc_iti_prev_EvD_WMC_array[,s_index,2,2,2] = colMeans(dec_isi_otc_iti_prev_EvD_WMC_array[,,s_index,2,2,2], na.rm = T)

    # NORMALIZED Mean Data Arrays
    mean_decision_norm_array[,s_index] = colMeans(decision_norm_array[,,s_index], na.rm = T) # All
      mean_decision_norm_EvD_array[,s_index,1] = colMeans(decision_norm_EvD_array[,,s_index,1], na.rm = T) # Current Difficulty
      mean_decision_norm_EvD_array[,s_index,2] = colMeans(decision_norm_EvD_array[,,s_index,2], na.rm = T)
        mean_decision_norm_EvD_RvS_array[,s_index,1,1] = colMeans(decision_norm_EvD_RvS_array[,,s_index,1,1], na.rm = T) # Current Difficulty x Choice Made
        mean_decision_norm_EvD_RvS_array[,s_index,1,2] = colMeans(decision_norm_EvD_RvS_array[,,s_index,1,2], na.rm = T)
        mean_decision_norm_EvD_RvS_array[,s_index,2,1] = colMeans(decision_norm_EvD_RvS_array[,,s_index,2,1], na.rm = T)
        mean_decision_norm_EvD_RvS_array[,s_index,2,2] = colMeans(decision_norm_EvD_RvS_array[,,s_index,2,2], na.rm = T)
          mean_decision_norm_prev_EvD_array[,s_index,1,1] = colMeans(decision_norm_prev_EvD_array[,,s_index,1,1], na.rm = T) # Previous Difficulty x Current Difficulty
          mean_decision_norm_prev_EvD_array[,s_index,1,2] = colMeans(decision_norm_prev_EvD_array[,,s_index,1,2], na.rm = T)
          mean_decision_norm_prev_EvD_array[,s_index,2,1] = colMeans(decision_norm_prev_EvD_array[,,s_index,2,1], na.rm = T)
          mean_decision_norm_prev_EvD_array[,s_index,2,2] = colMeans(decision_norm_prev_EvD_array[,,s_index,2,2], na.rm = T)
        mean_decision_norm_EvD_WMC_array[,s_index,1,1] = colMeans(decision_norm_EvD_WMC_array[,,s_index,1,1], na.rm = T) # Current Difficulty x WMC
        mean_decision_norm_EvD_WMC_array[,s_index,1,2] = colMeans(decision_norm_EvD_WMC_array[,,s_index,1,2], na.rm = T)
        mean_decision_norm_EvD_WMC_array[,s_index,2,1] = colMeans(decision_norm_EvD_WMC_array[,,s_index,2,1], na.rm = T)
        mean_decision_norm_EvD_WMC_array[,s_index,2,2] = colMeans(decision_norm_EvD_WMC_array[,,s_index,2,2], na.rm = T)
          mean_decision_norm_prev_EvD_WMC_array[,s_index,1,1,1] = colMeans(decision_norm_prev_EvD_WMC_array[,,s_index,1,1,1], na.rm = T) # Previous Difficulty x Current Difficulty x WMC
          mean_decision_norm_prev_EvD_WMC_array[,s_index,1,1,2] = colMeans(decision_norm_prev_EvD_WMC_array[,,s_index,1,1,2], na.rm = T)
          mean_decision_norm_prev_EvD_WMC_array[,s_index,1,2,1] = colMeans(decision_norm_prev_EvD_WMC_array[,,s_index,1,2,1], na.rm = T)
          mean_decision_norm_prev_EvD_WMC_array[,s_index,1,2,2] = colMeans(decision_norm_prev_EvD_WMC_array[,,s_index,1,2,2], na.rm = T)
          mean_decision_norm_prev_EvD_WMC_array[,s_index,2,1,1] = colMeans(decision_norm_prev_EvD_WMC_array[,,s_index,2,1,1], na.rm = T)
          mean_decision_norm_prev_EvD_WMC_array[,s_index,2,1,2] = colMeans(decision_norm_prev_EvD_WMC_array[,,s_index,2,1,2], na.rm = T)
          mean_decision_norm_prev_EvD_WMC_array[,s_index,2,2,1] = colMeans(decision_norm_prev_EvD_WMC_array[,,s_index,2,2,1], na.rm = T)
          mean_decision_norm_prev_EvD_WMC_array[,s_index,2,2,2] = colMeans(decision_norm_prev_EvD_WMC_array[,,s_index,2,2,2], na.rm = T)

          cat(sprintf('. Done.\n'))
}



### GRAPHING PUPILLOMETRY: Overall Arrays ######

# Plot the Dec/ISI/Otc/ITI Graphs
for (s in keep_participants){
  s_index = which(keep_participants == s);

  cat(sprintf('Subject CGE%03i (%i of %i): trial 000', s, s_index, length(keep_participants)))

  pdf(sprintf('%s/plots/cge%03i_downsampled_dec_isi_otc_iti_plot.pdf',config$path$data$processed, s),
      width = 8, height = 4)

  # Decision End | ISI | Outcome | ITI
  plot(1, type = "n", xlab = "milliseconds", ylab = "demeaned pupil diameter (mm)", main = "Aligned to Decision",
       xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(-2, 2))
  polygon(x = c(1000, 2000, 2000, 1000),
          y = c(3, 3, -3, -3),
          lty = 0, col = rgb(0,0,0,.1))
  abline(v = 0, lty = 'dashed')

  for (t in 1:number_of_trials){
    cat(sprintf('\b\b\b%03i',t))
    lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
          y = dec_isi_otc_iti_array[t,,s_index], col = rgb(0,0,0,.05), lwd = 3)
  }
  # Add the person's average
  lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
        y = mean_dec_isi_otc_iti_array[,s_index], col = rgb(1,0,0), lwd = 3)
  dev.off() # complete the plot
  cat(sprintf('. Done.\n'))
}


# Plot the downsampled Decisions
pdf(sprintf('%s/plots/mean_downsampled_decision_plot.pdf',config$path$data$processed),
    width = 5, height = 8)

par(mfrow = c(2,1)); # Set up the individual-level plot
# Pre-decision | Decision Start
matplot(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
        y = mean_decision_start_array,
        col = rgb(1, 0, 0, .2), type = 'l', lwd = 3, lty = 'solid',
        xlab = "milliseconds", ylab = "demeaned pupil diameter (mm)",
        main = "Aligned to Decision Window Start",
        xlim = c(-baseline_window_width, 3000), ylim = c(-1, 1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_array, na.rm = T),
      lwd = 3, col = 'black')
abline(v = 0, lty = 'dashed')

# pre-dec window, up until 3000 ms into the 4000ms response window

matplot(x = decision_end_bins[1:(length(decision_end_bins)-1)] + bin_increment/2,
        y = mean_decision_end_array,
        col = rgb(1, 0, 0, .2), type = 'l', lwd = 3, lty = 'solid',
        xlab = "milliseconds", ylab = "demeaned pupil diameter (mm)",
        main = "Aligned to Choice",
        xlim = c(-3000, baseline_window_width), ylim = c(-1, 1))
lines(x = decision_end_bins[1:(length(decision_end_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_end_array, na.rm = T),
      lwd = 3, col = 'black')
abline(v = 0, lty = 'dotted')

# the last 3000ms of the 4000ms response window, ISI (1000), Otc (1000), and ITI (3000 or 3500ms)
dev.off()


# Plot the downsampled dec/isi/otc/iti
pdf(sprintf('%s/plots/mean_downsampled_dec_isi_otc_iti_plot.pdf',config$path$data$processed),
    width = 8, height = 4)

matplot(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
        y = mean_dec_isi_otc_iti_array,
        col = rgb(1, 0, 0, .2), type = 'l', lwd = 3, lty = 'solid',
        xlab = "milliseconds", ylab = "demeaned pupil diameter (mm)",
        main = "Aligned to Choice",
        xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(-.5, .5))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_array, na.rm = T),
      lwd = 3, col = 'black')
abline(v = 0, lty = 'dashed')

# the last 3000ms of the 4000ms response window, ISI (1000), Otc (1000), and ITI (3000 or 3500ms)
dev.off()

# Plot NORMALIZED - Test

par(mfrow = c(1,1))
matplot(x = normBins,
        y = mean_decision_norm_array,
        col = rgb(1, 0, 0, .2), type = 'l', lwd = 3, lty = 'solid',
        xlab = "Normalized Time Points (200)", ylab = "Demeaned Pupil Diameter (mm)",
        main = "Normalized Decision Window",
        xlim = c(0, 200), ylim = c(-0.4, 0.4))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_array, na.rm = T),
      lwd = 3, col = 'black')


### Setting up for GRAPHING GROUP PUPILLOMETRY #####

sem_decision_start_array = sem(mean_decision_start_array)
sem_decision_end_array = sem(mean_decision_end_array)
sem_dec_isi_otc_iti_array = sem(mean_dec_isi_otc_iti_array)

sem_decision_start_Easy_array = sem(mean_decision_start_EvD_array[,,1])
sem_decision_start_Diff_array = sem(mean_decision_start_EvD_array[,,2])
sem_dec_isi_otc_iti_Easy_array = sem(mean_dec_isi_otc_iti_EvD_array[,,1])
sem_dec_isi_otc_iti_Diff_array = sem(mean_dec_isi_otc_iti_EvD_array[,,2])

sem_decision_start_Easy_Risky_array = sem(mean_decision_start_EvD_RvS_array[,,1,1])
sem_decision_start_Easy_Safe_array = sem(mean_decision_start_EvD_RvS_array[,,1,2])
sem_decision_start_Diff_Risky_array = sem(mean_decision_start_EvD_RvS_array[,,2,1])
sem_decision_start_Diff_Safe_array = sem(mean_decision_start_EvD_RvS_array[,,2,2])
sem_decision_start_Easy_lWMC_array = sem(mean_decision_start_EvD_WMC_array[,,1,1])
sem_decision_start_Easy_hWMC_array = sem(mean_decision_start_EvD_WMC_array[,,1,2])
sem_decision_start_Diff_lWMC_array = sem(mean_decision_start_EvD_WMC_array[,,2,1])
sem_decision_start_Diff_hWMC_array = sem(mean_decision_start_EvD_WMC_array[,,2,2])
sem_dec_isi_otc_iti_Easy_Risky_array = sem(mean_dec_isi_otc_iti_EvD_RvS_array[,,1,1])
sem_dec_isi_otc_iti_Easy_Safe_array = sem(mean_dec_isi_otc_iti_EvD_RvS_array[,,1,2])
sem_dec_isi_otc_iti_Diff_Risky_array = sem(mean_dec_isi_otc_iti_EvD_RvS_array[,,2,1])
sem_dec_isi_otc_iti_Diff_Safe_array = sem(mean_dec_isi_otc_iti_EvD_RvS_array[,,2,2])
sem_dec_isi_otc_iti_Easy_lWMC_array = sem(mean_dec_isi_otc_iti_EvD_WMC_array[,,1,1])
sem_dec_isi_otc_iti_Easy_hWMC_array = sem(mean_dec_isi_otc_iti_EvD_WMC_array[,,1,2])
sem_dec_isi_otc_iti_Diff_lWMC_array = sem(mean_dec_isi_otc_iti_EvD_WMC_array[,,2,1])
sem_dec_isi_otc_iti_Diff_hWMC_array = sem(mean_dec_isi_otc_iti_EvD_WMC_array[,,2,2])

sem_decision_start_Easy_Prev_Easy_array = sem(mean_decision_start_prev_EvD_array[,,1,1])
sem_decision_start_Easy_Prev_Diff_array = sem(mean_decision_start_prev_EvD_array[,,1,2])
sem_decision_start_Diff_Prev_Easy_array = sem(mean_decision_start_prev_EvD_array[,,2,1])
sem_decision_start_Diff_Prev_Diff_array = sem(mean_decision_start_prev_EvD_array[,,2,2])
sem_dec_isi_otc_iti_Easy_Prev_Easy_array = sem(mean_dec_isi_otc_iti_prev_EvD_array[,,1,1])
sem_dec_isi_otc_iti_Easy_Prev_Diff_array = sem(mean_dec_isi_otc_iti_prev_EvD_array[,,1,2])
sem_dec_isi_otc_iti_Diff_Prev_Easy_array = sem(mean_dec_isi_otc_iti_prev_EvD_array[,,2,1])
sem_dec_isi_otc_iti_Diff_Prev_Diff_array = sem(mean_dec_isi_otc_iti_prev_EvD_array[,,2,2])

sem_decision_start_Easy_Prev_Easy_lWMC_array = sem(mean_decision_start_prev_EvD_WMC_array[,,1,1,1])
sem_decision_start_Easy_Prev_Easy_hWMC_array = sem(mean_decision_start_prev_EvD_WMC_array[,,1,1,2])
sem_decision_start_Easy_Prev_Diff_lWMC_array = sem(mean_decision_start_prev_EvD_WMC_array[,,1,2,1])
sem_decision_start_Easy_Prev_Diff_hWMC_array = sem(mean_decision_start_prev_EvD_WMC_array[,,1,2,2])
sem_decision_start_Diff_Prev_Easy_lWMC_array = sem(mean_decision_start_prev_EvD_WMC_array[,,2,1,1])
sem_decision_start_Diff_Prev_Easy_hWMC_array = sem(mean_decision_start_prev_EvD_WMC_array[,,2,1,2])
sem_decision_start_Diff_Prev_Diff_lWMC_array = sem(mean_decision_start_prev_EvD_WMC_array[,,2,2,1])
sem_decision_start_Diff_Prev_Diff_hWMC_array = sem(mean_decision_start_prev_EvD_WMC_array[,,2,2,2])

sem_dec_isi_otc_iti_Easy_Prev_Easy_lWMC_array = sem(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,1,1])
sem_dec_isi_otc_iti_Easy_Prev_Easy_hWMC_array = sem(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,1,2])
sem_dec_isi_otc_iti_Easy_Prev_Diff_lWMC_array = sem(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,2,1])
sem_dec_isi_otc_iti_Easy_Prev_Diff_hWMC_array = sem(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,2,2])
sem_dec_isi_otc_iti_Diff_Prev_Easy_lWMC_array = sem(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,1,1])
sem_dec_isi_otc_iti_Diff_Prev_Easy_hWMC_array = sem(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,1,2])
sem_dec_isi_otc_iti_Diff_Prev_Diff_lWMC_array = sem(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,2,1])
sem_dec_isi_otc_iti_Diff_Prev_Diff_hWMC_array = sem(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,2,2])

decision_start_upper = rowMeans(mean_decision_start_array, na.rm = T) + sem_decision_start_array
decision_start_lower = rowMeans(mean_decision_start_array, na.rm = T) - sem_decision_start_array

decision_end_upper = rowMeans(mean_decision_end_array, na.rm = T) + sem_decision_end_array
decision_end_lower = rowMeans(mean_decision_end_array, na.rm = T) - sem_decision_end_array

dec_isi_otc_iti_upper = rowMeans(mean_dec_isi_otc_iti_array, na.rm = T) + sem_dec_isi_otc_iti_array
dec_isi_otc_iti_lower = rowMeans(mean_dec_isi_otc_iti_array, na.rm = T) - sem_dec_isi_otc_iti_array

decision_start_Easy_upper = rowMeans(mean_decision_start_EvD_array[,,1], na.rm = T) + sem_decision_start_Easy_array
decision_start_Easy_lower = rowMeans(mean_decision_start_EvD_array[,,1], na.rm = T) - sem_decision_start_Easy_array
decision_start_Diff_upper = rowMeans(mean_decision_start_EvD_array[,,2], na.rm = T) + sem_decision_start_Diff_array
decision_start_Diff_lower = rowMeans(mean_decision_start_EvD_array[,,2], na.rm = T) - sem_decision_start_Diff_array

dec_isi_otc_iti_Easy_upper = rowMeans(mean_dec_isi_otc_iti_EvD_array[,,1], na.rm = T) + sem_dec_isi_otc_iti_Easy_array
dec_isi_otc_iti_Easy_lower = rowMeans(mean_dec_isi_otc_iti_EvD_array[,,1], na.rm = T) - sem_dec_isi_otc_iti_Easy_array
dec_isi_otc_iti_Diff_upper = rowMeans(mean_dec_isi_otc_iti_EvD_array[,,2], na.rm = T) + sem_dec_isi_otc_iti_Diff_array
dec_isi_otc_iti_Diff_lower = rowMeans(mean_dec_isi_otc_iti_EvD_array[,,2], na.rm = T) - sem_dec_isi_otc_iti_Diff_array

decision_start_Easy_Risky_upper = rowMeans(mean_decision_start_EvD_RvS_array[,,1,1], na.rm = T) + sem_decision_start_Easy_Risky_array
decision_start_Easy_Risky_lower = rowMeans(mean_decision_start_EvD_RvS_array[,,1,1], na.rm = T) - sem_decision_start_Easy_Risky_array
decision_start_Easy_Safe_upper = rowMeans(mean_decision_start_EvD_RvS_array[,,1,2], na.rm = T) + sem_decision_start_Easy_Safe_array
decision_start_Easy_Safe_lower = rowMeans(mean_decision_start_EvD_RvS_array[,,1,2], na.rm = T) - sem_decision_start_Easy_Safe_array
decision_start_Diff_Risky_upper = rowMeans(mean_decision_start_EvD_RvS_array[,,2,1], na.rm = T) + sem_decision_start_Diff_Risky_array
decision_start_Diff_Risky_lower = rowMeans(mean_decision_start_EvD_RvS_array[,,2,1], na.rm = T) - sem_decision_start_Diff_Risky_array
decision_start_Diff_Safe_upper = rowMeans(mean_decision_start_EvD_RvS_array[,,2,2], na.rm = T) + sem_decision_start_Diff_Safe_array
decision_start_Diff_Safe_lower = rowMeans(mean_decision_start_EvD_RvS_array[,,2,2], na.rm = T) - sem_decision_start_Diff_Safe_array

decision_start_Easy_lWMC_upper = rowMeans(mean_decision_start_EvD_WMC_array[,,1,1], na.rm = T) + sem_decision_start_Easy_lWMC_array
decision_start_Easy_lWMC_lower = rowMeans(mean_decision_start_EvD_WMC_array[,,1,1], na.rm = T) - sem_decision_start_Easy_lWMC_array
decision_start_Easy_hWMC_upper = rowMeans(mean_decision_start_EvD_WMC_array[,,1,2], na.rm = T) + sem_decision_start_Easy_hWMC_array
decision_start_Easy_hWMC_lower = rowMeans(mean_decision_start_EvD_WMC_array[,,1,2], na.rm = T) - sem_decision_start_Easy_hWMC_array
decision_start_Diff_lWMC_upper = rowMeans(mean_decision_start_EvD_WMC_array[,,2,1], na.rm = T) + sem_decision_start_Diff_lWMC_array
decision_start_Diff_lWMC_lower = rowMeans(mean_decision_start_EvD_WMC_array[,,2,1], na.rm = T) - sem_decision_start_Diff_lWMC_array
decision_start_Diff_hWMC_upper = rowMeans(mean_decision_start_EvD_WMC_array[,,2,2], na.rm = T) + sem_decision_start_Diff_hWMC_array
decision_start_Diff_hWMC_lower = rowMeans(mean_decision_start_EvD_WMC_array[,,2,2], na.rm = T) - sem_decision_start_Diff_hWMC_array

decision_start_Easy_Prev_Easy_upper = rowMeans(mean_decision_start_prev_EvD_array[,,1,1], na.rm = T) + sem_decision_start_Easy_Prev_Easy_array
decision_start_Easy_Prev_Easy_lower = rowMeans(mean_decision_start_prev_EvD_array[,,1,1], na.rm = T) - sem_decision_start_Easy_Prev_Easy_array
decision_start_Easy_Prev_Diff_upper = rowMeans(mean_decision_start_prev_EvD_array[,,1,2], na.rm = T) + sem_decision_start_Easy_Prev_Diff_array
decision_start_Easy_Prev_Diff_lower = rowMeans(mean_decision_start_prev_EvD_array[,,1,2], na.rm = T) - sem_decision_start_Easy_Prev_Diff_array
decision_start_Diff_Prev_Easy_upper = rowMeans(mean_decision_start_prev_EvD_array[,,2,1], na.rm = T) + sem_decision_start_Diff_Prev_Easy_array
decision_start_Diff_Prev_Easy_lower = rowMeans(mean_decision_start_prev_EvD_array[,,2,1], na.rm = T) - sem_decision_start_Diff_Prev_Easy_array
decision_start_Diff_Prev_Diff_upper = rowMeans(mean_decision_start_prev_EvD_array[,,2,2], na.rm = T) + sem_decision_start_Diff_Prev_Diff_array
decision_start_Diff_Prev_Diff_lower = rowMeans(mean_decision_start_prev_EvD_array[,,2,2], na.rm = T) - sem_decision_start_Diff_Prev_Diff_array

decision_start_Easy_Prev_Easy_lWMC_upper = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,1,1], na.rm = T) + sem_decision_start_Easy_Prev_Easy_lWMC_array
decision_start_Easy_Prev_Easy_lWMC_lower = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,1,1], na.rm = T) - sem_decision_start_Easy_Prev_Easy_lWMC_array
decision_start_Easy_Prev_Easy_hWMC_upper = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,1,2], na.rm = T) + sem_decision_start_Easy_Prev_Easy_hWMC_array
decision_start_Easy_Prev_Easy_hWMC_lower = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,1,2], na.rm = T) - sem_decision_start_Easy_Prev_Easy_hWMC_array
decision_start_Easy_Prev_Diff_lWMC_upper = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,2,1], na.rm = T) + sem_decision_start_Easy_Prev_Diff_lWMC_array
decision_start_Easy_Prev_Diff_lWMC_lower = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,2,1], na.rm = T) - sem_decision_start_Easy_Prev_Diff_lWMC_array
decision_start_Easy_Prev_Diff_hWMC_upper = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,2,2], na.rm = T) + sem_decision_start_Easy_Prev_Diff_hWMC_array
decision_start_Easy_Prev_Diff_hWMC_lower = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,2,2], na.rm = T) - sem_decision_start_Easy_Prev_Diff_hWMC_array
decision_start_Diff_Prev_Easy_lWMC_upper = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,1,1], na.rm = T) + sem_decision_start_Diff_Prev_Easy_lWMC_array
decision_start_Diff_Prev_Easy_lWMC_lower = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,1,1], na.rm = T) - sem_decision_start_Diff_Prev_Easy_lWMC_array
decision_start_Diff_Prev_Easy_hWMC_upper = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,1,2], na.rm = T) + sem_decision_start_Diff_Prev_Easy_hWMC_array
decision_start_Diff_Prev_Easy_hWMC_lower = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,1,2], na.rm = T) - sem_decision_start_Diff_Prev_Easy_hWMC_array
decision_start_Diff_Prev_Diff_lWMC_upper = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,2,1], na.rm = T) + sem_decision_start_Diff_Prev_Diff_lWMC_array
decision_start_Diff_Prev_Diff_lWMC_lower = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,2,1], na.rm = T) - sem_decision_start_Diff_Prev_Diff_lWMC_array
decision_start_Diff_Prev_Diff_hWMC_upper = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,2,2], na.rm = T) + sem_decision_start_Diff_Prev_Diff_hWMC_array
decision_start_Diff_Prev_Diff_hWMC_lower = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,2,2], na.rm = T) - sem_decision_start_Diff_Prev_Diff_hWMC_array

dec_isi_otc_iti_Easy_Risky_upper = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,1,1], na.rm = T) + sem_dec_isi_otc_iti_Easy_Risky_array
dec_isi_otc_iti_Easy_Risky_lower = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,1,1], na.rm = T) - sem_dec_isi_otc_iti_Easy_Risky_array
dec_isi_otc_iti_Easy_Safe_upper = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,1,2], na.rm = T) + sem_dec_isi_otc_iti_Easy_Safe_array
dec_isi_otc_iti_Easy_Safe_lower = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,1,2], na.rm = T) - sem_dec_isi_otc_iti_Easy_Safe_array
dec_isi_otc_iti_Diff_Risky_upper = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,2,1], na.rm = T) + sem_dec_isi_otc_iti_Diff_Risky_array
dec_isi_otc_iti_Diff_Risky_lower = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,2,1], na.rm = T) - sem_dec_isi_otc_iti_Diff_Risky_array
dec_isi_otc_iti_Diff_Safe_upper = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,2,2], na.rm = T) + sem_dec_isi_otc_iti_Diff_Safe_array
dec_isi_otc_iti_Diff_Safe_lower = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,2,2], na.rm = T) - sem_dec_isi_otc_iti_Diff_Safe_array

dec_isi_otc_iti_Easy_lWMC_upper = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,1,1], na.rm = T) + sem_dec_isi_otc_iti_Easy_lWMC_array
dec_isi_otc_iti_Easy_lWMC_lower = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,1,1], na.rm = T) - sem_dec_isi_otc_iti_Easy_lWMC_array
dec_isi_otc_iti_Easy_hWMC_upper = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,1,2], na.rm = T) + sem_dec_isi_otc_iti_Easy_hWMC_array
dec_isi_otc_iti_Easy_hWMC_lower = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,1,2], na.rm = T) - sem_dec_isi_otc_iti_Easy_hWMC_array
dec_isi_otc_iti_Diff_lWMC_upper = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,2,1], na.rm = T) + sem_dec_isi_otc_iti_Diff_lWMC_array
dec_isi_otc_iti_Diff_lWMC_lower = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,2,1], na.rm = T) - sem_dec_isi_otc_iti_Diff_lWMC_array
dec_isi_otc_iti_Diff_hWMC_upper = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,2,2], na.rm = T) + sem_dec_isi_otc_iti_Diff_hWMC_array
dec_isi_otc_iti_Diff_hWMC_lower = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,2,2], na.rm = T) - sem_dec_isi_otc_iti_Diff_hWMC_array

dec_isi_otc_iti_Easy_Prev_Easy_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,1,1], na.rm = T) + sem_dec_isi_otc_iti_Easy_Prev_Easy_array
dec_isi_otc_iti_Easy_Prev_Easy_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,1,1], na.rm = T) - sem_dec_isi_otc_iti_Easy_Prev_Easy_array
dec_isi_otc_iti_Easy_Prev_Diff_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,1,2], na.rm = T) + sem_dec_isi_otc_iti_Easy_Prev_Diff_array
dec_isi_otc_iti_Easy_Prev_Diff_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,1,2], na.rm = T) - sem_dec_isi_otc_iti_Easy_Prev_Diff_array
dec_isi_otc_iti_Diff_Prev_Easy_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,2,1], na.rm = T) + sem_dec_isi_otc_iti_Diff_Prev_Easy_array
dec_isi_otc_iti_Diff_Prev_Easy_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,2,1], na.rm = T) - sem_dec_isi_otc_iti_Diff_Prev_Easy_array
dec_isi_otc_iti_Diff_Prev_Diff_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,2,2], na.rm = T) + sem_dec_isi_otc_iti_Diff_Prev_Diff_array
dec_isi_otc_iti_Diff_Prev_Diff_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,2,2], na.rm = T) - sem_dec_isi_otc_iti_Diff_Prev_Diff_array

dec_isi_otc_iti_Easy_Prev_Easy_lWMC_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,1,1], na.rm = T) + sem_dec_isi_otc_iti_Easy_Prev_Easy_lWMC_array
dec_isi_otc_iti_Easy_Prev_Easy_lWMC_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,1,1], na.rm = T) - sem_dec_isi_otc_iti_Easy_Prev_Easy_lWMC_array
dec_isi_otc_iti_Easy_Prev_Easy_hWMC_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,1,2], na.rm = T) + sem_dec_isi_otc_iti_Easy_Prev_Easy_hWMC_array
dec_isi_otc_iti_Easy_Prev_Easy_hWMC_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,1,2], na.rm = T) - sem_dec_isi_otc_iti_Easy_Prev_Easy_hWMC_array
dec_isi_otc_iti_Easy_Prev_Diff_lWMC_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,2,1], na.rm = T) + sem_dec_isi_otc_iti_Easy_Prev_Diff_lWMC_array
dec_isi_otc_iti_Easy_Prev_Diff_lWMC_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,2,1], na.rm = T) - sem_dec_isi_otc_iti_Easy_Prev_Diff_lWMC_array
dec_isi_otc_iti_Easy_Prev_Diff_hWMC_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,2,2], na.rm = T) + sem_dec_isi_otc_iti_Easy_Prev_Diff_hWMC_array
dec_isi_otc_iti_Easy_Prev_Diff_hWMC_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,2,2], na.rm = T) - sem_dec_isi_otc_iti_Easy_Prev_Diff_hWMC_array
dec_isi_otc_iti_Diff_Prev_Easy_lWMC_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,1,1], na.rm = T) + sem_dec_isi_otc_iti_Diff_Prev_Easy_lWMC_array
dec_isi_otc_iti_Diff_Prev_Easy_lWMC_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,1,1], na.rm = T) - sem_dec_isi_otc_iti_Diff_Prev_Easy_lWMC_array
dec_isi_otc_iti_Diff_Prev_Easy_hWMC_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,1,2], na.rm = T) + sem_dec_isi_otc_iti_Diff_Prev_Easy_hWMC_array
dec_isi_otc_iti_Diff_Prev_Easy_hWMC_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,1,2], na.rm = T) - sem_dec_isi_otc_iti_Diff_Prev_Easy_hWMC_array
dec_isi_otc_iti_Diff_Prev_Diff_lWMC_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,2,1], na.rm = T) + sem_dec_isi_otc_iti_Diff_Prev_Diff_lWMC_array
dec_isi_otc_iti_Diff_Prev_Diff_lWMC_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,2,1], na.rm = T) - sem_dec_isi_otc_iti_Diff_Prev_Diff_lWMC_array
dec_isi_otc_iti_Diff_Prev_Diff_hWMC_upper = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,2,2], na.rm = T) + sem_dec_isi_otc_iti_Diff_Prev_Diff_hWMC_array
dec_isi_otc_iti_Diff_Prev_Diff_hWMC_lower = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,2,2], na.rm = T) - sem_dec_isi_otc_iti_Diff_Prev_Diff_hWMC_array

# NORMALIZED Pupillometry
sem_decision_norm_array = sem(mean_decision_norm_array)
sem_decision_norm_Easy_array = sem(mean_decision_norm_EvD_array[,,1])
sem_decision_norm_Diff_array = sem(mean_decision_norm_EvD_array[,,2])

sem_decision_norm_Easy_Risky_array = sem(mean_decision_norm_EvD_RvS_array[,,1,1])
sem_decision_norm_Easy_Safe_array = sem(mean_decision_norm_EvD_RvS_array[,,1,2])
sem_decision_norm_Diff_Risky_array = sem(mean_decision_norm_EvD_RvS_array[,,2,1])
sem_decision_norm_Diff_Safe_array = sem(mean_decision_norm_EvD_RvS_array[,,2,2])

sem_decision_norm_Diff_Safe_array = sem(mean_decision_norm_EvD_RvS_array[,,2,2])
sem_decision_norm_Easy_lWMC_array = sem(mean_decision_norm_EvD_WMC_array[,,1,1])
sem_decision_norm_Easy_hWMC_array = sem(mean_decision_norm_EvD_WMC_array[,,1,2])
sem_decision_norm_Diff_lWMC_array = sem(mean_decision_norm_EvD_WMC_array[,,2,1])
sem_decision_norm_Diff_hWMC_array = sem(mean_decision_norm_EvD_WMC_array[,,2,2])

sem_decision_norm_Easy_Prev_Easy_array = sem(mean_decision_norm_prev_EvD_array[,,1,1])
sem_decision_norm_Easy_Prev_Diff_array = sem(mean_decision_norm_prev_EvD_array[,,1,2])
sem_decision_norm_Diff_Prev_Easy_array = sem(mean_decision_norm_prev_EvD_array[,,2,1])
sem_decision_norm_Diff_Prev_Diff_array = sem(mean_decision_norm_prev_EvD_array[,,2,2])

sem_decision_norm_Easy_Prev_Easy_lWMC_array = sem(mean_decision_norm_prev_EvD_WMC_array[,,1,1,1])
sem_decision_norm_Easy_Prev_Easy_hWMC_array = sem(mean_decision_norm_prev_EvD_WMC_array[,,1,1,2])
sem_decision_norm_Easy_Prev_Diff_lWMC_array = sem(mean_decision_norm_prev_EvD_WMC_array[,,1,2,1])
sem_decision_norm_Easy_Prev_Diff_hWMC_array = sem(mean_decision_norm_prev_EvD_WMC_array[,,1,2,2])
sem_decision_norm_Diff_Prev_Easy_lWMC_array = sem(mean_decision_norm_prev_EvD_WMC_array[,,2,1,1])
sem_decision_norm_Diff_Prev_Easy_hWMC_array = sem(mean_decision_norm_prev_EvD_WMC_array[,,2,1,2])
sem_decision_norm_Diff_Prev_Diff_lWMC_array = sem(mean_decision_norm_prev_EvD_WMC_array[,,2,2,1])
sem_decision_norm_Diff_Prev_Diff_hWMC_array = sem(mean_decision_norm_prev_EvD_WMC_array[,,2,2,2])

decision_norm_array_upper = rowMeans(mean_decision_norm_array, na.rm = T) + sem_decision_norm_array
decision_norm_array_lower = rowMeans(mean_decision_norm_array, na.rm = T) - sem_decision_norm_array

decision_norm_Easy_upper = rowMeans(mean_decision_norm_EvD_array[,,1], na.rm = T) + sem_decision_norm_Easy_array
decision_norm_Easy_lower = rowMeans(mean_decision_norm_EvD_array[,,1], na.rm = T) - sem_decision_norm_Easy_array
decision_norm_Diff_upper = rowMeans(mean_decision_norm_EvD_array[,,2], na.rm = T) + sem_decision_norm_Diff_array
decision_norm_Diff_lower = rowMeans(mean_decision_norm_EvD_array[,,2], na.rm = T) - sem_decision_norm_Diff_array

decision_norm_Easy_Risky_upper = rowMeans(mean_decision_norm_EvD_RvS_array[,,1,1], na.rm = T) + sem_decision_norm_Easy_Risky_array
decision_norm_Easy_Risky_lower = rowMeans(mean_decision_norm_EvD_RvS_array[,,1,1], na.rm = T) - sem_decision_norm_Easy_Risky_array
decision_norm_Easy_Safe_upper = rowMeans(mean_decision_norm_EvD_RvS_array[,,1,2], na.rm = T) + sem_decision_norm_Easy_Safe_array
decision_norm_Easy_Safe_lower = rowMeans(mean_decision_norm_EvD_RvS_array[,,1,2], na.rm = T) - sem_decision_norm_Easy_Safe_array
decision_norm_Diff_Risky_upper = rowMeans(mean_decision_norm_EvD_RvS_array[,,2,1], na.rm = T) + sem_decision_norm_Diff_Risky_array
decision_norm_Diff_Risky_lower = rowMeans(mean_decision_norm_EvD_RvS_array[,,2,1], na.rm = T) - sem_decision_norm_Diff_Risky_array
decision_norm_Diff_Safe_upper = rowMeans(mean_decision_norm_EvD_RvS_array[,,2,2], na.rm = T) + sem_decision_norm_Diff_Safe_array
decision_norm_Diff_Safe_lower = rowMeans(mean_decision_norm_EvD_RvS_array[,,2,2], na.rm = T) - sem_decision_norm_Diff_Safe_array

decision_norm_Easy_lWMC_upper = rowMeans(mean_decision_norm_EvD_WMC_array[,,1,1], na.rm = T) + sem_decision_norm_Easy_lWMC_array
decision_norm_Easy_lWMC_lower = rowMeans(mean_decision_norm_EvD_WMC_array[,,1,1], na.rm = T) - sem_decision_norm_Easy_lWMC_array
decision_norm_Easy_hWMC_upper = rowMeans(mean_decision_norm_EvD_WMC_array[,,1,2], na.rm = T) + sem_decision_norm_Easy_hWMC_array
decision_norm_Easy_hWMC_lower = rowMeans(mean_decision_norm_EvD_WMC_array[,,1,2], na.rm = T) - sem_decision_norm_Easy_hWMC_array
decision_norm_Diff_lWMC_upper = rowMeans(mean_decision_norm_EvD_WMC_array[,,2,1], na.rm = T) + sem_decision_norm_Diff_lWMC_array
decision_norm_Diff_lWMC_lower = rowMeans(mean_decision_norm_EvD_WMC_array[,,2,1], na.rm = T) - sem_decision_norm_Diff_lWMC_array
decision_norm_Diff_hWMC_upper = rowMeans(mean_decision_norm_EvD_WMC_array[,,2,2], na.rm = T) + sem_decision_norm_Diff_hWMC_array
decision_norm_Diff_hWMC_lower = rowMeans(mean_decision_norm_EvD_WMC_array[,,2,2], na.rm = T) - sem_decision_norm_Diff_hWMC_array

decision_norm_Easy_Prev_Easy_upper = rowMeans(mean_decision_norm_prev_EvD_array[,,1,1], na.rm = T) + sem_decision_norm_Easy_Prev_Easy_array
decision_norm_Easy_Prev_Easy_lower = rowMeans(mean_decision_norm_prev_EvD_array[,,1,1], na.rm = T) - sem_decision_norm_Easy_Prev_Easy_array
decision_norm_Easy_Prev_Diff_upper = rowMeans(mean_decision_norm_prev_EvD_array[,,1,2], na.rm = T) + sem_decision_norm_Easy_Prev_Diff_array
decision_norm_Easy_Prev_Diff_lower = rowMeans(mean_decision_norm_prev_EvD_array[,,1,2], na.rm = T) - sem_decision_norm_Easy_Prev_Diff_array
decision_norm_Diff_Prev_Easy_upper = rowMeans(mean_decision_norm_prev_EvD_array[,,2,1], na.rm = T) + sem_decision_norm_Diff_Prev_Easy_array
decision_norm_Diff_Prev_Easy_lower = rowMeans(mean_decision_norm_prev_EvD_array[,,2,1], na.rm = T) - sem_decision_norm_Diff_Prev_Easy_array
decision_norm_Diff_Prev_Diff_upper = rowMeans(mean_decision_norm_prev_EvD_array[,,2,2], na.rm = T) + sem_decision_norm_Diff_Prev_Diff_array
decision_norm_Diff_Prev_Diff_lower = rowMeans(mean_decision_norm_prev_EvD_array[,,2,2], na.rm = T) - sem_decision_norm_Diff_Prev_Diff_array

decision_norm_Easy_Prev_Easy_lWMC_upper = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,1,1], na.rm = T) + sem_decision_norm_Easy_Prev_Easy_lWMC_array
decision_norm_Easy_Prev_Easy_lWMC_lower = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,1,1], na.rm = T) - sem_decision_norm_Easy_Prev_Easy_lWMC_array
decision_norm_Easy_Prev_Easy_hWMC_upper = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,1,2], na.rm = T) + sem_decision_norm_Easy_Prev_Easy_hWMC_array
decision_norm_Easy_Prev_Easy_hWMC_lower = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,1,2], na.rm = T) - sem_decision_norm_Easy_Prev_Easy_hWMC_array
decision_norm_Easy_Prev_Diff_lWMC_upper = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,2,1], na.rm = T) + sem_decision_norm_Easy_Prev_Diff_lWMC_array
decision_norm_Easy_Prev_Diff_lWMC_lower = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,2,1], na.rm = T) - sem_decision_norm_Easy_Prev_Diff_lWMC_array
decision_norm_Easy_Prev_Diff_hWMC_upper = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,2,2], na.rm = T) + sem_decision_norm_Easy_Prev_Diff_hWMC_array
decision_norm_Easy_Prev_Diff_hWMC_lower = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,2,2], na.rm = T) - sem_decision_norm_Easy_Prev_Diff_hWMC_array
decision_norm_Diff_Prev_Easy_lWMC_upper = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,1,1], na.rm = T) + sem_decision_norm_Diff_Prev_Easy_lWMC_array
decision_norm_Diff_Prev_Easy_lWMC_lower = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,1,1], na.rm = T) - sem_decision_norm_Diff_Prev_Easy_lWMC_array
decision_norm_Diff_Prev_Easy_hWMC_upper = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,1,2], na.rm = T) + sem_decision_norm_Diff_Prev_Easy_hWMC_array
decision_norm_Diff_Prev_Easy_hWMC_lower = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,1,2], na.rm = T) - sem_decision_norm_Diff_Prev_Easy_hWMC_array
decision_norm_Diff_Prev_Diff_lWMC_upper = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,2,1], na.rm = T) + sem_decision_norm_Diff_Prev_Diff_lWMC_array
decision_norm_Diff_Prev_Diff_lWMC_lower = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,2,1], na.rm = T) - sem_decision_norm_Diff_Prev_Diff_lWMC_array
decision_norm_Diff_Prev_Diff_hWMC_upper = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,2,2], na.rm = T) + sem_decision_norm_Diff_Prev_Diff_hWMC_array
decision_norm_Diff_Prev_Diff_hWMC_lower = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,2,2], na.rm = T) - sem_decision_norm_Diff_Prev_Diff_hWMC_array


sem_decision_start_x_vals = c(decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
                              rev(decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2))
sem_decision_end_x_vals = c(decision_end_bins[1:(length(decision_end_bins)-1)] + bin_increment/2,
                            rev(decision_end_bins[1:(length(decision_end_bins)-1)] + bin_increment/2))
sem_dec_isi_otc_iti_x_vals = c(dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
                               rev(dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2))

sem_decision_norm_x_vals = c(normBins, rev(normBins));



# GRAPHING GROUP PUPILLOMETRY: Splitting into Choice Difficulty, Choice Made, & Previous Difficulty ########

## - ALL TRIALS - #####

# Plotting from Decision Window Start to Choice & from Choice to Decision Window End:
# All Trials
pdf(sprintf('%s/plots/mean_downsampled_decision_plot_groupOnly.pdf',config$path$data$processed),
    width = 5, height = 8)
par(mfrow = c(2,1)); # Set up the individual-level plot
# Pre-decision | Decision Start
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Decision Window Start",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(decision_start_lower),max(decision_start_upper)))
polygon(x = sem_decision_start_x_vals,y = c(decision_start_upper,rev(decision_start_lower)),
        lty = 0, col = rgb(0,0,0,.2))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_array, na.rm = T), type = 'l',
      lwd = 3, col = 'black')
abline(v = 0, lty = 'dashed')
# pre-dec window, up until 3000 ms into the 4000ms response window
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice",
     xlim = c(-3000, baseline_window_width), ylim = c(min(decision_start_lower),max(decision_start_upper)))
polygon(x = sem_decision_end_x_vals,y = c(decision_end_upper,rev(decision_end_lower)),
        lty = 0, col = rgb(0,0,0,.2))
lines(x = decision_end_bins[1:(length(decision_end_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_end_array, na.rm = T), type = 'l',
      lwd = 3, col = 'black')
abline(v = 0, lty = 'dotted')
dev.off()

# Plotting NORMALIZED pupillometry: Decision Window Start to Choice:
# All Trials
pdf(sprintf('%s/plots/mean_downsampled_decision_normalized_plot_groupOnly.pdf',config$path$data$processed),
    width = 5, height = 4)
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Normalized Time Points (200)", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Normalized Decision Window: From Start to Choice",
     xlim = c(1, number_of_normBins), ylim = c(min(decision_norm_array_lower),max(decision_norm_array_upper)))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_array_upper,rev(decision_norm_array_lower)),
        lty = 0, col = rgb(0,0,0,.2))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_array, na.rm = T), type = 'l',
      lwd = 3, col = 'black')
dev.off()

# Plotting from Choice to ITI:
# All Trials
pdf(sprintf('%s/plots/mean_downsampled_dec_isi_otc_iti_plot_groupOnly.pdf',config$path$data$processed),
    width = 8, height = 4)
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(dec_isi_otc_iti_lower),max(dec_isi_otc_iti_upper)))
polygon(x = c(1000, 2000, 2000, 1000),
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(dec_isi_otc_iti_lower),
     labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_upper,rev(dec_isi_otc_iti_lower)),
        lty = 0, col = rgb(0,0,0,.2))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_array, na.rm = T), type = 'l',
      lwd = 3, col = 'black')
abline(v = 0, lty = 'dashed')
dev.off()

## - CHOICE DIFFICULTY - #####

# Plotting from Decision Window Start to Choice & from Choice to Decision Window End:
# Choice Difficulty (Easy v. Difficult Trials)
pdf(sprintf('%s/plots/mean_downsampled_decision_plot_EvD_groupOnly.pdf',config$path$data$processed),
    width = 5, height = 4)
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Decision Window Start",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(decision_start_Easy_lower),max(decision_start_Easy_upper))) # use Easy to specify height
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Easy_upper,rev(decision_start_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.1)) # blue for easy
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Diff_upper,rev(decision_start_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.1)) # red for difficult
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_array[,,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_array[,,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy Trial", "Difficult Trial"),
       col = c('blue', 'red'), lty = c(1, 1))
dev.off()

# Plotting NORMALIZED pupillometry: Decision Window Start to Choice:
# Choice Difficulty (Easy v. Difficult Trials)
pdf(sprintf('%s/plots/mean_downsampled_decision_normalized_plot_EvD_groupOnly.pdf',config$path$data$processed),
    width = 5, height = 4)
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Normalized Time Points (200)", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Normalized Decision Window: From Start to Choice",
     xlim = c(1, number_of_normBins), ylim = c(min(c(decision_norm_Easy_lower,decision_norm_Diff_lower)),
                                               max(c(decision_norm_Easy_upper,decision_norm_Diff_upper))))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_upper,rev(decision_norm_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.2))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_upper,rev(decision_norm_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.2))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_array[,,1], na.rm = T), type = 'l',
      lwd = 3, col = 'blue')
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_array[,,2], na.rm = T), type = 'l',
      lwd = 3, col = 'red')
legend("bottomright", legend = c("Easy Trial", "Difficult Trial"),
       col = c('blue', 'red'), lty = c(1, 1))
dev.off()

# Plotting from Choice to ITI:
# Choice Difficulty (Easy v. Difficult Trials)
pdf(sprintf('%s/plots/mean_downsampled_dec_isi_otc_iti_plot_EvD_groupOnly.pdf',config$path$data$processed),
    width = 8, height = 4)
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(dec_isi_otc_iti_Easy_lower),max(dec_isi_otc_iti_Easy_upper)))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(dec_isi_otc_iti_Easy_lower),
     labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_upper,rev(dec_isi_otc_iti_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.1)) # blue (easy)
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_upper,rev(dec_isi_otc_iti_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.1)) # red (difficult)
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_array[,,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1)) # blue (easy) mean
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_array[,,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0)) # red (diff) mean
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy Trial", "Difficult Trial"),
       col = c('blue', 'red'), lty = c(1, 1))
dev.off()

## - CHOICE MADE - #####

# Plotting from Decision Window Start to Choice:
# Choice Difficulty (Easy v. Difficult Trials) x Choice Made (Risky v. Safe Choice)
# Easy Trials x Choice Made
par(mfrow = c(2,1))
pdf(sprintf('%s/plots/mean_downsampled_dec_isi_otc_iti_plot_EvD_groupOnly.pdf',config$path$data$processed),
    width = 8, height = 4)
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Decision Window Start: Easy Trials x Choice Made",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(c(decision_start_Easy_Risky_lower, decision_start_Easy_Safe_lower)),
                                                      max(c(decision_start_Easy_Risky_upper, decision_start_Easy_Safe_upper)))) # use Easy to specify height
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Easy_Risky_upper,rev(decision_start_Easy_Risky_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Easy_Safe_upper,rev(decision_start_Easy_Safe_lower)),
        lty = 0, col = rgb(0,0,1,.1))

lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_RvS_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_RvS_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Risky", "Safe"),
       col = c("red", "blue"), lty = c(1, 1))
# Difficult Trials x Choice Made
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Decision Window Start: Difficult Trials x Choice Made",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(c(decision_start_Diff_Risky_lower, decision_start_Diff_Safe_lower)),
                                                      max(c(decision_start_Diff_Risky_upper, decision_start_Diff_Safe_upper)))) # use Easy to specify height
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Diff_Risky_upper,rev(decision_start_Diff_Risky_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Diff_Safe_upper,rev(decision_start_Diff_Safe_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_RvS_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_RvS_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Risky", "Safe"),
       col = c("red", "blue"), lty = c(1, 1))
# Choice Difficulty x Choice Made
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Decision Window Start: Choice Difficulty x Choice Made",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(c(decision_start_Diff_Risky_lower, decision_start_Diff_Safe_lower)),
                                                      max(c(decision_start_Diff_Risky_upper, decision_start_Diff_Safe_upper)))) # use Easy to specify height
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Easy_Risky_upper,rev(decision_start_Easy_Risky_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Easy_Safe_upper,rev(decision_start_Easy_Safe_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_RvS_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_RvS_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Diff_Risky_upper,rev(decision_start_Diff_Risky_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Diff_Safe_upper,rev(decision_start_Diff_Safe_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_RvS_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_RvS_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy Trial", "Risky", "Safe", "Difficult Trial", "Risky", "Safe"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))

# Plotting NORMALIZED pupillometry: Decision Window Start to Choice:
# Choice Difficulty (Easy v. Difficult Trials) x Choice Made (Risky v. Safe Choice)
# Easy Trials x Choice Made
par(mfrow = c(2,1))
plot(1, type = 'n',
     xlab = "Normalized Time Points (200)", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Normalized Decision Window: From Start to Choice: Easy Trials x Choice Made",
     xlim = c(1, number_of_normBins), ylim = c(min(c(decision_norm_Easy_Risky_lower,decision_norm_Easy_Safe_lower)),
                                               max(c(decision_norm_Easy_Risky_upper,decision_norm_Easy_Safe_upper))))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Risky_upper,rev(decision_norm_Easy_Risky_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Safe_upper,rev(decision_norm_Easy_Safe_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_RvS_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_RvS_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
legend("bottomright", legend = c("Risky", "Safe"),
       col = c("red", "blue"), lty = c(1, 1))
# Difficult Trials x Choice Made
plot(1, type = 'n',
     xlab = "Normalized Time Points (200)", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Normalized Decision Window: From Start to Choice: Difficult Trials x Choice Made",
     xlim = c(1, number_of_normBins), ylim = c(min(c(decision_norm_Diff_Risky_lower,decision_norm_Diff_Safe_lower)),
                                               max(c(decision_norm_Diff_Risky_upper,decision_norm_Diff_Safe_upper))))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Risky_upper,rev(decision_norm_Diff_Risky_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Safe_upper,rev(decision_norm_Diff_Safe_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_RvS_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_RvS_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
legend("bottomright", legend = c("Risky", "Safe"),
       col = c("red", "blue"), lty = c(1, 1))
# Choice Difficulty x Choice Made
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Normalized Time Points (200)", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Normalized Decision Window: From Start to Choice: Choice Difficulty x Choice Made",
     xlim = c(1, number_of_normBins), ylim = c(min(c(decision_norm_Diff_Risky_lower,decision_norm_Diff_Safe_lower)),
                                               max(c(decision_norm_Diff_Risky_upper,decision_norm_Diff_Safe_upper))))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Risky_upper,rev(decision_norm_Easy_Risky_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Safe_upper,rev(decision_norm_Easy_Safe_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_RvS_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_RvS_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Risky_upper,rev(decision_norm_Diff_Risky_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Safe_upper,rev(decision_norm_Diff_Safe_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_RvS_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_RvS_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
legend("bottomright", legend = c("Easy Trial", "Risky", "Safe", "Difficult Trial", "Risky", "Safe"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))

# Plotting from Choice to ITI:
# Choice Difficulty (Easy v. Difficult Trials) x Choice Made (Risky v. Safe Choice)
# Easy Trials x Choice Made
par(mfrow = c(2,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: Easy Trials x Choice Made",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(dec_isi_otc_iti_Easy_Risky_lower, dec_isi_otc_iti_Easy_Safe_lower)),
                                                                             max(c(dec_isi_otc_iti_Easy_Risky_upper, dec_isi_otc_iti_Easy_Safe_upper))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(dec_isi_otc_iti_Easy_Risky_lower, dec_isi_otc_iti_Easy_Safe_lower)),
     labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Risky_upper,rev(dec_isi_otc_iti_Easy_Risky_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Safe_upper,rev(dec_isi_otc_iti_Easy_Safe_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Risky", "Safe"),
       col = c("red", "blue"), lty = c(1, 1))
# Difficult Trials x Choice Made
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: Difficult Trials x Choice Made",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(dec_isi_otc_iti_Diff_Risky_lower, dec_isi_otc_iti_Diff_Safe_lower)),
                                                                             max(c(dec_isi_otc_iti_Diff_Risky_upper, dec_isi_otc_iti_Diff_Safe_upper))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(dec_isi_otc_iti_Diff_Risky_lower, dec_isi_otc_iti_Diff_Safe_lower)),
     labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Risky_upper,rev(dec_isi_otc_iti_Diff_Risky_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Safe_upper,rev(dec_isi_otc_iti_Diff_Safe_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Risky", "Safe"),
       col = c("red", "blue"), lty = c(1, 1))
# Choice Difficulty x Choice Made
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: Choice Difficulty x Choice Made",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(dec_isi_otc_iti_Diff_Risky_lower, dec_isi_otc_iti_Diff_Safe_lower)),
                                                                             max(c(dec_isi_otc_iti_Diff_Risky_upper, dec_isi_otc_iti_Diff_Safe_upper))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(dec_isi_otc_iti_Easy_Risky_lower, dec_isi_otc_iti_Easy_Safe_lower)),
     labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Risky_upper,rev(dec_isi_otc_iti_Easy_Risky_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Safe_upper,rev(dec_isi_otc_iti_Easy_Safe_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Risky_upper,rev(dec_isi_otc_iti_Diff_Risky_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Safe_upper,rev(dec_isi_otc_iti_Diff_Safe_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_RvS_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy Trial", "Risky", "Safe", "Difficult Trial", "Risky", "Safe"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))

## - PREVIOUS DIFFICULTY - #####

# Plotting from Decision Window Start to Choice:
# Choice Difficulty (Easy v. Difficult Trials) x Previous Difficulty (Previous Easy v. Previous Difficult)
# Easy Trials x Previous Difficult
pdf(sprintf('%s/plots/mean_downsampled_decision_prev_EvD_groupOnly.pdf',config$path$data$processed),
    width = 8, height = 4)
par(mfrow = c(2,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Decision Window Start: Easy Trials x Previous Difficulty",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(c(decision_start_Easy_Prev_Easy_lower, decision_start_Easy_Prev_Diff_lower)),
                                                      max(c(decision_start_Easy_Prev_Easy_upper, decision_start_Easy_Prev_Diff_upper)))) # use Easy to specify height
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Easy_Prev_Easy_upper,rev(decision_start_Easy_Prev_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Easy_Prev_Diff_upper,rev(decision_start_Easy_Prev_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Previous Easy", "Previous Difficult"),
       col = c("blue", "red"), lty = c(1, 1))
# Difficult Trials x Previous Difficult
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Decision Window Start: Difficult Trials x Previous Difficulty",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(c(decision_start_Diff_Prev_Easy_lower, decision_start_Diff_Prev_Diff_lower)),
                                                      max(c(decision_start_Diff_Prev_Easy_upper, decision_start_Diff_Prev_Diff_upper)))) # use Easy to specify height
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Diff_Prev_Easy_upper,rev(decision_start_Diff_Prev_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Diff_Prev_Diff_upper,rev(decision_start_Diff_Prev_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Previous Easy", "Previous Difficult"),
       col = c("blue", "red"), lty = c(1, 1))
dev.off()
# Choice Difficulty x Previous Difficulty
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Decision Window Start: Choice Difficulty x Previous Difficulty",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(c(decision_start_Diff_Prev_Easy_lower, decision_start_Diff_Prev_Diff_lower)),
                                                      max(c(decision_start_Diff_Prev_Easy_upper, decision_start_Diff_Prev_Diff_upper)))) # use Easy to specify height
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Easy_Prev_Easy_upper,rev(decision_start_Easy_Prev_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Easy_Prev_Diff_upper,rev(decision_start_Easy_Prev_Diff_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Diff_Prev_Easy_upper,rev(decision_start_Diff_Prev_Easy_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Diff_Prev_Diff_upper,rev(decision_start_Diff_Prev_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy Trial", "Previous Easy", "Previous Difficult", "Difficult Trial", "Previous Easy", "Previous Difficult"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))

# Plotting NORMALIZED pupillometry: Decision Window Start to Choice:
# Choice Difficulty (Easy v. Difficult Trials) x Previous Difficulty (Previous Easy v. Difficult)
# Easy Trials x Previous Difficulty
par(mfrow = c(2,1))
pdf(sprintf('%s/plots/mean_downsampled_decision_normalized_EvD_RvS_groupOnly.pdf',config$path$data$processed),
    width = 8, height = 4)
plot(1, type = 'n',
     xlab = "Normalized Time Points (200)", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Normalized Decision Window: From Start to Choice: Easy Trials x Previous Difficulty",
     xlim = c(1, number_of_normBins), ylim = c(min(c(decision_norm_Easy_Prev_Easy_lower,decision_norm_Easy_Prev_Diff_lower)),
                                               max(c(decision_norm_Easy_Prev_Easy_upper,decision_norm_Easy_Prev_Diff_upper))))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Prev_Easy_upper,rev(decision_norm_Easy_Prev_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.2))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Prev_Diff_upper,rev(decision_norm_Easy_Prev_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.2))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = 'blue')
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = 'red')
legend("bottomright", legend = c("Previous Easy", "Previous Difficult"),
       col = c("blue", "red"), lty = c(1, 1))
# Difficult Trials x Previous Difficulty
plot(1, type = 'n',
     xlab = "Normalized Time Points (200)", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Normalized Decision Window: From Start to Choice: Difficult Trials x Previous Difficulty",
     xlim = c(1, number_of_normBins), ylim = c(min(c(decision_norm_Diff_Prev_Easy_lower,decision_norm_Diff_Prev_Diff_lower)),
                                               max(c(decision_norm_Diff_Prev_Easy_upper,decision_norm_Diff_Prev_Diff_upper))))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Prev_Easy_upper,rev(decision_norm_Diff_Prev_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.2))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Prev_Diff_upper,rev(decision_norm_Diff_Prev_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.2))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = 'blue')
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = 'red')
legend("bottomright", legend = c("Previous Easy", "Previous Difficult"),
       col = c("blue", "red"), lty = c(1, 1))
# Choice Difficulty x Previous Difficulty
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Normalized Time Points (200)", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Normalized Decision Window: From Start to Choice: Choice Difficulty x Previous Difficulty",
     xlim = c(1, number_of_normBins), ylim = c(min(c(decision_norm_Diff_Prev_Easy_lower,decision_norm_Diff_Prev_Diff_lower)),
                                               max(c(decision_norm_Diff_Prev_Easy_upper,decision_norm_Diff_Prev_Diff_upper))))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Prev_Easy_upper,rev(decision_norm_Easy_Prev_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.2))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Prev_Diff_upper,rev(decision_norm_Easy_Prev_Diff_lower)),
        lty = 0, col = rgb(0,0,1,.2))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = 'blue')
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = 'blue', lty = 2)
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Prev_Easy_upper,rev(decision_norm_Diff_Prev_Easy_lower)),
        lty = 0, col = rgb(1,0,0,.2))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Prev_Diff_upper,rev(decision_norm_Diff_Prev_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.2))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = 'red')
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = 'red', lty = 2)
legend("bottomright", legend = c("Easy Trial", "Previous Easy", "Previous Difficult", "Difficult Trial", "Previous Easy", "Previous Difficult"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))

# Plotting from Choice to ITI:
# Choice Difficulty (Easy v. Difficult Trials) x Previous Difficulty (Previous Easy v. Difficult)
# Easy Trials x Previous Difficulty
par(mfrow = c(2,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: Easy Trials x Choice Difficulty",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(dec_isi_otc_iti_Easy_Prev_Easy_lower, dec_isi_otc_iti_Easy_Prev_Diff_lower)),
                                                                             max(c(dec_isi_otc_iti_Easy_Prev_Easy_upper, dec_isi_otc_iti_Easy_Prev_Diff_upper))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(dec_isi_otc_iti_Easy_Prev_Easy_lower, dec_isi_otc_iti_Easy_Prev_Diff_lower)),
     labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Easy_upper,rev(dec_isi_otc_iti_Easy_Prev_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Diff_upper,rev(dec_isi_otc_iti_Easy_Prev_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Previous Easy", "Previous Difficult"),
       col = c("blue", "red"), lty = c(1, 1))
# Difficult Trials x Previous Difficulty
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: Difficult Trials x Previous Difficulty",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(dec_isi_otc_iti_Diff_Prev_Easy_lower, dec_isi_otc_iti_Diff_Prev_Diff_lower)),
                                                                             max(c(dec_isi_otc_iti_Diff_Prev_Easy_upper, dec_isi_otc_iti_Diff_Prev_Diff_upper))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(dec_isi_otc_iti_Diff_Prev_Easy_lower, dec_isi_otc_iti_Diff_Prev_Diff_lower)),
     labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Easy_upper,rev(dec_isi_otc_iti_Diff_Prev_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Diff_upper,rev(dec_isi_otc_iti_Diff_Prev_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Previous Easy", "Previous Difficult"),
       col = c("blue", "red"), lty = c(1, 1))
# Choice Difficulty x Previous Difficulty
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: Choice Difficulty x Previous Difficulty",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(min(c(dec_isi_otc_iti_Easy_Prev_Easy_lower, dec_isi_otc_iti_Easy_Prev_Diff_lower)),
                                                                                   min(c(dec_isi_otc_iti_Diff_Prev_Easy_lower, dec_isi_otc_iti_Diff_Prev_Diff_lower)))),
                                                                             max(c(max(c(dec_isi_otc_iti_Easy_Prev_Easy_upper, dec_isi_otc_iti_Easy_Prev_Diff_upper)),
                                                                                   max(c(dec_isi_otc_iti_Diff_Prev_Easy_upper, dec_isi_otc_iti_Diff_Prev_Diff_upper))))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(min(c(dec_isi_otc_iti_Easy_Prev_Easy_lower, dec_isi_otc_iti_Easy_Prev_Diff_lower)),
                                        min(c(dec_isi_otc_iti_Diff_Prev_Easy_lower, dec_isi_otc_iti_Diff_Prev_Diff_lower)))), labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Easy_upper,rev(dec_isi_otc_iti_Easy_Prev_Easy_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Diff_upper,rev(dec_isi_otc_iti_Easy_Prev_Diff_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Easy_upper,rev(dec_isi_otc_iti_Diff_Prev_Easy_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Diff_upper,rev(dec_isi_otc_iti_Diff_Prev_Diff_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy Trial", "Previous Easy", "Previous Difficult", "Difficult Trial", "Previous Easy", "Previous Difficult"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))

## - WMC - ######

# Plotting from Decision Window Start to Choice & from Choice to Decision Window End:
# Choice Difficulty (Easy v. Difficult Trials) x WMC (Low vs. High)
# Low WMC x Choice Difficulty
par(mfrow = c(2,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Decision Window Start: Low WMC x Choice Difficulty",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(c(decision_start_Easy_lWMC_lower, decision_start_Diff_lWMC_lower)),
                                                      max(c(decision_start_Easy_lWMC_upper, decision_start_Diff_lWMC_upper)))) #
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Easy_lWMC_upper,rev(decision_start_Easy_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Diff_lWMC_upper,rev(decision_start_Diff_lWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_WMC_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_WMC_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy", "Difficult"),
       col = c("blue", "red"), lty = c(1, 1))
# High WMC x Choice Difficulty
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Decision Window Start: High WMC x Choice Difficulty",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(c(decision_start_Easy_hWMC_lower, decision_start_Diff_hWMC_lower)),
                                                      max(c(decision_start_Easy_hWMC_upper, decision_start_Diff_hWMC_upper)))) #
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Easy_hWMC_upper,rev(decision_start_Easy_hWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_start_x_vals,y = c(decision_start_Diff_hWMC_upper,rev(decision_start_Diff_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_WMC_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_EvD_WMC_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy", "Difficult"),
       col = c("blue", "red"), lty = c(1, 1))

# Plotting NORMALIZED pupillometry: Decision Window Start to Choice:
# Choice Difficulty (Easy v. Difficult Trials) x WMC (Low vs. High)
# Low WMC x Choice Difficulty
par(mfrow = c(2,1))
plot(1, type = 'n',
     xlab = "Normalized Time Points (200)", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Normalized Decision Window: From Start to Choice: Low WMC x Choice Difficulty",
     xlim = c(1, number_of_normBins), ylim = c(min(c(decision_norm_Easy_lWMC_lower,decision_norm_Diff_lWMC_lower)),
                                               max(c(decision_norm_Easy_lWMC_upper,decision_norm_Diff_lWMC_upper))))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_lWMC_upper,rev(decision_norm_Easy_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_lWMC_upper,rev(decision_norm_Diff_lWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_WMC_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_WMC_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
legend("bottomright", legend = c("Easy", "Difficult"),
       col = c("blue", "red"), lty = c(1, 1))
# High WMC x Choice Difficulty
plot(1, type = 'n',
     xlab = "Normalized Time Points (200)", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Normalized Decision Window: From Start to Choice: High WMC x Choice Difficulty",
     xlim = c(1, number_of_normBins), ylim = c(min(c(decision_norm_Easy_lWMC_lower,decision_norm_Diff_lWMC_lower)),
                                               max(c(decision_norm_Easy_lWMC_upper,decision_norm_Diff_lWMC_upper))))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_lWMC_upper,rev(decision_norm_Easy_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_lWMC_upper,rev(decision_norm_Diff_lWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_WMC_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_EvD_WMC_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
legend("bottomright", legend = c("Easy", "Difficult"),
       col = c("blue", "red"), lty = c(1, 1))

# Plotting from Choice to ITI:
# Choice Difficulty (Easy v. Difficult Trials) x WMC (Low vs. High)
# Low WMC x Choice Difficulty
par(mfrow = c(2,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: Low WMC x Choice Difficulty",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(dec_isi_otc_iti_Easy_lWMC_lower, dec_isi_otc_iti_Diff_lWMC_lower)),
                                                                             max(c(dec_isi_otc_iti_Easy_lWMC_upper, dec_isi_otc_iti_Diff_lWMC_upper))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(dec_isi_otc_iti_Easy_lWMC_lower, dec_isi_otc_iti_Diff_lWMC_lower)),
     labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_lWMC_upper,rev(dec_isi_otc_iti_Easy_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_lWMC_upper,rev(dec_isi_otc_iti_Diff_lWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy", "Low"),
       col = c("blue", "red"), lty = c(1, 1))
# High WMC x Choice Difficulty
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: High WMC x Choice Difficulty",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(dec_isi_otc_iti_Easy_hWMC_lower, dec_isi_otc_iti_Diff_hWMC_lower)),
                                                                             max(c(dec_isi_otc_iti_Easy_hWMC_upper, dec_isi_otc_iti_Diff_hWMC_upper))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(dec_isi_otc_iti_Easy_hWMC_lower, dec_isi_otc_iti_Diff_hWMC_lower)),
     labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_hWMC_upper,rev(dec_isi_otc_iti_Easy_hWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_hWMC_upper,rev(dec_isi_otc_iti_Diff_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_EvD_WMC_array[,,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy", "Difficult"),
       col = c("blue", "red"), lty = c(1, 1))


## - WMC x PREVIOUS DIFFICULTY x CURRENT DIFFICULTY #####

# Plotting from Decision Window Start to Choice & from Choice to Decision Window End:
# Choice Difficulty (Easy v. Difficult Trials) x WMC (Low vs. High)
# Low WMC x Choice Difficulty x Previous Difficulty
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: Low WMC x Current Difficulty x Previous Difficulty",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(c(min(c(decision_start_Easy_Prev_Easy_lWMC_lower, decision_start_Diff_Prev_Easy_lWMC_lower)),
                                                            min(c(decision_start_Easy_Prev_Diff_lWMC_lower, decision_start_Diff_Prev_Diff_lWMC_lower)))),
                                                      max(c(max(c(decision_start_Easy_Prev_Easy_lWMC_upper, decision_start_Diff_Prev_Easy_lWMC_upper)),
                                                            max(c(decision_start_Easy_Prev_Diff_lWMC_upper, decision_start_Diff_Prev_Diff_lWMC_upper))))))
polygon(x = sem_decision_start_x_vals,
        y = c(decision_start_Easy_Prev_Easy_lWMC_upper,rev(decision_start_Easy_Prev_Easy_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_start_x_vals,
        y = c(decision_start_Easy_Prev_Diff_lWMC_upper,rev(decision_start_Easy_Prev_Diff_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_decision_start_x_vals,
        y = c(decision_start_Diff_Prev_Easy_lWMC_upper,rev(decision_start_Diff_Prev_Easy_lWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_decision_start_x_vals,
        y = c(decision_start_Diff_Prev_Diff_lWMC_upper,rev(decision_start_Diff_Prev_Diff_lWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy Trial", "Previous Easy", "Previous Difficult", "Difficult Trial", "Previous Easy", "Previous Difficult"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))
# High WMC x Choice Difficulty x Previous Difficulty
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: High WMC x Current Difficulty x Previous Difficulty",
     xlim = c(-baseline_window_width, 3000), ylim = c(min(c(min(c(decision_start_Easy_Prev_Easy_hWMC_lower, decision_start_Diff_Prev_Easy_hWMC_lower)),
                                                            min(c(decision_start_Easy_Prev_Diff_hWMC_lower, decision_start_Diff_Prev_Diff_hWMC_lower)))),
                                                      max(c(max(c(decision_start_Easy_Prev_Easy_hWMC_upper, decision_start_Diff_Prev_Easy_hWMC_upper)),
                                                            max(c(decision_start_Easy_Prev_Diff_hWMC_upper, decision_start_Diff_Prev_Diff_hWMC_upper))))))
polygon(x = sem_decision_start_x_vals,
        y = c(decision_start_Easy_Prev_Easy_hWMC_upper,rev(decision_start_Easy_Prev_Easy_hWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_start_x_vals,
        y = c(decision_start_Easy_Prev_Diff_hWMC_upper,rev(decision_start_Easy_Prev_Diff_hWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,1,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_decision_start_x_vals,
        y = c(decision_start_Diff_Prev_Easy_hWMC_upper,rev(decision_start_Diff_Prev_Easy_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_decision_start_x_vals,
        y = c(decision_start_Diff_Prev_Diff_hWMC_upper,rev(decision_start_Diff_Prev_Diff_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = decision_start_bins[1:(length(decision_start_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_decision_start_prev_EvD_WMC_array[,,2,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy Trial", "Previous Easy", "Previous Difficult", "Difficult Trial", "Previous Easy", "Previous Difficult"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))

# Plotting NORMALIZED pupillometry: Decision Window Start to Choice:
# Choice Difficulty (Easy v. Difficult Trials) x WMC (Low vs. High)
# Low WMC x Choice Difficulty x Previous Difficulty
par(mfrow = c(1,1))
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: Low WMC x Current Difficulty x Previous Difficulty",
     xlim = c(1, number_of_normBins), ylim = c(min(c(min(c(decision_norm_Easy_Prev_Easy_lWMC_lower, decision_norm_Diff_Prev_Easy_lWMC_lower)),
                                                     min(c(decision_norm_Easy_Prev_Diff_lWMC_lower, decision_norm_Diff_Prev_Diff_lWMC_lower)))),
                                               max(c(max(c(decision_norm_Easy_Prev_Easy_lWMC_upper, decision_norm_Diff_Prev_Easy_lWMC_upper)),
                                                     max(c(decision_norm_Easy_Prev_Diff_lWMC_upper, decision_norm_Diff_Prev_Diff_lWMC_upper))))))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Prev_Easy_lWMC_upper,rev(decision_norm_Easy_Prev_Easy_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Prev_Diff_lWMC_upper,rev(decision_norm_Easy_Prev_Diff_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Prev_Easy_lWMC_upper,rev(decision_norm_Diff_Prev_Easy_lWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Prev_Diff_lWMC_upper,rev(decision_norm_Diff_Prev_Diff_lWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
legend("bottomright", legend = c("Easy Trial", "Previous Easy", "Previous Difficult", "Difficult Trial", "Previous Easy", "Previous Difficult"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))
# High WMC x Choice Difficulty x Previous Difficulty
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: High WMC x Current Difficulty x Previous Difficulty",
     xlim = c(1, number_of_normBins), ylim = c(min(c(min(c(decision_norm_Easy_Prev_Easy_hWMC_lower, decision_norm_Diff_Prev_Easy_hWMC_lower)),
                                                     min(c(decision_norm_Easy_Prev_Diff_hWMC_lower, decision_norm_Diff_Prev_Diff_hWMC_lower)))),
                                               max(c(max(c(decision_norm_Easy_Prev_Easy_hWMC_upper, decision_norm_Diff_Prev_Easy_hWMC_upper)),
                                                     max(c(decision_norm_Easy_Prev_Diff_hWMC_upper, decision_norm_Diff_Prev_Diff_hWMC_upper))))))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Prev_Easy_hWMC_upper,rev(decision_norm_Easy_Prev_Easy_hWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Easy_Prev_Diff_hWMC_upper,rev(decision_norm_Easy_Prev_Diff_hWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,1,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Prev_Easy_hWMC_upper,rev(decision_norm_Diff_Prev_Easy_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_decision_norm_x_vals,
        y = c(decision_norm_Diff_Prev_Diff_hWMC_upper,rev(decision_norm_Diff_Prev_Diff_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = normBins,
      y = rowMeans(mean_decision_norm_prev_EvD_WMC_array[,,2,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
legend("bottomright", legend = c("Easy Trial", "Previous Easy", "Previous Difficult", "Difficult Trial", "Previous Easy", "Previous Difficult"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))

# Plotting from Choice to ITI:
# Choice Difficulty (Easy v. Difficult Trials) x WMC (Low vs. High)
# Low WMC x Choice Difficulty x Previous Difficulty
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: Low WMC x Current Difficulty x Previous Difficulty",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(min(c(dec_isi_otc_iti_Easy_Prev_Easy_lWMC_lower, dec_isi_otc_iti_Diff_Prev_Easy_lWMC_lower)),
                                                                                   min(c(dec_isi_otc_iti_Easy_Prev_Diff_lWMC_lower, dec_isi_otc_iti_Diff_Prev_Diff_lWMC_lower)))),
                                                                             max(c(max(c(dec_isi_otc_iti_Easy_Prev_Easy_lWMC_upper, dec_isi_otc_iti_Diff_Prev_Easy_lWMC_upper)),
                                                                                   max(c(dec_isi_otc_iti_Easy_Prev_Diff_lWMC_upper, dec_isi_otc_iti_Diff_Prev_Diff_lWMC_upper))))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(min(c(dec_isi_otc_iti_Easy_Prev_Easy_lWMC_lower, dec_isi_otc_iti_Diff_Prev_Easy_lWMC_lower)),
                                        min(c(dec_isi_otc_iti_Easy_Prev_Diff_lWMC_lower, dec_isi_otc_iti_Diff_Prev_Diff_lWMC_lower)))), labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Easy_lWMC_upper,rev(dec_isi_otc_iti_Easy_Prev_Easy_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Diff_lWMC_upper,rev(dec_isi_otc_iti_Easy_Prev_Diff_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Easy_lWMC_upper,rev(dec_isi_otc_iti_Diff_Prev_Easy_lWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Diff_lWMC_upper,rev(dec_isi_otc_iti_Diff_Prev_Diff_lWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy Trial", "Previous Easy", "Previous Difficult", "Difficult Trial", "Previous Easy", "Previous Difficult"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))
# High WMC x Choice Difficulty x Previous Difficulty
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: High WMC x Current Difficulty x Previous Difficulty",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(min(c(dec_isi_otc_iti_Easy_Prev_Easy_hWMC_lower, dec_isi_otc_iti_Diff_Prev_Easy_hWMC_lower)),
                                                                                   min(c(dec_isi_otc_iti_Easy_Prev_Diff_hWMC_lower, dec_isi_otc_iti_Diff_Prev_Diff_hWMC_lower)))),
                                                                             max(c(max(c(dec_isi_otc_iti_Easy_Prev_Easy_hWMC_upper, dec_isi_otc_iti_Diff_Prev_Easy_hWMC_upper)),
                                                                                   max(c(dec_isi_otc_iti_Easy_Prev_Diff_hWMC_upper, dec_isi_otc_iti_Diff_Prev_Diff_hWMC_upper))))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(min(c(dec_isi_otc_iti_Easy_Prev_Easy_hWMC_lower, dec_isi_otc_iti_Diff_Prev_Easy_hWMC_lower)),
                                        min(c(dec_isi_otc_iti_Easy_Prev_Diff_hWMC_lower, dec_isi_otc_iti_Diff_Prev_Diff_hWMC_lower)))), labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Easy_hWMC_upper,rev(dec_isi_otc_iti_Easy_Prev_Easy_hWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Diff_hWMC_upper,rev(dec_isi_otc_iti_Easy_Prev_Diff_hWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Easy_hWMC_upper,rev(dec_isi_otc_iti_Diff_Prev_Easy_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Diff_hWMC_upper,rev(dec_isi_otc_iti_Diff_Prev_Diff_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Easy Trial", "Previous Easy", "Previous Difficult", "Difficult Trial", "Previous Easy", "Previous Difficult"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))
# WMC x Choice Difficulty x Previous Easy Only
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: WMC x Current Difficulty x Previous Easy Only",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(min(c(dec_isi_otc_iti_Easy_Prev_Easy_lWMC_lower, dec_isi_otc_iti_Diff_Prev_Easy_lWMC_lower)),
                                                                                   min(c(dec_isi_otc_iti_Easy_Prev_Easy_hWMC_lower, dec_isi_otc_iti_Diff_Prev_Easy_hWMC_lower)))),
                                                                             max(c(max(c(dec_isi_otc_iti_Easy_Prev_Easy_lWMC_upper, dec_isi_otc_iti_Diff_Prev_Easy_lWMC_upper)),
                                                                                   max(c(dec_isi_otc_iti_Easy_Prev_Easy_hWMC_upper, dec_isi_otc_iti_Diff_Prev_Easy_hWMC_upper))))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(min(c(dec_isi_otc_iti_Easy_Prev_Easy_lWMC_lower, dec_isi_otc_iti_Diff_Prev_Easy_lWMC_lower)),
                                        min(c(dec_isi_otc_iti_Easy_Prev_Easy_hWMC_lower, dec_isi_otc_iti_Diff_Prev_Easy_hWMC_lower)))), labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Easy_lWMC_upper,rev(dec_isi_otc_iti_Easy_Prev_Easy_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Easy_lWMC_upper,rev(dec_isi_otc_iti_Diff_Prev_Easy_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,1,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Easy_hWMC_upper,rev(dec_isi_otc_iti_Easy_Prev_Easy_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Easy_hWMC_upper,rev(dec_isi_otc_iti_Diff_Prev_Easy_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,1,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Low WMC", "Easy", "Difficult", "High WMC", "Easy", "Difficult"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))
# WMC x Choice Difficulty x Previous Difficult Only
plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice: WMC x Current Difficulty x Previous Difficult Only",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(c(min(c(dec_isi_otc_iti_Easy_Prev_Diff_lWMC_lower, dec_isi_otc_iti_Diff_Prev_Diff_lWMC_lower)),
                                                                                   min(c(dec_isi_otc_iti_Easy_Prev_Diff_hWMC_lower, dec_isi_otc_iti_Diff_Prev_Diff_hWMC_lower)))),
                                                                             max(c(max(c(dec_isi_otc_iti_Easy_Prev_Diff_lWMC_upper, dec_isi_otc_iti_Diff_Prev_Diff_lWMC_upper)),
                                                                                   max(c(dec_isi_otc_iti_Easy_Prev_Diff_hWMC_upper, dec_isi_otc_iti_Diff_Prev_Diff_hWMC_upper))))))
polygon(x = c(1000, 2000, 2000, 1000), # outcome
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(1000, 2000)), y = min(c(min(c(dec_isi_otc_iti_Easy_Prev_Diff_lWMC_lower, dec_isi_otc_iti_Diff_Prev_Diff_lWMC_lower)),
                                        min(c(dec_isi_otc_iti_Easy_Prev_Diff_hWMC_lower, dec_isi_otc_iti_Diff_Prev_Diff_hWMC_lower)))), labels = "Outcome")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Diff_lWMC_upper,rev(dec_isi_otc_iti_Easy_Prev_Diff_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Diff_lWMC_upper,rev(dec_isi_otc_iti_Diff_Prev_Diff_lWMC_lower)),
        lty = 0, col = rgb(0,0,1,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,2,1], na.rm = T), type = 'l',
      lwd = 3, col = rgb(0,0,1), lty = 2)
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Easy_Prev_Diff_hWMC_upper,rev(dec_isi_otc_iti_Easy_Prev_Diff_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_Diff_Prev_Diff_hWMC_upper,rev(dec_isi_otc_iti_Diff_Prev_Diff_hWMC_lower)),
        lty = 0, col = rgb(1,0,0,.1))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,1,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_prev_EvD_WMC_array[,,2,2,2], na.rm = T), type = 'l',
      lwd = 3, col = rgb(1,0,0), lty = 2)
abline(v = 0, lty = 'dashed')
legend("bottomright", legend = c("Low WMC", "Easy", "Difficult", "High WMC", "Easy", "Difficult"),  # Labels
       col = c(NA, "blue", "blue", NA, "red", "red"),  # Blue for Easy, Red for Difficult
       lty = c(NA, 1, 2, NA, 1, 2))







dev.off()
# PUPILLOMETRY REGRESSIONS #################
par(mfrow = c(1,1))

cor_matrix = cor(clean_data_dm[,c('wind1_predisp_onset_mean','wind2_effort_isi_mean','wind3_eval_otciti_mean','wind4_prep_lateiti_mean')],
                                   use = 'complete.obs')
corrplot(cor_matrix, type = 'lower')

sqrtRT_m0_diffCat_optCat_prevdiffCat_intxn_rfx = lmer(sqrtRT ~ 1 + easyP1difficultN1 * choice * easyP1difficultN1_prev +
                                                                (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(sqrtRT_m0_diffCat_optCat_prevdiffCat_intxn_rfx)
# easyP1difficultN1                               -4.994e-02  2.394e-03  1.009e+04 -20.864  < 2e-16 ***
# choice                                          -6.453e-03  3.494e-03  1.011e+04  -1.847  0.06483 .
# easyP1difficultN1_prev                           5.943e-04  2.376e-03  1.008e+04   0.250  0.80248
# easyP1difficultN1:choice                         9.383e-03  3.455e-03  1.010e+04   2.716  0.00663 **
# easyP1difficultN1:easyP1difficultN1_prev        -2.381e-03  2.381e-03  1.008e+04  -1.000  0.31726
# choice:easyP1difficultN1_prev                    6.160e-03  3.389e-03  1.008e+04   1.818  0.06913 .
# easyP1difficultN1:choice:easyP1difficultN1_prev  3.555e-03  3.395e-03  1.008e+04   1.047  0.29502

sqrtRT_m1_diffContAll_optCat_prevdiffContAll_intxn_rfx = lmer(sqrtRT ~ 1 + all_diff_cont * choice * prev_all_diff_cont +
                                                                (1 | subjectnumber), data = clean_data_dm)
summary(sqrtRT_m1_diffContAll_optCat_prevdiffContAll_intxn_rfx)
# all_diff_cont                            1.259e-01  8.150e-03  1.417e+04  15.446  < 2e-16 ***
# choice                                   2.194e-02  7.199e-03  1.416e+04   3.047 0.002314 **
# prev_all_diff_cont                      -2.525e-02  8.279e-03  1.416e+04  -3.049 0.002297 **
# all_diff_cont:choice                    -3.802e-02  1.145e-02  1.416e+04  -3.322 0.000896 ***
# all_diff_cont:prev_all_diff_cont        -3.727e-03  1.225e-02  1.416e+04  -0.304 0.760843
# choice:prev_all_diff_cont               -1.573e-02  1.135e-02  1.415e+04  -1.386 0.165711
# all_diff_cont:choice:prev_all_diff_cont  1.647e-02  1.712e-02  1.415e+04   0.962 0.336069

sqrtRT_m1.2_diffContAll_optCat_prevdiffContAll_intxn_rfx = lmer(sqrtRT ~ 1 + all_diff_cont * choice + prev_all_diff_cont * choice +
                                                                (1 | subjectnumber), data = clean_data_dm)
summary(sqrtRT_m1.2_diffContAll_optCat_prevdiffContAll_intxn_rfx)
# all_diff_cont              1.240e-01  5.283e-03  1.417e+04  23.471  < 2e-16 ***
# choice                     1.792e-02  5.805e-03  1.416e+04   3.087  0.00203 **
# prev_all_diff_cont        -2.723e-02  5.099e-03  1.416e+04  -5.340 9.42e-08 ***
# all_diff_cont:choice      -2.954e-02  7.353e-03  1.417e+04  -4.018 5.91e-05 ***
# choice:prev_all_diff_cont -7.490e-03  7.201e-03  1.415e+04  -1.040  0.29828

sqrtRT_m1_diffContAll_optCat_wmcCat_intxn_rfx = lmer(sqrtRT ~ 1 + all_diff_cont * choice * capacity_HighP1_lowN1_best +
                                                                (1 | subjectnumber), data = clean_data_dm)
summary(sqrtRT_m1_diffContAll_optCat_wmcCat_intxn_rfx)
# all_diff_cont                                    1.301e-01  5.531e-03  1.382e+04  23.520  < 2e-16 ***
# choice                                           1.019e-02  4.927e-03  1.381e+04   2.067 0.038708 *
# capacity_HighP1_lowN1_best                      -5.589e-03  1.259e-02  9.160e+01  -0.444 0.658256
# all_diff_cont:choice                            -3.142e-02  7.799e-03  1.382e+04  -4.029 5.63e-05 ***
# all_diff_cont:capacity_HighP1_lowN1_best         2.056e-02  5.531e-03  1.382e+04   3.718 0.000202 ***
# choice:capacity_HighP1_lowN1_best               -1.545e-02  4.927e-03  1.381e+04  -3.136 0.001716 **
# all_diff_cont:choice:capacity_HighP1_lowN1_best  6.806e-03  7.799e-03  1.382e+04   0.873 0.382802


# TODO
# FIXME
# CHANGED



# PREDICTORS
# ~ trial number
# ~ Current Difficulty and Previous Difficulty
# ~ Choice and Outcome
# ~ WMC, NCS, PSS, and SNS
# ~ quantitative post-study questionnaire

# W1 - WMC, CD, PD
# W2 - WMC, CD, PD
# W3 - WMC, CD, PD
# W4 - WMC, CD, PD

# Question: What difference is there between predisposition and preparation?
# - predisposition takes 500ms from the iti of the previous trial and 500ms from the instructions before the first trial
# -- from the second trial onward, they are being influenced by the previous trial, which is like preparation
# -- some of the iti are 3000ms while others are 3500ms, which makes the 500ms not match for everyone exactly, but may not matter
# -- only the first trial is different from all the other trials, which might reflect a a predisposition not influenced by previous difficulty
# - preparation is from +1000ms to +3000ms into the iti


# Outcome - we have the monetary value but not coded whether or not received the gain or the loss
# For each participant I need to get what choice they made (safe = 0 or risky = 1) and whether their outcome was a gain (more than 0) or a loss (0)
# if choice == 0 & outcome > 0 then 0 = safeGain
# if choice == 1 & outcome = 0 then 1 = riskyGain
# if choice == 1 & outcome > 0 then -1 = riskyLoss


# Considerations for SANS
# Pupillometry regressions that mirror similar models used for RTs
# ~ Current Difficulty
# ~ Current Difficulty x  Previous Difficulty
# ~ Current Difficulty x Previous Difficulty x WMC
# The main difference being trial number as a predictor




# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### WINDOW 1 REGRESSIONS: Predisposition (1 second, -500 before choice onset to +500 after choice onset) #####
# ~ predictors: current difficulty, choice made, previous difficulty, WMC, NFC, and choice

### single predictor models
# ~ Window 1 Model 0: current difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind1_m0_diffCat = lm(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind1_m0_diffCat) # easyP1difficultN1 0.0002106  0.0074478   0.028    0.977

wind1_m0_diffCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m0_diffCat_rfx) # easyP1difficultN1 3.227e-04  3.112e-03 9.822e+03   0.104    0.917

wind1_m0_diffCont = lm(wind1_predisp_onset_mean ~ 1 + diff_cont, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous dynamic only
summary(wind1_m0_diffCont) # diff_cont   -0.003317   0.016043  -0.207    0.836

wind1_m0_diffCont_rfx = lmer(wind1_predisp_onset_mean ~ 1 + diff_cont + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m0_diffCont_rfx) # diff_cont   -9.499e-04  6.707e-03  9.822e+03  -0.142    0.887

wind1_m0_diffContAll = lm(wind1_predisp_onset_mean ~ 1 + all_diff_cont, data = clean_data_dm) # continuous all
summary(wind1_m0_diffContAll) # all_diff_cont -0.07009    0.01534  -4.568 4.96e-06 ***

wind1_m0_diffContAll_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind1_m0_diffContAll_rfx) # all_diff_cont -3.794e-02  6.507e-03  1.395e+04  -5.831 5.63e-09 ***

AIC(wind1_m0_diffContAll) # 31664.36
AIC(wind1_m0_diffContAll_rfx) # 7647.008 - the better model

# ~ Window 1 Model 0: previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind1_m0_prevdiffCat = lm(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1_prev, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind1_m0_prevdiffCat) # easyP1difficultN1_prev -0.010688   0.007478  -1.429    0.153

wind1_m0_prevdiffCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1_prev + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m0_prevdiffCat_rfx) # easyP1difficultN1_prev -9.630e-03  3.124e-03  9.822e+03  -3.082  0.00206 **

wind1_m0_prevdiffContAll = lm(wind1_predisp_onset_mean ~ 1 + prev_all_diff_cont, data = clean_data_dm) # continuous all
summary(wind1_m0_prevdiffContAll) # prev_all_diff_cont -0.04873    0.01545  -3.154  0.00162 **

wind1_m0_prevdiffContAll_rfx = lmer(wind1_predisp_onset_mean ~ 1 + prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind1_m0_prevdiffContAll_rfx) # prev_all_diff_cont -1.690e-02  6.535e-03  1.381e+04  -2.586  0.00973 **

AIC(wind1_m0_prevdiffCat_rfx) # 5430.5
AIC(wind1_m0_prevdiffContAll_rfx) # 7542.62

# ~ Window 1 Model 0: capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind1_m0_wmcCat = lm(wind1_predisp_onset_mean ~ 1 + capacity_HighP1_lowN1_best, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind1_m0_wmcCat) # capacity_HighP1_lowN1_best 0.117947   0.007987   14.77   <2e-16 ***

wind1_m0_wmcCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m0_wmcCat_rfx) # capacity_HighP1_lowN1_best  0.11906    0.08007 79.00058   1.487    0.141

wind1_m0_wmcCont = lm(wind1_predisp_onset_mean ~ 1 + complexspan_demeaned, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous
summary(wind1_m0_wmcCont) # complexspan_demeaned  0.66198    0.03825    17.3   <2e-16 ***

wind1_m0_wmcCont_rfx = lmer(wind1_predisp_onset_mean ~ 1 + complexspan_demeaned + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m0_wmcCont_rfx) # complexspan_demeaned  0.66626    0.38292 79.00102    1.74   0.0858 .

AIC(wind1_m0_wmcCat) # 21448.59

# ~ Window 1 Model 0: need for cognition scale (ncs -> nfc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind1_m0_nfcCat = lm(wind1_predisp_onset_mean ~ 1 + NCS_HighP1_LowN1, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind1_m0_nfcCat) # NCS_HighP1_LowN1 -0.031024   0.007539  -4.115  3.9e-05 ***

wind1_m0_nfcCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + NCS_HighP1_LowN1 + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m0_nfcCat_rfx) # NCS_HighP1_LowN1 -0.03073    0.07572 80.00065  -0.406    0.686

wind1_m0_nfcCont = lm(wind1_predisp_onset_mean ~ 1 + NCS, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous
summary(wind1_m0_nfcCont) # NCS         0.0008047  0.0007061    1.14    0.254

wind1_m0_nfcCont_rfx = lmer(wind1_predisp_onset_mean ~ 1 + NCS + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m0_nfcCont_rfx) # NCS         8.067e-04  7.094e-03 8.000e+01   0.114     0.91

AIC(wind1_m0_nfcCat) # 21986.32

# ~ Window 2 Model 0: choice made ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind1_m0_optCat = lm(wind1_predisp_onset_mean ~ 1 + choice, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical choice made
summary(wind1_m0_optCat) # choice       0.06560    0.01488   4.408 1.05e-05 ***

wind1_m0_optCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + choice + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m0_optCat_rfx) # choice      1.326e-02  6.461e-03 9.825e+03   2.053   0.0401 *

AIC(wind1_m0_optCat) # 22165.01
AIC(wind1_m0_optCat_rfx) # 5434.33 - the better model

### main & interaction effects
# ~ Window 1 Model 1: current difficulty x choice made ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind1_m1_diffCat_optCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 + choice +
                                     (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & choice made
summary(wind1_m1_diffCat_optCat_rfx)
# easyP1difficultN1 2.318e-04  3.112e-03 9.821e+03   0.074   0.9406
# choice            1.326e-02  6.462e-03 9.824e+03   2.051   0.0403 *

wind1_m1_diffCat_optCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 * choice +
                                           (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m1_diffCat_optCat_intxn_rfx)
# easyP1difficultN1         1.528e-03  4.429e-03  9.821e+03   0.345   0.7301
# choice                    1.342e-02  6.474e-03  9.823e+03   2.073   0.0382 *
# easyP1difficultN1:choice -2.632e-03  6.398e-03  9.822e+03  -0.411   0.6808

wind1_m1_diffContAll_optCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont + choice +
                                         (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & categorical choice made
summary(wind1_m1_diffContAll_optCat_rfx)
# all_diff_cont -3.767e-02  6.518e-03  1.395e+04   -5.78 7.63e-09 ***
# choice         3.878e-03  5.388e-03  1.395e+04    0.72    0.472

wind1_m1_diffContAll_optCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont * choice +
                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind1_m1_diffContAll_optCat_intxn_rfx)
# all_diff_cont        -3.976e-02  9.426e-03  1.394e+04  -4.218 2.48e-05 ***
# choice                1.888e-03  8.443e-03  1.394e+04   0.224    0.823
# all_diff_cont:choice  4.014e-03  1.311e-02  1.394e+04   0.306    0.759

# ~ Window 1 Model 2: current difficulty x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind1_m2_diffCat_prevdiffCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 + easyP1difficultN1_prev +
                                          (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & previous difficulty
summary(wind1_m2_diffCat_prevdiffCat_rfx)
# easyP1difficultN1       1.584e-04  3.112e-03  9.821e+03   0.051  0.95940
# easyP1difficultN1_prev -9.627e-03  3.125e-03  9.821e+03  -3.081  0.00207 **

wind1_m2_diffCat_prevdiffCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev +
                                                (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m2_diffCat_prevdiffCat_intxn_rfx)
# easyP1difficultN1                         1.584e-04  3.112e-03  9.820e+03   0.051  0.95942
# easyP1difficultN1_prev                   -9.628e-03  3.125e-03  9.820e+03  -3.081  0.00207 **
# easyP1difficultN1:easyP1difficultN1_prev  3.532e-04  3.139e-03  9.820e+03   0.113  0.91041

wind1_m2_diffContAll_prevdiffContAll_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont + prev_all_diff_cont +
                                                  (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & previous difficulty
summary(wind1_m2_diffContAll_prevdiffContAll_rfx)
# all_diff_cont      -3.659e-02  6.524e-03  1.381e+04  -5.608 2.08e-08 ***
# prev_all_diff_cont -1.517e-02  6.535e-03  1.381e+04  -2.322   0.0203 *

wind1_m2_diffContAll_prevdiffContAll_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont * prev_all_diff_cont +
                                                        (1 | subjectnumber), data = clean_data_dm)
summary(wind1_m2_diffContAll_prevdiffContAll_intxn_rfx)
# all_diff_cont                    -5.144e-02  1.018e-02  1.381e+04  -5.052 4.44e-07 ***
# prev_all_diff_cont               -3.007e-02  1.021e-02  1.381e+04  -2.945  0.00323 **
# all_diff_cont:prev_all_diff_cont  2.906e-02  1.530e-02  1.381e+04   1.899  0.05757 .

# ~ Window 1 Model 3: current difficulty x capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind1_m3_diffCat_wmcCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 + capacity_HighP1_lowN1_best +
                                     (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & wmc
summary(wind1_m3_diffCat_wmcCat_rfx)
# easyP1difficultN1          -9.626e-05  3.150e-03  9.586e+03  -0.031    0.976
# capacity_HighP1_lowN1_best  1.191e-01  8.007e-02  7.900e+01   1.487    0.141

wind1_m3_diffCat_wmcCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 * capacity_HighP1_lowN1_best +
                                           (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m3_diffCat_wmcCat_intxn_rfx)
# easyP1difficultN1                            1.839e-03  3.372e-03 9.585e+03   0.545    0.585
# capacity_HighP1_lowN1_best                   1.191e-01  8.007e-02 7.900e+01   1.487    0.141
# easyP1difficultN1:capacity_HighP1_lowN1_best 5.420e-03  3.372e-03 9.585e+03   1.608    0.108

wind1_m3_diffContAll_wmcCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont + capacity_HighP1_lowN1_best +
                                         (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & categorical wmc
summary(wind1_m3_diffContAll_wmcCat_rfx)
# all_diff_cont              -3.652e-02  6.591e-03  1.361e+04  -5.541 3.06e-08 ***
# capacity_HighP1_lowN1_best  1.141e-01  8.100e-02  7.900e+01   1.408    0.163

wind1_m3_diffContAll_wmcCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best +
                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind1_m3_diffContAll_wmcCat_intxn_rfx)
# all_diff_cont                            -3.820e-02  6.946e-03  1.361e+04  -5.499 3.89e-08 ***
# capacity_HighP1_lowN1_best                1.167e-01  8.107e-02  7.928e+01   1.439    0.154
# all_diff_cont:capacity_HighP1_lowN1_best -5.307e-03  6.946e-03  1.361e+04  -0.764    0.445

wind1_m3_diffContAll_wmcCont_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont + complexspan_demeaned +
                                          (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & wmc
summary(wind1_m3_diffContAll_wmcCont_rfx)
# all_diff_cont        -3.651e-02  6.591e-03  1.361e+04  -5.539 3.09e-08 ***
# complexspan_demeaned  6.434e-01  3.875e-01  7.900e+01   1.661    0.101

wind1_m3_diffContAll_wmcCont_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont * complexspan_demeaned +
                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind1_m3_diffContAll_wmcCont_intxn_rfx)
# all_diff_cont                      -3.590e-02  6.608e-03  1.361e+04  -5.433 5.63e-08 ***
# complexspan_demeaned                6.671e-01  3.878e-01  7.936e+01   1.720   0.0893 .
# all_diff_cont:complexspan_demeaned -4.546e-02  3.538e-02  1.361e+04  -1.285   0.1989

# ~ Window 1 Model 4: current difficulty x choice x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind1_m4_diffCat_optCat_prevdiffCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 + choice + easyP1difficultN1_prev +
                                                 (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty, choice made, & previous difficulty
summary(wind1_m4_diffCat_optCat_prevdiffCat_rfx)
# easyP1difficultN1       6.851e-05  3.111e-03  9.820e+03   0.022  0.98243
# choice                  1.318e-02  6.459e-03  9.823e+03   2.040  0.04140 *
# easyP1difficultN1_prev -9.601e-03  3.124e-03  9.820e+03  -3.073  0.00212 **

wind1_m4_diffCat_optCat_prevdiffCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 * choice * easyP1difficultN1_prev +
                                                       (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m4_diffCat_optCat_prevdiffCat_intxn_rfx)
# easyP1difficultN1                                1.379e-03  4.428e-03  9.817e+03   0.311   0.7555
# choice                                           1.331e-02  6.473e-03  9.819e+03   2.056   0.0398 *
# easyP1difficultN1_prev                          -9.106e-03  4.393e-03  9.816e+03  -2.073   0.0382 *
# easyP1difficultN1:choice                        -2.672e-03  6.398e-03  9.818e+03  -0.418   0.6762
# easyP1difficultN1:easyP1difficultN1_prev         1.406e-03  4.402e-03  9.816e+03   0.319   0.7495
# choice:easyP1difficultN1_prev                   -9.792e-04  6.266e-03  9.816e+03  -0.156   0.8758
# easyP1difficultN1:choice:easyP1difficultN1_prev -2.183e-03  6.279e-03  9.816e+03  -0.348   0.7281

wind1_m4_diffContAll_optCat_prevdiffContAll_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont + choice + prev_all_diff_cont +
                                                         (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and previous difficulty & categorical choice made
summary(wind1_m4_diffContAll_optCat_prevdiffContAll_rfx)
# all_diff_cont      -3.635e-02  6.535e-03  1.381e+04  -5.562 2.71e-08 ***
# choice              3.434e-03  5.402e-03  1.381e+04   0.636   0.5250
# prev_all_diff_cont -1.522e-02  6.535e-03  1.381e+04  -2.328   0.0199 *

wind1_m4_diffContAll_optCat_prevdiffContAll_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont * choice * prev_all_diff_cont +
                                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind1_m4_diffContAll_optCat_prevdiffContAll_intxn_rfx)
# all_diff_cont                           -5.817e-02  1.458e-02  1.380e+04  -3.989 6.67e-05 ***
# choice                                  -6.180e-03  1.287e-02  1.380e+04  -0.480  0.63104
# prev_all_diff_cont                      -3.827e-02  1.481e-02  1.380e+04  -2.583  0.00979 **
# all_diff_cont:choice                     1.335e-02  2.048e-02  1.380e+04   0.652  0.51456
# all_diff_cont:prev_all_diff_cont         3.869e-02  2.190e-02  1.380e+04   1.767  0.07733 .
# choice:prev_all_diff_cont                1.545e-02  2.030e-02  1.380e+04   0.761  0.44684
# all_diff_cont:choice:prev_all_diff_cont -1.823e-02  3.064e-02  1.380e+04  -0.595  0.55189

# ~ Window 1 Model 5: current difficulty x choice x capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind1_m5_diffCat_optCat_wmcCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 + choice + capacity_HighP1_lowN1_best +
                                            (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty, choice made, & wmc
summary(wind1_m5_diffCat_optCat_wmcCat_rfx)
# easyP1difficultN1          -1.665e-04  3.149e-03  9.585e+03  -0.053   0.9578
# choice                      1.133e-02  6.540e-03  9.588e+03   1.733   0.0832 .
# capacity_HighP1_lowN1_best  1.191e-01  8.005e-02  7.900e+01   1.488   0.1407

wind1_m5_diffCat_optCat_wmcCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 * choice * capacity_HighP1_lowN1_best +
                                                  (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m5_diffCat_optCat_wmcCat_intxn_rfx)
# easyP1difficultN1                                   -4.132e-04  4.793e-03  9.582e+03  -0.086   0.9313
# choice                                               1.044e-02  7.070e-03  9.585e+03   1.476   0.1399
# capacity_HighP1_lowN1_best                           1.198e-01  8.016e-02  7.929e+01   1.495   0.1389
# easyP1difficultN1:choice                             4.630e-03  6.975e-03  9.584e+03   0.664   0.5068
# easyP1difficultN1:capacity_HighP1_lowN1_best        -2.232e-03  4.793e-03  9.582e+03  -0.466   0.6414
# choice:capacity_HighP1_lowN1_best                   -1.742e-03  7.070e-03  9.585e+03  -0.246   0.8054
# easyP1difficultN1:choice:capacity_HighP1_lowN1_best  1.557e-02  6.975e-03  9.584e+03   2.232   0.0256 *

wind1_m5_diffContAll_optCat_wmcCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont + choice + capacity_HighP1_lowN1_best +
                                                (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty & categorical choice and wmc
summary(wind1_m5_diffContAll_optCat_wmcCat_rfx)
# all_diff_cont              -3.634e-02  6.602e-03  1.361e+04  -5.505 3.75e-08 ***
# choice                      2.677e-03  5.457e-03  1.361e+04   0.491    0.624
# capacity_HighP1_lowN1_best  1.141e-01  8.100e-02  7.900e+01   1.409    0.163

wind1_m5_diffContAll_optCat_wmcCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont * choice * capacity_HighP1_lowN1_best +
                                                      (1 | subjectnumber), data = clean_data_dm)
summary(wind1_m5_diffContAll_optCat_wmcCat_intxn_rfx)
# all_diff_cont                                   -3.593e-02  1.002e-02  1.361e+04  -3.587 0.000335 ***
# choice                                           4.959e-03  8.925e-03  1.361e+04   0.556 0.578423
# capacity_HighP1_lowN1_best                       1.121e-01  8.120e-02  7.982e+01   1.381 0.171171
# all_diff_cont:choice                            -4.465e-03  1.415e-02  1.361e+04  -0.316 0.752358
# all_diff_cont:capacity_HighP1_lowN1_best         2.306e-03  1.002e-02  1.361e+04   0.230 0.817885
# choice:capacity_HighP1_lowN1_best                8.574e-03  8.925e-03  1.361e+04   0.961 0.336727
# all_diff_cont:choice:capacity_HighP1_lowN1_best -1.470e-02  1.415e-02  1.361e+04  -1.039 0.298959

wind1_m5_diffContAll_optCat_wmcCont_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont + choice + complexspan_demeaned +
                                                 (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and wmc & categorical choice
summary(wind1_m5_diffContAll_optCat_wmcCont_rfx)
# all_diff_cont        -3.633e-02  6.602e-03  1.361e+04  -5.504 3.78e-08 ***
# choice                2.674e-03  5.457e-03  1.361e+04   0.490    0.624
# complexspan_demeaned  6.434e-01  3.874e-01  7.900e+01   1.661    0.101

wind1_m5_diffContAll_optCat_wmcCont_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont * choice * complexspan_demeaned +
                                                       (1 | subjectnumber), data = clean_data_dm)
summary(wind1_m5_diffContAll_optCat_wmcCont_intxn_rfx)
# all_diff_cont                             -3.595e-02  9.616e-03  1.361e+04  -3.739 0.000185 ***
# choice                                     2.279e-03  8.611e-03  1.361e+04   0.265 0.791322
# complexspan_demeaned                       6.600e-01  3.887e-01  8.005e+01   1.698 0.093420 .
# all_diff_cont:choice                       2.556e-04  1.335e-02  1.361e+04   0.019 0.984727
# all_diff_cont:complexspan_demeaned        -2.087e-02  5.205e-02  1.361e+04  -0.401 0.688438
# choice:complexspan_demeaned                1.223e-02  4.608e-02  1.361e+04   0.265 0.790748
# all_diff_cont:choice:complexspan_demeaned -4.695e-02  7.049e-02  1.361e+04  -0.666 0.505421

# ~ Window 1 Model 6: current difficulty x previous difficulty x capacity (wmc) ~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind1_m6_diffCat_prevdiffCat_wmcCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 + easyP1difficultN1_prev + capacity_HighP1_lowN1_best +
                                                 (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty and wmc
summary(wind1_m6_diffCat_prevdiffCat_wmcCat_rfx)
# easyP1difficultN1          -2.528e-04  3.148e-03  9.585e+03  -0.080  0.93600
# easyP1difficultN1_prev     -1.002e-02  3.162e-03  9.585e+03  -3.170  0.00153 **
# capacity_HighP1_lowN1_best  1.191e-01  8.007e-02  7.900e+01   1.487  0.14103

wind1_m6_diffCat_prevdiffCat_wmcCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev * capacity_HighP1_lowN1_best +
                                                       (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind1_m6_diffCat_prevdiffCat_wmcCat_intxn_rfx)
# easyP1difficultN1                                                    1.632e-03  3.371e-03  9.581e+03   0.484  0.62822
# easyP1difficultN1_prev                                              -1.091e-02  3.385e-03  9.581e+03  -3.224  0.00127 **
# capacity_HighP1_lowN1_best                                           1.191e-01  8.007e-02  7.900e+01   1.487  0.14087
# easyP1difficultN1:easyP1difficultN1_prev                             1.147e-03  3.401e-03  9.581e+03   0.337  0.73593
# easyP1difficultN1:capacity_HighP1_lowN1_best                         5.313e-03  3.371e-03  9.581e+03   1.576  0.11501
# easyP1difficultN1_prev:capacity_HighP1_lowN1_best                   -2.573e-03  3.385e-03  9.581e+03  -0.760  0.44720
# easyP1difficultN1:easyP1difficultN1_prev:capacity_HighP1_lowN1_best  2.762e-03  3.401e-03  9.581e+03   0.812  0.41676

wind1_m6_diffContAll_prevdiffContAll_wmcCat_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont + prev_all_diff_cont + capacity_HighP1_lowN1_best +
                                                         (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and previous difficulty and categorical wmc
summary(wind1_m6_diffContAll_prevdiffContAll_wmcCat_rfx)
# all_diff_cont              -3.521e-02  6.612e-03  1.348e+04  -5.325 1.02e-07 ***
# prev_all_diff_cont         -1.398e-02  6.623e-03  1.348e+04  -2.111   0.0348 *
# capacity_HighP1_lowN1_best  1.142e-01  8.111e-02  7.900e+01   1.408   0.1629

wind1_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind1_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)
# all_diff_cont                                               -5.362e-02  1.072e-02  1.347e+04  -5.004  5.7e-07 ***
# prev_all_diff_cont                                          -2.938e-02  1.075e-02  1.347e+04  -2.732  0.00631 **
# capacity_HighP1_lowN1_best                                   1.163e-01  8.136e-02  7.990e+01   1.430  0.15665
# all_diff_cont:prev_all_diff_cont                             3.348e-02  1.628e-02  1.347e+04   2.057  0.03974 *
# all_diff_cont:capacity_HighP1_lowN1_best                    -9.412e-03  1.072e-02  1.347e+04  -0.878  0.37982
# prev_all_diff_cont:capacity_HighP1_lowN1_best                2.915e-04  1.075e-02  1.347e+04   0.027  0.97838
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best  9.476e-03  1.628e-02  1.347e+04   0.582  0.56053

wind1_m6_diffContAll_prevdiffContAll_wmcCont_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont + prev_all_diff_cont + complexspan_demeaned +
                                                          (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty, previous difficulty, & wmc
summary(wind1_m6_diffContAll_prevdiffContAll_wmcCont_rfx)
# all_diff_cont        -3.520e-02  6.612e-03  1.348e+04  -5.324 1.03e-07 ***
# prev_all_diff_cont   -1.397e-02  6.623e-03  1.348e+04  -2.110   0.0349 *
# complexspan_demeaned  6.433e-01  3.880e-01  7.900e+01   1.658   0.1013

wind1_m6_diffContAll_prevdiffContAll_wmcCont_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + all_diff_cont * prev_all_diff_cont * complexspan_demeaned +
                                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind1_m6_diffContAll_prevdiffContAll_wmcCont_intxn_rfx)
# all_diff_cont                                         -4.861e-02  1.046e-02  1.347e+04  -4.650 3.36e-06 ***
# prev_all_diff_cont                                    -2.796e-02  1.048e-02  1.347e+04  -2.668  0.00763 **
# complexspan_demeaned                                   6.860e-01  3.894e-01  8.018e+01   1.762  0.08196 .
# all_diff_cont:prev_all_diff_cont                       2.799e-02  1.567e-02  1.347e+04   1.786  0.07410 .
# all_diff_cont:complexspan_demeaned                    -8.051e-02  5.614e-02  1.347e+04  -1.434  0.15156
# prev_all_diff_cont:complexspan_demeaned               -4.432e-02  5.632e-02  1.347e+04  -0.787  0.43130
# all_diff_cont:prev_all_diff_cont:complexspan_demeaned  7.879e-02  8.442e-02  1.347e+04   0.933  0.35072

# ~ Window 1 Model 7: trial number x current difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind1_m7_time_diffContAll_rfx = lmer(wind1_predisp_onset_mean ~ 1 + trialnumberRS +
                                                            all_diff_cont +
                                                            (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind1_m7_time_diffContAll_rfx)
# trialnumberRS -2.559e-01  3.416e-02  8.226e+01  -7.491 6.96e-11 ***
# all_diff_cont -6.643e-03  6.182e-03  1.388e+04  -1.075    0.283

AIC(wind1_m0_diffContAll_rfx) # 7647.008
AIC(wind1_m7_time_diffContAll_rfx) # 5912.285


# ~ Window 1 Model 8: trial number x current difficulty x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind1_m8_time_diffContAll_prevdiffContAll_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + trialnumberRS +
                                                                   all_diff_cont * prev_all_diff_cont +
                                                                   (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind1_m8_time_diffContAll_prevdiffContAll_intxn_rfx)
# trialnumberRS                    -2.555e-01  3.420e-02  8.260e+01  -7.472 7.42e-11 ***
# all_diff_cont                    -8.586e-03  9.728e-03  1.376e+04  -0.883    0.377
# prev_all_diff_cont                1.310e-02  9.757e-03  1.376e+04   1.342    0.180
# all_diff_cont:prev_all_diff_cont  3.045e-03  1.444e-02  1.375e+04   0.211    0.833

AIC(wind1_m2_diffContAll_prevdiffContAll_intxn_rfx) # 7526.341
AIC(wind1_m8_time_diffContAll_prevdiffContAll_intxn_rfx) # 5856.812

# ~ Window 1 Model 9: trial number x current difficulty x previous difficulty x wmc ~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind1_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + trialnumberRS +
                                                                   all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                                                   (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind1_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)
# trialnumberRS                                               -2.545e-01  3.502e-02  8.054e+01  -7.268 2.09e-10 ***
# all_diff_cont                                               -1.197e-02  1.024e-02  1.343e+04  -1.169    0.242
# prev_all_diff_cont                                           1.261e-02  1.029e-02  1.343e+04   1.226    0.220
# capacity_HighP1_lowN1_best                                   1.236e-01  7.952e-02  8.025e+01   1.555    0.124
# all_diff_cont:prev_all_diff_cont                             7.220e-03  1.536e-02  1.342e+04   0.470    0.638
# all_diff_cont:capacity_HighP1_lowN1_best                    -8.771e-03  1.023e-02  1.346e+04  -0.857    0.391
# prev_all_diff_cont:capacity_HighP1_lowN1_best                1.029e-03  1.028e-02  1.346e+04   0.100    0.920
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best  5.425e-03  1.536e-02  1.343e+04   0.353    0.724

AIC(wind1_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx) # 7409.359
AIC(wind1_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx) # 5760.773



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### WINDOW 2 REGRESSIONS: Effort (isi, 1 second, right after choice before outcome) #####
# ~ predictors: current difficulty, choice made, previous difficulty, WMC, NFC, and choice

### single predictor models
# ~ Window 2 Model 0: current difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m0_diffCat = lm(wind2_effort_isi_mean ~ 1 + easyP1difficultN1, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind2_m0_diffCat) # easyP1difficultN1  0.00854    0.00712   1.199     0.23

wind2_m0_diffCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m0_diffCat_rfx) # easyP1difficultN1 8.754e-03  2.256e-03 9.818e+03    3.88 0.000105 ***

wind2_m0_diffCont = lm(wind2_effort_isi_mean ~ 1 + diff_cont, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous dynamic only
summary(wind2_m0_diffCont) # diff_cont   -0.02150    0.01534  -1.402    0.161

wind2_m0_diffCont_rfx = lmer(wind2_effort_isi_mean ~ 1 + diff_cont + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m0_diffCont_rfx) # diff_cont   -1.934e-02  4.862e-03  9.818e+03  -3.978 7.01e-05 ***

wind2_m0_diffContAll = lm(wind2_effort_isi_mean ~ 1 + all_diff_cont, data = clean_data_dm) # continuous all
summary(wind2_m0_diffContAll) # all_diff_cont -0.089977   0.014770  -6.092 1.14e-09 ***

wind2_m0_diffContAll_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m0_diffContAll_rfx) # all_diff_cont -6.154e-02  4.988e-03  1.394e+04  -12.34   <2e-16 ***

AIC(wind2_m0_diffCat_rfx) # -882.7885
AIC(wind2_m0_diffCont_rfx) # -885.094
AIC(wind2_m0_diffContAll_rfx) # 229.9473

wind2_m0_trial_currDiff_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS + all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m0_trial_currDiff_rfx)
# trialnumberRS -2.929e-01  6.639e-03  1.394e+04 -44.120  < 2e-16 ***
# all_diff_cont -2.580e-02  4.742e-03  1.394e+04  -5.439 5.44e-08 ***

anova(wind2_m0_diffContAll_rfx,wind2_m0_trial_currDiff_rfx)
#                             npar      AIC      BIC  logLik deviance  Chisq Df Pr(>Chisq)
# wind2_m0_diffContAll_rfx       4   217.83   248.03 -104.92   209.83
# wind2_m0_trial_currDiff_rfx    5 -1602.57 -1564.82  806.28 -1612.57 1822.4  1  < 2.2e-16 ***

# including trial makes for a stronger model
# As the current trial increases in choice difficulty, pupil dilation is more constricted in window 2
# Also it is the opposite of what we hypothesized - constriction instead of dilation

wind2_m0_trial_currDiff_intfx_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS * all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m0_trial_currDiff_intfx_rfx)
# trialnumberRS               -3.192e-01  1.026e-02  1.394e+04 -31.105  < 2e-16 ***
# all_diff_cont               -5.741e-02  1.055e-02  1.394e+04  -5.441  5.4e-08 ***
# trialnumberRS:all_diff_cont  5.719e-02  1.705e-02  1.394e+04   3.354    8e-04 ***

# as trial progresses, pupil is more constricted
# as difficulty increases, pupil is more constricted

anova(wind2_m0_trial_currDiff_rfx,wind2_m0_trial_currDiff_intfx_rfx)
#                                   npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# wind2_m0_trial_currDiff_rfx          5 -1602.6 -1564.8 806.28  -1612.6
# wind2_m0_trial_currDiff_intfx_rfx    6 -1611.8 -1566.5 811.91  -1623.8 11.244  1  0.0007987 ***

# interacting trial makes for a slightly better model

# ~ Window 2 Model 0: previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m0_prevdiffCat = lm(wind2_effort_isi_mean ~ 1 + easyP1difficultN1_prev, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind2_m0_prevdiffCat) # easyP1difficultN1_prev -0.002120   0.007151  -0.296    0.767

wind2_m0_prevdiffCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1_prev + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m0_prevdiffCat_rfx) # easyP1difficultN1_prev -1.029e-03  2.268e-03  9.818e+03  -0.454     0.65

wind2_m0_prevdiffContAll = lm(wind2_effort_isi_mean ~ 1 + prev_all_diff_cont, data = clean_data_dm) # continuous all
summary(wind2_m0_prevdiffContAll) # prev_all_diff_cont -0.069008   0.014856  -4.645 3.43e-06 ***

wind2_m0_prevdiffContAll_rfx = lmer(wind2_effort_isi_mean ~ 1 + prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m0_prevdiffContAll_rfx) # prev_all_diff_cont -4.070e-02  4.997e-03  1.380e+04  -8.146  4.1e-16 ***

AIC(wind2_m0_prevdiffContAll) # 30293.03
AIC(wind2_m0_prevdiffContAll_rfx) # 126.3741

wind2_m0_trial_prevDiff_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS + prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m0_trial_prevDiff_rfx)
# trialnumberRS      -2.908e-01  6.697e-03  1.380e+04 -43.417   <2e-16 ***
# prev_all_diff_cont -5.343e-03  4.757e-03  1.380e+04  -1.123    0.261

anova(wind2_m0_prevdiffContAll_rfx,wind2_m0_trial_prevDiff_rfx)
#                              npar      AIC      BIC logLik deviance  Chisq Df Pr(>Chisq)
# wind2_m0_prevdiffContAll_rfx    4   114.27   144.42 -53.13   106.27
# wind2_m0_trial_prevDiff_rfx     5 -1650.92 -1613.23 830.46 -1660.92 1767.2  1  < 2.2e-16 ***

# including trial makes a better model

wind2_m0_trial_prevDiff_intfx_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS * prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m0_trial_prevDiff_intfx_rfx)
# trialnumberRS                    -3.050e-01  1.035e-02  1.380e+04 -29.477   <2e-16 ***
# prev_all_diff_cont               -2.265e-02  1.069e-02  1.380e+04  -2.119   0.0341 *
# trialnumberRS:prev_all_diff_cont  3.113e-02  1.721e-02  1.380e+04   1.808   0.0706 .

anova(wind2_m0_trial_prevDiff_rfx,wind2_m0_trial_prevDiff_intfx_rfx)
#                                   npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# wind2_m0_trial_prevDiff_rfx          5 -1650.9 -1613.2 830.46  -1660.9
# wind2_m0_trial_prevDiff_intfx_rfx    6 -1652.2 -1607.0 832.10  -1664.2 3.2694  1    0.07058 .

# interacting trial does not make for a significantly better model

# ~ Window 2 Model 0: capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m0_wmcCat = lm(wind2_effort_isi_mean ~ 1 + capacity_HighP1_lowN1_best, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind2_m0_wmcCat) # capacity_HighP1_lowN1_best 0.136904   0.007617   17.97   <2e-16 ***

wind2_m0_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m0_wmcCat_rfx) # capacity_HighP1_lowN1_best  0.13819    0.07976 79.00021   1.733   0.0871 .

wind2_m0_all_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m0_all_wmcCat_rfx) # capacity_HighP1_lowN1_best  0.13626    0.08078 79.00008   1.687   0.0956 .

wind2_m0_wmcCont = lm(wind2_effort_isi_mean ~ 1 + complexspan_demeaned, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous
summary(wind2_m0_wmcCont) # complexspan_demeaned 0.722551   0.036490    19.8   <2e-16 ***

wind2_m0_wmcCont_rfx = lmer(wind2_effort_isi_mean ~ 1 + complexspan_demeaned + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m0_wmcCont_rfx) # complexspan_demeaned  0.72819    0.38186 79.00026   1.907   0.0602 .

AIC(wind2_m0_wmcCat_rfx) # -846.2197
AIC(wind2_m0_wmcCont_rfx) # -783.5134

wind2_m0_time_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                        trialnumberRS + capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m0_time_wmcCat_rfx)
# trialnumberRS              -2.965e-01  6.644e-03  1.361e+04 -44.630   <2e-16 ***
# capacity_HighP1_lowN1_best  1.361e-01  8.078e-02  7.900e+01   1.685   0.0959 .

wind2_m0_time_wmcCat_intfx_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                        trialnumberRS * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m0_time_wmcCat_intfx_rfx)
# trialnumberRS                            -2.945e-01  7.113e-03  1.360e+04 -41.405   <2e-16 ***
# capacity_HighP1_lowN1_best                1.333e-01  8.086e-02  7.931e+01   1.648    0.103
# trialnumberRS:capacity_HighP1_lowN1_best  5.685e-03  7.113e-03  1.360e+04   0.799    0.424

anova(wind2_m0_time_wmcCat_rfx,wind2_m0_time_wmcCat_intfx_rfx)
#                                npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# wind2_m0_time_wmcCat_rfx          5 -1472.5 -1434.9 741.27  -1482.5
# wind2_m0_time_wmcCat_intfx_rfx    6 -1471.2 -1426.0 741.59  -1483.2 0.6389  1     0.4241

# interacting trials does not make for better models
# wmc doesn't have any mfx

wind2_m0_time_wmcCont_intfx_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                         trialnumberRS * complexspan_demeaned + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m0_time_wmcCont_intfx_rfx)
# trialnumberRS                      -2.966e-01  6.643e-03  1.360e+04 -44.643   <2e-16 ***
# complexspan_demeaned                6.738e-01  3.874e-01  7.931e+01   1.740   0.0858 .
# trialnumberRS:complexspan_demeaned  7.465e-02  3.417e-02  1.360e+04   2.184   0.0290 *

# similar pattern as window 4 - interaction between trial and continuous but not categorical wmc
# effect of wmc is dependent - but will focus on categorical for the SANS abstract


# ~ Window 2 Model 0: need for cognition scale (ncs -> nfc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m0_nfcCat = lm(wind2_effort_isi_mean ~ 1 + NCS_HighP1_LowN1, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind2_m0_nfcCat) # NCS_HighP1_LowN1 -0.030267   0.007215  -4.195 2.76e-05 ***

wind2_m0_nfcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + NCS_HighP1_LowN1 + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m0_nfcCat_rfx) # NCS_HighP1_LowN1 -0.03050    0.07567 80.00004  -0.403    0.688

wind2_m0_nfcCont = lm(wind2_effort_isi_mean ~ 1 + NCS, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous
summary(wind2_m0_nfcCont) # NCS         0.0011624  0.0006756   1.721   0.0854 .

wind2_m0_nfcCont_rfx = lmer(wind2_effort_isi_mean ~ 1 + NCS + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m0_nfcCont_rfx) # NCS          0.001150   0.007089 80.000315   0.162    0.872

AIC(wind2_m0_nfcCat) # 21117.58
AIC(wind2_m0_nfcCont) # 21132.21

# ~ Window 2 Model 0: choice made ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m0_optCat = lm(wind2_effort_isi_mean ~ 1 + choice, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical choice made
summary(wind2_m0_optCat) # choice      0.072948   0.014224   5.128 2.98e-07 ***

wind2_m0_optCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + choice + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m0_optCat_rfx) # choice      2.305e-02  4.683e-03 9.819e+03   4.923 8.67e-07 ***

AIC(wind2_m0_optCat) # 21256.67
AIC(wind2_m0_optCat_rfx) # -893.4146

# ~ Window 2 Model 0: time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m0_time = lm(wind2_effort_isi_mean ~ 1 + trialnumberRS, data = clean_data_dm)
summary(wind2_m0_time) # trialnumberRS -0.29989    0.02094  -14.32   <2e-16 ***

wind2_m0_time_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m0_time_rfx) # trialnumberRS -2.991e-01  6.548e-03  1.394e+04  -45.67   <2e-16 ***

# Time has a strong negative effect on pupil dilation during Window 2

wind2_m0_time_Fullyrfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS + (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind2_m0_time_Fullyrfx) # trialnumberRS -0.29927    0.02970 81.96775  -10.08 5.31e-16 ***

AIC(wind2_m0_time_rfx)
AIC(wind2_m0_time_Fullyrfx) # waaaay better AIC


### main & interaction effects
# ~ Window 2 Model 1: current difficulty x choice made ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m1_diffCat_optCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 + choice +
                                           (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & choice made
summary(wind2_m1_diffCat_optCat_rfx)
# easyP1difficultN1 8.596e-03  2.254e-03 9.817e+03   3.814 0.000138 ***
# choice            2.280e-02  4.681e-03 9.818e+03   4.871 1.13e-06 ***

wind2_m1_diffCat_optCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 * choice +
                                           (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m1_diffCat_optCat_intxn_rfx)
# easyP1difficultN1         1.491e-02  3.208e-03  9.817e+03   4.648 3.40e-06 ***
# choice                    2.359e-02  4.688e-03  9.818e+03   5.031 4.95e-07 ***
# easyP1difficultN1:choice -1.281e-02  4.633e-03  9.817e+03  -2.765  0.00571 **

wind2_m1_diffContAll_optCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont + choice +
                                     (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & categorical choice made
summary(wind2_m1_diffContAll_optCat_rfx)
# all_diff_cont -5.954e-02  4.988e-03  1.394e+04 -11.937  < 2e-16 ***
# choice         2.864e-02  4.124e-03  1.394e+04   6.946 3.93e-12 ***

wind2_m1_diffContAll_optCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont * choice +
                                           (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m1_diffContAll_optCat_intxn_rfx)
# all_diff_cont        -6.166e-02  7.219e-03  1.394e+04  -8.542  < 2e-16 ***
# choice                2.662e-02  6.463e-03  1.394e+04   4.119 3.82e-05 ***
# all_diff_cont:choice  4.072e-03  1.003e-02  1.394e+04   0.406    0.685

# ~ Window 2 Model 2: current difficulty x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m2_diffCat_prevdiffCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 + easyP1difficultN1_prev +
                                     (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & previous difficulty
summary(wind2_m2_diffCat_prevdiffCat_rfx)
# easyP1difficultN1       8.738e-03  2.257e-03  9.817e+03   3.872 0.000109 ***
# easyP1difficultN1_prev -8.754e-04  2.266e-03  9.817e+03  -0.386 0.699330

wind2_m2_diffCat_prevdiffCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev +
                                           (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m2_diffCat_prevdiffCat_intxn_rfx)
# easyP1difficultN1                         8.737e-03  2.257e-03  9.816e+03   3.871 0.000109 ***
# easyP1difficultN1_prev                   -8.712e-04  2.266e-03  9.816e+03  -0.384 0.700682
# easyP1difficultN1:easyP1difficultN1_prev -2.270e-03  2.277e-03  9.816e+03  -0.997 0.318747

wind2_m2_diffContAll_prevdiffContAll_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont + prev_all_diff_cont +
                                         (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & previous difficulty
summary(wind2_m2_diffContAll_prevdiffContAll_rfx)
# all_diff_cont      -5.876e-02  4.969e-03  1.380e+04 -11.827  < 2e-16 ***
# prev_all_diff_cont -3.792e-02  4.977e-03  1.380e+04  -7.619 2.72e-14 ***

wind2_m2_diffContAll_prevdiffContAll_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont * prev_all_diff_cont +
                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m2_diffContAll_prevdiffContAll_intxn_rfx)
# all_diff_cont                    -6.885e-02  7.758e-03  1.380e+04  -8.875  < 2e-16 ***
# prev_all_diff_cont               -4.804e-02  7.776e-03  1.380e+04  -6.177 6.71e-10 ***
# all_diff_cont:prev_all_diff_cont  1.972e-02  1.165e-02  1.380e+04   1.693   0.0906 .

wind2_m2_trial_currDiff_prevDiff_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS + all_diff_cont * prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m2_trial_currDiff_prevDiff_rfx)
# trialnumberRS                    -2.853e-01  6.800e-03  1.380e+04 -41.959   <2e-16 ***
# all_diff_cont                    -1.862e-02  7.403e-03  1.380e+04  -2.515   0.0119 *
# prev_all_diff_cont                2.772e-03  7.423e-03  1.380e+04   0.373   0.7088
# all_diff_cont:prev_all_diff_cont -1.471e-02  1.101e-02  1.380e+04  -1.337   0.1814

# there is still an effect of current difficulty where the more difficult the current choice is, the more constricted, but there is no interaction with previous difficulty

wind2_m2_trial_currDiff_prevDiff_intfx_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS * all_diff_cont * prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m2_trial_currDiff_prevDiff_intfx_rfx)
# trialnumberRS                                  -3.297e-01  1.567e-02  1.380e+04 -21.036  < 2e-16 ***
# all_diff_cont                                  -5.351e-02  1.620e-02  1.380e+04  -3.304 0.000955 ***
# prev_all_diff_cont                             -1.372e-02  1.628e-02  1.380e+04  -0.843 0.399259
# trialnumberRS:all_diff_cont                     7.031e-02  2.664e-02  1.380e+04   2.639 0.008325 **
# trialnumberRS:prev_all_diff_cont                3.795e-02  2.673e-02  1.380e+04   1.420 0.155736
# all_diff_cont:prev_all_diff_cont               -6.731e-03  2.774e-02  1.380e+04  -0.243 0.808252
# trialnumberRS:all_diff_cont:prev_all_diff_cont -2.637e-02  4.346e-02  1.380e+04  -0.607 0.543970

# consistent no interaction between current and previous difficulty
# ~ future models will probably not interact them


# ~ Window 2 Model 3: current difficulty x capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m3_diffCat_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 + capacity_HighP1_lowN1_best +
                                          (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & wmc
summary(wind2_m3_diffCat_wmcCat_rfx)
# easyP1difficultN1          8.878e-03  2.285e-03 9.582e+03   3.885 0.000103 ***
# capacity_HighP1_lowN1_best 1.382e-01  7.976e-02 7.900e+01   1.733 0.087076 .

wind2_m3_diffCat_wmcCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 * capacity_HighP1_lowN1_best +
                                                (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m3_diffCat_wmcCat_intxn_rfx)
# easyP1difficultN1                             8.828e-03  2.447e-03  9.581e+03   3.608  0.00031 ***
# capacity_HighP1_lowN1_best                    1.382e-01  7.976e-02  7.900e+01   1.733  0.08708 .
# easyP1difficultN1:capacity_HighP1_lowN1_best -1.411e-04  2.447e-03  9.581e+03  -0.058  0.95402

wind2_m3_diffContAll_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont + capacity_HighP1_lowN1_best +
                                                  (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & categorical wmc
summary(wind2_m3_diffContAll_wmcCat_rfx)
# all_diff_cont              -6.080e-02  5.055e-03  1.361e+04 -12.027   <2e-16 ***
# capacity_HighP1_lowN1_best  1.347e-01  8.075e-02  7.900e+01   1.668   0.0993 .

wind2_m3_diffContAll_wmcCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best +
                                                        (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m3_diffContAll_wmcCat_intxn_rfx)
# all_diff_cont                            -5.949e-02  5.328e-03  1.360e+04 -11.167   <2e-16 ***
# capacity_HighP1_lowN1_best                1.327e-01  8.080e-02  7.916e+01   1.642    0.105
# all_diff_cont:capacity_HighP1_lowN1_best  4.136e-03  5.328e-03  1.360e+04   0.776    0.438

wind2_m3_diffContAll_wmcCont_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont + complexspan_demeaned +
                                                  (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & wmc
summary(wind2_m3_diffContAll_wmcCont_rfx)
# all_diff_cont        -6.079e-02  5.055e-03  1.361e+04 -12.026   <2e-16 ***
# complexspan_demeaned  7.025e-01  3.869e-01  7.900e+01   1.816   0.0732 .

wind2_m3_diffContAll_wmcCont_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont * complexspan_demeaned +
                                                        (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m3_diffContAll_wmcCont_intxn_rfx)
# all_diff_cont                      -6.103e-02  5.068e-03  1.360e+04 -12.042   <2e-16 ***
# complexspan_demeaned                6.932e-01  3.872e-01  7.921e+01   1.790   0.0772 .
# all_diff_cont:complexspan_demeaned  1.796e-02  2.713e-02  1.361e+04   0.662   0.5079

wind2_m3_trial_currDiff_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                            trialnumberRS + all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m3_trial_currDiff_wmcCat_rfx)
# trialnumberRS                            -2.905e-01  6.733e-03  1.360e+04 -43.142  < 2e-16 ***
# all_diff_cont                            -2.413e-02  5.064e-03  1.360e+04  -4.764 1.91e-06 ***
# capacity_HighP1_lowN1_best                1.327e-01  8.081e-02  7.914e+01   1.642    0.105
# all_diff_cont:capacity_HighP1_lowN1_best  5.654e-03  4.997e-03  1.360e+04   1.131    0.258

# still an effect of current difficulty, but it doesn't interact with wmc

wind2_m3_time_currDiffcont_wmcCat_intfx_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                        trialnumberRS * all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m3_time_currDiffcont_wmcCat_intfx_rfx)
# trialnumberRS                                          -3.122e-01  1.077e-02  1.360e+04 -28.991  < 2e-16 ***
# all_diff_cont                                          -5.216e-02  1.146e-02  1.360e+04  -4.552 5.36e-06 ***
# capacity_HighP1_lowN1_best                              1.223e-01  8.098e-02  7.980e+01   1.510  0.13491
# trialnumberRS:all_diff_cont                             5.028e-02  1.853e-02  1.360e+04   2.713  0.00667 **
# trialnumberRS:capacity_HighP1_lowN1_best                2.074e-02  1.077e-02  1.360e+04   1.926  0.05417 .   # marginal
# all_diff_cont:capacity_HighP1_lowN1_best                2.211e-02  1.146e-02  1.360e+04   1.930  0.05366 .   # marginal
# trialnumberRS:all_diff_cont:capacity_HighP1_lowN1_best -3.100e-02  1.853e-02  1.360e+04  -1.673  0.09443 .

wind2_m3_time_currDiffcont_wmcCont_intfx_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                         trialnumberRS * all_diff_cont * complexspan_demeaned + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m3_time_currDiffcont_wmcCont_intfx_rfx)
# trialnumberRS                                    -3.221e-01  1.050e-02  1.360e+04 -30.687  < 2e-16 ***
# all_diff_cont                                    -6.279e-02  1.079e-02  1.360e+04  -5.821 5.97e-09 ***
# complexspan_demeaned                              6.085e-01  3.882e-01  7.995e+01   1.568 0.120910
# trialnumberRS:all_diff_cont                       6.581e-02  1.744e-02  1.360e+04   3.774 0.000161 ***
# trialnumberRS:complexspan_demeaned                1.698e-01  5.531e-02  1.360e+04   3.070 0.002145 **
# all_diff_cont:complexspan_demeaned                1.166e-01  5.657e-02  1.360e+04   2.062 0.039263 *
# trialnumberRS:all_diff_cont:complexspan_demeaned -1.736e-01  9.166e-02  1.360e+04  -1.894 0.058281 . # marginal

anova(wind2_m3_trial_currDiff_wmcCat_rfx,wind2_m3_time_currDiffcont_wmcCat_intfx_rfx)
#                                             npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# wind2_m3_trial_currDiff_wmcCat_rfx             7 -1498.8 -1446.2 756.42  -1512.8
# wind2_m3_time_currDiffcont_wmcCat_intfx_rfx   10 -1508.4 -1433.2 764.20  -1528.4 15.572  3   0.001388 **

# if interacting with trial
### categorical wmc has stronger but still marginal interaction effects
### continuous wmc has stronger and significant two-way effects and marginal three-way
# the non-interacting trial model is better - less complex model is better?

anova(wind2_m3_time_currDiffcont_wmcCat_intfx_rfx, wind2_m3_time_currDiffcont_wmcCont_intfx_rfx)

# what if I interact with previous difficulty
wind2_m35_time_prevDiffcont_wmcCat_intfx_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                                     trialnumberRS * prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m35_time_prevDiffcont_wmcCat_intfx_rfx)
# trialnumberRS                                               -2.969e-01  1.087e-02  1.347e+04 -27.326  < 2e-16 ***
# prev_all_diff_cont                                          -1.279e-02  1.161e-02  1.347e+04  -1.102  0.27028
# capacity_HighP1_lowN1_best                                   1.215e-01  8.100e-02  7.982e+01   1.500  0.13747
# trialnumberRS:prev_all_diff_cont                             1.751e-02  1.870e-02  1.347e+04   0.936  0.34908
# trialnumberRS:capacity_HighP1_lowN1_best                     2.304e-02  1.087e-02  1.347e+04   2.120  0.03404 *
# prev_all_diff_cont:capacity_HighP1_lowN1_best                3.028e-02  1.161e-02  1.347e+04   2.609  0.00909 **
# trialnumberRS:prev_all_diff_cont:capacity_HighP1_lowN1_best -4.319e-02  1.870e-02  1.347e+04  -2.309  0.02093 *

# seems like previous interacts with wmc categorical
# no mfx of previous or wmc
# interacting effect of previous and wmc
# no interacting effect of previous and trial
# 3 way interacting effect

wind2_m35_time_prevDiffcont_wmcCont_intfx_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                                      trialnumberRS * prev_all_diff_cont * complexspan_demeaned + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m35_time_prevDiffcont_wmcCont_intfx_rfx)
# trialnumberRS                                         -3.066e-01  1.059e-02  1.347e+04 -28.948   <2e-16 ***
# prev_all_diff_cont                                    -2.594e-02  1.093e-02  1.347e+04  -2.373   0.0176 *
# complexspan_demeaned                                   6.274e-01  3.882e-01  7.997e+01   1.616   0.1100
# trialnumberRS:prev_all_diff_cont                       3.657e-02  1.761e-02  1.347e+04   2.077   0.0378 *
# trialnumberRS:complexspan_demeaned                     1.361e-01  5.583e-02  1.347e+04   2.438   0.0148 *
# prev_all_diff_cont:complexspan_demeaned                1.051e-01  5.730e-02  1.347e+04   1.834   0.0667 .
# trialnumberRS:prev_all_diff_cont:complexspan_demeaned -1.400e-01  9.253e-02  1.347e+04  -1.513   0.1302

# if interacting with trial and previous difficulty
### categorical wmc seems to have stronger effects, plus a 2 way with previous difficulty and 3 way effect with trial
### continuous wmc has weaker effects. 2 way with trial remains, but none with previous and no 3 way

anova(wind2_m35_time_prevDiffcont_wmcCat_intfx_rfx, wind2_m35_time_prevDiffcont_wmcCont_intfx_rfx)

wind2_m35_time_prevDiffcont_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                                      trialnumberRS + prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m35_time_prevDiffcont_wmcCat_rfx)
# trialnumberRS                                 -2.886e-01  6.793e-03  1.347e+04 -42.490   <2e-16 ***
# prev_all_diff_cont                            -2.949e-03  5.079e-03  1.347e+04  -0.581    0.561
# capacity_HighP1_lowN1_best                     1.333e-01  8.082e-02  7.915e+01   1.649    0.103
# prev_all_diff_cont:capacity_HighP1_lowN1_best  6.645e-03  5.011e-03  1.347e+04   1.326    0.185

anova(wind2_m35_time_prevDiffcont_wmcCat_intfx_rfx, wind2_m35_time_prevDiffcont_wmcCat_rfx)
#                                              npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# wind2_m35_time_prevDiffcont_wmcCat_rfx          7 -1540.8 -1488.2 777.39  -1554.8
# wind2_m35_time_prevDiffcont_wmcCat_intfx_rfx   10 -1543.7 -1468.6 781.85  -1563.7 8.9315  3    0.03022 *

# Interacting previous with trial is a better model than just controlling for it

# ~ Window 2 Model 4: current difficulty x choice x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m4_diffCat_optCat_prevdiffCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 + choice + easyP1difficultN1_prev +
                                            (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty, choice made, & previous difficulty
summary(wind2_m4_diffCat_optCat_prevdiffCat_rfx)
# easyP1difficultN1       8.582e-03  2.254e-03  9.816e+03   3.807 0.000142 ***
# choice                  2.279e-02  4.681e-03  9.817e+03   4.869 1.14e-06 ***
# easyP1difficultN1_prev -8.363e-04  2.264e-03  9.816e+03  -0.369 0.711830

wind2_m4_diffCat_optCat_prevdiffCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 * choice * easyP1difficultN1_prev +
                                                  (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m4_diffCat_optCat_prevdiffCat_intxn_rfx)
# easyP1difficultN1                                1.493e-02  3.209e-03  9.813e+03   4.654  3.3e-06 ***
# choice                                           2.360e-02  4.689e-03  9.814e+03   5.033  4.9e-07 ***
# easyP1difficultN1_prev                           7.929e-04  3.183e-03  9.812e+03   0.249  0.80328
# easyP1difficultN1:choice                        -1.289e-02  4.635e-03  9.813e+03  -2.782  0.00542 **
# easyP1difficultN1:easyP1difficultN1_prev        -2.978e-03  3.190e-03  9.812e+03  -0.934  0.35056
# choice:easyP1difficultN1_prev                   -3.327e-03  4.538e-03  9.812e+03  -0.733  0.46352
# easyP1difficultN1:choice:easyP1difficultN1_prev  1.326e-03  4.548e-03  9.812e+03   0.292  0.77055

wind2_m4_diffContAll_optCat_prevdiffContAll_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont + choice + prev_all_diff_cont +
                                                 (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and previous difficulty & categorical choice made
summary(wind2_m4_diffContAll_optCat_prevdiffContAll_rfx)
# all_diff_cont      -5.680e-02  4.969e-03  1.380e+04 -11.431  < 2e-16 ***
# choice              2.805e-02  4.108e-03  1.380e+04   6.829 8.92e-12 ***
# prev_all_diff_cont -3.828e-02  4.970e-03  1.380e+04  -7.703 1.42e-14 ***

wind2_m4_diffContAll_optCat_prevdiffContAll_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont * choice * prev_all_diff_cont +
                                                       (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m4_diffContAll_optCat_prevdiffContAll_intxn_rfx)
# all_diff_cont                           -6.427e-02  1.110e-02  1.380e+04  -5.790 7.18e-09 ***
# choice                                   3.108e-02  9.789e-03  1.380e+04   3.175 0.001501 **
# prev_all_diff_cont                      -4.241e-02  1.127e-02  1.380e+04  -3.763 0.000169 ***
# all_diff_cont:choice                    -5.142e-03  1.558e-02  1.380e+04  -0.330 0.741342
# all_diff_cont:prev_all_diff_cont         9.564e-03  1.666e-02  1.380e+04   0.574 0.565929
# choice:prev_all_diff_cont               -1.121e-02  1.544e-02  1.380e+04  -0.726 0.467743
# all_diff_cont:choice:prev_all_diff_cont  1.960e-02  2.330e-02  1.380e+04   0.841 0.400182

# ~ Window 2 Model 5: current difficulty x choice x capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m5_diffCat_optCat_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 + choice + capacity_HighP1_lowN1_best +
                                          (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty, choice made, & wmc
summary(wind2_m5_diffCat_optCat_wmcCat_rfx)
# easyP1difficultN1          8.743e-03  2.283e-03 9.581e+03   3.830 0.000129 ***
# choice                     2.162e-02  4.741e-03 9.582e+03   4.561 5.16e-06 ***
# capacity_HighP1_lowN1_best 1.383e-01  7.971e-02 7.900e+01   1.735 0.086626 .

wind2_m5_diffCat_optCat_wmcCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 * choice * capacity_HighP1_lowN1_best +
                                                (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m5_diffCat_optCat_wmcCat_intxn_rfx)
# easyP1difficultN1                                    1.497e-02  3.479e-03  9.578e+03   4.304 1.70e-05 ***
# choice                                               2.075e-02  5.127e-03  9.579e+03   4.047 5.24e-05 ***
# capacity_HighP1_lowN1_best                           1.405e-01  7.974e-02  7.916e+01   1.762   0.0819 .
# easyP1difficultN1:choice                            -1.278e-02  5.059e-03  9.578e+03  -2.526   0.0115 *
# easyP1difficultN1:capacity_HighP1_lowN1_best         6.170e-05  3.479e-03  9.578e+03   0.018   0.9858
# choice:capacity_HighP1_lowN1_best                   -4.421e-03  5.127e-03  9.579e+03  -0.862   0.3885
# easyP1difficultN1:choice:capacity_HighP1_lowN1_best -6.681e-04  5.059e-03  9.578e+03  -0.132   0.8949

wind2_m5_diffContAll_optCat_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont + choice + capacity_HighP1_lowN1_best +
                                          (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty & categorical choice and wmc
summary(wind2_m5_diffContAll_optCat_wmcCat_rfx)
# all_diff_cont              -5.888e-02  5.055e-03  1.360e+04 -11.650  < 2e-16 ***
# choice                      2.850e-02  4.178e-03  1.360e+04   6.821 9.39e-12 ***
# capacity_HighP1_lowN1_best  1.349e-01  8.069e-02  7.900e+01   1.672   0.0984 .

wind2_m5_diffContAll_optCat_wmcCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont * choice * capacity_HighP1_lowN1_best +
                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m5_diffContAll_optCat_wmcCat_intxn_rfx)
# all_diff_cont                                   -6.075e-02  7.677e-03  1.360e+04  -7.913  2.7e-15 ***
# choice                                           2.554e-02  6.835e-03  1.360e+04   3.736 0.000187 ***
# capacity_HighP1_lowN1_best                       1.358e-01  8.081e-02  7.949e+01   1.680 0.096863 .
# all_diff_cont:choice                             6.621e-03  1.084e-02  1.360e+04   0.611 0.541309
# all_diff_cont:capacity_HighP1_lowN1_best        -1.361e-03  7.677e-03  1.360e+04  -0.177 0.859312
# choice:capacity_HighP1_lowN1_best               -5.513e-03  6.835e-03  1.360e+04  -0.807 0.419886
# all_diff_cont:choice:capacity_HighP1_lowN1_best  1.117e-02  1.084e-02  1.360e+04   1.030 0.302909

wind2_m5_diffContAll_optCat_wmcCont_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont + choice + complexspan_demeaned +
                                                  (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and wmc & categorical choice
summary(wind2_m5_diffContAll_optCat_wmcCont_rfx)
# all_diff_cont        -5.888e-02  5.055e-03  1.360e+04 -11.648  < 2e-16 ***
# choice                2.850e-02  4.178e-03  1.360e+04   6.821 9.43e-12 ***
# complexspan_demeaned  7.028e-01  3.866e-01  7.900e+01   1.818   0.0729 .

wind2_m5_diffContAll_optCat_wmcCont_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont * choice * complexspan_demeaned +
                                                        (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m5_diffContAll_optCat_wmcCont_intxn_rfx)
# all_diff_cont                             -5.978e-02  7.367e-03  1.360e+04  -8.114 5.30e-16 ***
# choice                                     2.806e-02  6.595e-03  1.360e+04   4.254 2.12e-05 ***
# complexspan_demeaned                       7.187e-01  3.874e-01  7.962e+01   1.855   0.0673 .
# all_diff_cont:choice                       1.551e-03  1.023e-02  1.360e+04   0.152   0.8795
# all_diff_cont:complexspan_demeaned        -1.231e-02  3.988e-02  1.360e+04  -0.309   0.7575
# choice:complexspan_demeaned               -4.466e-02  3.529e-02  1.360e+04  -1.266   0.2057
# all_diff_cont:choice:complexspan_demeaned  5.246e-02  5.398e-02  1.360e+04   0.972   0.3312

# ~ Window 2 Model 6: current difficulty x previous difficulty x capacity (wmc) ~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind2_m6_diffCat_prevdiffCat_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 + easyP1difficultN1_prev + capacity_HighP1_lowN1_best +
                                            (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty and wmc
summary(wind2_m6_diffCat_prevdiffCat_wmcCat_rfx)
# easyP1difficultN1           8.860e-03  2.285e-03  9.581e+03   3.877 0.000107 ***
# easyP1difficultN1_prev     -1.142e-03  2.295e-03  9.581e+03  -0.498 0.618746
# capacity_HighP1_lowN1_best  1.382e-01  7.976e-02  7.900e+01   1.733 0.087075 .

wind2_m6_diffCat_prevdiffCat_wmcCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev * capacity_HighP1_lowN1_best +
                                                  (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind2_m6_diffCat_prevdiffCat_wmcCat_intxn_rfx)
# easyP1difficultN1                                                    8.788e-03  2.448e-03  9.577e+03   3.590 0.000332 ***
# easyP1difficultN1_prev                                              -1.700e-03  2.458e-03  9.577e+03  -0.692 0.489247
# capacity_HighP1_lowN1_best                                           1.382e-01  7.975e-02  7.900e+01   1.733 0.087066 .
# easyP1difficultN1:easyP1difficultN1_prev                            -2.481e-03  2.469e-03  9.577e+03  -1.005 0.315029
# easyP1difficultN1:capacity_HighP1_lowN1_best                        -1.772e-04  2.448e-03  9.577e+03  -0.072 0.942272
# easyP1difficultN1_prev:capacity_HighP1_lowN1_best                   -1.569e-03  2.458e-03  9.577e+03  -0.638 0.523215
# easyP1difficultN1:easyP1difficultN1_prev:capacity_HighP1_lowN1_best  3.333e-04  2.469e-03  9.577e+03   0.135 0.892646

wind2_m6_diffContAll_prevdiffContAll_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont + prev_all_diff_cont + capacity_HighP1_lowN1_best +
                                                          (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and previous difficulty and categorical wmc
summary(wind2_m6_diffContAll_prevdiffContAll_wmcCat_rfx)
# all_diff_cont              -5.800e-02  5.038e-03  1.347e+04 -11.513  < 2e-16 ***
# prev_all_diff_cont         -3.680e-02  5.047e-03  1.347e+04  -7.293 3.21e-13 ***
# capacity_HighP1_lowN1_best  1.344e-01  8.072e-02  7.900e+01   1.664      0.1 .

wind2_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)
# all_diff_cont                                               -6.516e-02  8.168e-03  1.347e+04  -7.977 1.62e-15 ***
# prev_all_diff_cont                                          -4.400e-02  8.194e-03  1.347e+04  -5.369 8.03e-08 ***
# capacity_HighP1_lowN1_best                                   1.283e-01  8.088e-02  7.953e+01   1.586    0.117
# all_diff_cont:prev_all_diff_cont                             1.684e-02  1.241e-02  1.347e+04   1.358    0.175
# all_diff_cont:capacity_HighP1_lowN1_best                     7.562e-03  8.168e-03  1.347e+04   0.926    0.355
# prev_all_diff_cont:capacity_HighP1_lowN1_best                7.548e-03  8.194e-03  1.347e+04   0.921    0.357
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -5.133e-03  1.241e-02  1.347e+04  -0.414    0.679

wind2_m6_diffContAll_prevdiffContAll_wmcCont_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont + prev_all_diff_cont + complexspan_demeaned +
                                                 (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty, previous difficulty, & wmc
summary(wind2_m6_diffContAll_prevdiffContAll_wmcCont_rfx)
# all_diff_cont        -5.800e-02  5.038e-03  1.347e+04 -11.512  < 2e-16 ***
# prev_all_diff_cont   -3.680e-02  5.047e-03  1.347e+04  -7.292 3.23e-13 ***
# complexspan_demeaned  6.993e-01  3.868e-01  7.900e+01   1.808   0.0744 .

wind2_m6_diffContAll_prevdiffContAll_wmcCont_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont * prev_all_diff_cont * complexspan_demeaned +
                                                       (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m6_diffContAll_prevdiffContAll_wmcCont_intxn_rfx)
# all_diff_cont                                         -6.795e-02  7.970e-03  1.347e+04  -8.526  < 2e-16 ***
# prev_all_diff_cont                                    -4.677e-02  7.986e-03  1.347e+04  -5.856 4.85e-09 ***
# complexspan_demeaned                                   6.728e-01  3.877e-01  7.969e+01   1.735   0.0866 .
# all_diff_cont:prev_all_diff_cont                       1.874e-02  1.194e-02  1.347e+04   1.569   0.1166
# all_diff_cont:complexspan_demeaned                     2.873e-02  4.276e-02  1.347e+04   0.672   0.5018
# prev_all_diff_cont:complexspan_demeaned                2.772e-02  4.291e-02  1.347e+04   0.646   0.5184
# all_diff_cont:prev_all_diff_cont:complexspan_demeaned -9.861e-03  6.432e-02  1.347e+04  -0.153   0.8782

wind2_m6_trial_currDiff_prevDiff_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                                     trialnumberRS + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m6_trial_currDiff_prevDiff_wmcCat_rfx)
# trialnumberRS                                               -2.834e-01  6.894e-03  1.347e+04 -41.110   <2e-16 ***
# all_diff_cont                                               -1.530e-02  7.795e-03  1.347e+04  -1.963   0.0496 *
# prev_all_diff_cont                                           6.709e-03  7.822e-03  1.347e+04   0.858   0.3911
# capacity_HighP1_lowN1_best                                   1.278e-01  8.089e-02  7.947e+01   1.579   0.1182
# all_diff_cont:prev_all_diff_cont                            -1.835e-02  1.172e-02  1.347e+04  -1.565   0.1177
# all_diff_cont:capacity_HighP1_lowN1_best                     1.054e-02  7.700e-03  1.347e+04   1.369   0.1710
# prev_all_diff_cont:capacity_HighP1_lowN1_best                1.140e-02  7.725e-03  1.347e+04   1.476   0.1399
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -1.046e-02  1.170e-02  1.347e+04  -0.895   0.3711

# still only an effect of current difficulty

wind2_m65_trial_currDiff_prevDiff_wmcCat_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                                      trialnumberRS + all_diff_cont * capacity_HighP1_lowN1_best +
                                                      prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind2_m65_trial_currDiff_prevDiff_wmcCat_rfx)
# trialnumberRS                                 -2.827e-01  6.875e-03  1.347e+04 -41.112  < 2e-16 ***
# all_diff_cont                                 -2.447e-02  5.065e-03  1.347e+04  -4.831 1.38e-06 ***
# capacity_HighP1_lowN1_best                     1.300e-01  8.085e-02  7.928e+01   1.608    0.112
# prev_all_diff_cont                            -2.505e-03  5.075e-03  1.347e+04  -0.494    0.622
# all_diff_cont:capacity_HighP1_lowN1_best       5.594e-03  5.005e-03  1.347e+04   1.118    0.264
# capacity_HighP1_lowN1_best:prev_all_diff_cont  6.414e-03  5.012e-03  1.347e+04   1.280    0.201

# same


# SO:
# - current difficulty reliably matters
# - prev. difficulty only sometimes matters, and only when trial number is not included
# - trial number matters.
# - capacity does not seem to matter (and if it does, it's a weak main effect)


wind2_m8_time_diffContAll_prevdiffContAll_intxnwTime = lmer(wind2_effort_isi_mean ~ 1 +
                                                              trialnumberRS * all_diff_cont +
                                                              prev_all_diff_cont * trialnumberRS +
                                                              (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$prev_all_diff_cont),])
summary(wind2_m8_time_diffContAll_prevdiffContAll_intxnwTime)
AIC(wind2_m8_time_diffContAll_prevdiffContAll_intxnwTime)


wind2_m8_time_diffContAll_intxnwTime = lmer(wind2_effort_isi_mean ~ 1 +
                                              trialnumberRS * all_diff_cont +
                                              (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$prev_all_diff_cont),])
summary(wind2_m8_time_diffContAll_intxnwTime)
AIC(wind2_m8_time_diffContAll_intxnwTime)

anova(wind2_m8_time_diffContAll_prevdiffContAll_intxnwTime,wind2_m8_time_diffContAll_intxnwTime)
# The better model includes current difficulty interacting with trial number

wind2_m8_all_Time_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                    trialnumberRS + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                    (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$prev_all_diff_cont),])
summary(wind2_m8_all_Time_rfx)
# trialnumberRS                                               -2.834e-01  6.894e-03  1.347e+04 -41.110   <2e-16 ***
# all_diff_cont                                               -1.530e-02  7.795e-03  1.347e+04  -1.963   0.0496 *
# prev_all_diff_cont                                           6.709e-03  7.822e-03  1.347e+04   0.858   0.3911
# capacity_HighP1_lowN1_best                                   1.278e-01  8.089e-02  7.947e+01   1.579   0.1182
# all_diff_cont:prev_all_diff_cont                            -1.835e-02  1.172e-02  1.347e+04  -1.565   0.1177
# all_diff_cont:capacity_HighP1_lowN1_best                     1.054e-02  7.700e-03  1.347e+04   1.369   0.1710
# prev_all_diff_cont:capacity_HighP1_lowN1_best                1.140e-02  7.725e-03  1.347e+04   1.476   0.1399
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -1.046e-02  1.170e-02  1.347e+04  -0.895   0.3711

wind2_m8_all_intfxTime_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                trialnumberRS * all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$prev_all_diff_cont),])
summary(wind2_m8_all_intfxTime_rfx)
# trialnumberRS                                                             -3.220e-01  1.622e-02  1.346e+04 -19.850  < 2e-16 ***
# all_diff_cont                                                             -5.077e-02  1.711e-02  1.346e+04  -2.967  0.00302 **
# prev_all_diff_cont                                                        -6.906e-03  1.724e-02  1.346e+04  -0.401  0.68867
# capacity_HighP1_lowN1_best                                                 1.138e-01  8.123e-02  8.081e+01   1.401  0.16509
# trialnumberRS:all_diff_cont                                                6.930e-02  2.809e-02  1.346e+04   2.467  0.01364 *
# trialnumberRS:prev_all_diff_cont                                           3.098e-02  2.823e-02  1.346e+04   1.097  0.27244
# all_diff_cont:prev_all_diff_cont                                          -9.148e-04  2.983e-02  1.346e+04  -0.031  0.97553
# trialnumberRS:capacity_HighP1_lowN1_best                                   3.093e-02  1.622e-02  1.346e+04   1.907  0.05659 .
# all_diff_cont:capacity_HighP1_lowN1_best                                   1.742e-02  1.711e-02  1.346e+04   1.018  0.30873
# prev_all_diff_cont:capacity_HighP1_lowN1_best                              2.611e-02  1.724e-02  1.346e+04   1.515  0.12986
# trialnumberRS:all_diff_cont:prev_all_diff_cont                            -3.965e-02  4.682e-02  1.346e+04  -0.847  0.39705
# trialnumberRS:all_diff_cont:capacity_HighP1_lowN1_best                    -1.940e-02  2.809e-02  1.346e+04  -0.691  0.48989
# trialnumberRS:prev_all_diff_cont:capacity_HighP1_lowN1_best               -3.291e-02  2.823e-02  1.346e+04  -1.166  0.24374
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best                4.443e-03  2.983e-02  1.346e+04   0.149  0.88159
# trialnumberRS:all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -1.543e-02  4.682e-02  1.346e+04  -0.330  0.74170

wind2_m8_all_intfxTime_fulldatafit_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                    trialnumberRS * all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                    (1 | subjectnumber), data = clean_data_dm, REML = F)


# Fully interacting models (4-way) are not any better
# seems like the simpler models are better

# ~ Window 2 Model 7: trial number x current difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind2_m7_time_diffContAll_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS +
                                       all_diff_cont +
                                       (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind2_m7_time_diffContAll_rfx)
# trialnumberRS -2.934e-01  2.970e-02  8.218e+01  -9.878 1.27e-15 ***
# all_diff_cont -2.475e-02  4.482e-03  1.387e+04  -5.523 3.39e-08 ***

AIC(wind2_m0_diffContAll_rfx) # 229.9473
AIC(wind2_m7_time_diffContAll_rfx) # -3033.568


# ~ Window 2 Model 8: trial number x current difficulty x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind2_m8_time_diffContAll_prevdiffContAll_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS +
                                                             all_diff_cont * prev_all_diff_cont +
                                                             (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind2_m8_time_diffContAll_prevdiffContAll_intxn_rfx)
# trialnumberRS                    -2.854e-01  2.960e-02  8.243e+01  -9.642 3.63e-15 ***
# all_diff_cont                    -2.071e-02  7.042e-03  1.374e+04  -2.940  0.00328 **
# prev_all_diff_cont                4.662e-04  7.061e-03  1.374e+04   0.066  0.94736
# all_diff_cont:prev_all_diff_cont -8.562e-03  1.045e-02  1.374e+04  -0.819  0.41252

AIC(wind2_m2_diffContAll_prevdiffContAll_intxn_rfx) # 4.161395
AIC(wind2_m8_time_diffContAll_prevdiffContAll_intxn_rfx) # -3050.916

# ~ Window 2 Model 9: trial number x current difficulty x previous difficulty x wmc ~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind2_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS +
                                                                    all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                                                    (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind2_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)
# trialnumberRS                                               -2.832e-01  3.020e-02  8.039e+01  -9.379 1.52e-14 ***
# all_diff_cont                                               -1.920e-02  7.427e-03  1.342e+04  -2.586  0.00973 **
# prev_all_diff_cont                                           2.500e-03  7.455e-03  1.342e+04   0.335  0.73734
# capacity_HighP1_lowN1_best                                   1.312e-01  7.817e-02  7.976e+01   1.679  0.09706 .
# all_diff_cont:prev_all_diff_cont                            -1.039e-02  1.114e-02  1.341e+04  -0.933  0.35101
# all_diff_cont:capacity_HighP1_lowN1_best                     6.918e-03  7.422e-03  1.344e+04   0.932  0.35134
# prev_all_diff_cont:capacity_HighP1_lowN1_best                7.459e-03  7.451e-03  1.344e+04   1.001  0.31680
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -6.466e-03  1.113e-02  1.341e+04  -0.581  0.56143

AIC(wind2_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx) # 79.807
AIC(wind2_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx) # -2894.476

wind2_m0_time_choice_currDiff_prevDiff_span_Fullyrfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS + choice + all_diff_cont * prev_all_diff_cont * complexspan_demeaned +
                                (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind2_m0_time_choice_currDiff_prevDiff_span_Fullyrfx)
# trialnumberRS                                         -2.831e-01  3.025e-02  8.039e+01  -9.359 1.66e-14 ***
# choice                                                 2.822e-02  3.740e-03  1.342e+04   7.546 4.76e-14 ***
# all_diff_cont                                         -2.010e-02  7.206e-03  1.341e+04  -2.789  0.00529 **
# prev_all_diff_cont                                    -8.449e-04  7.217e-03  1.341e+04  -0.117  0.90680
# complexspan_demeaned                                   7.072e-01  3.738e-01  7.996e+01   1.892  0.06212 .
# all_diff_cont:prev_all_diff_cont                      -7.779e-03  1.067e-02  1.340e+04  -0.729  0.46584
# all_diff_cont:complexspan_demeaned                     4.255e-02  3.894e-02  1.344e+04   1.093  0.27449
# prev_all_diff_cont:complexspan_demeaned                4.621e-02  3.910e-02  1.344e+04   1.182  0.23732
# all_diff_cont:prev_all_diff_cont:complexspan_demeaned -3.660e-02  5.781e-02  1.342e+04  -0.633  0.52664

wind2_m0_time_choice_currDiff_prevDiff_capacity_Fullyrfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS + choice + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                                              (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind2_m0_time_choice_currDiff_prevDiff_capacity_Fullyrfx) # trialnumberRS -0.29927    0.02970 81.96775  -10.08 5.31e-16 ***
# trialnumberRS                                               -2.830e-01  3.024e-02  8.039e+01  -9.357 1.67e-14 ***
# choice                                                       2.823e-02  3.740e-03  1.342e+04   7.549 4.68e-14 ***
# all_diff_cont                                               -1.690e-02  7.417e-03  1.341e+04  -2.278   0.0227 *
# prev_all_diff_cont                                           2.440e-03  7.440e-03  1.341e+04   0.328   0.7430
# capacity_HighP1_lowN1_best                                   1.312e-01  7.815e-02  7.975e+01   1.679   0.0971 .
# all_diff_cont:prev_all_diff_cont                            -1.108e-02  1.111e-02  1.341e+04  -0.997   0.3189
# all_diff_cont:capacity_HighP1_lowN1_best                     7.424e-03  7.407e-03  1.344e+04   1.002   0.3162
# prev_all_diff_cont:capacity_HighP1_lowN1_best                7.581e-03  7.436e-03  1.344e+04   1.020   0.3079
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -7.243e-03  1.111e-02  1.341e+04  -0.652   0.5145

# In window 2:
# - Smaller pupil with time
# - Larger pupil with risky choice
# - Smaller pupil with increasing difficulty (i.e. easier choices >>> difficult choices)
#
# - weak (?) effect of capacity (larger pupil with increasing cog. capacity)


# TODO: there are some signs that reducing model complexity strengthens all_diff_cont
# effect. Might be worth comparing AICs to pick "best model" from this class of
# models for Window 2.

# ~ "All" 2-way & 3-way interaction model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind2_m11_allIntfx_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                  trialnumberRS * all_diff_cont * capacity_HighP1_lowN1_best +
                                  trialnumberRS * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                  (1 | subjectnumber), data = clean_data_dm, REML = F)
summary(wind2_m11_allIntfx_rfx)
# trialnumberRS                                               -3.093e-01  1.332e-02  1.346e+04 -23.218  < 2e-16 ***
# all_diff_cont                                               -5.066e-02  1.162e-02  1.346e+04  -4.360 1.31e-05 ***
# prev_all_diff_cont                                          -6.843e-03  1.165e-02  1.346e+04  -0.587   0.5571
# capacity_HighP1_lowN1_best                                   1.130e-01  8.110e-02  8.023e+01   1.394   0.1673
# trialnumberRS:all_diff_cont                                  4.709e-02  1.873e-02  1.346e+04   2.515   0.0119 *
# trialnumberRS:prev_all_diff_cont                             8.784e-03  1.875e-02  1.346e+04   0.468   0.6395
# trialnumberRS:capacity_HighP1_lowN1_best                     3.398e-02  1.332e-02  1.346e+04   2.551   0.0108 *
# all_diff_cont:capacity_HighP1_lowN1_best                     1.929e-02  1.162e-02  1.346e+04   1.660   0.0970 .
# prev_all_diff_cont:capacity_HighP1_lowN1_best                2.795e-02  1.165e-02  1.346e+04   2.399   0.0165 *
# trialnumberRS:all_diff_cont:capacity_HighP1_lowN1_best      -2.620e-02  1.873e-02  1.346e+04  -1.399   0.1618
# trialnumberRS:prev_all_diff_cont:capacity_HighP1_lowN1_best -3.972e-02  1.875e-02  1.346e+04  -2.118   0.0342 *

wind2_m11_simpler_allIntfx_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                trialnumberRS * all_diff_cont +
                                trialnumberRS * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                (1 | subjectnumber), data = clean_data_dm, REML = F)
summary(wind2_m11_simpler_allIntfx_rfx)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind2_m11_all2way_Intfx_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                trialnumberRS * all_diff_cont +
                                all_diff_cont * capacity_HighP1_lowN1_best +
                                trialnumberRS * prev_all_diff_cont +
                                trialnumberRS * capacity_HighP1_lowN1_best +
                                prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                (1 | subjectnumber), data = clean_data_dm, REML = F)
summary(wind2_m11_all2way_Intfx_rfx)
# trialnumberRS                            -3.164e-01  1.293e-02  1.347e+04 -24.476  < 2e-16 ***
# all_diff_cont                            -5.758e-02  1.092e-02  1.347e+04  -5.272 1.37e-07 ***
# prev_all_diff_cont                       -1.662e-02  1.095e-02  1.347e+04  -1.517  0.12919
# capacity_HighP1_lowN1_best                1.314e-01  8.085e-02  7.933e+01   1.626  0.10793
# trialnumberRS:all_diff_cont               5.686e-02  1.760e-02  1.347e+04   3.231  0.00123 **
# trialnumberRS:prev_all_diff_cont          2.297e-02  1.762e-02  1.347e+04   1.304  0.19241
# trialnumberRS:capacity_HighP1_lowN1_best  8.280e-03  7.210e-03  1.347e+04   1.148  0.25082


# full: wind2_m8_all_intfxTime_rfx
# 3-ways: wind2_m11_allIntfx_rfx
# 2-ways: wind2_m11_all2way_Intfx_rfx

anova(wind2_m8_all_intfxTime_fulldatafit_rfx, wind2_m11_allIntfx_rfx, wind2_m11_all2way_Intfx_rfx)
# 3-way regression is performing the best (SO FAR).


summary(wind2_m11_allIntfx_rfx)
# AIC -1577.7
# Finds that...
# - pupil constricts with increasing current difficulty (and this effect goes away by the end of the study)
#    - ... and this might be stronger for low capacity folks than high capacity folks (p = 0.10)
# - pupil constricts in general over time (more so for low capacity than high capacity folks)
# - pupil constricts with previous difficulty in low capacity folks (and dilates for high capacity folks)
#    - ... and this effect goes away with time-in-study.
#


# adding in choice

wind2_m11_full_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                trialnumberRS * all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best * choice +
                                (1 | subjectnumber), data = clean_data_dm, REML = F)
summary(wind2_m11_full_rfx)

wind2_m11_sepdifficulties_4ways_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                            trialnumberRS * all_diff_cont * capacity_HighP1_lowN1_best * choice +
                            trialnumberRS * prev_all_diff_cont * capacity_HighP1_lowN1_best * choice +
                            (1 | subjectnumber), data = clean_data_dm, REML = F)
summary(wind2_m11_sepdifficulties_4ways_rfx)

wind2_m11_sepdifficulties_3ways_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                                             trialnumberRS * capacity_HighP1_lowN1_best * choice +
                                             all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best +
                                             all_diff_cont * trialnumberRS * choice +
                                             all_diff_cont * capacity_HighP1_lowN1_best * choice +
                                             prev_all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best +
                                             prev_all_diff_cont * trialnumberRS * choice +
                                             prev_all_diff_cont * capacity_HighP1_lowN1_best * choice +
                                             (1 | subjectnumber), data = clean_data_dm, REML = F)
summary(wind2_m11_sepdifficulties_3ways_rfx)

wind2_m11_2ways_rfx = lmer(wind2_effort_isi_mean ~ 1 +
                             trialnumberRS * capacity_HighP1_lowN1_best +
                             trialnumberRS * choice +
                             capacity_HighP1_lowN1_best * choice +
                             all_diff_cont * trialnumberRS +
                             all_diff_cont * capacity_HighP1_lowN1_best +
                             all_diff_cont * choice +
                             prev_all_diff_cont * trialnumberRS +
                             prev_all_diff_cont * capacity_HighP1_lowN1_best +
                             prev_all_diff_cont * choice +
                             (1 | subjectnumber), data = clean_data_dm, REML = F)
summary(wind2_m11_2ways_rfx)

anova(wind2_m11_full_rfx, wind2_m11_sepdifficulties_4ways_rfx, wind2_m11_sepdifficulties_3ways_rfx, wind2_m11_2ways_rfx)
# pairwise model comparisons to best-performing model
anova(wind2_m11_full_rfx, wind2_m11_sepdifficulties_3ways_rfx)
anova(wind2_m11_2ways_rfx, wind2_m11_sepdifficulties_3ways_rfx)
anova(wind2_m11_sepdifficulties_4ways_rfx, wind2_m11_sepdifficulties_3ways_rfx)
# The 3-way regression 'wins', doing much better than the 2-way regression, and
# virtually the same as the 4-way & full regressions

summary(wind2_m11_sepdifficulties_3ways_rfx)
#                                                               Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)                                                  4.118e+00  8.038e-02  8.375e+01  51.225  < 2e-16 ***
# trialnumberRS                                               -2.512e-01  1.879e-02  1.347e+04 -13.367  < 2e-16 ***
# choice                                                       7.751e-02  1.445e-02  1.347e+04   5.365 8.24e-08 ***
# trialnumberRS:capacity_HighP1_lowN1_best                     3.278e-02  1.535e-02  1.347e+04   2.135  0.03276 *
# trialnumberRS:choice                                        -1.125e-01  2.591e-02  1.347e+04  -4.341 1.43e-05 ***
# choice:all_diff_cont                                        -5.991e-02  2.205e-02  1.347e+04  -2.717  0.00659 **
# trialnumberRS:choice:all_diff_cont                           1.231e-01  3.524e-02  1.347e+04   3.493  0.00048 ***
# trialnumberRS:capacity_HighP1_lowN1_best:prev_all_diff_cont -3.855e-02  1.872e-02  1.347e+04  -2.059  0.03951 *
# capacity_HighP1_lowN1_best:choice:prev_all_diff_cont         2.350e-02  1.005e-02  1.347e+04   2.338  0.01943 *

# TREND?
# trialnumberRS:choice:prev_all_diff_cont                      5.919e-02  3.511e-02  1.347e+04   1.686  0.09188 .


# NON-SIGNIFICANT TERMS:
# capacity_HighP1_lowN1_best                                   1.206e-01  8.020e-02  8.298e+01   1.504  0.13637
# all_diff_cont                                               -1.557e-02  1.649e-02  1.347e+04  -0.944  0.34514
# prev_all_diff_cont                                           3.267e-03  1.607e-02  1.347e+04   0.203  0.83886
# capacity_HighP1_lowN1_best:choice                           -1.224e-02  9.951e-03  1.347e+04  -1.230  0.21878
# trialnumberRS:all_diff_cont                                 -1.972e-02  2.626e-02  1.347e+04  -0.751  0.45274
# capacity_HighP1_lowN1_best:all_diff_cont                     1.491e-02  1.286e-02  1.347e+04   1.159  0.24646
# trialnumberRS:prev_all_diff_cont                            -1.676e-02  2.570e-02  1.347e+04  -0.652  0.51435
# capacity_HighP1_lowN1_best:prev_all_diff_cont                1.561e-02  1.269e-02  1.347e+04   1.231  0.21851
# choice:prev_all_diff_cont                                   -2.488e-02  2.194e-02  1.347e+04  -1.134  0.25689
# trialnumberRS:capacity_HighP1_lowN1_best:choice             -2.218e-03  1.501e-02  1.347e+04  -0.148  0.88250
# trialnumberRS:capacity_HighP1_lowN1_best:all_diff_cont      -2.315e-02  1.873e-02  1.347e+04  -1.236  0.21639
# capacity_HighP1_lowN1_best:choice:all_diff_cont              5.444e-03  1.027e-02  1.347e+04   0.530  0.59613


# SO
# In window 2 (the ISI immediately following choice & preceding outcome), pupil...
# - constricts over time (more for low vs. high capacity folks)
# - dilates after risky choices (this happens less with time, and is mostly on easy choices)
# - exhibits a complex pattern depending prev. difficulty (modified by time, capacity, and choice),
#   dominated by a shift to dilation for prev. difficulty for low capacity people late (and
#   constriction for prev. difficulty for high capacity people late).

# - HIGHER LEVEL OF TAKEAWAY -
# Exhibits patterns of dilation/constriction that appear to reflect effort AND arousal
# (related to risky choices and current difficulty) that change over time, and
# are affected by individual cognitive differences.



# Difficulty & Choice effect calculations
adc = 1 # current difficulty (All Diff Cont)
chc = 1 # choice
trialn = 1 # trial number

chc * 7.751e-02 + chc * trialn * -1.125e-01 + chc * adc * -5.991e-02 + chc * adc * trialn * 1.231e-01

# easy, risky, early --> 0.08
# diff, risky, early --> 0.02

# easy, risky, late --> -0.03
# diff, risky, late --> 0.03

# --> This is mostly about changes in EASY risky choices.
# for those trials --> Early, big dilation; late, constriction.


padc = 1 # prev. difficulty (prev_all_diff_cont) (0->1)
chc = 0 # choice (0/1)
cap = 1 # WMC group +1/-1
trialn = 1 # trial number (0->1)


# prev. easy --> 0 (NO EFFECTS)

# prev. diff, risky, low cap, early -->  -0.02
# prev. diff, safe, low cap, early -->    0
# prev. diff, risky, low cap, late -->    0.01
# prev. diff, safe, low cap, late -->     0.03

# prev. diff, risky, high cap, early -->  0.02
# prev. diff, safe, high cap, early -->    0
# prev. diff, risky, high cap, late -->  -0.02
# prev. diff, safe, high cap, late -->    -0.04

-3.855e-02 * trialn * cap * padc + 2.350e-02 * cap * chc * padc

# trialnumberRS:capacity_HighP1_lowN1_best:prev_all_diff_cont -3.855e-02  1.872e-02  1.347e+04  -2.059  0.03951 *
# capacity_HighP1_lowN1_best:choice:prev_all_diff_cont         2.350e-02  1.005e-02  1.347e+04   2.338  0.01943 *


# For thesis: INCLUDE NCS
wind2_m11_sepdifficulties_3ways_rfx_NCS = lmer(wind2_effort_isi_mean ~ 1 +
                                                 trialnumberRS * capacity_HighP1_lowN1_best * choice +
                                                 trialnumberRS * NCS_HighP1_LowN1 +
                                                 all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best +
                                                 all_diff_cont * trialnumberRS * choice +
                                                 all_diff_cont * capacity_HighP1_lowN1_best * choice +
                                                 all_diff_cont * NCS_HighP1_LowN1 +
                                                 prev_all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best +
                                                 prev_all_diff_cont * trialnumberRS * choice +
                                                 prev_all_diff_cont * capacity_HighP1_lowN1_best * choice +
                                                 prev_all_diff_cont * NCS_HighP1_LowN1 +
                                                 (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$NCS_HighP1_LowN1),], REML = F)
summary(wind2_m11_sepdifficulties_3ways_rfx_NCS)
# ^^^ Can use this model! NCS interacts with time - stronger effect of time for
# higher NCS people (lesser effect of time for lower NCS folks), modulating the
# non-specific changes in pupil dilation over time in the study.
#
# No interactions with current or prev. difficulty, and no main effect.

# trialnumberRS                                               -2.452e-01  1.892e-02  1.331e+04 -12.962  < 2e-16 ***
# choice                                                       8.183e-02  1.455e-02  1.331e+04   5.624 1.90e-08 ***
# trialnumberRS:capacity_HighP1_lowN1_best                     3.376e-02  1.543e-02  1.331e+04   2.187 0.028742 *
# trialnumberRS:choice                                        -1.186e-01  2.610e-02  1.331e+04  -4.545 5.55e-06 ***
# trialnumberRS:NCS_HighP1_LowN1                              -2.666e-02  6.967e-03  1.331e+04  -3.827 0.000131 ***
# trialnumberRS:NCS_HighP1_LowN1                              -2.666e-02  6.967e-03  1.331e+04  -3.827 0.000131 ***
# choice:all_diff_cont                                        -6.596e-02  2.223e-02  1.331e+04  -2.967 0.003015 **
# trialnumberRS:choice:all_diff_cont                           1.336e-01  3.557e-02  1.331e+04   3.757 0.000173 ***
# trialnumberRS:capacity_HighP1_lowN1_best:prev_all_diff_cont -3.858e-02  1.883e-02  1.331e+04  -2.049 0.040509 *
# trialnumberRS:choice:prev_all_diff_cont                      6.139e-02  3.542e-02  1.331e+04   1.733 0.083137 .
# capacity_HighP1_lowN1_best:choice:prev_all_diff_cont         2.337e-02  1.011e-02  1.331e+04   2.312 0.020791 *

# capacity_HighP1_lowN1_best                                   1.214e-01  8.085e-02  8.195e+01   1.502 0.136931
# NCS_HighP1_LowN1                                            -3.253e-02  7.562e-02  8.052e+01  -0.430 0.668170
# all_diff_cont                                               -1.297e-02  1.662e-02  1.331e+04  -0.780 0.435119
# prev_all_diff_cont                                           2.779e-03  1.619e-02  1.331e+04   0.172 0.863683
# capacity_HighP1_lowN1_best:choice                           -1.165e-02  1.001e-02  1.331e+04  -1.164 0.244406
# trialnumberRS:all_diff_cont                                 -2.474e-02  2.645e-02  1.331e+04  -0.935 0.349784
# capacity_HighP1_lowN1_best:all_diff_cont                     1.494e-02  1.294e-02  1.331e+04   1.154 0.248322
# NCS_HighP1_LowN1:all_diff_cont                              -5.003e-03  4.869e-03  1.331e+04  -1.028 0.304168
# trialnumberRS:prev_all_diff_cont                            -1.687e-02  2.589e-02  1.331e+04  -0.652 0.514712
# capacity_HighP1_lowN1_best:prev_all_diff_cont                1.587e-02  1.276e-02  1.331e+04   1.243 0.213743
# choice:prev_all_diff_cont                                   -2.704e-02  2.212e-02  1.331e+04  -1.222 0.221553
# NCS_HighP1_LowN1:prev_all_diff_cont                          2.030e-03  4.872e-03  1.331e+04   0.417 0.676997
# trialnumberRS:capacity_HighP1_lowN1_best:choice             -2.130e-03  1.509e-02  1.331e+04  -0.141 0.887747
# trialnumberRS:capacity_HighP1_lowN1_best:all_diff_cont      -2.329e-02  1.883e-02  1.331e+04  -1.237 0.216157

# capacity_HighP1_lowN1_best:choice:all_diff_cont              6.608e-03  1.033e-02  1.331e+04   0.640 0.522462


wind2_m11_sepdifficulties_3ways_rfx_limNCS = lmer(wind2_effort_isi_mean ~ 1 +
                                             trialnumberRS * capacity_HighP1_lowN1_best * choice +
                                             all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best +
                                             all_diff_cont * trialnumberRS * choice +
                                             all_diff_cont * capacity_HighP1_lowN1_best * choice +
                                             prev_all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best +
                                             prev_all_diff_cont * trialnumberRS * choice +
                                             prev_all_diff_cont * capacity_HighP1_lowN1_best * choice +
                                             (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$NCS_HighP1_LowN1),], REML = F)
summary(wind2_m11_sepdifficulties_3ways_rfx_limNCS)
anova(wind2_m11_sepdifficulties_3ways_rfx_NCS, wind2_m11_sepdifficulties_3ways_rfx_limNCS)
#


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### WINDOW 3 REGRESSIONS: Evaluation (2 seconds, outcome start to +1000 after iti start) #####
# ~ predictors: current difficulty, choice made, previous difficulty, WMC, NFC, and choice

### single predictor models
# ~ Window 3 Model 0: current difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind3_m0_diffCat = lm(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind3_m0_diffCat) # easyP1difficultN1 0.001395   0.007025   0.199    0.843

wind3_m0_diffCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m0_diffCat_rfx) # easyP1difficultN1 1.815e-03  2.502e-03 9.821e+03   0.726    0.468

wind3_m0_diffCont = lm(wind3_eval_otciti_mean ~ 1 + diff_cont, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous dynamic only
summary(wind3_m0_diffCont) # diff_cont   -0.005569   0.015134  -0.368    0.713

wind3_m0_diffCont_rfx = lmer(wind3_eval_otciti_mean ~ 1 + diff_cont + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m0_diffCont_rfx) # diff_cont   -3.451e-03  5.391e-03  9.821e+03   -0.64    0.522

wind3_m0_diffContAll = lm(wind3_eval_otciti_mean ~ 1 + all_diff_cont, data = clean_data_dm) # continuous all
summary(wind3_m0_diffContAll) # all_diff_cont -0.076171   0.014544  -5.237 1.65e-07 ***

wind3_m0_diffContAll_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind3_m0_diffContAll_rfx) # all_diff_cont -4.869e-02  5.430e-03  1.394e+04  -8.968   <2e-16 ***

AIC(wind3_m0_diffContAll) # 30153.6
AIC(wind3_m0_diffContAll_rfx) # 2591.443

# ~ Window 3 Model 0: previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind3_m0_prevdiffCat = lm(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1_prev, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind3_m0_prevdiffCat) # easyP1difficultN1_prev -0.003894   0.007055  -0.552    0.581

wind3_m0_prevdiffCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1_prev + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m0_prevdiffCat_rfx) # easyP1difficultN1_prev -2.685e-03  2.512e-03  9.821e+03  -1.069    0.285

wind3_m0_prevdiffContAll = lm(wind3_eval_otciti_mean ~ 1 + prev_all_diff_cont, data = clean_data_dm) # continuous all
summary(wind3_m0_prevdiffContAll) # prev_all_diff_cont -0.058917   0.014624  -4.029 5.63e-05 ***

wind3_m0_prevdiffContAll_rfx = lmer(wind3_eval_otciti_mean ~ 1 + prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind3_m0_prevdiffContAll_rfx) # prev_all_diff_cont -3.246e-02  5.440e-03  1.380e+04  -5.966 2.48e-09 ***

AIC(wind3_m0_prevdiffContAll) # 29860.87
AIC(wind3_m0_prevdiffContAll_rfx) # 2475.255

# ~ Window 3 Model 0: capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind3_m0_wmcCat = lm(wind3_eval_otciti_mean ~ 1 + capacity_HighP1_lowN1_best, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind3_m0_wmcCat) # capacity_HighP1_lowN1_best 0.124647   0.007525   16.57   <2e-16 ***

wind3_m0_wmcCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m0_wmcCat_rfx) # capacity_HighP1_lowN1_best   0.1259     0.0776 79.0002   1.622    0.109

wind3_m0_wmcCont = lm(wind3_eval_otciti_mean ~ 1 + complexspan_demeaned, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous
# complexspan_demeaned 0.682062   0.036026   18.93   <2e-16 ***

wind3_m0_wmcCont_rfx = lmer(wind3_eval_otciti_mean ~ 1 + complexspan_demeaned + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
# complexspan_demeaned  0.68721    0.37115 79.00038   1.852   0.0678 .

AIC(wind3_m0_wmcCat) # 20293.03

# ~ Window 3 Model 0: need for cognition scale (ncs -> nfc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind3_m0_nfcCat = lm(wind3_eval_otciti_mean ~ 1 + NCS_HighP1_LowN1, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind3_m0_nfcCat) # NCS_HighP1_LowN1 -0.024045   0.007119  -3.377 0.000734 ***

wind3_m0_nfcCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + NCS_HighP1_LowN1 + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m0_nfcCat_rfx) # NCS_HighP1_LowN1 -0.02413    0.07359 80.00016  -0.328    0.744

wind3_m0_nfcCont = lm(wind3_eval_otciti_mean ~ 1 + NCS, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous
summary(wind3_m0_nfcCont) # NCS         0.0016210  0.0006664   2.432    0.015 *

wind3_m0_nfcCont_rfx = lmer(wind3_eval_otciti_mean ~ 1 + NCS + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m0_nfcCont_rfx) # NCS          0.001606   0.006890 80.000140   0.233    0.816

AIC(wind3_m0_nfcCat) #20864.19
AIC(wind3_m0_nfcCont) # 20869.67

# ~ Window 3 Model 0: choice made ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind3_m0_optCat = lm(wind3_eval_otciti_mean ~ 1 + choice, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical choice made
summary(wind3_m0_optCat) # choice      0.144999   0.013976   10.38   <2e-16 ***

wind3_m0_optCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + choice + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m0_optCat_rfx) # choice      9.633e-02  5.103e-03 9.823e+03   18.88   <2e-16 ***

AIC(wind3_m0_optCat) # 20917.61
AIC(wind3_m0_optCat_rfx) # 791.5353

### main & interaction effects
# ~ Window 3 Model 1: current difficulty x choice made ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind3_m1_diffCat_optCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 + choice +
                                     (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & choice made
summary(wind3_m1_diffCat_optCat_rfx)
# easyP1difficultN1 1.135e-03  2.458e-03 9.820e+03   0.462    0.644
# choice            9.630e-02  5.104e-03 9.822e+03  18.867   <2e-16 ***

wind3_m1_diffCat_optCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 * choice +
                                           (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m1_diffCat_optCat_intxn_rfx)
# easyP1difficultN1         1.447e-03  3.499e-03  9.820e+03   0.413    0.679
# choice                    9.634e-02  5.114e-03  9.821e+03  18.838   <2e-16 ***
# easyP1difficultN1:choice -6.332e-04  5.055e-03  9.820e+03  -0.125    0.900

wind3_m1_diffContAll_optCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont + choice +
                                         (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & categorical choice made
summary(wind3_m1_diffContAll_optCat_rfx)
# all_diff_cont -4.169e-02  5.341e-03  1.394e+04  -7.805 6.35e-15 ***
# choice         1.002e-01  4.416e-03  1.394e+04  22.683  < 2e-16 ***

wind3_m1_diffContAll_optCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont * choice +
                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind3_m1_diffContAll_optCat_intxn_rfx)
# all_diff_cont        -3.888e-02  7.727e-03  1.394e+04  -5.032 4.92e-07 ***
# choice                1.028e-01  6.920e-03  1.394e+04  14.863  < 2e-16 ***
# all_diff_cont:choice -5.409e-03  1.074e-02  1.394e+04  -0.504    0.615

# ~ Window 3 Model 2: current difficulty x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind3_m2_diffCat_prevdiffCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 + easyP1difficultN1_prev +
                                          (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & previous difficulty
summary(wind3_m2_diffCat_prevdiffCat_rfx)
# easyP1difficultN1       1.769e-03  2.502e-03  9.820e+03   0.707    0.480
# easyP1difficultN1_prev -2.654e-03  2.513e-03  9.820e+03  -1.056    0.291

wind3_m2_diffCat_prevdiffCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev +
                                                (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m2_diffCat_prevdiffCat_intxn_rfx)
# easyP1difficultN1                         1.767e-03  2.502e-03  9.819e+03   0.706    0.480
# easyP1difficultN1_prev                   -2.645e-03  2.513e-03  9.819e+03  -1.053    0.292
# easyP1difficultN1:easyP1difficultN1_prev -3.663e-03  2.524e-03  9.819e+03  -1.451    0.147

wind3_m2_diffContAll_prevdiffContAll_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont + prev_all_diff_cont +
                                                  (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & previous difficulty
summary(wind3_m2_diffContAll_prevdiffContAll_rfx)
# all_diff_cont      -4.646e-02  5.423e-03  1.380e+04  -8.567  < 2e-16 ***
# prev_all_diff_cont -3.027e-02  5.432e-03  1.380e+04  -5.573 2.55e-08 ***

wind3_m2_diffContAll_prevdiffContAll_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont * prev_all_diff_cont +
                                                        (1 | subjectnumber), data = clean_data_dm)
summary(wind3_m2_diffContAll_prevdiffContAll_intxn_rfx)
# all_diff_cont                    -4.945e-02  8.467e-03  1.380e+04  -5.841 5.32e-09 ***
# prev_all_diff_cont               -3.328e-02  8.488e-03  1.380e+04  -3.921 8.88e-05 ***
# all_diff_cont:prev_all_diff_cont  5.860e-03  1.272e-02  1.380e+04   0.461    0.645

# ~ Window 3 Model 3: current difficulty x capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind3_m3_diffCat_wmcCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 + capacity_HighP1_lowN1_best +
                                     (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & wmc
summary(wind3_m3_diffCat_wmcCat_rfx)
# easyP1difficultN1          1.557e-03  2.542e-03 9.586e+03   0.612    0.540
# capacity_HighP1_lowN1_best 1.259e-01  7.760e-02 7.900e+01   1.622    0.109

wind3_m3_diffCat_wmcCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 * capacity_HighP1_lowN1_best +
                                           (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m3_diffCat_wmcCat_intxn_rfx)
# easyP1difficultN1                            -5.498e-04  2.721e-03  9.585e+03  -0.202   0.8399
# capacity_HighP1_lowN1_best                    1.259e-01  7.760e-02  7.900e+01   1.622   0.1087
# easyP1difficultN1:capacity_HighP1_lowN1_best -5.893e-03  2.721e-03  9.585e+03  -2.166   0.0303 *

wind3_m3_diffContAll_wmcCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont + capacity_HighP1_lowN1_best +
                                         (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & categorical wmc
summary(wind3_m3_diffContAll_wmcCat_rfx)
# all_diff_cont              -4.824e-02  5.520e-03  1.361e+04  -8.739   <2e-16 ***
# capacity_HighP1_lowN1_best  1.199e-01  7.849e-02  7.900e+01   1.527    0.131

wind3_m3_diffContAll_wmcCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best +
                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind3_m3_diffContAll_wmcCat_intxn_rfx)
# all_diff_cont                            -4.311e-02  5.816e-03  1.361e+04  -7.413 1.31e-13 ***
# capacity_HighP1_lowN1_best                1.119e-01  7.856e-02  7.921e+01   1.425  0.15817
# all_diff_cont:capacity_HighP1_lowN1_best  1.626e-02  5.816e-03  1.361e+04   2.795  0.00519 **

wind3_m3_diffContAll_wmcCont_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont + complexspan_demeaned +
                                          (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & wmc
summary(wind3_m3_diffContAll_wmcCont_rfx)
# all_diff_cont        -4.823e-02  5.520e-03  1.361e+04  -8.738   <2e-16 ***
# complexspan_demeaned  6.531e-01  3.757e-01  7.900e+01   1.739    0.086 .

wind3_m3_diffContAll_wmcCont_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont * complexspan_demeaned +
                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind3_m3_diffContAll_wmcCont_intxn_rfx)
# all_diff_cont                      -4.907e-02  5.533e-03  1.361e+04  -8.867   <2e-16 ***
# complexspan_demeaned                6.205e-01  3.761e-01  7.927e+01   1.650   0.1029
# all_diff_cont:complexspan_demeaned  6.251e-02  2.963e-02  1.361e+04   2.110   0.0349 *

# ~ Window 3 Model 4: current difficulty x choice x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind3_m4_diffCat_optCat_prevdiffCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 + choice + easyP1difficultN1_prev +
                                                 (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty, choice made, & previous difficulty
summary(wind3_m4_diffCat_optCat_prevdiffCat_rfx)
# easyP1difficultN1       1.091e-03  2.458e-03  9.819e+03   0.444    0.657
# choice                  9.628e-02  5.104e-03  9.821e+03  18.864   <2e-16 ***
# easyP1difficultN1_prev -2.510e-03  2.469e-03  9.819e+03  -1.017    0.309

wind3_m4_diffCat_optCat_prevdiffCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 * choice * easyP1difficultN1_prev +
                                                       (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m4_diffCat_optCat_prevdiffCat_intxn_rfx)
# easyP1difficultN1                                1.500e-03  3.499e-03  9.816e+03   0.429   0.6682
# choice                                           9.632e-02  5.114e-03  9.817e+03  18.836   <2e-16 ***
# easyP1difficultN1_prev                           2.357e-03  3.471e-03  9.815e+03   0.679   0.4971
# easyP1difficultN1:choice                        -8.412e-04  5.054e-03  9.816e+03  -0.166   0.8678
# easyP1difficultN1:easyP1difficultN1_prev        -3.939e-03  3.478e-03  9.815e+03  -1.133   0.2574
# choice:easyP1difficultN1_prev                   -9.863e-03  4.949e-03  9.815e+03  -1.993   0.0463 *
# easyP1difficultN1:choice:easyP1difficultN1_prev  4.531e-04  4.960e-03  9.815e+03   0.091   0.9272

wind3_m4_diffContAll_optCat_prevdiffContAll_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont + choice + prev_all_diff_cont +
                                                         (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and previous difficulty & categorical choice made
summary(wind3_m4_diffContAll_optCat_prevdiffContAll_rfx)
# all_diff_cont      -3.950e-02  5.336e-03  1.380e+04  -7.402 1.42e-13 ***
# choice              9.924e-02  4.412e-03  1.380e+04  22.494  < 2e-16 ***
# prev_all_diff_cont -3.151e-02  5.336e-03  1.380e+04  -5.905 3.60e-09 ***

wind3_m4_diffContAll_optCat_prevdiffContAll_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont * choice * prev_all_diff_cont +
                                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind3_m4_diffContAll_optCat_prevdiffContAll_intxn_rfx)
# all_diff_cont                           -4.546e-02  1.192e-02  1.380e+04  -3.816 0.000136 ***
# choice                                   8.777e-02  1.051e-02  1.380e+04   8.351  < 2e-16 ***
# prev_all_diff_cont                      -5.011e-02  1.210e-02  1.380e+04  -4.141 3.48e-05 ***
# all_diff_cont:choice                     5.057e-03  1.672e-02  1.380e+04   0.302 0.762361
# all_diff_cont:prev_all_diff_cont         1.852e-02  1.788e-02  1.380e+04   1.035 0.300537
# choice:prev_all_diff_cont                2.964e-02  1.658e-02  1.380e+04   1.788 0.073794 .
# all_diff_cont:choice:prev_all_diff_cont -2.286e-02  2.502e-02  1.380e+04  -0.914 0.360921

# ~ Window 3 Model 5: current difficulty x choice x capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind3_m5_diffCat_optCat_wmcCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 + choice + capacity_HighP1_lowN1_best +
                                            (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty, choice made, & wmc
summary(wind3_m5_diffCat_optCat_wmcCat_rfx)
# easyP1difficultN1          9.370e-04  2.498e-03 9.585e+03   0.375    0.708
# choice                     9.609e-02  5.187e-03 9.587e+03  18.524   <2e-16 ***
# capacity_HighP1_lowN1_best 1.264e-01  7.738e-02 7.900e+01   1.633    0.106

wind3_m5_diffCat_optCat_wmcCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 * choice * capacity_HighP1_lowN1_best +
                                                  (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m5_diffCat_optCat_wmcCat_intxn_rfx)
# easyP1difficultN1                                    1.094e-04  3.805e-03  9.582e+03   0.029   0.9771
# choice                                               9.433e-02  5.610e-03  9.583e+03  16.816   <2e-16 ***
# capacity_HighP1_lowN1_best                           1.291e-01  7.743e-02  7.920e+01   1.667   0.0995 .
# easyP1difficultN1:choice                            -2.879e-03  5.535e-03  9.583e+03  -0.520   0.6030
# easyP1difficultN1:capacity_HighP1_lowN1_best        -3.798e-03  3.805e-03  9.582e+03  -0.998   0.3183
# choice:capacity_HighP1_lowN1_best                   -5.326e-03  5.610e-03  9.583e+03  -0.949   0.3425
# easyP1difficultN1:choice:capacity_HighP1_lowN1_best -5.022e-03  5.535e-03  9.583e+03  -0.907   0.3643

wind3_m5_diffContAll_optCat_wmcCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont + choice + capacity_HighP1_lowN1_best +
                                                (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty & categorical choice and wmc
summary(wind3_m5_diffContAll_optCat_wmcCat_rfx)
# all_diff_cont              -4.142e-02  5.428e-03  1.361e+04  -7.630 2.51e-14 ***
# choice                      1.013e-01  4.488e-03  1.361e+04  22.571  < 2e-16 ***
# capacity_HighP1_lowN1_best  1.208e-01  7.826e-02  7.900e+01   1.544    0.127

wind3_m5_diffContAll_optCat_wmcCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont * choice * capacity_HighP1_lowN1_best +
                                                      (1 | subjectnumber), data = clean_data_dm)
summary(wind3_m5_diffContAll_optCat_wmcCat_intxn_rfx)
# all_diff_cont                                   -3.657e-02  8.238e-03  1.360e+04  -4.439 9.12e-06 ***
# choice                                           1.011e-01  7.337e-03  1.360e+04  13.783  < 2e-16 ***
# capacity_HighP1_lowN1_best                       1.187e-01  7.842e-02  7.960e+01   1.514   0.1341
# all_diff_cont:choice                             1.664e-03  1.163e-02  1.360e+04   0.143   0.8863
# all_diff_cont:capacity_HighP1_lowN1_best         5.065e-03  8.238e-03  1.360e+04   0.615   0.5387
# choice:capacity_HighP1_lowN1_best               -1.133e-02  7.337e-03  1.360e+04  -1.544   0.1225
# all_diff_cont:choice:capacity_HighP1_lowN1_best  2.285e-02  1.163e-02  1.360e+04   1.964   0.0495 *

wind3_m5_diffContAll_optCat_wmcCont_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont + choice + complexspan_demeaned +
                                                 (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and wmc & categorical choice
summary(wind3_m5_diffContAll_optCat_wmcCont_rfx)
# all_diff_cont        -4.141e-02  5.428e-03  1.361e+04  -7.628 2.53e-14 ***
# choice                1.013e-01  4.488e-03  1.361e+04  22.570  < 2e-16 ***
# complexspan_demeaned  6.541e-01  3.746e-01  7.900e+01   1.746   0.0847 .

wind3_m5_diffContAll_optCat_wmcCont_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont * choice * complexspan_demeaned +
                                                       (1 | subjectnumber), data = clean_data_dm)
summary(wind3_m5_diffContAll_optCat_wmcCont_intxn_rfx)
# all_diff_cont                             -3.788e-02  7.906e-03  1.360e+04  -4.791 1.67e-06 ***
# choice                                     1.061e-01  7.079e-03  1.360e+04  14.984  < 2e-16 ***
# complexspan_demeaned                       6.629e-01  3.754e-01  7.976e+01   1.766  0.08125 .
# all_diff_cont:choice                      -7.597e-03  1.098e-02  1.360e+04  -0.692  0.48895
# all_diff_cont:complexspan_demeaned        -2.891e-02  4.281e-02  1.360e+04  -0.675  0.49958
# choice:complexspan_demeaned               -6.925e-02  3.789e-02  1.360e+04  -1.828  0.06759 .
# all_diff_cont:choice:complexspan_demeaned  1.639e-01  5.796e-02  1.360e+04   2.828  0.00468 **

# ~ Window 3 Model 6: current difficulty x previous difficulty x capacity (wmc) ~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind3_m6_diffCat_prevdiffCat_wmcCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 + easyP1difficultN1_prev + capacity_HighP1_lowN1_best +
                                                 (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty and wmc
summary(wind3_m6_diffCat_prevdiffCat_wmcCat_rfx)
# easyP1difficultN1           1.508e-03  2.542e-03  9.585e+03   0.593    0.553
# easyP1difficultN1_prev     -3.019e-03  2.553e-03  9.585e+03  -1.183    0.237
# capacity_HighP1_lowN1_best  1.259e-01  7.760e-02  7.900e+01   1.622    0.109

wind3_m6_diffCat_prevdiffCat_wmcCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev * capacity_HighP1_lowN1_best +
                                                       (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind3_m6_diffCat_prevdiffCat_wmcCat_intxn_rfx)
# easyP1difficultN1                                                   -6.008e-04  2.721e-03  9.581e+03  -0.221   0.8253
# easyP1difficultN1_prev                                              -2.841e-03  2.733e-03  9.581e+03  -1.040   0.2985
# capacity_HighP1_lowN1_best                                           1.258e-01  7.759e-02  7.900e+01   1.622   0.1088
# easyP1difficultN1:easyP1difficultN1_prev                            -4.376e-03  2.746e-03  9.581e+03  -1.594   0.1110
# easyP1difficultN1:capacity_HighP1_lowN1_best                        -5.900e-03  2.721e-03  9.581e+03  -2.168   0.0302 *
# easyP1difficultN1_prev:capacity_HighP1_lowN1_best                    5.447e-04  2.733e-03  9.581e+03   0.199   0.8420
# easyP1difficultN1:easyP1difficultN1_prev:capacity_HighP1_lowN1_best -1.305e-03  2.746e-03  9.581e+03  -0.475   0.6345

wind3_m6_diffContAll_prevdiffContAll_wmcCat_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont + prev_all_diff_cont + capacity_HighP1_lowN1_best +
                                                         (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and previous difficulty and categorical wmc
summary(wind3_m6_diffContAll_prevdiffContAll_wmcCat_rfx)
# all_diff_cont              -4.610e-02  5.516e-03  1.347e+04  -8.358  < 2e-16 ***
# prev_all_diff_cont         -2.967e-02  5.525e-03  1.347e+04  -5.371 7.97e-08 ***
# capacity_HighP1_lowN1_best  1.197e-01  7.845e-02  7.900e+01   1.526    0.131

wind3_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind3_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)
# all_diff_cont                                               -4.335e-02  8.941e-03  1.347e+04  -4.849 1.26e-06 ***
# prev_all_diff_cont                                          -3.118e-02  8.970e-03  1.347e+04  -3.476  0.00051 ***
# capacity_HighP1_lowN1_best                                   1.086e-01  7.865e-02  7.967e+01   1.380  0.17133
# all_diff_cont:prev_all_diff_cont                             5.218e-03  1.358e-02  1.347e+04   0.384  0.70081
# all_diff_cont:capacity_HighP1_lowN1_best                     1.874e-02  8.941e-03  1.347e+04   2.096  0.03612 *
# prev_all_diff_cont:capacity_HighP1_lowN1_best                5.428e-03  8.970e-03  1.347e+04   0.605  0.54513
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -2.706e-03  1.358e-02  1.347e+04  -0.199  0.84208

wind3_m6_diffContAll_prevdiffContAll_wmcCont_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont + prev_all_diff_cont + complexspan_demeaned +
                                                          (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty, previous difficulty, & wmc
summary(wind3_m6_diffContAll_prevdiffContAll_wmcCont_rfx)
# all_diff_cont        -4.609e-02  5.516e-03  1.347e+04  -8.357  < 2e-16 ***
# prev_all_diff_cont   -2.967e-02  5.525e-03  1.347e+04  -5.369 8.03e-08 ***
# complexspan_demeaned  6.512e-01  3.755e-01  7.900e+01   1.734   0.0868 .

wind3_m6_diffContAll_prevdiffContAll_wmcCont_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + all_diff_cont * prev_all_diff_cont * complexspan_demeaned +
                                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind3_m6_diffContAll_prevdiffContAll_wmcCont_intxn_rfx)
# all_diff_cont                                         -5.017e-02  8.724e-03  1.347e+04  -5.750 9.11e-09 ***
# prev_all_diff_cont                                    -3.298e-02  8.742e-03  1.347e+04  -3.773 0.000162 ***
# complexspan_demeaned                                   6.083e-01  3.767e-01  7.987e+01   1.615 0.110273
# all_diff_cont:prev_all_diff_cont                       6.092e-03  1.307e-02  1.347e+04   0.466 0.641159
# all_diff_cont:complexspan_demeaned                     7.250e-02  4.683e-02  1.347e+04   1.548 0.121608
# prev_all_diff_cont:complexspan_demeaned                1.410e-02  4.699e-02  1.347e+04   0.300 0.764175
# all_diff_cont:prev_all_diff_cont:complexspan_demeaned -7.725e-03  7.043e-02  1.347e+04  -0.110 0.912663


# ~ Window 3 Model 7: trial number x current difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind3_m7_time_diffContAll_rfx = lmer(wind3_eval_otciti_mean ~ 1 + trialnumberRS +
                                       all_diff_cont +
                                       (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind3_m7_time_diffContAll_rfx)
# trialnumberRS -2.788e-01  3.121e-02  8.221e+01  -8.933 9.58e-14 ***
# all_diff_cont -1.430e-02  4.990e-03  1.387e+04  -2.866  0.00416 **

AIC(wind3_m0_diffContAll_rfx) # 2591.443
AIC(wind3_m7_time_diffContAll_rfx) # -43.13793


# ~ Window 3 Model 8: trial number x current difficulty x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind3_m8_time_diffContAll_prevdiffContAll_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + trialnumberRS +
                                                             all_diff_cont * prev_all_diff_cont +
                                                             (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind3_m8_time_diffContAll_prevdiffContAll_intxn_rfx)
# trialnumberRS                    -2.739e-01  3.117e-02  8.248e+01  -8.788 1.82e-13 ***
# all_diff_cont                    -4.831e-03  7.845e-03  1.375e+04  -0.616    0.538
# prev_all_diff_cont                1.187e-02  7.867e-03  1.375e+04   1.508    0.132
# all_diff_cont:prev_all_diff_cont -1.904e-02  1.164e-02  1.374e+04  -1.635    0.102

AIC(wind3_m2_diffContAll_prevdiffContAll_intxn_rfx) # 2421.331
AIC(wind3_m8_time_diffContAll_prevdiffContAll_intxn_rfx) # -74.10818

# ~ Window 3 Model 9: trial number x current difficulty x previous difficulty x wmc ~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind3_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + trialnumberRS +
                                                                    all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                                                    (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind3_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)
# trialnumberRS                                               -2.750e-01  3.177e-02  8.044e+01  -8.658 3.98e-13 ***
# all_diff_cont                                                3.322e-05  8.292e-03  1.342e+04   0.004   0.9968
# prev_all_diff_cont                                           1.297e-02  8.325e-03  1.342e+04   1.557   0.1194
# capacity_HighP1_lowN1_best                                   1.153e-01  7.654e-02  7.992e+01   1.506   0.1359
# all_diff_cont:prev_all_diff_cont                            -1.976e-02  1.243e-02  1.341e+04  -1.589   0.1121
# all_diff_cont:capacity_HighP1_lowN1_best                     1.831e-02  8.286e-03  1.344e+04   2.209   0.0272 *
# prev_all_diff_cont:capacity_HighP1_lowN1_best                5.703e-03  8.320e-03  1.344e+04   0.686   0.4930
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -5.635e-03  1.243e-02  1.342e+04  -0.453   0.6504

AIC(wind3_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx) # 2515.433
AIC(wind3_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx) # 69.66496



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### WINDOW 4 REGRESSIONS: Preparation (2 seconds, +1000 after iti start to +3000 after iti start) #####
# ~ predictors: current difficulty, choice made, previous difficulty, WMC, NFC, and choice

### single predictor models
# ~ Window 4 Model 0: current difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind4_m0_diffCat = lm(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind4_m0_diffCat) # easyP1difficultN1 -0.007446   0.007264  -1.025    0.305

wind4_m0_diffCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m0_diffCat_rfx) # easyP1difficultN1 -7.438e-03  2.968e-03  9.832e+03  -2.506   0.0122 *

wind4_m0_diffCont = lm(wind4_prep_lateiti_mean ~ 1 + diff_cont, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous dynamic only
summary(wind4_m0_diffCont) # diff_cont    0.01283    0.01565    0.82    0.412

wind4_m0_diffCont_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + diff_cont + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m0_diffCont_rfx) # diff_cont   1.568e-02  6.395e-03 9.832e+03   2.452   0.0142 *

wind4_m0_diffContAll = lm(wind4_prep_lateiti_mean ~ 1 + all_diff_cont, data = clean_data_dm) # continuous all
summary(wind4_m0_diffContAll) # all_diff_cont -0.057336   0.015039  -3.813 0.000138 ***

wind4_m0_diffContAll_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m0_diffContAll_rfx) # all_diff_cont -2.724e-02  6.271e-03  1.396e+04  -4.344 1.41e-05 ***

AIC(wind4_m0_diffCat_rfx) # 4514.823
AIC(wind4_m0_diffContAll_rfx) # 6632.549

wind4_m0_trial_currDiff_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS + all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m0_trial_currDiff_rfx)
# trialnumberRS -2.773e-01  8.597e-03  1.395e+04  -32.25   <2e-16 ***
# all_diff_cont  6.510e-03  6.140e-03  1.396e+04    1.06    0.289

# no effect of current difficulty for window 4 when controlling for trial

wind4_m0_trial_currDiff_intfx_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS * all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m0_trial_currDiff_intfx_rfx)
# trialnumberRS               -3.012e-01  1.329e-02  1.395e+04 -22.667   <2e-16 ***
# all_diff_cont               -2.236e-02  1.367e-02  1.395e+04  -1.635   0.1020
# trialnumberRS:all_diff_cont  5.222e-02  2.210e-02  1.395e+04   2.363   0.0181 *

# no mfx of current difficulty but there is a 2-way with trial

anova(wind4_m0_trial_currDiff_rfx,wind4_m0_trial_currDiff_intfx_rfx)
#                                   npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# wind4_m0_trial_currDiff_rfx          5 5619.4 5657.2 -2804.7   5609.4
# wind4_m0_trial_currDiff_intfx_rfx    6 5615.8 5661.1 -2801.9   5603.8 5.5849  1    0.01812 *

# ~ Window 4 Model 0: previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind4_m0_prevdiffCat = lm(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1_prev, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind4_m0_prevdiffCat) # easyP1difficultN1_prev -0.003852   0.007295  -0.528    0.597

wind4_m0_prevdiffCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1_prev + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m0_prevdiffCat_rfx) # easyP1difficultN1_prev -2.935e-03  2.981e-03  9.832e+03  -0.985    0.325

wind4_m0_prevdiffContAll = lm(wind4_prep_lateiti_mean ~ 1 + prev_all_diff_cont, data = clean_data_dm) # continuous all
summary(wind4_m0_prevdiffContAll) # prev_all_diff_cont -0.063911   0.015108   -4.23 2.35e-05 ***

wind4_m0_prevdiffContAll_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m0_prevdiffContAll_rfx) # prev_all_diff_cont -3.543e-02  6.279e-03  1.382e+04  -5.642 1.71e-08 ***

AIC(wind4_m0_prevdiffContAll) # 30811.58
AIC(wind4_m0_prevdiffContAll_rfx) # 6455.823

wind4_m0_trial_prevDiff_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS + prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m0_trial_prevDiff_rfx)
# trialnumberRS      -2.701e-01  8.675e-03  1.382e+04 -31.140   <2e-16 ***
# prev_all_diff_cont -2.717e-03  6.160e-03  1.382e+04  -0.441    0.659

# no effect of previous difficulty either

wind4_m0_trial_prevDiff_intfx_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS * prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m0_trial_prevDiff_intfx_rfx)
# trialnumberRS                    -3.020e-01  1.340e-02  1.382e+04 -22.538  < 2e-16 ***
# prev_all_diff_cont               -4.138e-02  1.385e-02  1.382e+04  -2.987  0.00282 **
# trialnumberRS:prev_all_diff_cont  6.949e-02  2.230e-02  1.382e+04   3.116  0.00184 **

# when interacting, there is a mfx of previous difficulty and an intfx with trial
# when the previous trial is more difficult, the more constricted the pupil
# as trial progressses, the effect of previous gets weaker???

anova(wind4_m0_trial_prevDiff_rfx,wind4_m0_trial_prevDiff_intfx_rfx)
#                                   npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# wind4_m0_trial_prevDiff_rfx          5 5508.8 5546.5 -2749.4   5498.8
# wind4_m0_trial_prevDiff_intfx_rfx    6 5501.1 5546.3 -2744.6   5489.1 9.709  1   0.001834 **

# ~ Window 4 Model 0: capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind4_m0_wmcCat = lm(wind4_prep_lateiti_mean ~ 1 + capacity_HighP1_lowN1_best, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind4_m0_wmcCat) # capacity_HighP1_lowN1_best 0.121672   0.007778   15.64   <2e-16 ***

wind4_m0_wmcCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m0_wmcCat_rfx) # capacity_HighP1_lowN1_best  0.12256    0.07838 79.00035   1.564    0.122

wind4_m0_wmcCont = lm(wind4_prep_lateiti_mean ~ 1 + complexspan_demeaned, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous
summary(wind4_m0_wmcCont) # complexspan_demeaned 0.673266   0.037248   18.07   <2e-16 ***

wind4_m0_wmcCont_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + complexspan_demeaned + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m0_wmcCont_rfx) # complexspan_demeaned  0.67694    0.37483 79.00066   1.806   0.0747 .

AIC(wind4_m0_wmcCat) # 20967.2

wind4_m0_time_wmcCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                                        trialnumberRS + capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m0_time_wmcCat_rfx)
# trialnumberRS              -2.764e-01  8.594e-03  1.362e+04  -32.16   <2e-16 ***
# capacity_HighP1_lowN1_best  1.170e-01  7.959e-02  7.900e+01    1.47    0.146

wind4_m0_time_wmcCat_intfx_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                                               trialnumberRS * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m0_time_wmcCat_intfx_rfx)
# trialnumberRS                            -2.711e-01  9.200e-03  1.362e+04 -29.467   <2e-16 ***
# capacity_HighP1_lowN1_best                1.095e-01  7.973e-02  7.953e+01   1.374    0.173
# trialnumberRS:capacity_HighP1_lowN1_best  1.481e-02  9.200e-03  1.362e+04   1.610    0.108

# no mfx of wmc

anova(wind4_m0_time_wmcCat_rfx,wind4_m0_time_wmcCat_intfx_rfx)
#                                npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# wind4_m0_time_wmcCat_rfx          5 5544.5 5582.1 -2767.2   5534.5
# wind4_m0_time_wmcCat_intfx_rfx    6 5543.9 5589.0 -2765.9   5531.9 2.5909  1     0.1075

wind4_m0_time_wmcCont_intfx_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                               trialnumberRS * complexspan_demeaned + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m0_time_wmcCont_intfx_rfx)
# trialnumberRS                      -2.764e-01  8.593e-03  1.362e+04 -32.172   <2e-16 ***
# complexspan_demeaned                5.974e-01  3.814e-01  7.954e+01   1.566   0.1213
# trialnumberRS:complexspan_demeaned  1.031e-01  4.421e-02  1.362e+04   2.332   0.0197 *

# if interacting with trial, there is an interaction effect between trial and continuous but not categorical wmc

# ~ Window 4 Model 0: need for cognition scale (ncs -> nfc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind4_m0_nfcCat = lm(wind4_prep_lateiti_mean ~ 1 + NCS_HighP1_LowN1, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical
summary(wind4_m0_nfcCat) # NCS_HighP1_LowN1 -0.033375   0.007357  -4.537 5.79e-06 ***

wind4_m0_nfcCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + NCS_HighP1_LowN1 + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m0_nfcCat_rfx) # NCS_HighP1_LowN1 -0.03331    0.07431 80.00048  -0.448    0.655

wind4_m0_nfcCont = lm(wind4_prep_lateiti_mean ~ 1 + NCS, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # continuous
summary(wind4_m0_nfcCont) # NCS         0.0008455  0.0006892   1.227     0.22

wind4_m0_nfcCont_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + NCS + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m0_nfcCont_rfx) # NCS         8.323e-04  6.963e-03 8.000e+01   0.120    0.905

AIC(wind4_m0_nfcCat) # 21539.57

# ~ Window 4 Model 0: choice made ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind4_m0_optCat = lm(wind4_prep_lateiti_mean ~ 1 + choice, data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical choice made
summary(wind4_m0_optCat) # choice       0.07077    0.01451   4.876  1.1e-06 ***

wind4_m0_optCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + choice + (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m0_optCat_rfx) # choice      2.497e-02  6.159e-03 9.835e+03   4.054 5.08e-05 ***

AIC(wind4_m0_optCat) # 21699.22
AIC(wind4_m0_optCat_rfx) # 4503.22

### main & interaction effects
# ~ Window 4 Model 1: current difficulty x choice made ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind4_m1_diffCat_optCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 + choice +
                                     (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & choice made
summary(wind4_m1_diffCat_optCat_rfx)
# easyP1difficultN1 -7.608e-03  2.966e-03  9.831e+03  -2.565   0.0103 *
# choice             2.519e-02  6.158e-03  9.834e+03   4.091 4.34e-05 ***

wind4_m1_diffCat_optCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 * choice +
                                           (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m1_diffCat_optCat_intxn_rfx)
# easyP1difficultN1        -1.282e-02  4.220e-03  9.831e+03  -3.039  0.00238 **
# choice                    2.454e-02  6.169e-03  9.833e+03   3.978 7.01e-05 ***
# easyP1difficultN1:choice  1.059e-02  6.096e-03  9.832e+03   1.737  0.08243 .

wind4_m1_diffContAll_optCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont + choice +
                                         (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & categorical choice made
summary(wind4_m1_diffContAll_optCat_rfx)
# all_diff_cont -2.555e-02  6.277e-03  1.396e+04  -4.070 4.73e-05 ***
# choice         2.451e-02  5.189e-03  1.396e+04   4.723 2.34e-06 ***

wind4_m1_diffContAll_optCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont * choice +
                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m1_diffContAll_optCat_intxn_rfx)
# all_diff_cont        -1.361e-02  9.078e-03  1.395e+04  -1.499   0.1338
# choice                3.590e-02  8.132e-03  1.395e+04   4.415 1.02e-05 ***
# all_diff_cont:choice -2.297e-02  1.262e-02  1.395e+04  -1.820   0.0688 .

# ~ Window 4 Model 2: current difficulty x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind4_m2_diffCat_prevdiffCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 + easyP1difficultN1_prev +
                                          (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & previous difficulty
summary(wind4_m2_diffCat_prevdiffCat_rfx)
# easyP1difficultN1      -7.490e-03  2.968e-03  9.831e+03  -2.524   0.0116 *
# easyP1difficultN1_prev -3.064e-03  2.981e-03  9.831e+03  -1.028   0.3040

wind4_m2_diffCat_prevdiffCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev +
                                                (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m2_diffCat_prevdiffCat_intxn_rfx)
# easyP1difficultN1                        -7.490e-03  2.968e-03  9.830e+03  -2.523   0.0116 *
# easyP1difficultN1_prev                   -3.064e-03  2.981e-03  9.830e+03  -1.028   0.3039
# easyP1difficultN1:easyP1difficultN1_prev  2.806e-04  2.994e-03  9.830e+03   0.094   0.9253

wind4_m2_diffContAll_prevdiffContAll_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont + prev_all_diff_cont +
                                                  (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & previous difficulty
summary(wind4_m2_diffContAll_prevdiffContAll_rfx)
# all_diff_cont      -2.495e-02  6.273e-03  1.382e+04  -3.978  7.0e-05 ***
# prev_all_diff_cont -3.425e-02  6.283e-03  1.382e+04  -5.451  5.1e-08 ***

wind4_m2_diffContAll_prevdiffContAll_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont * prev_all_diff_cont +
                                                        (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m2_diffContAll_prevdiffContAll_intxn_rfx)
# all_diff_cont                    -4.645e-02  9.791e-03  1.382e+04  -4.745 2.11e-06 ***
# prev_all_diff_cont               -5.582e-02  9.816e-03  1.382e+04  -5.687 1.32e-08 ***
# all_diff_cont:prev_all_diff_cont  4.207e-02  1.471e-02  1.382e+04   2.860  0.00424 **

# there seems to be an intfx between current and previous difficulty for window 4 unlike window 2

wind4_m2_trial_currDiff_prevDiff_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS +
                                              all_diff_cont * prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m2_trial_currDiff_prevDiff_rfx)
# trialnumberRS                    -2.711e-01  8.818e-03  1.381e+04 -30.747   <2e-16 ***
# all_diff_cont                     1.238e-03  9.598e-03  1.381e+04   0.129    0.897
# prev_all_diff_cont               -7.631e-03  9.625e-03  1.381e+04  -0.793    0.428
# all_diff_cont:prev_all_diff_cont  9.262e-03  1.427e-02  1.381e+04   0.649    0.516

# no effect of current or previous or an interaction

anova(wind4_m2_diffContAll_prevdiffContAll_intxn_rfx,wind4_m2_trial_currDiff_prevDiff_rfx)
#                                                npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# wind4_m2_diffContAll_prevdiffContAll_intxn_rfx    6 6424.1 6469.4 -3206.1   6412.1
# wind4_m2_trial_currDiff_prevDiff_rfx              7 5511.4 5564.2 -2748.7   5497.4 914.71  1  < 2.2e-16 ***

wind4_m2_trial_currDiff_prevDiff_intfx_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS *
                                              all_diff_cont * prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m2_trial_currDiff_prevDiff_intfx_rfx)
# trialnumberRS                                  -3.236e-01  2.033e-02  1.381e+04 -15.923   <2e-16 ***
# all_diff_cont                                  -1.846e-02  2.101e-02  1.381e+04  -0.878   0.3797
# prev_all_diff_cont                             -3.961e-02  2.112e-02  1.381e+04  -1.875   0.0608 .
# trialnumberRS:all_diff_cont                     4.624e-02  3.456e-02  1.381e+04   1.338   0.1809
# trialnumberRS:prev_all_diff_cont                6.772e-02  3.467e-02  1.381e+04   1.953   0.0508 .
# all_diff_cont:prev_all_diff_cont                7.742e-04  3.598e-02  1.381e+04   0.022   0.9828
# trialnumberRS:all_diff_cont:prev_all_diff_cont -2.300e-03  5.638e-02  1.381e+04  -0.041   0.9675

anova(wind4_m2_trial_currDiff_prevDiff_rfx,wind4_m2_trial_currDiff_prevDiff_intfx_rfx)
#                                            npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# wind4_m2_trial_currDiff_prevDiff_rfx          7 5511.4 5564.2 -2748.7   5497.4
# wind4_m2_trial_currDiff_prevDiff_intfx_rfx   10 5503.8 5579.2 -2741.9   5483.8 13.676  3   0.003381 **

# seems like the intfx model is better
# the intfx between current and previous difficulty is gone
# marginal effect of previous and intfx with trial

# ~ Window 4 Model 3: current difficulty x capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind4_m3_diffCat_wmcCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 + capacity_HighP1_lowN1_best +
                                     (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty & wmc
summary(wind4_m3_diffCat_wmcCat_rfx)
# easyP1difficultN1          -7.363e-03  3.005e-03  9.596e+03  -2.450   0.0143 *
# capacity_HighP1_lowN1_best  1.226e-01  7.838e-02  7.900e+01   1.564   0.1219

wind4_m3_diffCat_wmcCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 * capacity_HighP1_lowN1_best +
                                           (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m3_diffCat_wmcCat_intxn_rfx)
# easyP1difficultN1                            -8.564e-03  3.218e-03  9.595e+03  -2.662  0.00779 **
# capacity_HighP1_lowN1_best                    1.226e-01  7.838e-02  7.900e+01   1.564  0.12188
# easyP1difficultN1:capacity_HighP1_lowN1_best -3.361e-03  3.218e-03  9.595e+03  -1.045  0.29619

wind4_m3_diffContAll_wmcCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont + capacity_HighP1_lowN1_best +
                                         (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & categorical wmc
summary(wind4_m3_diffContAll_wmcCat_rfx)
# all_diff_cont              -2.726e-02  6.363e-03  1.362e+04  -4.284 1.85e-05 ***
# capacity_HighP1_lowN1_best  1.164e-01  7.958e-02  7.900e+01   1.462    0.148

wind4_m3_diffContAll_wmcCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best +
                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m3_diffContAll_wmcCat_intxn_rfx)
# all_diff_cont                            -2.434e-02  6.705e-03  1.362e+04  -3.630 0.000285 ***
# capacity_HighP1_lowN1_best                1.119e-01  7.966e-02  7.927e+01   1.404 0.164163
# all_diff_cont:capacity_HighP1_lowN1_best  9.246e-03  6.705e-03  1.362e+04   1.379 0.167934

wind4_m3_diffContAll_wmcCont_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont + complexspan_demeaned +
                                          (1 | subjectnumber), data = clean_data_dm) # continuous all current difficulty & wmc
summary(wind4_m3_diffContAll_wmcCont_rfx)
# all_diff_cont        -2.725e-02  6.363e-03  1.362e+04  -4.282 1.86e-05 ***
# complexspan_demeaned  6.457e-01  3.807e-01  7.900e+01   1.696   0.0938 .

wind4_m3_diffContAll_wmcCont_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont * complexspan_demeaned +
                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m3_diffContAll_wmcCont_intxn_rfx)
# all_diff_cont                      -2.761e-02  6.379e-03  1.362e+04  -4.329 1.51e-05 ***
# complexspan_demeaned                6.315e-01  3.812e-01  7.935e+01   1.657    0.102
# all_diff_cont:complexspan_demeaned  2.738e-02  3.416e-02  1.362e+04   0.802    0.423

wind4_m3_time_currDiff_wmcCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS + all_diff_cont * capacity_HighP1_lowN1_best +
                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m3_time_currDiff_wmcCat_intxn_rfx)
# trialnumberRS                            -2.779e-01  8.717e-03  1.362e+04 -31.879   <2e-16 ***
# all_diff_cont                             9.432e-03  6.555e-03  1.362e+04   1.439   0.1502
# capacity_HighP1_lowN1_best                1.119e-01  7.968e-02  7.925e+01   1.404   0.1642
# all_diff_cont:capacity_HighP1_lowN1_best  1.077e-02  6.469e-03  1.362e+04   1.665   0.0959 .

# similar to window 2: no effects of wmc

wind4_m0_time_currDiffcont_wmcCat_intfx_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                                                     trialnumberRS * all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m0_time_currDiffcont_wmcCat_intfx_rfx)
# trialnumberRS                                          -2.983e-01  1.395e-02  1.362e+04 -21.388   <2e-16 ***
# all_diff_cont                                          -2.209e-02  1.485e-02  1.362e+04  -1.488   0.1368
# capacity_HighP1_lowN1_best                              1.043e-01  7.996e-02  8.039e+01   1.304   0.1959
# trialnumberRS:all_diff_cont                             5.582e-02  2.401e-02  1.362e+04   2.325   0.0201 *
# trialnumberRS:capacity_HighP1_lowN1_best                1.626e-02  1.395e-02  1.362e+04   1.166   0.2437
# all_diff_cont:capacity_HighP1_lowN1_best                1.125e-02  1.485e-02  1.362e+04   0.758   0.4487
# trialnumberRS:all_diff_cont:capacity_HighP1_lowN1_best -3.794e-03  2.401e-02  1.362e+04  -0.158   0.8744

anova(wind4_m3_time_currDiff_wmcCat_intxn_rfx,wind4_m0_time_currDiffcont_wmcCat_intfx_rfx)
#                                             npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# wind4_m3_time_currDiff_wmcCat_intxn_rfx        7 5544.8 5597.4 -2765.4   5530.8
# wind4_m0_time_currDiffcont_wmcCat_intfx_rfx   10 5542.6 5617.8 -2761.3   5522.6 8.2093  3    0.04188 *

# intfx model seems to be better? but there is no effect of wmc

wind4_m0_time_currDiffcont_wmcCont_intfx_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                                                      trialnumberRS * all_diff_cont * complexspan_demeaned + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m0_time_currDiffcont_wmcCont_intfx_rfx)
# trialnumberRS                                    -3.056e-01  1.359e-02  1.362e+04 -22.482  < 2e-16 ***
# all_diff_cont                                    -2.792e-02  1.397e-02  1.362e+04  -1.998  0.04572 *
# complexspan_demeaned                              5.731e-01  3.828e-01  8.065e+01   1.497  0.13832
# trialnumberRS:all_diff_cont                       6.013e-02  2.259e-02  1.362e+04   2.662  0.00779 **
# trialnumberRS:complexspan_demeaned                1.218e-01  7.168e-02  1.362e+04   1.699  0.08943 .
# all_diff_cont:complexspan_demeaned                4.234e-02  7.330e-02  1.362e+04   0.578  0.56350
# trialnumberRS:all_diff_cont:complexspan_demeaned -2.672e-02  1.188e-01  1.362e+04  -0.225  0.82202

# if interacting with trial, no effect of wmc - though without current difficulty, continuous appeared to have an interaction effect

# what if I interact with previous difficulty
wind4_m35_time_prevDiffcont_wmcCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                                                      trialnumberRS + prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m35_time_prevDiffcont_wmcCat_rfx)
# trialnumberRS                                 -2.710e-01  8.795e-03  1.349e+04 -30.809   <2e-16 ***
# prev_all_diff_cont                            -1.374e-03  6.574e-03  1.349e+04  -0.209    0.834
# capacity_HighP1_lowN1_best                     1.150e-01  7.962e-02  7.925e+01   1.444    0.153
# prev_all_diff_cont:capacity_HighP1_lowN1_best  4.517e-03  6.487e-03  1.349e+04   0.696    0.486

wind4_m35_time_prevDiffcont_wmcCat_intfx_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                                                      trialnumberRS * prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m35_time_prevDiffcont_wmcCat_intfx_rfx)
# trialnumberRS                                               -2.961e-01  1.406e-02  1.348e+04 -21.054  < 2e-16 ***
# prev_all_diff_cont                                          -3.840e-02  1.503e-02  1.348e+04  -2.554  0.01065 *
# capacity_HighP1_lowN1_best                                   9.998e-02  7.992e-02  8.042e+01   1.251  0.21453
# trialnumberRS:prev_all_diff_cont                             6.527e-02  2.422e-02  1.348e+04   2.695  0.00705 **
# trialnumberRS:capacity_HighP1_lowN1_best                     3.060e-02  1.406e-02  1.348e+04   2.176  0.02959 *
# prev_all_diff_cont:capacity_HighP1_lowN1_best                2.028e-02  1.503e-02  1.348e+04   1.349  0.17731
# trialnumberRS:prev_all_diff_cont:capacity_HighP1_lowN1_best -3.159e-02  2.422e-02  1.348e+04  -1.304  0.19221

anova(wind4_m35_time_prevDiffcont_wmcCat_rfx,wind4_m35_time_prevDiffcont_wmcCat_intfx_rfx)
#                                              npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# wind4_m35_time_prevDiffcont_wmcCat_rfx          7 5436.6 5489.2 -2711.3   5422.6
# wind4_m35_time_prevDiffcont_wmcCat_intfx_rfx   10 5427.4 5502.6 -2703.7   5407.4 15.151  3   0.001692 **

# non-intfx model seems to be the better mode???
# in the intfx model
# ~ when previous is difficult more constricted
# ~ intfx with previous and wmc

wind4_m35_time_prevDiffcont_wmcCont_intfx_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                                                       trialnumberRS * prev_all_diff_cont * complexspan_demeaned + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m35_time_prevDiffcont_wmcCont_intfx_rfx)
# trialnumberRS                                         -3.069e-01  1.371e-02  1.348e+04 -22.396  < 2e-16 ***
# prev_all_diff_cont                                    -4.706e-02  1.415e-02  1.348e+04  -3.325 0.000887 ***
# complexspan_demeaned                                   5.836e-01  3.826e-01  8.069e+01   1.525 0.131143
# trialnumberRS:prev_all_diff_cont                       7.851e-02  2.279e-02  1.348e+04   3.444 0.000574 ***
# trialnumberRS:complexspan_demeaned                     1.114e-01  7.229e-02  1.348e+04   1.541 0.123448
# prev_all_diff_cont:complexspan_demeaned                1.972e-02  7.421e-02  1.348e+04   0.266 0.790445
# trialnumberRS:prev_all_diff_cont:complexspan_demeaned -5.953e-03  1.198e-01  1.348e+04  -0.050 0.960385

# if interacting with trial and previous difficulty, there is a 2 way for categorical (continuous) wmc and trial


# ~ Window 4 Model 4: current difficulty x choice x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind4_m4_diffCat_optCat_prevdiffCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 + choice + easyP1difficultN1_prev +
                                                 (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty, choice made, & previous difficulty
summary(wind4_m4_diffCat_optCat_prevdiffCat_rfx)
# easyP1difficultN1      -7.659e-03  2.966e-03  9.830e+03  -2.582  0.00983 **
# choice                  2.517e-02  6.158e-03  9.833e+03   4.087 4.41e-05 ***
# easyP1difficultN1_prev -3.019e-03  2.978e-03  9.830e+03  -1.014  0.31078

wind4_m4_diffCat_optCat_prevdiffCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 * choice * easyP1difficultN1_prev +
                                                       (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m4_diffCat_optCat_prevdiffCat_intxn_rfx)
# easyP1difficultN1                               -1.286e-02  4.221e-03  9.827e+03  -3.047  0.00232 **
# choice                                           2.453e-02  6.170e-03  9.829e+03   3.975 7.09e-05 ***
# easyP1difficultN1_prev                          -2.060e-03  4.187e-03  9.826e+03  -0.492  0.62272
# easyP1difficultN1:choice                         1.057e-02  6.098e-03  9.828e+03   1.733  0.08319 .
# easyP1difficultN1:easyP1difficultN1_prev        -4.333e-04  4.197e-03  9.826e+03  -0.103  0.91776
# choice:easyP1difficultN1_prev                   -1.957e-03  5.973e-03  9.826e+03  -0.328  0.74316
# easyP1difficultN1:choice:easyP1difficultN1_prev  1.495e-03  5.985e-03  9.826e+03   0.250  0.80270

wind4_m4_diffContAll_optCat_prevdiffContAll_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont + choice + prev_all_diff_cont +
                                                         (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and previous difficulty & categorical choice made
summary(wind4_m4_diffContAll_optCat_prevdiffContAll_rfx)
# all_diff_cont      -2.333e-02  6.279e-03  1.382e+04  -3.716 0.000203 ***
# choice              2.327e-02  5.191e-03  1.382e+04   4.483 7.43e-06 ***
# prev_all_diff_cont -3.455e-02  6.279e-03  1.382e+04  -5.502 3.81e-08 ***

wind4_m4_diffContAll_optCat_prevdiffContAll_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont * choice * prev_all_diff_cont +
                                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m4_diffContAll_optCat_prevdiffContAll_intxn_rfx)
# all_diff_cont                           -3.563e-02  1.401e-02  1.381e+04  -2.543   0.0110 *
# choice                                   2.961e-02  1.236e-02  1.381e+04   2.395   0.0166 *
# prev_all_diff_cont                      -6.192e-02  1.423e-02  1.381e+04  -4.352 1.36e-05 ***
# all_diff_cont:choice                    -1.799e-02  1.967e-02  1.381e+04  -0.915   0.3605
# all_diff_cont:prev_all_diff_cont         4.819e-02  2.104e-02  1.381e+04   2.291   0.0220 *
# choice:prev_all_diff_cont                1.093e-02  1.950e-02  1.381e+04   0.560   0.5753
# all_diff_cont:choice:prev_all_diff_cont -1.115e-02  2.943e-02  1.381e+04  -0.379   0.7049

wind4_m65_trial_currDiff_prevDiff_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                                               trialnumberRS * all_diff_cont +
                                               trialnumberRS * prev_all_diff_cont + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m65_trial_currDiff_prevDiff_rfx)
# trialnumberRS                    -3.231e-01  1.647e-02  1.381e+04 -19.618  < 2e-16 ***
# all_diff_cont                    -1.809e-02  1.391e-02  1.381e+04  -1.301  0.19344
# prev_all_diff_cont               -3.924e-02  1.395e-02  1.381e+04  -2.813  0.00491 **
# trialnumberRS:all_diff_cont       4.506e-02  2.240e-02  1.381e+04   2.012  0.04427 *
# trialnumberRS:prev_all_diff_cont  6.653e-02  2.242e-02  1.381e+04   2.967  0.00301 **

# ~ Window 4 Model 5: current difficulty x choice x capacity (wmc) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind4_m5_diffCat_optCat_wmcCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 + choice + capacity_HighP1_lowN1_best +
                                            (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty, choice made, & wmc
summary(wind4_m5_diffCat_optCat_wmcCat_rfx)
# easyP1difficultN1          -7.516e-03  3.003e-03  9.595e+03  -2.503   0.0123 *
# choice                      2.526e-02  6.236e-03  9.597e+03   4.050 5.16e-05 ***
# capacity_HighP1_lowN1_best  1.227e-01  7.833e-02  7.900e+01   1.567   0.1212

wind4_m5_diffCat_optCat_wmcCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 * choice * capacity_HighP1_lowN1_best +
                                                  (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m5_diffCat_optCat_wmcCat_intxn_rfx)
# easyP1difficultN1                                   -1.295e-02  4.572e-03  9.592e+03  -2.832 0.004630 **
#   choice                                               2.539e-02  6.744e-03  9.594e+03   3.764 0.000168 ***
#   capacity_HighP1_lowN1_best                           1.219e-01  7.841e-02  7.928e+01   1.555 0.123999
# easyP1difficultN1:choice                             8.467e-03  6.654e-03  9.593e+03   1.273 0.203194
# easyP1difficultN1:capacity_HighP1_lowN1_best        -1.674e-03  4.572e-03  9.592e+03  -0.366 0.714221
# choice:capacity_HighP1_lowN1_best                    1.654e-03  6.744e-03  9.594e+03   0.245 0.806303
# easyP1difficultN1:choice:capacity_HighP1_lowN1_best -3.581e-03  6.654e-03  9.593e+03  -0.538 0.590500

wind4_m5_diffContAll_optCat_wmcCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont + choice + capacity_HighP1_lowN1_best +
                                                (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty & categorical choice and wmc
summary(wind4_m5_diffContAll_optCat_wmcCat_rfx)
# all_diff_cont              -2.560e-02  6.368e-03  1.362e+04  -4.020 5.84e-05 ***
# choice                      2.489e-02  5.264e-03  1.362e+04   4.729 2.28e-06 ***
# capacity_HighP1_lowN1_best  1.166e-01  7.952e-02  7.900e+01   1.466    0.146

wind4_m5_diffContAll_optCat_wmcCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont * choice * capacity_HighP1_lowN1_best +
                                                      (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m5_diffContAll_optCat_wmcCat_intxn_rfx)
# all_diff_cont                                   -1.547e-02  9.660e-03  1.362e+04  -1.602 0.109185
# choice                                           3.324e-02  8.607e-03  1.362e+04   3.862 0.000113 ***
# capacity_HighP1_lowN1_best                       1.177e-01  7.972e-02  7.979e+01   1.476 0.143772
# all_diff_cont:choice                            -1.270e-02  1.365e-02  1.362e+04  -0.931 0.351865
# all_diff_cont:capacity_HighP1_lowN1_best        -4.941e-03  9.660e-03  1.362e+04  -0.512 0.609005
# choice:capacity_HighP1_lowN1_best               -1.031e-02  8.607e-03  1.362e+04  -1.198 0.230798
# all_diff_cont:choice:capacity_HighP1_lowN1_best  2.790e-02  1.365e-02  1.362e+04   2.045 0.040908 *

wind4_m5_diffContAll_optCat_wmcCont_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont + choice + complexspan_demeaned +
                                                 (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and wmc & categorical choice
summary(wind4_m5_diffContAll_optCat_wmcCont_rfx)
# all_diff_cont        -2.559e-02  6.368e-03  1.362e+04  -4.019 5.88e-05 ***
# choice                2.489e-02  5.264e-03  1.362e+04   4.728 2.29e-06 ***
# complexspan_demeaned  6.460e-01  3.805e-01  7.900e+01   1.698   0.0935 .

wind4_m5_diffContAll_optCat_wmcCont_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont * choice * complexspan_demeaned +
                                                       (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m5_diffContAll_optCat_wmcCont_intxn_rfx)
# all_diff_cont                             -1.339e-02  9.274e-03  1.362e+04  -1.444   0.1489
# choice                                     3.749e-02  8.305e-03  1.362e+04   4.514 6.41e-06 ***
# complexspan_demeaned                       6.628e-01  3.815e-01  8.002e+01   1.737   0.0862 .
# all_diff_cont:choice                      -2.344e-02  1.288e-02  1.362e+04  -1.820   0.0687 .
# all_diff_cont:complexspan_demeaned        -5.613e-02  5.020e-02  1.362e+04  -1.118   0.2635
# choice:complexspan_demeaned               -5.219e-02  4.445e-02  1.362e+04  -1.174   0.2404
# all_diff_cont:choice:complexspan_demeaned  1.542e-01  6.798e-02  1.362e+04   2.268   0.0234 *

# ~ Window 4 Model 6: current difficulty x previous difficulty x capacity (wmc) ~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wind4_m6_diffCat_prevdiffCat_wmcCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 + easyP1difficultN1_prev + capacity_HighP1_lowN1_best +
                                                 (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,]) # categorical current difficulty and wmc
summary(wind4_m6_diffCat_prevdiffCat_wmcCat_rfx)
# easyP1difficultN1          -7.414e-03  3.006e-03  9.595e+03  -2.467   0.0136 *
# easyP1difficultN1_prev     -3.285e-03  3.018e-03  9.595e+03  -1.088   0.2765
# capacity_HighP1_lowN1_best  1.226e-01  7.838e-02  7.900e+01   1.564   0.1219

wind4_m6_diffCat_prevdiffCat_wmcCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev * capacity_HighP1_lowN1_best +
                                                       (1 | subjectnumber), data = clean_data_dm[clean_data_dm$static0dynamic1 == 1,])
summary(wind4_m6_diffCat_prevdiffCat_wmcCat_intxn_rfx)
# easyP1difficultN1                                                   -8.619e-03  3.219e-03  9.591e+03  -2.678  0.00742 **
# easyP1difficultN1_prev                                              -3.219e-03  3.232e-03  9.591e+03  -0.996  0.31926
# capacity_HighP1_lowN1_best                                           1.225e-01  7.838e-02  7.900e+01   1.563  0.12199
# easyP1difficultN1:easyP1difficultN1_prev                            -5.752e-04  3.247e-03  9.591e+03  -0.177  0.85940
# easyP1difficultN1:capacity_HighP1_lowN1_best                        -3.374e-03  3.219e-03  9.591e+03  -1.048  0.29451
# easyP1difficultN1_prev:capacity_HighP1_lowN1_best                    2.273e-04  3.232e-03  9.591e+03   0.070  0.94395
# easyP1difficultN1:easyP1difficultN1_prev:capacity_HighP1_lowN1_best -2.215e-03  3.247e-03  9.591e+03  -0.682  0.49517

wind4_m6_diffContAll_prevdiffContAll_wmcCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont + prev_all_diff_cont + capacity_HighP1_lowN1_best +
                                                         (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty and previous difficulty and categorical wmc
summary(wind4_m6_diffContAll_prevdiffContAll_wmcCat_rfx)
# all_diff_cont              -2.496e-02  6.364e-03  1.349e+04  -3.922 8.83e-05 ***
# prev_all_diff_cont         -3.388e-02  6.374e-03  1.349e+04  -5.315 1.08e-07 ***
# capacity_HighP1_lowN1_best  1.159e-01  7.951e-02  7.900e+01   1.457    0.149

wind4_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                                               (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)
# all_diff_cont                                               -4.312e-02  1.031e-02  1.348e+04  -4.180 2.93e-05 ***
# prev_all_diff_cont                                          -5.445e-02  1.035e-02  1.348e+04  -5.260 1.46e-07 ***
# capacity_HighP1_lowN1_best                                   1.089e-01  7.977e-02  7.987e+01   1.366  0.17582
# all_diff_cont:prev_all_diff_cont                             4.186e-02  1.567e-02  1.348e+04   2.672  0.00756 **
# all_diff_cont:capacity_HighP1_lowN1_best                     1.106e-02  1.031e-02  1.348e+04   1.073  0.28349
# prev_all_diff_cont:capacity_HighP1_lowN1_best                3.752e-03  1.035e-02  1.348e+04   0.363  0.71698
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -1.317e-03  1.567e-02  1.348e+04  -0.084  0.93302

wind4_m6_diffContAll_prevdiffContAll_wmcCont_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont + prev_all_diff_cont + complexspan_demeaned +
                                                          (1 | subjectnumber), data = clean_data_dm) # continuous current difficulty, previous difficulty, & wmc
summary(wind4_m6_diffContAll_prevdiffContAll_wmcCont_rfx)
# all_diff_cont        -2.495e-02  6.364e-03  1.349e+04  -3.920 8.88e-05 ***
# prev_all_diff_cont   -3.387e-02  6.374e-03  1.349e+04  -5.314 1.09e-07 ***
# complexspan_demeaned  6.426e-01  3.804e-01  7.900e+01   1.689   0.0951 .

wind4_m6_diffContAll_prevdiffContAll_wmcCont_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont * prev_all_diff_cont * complexspan_demeaned +
                                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m6_diffContAll_prevdiffContAll_wmcCont_intxn_rfx)
# all_diff_cont                                         -4.680e-02  1.006e-02  1.348e+04  -4.650 3.35e-06 ***
# prev_all_diff_cont                                    -5.558e-02  1.009e-02  1.348e+04  -5.511 3.64e-08 ***
# complexspan_demeaned                                   6.188e-01  3.820e-01  8.013e+01   1.620  0.10917
# all_diff_cont:prev_all_diff_cont                       4.193e-02  1.508e-02  1.348e+04   2.781  0.00543 **
# all_diff_cont:complexspan_demeaned                     2.694e-02  5.402e-02  1.348e+04   0.499  0.61796
# prev_all_diff_cont:complexspan_demeaned                9.384e-03  5.422e-02  1.348e+04   0.173  0.86259
# all_diff_cont:prev_all_diff_cont:complexspan_demeaned  1.773e-02  8.125e-02  1.348e+04   0.218  0.82727

wind4_m6_trial_currDiff_prevDiff_wmcCat_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m6_trial_currDiff_prevDiff_wmcCat_rfx)
# trialnumberRS                                               -2.719e-01  8.935e-03  1.348e+04 -30.433   <2e-16 ***
# all_diff_cont                                                4.736e-03  1.010e-02  1.348e+04   0.469    0.639
# prev_all_diff_cont                                          -5.873e-03  1.014e-02  1.348e+04  -0.579    0.562
# capacity_HighP1_lowN1_best                                   1.084e-01  7.978e-02  7.981e+01   1.359    0.178
# all_diff_cont:prev_all_diff_cont                             7.930e-03  1.520e-02  1.348e+04   0.522    0.602
# all_diff_cont:capacity_HighP1_lowN1_best                     1.412e-02  9.978e-03  1.348e+04   1.415    0.157
# prev_all_diff_cont:capacity_HighP1_lowN1_best                7.505e-03  1.001e-02  1.348e+04   0.749    0.454
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -6.678e-03  1.516e-02  1.348e+04  -0.441    0.659

# I think window 1 might be a better candidate to examine the effect of previous difficulty
# Window 4 is reflecting what has happened within a trial while window 1 is capturing what has happened previous going into the next trial
# But how to account in differences between 3s and 3.5s when window 1 captures the last 500ms - does that matter here?
# surprised that current difficulty didn't have an effect at all across all the models

wind4_m6_trial_currDiff_prevDiff_wmcCat_intfx_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS * all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m6_trial_currDiff_prevDiff_wmcCat_intfx_rfx)
# trialnumberRS                                                             -3.217e-01  2.104e-02  1.347e+04 -15.291   <2e-16 ***
# all_diff_cont                                                             -2.082e-02  2.219e-02  1.347e+04  -0.938   0.3481
# prev_all_diff_cont                                                        -3.929e-02  2.235e-02  1.347e+04  -1.758   0.0788 .
# capacity_HighP1_lowN1_best                                                 9.862e-02  8.036e-02  8.215e+01   1.227   0.2232
# trialnumberRS:all_diff_cont                                                5.466e-02  3.642e-02  1.347e+04   1.501   0.1334
# trialnumberRS:prev_all_diff_cont                                           6.844e-02  3.661e-02  1.347e+04   1.869   0.0616 .
# all_diff_cont:prev_all_diff_cont                                           6.237e-03  3.869e-02  1.347e+04   0.161   0.8719
# trialnumberRS:capacity_HighP1_lowN1_best                                   2.384e-02  2.104e-02  1.347e+04   1.133   0.2571
# all_diff_cont:capacity_HighP1_lowN1_best                                   3.893e-03  2.219e-02  1.347e+04   0.175   0.8607
# prev_all_diff_cont:capacity_HighP1_lowN1_best                              1.449e-02  2.235e-02  1.347e+04   0.648   0.5167
# trialnumberRS:all_diff_cont:prev_all_diff_cont                            -1.212e-02  6.073e-02  1.347e+04  -0.199   0.8419
# trialnumberRS:all_diff_cont:capacity_HighP1_lowN1_best                     1.103e-02  3.642e-02  1.347e+04   0.303   0.7621
# trialnumberRS:prev_all_diff_cont:capacity_HighP1_lowN1_best               -1.924e-02  3.661e-02  1.347e+04  -0.525   0.5993
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best                9.645e-03  3.869e-02  1.347e+04   0.249   0.8031
# trialnumberRS:all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -2.056e-02  6.073e-02  1.347e+04  -0.338   0.7350

anova(wind4_m6_trial_currDiff_prevDiff_wmcCat_rfx,wind4_m6_trial_currDiff_prevDiff_wmcCat_intfx_rfx)
#                                                   npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# wind4_m6_trial_currDiff_prevDiff_wmcCat_rfx         11 5440.4 5523.1 -2709.2   5418.4
# wind4_m6_trial_currDiff_prevDiff_wmcCat_intfx_rfx   18 5435.7 5571.0 -2699.9   5399.7 18.706  7   0.009162 **

# more complex models don't really do anything better

# ~ Window 4 Model 7: trial number x current difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind4_m7_time_diffContAll_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS +
                                       all_diff_cont +
                                       (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind4_m7_time_diffContAll_rfx)
# trialnumberRS -2.775e-01  3.462e-02  8.222e+01  -8.015 6.42e-12 ***
# all_diff_cont  6.448e-03  5.878e-03  1.389e+04   1.097    0.273

AIC(wind4_m0_diffContAll_rfx) # 6632.549
AIC(wind4_m7_time_diffContAll_rfx) # 4528.01


# ~ Window 4 Model 8: trial number x current difficulty x previous difficulty ~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind4_m8_time_diffContAll_prevdiffContAll_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS +
                                                             all_diff_cont * prev_all_diff_cont +
                                                             (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind4_m8_time_diffContAll_prevdiffContAll_intxn_rfx)
# trialnumberRS                    -2.706e-01  3.468e-02  8.251e+01  -7.804 1.65e-11 ***
# all_diff_cont                    -2.313e-03  9.244e-03  1.376e+04  -0.250    0.802
# prev_all_diff_cont               -1.132e-02  9.271e-03  1.376e+04  -1.221    0.222
# all_diff_cont:prev_all_diff_cont  1.603e-02  1.372e-02  1.376e+04   1.168    0.243

AIC(wind4_m2_diffContAll_prevdiffContAll_intxn_rfx) # 6450.738
AIC(wind4_m8_time_diffContAll_prevdiffContAll_intxn_rfx) # 4461.615

# ~ Window 4 Model 9: trial number x current difficulty x previous difficulty x wmc ~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind4_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS +
                                                                    all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                                                    (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind4_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)
# trialnumberRS                                               -2.714e-01  3.551e-02  8.046e+01  -7.644  3.9e-11 ***
# all_diff_cont                                                7.028e-04  9.741e-03  1.344e+04   0.072    0.942
# prev_all_diff_cont                                          -1.022e-02  9.782e-03  1.344e+04  -1.044    0.296
# capacity_HighP1_lowN1_best                                   1.158e-01  7.722e-02  8.025e+01   1.499    0.138
# all_diff_cont:prev_all_diff_cont                             1.456e-02  1.461e-02  1.343e+04   0.997    0.319
# all_diff_cont:capacity_HighP1_lowN1_best                     1.219e-02  9.734e-03  1.346e+04   1.253    0.210
# prev_all_diff_cont:capacity_HighP1_lowN1_best                4.992e-03  9.775e-03  1.346e+04   0.511    0.610
# all_diff_cont:prev_all_diff_cont:capacity_HighP1_lowN1_best -6.375e-03  1.460e-02  1.344e+04  -0.436    0.662

AIC(wind4_m6_diffContAll_prevdiffContAll_wmcCat_intxn_rfx) # 6387.207
AIC(wind4_m9_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx) # 4417.294


# ~ "All" 2-way & 3-way interaction model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wind4_m11_allIntfx_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                                trialnumberRS * all_diff_cont +
                                trialnumberRS * prev_all_diff_cont +
                                trialnumberRS * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m11_allIntfx_rfx)
# trialnumberRS                                               -3.184e-01  1.702e-02  1.348e+04 -18.704   <2e-16 ***
# all_diff_cont                                               -2.098e-02  1.416e-02  1.348e+04  -1.482   0.1384
# prev_all_diff_cont                                          -3.636e-02  1.511e-02  1.348e+04  -2.407   0.0161 *
# capacity_HighP1_lowN1_best                                   9.947e-02  7.992e-02  8.043e+01   1.245   0.2169
# trialnumberRS:all_diff_cont                                  4.921e-02  2.282e-02  1.348e+04   2.157   0.0310 *
# trialnumberRS:prev_all_diff_cont                             6.258e-02  2.431e-02  1.348e+04   2.574   0.0101 *
# trialnumberRS:capacity_HighP1_lowN1_best                     3.163e-02  1.407e-02  1.348e+04   2.248   0.0246 *
# prev_all_diff_cont:capacity_HighP1_lowN1_best                1.964e-02  1.503e-02  1.348e+04   1.306   0.1914
# trialnumberRS:prev_all_diff_cont:capacity_HighP1_lowN1_best -3.032e-02  2.422e-02  1.348e+04  -1.252   0.2108


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# wind1_mX_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind1_predisp_onset_mean ~ 1 + trialnumberRS *
#                                                                     all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
#                                                                     (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
# summary(wind1_mX_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)
#
# wind2_mX_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS *
#                                                                      all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
#                                                                      (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
# summary(wind2_mX_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)
#
wind3_mX_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind3_eval_otciti_mean ~ 1 + trialnumberRS *
                                                                    all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                                                    (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
summary(wind3_mX_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)

# wind4_mX_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + trialnumberRS *
#                                                                     all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
#                                                                     (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
# summary(wind4_mX_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)

# when trial number is not interacted, there are no effects of our other predictors in almost all our models
# when it is interacted, there are some main effects and interaction with trial effects
# McLaughlin's fatigue method will probably be important to do
# Does the interaction effect between trial and difficulty speak to the importance that difficulty does reflect a slower more drawn out timecourse effect?

# In the same models that don't have trial number, there are consistent main effects of current and previous difficulty. In some models they also interact


wind2_mX5_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS *
                                                                    all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                                                    (1 | subjectnumber), data = clean_data_dm)
summary(wind2_mX5_time_diffContAll_prevdiffContAll_wmcCat_intxn_rfx)




wind4_m11_fullintxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                                trialnumberRS * all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best * choice +
                                trialnumberRS * all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best * riskywinP1_loseN1 +
                                (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m11_allIntfx_rfx)


wind4_m11_4way_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                            trialnumberRS * capacity_HighP1_lowN1_best * choice +
                            trialnumberRS * capacity_HighP1_lowN1_best * riskywinP1_loseN1 +
                            all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best * choice +
                            all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best * riskywinP1_loseN1 +
                            prev_all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best * choice +
                            prev_all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best * riskywinP1_loseN1 +
                            (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m11_4way_rfx)

wind4_m11_3way_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                            trialnumberRS * capacity_HighP1_lowN1_best * choice +
                            trialnumberRS * capacity_HighP1_lowN1_best * riskywinP1_loseN1 +
                            all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best +
                            all_diff_cont * trialnumberRS * choice +
                            all_diff_cont * trialnumberRS * riskywinP1_loseN1 +
                            all_diff_cont * capacity_HighP1_lowN1_best * choice +
                            all_diff_cont * capacity_HighP1_lowN1_best * riskywinP1_loseN1 +
                            prev_all_diff_cont * trialnumberRS * capacity_HighP1_lowN1_best +
                            prev_all_diff_cont * trialnumberRS * choice +
                            prev_all_diff_cont * trialnumberRS * riskywinP1_loseN1 +
                            prev_all_diff_cont * capacity_HighP1_lowN1_best * choice +
                            prev_all_diff_cont * capacity_HighP1_lowN1_best * riskywinP1_loseN1 +
                            (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m11_3way_rfx)

wind4_m11_2way_rfx = lmer(wind4_prep_lateiti_mean ~ 1 +
                            trialnumberRS * capacity_HighP1_lowN1_best +
                            trialnumberRS * choice +
                            trialnumberRS * riskywinP1_loseN1 +
                            capacity_HighP1_lowN1_best * choice +
                            capacity_HighP1_lowN1_best * riskywinP1_loseN1 +
                            all_diff_cont * trialnumberRS +
                            all_diff_cont * capacity_HighP1_lowN1_best +
                            all_diff_cont * choice +
                            all_diff_cont * riskywinP1_loseN1 +
                            prev_all_diff_cont * trialnumberRS +
                            prev_all_diff_cont * capacity_HighP1_lowN1_best +
                            prev_all_diff_cont * choice +
                            prev_all_diff_cont * riskywinP1_loseN1 +
                            (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m11_2way_rfx)



anova(wind4_m11_fullintxn_rfx, wind4_m11_4way_rfx, wind4_m11_3way_rfx, wind4_m11_2way_rfx)
# the 2-way-only regression doesn't do significantly worse than the other, more
# complex regressions.

summary(wind4_m11_2way_rfx)
# SIGNIFICANT TERMS
#                                                 Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)                                    4.106e+00  8.031e-02  8.210e+01  51.130  < 2e-16 ***
# trialnumberRS                                 -3.349e-01  1.918e-02  1.347e+04 -17.468  < 2e-16 ***
# choice                                         2.583e-02  1.230e-02  1.347e+04   2.100  0.03579 *
# prev_all_diff_cont                            -4.326e-02  1.566e-02  1.347e+04  -2.763  0.00573 **
# trialnumberRS:all_diff_cont                    4.877e-02  2.286e-02  1.347e+04   2.133  0.03291 *
# choice:all_diff_cont                          -2.562e-02  1.256e-02  1.347e+04  -2.039  0.04148 *
# trialnumberRS:prev_all_diff_cont               7.502e-02  2.284e-02  1.347e+04   3.285  0.00102 **

# TREND
# trialnumberRS:capacity_HighP1_lowN1_best       1.641e-02  9.624e-03  1.347e+04   1.705  0.08820 .

# NON-SIGNIFICANCE
# capacity_HighP1_lowN1_best                     1.010e-01  7.980e-02  8.005e+01   1.265  0.20938
# riskywinP1_loseN1                              1.365e-02  8.522e-03  1.347e+04   1.601  0.10933
# all_diff_cont                                 -2.843e-03  1.591e-02  1.347e+04  -0.179  0.85816
# trialnumberRS:choice                           2.288e-02  1.797e-02  1.347e+04   1.273  0.20300
# trialnumberRS:riskywinP1_loseN1                1.197e-04  1.258e-02  1.347e+04   0.010  0.99241
# capacity_HighP1_lowN1_best:choice              3.838e-03  5.484e-03  1.347e+04   0.700  0.48411
# capacity_HighP1_lowN1_best:riskywinP1_loseN1   2.445e-05  3.809e-03  1.347e+04   0.006  0.99488
# capacity_HighP1_lowN1_best:all_diff_cont       9.578e-03  6.587e-03  1.347e+04   1.454  0.14595
# riskywinP1_loseN1:all_diff_cont               -1.219e-02  8.569e-03  1.347e+04  -1.423  0.15481
# capacity_HighP1_lowN1_best:prev_all_diff_cont  2.398e-03  6.587e-03  1.347e+04   0.364  0.71586
# choice:prev_all_diff_cont                     -9.714e-04  1.233e-02  1.347e+04  -0.079  0.93719
# riskywinP1_loseN1:prev_all_diff_cont          -7.911e-03  8.739e-03  1.347e+04  -0.905  0.36531


# In WINDOW 4 (late ITI), we find that the pupil...
# - constricts with time
#   - ... and might constrict LESS for high-capacity folks/MORE for low-capacity folks (like window 2! but trend)
# - constricts more the more difficult the previous trial was, EARLY in the study,
#   but dilates more the more difficult the previous trial was, LATE in the study.
# - dilates after easy risky choices (and does so more with time) (vs. safe)
# - dilates after diff risky choices ONLY late in study
#
# NO EFFECT of risky outcome (!) as ME or interaction with anything, and nearly no
# effect of capacity either.

# TAKEAWAY:
# Window 4 pupil...
# - exhibits similar global patterns as window 2
# - is responsive to prev. difficulty in a way that flips (NEG -> POS) over the study
# - dilates after risky choices, with an initial NEG. modulation by curr. difficulty
#   that is attenuated with time.


adc = 1 # current difficulty (All Diff Cont)
chc = 1 # choice
trialn = 1 # trial number

chc * 2.583e-02 + chc * trialn * 4.877e-02 + chc * adc * -2.562e-02

# safe -> 0 (NO EFFECT OF DIFFICULTY OR TIME)
# risky, easy, early -> 0.03
# risky, diff, early -> 0.0002
# risky, easy, late -> 0.07
# risky, diff, late -> 0.05
#
# Early on, EASY >>> difficult; later on, easy > diff, but both are greater than early


wind4_m11_2way_rfx_noriskyotc = lmer(wind4_prep_lateiti_mean ~ 1 +
                            trialnumberRS * capacity_HighP1_lowN1_best +
                            trialnumberRS * choice +
                            capacity_HighP1_lowN1_best * choice +
                            all_diff_cont * trialnumberRS +
                            all_diff_cont * capacity_HighP1_lowN1_best +
                            all_diff_cont * choice +
                            prev_all_diff_cont * trialnumberRS +
                            prev_all_diff_cont * capacity_HighP1_lowN1_best +
                            prev_all_diff_cont * choice +
                            (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m11_2way_rfx_noriskyotc)

anova(wind4_m11_2way_rfx_noriskyotc, wind4_m11_2way_rfx)
# NO RISKY-OUTCOME TERM IS NOT SIG. WORSE
# Reg. w/o risky outcome term finds *same* pattern of sig. regressors



wind4_m11_2way_rfx_noriskyotc_diffintxn = lmer(wind4_prep_lateiti_mean ~ 1 +
                                       trialnumberRS * capacity_HighP1_lowN1_best +
                                       trialnumberRS * choice +
                                       capacity_HighP1_lowN1_best * choice +
                                       all_diff_cont * trialnumberRS +
                                       all_diff_cont * capacity_HighP1_lowN1_best +
                                       all_diff_cont * choice +
                                       prev_all_diff_cont * trialnumberRS +
                                       prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                       prev_all_diff_cont * choice +
                                       all_diff_cont * prev_all_diff_cont +
                                       (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m11_2way_rfx_noriskyotc_diffintxn) # nope
# the interaction of curr. & prev. diff is not sig.




wind4_m5_diffContAll_prevdiffContAll_wmcCont_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont * prev_all_diff_cont * choice +
                                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m5_diffContAll_prevdiffContAll_wmcCont_intxn_rfx)

wind4_m5_diffContAll_prevdiffContAll_wmcCont_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + all_diff_cont * choice + prev_all_diff_cont * choice +
                                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m5_diffContAll_prevdiffContAll_wmcCont_intxn_rfx)


wind4_m5_diffContAll_prevdiffContAll_wmcCont_intxn_rfx = lmer(wind4_prep_lateiti_mean ~ 1 + easyP1difficultN1 * easyP1difficultN1_prev * choice +
                                                                (1 | subjectnumber), data = clean_data_dm)
summary(wind4_m5_diffContAll_prevdiffContAll_wmcCont_intxn_rfx)




### LOOP: Regression Loop and Plotting a Predictor across all Pupillometry Windows #####

dev.off()

par(mfrow = c(1,1))
pupilWinds = c("wind1_predisp_onset_mean", "wind2_effort_isi_mean",
               "wind3_eval_otciti_mean", "wind4_prep_lateiti_mean") # All the pupillometry windows for analysis

columns = c("outcome", "predictor", "beta", "pval")

# current difficulty categorical ================================================ # ALL CATEGORICAL
mX_df = data.frame(matrix(NA, nrow = 1 * length(pupilWinds), ncol = 4))
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + easyP1difficultN1 + (1 | subjectnumber)"))
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 1 * (i - 1) + 1

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5])

}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}

# current difficulty categorical x previous difficulty categorical
mX_df = data.frame(matrix(NA, nrow = 3 * length(pupilWinds), ncol = 4))
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + easyP1difficultN1 * easyP1difficultN1_prev + (1 | subjectnumber)"))
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 3 * (i - 1) + 1

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5])
  mX_df[row_index+1,] = c(outcome, rownames(mX_sum$coefficients)[3], mX_sum$coefficients[3,1], mX_sum$coefficients[3,5])
  mX_df[row_index+2,] = c(outcome, rownames(mX_sum$coefficients)[4], mX_sum$coefficients[4,1], mX_sum$coefficients[4,5])

}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}

# current difficulty categorical x previous difficulty categorical x wmcc categorical
mX_df = data.frame(matrix(NA, nrow = 7 * length(pupilWinds), ncol = 4)) # nrow needs to change based on how much saving out
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + easyP1difficultN1 * easyP1difficultN1_prev * capacity_HighP1_lowN1_best + (1 | subjectnumber)")) # add or remove predictors as needed
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 7 * (i - 1) + 1 # increase like nrows # could I make this more flexible if used length(rownames(mX_sum))

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5]) # increase like nrows
  mX_df[row_index+1,] = c(outcome, rownames(mX_sum$coefficients)[3], mX_sum$coefficients[3,1], mX_sum$coefficients[3,5])
  mX_df[row_index+2,] = c(outcome, rownames(mX_sum$coefficients)[4], mX_sum$coefficients[4,1], mX_sum$coefficients[4,5])
  mX_df[row_index+3,] = c(outcome, rownames(mX_sum$coefficients)[5], mX_sum$coefficients[5,1], mX_sum$coefficients[5,5])
  mX_df[row_index+4,] = c(outcome, rownames(mX_sum$coefficients)[6], mX_sum$coefficients[6,1], mX_sum$coefficients[6,5])
  mX_df[row_index+5,] = c(outcome, rownames(mX_sum$coefficients)[7], mX_sum$coefficients[7,1], mX_sum$coefficients[7,5])
  mX_df[row_index+6,] = c(outcome, rownames(mX_sum$coefficients)[8], mX_sum$coefficients[8,1], mX_sum$coefficients[8,5])

}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}

# current difficulty continuous ================================================ TRIALS NOT INCLUDED
mX_df = data.frame(matrix(NA, nrow = 1 * length(pupilWinds), ncol = 4))
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + all_diff_cont + (1 | subjectnumber)"))
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 1 * (i - 1) + 1

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5])

}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}

# current difficulty continuous x previous difficulty continuous
mX_df = data.frame(matrix(NA, nrow = 3 * length(pupilWinds), ncol = 4))
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + all_diff_cont * prev_all_diff_cont + (1 | subjectnumber)"))
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 3 * (i - 1) + 1

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5])
  mX_df[row_index+1,] = c(outcome, rownames(mX_sum$coefficients)[3], mX_sum$coefficients[3,1], mX_sum$coefficients[3,5])
  mX_df[row_index+2,] = c(outcome, rownames(mX_sum$coefficients)[4], mX_sum$coefficients[4,1], mX_sum$coefficients[4,5])

}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}

# current difficulty continuous x previous difficulty continuous x wmcc categorical
mX_df = data.frame(matrix(NA, nrow = 7 * length(pupilWinds), ncol = 4)) # nrow needs to change based on how much saving out
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 | subjectnumber)")) # add or remove predictors as needed
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 7 * (i - 1) + 1 # increase like nrows # could I make this more flexible if used length(rownames(mX_sum))

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5]) # increase like nrows
  mX_df[row_index+1,] = c(outcome, rownames(mX_sum$coefficients)[3], mX_sum$coefficients[3,1], mX_sum$coefficients[3,5])
  mX_df[row_index+2,] = c(outcome, rownames(mX_sum$coefficients)[4], mX_sum$coefficients[4,1], mX_sum$coefficients[4,5])
  mX_df[row_index+3,] = c(outcome, rownames(mX_sum$coefficients)[5], mX_sum$coefficients[5,1], mX_sum$coefficients[5,5])
  mX_df[row_index+4,] = c(outcome, rownames(mX_sum$coefficients)[6], mX_sum$coefficients[6,1], mX_sum$coefficients[6,5])
  mX_df[row_index+5,] = c(outcome, rownames(mX_sum$coefficients)[7], mX_sum$coefficients[7,1], mX_sum$coefficients[7,5])
  mX_df[row_index+6,] = c(outcome, rownames(mX_sum$coefficients)[8], mX_sum$coefficients[8,1], mX_sum$coefficients[8,5])

}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}

# current difficulty continuous ================================================ MAIN EFFECTS OF TRIALS
mX_df = data.frame(matrix(NA, nrow = 2 * length(pupilWinds), ncol = 4))
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + trialnumberRS + all_diff_cont + (1 + trialnumberRS | subjectnumber)"))
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 2 * (i - 1) + 1

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5])
  mX_df[row_index+1,] = c(outcome, rownames(mX_sum$coefficients)[3], mX_sum$coefficients[3,1], mX_sum$coefficients[3,5])

}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}

# current difficulty continuous x previous difficulty continuous
mX_df = data.frame(matrix(NA, nrow = 4 * length(pupilWinds), ncol = 4))
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + trialnumberRS + all_diff_cont * prev_all_diff_cont + (1 + trialnumberRS | subjectnumber)"))
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 4 * (i - 1) + 1

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5])
  mX_df[row_index+1,] = c(outcome, rownames(mX_sum$coefficients)[3], mX_sum$coefficients[3,1], mX_sum$coefficients[3,5])
  mX_df[row_index+2,] = c(outcome, rownames(mX_sum$coefficients)[4], mX_sum$coefficients[4,1], mX_sum$coefficients[4,5])
  mX_df[row_index+3,] = c(outcome, rownames(mX_sum$coefficients)[5], mX_sum$coefficients[5,1], mX_sum$coefficients[5,5])

}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}

# current difficulty continuous x previous difficulty continuous x wmcc categorical
mX_df = data.frame(matrix(NA, nrow = 8 * length(pupilWinds), ncol = 4)) # nrow needs to change based on how much saving out
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + trialnumberRS + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 + trialnumberRS | subjectnumber)")) # add or remove predictors as needed
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 8 * (i - 1) + 1 # increase like nrows # could I make this more flexible if used length(rownames(mX_sum))

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5]) # increase like nrows
  mX_df[row_index+1,] = c(outcome, rownames(mX_sum$coefficients)[3], mX_sum$coefficients[3,1], mX_sum$coefficients[3,5])
  mX_df[row_index+2,] = c(outcome, rownames(mX_sum$coefficients)[4], mX_sum$coefficients[4,1], mX_sum$coefficients[4,5])
  mX_df[row_index+3,] = c(outcome, rownames(mX_sum$coefficients)[5], mX_sum$coefficients[5,1], mX_sum$coefficients[5,5])
  mX_df[row_index+4,] = c(outcome, rownames(mX_sum$coefficients)[6], mX_sum$coefficients[6,1], mX_sum$coefficients[6,5])
  mX_df[row_index+5,] = c(outcome, rownames(mX_sum$coefficients)[7], mX_sum$coefficients[7,1], mX_sum$coefficients[7,5])
  mX_df[row_index+6,] = c(outcome, rownames(mX_sum$coefficients)[8], mX_sum$coefficients[8,1], mX_sum$coefficients[8,5])
  mX_df[row_index+7,] = c(outcome, rownames(mX_sum$coefficients)[9], mX_sum$coefficients[9,1], mX_sum$coefficients[9,5])

}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}

# current difficulty continuous ================================================ INTERACTION EFFECTS W/ TRIALS
mX_df = data.frame(matrix(NA, nrow = 3 * length(pupilWinds), ncol = 4))
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + trialnumberRS * all_diff_cont + (1 + trialnumberRS | subjectnumber)"))
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 3 * (i - 1) + 1

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5])
  mX_df[row_index+1,] = c(outcome, rownames(mX_sum$coefficients)[3], mX_sum$coefficients[3,1], mX_sum$coefficients[3,5])
  mX_df[row_index+2,] = c(outcome, rownames(mX_sum$coefficients)[4], mX_sum$coefficients[4,1], mX_sum$coefficients[4,5])

}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}

# current difficulty continuous x previous difficulty continuous
mX_df = data.frame(matrix(NA, nrow = 7 * length(pupilWinds), ncol = 4))
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + trialnumberRS * all_diff_cont * prev_all_diff_cont + (1 + trialnumberRS | subjectnumber)"))
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 7 * (i - 1) + 1

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5])
  mX_df[row_index+1,] = c(outcome, rownames(mX_sum$coefficients)[3], mX_sum$coefficients[3,1], mX_sum$coefficients[3,5])
  mX_df[row_index+2,] = c(outcome, rownames(mX_sum$coefficients)[4], mX_sum$coefficients[4,1], mX_sum$coefficients[4,5])
  mX_df[row_index+3,] = c(outcome, rownames(mX_sum$coefficients)[5], mX_sum$coefficients[5,1], mX_sum$coefficients[5,5])
  mX_df[row_index+4,] = c(outcome, rownames(mX_sum$coefficients)[6], mX_sum$coefficients[6,1], mX_sum$coefficients[6,5])
  mX_df[row_index+5,] = c(outcome, rownames(mX_sum$coefficients)[7], mX_sum$coefficients[7,1], mX_sum$coefficients[7,5])
  mX_df[row_index+6,] = c(outcome, rownames(mX_sum$coefficients)[8], mX_sum$coefficients[8,1], mX_sum$coefficients[8,5])


}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}

# current difficulty continuous x previous difficulty continuous x wmcc categorical
mX_df = data.frame(matrix(NA, nrow = 15 * length(pupilWinds), ncol = 4)) # nrow needs to change based on how much saving out
colnames(mX_df) = columns

mX_allSum = list()

for (i in 1:length(pupilWinds)) {

  outcome = pupilWinds[i]

  # create the regression model
  model = as.formula(paste(outcome, "~ 1 + trialnumberRS * all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best + (1 + trialnumberRS | subjectnumber)")) # add or remove predictors as needed
  mX = lmer(model, data = clean_data_dm)

  # save the model summaries
  mX_sum = summary(mX)
  mX_allSum[[outcome]] = mX_sum

  # save out the model summaries into the data frame
  row_index = 15 * (i - 1) + 1 # increase like nrows # could I make this more flexible if used length(rownames(mX_sum))

  mX_df[row_index,] = c(outcome, rownames(mX_sum$coefficients)[2], mX_sum$coefficients[2,1], mX_sum$coefficients[2,5]) # increase like nrows
  mX_df[row_index+1,] = c(outcome, rownames(mX_sum$coefficients)[3], mX_sum$coefficients[3,1], mX_sum$coefficients[3,5])
  mX_df[row_index+2,] = c(outcome, rownames(mX_sum$coefficients)[4], mX_sum$coefficients[4,1], mX_sum$coefficients[4,5])
  mX_df[row_index+3,] = c(outcome, rownames(mX_sum$coefficients)[5], mX_sum$coefficients[5,1], mX_sum$coefficients[5,5])
  mX_df[row_index+4,] = c(outcome, rownames(mX_sum$coefficients)[6], mX_sum$coefficients[6,1], mX_sum$coefficients[6,5])
  mX_df[row_index+5,] = c(outcome, rownames(mX_sum$coefficients)[7], mX_sum$coefficients[7,1], mX_sum$coefficients[7,5])
  mX_df[row_index+6,] = c(outcome, rownames(mX_sum$coefficients)[8], mX_sum$coefficients[8,1], mX_sum$coefficients[8,5])
  mX_df[row_index+7,] = c(outcome, rownames(mX_sum$coefficients)[9], mX_sum$coefficients[9,1], mX_sum$coefficients[9,5])
  mX_df[row_index+8,] = c(outcome, rownames(mX_sum$coefficients)[10], mX_sum$coefficients[10,1], mX_sum$coefficients[10,5])
  mX_df[row_index+9,] = c(outcome, rownames(mX_sum$coefficients)[11], mX_sum$coefficients[11,1], mX_sum$coefficients[11,5])
  mX_df[row_index+10,] = c(outcome, rownames(mX_sum$coefficients)[12], mX_sum$coefficients[12,1], mX_sum$coefficients[12,5])
  mX_df[row_index+11,] = c(outcome, rownames(mX_sum$coefficients)[13], mX_sum$coefficients[13,1], mX_sum$coefficients[13,5])
  mX_df[row_index+12,] = c(outcome, rownames(mX_sum$coefficients)[14], mX_sum$coefficients[14,1], mX_sum$coefficients[14,5])
  mX_df[row_index+13,] = c(outcome, rownames(mX_sum$coefficients)[15], mX_sum$coefficients[15,1], mX_sum$coefficients[15,5])
  mX_df[row_index+14,] = c(outcome, rownames(mX_sum$coefficients)[16], mX_sum$coefficients[16,1], mX_sum$coefficients[16,5])

}

predictors = c(unique(mX_df$predictor))

for (i in predictors) {

  # set up plot parameters
  betas = as.numeric(mX_df[mX_df$predictor == i, "beta"])
  pvals = as.numeric(mX_df[mX_df$predictor == i, "pval"])
  otcs = mX_df[mX_df$predictor == i, "outcome"]

  # plotting single predictor across all pupillometry models
  plot(betas, ylim = range(betas, na.rm = T), xaxt = "n",
       type = "o", pch = 16, cex = 2, # added cex cause it kept making the circles into a diamond
       xlab = "Pupillometry Windows", ylab = "Beta Values",
       main = paste(i, "Betas across Pupillometry Windows"))
  axis(1, at = 1:length(otcs), labels = otcs)

  # marking each significant beta
  pval_indices <- which(pvals < 0.05)
  points(pval_indices, betas[pval_indices], pch = 16, cex = 3, col = "red")

}


### RFX for Trial Number? ###################################################

wind2_m10_T_CD_PD_intfx_noTrialRFX = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS +
                                            all_diff_cont * prev_all_diff_cont +
                                            (1 | subjectnumber), data = clean_data_dm)
w2_m10_noTrialRFX_sum = summary(wind2_m10_T_CD_PD_intfx_noTrialRFX)
coef(w2_m10_noTrialRFX_sum)
# coef(wind2_m10_T_CD_PD_intfx_noTrialRFX)

wind2_m10_T_CD_PD_intfx_TrialRFX = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS +
                                            all_diff_cont * prev_all_diff_cont +
                                            (1 + trialnumberRS | subjectnumber), data = clean_data_dm)
w2_m10_TrialRFX_sum = summary(wind2_m10_T_CD_PD_intfx_TrialRFX)
coef(w2_m10_TrialRFX_sum)
tmp_coefficients = coef(wind2_m10_T_CD_PD_intfx_TrialRFX);
mfx_estimates_trialnumberRS = tmp_coefficients$subjectnumber$trialnumberRS

# Does this basic regression do better or worse with the random effects of trial
# number (standing in for time-on-task)?
anova(wind2_m10_T_CD_PD_intfx_noTrialRFX, wind2_m10_T_CD_PD_intfx_TrialRFX)
# It does better *WITH* RFX for trialnumberRS (time-on-task).


# cor_matrix = cor(clean_data_dm[,c('trialnumberRS','complexspan_demeaned','NCS', 'IUS', 'PSS', 'SNS')],
#                  use = 'complete.obs')
cor_matrix = cor(cbind(mfx_estimates_trialnumberRS,
                       clean_data_complexspan$compositeSpanScore[keep_subj_pupil],
                       clean_data_survey$NCS[keep_subj_pupil],
                       clean_data_survey$IUS[keep_subj_pupil],
                       clean_data_survey$PSS[keep_subj_pupil],
                       clean_data_survey$SNS[keep_subj_pupil]
                       ),
                 use = 'complete.obs')
corrplot(cor_matrix, type = 'lower')

# There is no consistent relationship between the effect of time on pupil dilation
# and individual differences measures including composite span, NCS, IUS, PSS,
# or SNS. Double-checked with Spearman's in a few cases, and visually checked all
# pairwise correlations, no concerns re: distributions or outliers.

# TAKEAWAY: Effect of time-on-task on pupil dilation is its own effect.

# Overall, it seems like the random effects of trial number on pupil dilation are
# primary, unique effects, and meaningful in terms of fitting pupil dilation
# data. This RFX might need to stay in our regressions!








### Thesis Graphs ####

dev.off()

# T-tests: RTs
par(mfrow = c(1,2))

choiceDifficultyGraph = barplot(means, beside = T, col = c("blue", "red"),
                                ylim = c(0, 2), names.arg = c("Easy", "Difficult"),
                                ylab = "Average RTs in Seconds", main = "A")

arrows(choiceDifficultyGraph, means + ses,
       choiceDifficultyGraph, means - ses,
       angle = 90, code = 3, length = 0.1)
box(bty="l")

WMCgraph = barplot(means_matrix, beside = TRUE, col = c("blue", "red"),
                   ylim = c(0, 2), names.arg = colnames(means_matrix),
                   ylab = "Average RTs in Seconds", main = "B",
                   legend.text = rownames(means_matrix),
                   args.legend = list(x = "topright"))

arrows(WMCgraph, means_matrix + ses,
       WMCgraph, means_matrix - ses,
       angle = 90, code = 3, length = 0.1)
box(bty="l")



# Regressions
par(mfrow = c(1,1))
# RTs

plot(1, type = 'n',
     xlab = "Milliseconds", ylab = "Demeaned Pupil Diameter (mm)",
     main = "Aligned to Choice",
     xlim = c(-pre_dec_window_width, dec_isi_otc_iti_window_width), ylim = c(min(dec_isi_otc_iti_lower),max(dec_isi_otc_iti_upper)))
polygon(x = c(1000, 2000, 2000, 1000),
        y = c(3, 3, -3, -3),
        lty = 0, col = rgb(0,0,0,.1))
text(x = mean(c(0, 1000)), y = min(dec_isi_otc_iti_lower),
     labels = "ISI")
text(x = mean(c(1000, 2000)), y = min(dec_isi_otc_iti_lower),
     labels = "Outcome")
text(x = mean(c(2000, 5000)), y = min(dec_isi_otc_iti_lower),
     labels = "ITI")
polygon(x = sem_dec_isi_otc_iti_x_vals,
        y = c(dec_isi_otc_iti_upper,rev(dec_isi_otc_iti_lower)),
        lty = 0, col = rgb(0,0,0,.2))
lines(x = dec_isi_otc_iti_bins[1:(length(dec_isi_otc_iti_bins)-1)] + bin_increment/2,
      y = rowMeans(mean_dec_isi_otc_iti_array, na.rm = T), type = 'l',
      lwd = 3, col = 'black')
abline(v = 0, lty = 'dashed')

# ~ low WMC
# mean_HE = mean((clean_data_dm$all_diff_cont[clean_data_dm$choice == 0]), na.rm = T);
# sd_HE = sd((meanRT_easy_capacity_High), na.rm = T);
# mean_HD = mean((meanRT_diff_capacity_High), na.rm = T);
# sd_HD = sd((meanRT_diff_capacity_High), na.rm = T);
#
# mean_LE = mean((meanRT_easy_capacity_Low), na.rm = T);
# sd_LE = sd((meanRT_easy_capacity_Low), na.rm = T);
# mean_LD = mean((meanRT_diff_capacity_Low), na.rm = T);
# sd_LD =  sd((meanRT_diff_capacity_Low), na.rm = T);
#
# se_HE = sd_HE/sqrt(length(meanRT_easy_capacity_High))
# se_HD = sd_HD/sqrt(length(meanRT_diff_capacity_High))
# se_LE = sd_LE/sqrt(length(meanRT_easy_capacity_Low))
# se_LD = sd_LD/sqrt(length(meanRT_diff_capacity_Low))
# means = c(mean_LE, mean_HE, mean_LD, mean_HD)
# ses = c(se_LE, se_HE, se_LD, se_HD)
#
# means_matrix = matrix(means, nrow=2, byrow=TRUE)
# rownames(means_matrix) = c("Easy", "Difficult")
# colnames(means_matrix) = c("Low WMC", "High WMC")
#
# WMCgraph = barplot(means_matrix, beside = TRUE, col = c("blue", "red"),
#                    ylim = c(0, 2.5), names.arg = colnames(means_matrix),
#                    ylab = "Average RTs in Seconds",
#                    main = "B")
#
# arrows(WMCgraph, means_matrix,
#        WMCgraph, means_matrix + ses,
#        angle = 90, code = 3, length = 0.1)
# box(bty="l")


xval_plot = seq(from = 0, to = 1, length.out = 10)
predict_data_m3_best_H = clean_data_dm[0,];
predict_data_m3_best_H[1:20,] = NA;
predict_data_m3_best_H$all_diff_cont[1:10] = xval_plot
predict_data_m3_best_H$all_diff_cont[11:20] = xval_plot
predict_data_m3_best_H$prev_all_diff_cont[1:10] = 0;
predict_data_m3_best_H$prev_all_diff_cont[11:20] = 1;
predict_data_m3_best_H$capacity_HighP1_lowN1_best = 1;

predict_data_m3_best_L = predict_data_m3_best_H;
predict_data_m3_best_L$capacity_HighP1_lowN1_best = -1;

predict_output_m3_best_H = predict(m3_best, newdata = predict_data_m3_best_H, type = 'response', re.form = NA)^2
predict_output_m3_best_L = predict(m3_best, newdata = predict_data_m3_best_L, type = 'response', re.form = NA)^2

par(mfrow = c(1,2))
#SEPERATE INTO TWO PLOTS (LOW CAPACITY BELOW)
# Third plot PREV easy & CAPACITY low
plot(x = xval_plot, y = predict_output_m3_best_L[1:10],
     type = 'l',lwd = 5, col = 'blue',
     main = 'A: Low WMC', xlab = 'Current difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)',
     ylim = c(1.25,1.85))
# Fourth (last) plot PREV diff & CAPACITY low
lines(x = xval_plot, y = predict_output_m3_best_L[11:20],
      lwd = 5, col = 'red')
legend("bottomright", legend = c("Previous Easy", "Previous Difficult"),
       col = c('blue', 'red'), lty = c(1, 1))

#HIGH CAPACITY PLOT
# First plot PREV easy & CAPACITY high
plot(x = xval_plot, y = predict_output_m3_best_H[1:10],
     type = 'l', lwd = 5, col = 'blue',
     main = 'B: High WMC', xlab = 'Current difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)',
     ylim = c(1.25, 1.85))
# Second plot PREV diff & CAPACITY high
lines(x = xval_plot, y = predict_output_m3_best_H[11:20],
      lwd = 5, col = 'red')
legend("bottomright", legend = c("Previous Easy", "Previous Difficult"),
       col = c('blue', 'red'), lty = c(1, 1))




wind2_m11_sepdifficulties_3ways_rfx_NCS

# xval_plot = seq(from = 0, to = 1, by = .1);
# coef_vals = fixef(m0_alldiffcont_rfx)
#
# plot(x = xval_plot, y = (coef_vals["(Intercept)"] + xval_plot*coef_vals["all_diff_cont"])^2,
#      type = 'l', lwd = 5, col = 'purple',
#      main = 'Effect of current difficulty', xlab = 'Difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)')
#
# # Plot it!
# xval_plot = seq(from = 0, to = 1, by = .1); # current difficulty (easy = 0, difficult = 1)
# prev_trial_diff = c(1,2); # easy = 0, difficult = 1
# capacity = c(1, -1); # HIGH = 1, low = -1
# coef_vals = fixef(m3_prev_diffCont_capacityCat_intxn_rfx)
#
# #HIGH CAPACITY PLOT
# # First plot PREV easy & CAPACITY high
# plot(x = xval_plot, y = (coef_vals["(Intercept)"] +
#                            xval_plot*coef_vals["all_diff_cont"] +
#                            prev_trial_diff[1]*coef_vals["prev_all_diff_cont"] +
#                            xval_plot*capacity[1]* coef_vals["all_diff_cont:capacity_HighP1_lowN1"] +
#                            prev_trial_diff[1]*capacity[1]*coef_vals["prev_all_diff_cont:capacity_HighP1_lowN1"])^2,
#      type = 'l', lwd = 5, col = 'blue',
#      main = 'Effect of current & previous difficulty', xlab = 'Current difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)',
#      ylim = c(1.25, 1.575))
# # Second plot PREV diff & CAPACITY high
# lines(x = xval_plot, y = (coef_vals["(Intercept)"] +
#                             xval_plot*coef_vals["all_diff_cont"] +
#                             prev_trial_diff[2]*coef_vals["prev_all_diff_cont"] +
#                             xval_plot*capacity[1]* coef_vals["all_diff_cont:capacity_HighP1_lowN1"] +
#                             prev_trial_diff[2]*capacity[1]*coef_vals["prev_all_diff_cont:capacity_HighP1_lowN1"])^2,
#       lwd = 5, col = 'red')


par(mfrow = c(1,2))

xval_plot = seq(from = 0, to = 1, by = .1)
cd = c(0,1)
pd = c(0,1)
choice = c(0,1)
wmc = c(-1, 1)
coef_vals = fixef(wind2_m11_sepdifficulties_3ways_rfx_NCS)
# low WMC pupil
# CD x Choice
plot(x = xval_plot, y = coef_vals["(Intercept)"] +
       xval_plot*coef_vals["all_diff_cont"] +
       choice[1]*coef_vals["choice"] +
       xval_plot*wmc[1]*coef_vals["capacity_HighP1_lowN1_best:all_diff_cont"] +
       choice[1]*wmc[1]*coef_vals["choice:all_diff_cont"],
     type = 'l', lwd = 5, col = 'green',
     main = 'A: Low WMC x Current Difficulty x Choice',
     xlab = 'Current difficulty (0 = easy, 1 = difficult)', ylab = 'Reaction Time (seconds)')
lines(x = xval_plot, y = coef_vals["(Intercept)"] +
        xval_plot*coef_vals["all_diff_cont"] +
        choice[2]*coef_vals["choice"] +
        xval_plot*wmc[1]*coef_vals["capacity_HighP1_lowN1_best:all_diff_cont"] +
        choice[2]*wmc[1]*coef_vals["choice:all_diff_cont"],
      type = 'l', lwd = 5, col = 'orange')





#                                                               Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)                                                  4.120e+00  8.124e-02  8.270e+01  50.716  < 2e-16 ***
# trialnumberRS                                               -2.452e-01  1.892e-02  1.331e+04 -12.962  < 2e-16 ***
# capacity_HighP1_lowN1_best                                   1.214e-01  8.085e-02  8.195e+01   1.502 0.136931
# choice                                                       8.183e-02  1.455e-02  1.331e+04   5.624 1.90e-08 ***
# NCS_HighP1_LowN1                                            -3.253e-02  7.562e-02  8.052e+01  -0.430 0.668170
# all_diff_cont                                               -1.297e-02  1.662e-02  1.331e+04  -0.780 0.435119
# prev_all_diff_cont                                           2.779e-03  1.619e-02  1.331e+04   0.172 0.863683

# trialnumberRS:capacity_HighP1_lowN1_best                     3.376e-02  1.543e-02  1.331e+04   2.187 0.028742 *
# trialnumberRS:choice                                        -1.186e-01  2.610e-02  1.331e+04  -4.545 5.55e-06 ***
# capacity_HighP1_lowN1_best:choice                           -1.165e-02  1.001e-02  1.331e+04  -1.164 0.244406
# trialnumberRS:NCS_HighP1_LowN1                              -2.666e-02  6.967e-03  1.331e+04  -3.827 0.000131 ***
# trialnumberRS:all_diff_cont                                 -2.474e-02  2.645e-02  1.331e+04  -0.935 0.349784

# capacity_HighP1_lowN1_best:all_diff_cont                     1.494e-02  1.294e-02  1.331e+04   1.154 0.248323
# choice:all_diff_cont                                        -6.596e-02  2.223e-02  1.331e+04  -2.967 0.003015 **
# NCS_HighP1_LowN1:all_diff_cont                              -5.003e-03  4.869e-03  1.331e+04  -1.028 0.304168
# trialnumberRS:prev_all_diff_cont                            -1.687e-02  2.589e-02  1.331e+04  -0.652 0.514713
# capacity_HighP1_lowN1_best:prev_all_diff_cont                1.587e-02  1.276e-02  1.331e+04   1.243 0.213743
# choice:prev_all_diff_cont                                   -2.704e-02  2.212e-02  1.331e+04  -1.222 0.221552
# NCS_HighP1_LowN1:prev_all_diff_cont                          2.030e-03  4.872e-03  1.331e+04   0.417 0.676996

# trialnumberRS:capacity_HighP1_lowN1_best:choice             -2.130e-03  1.509e-02  1.331e+04  -0.141 0.887747
# trialnumberRS:capacity_HighP1_lowN1_best:all_diff_cont      -2.329e-02  1.883e-02  1.331e+04  -1.237 0.216158
# trialnumberRS:choice:all_diff_cont                           1.336e-01  3.557e-02  1.331e+04   3.757 0.000173 ***
# capacity_HighP1_lowN1_best:choice:all_diff_cont              6.608e-03  1.033e-02  1.331e+04   0.640 0.522462
# trialnumberRS:capacity_HighP1_lowN1_best:prev_all_diff_cont -3.858e-02  1.883e-02  1.331e+04  -2.049 0.040509 *
# trialnumberRS:choice:prev_all_diff_cont                      6.139e-02  3.542e-02  1.331e+04   1.733 0.083137 .
# capacity_HighP1_lowN1_best:choice:prev_all_diff_cont         2.337e-02  1.011e-02  1.331e+04   2.312 0.020791 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1















##### thesis: single predictor models #####

wind2_m01_trial = lmer(wind2_effort_isi_mean ~ 1 + trialnumberRS  +
                                                 (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$NCS_HighP1_LowN1),], REML = F)
summary(wind2_m01_trial)
# trialnumberRS -2.986e-01  6.610e-03  1.378e+04  -45.17   <2e-16 ***

wind2_m01_wmc = lmer(wind2_effort_isi_mean ~ 1 + capacity_HighP1_lowN1_best +
                       (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$NCS_HighP1_LowN1),], REML = F)
summary(wind2_m01_wmc)
# capacity_HighP1_lowN1_best  0.13546    0.08051 80.00013   1.683   0.0963 .

wind2_m01_choice = lmer(wind2_effort_isi_mean ~ 1 + choice +
                          (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$NCS_HighP1_LowN1),], REML = F)
summary(wind2_m01_choice)
# choice      3.135e-02  4.175e-03 1.378e+04   7.509 6.34e-14 ***

wind2_m01_nfc = lmer(wind2_effort_isi_mean ~ 1 + NCS_HighP1_LowN1 +
                       (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$NCS_HighP1_LowN1),], REML = F)
summary(wind2_m01_nfc)
# NCS_HighP1_LowN1 -0.02867    0.07560 82.00004  -0.379    0.706

wind2_m01_cd = lmer(wind2_effort_isi_mean ~ 1 + all_diff_cont +
                      (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$NCS_HighP1_LowN1),], REML = F)
summary(wind2_m01_cd)
# all_diff_cont -6.233e-02  5.029e-03  1.378e+04  -12.39   <2e-16 ***

wind2_m01_pd = lmer(wind2_effort_isi_mean ~ 1 + prev_all_diff_cont +
                      (1 | subjectnumber), data = clean_data_dm[is.finite(clean_data_dm$NCS_HighP1_LowN1),], REML = F)
summary(wind2_m01_pd)
# prev_all_diff_cont -4.156e-02  5.037e-03  1.364e+04  -8.251   <2e-16 ***






















# IGNORE # MOVE ON #############



# Proposed RT regression models
# Outcomes: Average RTs, Trial-by-Trial RTs, and SD RTs
# Predictors: current difficulty (cat v. cont), previous difficulty (cont v. cont), wmc, and nfc





# Mean RT Loop
for (subj in 1:number_of_clean_subjects) {
  subj_id = keep_participants[subj]

  tmpdata = clean_data_dm[clean_data_dm$subjectnumber == subj_id, ]

  mean_RT = mean(tmpdata$reactiontime, na.rm = T)

  clean_data_dm$meanRT[clean_data_dm$subjectnumber == subj_id] = mean_RT
}



m1_meanRT_allNoNFC_intfx = lmer(meanRT ~ 1 + all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1_best +
                             (1 | subjectnumber), data = clean_data_dm)
summary(m1_meanRT_allNoNFC_intfx)

m1_meanRT_allNoWMC_intfx = lmer(meanRT ~ 1 + all_diff_cont * prev_all_diff_cont * NCS_HighP1_LowN1 +
                                  (1 | subjectnumber), data = clean_data_dm)
summary(m1_meanRT_allNoWMC_intfx)












######



## Using continuous difficulty #################
m0_pupil_decision_cont = lmer(decision_mean ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best + prev_all_diff_cont * capacity_HighP1_lowN1_best +
                                (1 | subjectnumber), data = clean_data_dm)
summary(m0_pupil_decision_cont)
# Negative effects of current and previous difficulty

m0_pupil_isi_cont = lmer(isi_mean ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best + prev_all_diff_cont * capacity_HighP1_lowN1_best +
                           (1 | subjectnumber), data = clean_data_dm)
summary(m0_pupil_isi_cont)
# Negative effects of current & previous difficulty

m0_pupil_otc_cont = lmer(outcome_mean ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best + prev_all_diff_cont * capacity_HighP1_lowN1_best +
                           (1 | subjectnumber), data = clean_data_dm)
summary(m0_pupil_otc_cont)
# Negative effects of current and previous difficulty
# INTERACTION btwn current difficulty & capacity --> higher cap have larger pupil dilation, lower cap have more constriction

m0_pupil_iti_cont = lmer(iti_mean ~ 1 + all_diff_cont * capacity_HighP1_lowN1_best + prev_all_diff_cont * capacity_HighP1_lowN1_best +
                           (1 | subjectnumber), data = clean_data_dm)
summary(m0_pupil_iti_cont)
# Negative effects of current and previous difficulty
# Trend INTERACTION btwn current difficulty & capacity (like otc, see above



## Using categorical difficulty #################
m0_pupil_decision_cat = lmer(decision_mean ~ 1 + easyP1difficultN1 * capacity_HighP1_lowN1_best + easyP1difficultN1_prev * capacity_HighP1_lowN1_best +
                               (1 | subjectnumber), data = clean_data_dm)
summary(m0_pupil_decision_cat)
# trend of previous difficulty (larger on difficult)

m0_pupil_isi_cat = lmer(isi_mean ~ 1 + easyP1difficultN1 * capacity_HighP1_lowN1_best + easyP1difficultN1_prev * capacity_HighP1_lowN1_best +
                          (1 | subjectnumber), data = clean_data_dm)
summary(m0_pupil_isi_cat)
# current difficulty (larger on easy trials)

m0_pupil_otc_cat = lmer(outcome_mean ~ 1 + easyP1difficultN1 * capacity_HighP1_lowN1_best + easyP1difficultN1_prev * capacity_HighP1_lowN1_best +
                          (1 | subjectnumber), data = clean_data_dm)
summary(m0_pupil_otc_cat)
# no effects

m0_pupil_iti_cat = lmer(iti_mean ~ 1 + easyP1difficultN1 * capacity_HighP1_lowN1_best + easyP1difficultN1_prev * capacity_HighP1_lowN1_best +
                          (1 | subjectnumber), data = clean_data_dm)
summary(m0_pupil_iti_cat)
# current difficulty (larger on difficult)

# These effects flip-flop and move around!





#
# MOST STUFF BELOW HERE CAN BE IGNORED ########################################################
#

# Continuous difficulty (including previous) and continuous capacity and categorical capacity
m1_prev_diffCont_capacityCont_capacityCat_intxn_rfx = lmer(sqrtRT ~ 1 + all_diff_cont * prev_all_diff_cont * complexspan_demeaned +
                                                             all_diff_cont * prev_all_diff_cont * capacity_HighP1_lowN1 +
                                                             (1 | subjectnumber), data = clean_data_dm);
summary(m1_prev_diffCont_capacityCont_capacityCat_intxn_rfx)





### Using Best Span Overall ###
m1_diffCat_capacityCont_intxn_rfx = lmer(sqrtRT ~ 1 + easyP1difficultN1 * complexspan_demeaned +
                                           (1 | subjectnumber), data = clean_data_dm);
summary(m1_diffCat_capacityCont_intxn_rfx)
# difficulty interacts with span - difficulty's slowing of RTs is potentiated for people with high cap.

m1_prev_capacityCont_intxn_rfx = lmer(sqrtRT ~ 1 +
                                        easyP1difficultN1 * easyP1difficultN1_prev * complexspan_demeaned +
                                        (1 | subjectnumber), data = clean_data_dm);
summary(m1_prev_capacityCont_intxn_rfx)
# same main effects (curr. difficult -> slower RTs; prev. difficulty -> faster RTs; curr. difficulty effect
# is stronger for high cap. than low cap. folks)

m1_capacityCont_intxn_rfx = lmer(sqrtRT ~ 1 + easy * complexspan_demeaned + difficult * complexspan_demeaned +
                                   (1 | subjectnumber), data = clean_data_dm);
summary(m1_capacityCont_intxn_rfx)
# easy is faster (difficult is trendingly slower); No net effect of complex span.
# but: BOTH easy & difficult are slower with complex span, but to diff. degrees?

m1_diffCont_capacityCont_intxn_rfx = lmer(sqrtRT ~ 1 + all_diff_cont * complexspan_demeaned +
                                            (1 | subjectnumber), data = clean_data_dm);
summary(m1_diffCont_capacityCont_intxn_rfx)
# same as observed above; span interacts with current difficulty.

# Continuous difficulty (including previous) and continuous capacity
m1_prev_diffCont_capacityCont_intxn_rfx = lmer(sqrtRT ~ 1 +
                                                 all_diff_cont * prev_all_diff_cont * complexspan_demeaned +
                                                 (1 | subjectnumber), data = clean_data_dm);
summary(m1_prev_diffCont_capacityCont_intxn_rfx)
# Curr. difficulty predicts slower
# Prev. difficulty predicts faster
# Span potentiates current difficulty
# Span *trendingly* eliminates prev. difficulty?



#### Average RTs by Capacity and Trial Type ####

#look at average RT for different types of controllers
meanRT_capacity_High <- numeric(number_of_clean_subjects)
meanRT_capacity_Low <- numeric(number_of_clean_subjects)
meanRT_diff_capacity_High <- numeric(number_of_clean_subjects)
meanRT_diff_capacity_Low <- numeric(number_of_clean_subjects)
meanRT_easy_capacity_High <- numeric(number_of_clean_subjects)
meanRT_easy_capacity_Low <- numeric(number_of_clean_subjects)

for (subj in 1:number_of_clean_subjects) {
  subj_id <- keep_participants[subj]
  tmpdata <- clean_data_dm[clean_data_dm$subjectnumber == subj_id, ]
  meanRT_capacity_High[subj] <- mean(tmpdata$reactiontime[tmpdata$capacity_HighP1_lowN1 == 1], na.rm = TRUE)
  meanRT_capacity_Low[subj] <- mean(tmpdata$reactiontime[tmpdata$capacity_HighP1_lowN1 == -1], na.rm = TRUE)
  meanRT_diff_capacity_High[subj] <- mean(tmpdata$reactiontime[(tmpdata$capacity_HighP1_lowN1 == 1) & (tmpdata$easyP1difficultN1 == -1)], na.rm = TRUE)
  meanRT_easy_capacity_High[subj] <- mean(tmpdata$reactiontime[(tmpdata$capacity_HighP1_lowN1 == 1) & (tmpdata$easyP1difficultN1 == 1)], na.rm = TRUE)
  meanRT_diff_capacity_Low[subj] <- mean(tmpdata$reactiontime[(tmpdata$capacity_HighP1_lowN1 == -1) & (tmpdata$easyP1difficultN1 == -1)], na.rm = TRUE)
  meanRT_easy_capacity_Low[subj] <- mean(tmpdata$reactiontime[(tmpdata$capacity_HighP1_lowN1 == -1) & (tmpdata$easyP1difficultN1 == 1)], na.rm = TRUE)
}

mean(meanRT_capacity_Low, na.rm = T)
sd(meanRT_capacity_Low, na.rm = T)

mean(meanRT_capacity_High, na.rm = T)
sd(meanRT_capacity_High, na.rm = T)

t.test(meanRT_capacity_High, meanRT_capacity_Low, na.rm = T)

mean((meanRT_diff_capacity_High), na.rm = T);
sd((meanRT_diff_capacity_High), na.rm = T);
mean((meanRT_easy_capacity_High), na.rm = T);
sd((meanRT_easy_capacity_High), na.rm = T);
mean((meanRT_diff_capacity_Low), na.rm = T);
sd((meanRT_diff_capacity_Low), na.rm = T);
mean((meanRT_easy_capacity_Low), na.rm = T);
sd((meanRT_easy_capacity_Low), na.rm = T);

print('the end')
