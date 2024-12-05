# EDI Eyetracking Data Processing Script
#
# Script to process the eyetracking data collected from the EDI (Effort & 
# Decision-Making & Interoception Task) study in 2024-2025.
#
# NOTE: This processing pathway can take 30-60 seconds *per* participant. Processing the
# pupillometry for e.g. 80 participants is thus expected to take 1h30m.
#
# Plan accordingly!


#### STEP 1: SET YOUR WORKING DIRECTORY! ####
# # On PSH's computers...
# setwd('/Users/sokolhessner/Documents/gitrepos/edi/');

# setwd('C:/Users/jvonm/Documents/GitHub/cge');
# Sys.setenv(R_CONFIG_ACTIVE = 'tabletas');

library(tictoc);
tic();

#### STEP 2: then run from here on the same ####
config = config::get();

setwd(config$path$data$raw);

#### STEP 3: Get the file names & set variables ####
cat('Identifying eye-tracking data file locations.\n');
etfn = dir(pattern = glob2rx('edi*.asc'),full.names = T, recursive = T);
rdmfn = dir(pattern = glob2rx('edi*RDM*.csv'),full.names = T, recursive = T);

all_dirs = dir(pattern = glob2rx('edi*'));
subject_IDs = c();
for (s in 1:length(all_dirs)){
  subject_IDs = c(subject_IDs,as.numeric(substr(all_dirs[s],4,6))) # read subject IDs from directory names
}

## Setting parameters for eye-tracking preprocessing ##
blink_length_threshold = 20; # milliseconds above which (ms) counts as a 'blink'. There are plenty of super-short missing samples.
na_fill_before = 100; # how many milliseconds to expand the blinks backward
na_fill_after = 200; # how many milliseconds to expand the blinks forward
# NON-BLINK gaps are not extended; they are interpolated across.

maximum_width_allowable_missing = 1000; # milliseconds above which we do NOT interpolate across and just leave as an NA

smoothing_window_width = 11; # points to smooth over; 11 pts @ 1000Hz = 11ms

number_of_trials = 170; # number of RDM trials

baseline_window_width = 500; # milliseconds of the baseline window width used for baseline correction

# Exclusion Criteria-Relevant Values
fraction_allowable_missing_samples_raw = 0.2; # max. fraction of raw samples that are allowed to be missing for a participant

fraction_allowable_missing_samples = 0.5; # max. fraction of samples that are allowed to be missing and the trial still being used

fraction_allowable_missing_trials = 0.2; # max. fraction of trials that are allowed to be missing and the participant still contribute to analysis

#### STEP 4: SET UP FOR SUBJECT LOOP ####
library('eyelinker') # for eyelink-specific file interaction
library('zoo') # for interpolation
# library('tictoc') # for timing this process
# library('gazer') # Requires R 4.2 or higher; may need to upgrade?

number_of_subjects = length(etfn);

pupil_qa_metric_columns = c(
  'subject_number',
  'number_of_missing_samples_raw',
  'fraction_of_missing_samples_raw',
  'number_of_blinks',
  'number_of_missing_samples_proc',
  'fraction_of_missing_samples_proc',
  'number_missing_trials_proc',
  'fraction_missing_trials_proc',
  'validation_average_error',
  'validation_max_error',
  'validationG0F1P2',
  'keep_subj_pupil_raw',
  'keep_subj_pupil_proc',
  'keep_subj_pupil'
)
pupil_QA_metrics = array(data = NA, dim = c(number_of_subjects,length(pupil_qa_metric_columns)));
pupil_QA_metrics = as.data.frame(pupil_QA_metrics);
colnames(pupil_QA_metrics) <- pupil_qa_metric_columns;

et_summary_data_column_names = c(
  'subject_number',
  'trial_number',
  'wind1_predisp_onset_mean', # predisposition window
  'wind2_effort_isi_mean', # effort window
  'wind3_eval_otciti_mean', # evaluation window
  'wind4_prep_lateiti_mean' # preparation
)
timestamp_column_names = c(
  'decision_start',
  'decision_end',
  'outcome_start',
  'outcome_end',
  'iti_end'
)
data_pupil = array(data = NA, dim = c(0, length(et_summary_data_column_names) + length(timestamp_column_names)));
data_pupil = as.data.frame(data_pupil);
colnames(data_pupil) <- c(et_summary_data_column_names, timestamp_column_names);

#### STEP 5: SUBJECT LOOP ####
for (s in 1:number_of_subjects){

  # tic() # for timing this process
  # s = 4; # for debugging

  cat(sprintf('Starting subject CGE%03i.\n',subject_IDs[s]))
  cat('Loading ET data...  ')
  raw_et_data = read.asc(etfn[s], samples = T, events = F)
  cat('Done.\n')

  # Settings:
  # 1000 Hz sample rate
  # 1280 (w) x 1024 (h) screen size)
  # Diameter pupil data type (not area)

  # # How to visualize the data
  # # NOTE: these are slow to execute because the dataset is so large (~1.2 million datapoints/person)
  # # Gaze location (coordinates (0,0) are TOP LEFT)
  # plot(raw_et_data$raw$xp, -raw_et_data$raw$yp, pch = 16, cex = .5, col = rgb(1,0,0,.1),
  #      xlim = c(-200, raw_et_data$info$screen.x + 200), ylim = c(200, -raw_et_data$info$screen.y-200))
  # lines(x = c(0, raw_et_data$info$screen.x, raw_et_data$info$screen.x, 0, 0),
  #       y = c(0, 0, -raw_et_data$info$screen.y, -raw_et_data$info$screen.y, 0))
  # plot(raw_et_data$raw$ps, type = 'l') # the pupil dilation data
  #
  # # Code to construct a low-res 'heatmap' that displays more easily than the full data.
  # xres = 50;
  # yres = 51;
  # heatmap_density = array(data = NA, dim = c(xres, yres));
  # xd = raw_et_data$info$screen.x;
  # yd = raw_et_data$info$screen.y;
  # for (x in 1:xres){
  #   print(x)
  #   for (y in 1:yres){
  #     heatmap_density[x,y] = sum((raw_et_data$raw$xp >= (xd/xres*(x-1))) & (raw_et_data$raw$xp < (xd/xres*x)) &
  #                                (raw_et_data$raw$yp >= (yd/yres*(y-1))) & (raw_et_data$raw$yp < (yd/yres*y)),
  #                                na.rm = T);
  #   }
  # }
  # image(z = log(heatmap_density))

  ##### Processing #####

  pupil_data_raw = raw_et_data$raw$ps;
  time_data = raw_et_data$raw$time; # UNCORRECTED! in milliseconds

  ###### 1. Identify blink points ######
  missing_points_ind = which(is.na(pupil_data_raw)); # index of all missing datapoints
  blink_init_sample = missing_points_ind[c(1,which(diff(missing_points_ind) > 1) + 1)] # first sample of missing data epochs
  blink_final_sample = missing_points_ind[c(which(diff(missing_points_ind) > 1), length(missing_points_ind))] # final sample
  blink_length = time_data[blink_final_sample] - time_data[blink_init_sample]; # length in ms

  blink_data = as.data.frame(cbind(blink_init_sample, blink_final_sample, blink_length))
  hist(blink_data$blink_length, xlab = 'milliseconds', main = 'Blink Lengths', breaks = 200) # opt: visualize blink lengths

  ###### 2. Summarize Missing Data ######
  number_of_missing_samples_raw = length(missing_points_ind);
  fraction_of_missing_samples_raw = number_of_missing_samples_raw/length(pupil_data_raw);
  number_of_blinks = length(blink_init_sample);

  ###### 3. Extend blink points ######
  pupil_data_extend = pupil_data_raw;

  cat('Extending blink gaps...  ')
  for (b in 1:number_of_blinks){
    if (blink_data$blink_length[b] > blink_length_threshold){ # if it meets criterion as a blink

      index1 = which(time_data > (time_data[blink_init_sample[b]] - na_fill_before))[1];
      if (is.na(index1)){
        index1 = 1; # make it the first index
      }

      index2 = which(time_data > (time_data[blink_final_sample[b]] + na_fill_after))[1] - 1;
      if (is.na(index2)){
        index2 = length(time_data); # make it the final index
      }


      pupil_data_extend[index1:index2] = NA;
      # Both of these are ">" because this code identifies SINGLE POINTS (see the [1] element)
      # This code identifies the first sample within the na_fill_before window before the initial blink sample
      # and the first sample BEFORE the na_fill_after window after the final blink sample (see: both the
      # [1] and the "-1" portions of that line)
      #
      # Alternately phrased, this code identifies all samples WITHIN the before-to-after window.
    }
  }
  cat('Done.\n')

  ###### 4. Interpolate ######

  # Interpolate linearly across NA gaps
  cat('Interpolating linearly across data gaps...  ')
  pupil_data_extend_interp = na.approx(pupil_data_extend);
  cat('Done.\n')
  # technically, this approach linearly interpolates across datapoints, not timepoints
  # i.e. if one or more timepoints are simply missing (not that they would be NAs, but
  # that they would not be *present*), this would inaccurately interpolate. This is
  # probably fine, though - missing samples are not expected, and the harm done would
  # be pretty minor.

  ######  5. Replace excessively long missing sections with NAs ######

  # Put back in the NAs for the excessively long missing sections, INCLUDING  the na_fill*
  # extensions backward and forward.
  for (b in 1:number_of_blinks){
    if (blink_data$blink_length[b] >= maximum_width_allowable_missing) { # if it was too big of a gap
      pupil_data_extend_interp[(blink_data$blink_init_sample[b] - na_fill_before)
                               :(blink_data$blink_final_sample[b] + na_fill_after)] = NA;
    }
  }
  # Technically, na_fill_before (and *after) are in milliseconds not samples, but b/c
  # sampling rate was 1000Hz, they're the same - e.g., 100 timestamps back is 100ms back.

  ###### 6. Smooth (N-point moving window) ######
  cat('Smoothing pupil diameter data...  ')
  pupil_data_extend_interp_smooth = rollapply(pupil_data_extend_interp,
                                              width = smoothing_window_width, FUN = 'mean', partial = T,
                                              align = 'center', na.rm = T)
  cat('Done.\n')

  number_of_missing_samples_proc = length(which(is.na(pupil_data_extend_interp_smooth)));
  fraction_of_missing_samples_proc = number_of_missing_samples_proc/length(pupil_data_extend_interp_smooth);

  ###### 7. Convert pupil diameter data to mm ######
  pupil_data_extend_interp_smooth_mm = pupil_data_extend_interp_smooth*8/7637.32;
  # SEE THIS LINK: https://researchwiki.solo.universiteitleiden.nl/xwiki/wiki/researchwiki.solo.universiteitleiden.nl/view/Hardware/EyeLink/#:~:text=EyeLink%20reports%20pupil%20size%20as,circle%20with%20a%20known%20diameter.

  ###### 8. Downsample Eyetracking Data for later saving ######
  downsampled_rate = 40;
  downsampled_intersample_interval = 1000/downsampled_rate;

  downsample_indices = seq(from = 1, to = length(time_data), by = downsampled_intersample_interval);
  pupil_data_extend_interp_smooth_mm_downsampled = pupil_data_extend_interp_smooth_mm[downsample_indices];
  time_data_downsampled = time_data[downsample_indices];

  downsampled_et_data = cbind(time_data_downsampled, pupil_data_extend_interp_smooth_mm_downsampled);

  ###### 9. Identify and extract timestamp for practice start from ET file ######
  cat('Aligning timestamps...  ')
  et_file_connection = file(etfn[s],'r') # open the file connection to the ASC file.

  msgs = ''; # where we'll store messages
  number_of_messages = 0; # set our counter

  while ( TRUE ) {
    line = readLines(et_file_connection, n = 1) # read a line of the file
    if ( length(line) == 0 ) { # if we have read the full file and there are no more lines left, stop
      break
    }
    # print(line) # for debugging
    if ('MSG' == substr(line, 1, 3)) { # if this is a 'message' line...
      number_of_messages = number_of_messages + 1; # increment the # of messages we've found
      # print(line) # for debugging
      msgs[number_of_messages] = line; # store the message
    }
  }

  close(et_file_connection) # close the file connection

  for (m in 1:number_of_messages){
    if ('Practice Instructions Shown' == substr(msgs[m], nchar(msgs[m])-26, nchar(msgs[m]))){ # identify the msg with this text
      first_ind = 5; # the first digit is always 5 characters in
      last_ind = nchar(msgs[m])-68; # the "practice text shown" is always 18 characters long, preceded by a 1 char space; so last number is 20 back from end
      et_alignment_time = as.numeric(substr(msgs[m], first_ind, last_ind)) # pull out the start time from that msg
    }
  }

  ####### 10. Identify and extract Validation Information/metrics from ET file #######
  val_msgs = grep('VALIDATION HV', msgs);
  last_val_msg = msgs[val_msgs[length(val_msgs)]];

  validation_average_error = as.numeric(substr(sub(".*ERROR ","",last_val_msg),1,5));
  val_msg_subset = sub(" max.*","",last_val_msg);
  validation_max_error = as.numeric(substr(val_msg_subset, nchar(val_msg_subset)-4, nchar(val_msg_subset)))
  val_msg_subset = sub(" ERROR.*","",last_val_msg);
  validation_str = substr(val_msg_subset, nchar(val_msg_subset)-3,nchar(val_msg_subset))
  if (validation_str == 'GOOD'){
    validationG0F1P2 = 0
  } else if (validation_str == 'FAIR'){
    validationG0F1P2 = 1
  } else if (validation_str == 'POOR'){
    validationG0F1P2 = 2
  }

  cat(sprintf('alignment timestamp = %i...  ', et_alignment_time))

  time_data = time_data - et_alignment_time; # Correct all timestamps to be relative to this moment
  downsampled_et_data[,1] = downsampled_et_data[,1] - et_alignment_time # correct downsampled timestamps too!

  cat('Done.\n')

  ##### Conduct basic trial-level analyses ######
  ###### 1. Create Behavioral Event Timestamp Matrix ######
  tmpdata = read.csv(rdmfn[s]);

  event_timestamps = array(data = NA, dim = c(number_of_trials, length(timestamp_column_names)))
  colnames(event_timestamps) <- timestamp_column_names;
  event_timestamps = as.data.frame(event_timestamps)

  beh_alignment_time = tmpdata$pracStartTxt.started[3];

  trial_index = is.finite(tmpdata$realChoiceResp.started);

  event_timestamps$decision_start = tmpdata$realChoiceResp.started[trial_index];
  event_timestamps$decision_end = tmpdata$isiRealFix.started[trial_index];
  event_timestamps$outcome_start = tmpdata$isiRealFix.stopped[trial_index];
  event_timestamps$outcome_end = tmpdata$itiRealFix.started[trial_index];
  event_timestamps$iti_end = tmpdata$itiRealFix.stopped[trial_index];

  event_timestamps[!(is.finite(tmpdata$choices[trial_index])),] = NA;

  event_timestamps = event_timestamps - beh_alignment_time; # align the behavioral data to the same moment
  event_timestamps = event_timestamps * 1000; # into milliseconds, like eyetracking

  ###### 2. Calculate Summary Metrics of Pupillometry #####
  cat('Calculating summary pupillometry measures...  ')

  et_summary_stats = array(data = NA, dim = c(number_of_trials, length(et_summary_data_column_names)))
  colnames(et_summary_stats) <- et_summary_data_column_names;
  et_summary_stats = as.data.frame(et_summary_stats)
  et_summary_stats$subject_number = subject_IDs[s];
  et_summary_stats$trial_number = 1:number_of_trials;

  # # PLOTTING CODE
  # par(mfrow = c(2,1)); # Set up the individual-level plot
  # # Pre-decision | Decision Start
  # # Decision End | ISI | Outcome | ITI
  # plot(1, type = "n", xlab = "milliseconds", ylab = "raw pupil diameter (px)", main = "Aligned to Decision Window Start",
  #      xlim = c(-baseline_window_width, 3000), ylim = c(1000, 8000))
  # p1_coords = par('usr');
  #         # pre-dec window, up until 3000 ms into the 4000ms response window
  #
  # plot(1, type = "n", xlab = "milliseconds", ylab = "raw pupil diameter (px)", main = "Aligned to Choice",
  #      xlim = c(-3000, 5500), ylim = c(1000, 8000))
  # p2_coords = par('usr');
  #         # the last 3000ms of the 4000ms response window, ISI (1000), Otc (1000), and ITI (3000 or 3500ms)

  for (t in 1:number_of_trials){

    ## Check how much data is missing in this trial ##

    # Using FULLY-PROCESSED pupil data from start of DECISION to end of ITI
    full_trial_pupil = pupil_data_extend_interp_smooth_mm[(time_data >= (event_timestamps$decision_start[t])) &
                                                            (time_data < event_timestamps$iti_end[t])];

    # Calculate missing fraction of data
    fraction_missing_trial_data = sum(is.na(full_trial_pupil))/length(full_trial_pupil);

    if (fraction_missing_trial_data > fraction_allowable_missing_samples){ # if missing fraction is > 0.5 (50%)...
      next # skip analysis of this trial
    }

    # WINDOW 1: PREDISPOSITION (500MS BEFORE & AFTER ONSET OF DECISION OPTIONS)
    indices = (time_data >= event_timestamps$decision_start[t] - 500) &
      (time_data < event_timestamps$decision_start[t] + 500);
    pupil_tmp = pupil_data_extend_interp_smooth_mm[indices];
    et_summary_stats$wind1_predisp_onset_mean[t] = mean(pupil_tmp, na.rm = T)


    # WINDOW 2: EFFORT (DECISION END TO START OF OUTCOME, AKA ISI)
    indices = (time_data >= event_timestamps$decision_end[t]) &
      (time_data < event_timestamps$outcome_start[t]);
    pupil_tmp = pupil_data_extend_interp_smooth_mm[indices];
    et_summary_stats$wind2_effort_isi_mean[t] = mean(pupil_tmp, na.rm = T)

    # WINDOW 3: EVALUATION (OUTCOME START TO ITI START + 1000MS)
    indices = (time_data >= event_timestamps$outcome_start[t]) &
      (time_data < event_timestamps$outcome_end[t]+1000);
    pupil_tmp = pupil_data_extend_interp_smooth_mm[indices];
    et_summary_stats$wind3_eval_otciti_mean[t] = mean(pupil_tmp, na.rm = T)

    # WINDOW 4: PREPARATION (ITI START + 1000MS TO +3000MS)
    indices = (time_data >= event_timestamps$outcome_end[t] + 1000) &
      (time_data < event_timestamps$outcome_end[t] + 3000);
    pupil_tmp = pupil_data_extend_interp_smooth_mm[indices];
    et_summary_stats$wind4_prep_lateiti_mean[t] = mean(pupil_tmp, na.rm = T)

  }

  cat('Done.\n')
  # loop_time_elapsed = toc(quiet = T); # for timing this process

  number_of_missing_trials_proc = sum(is.na(et_summary_stats$wind1_predisp_onset_mean));
  fraction_of_missing_trials_proc = number_of_missing_trials_proc/number_of_trials;

  cat(sprintf('CGE%03i: RAW missing %i samples (%.1f%%); %i blinks. PROCESSED missing %i samples (%.1f%%). %i trial(s) (%.0f%%) not analyzed for missing data.\n',
              subject_IDs[s], number_of_missing_samples_raw, fraction_of_missing_samples_raw*100, number_of_blinks,
              number_of_missing_samples_proc, fraction_of_missing_samples_proc*100, number_of_missing_trials_proc,
              fraction_of_missing_trials_proc*100))

  # Perform Exclusions
  keep_subj_pupil_raw = NA; # exclusion for raw data missing
  keep_subj_pupil_proc = NA; # exclusion for processed data missing
  keep_subj_pupil = NA; # overall exclusion

  keep_subj_pupil_raw = !(fraction_of_missing_samples_raw >= fraction_allowable_missing_samples_raw);
  keep_subj_pupil_proc = !(fraction_of_missing_trials_proc >= fraction_allowable_missing_trials);

  keep_subj_pupil = keep_subj_pupil_raw & keep_subj_pupil_proc

  if (!keep_subj_pupil) {
    et_summary_stats[,] = NA; # remove all ET summary data for this participant
  }

  # Store info
  pupil_QA_metrics[s,] = c(
    subject_IDs[s],
    number_of_missing_samples_raw,
    fraction_of_missing_samples_raw,
    number_of_blinks,
    number_of_missing_samples_proc,
    fraction_of_missing_samples_proc,
    number_of_missing_trials_proc,
    fraction_of_missing_trials_proc,
    validation_average_error,
    validation_max_error,
    validationG0F1P2,
    keep_subj_pupil_raw,
    keep_subj_pupil_proc,
    keep_subj_pupil
  )

  rm(list = c(
    'pupil_data_raw',
    'pupil_data_extend',
    'pupil_data_extend_interp',
    'pupil_data_extend_interp_smooth'
    ))

  pdf(sprintf('%s/plots/cge%03i_processed_pupil_plot.pdf',config$path$data$processed, subject_IDs[s]))
  plot(pupil_data_extend_interp_smooth_mm, type = 'l', main = sprintf('Processed pupil data for CGE%03i', subject_IDs[s]),
       xlab = 'milliseconds', ylab = 'pupil diameter (mm)')
  dev.off()

  data_pupil = rbind(data_pupil, cbind(et_summary_stats, event_timestamps));

  save(pupil_data_extend_interp_smooth_mm, time_data, et_summary_stats, event_timestamps, blink_data,
       file=sprintf('%s/cge%03i/cge%03i_et_processed_%s.RData',config$path$data$processed,subject_IDs[s],subject_IDs[s],format(Sys.Date(), format="%Y%m%d")))
  save(downsampled_et_data, event_timestamps, et_summary_stats,
       file=sprintf('%s/cge%03i/cge%03i_et_processed_downsampled_%s.RData',config$path$data$processed,subject_IDs[s],subject_IDs[s],format(Sys.Date(), format="%Y%m%d")))

  cat(sprintf('Finished with subject CGE%03i.\n\n',subject_IDs[s]))
}


setwd(config$path$data$processed); # set path to processed to save out the summary data

write.csv(data_pupil, file=sprintf('cge_processed_eyetracking_data_%s.csv',format(Sys.Date(), format="%Y%m%d")),
          row.names = F);
write.csv(pupil_QA_metrics, file = sprintf('cge_pupil_QA_metrics_%s.csv',format(Sys.Date(), format="%Y%m%d")),
          row.names = F);

setwd(config$path$data$raw); # reset the path to raw.

timeElapsed = toc();

cat(sprintf('Processing took %s.\n',substr(timeElapsed$callback_msg, start = 0, stop = nchar(timeElapsed$callback_msg)-8)))
cat('Finished processing eye-tracking data.\n\n\n')

# For Future Eye-Tracking Analysis Development ####
# - store baseline-corrected pupil diameter traces for event segments (i.e. dec; otc)

