### NOTES TO MYSELF ON DESIGN NEEDS ###
# import the right packages (pychopy, numpy, random, os, math, statistics, pandas, time)
# import modules (psychopy: visual, core, event, monitors - from psychopy.hardware import keyboard)
# ensure paths start from the same directory
# make sure psychopy version is correct ('2022.2.4')
# create escape function to be used at any point during the task
# set up window
# don't quite know yet how to exchange height setting to pixel setting so that everything looks like
# create a file to be saved (make sure to use os.sep to work on both mac and windows)
# create an empty data frame to be filled
### record stimuli shown, choices shown, choices made, RTs, timestamps, probabilities
# create objects at the beginning that will be used throughout (avoid having to type over and over)
### real vs. test, how many for the test, colors, fonts, font size, shapes, shape dimensions, shape locations
# create components (instructions, shapes, monetary values)
# read the appropriate CSVs to load the choices
### practice, static, and dynamic
### the dynamic choice set might be a little to complex to do at the moment

"""

Originally created as a project for Programming I with Dr. Sweeny: 
- Standalone cgeRDM task (cognition, gambling, and eye-tracking: Risky Monetary Decision-making) with Practice and Static trials, but without Dynamic trials or Eye-tracking
- Based on the cgeRDM task originally modified with PsychoPy Coder to work in-person, which in turn was based on Anna Rini's Honors Thesis' cgtRDM task built with PsychoPy Builder to work on online (Pavlovia)

Modified for Sophie Forcier's Honors Thesis, EDI (effort, decision-making, and interoception): 
- ediRDM Task Design(Practice, Static, and Dynamic trials with Eye-tracking)
- Part of the 1st Session (1st Phase: RDM, Ospan, Symspan, 2nd Phase: Qualtrics EDI Survey I (IUS, SNS), and 3rd Phase: Post-Study Questionnaire)

Author: J. Von R. Monteza (07/01/2024)

"""



##### ediRDMtask ##### (effort, decision-making, and interoception: Risky Monetary Decision-making task)



#def ediRDMtask(subID, isReal, doET, dirName, dataDirName) # ediMain wrapper function

#
##
### IMPORTING PACKAGES & MODULES ### 

# directory and filing
import os
import sys
import time

# psychopy 
from psychopy import visual, core, event, monitors

# task related
import random
import pandas as pd
import numpy as np # have just in case
import math

subID = random.randint(1, 10)

#
##
### CHANGING DIRECTORIES ### 

# change directory to ediRDM folder with all the task-related files
print(os.getcwd()) # getting the current directory
homeDir = os.path.expanduser('~') # setting to home directory 
ediTasksDir = os.path.join("GitHub", "edi", "ediTasks") # getting ediTasks directory
if os.path.exists(os.path.join(homeDir, "Desktop", ediTasksDir)): # finding which path exists for edi Day 1
    ediDay1Dir = os.path.join(homeDir, "Desktop", ediTasksDir, "day1_rdm_wmc")
elif os.path.exists(os.path.join(homeDir, "Documents", ediTasksDir)):
    ediDay1Dir = os.path.join(homeDir, "Documents", ediTasksDir, "day1_rdm_wmc")
os.chdir(os.path.join(homeDir, ediDay1Dir, "ediRDM")) # joining the pathways together and then changing the directory to it
print(os.getcwd())
ediRDMdir = os.getcwd()
#dirPath = os.chdir(dirName + os.sep + "ediRDM") # os.sep for mac and windows
#dirPath = os.chdir("Desktop" + os.sep + "edi" + os.sep + "ediTasks" + os.sep + "ediRDM") # test run on Von's laptop, tabletas

# set directory for saving ediRDMtask data
datadirPath = os.path.join(ediDay1Dir, "ediData")
datadirPathbehavioral = os.path.join(datadirPath, "ediRDMbehavioral") 
datadirPathpupillometry = os.path.join(datadirPath, "ediRDMpupillometry") 
#datadirPath = dataDirName + os.sep + "ediData"
#datadirPathbehavioral = dataDirName + os.sep + "ediData" + os.sep + "ediRDMbehavioral"
#datadirPathpupillometry = dataDirName + os.sep + "ediData" + os.sep + "ediRDMpupillometry"
#datadirPath = ("C:\\Users\\jvonm\\Desktop\\edi\\ediTasks\\ediData") # folder for all data - test run on Von's laptop, tabletas
#datadirPathbehavioral = ("C:\\Users\\jvonm\\Desktop\\edi\\ediTasks\\ediData\\ediRDMbehavioral") # rdm behavioral data - test run on Von's laptop, tabletas
#datadirPathpupillometry = ("C:\\Users\\jvonm\\Desktop\\edi\\ediTasks\\ediData\\ediRDMpupillometry") # rdm pupillometry data - test run on Von's laptop, tabletas

#
##
### CREATING GENERAL FUNCTIONS ###

# end the task
def endTask():
    endIT = event.getKeys(keyList = ['escape']) # kb. vs. event.
    
    if 'escape' in endIT:
        win.close()
        core.quit()  
        sys.exit()

# random iti time of 3 or 3.5 sec for each of the trials in the static and dynamic
def shuffle(array):
    currentIndex = len(array)
    while currentIndex != 0:
        randomIndex = random.randint(0, currentIndex - 1)
        currentIndex -= 1
        array[currentIndex], array[randomIndex] = array[randomIndex], array[currentIndex]

# prospect model
def choice_probability(parameters, riskyGv, riskyLv, certv):
    # Pull out parameters
    rho = parameters[0];
    mu = parameters[1];
    
    nTrials = len(riskyGv);
    
    # Calculate utility of the two options
    utility_riskygain_value = [math.pow(value, rho) for value in riskyGv];
    utility_riskyloss_value = [math.pow(value, rho) for value in riskyLv];
    utility_risky_option = [.5 * utility_riskygain_value[t] + .5 * utility_riskyloss_value[t] for t in range(nTrials)];
    utility_safe_option = [math.pow(value, rho) for value in certv]
    
    # Normalize values with div
    div = max(riskyGv)**rho;
    
    # Softmax
    p = [1/(1 + math.exp(-mu/div*(utility_risky_option[t] - utility_safe_option[t]))) for t in range(nTrials)];
    return p

# negative log likelihood (nll) of the participant's static choice set responses for the grid search
def pt_nll(parameters, riskyGv, riskyLv, certv, choices):
    choiceP = choice_probability(parameters, riskyGv, riskyLv, certv);
    
    nTrials = len(choiceP);
    
    likelihood = [choices[t]*choiceP[t] + (1-choices[t])*(1-choiceP[t]) for t in range(nTrials)];
    zeroindex = [likelihood[t] == 0 for t in range(nTrials)];
    for ind in range(nTrials):
        if zeroindex[ind]:
            likelihood[ind] = 0.000000000000001;
    
    loglikelihood = [math.log(likelihood[t]) for t in range(nTrials)];
    
    nll = -sum(loglikelihood);
    return nll

#
##
### GENERAL OBJECT SETTINGS ###

# luminance equated colors
c1 = [0.5216,0.5216,0.5216] # background, choice option values, and risky option line
c2 = [-0.0667,0.6392,1] # choice option circles, OR, V-Left, and N-Left

# monitor screen
screenSize = [1280, 1024] # How do I make it so that it gets the right screen size no matter what device?

# font
a = 'Arial'

# font size
instructionsH = .05
choiceTextH = .05
choiceValuesH = .1
fixCrossH = .05
noRespH = .08
ocH = .1 # oc = Outcome

# shape size
choiceSize = [.5,.5]
riskSize = [.5,.01]
hideSize = [.6,.3] # may not need this

# locations for shapes and texts # May work in a new place from where I originally have it
center = [0, 0]
leftLoc = [-.35,0]
rightLoc = [.35,0]
vLoc = [-.35,-.35]
nLoc = [.35,-.35]

# choice trial timing
choiceWin = 4

isi = 1 # interstimulus interval

itiStatic = [] # intertrial interval
itiDynamic = []
itiStatic = [3, 3.5] * 25 # jittered between 3 and 3.5 seconds for all 50 trials
itiDynamic = [3, 3.5] * 60 # jittered between 3 and 3.5 seconds for all 120 trials
shuffle(itiStatic)
shuffle(itiDynamic)

oc = 1 # outcome

#
##
### REAL VS. TEST RUN ###
isReal = 0 # 1 = yes, a real run vs. 0 = no, a test run

if isReal == 0:
    practiceSet = 3
    staticSet = 5
    dynamicSet = 5
elif isReal == 1:
    practiceSet = 5
    staticSet = 50 # 40 real trials & 10 check trials
    dynamicSet = 120 # 60 easy trials & 60 difficult trials

#
##
### COUNTING TRIALS VS. NOT
isCount = 1 # 1 = yes, count trials vs. 0 = no, don't count trials

if isCount == 0:
    countLoc = [0, 7]
    countColor = c1
elif isCount == 1:
    countLoc = [0, -.35]
    countColor = c2

#
##
### WINDOW SETUP ###
win = visual.Window(
    size = screenSize, 
    units = 'height', 
    monitor ='testMonitor', 
    fullscr = False, 
    color = c1
) # Don't quite now yet the exact translation from height to pix in terms of size and location

#
##
### WITH EYE-TRACKING VS. ONLY BEHAVIORAL ###
doET = 0 # 1 = yes, do eye-tracking vs. 0 = no, only do behavioral

if doET:
    
    #
    ##
    ### IMPORTING EYE-TRACKING MODULES ###
    import pylink
    from PIL import Image  # for preparing the Host backdrop image
    from EyeLinkCoreGraphicsPsychoPy import EyeLinkCoreGraphicsPsychoPy # eye-tracking py module located in ediTasks
    import subprocess # for converting edf to asc 
    
    #
    ##
    ### SETTING UP EYE-TRACKER ###
    
    # Step 1: Connect to the EyeLink Host PC # The Host IP address, by default, is "100.1.1.1".
    et = pylink.EyeLink("100.1.1.1")
    # Step 2: Open an EDF data file on the Host PC
    edf_fname = 'edi%s' % subID
    et.openDataFile(edf_fname + '.edf')
    # Step 3: Configure the tracker
    # Put the tracker in offline mode before we change tracking parameters
    et.setOfflineMode()
    # Step 4: Setting parameters
    # File and Link data control
    file_sample_flags = 'GAZE,GAZERES,AREA,BUTTON,STATUS,INPUT'
    et.sendCommand("file_sample_data = %s" % file_sample_flags)
    # Optional tracking parameters
    # Sample rate, 250, 500, 1000, or 2000, check your tracker specification
    et.sendCommand("sample_rate 1000")
    # Choose a calibration type, H3, HV3, HV5, HV13 (HV = horizontal/vertical),
    et.sendCommand("calibration_type = HV9")

    #
    ##
    ### CALIBRATION AND VALIDATION SETUP ###
    # get the native screen resolution used by PsychoPy
    scn_width, scn_height = win.size

    # Pass the display pixel coordinates (left, top, right, bottom) to the tracker
    # see the EyeLink Installation Guide, "Customizing Screen Settings"
    et_coords = "screen_pixel_coords = 0 0 %d %d" % (scn_width - 1, scn_height - 1)
    et.sendCommand(et_coords)

    # Write a DISPLAY_COORDS message to the EDF file
    # Data Viewer needs this piece of info for proper visualization, see Data
    # Viewer User Manual, "Protocol for EyeLink Data to Viewer Integration"
    dv_coords = "DISPLAY_COORDS  0 0 %d %d" % (scn_width - 1, scn_height - 1)
    et.sendMessage(dv_coords)

    # Configure a graphics environment (genv) for tracker calibration
    genv = EyeLinkCoreGraphicsPsychoPy(et, win)
    print(genv)  # print out the version number of the CoreGraphics library
    # Set up the calibration target # genv.setTargetType to "circle", "picture", "movie", or "spiral", e.g.,
    genv.setTargetType('circle')
    # Use a picture as the calibration target
    #genv.setTargetType('picture') ### The picture doesn't show the second time around
    #genv.setPictureTarget(os.path.join('images', 'fixTarget.bmp'))
    # Beeps to play during calibration, validation and drift correction # parameters: target, good, error
    # Each parameter could be ''--default sound, 'off'--no sound, or a wav file
    genv.setCalibrationSounds('', '', '')
    # Request Pylink to use the PsychoPy window we opened above for calibration
    pylink.openGraphicsEx(genv)
    et.doTrackerSetup()

#
##
### STIMULI SETUP ###

# ~ Instructions ~

# general instructions
rdmStartTxt = visual.TextStim(
    win,
    text = 'As discussed in the instructions, you will choose between a gamble and a guaranteed alternative choice option.\n\nPress "V" to select the option on the left OR "N" to select the option on the right.\n\n Press "Enter/Return" to move on the next screen.',
    font = a,
    height = instructionsH,
    pos = center,
    color = c2
)

# practice instructions
pracStartTxt = visual.TextStim(
    win,
    text = 'There will now be 5 practice trials.\n\nThe structure in the practice round is identical to what you will encounter in the real rounds. The goal of the practice round is to practice timing of decision-making within the four (4) second response window.\n\nWhen you are ready to begin the practice, press "V" or "N" to begin.',
    font = a,
    height = instructionsH,
    pos = center,
    color = c2
)

# static choice set instructions
statStartTxt = visual.TextStim(
    win,
    text = 'Practice complete.\n\nKeep in mind that responding quickly in this task will not speed up the task. Please take enough time to view and consider each choice option before you make a choice within the four (4) second response window.\n\nWhen you are ready to start ROUND 1 of the task, press "V" or "N".',
    font = a,
    height = instructionsH,
    pos = center,
    color = c2
)

# fitting process instructions
fittingStartTxt = visual.TextStim(
    win,
    text = 'ROUND 1 of the gambling task is complete!\n\nSetting up for the last round of the gambling task.\n\nPlease wait...',
    font = a,
    height = instructionsH,
    pos = center,
    color = c2
)

# dynamic choice set instructions
dynaStartTxt = visual.TextStim(
    win,
    text = 'Keep in mind that responding quickly in this task will not speed up the task. Please take enough time to view and consider each choice option before you make a choice within the four (4) second response window.\n\nWhen you are ready to start ROUND 2 of the task, press "V" or "N".',
    font = a,
    height = instructionsH,
    pos = center,
    color = c2
)

# task closing instructions
endStartTxt = visual.TextStim(
    win,
    text = 'You have sucessfully completed the first task in this experiment!\n\nPlease take a brief 1 minute break. \n\nYou are welcome to take a longer break, but keep in mind this study should take no longer than 1 hour to complete. \n\nWhen you are ready to move on, press "enter to continue.\n',
    font = a,
    height = instructionsH,
    pos = center,
    color = c2
)

# ~ Choice Trial Components ~

# decision window stimuli
vOpt = visual.Circle(
    win,
    size = choiceSize, 
    fillColor = c2, 
    lineColor = c2,
    pos = leftLoc,
    edges = 128
)

nOpt = visual.Circle(
    win,
    size = choiceSize, 
    fillColor = c2, 
    lineColor = c2,
    pos = rightLoc,
    edges = 128
)

riskSplit = visual.Rect(
    win, 
    size = riskSize,
    fillColor = c1, 
    lineColor = c1 
)

gainTxt = visual.TextStim(
    win, 
    font = a,
    height = choiceValuesH,
    color = c1
)

lossTxt = visual.TextStim(
    win, 
    font = a,
    height = choiceValuesH,
    color = c1
)

safeTxt = visual.TextStim(
    win, 
    font = a,
    height = choiceValuesH,
    color = c1
)

orTxt = visual.TextStim(
    win, 
    text = "OR",
    font = a,
    height = choiceTextH,
    pos = center,
    color = c2
)

vTxt = visual.TextStim(
    win, 
    text = "V - Left",
    font = a,
    height = choiceTextH,
    pos = vLoc,
    color = c2);

nTxt = visual.TextStim(
    win, 
    text = "N - Left",
    font = a,
    height = choiceTextH,
    pos = nLoc,
    color = c2);

countTrialTxt = visual.TextStim(
    win, 
    text = "",
    font = a,
    height = choiceTextH,
    pos = countLoc,
    color = countColor)

# isi and iti fixation stimuli
fixTxt = visual.TextStim(
    win, 
    text = "+",
    font = a,
    height = choiceTextH,
    pos = center,
    color = c2
)

# choice outcome stimuli
noRespTxt = visual.TextStim(
    win, 
    text = "You did not respond in time",
    font = a,
    height = ocH,
    pos = center,
    color = c2
)

ocRiskyHide = visual.Rect(
    win,
    size = hideSize, 
    fillColor = c1, 
    lineColor = c1,
)

#
##
### TIMER ###
timer = core.Clock()

#
##
### EDI DATA STRUCTURE SETUP ###
ediData = []
ediData.append(
    [
        "trialNumber", # [0] # should incrementally increase by 1
        "checkTrial", # [1] # should be "0" for no, not a check trial or "1" for yes, a check trial
        "gainValue", # [2]
        "lossValue", # [3] # should always be $0 (except if a check trial)
        "safeValue", # [4]
        "choiceProbability", # [5]
        "easy0difficult1", # [6]
        "choiceMade", # [7] # should be "0" if safe option was chosen or "1" if the risky option was chosen
        "choiceKey", # [8]
        "outcomeValue", # [9] # if a choice was made, the value should match the value location and key response of choiceMade
        "location", # [10]
        "riskSplitLocation", # [11]
        "gainLocation", # [12]
        "lossLocation", # [13]
        "safeLocation", # [14]
        "hideGainLocation", # [15]
        "hideLossLocation", # [16]
        "instructionStart", # [17] # first point should be ground 0 for when the task starts # second point should be ground 0 for eye-tracking # last should be for closing instructions
        "instructionEnd", # [18]
        "choiceStart", # [19] # should be the same time as when the choice values and texts are shown - their Start (choices are shown)
        "choiceEnd", # [20] # should be the same time as when the choice values and texts are disappear - their End (choice is made)
        "isiStart", # [21] # should be just after or exactly at the moment of choiceEND
        "isiEnd", # [22] # should be 1 sec
        "outcomeStart", # [23] # should be just after or exactly at the moment of isiEND
        "outcomeEnd", # [24] # should be 1 sec
        "itiStart", # [25] # should be just after or exactly at the moment of outcomeEND
        "itiEnd", # [26] # should be either 3 or 3.5 sec
        "bestRho", # [27]
        "bestMu" # [28]
    ]
)

#
##
### CREATING DATA APPENDING FUNCTIONS ###

# append a new row of empty data ("") into the data structure that matches the length of the columns 
def empty_data_appending():
    ediData.append([""] * len(ediData[0]))

# index the last row of the data structure for future appending (e.g., ediData[data_appending_index][<whatever column>])
def data_appending_index():
    return len(ediData) - 1

#
##
### GENERAL INSTRUCTIONS START ### 
rdmStartTxt.draw()
win.flip()
genInstructionsStart = timer.getTime()
response = event.waitKeys(keyList = ['return'], timeStamped = timer)
genInstructionsEnd = response[0][1]
endTask()
empty_data_appending()
ediData[data_appending_index()][17:19] = [genInstructionsStart, genInstructionsEnd]

#
##
### CREATING TRIAL DESIGN FUNCTIONS ### (this way I don't have to repeat code since practice, static, and dynamic trial structure is the same)

# rounding the choice option values in the choice stimuli file to be shown during the decision window
def choice_value_rounding():
    global gainRounded, lossRounded, safeRounded
    
    gainRounded = '%.2f' % round(gain, 2) # removed the $ here and moved it to the bottom - it was being saved into the data
    lossRounded = '%.0f' % round(loss, 0)
    safeRounded = '%.2f' % round(safe, 2)
    
    gainTxt.setText('$' + gainRounded)
    lossTxt.setText('$' + lossRounded)
    safeTxt.setText('$' + safeRounded)

# randomize choice option locations
def choice_location_randomizing():
    global loc, riskSplitLoc, gainTxtLoc, lossTxtLoc, safeTxtLoc, hideGainLoc, hideLossLoc
    
    loc = random.choice([1,2]) # initial randomization of the decision window stimuli

    if loc == 1:
        riskSplitLoc = [-.35,0] # risky option is on the left = v
        gainTxtLoc = [-.35,.1]
        lossTxtLoc = [-.35,-.1]
        safeTxtLoc = [.35,0]
        hideGainLoc = [-.35, .15]
        hideLossLoc = [-.35, -.15]
    else:
        riskSplitLoc = [.35,0] # risky option is on the right = n
        gainTxtLoc = [.35,.1]
        lossTxtLoc = [.35,-.1]
        safeTxtLoc = [-.35,0]
        hideGainLoc = [.35, .15]
        hideLossLoc = [.35, -.15]
    
    riskSplit.setPos(riskSplitLoc) # set the position of the decision window stimuli # do these get properly used in the decision_window_starting() function??? - seems to be
    gainTxt.setPos(gainTxtLoc)
    lossTxt.setPos(lossTxtLoc)
    safeTxt.setPos(safeTxtLoc)

# drawing decision window stimuli and retrieving trial start time
def decision_window_starting():
    global choiceStart
    
    vOpt.draw() # draw choice options
    nOpt.draw()
    riskSplit.draw()
    gainTxt.draw()
    lossTxt.draw()
    safeTxt.draw()
    orTxt.draw()
    vTxt.draw()
    nTxt.draw()
    
    win.flip() # show choice options
      
    choiceStart = timer.getTime() # get time 

# counting trials if set to count trials
def trial_counting():
    if isCount == 1:
        countTrialTxt.setText(trial)
        countTrialTxt.draw()

# recording the choice made and time
def decision_making():
    global response, choiceMade, choiceKey, outcomeValue, choiceEnd

    endTask() # end task if wanted/needed
    
    response = event.waitKeys(maxWait = choiceWin, keyList = ['v', 'n'], timeStamped = timer)

    if response is None:
        choiceMade = math.nan
        choiceKey = math.nan
        outcomeValue = math.nan
        choiceEnd = math.nan
    elif response[0][0] == 'v' or response[0][0] == 'n':
        if (loc == 1 and response[0][0] == 'v') or (loc == 2 and response[0][0] == 'n'):
            choiceMade = 1 # chose the risky option
            choiceKey = response[0][0]
            outcomeValue = random.choice([gainRounded, lossRounded]) # randomly chooses the gain or loss
        elif (loc == 1 and response[0][0] == 'n') or (loc == 2 and response[0][0] == 'v'):
            choiceMade = 0 # chose the safe option
            choiceKey = response[0][0]
            outcomeValue = safeRounded
        choiceEnd = response[0][1]

# setting up isi
def isi_waiting():
    global isiStart, isiEnd
    
    fixTxt.draw()
    win.flip()
    
    endTask() # end task if wanted/needed
    
    isiStart = timer.getTime()
    core.wait(isi)
    isiEnd = timer.getTime()

# showing outcome of the choice made
def outcome_showing():
    global outcomeStart, outcomeEnd
    
    if response is None:
        noRespTxt.draw()
        win.flip()
        endTask() # end task if wanted/needed
        outcomeStart = timer.getTime()
        core.wait(oc)
        outcomeEnd = timer.getTime()
    elif (loc == 1 and response[0][0] == 'v'):
        if outcomeValue == gainRounded: # risky option on the left was chosen and won
            ocRiskyHide.setPos(hideLossLoc)
            vOpt.draw()
            gainTxt.draw()
            ocRiskyHide.draw()
            win.flip()
            endTask() # end task if wanted/needed
            outcomeStart = timer.getTime()
            core.wait(oc)
            outcomeEnd = timer.getTime()
        elif outcomeValue == lossRounded: # risky option on the left was chosen and lost 
            ocRiskyHide.setPos(hideGainLoc)
            vOpt.draw()
            lossTxt.draw()
            ocRiskyHide.draw()
            win.flip()
            endTask() # end task if wanted/needed
            outcomeStart = timer.getTime()
            core.wait(oc)
            outcomeEnd = timer.getTime()
    elif (loc == 2 and response[0][0] == 'n'):
        if outcomeValue == gainRounded: # risky option on the right was chosen and won
            ocRiskyHide.setPos(hideLossLoc)
            nOpt.draw()
            gainTxt.draw()
            ocRiskyHide.draw()
            win.flip()
            endTask() # end task if wanted/needed
            outcomeStart = timer.getTime()
            core.wait(oc)
            outcomeEnd = timer.getTime()
        elif outcomeValue == lossRounded: # risky option on the right was chosen and lost
            ocRiskyHide.setPos(hideGainLoc)
            nOpt.draw()
            lossTxt.draw()
            ocRiskyHide.draw()
            win.flip()
            endTask() # end task if wanted/needed
            outcomeStart = timer.getTime()
            core.wait(oc)
            outcomeEnd = timer.getTime()
    elif (loc == 1 and response[0][0] == 'n') and outcomeValue == safeRounded: # safe option on the right was chosen
        nOpt.draw()
        safeTxt.draw()
        win.flip()
        endTask() # end task if wanted/needed
        outcomeStart = timer.getTime()
        core.wait(oc)
        outcomeEnd = timer.getTime()
    elif (loc == 2 and response[0][0] == 'v') and outcomeValue == safeRounded: # safe option on the left was chosen
        vOpt.draw()
        safeTxt.draw()
        win.flip()
        endTask() # end task if wanted/needed
        outcomeStart = timer.getTime()
        core.wait(oc)
        outcomeEnd = timer.getTime()
        
# setting up iti
def iti_waiting(): # each of the choice set's iti's are done differently - doesn't clearly work as well to do itiEnd
    fixTxt.draw()
    win.flip()
    
    endTask() # end task if wanted/needed

    
#
##
### PRACTICE CHOICE SET ###

# practice choice set instructions
pracStartTxt.draw()
win.flip()
pracInstructionsStart = timer.getTime()
if doET: # if doing eye-tracking, then start recording
    # put tracker in idle/offline mode before recording
    et.setOfflineMode()
    # start recording events
    et.startRecording(1, 0, 0, 0)
    # allocate some time for the tracker to cache some samples
    et.sendMessage('pre 100 pause')
    pylink.pumpDelay(100)
    # send message that recording has started
    et.sendMessage('ediRDM Pupillometry Recording Started - Practice Instructions Shown')
endTask()
response = event.waitKeys(keyList = ['v', 'n'], timeStamped = timer)
pracInstructionsEnd = response[0][1]
empty_data_appending()
ediData[data_appending_index()][17:19] = [pracInstructionsStart, pracInstructionsEnd]

# load task stimuli file 
practiceDF = pd.read_excel("ediRDMpractice.xlsx") # sequentially presented

# practice choice set task
for p in range(practiceSet):

    # Need to call in the practice file for the gain, loss, and safe text
    gain = practiceDF.riskyGain[p]
    loss = practiceDF.riskyLoss[p]
    safe = practiceDF.alternative[p]

    # Adjusting Trial Start - Python starts at 0: This makes trials start at 1
    trial = p + 1 

    # Round Choice Option Monetary Values to be Shown
    choice_value_rounding()

    # Randomize Choice Option Locations
    choice_location_randomizing()

    # Start of Trial
    decision_window_starting()

    # Counting Trials if Counting
    trial_counting()

    # Choice Made
    decision_making()
    
    # ISI
    isi_waiting() # technically in the practice file, but all isi = 1, so this was easier
    
    # Choice Outcome
    outcome_showing()
    
    # ITI
    iti = practiceDF.iti[p] # iti set in file to each choice option combination
    
    iti_waiting()
    
    itiStart = timer.getTime()
    core.wait(iti)
    itiEnd = timer.getTime()
    
    # Saving Data
    empty_data_appending()
    ediData[data_appending_index()][0] = trial 
    ediData[data_appending_index()][2:5] = [gainRounded, lossRounded, safeRounded] 
    ediData[data_appending_index()][7:17] = [choiceMade, choiceKey, outcomeValue, 
                                          loc, riskSplitLoc, gainTxtLoc, lossTxtLoc, safeTxtLoc, hideGainLoc, hideLossLoc]
    ediData[data_appending_index()][19:27] = [choiceStart, choiceEnd, 
                                           isiStart, isiEnd, 
                                           outcomeStart, outcomeEnd,
                                           itiStart, itiEnd]

#
##
### STATIC CHOICE SET ###

# static choice set instructions
statStartTxt.draw()
win.flip()
statInstructionsStart = timer.getTime()
endTask()
response = event.waitKeys(keyList = ['v', 'n'], timeStamped = timer)
statInstructionsEnd = response[0][1]
empty_data_appending()
ediData[data_appending_index()][17:19] = [statInstructionsStart, statInstructionsEnd]

# load task stimuli file 
staticDF = pd.read_csv("ediRDMstatic.csv") # create before I officially joined the lab - I never noticed before - I wonder why the practice is an excel while the static is a csv

# randomize trials 
staticRandTrial = staticDF.sample(frac = 1).reset_index(drop = True) # use pandas to take all the rows and randomize them

# preparation for grid search
riskygain_values = [] # for gain (riskyoption1)
riskyloss_values = [] # for loss (riskyoption2)
certain_values = [] # for safe (safeoption)
choices = [] # for choiceMade

# static choice set task
for s in range(staticSet):

    # Need to call in the practice file for the gain, loss, and safe text 
    gain = staticRandTrial.riskyoption1[s]
    loss = staticRandTrial.riskyoption2[s]
    safe = staticRandTrial.safeoption[s]

    # Adjusting Trial Start - Python starts at 0: This makes trials start at 1
    trial = s + 1 

    # Check Trial
    checkTrial = staticRandTrial.ischecktrial[s]

    # Round Choice Option Monetary Values to be Shown
    choice_value_rounding()

    # Randomize Choice Option Locations
    choice_location_randomizing()

    # Start of Trial
    decision_window_starting()

    # Counting Trials if Counting
    trial_counting()

    # End Task if Wanted/Needed
    endTask()

    # Choice Made
    decision_making()
    
    # ISI
    isi_waiting()
    
    # Choice Outcome
    outcome_showing()
    
    # ITI
    iti_waiting()
    
    itiStart = timer.getTime()
    core.wait(itiStatic[s])
    itiEnd = timer.getTime()
    
    # Saving Data
    empty_data_appending()
    ediData[data_appending_index()][0:2] = [trial, checkTrial] 
    ediData[data_appending_index()][2:5] = [gainRounded, lossRounded, safeRounded] 
    ediData[data_appending_index()][7:17] = [choiceMade, choiceKey, outcomeValue, 
                                          loc, riskSplitLoc, gainTxtLoc, lossTxtLoc, safeTxtLoc, hideGainLoc, hideLossLoc]
    ediData[data_appending_index()][19:27] = [choiceStart, choiceEnd, 
                                           isiStart, isiEnd, 
                                           outcomeStart, outcomeEnd,
                                           itiStart, itiEnd]
    
    # Grid Search Data
    riskygain_values.append(gain)
    riskyloss_values.append(loss)
    certain_values.append(safe)
    choices.append(choiceMade)
    

#
##
### GRID SEARCH ###

# Prepare choice set values to remove any nans
finiteGainVals = []
finiteLossVals = []
finiteSafeVals = []
finiteChoices = []

# just save trial things where participant responded
for t in range(len(choices)):
    if math.isfinite(choices[t]):
        finiteGainVals.append(riskygain_values[t])
        finiteLossVals.append(riskyloss_values[t])
        finiteSafeVals.append(certain_values[t])
        finiteChoices.append(choices[t])
        
# Prepare rho & mu values
n_rho_values = 200;
n_mu_values = 201;

rmin = 0.3
rmax = 2.2
rstep = (rmax - rmin)/(n_rho_values-1)

mmin = 7
mmax = 80
mstep = (mmax - mmin)/(n_mu_values-1)

rho_values = [];
mu_values = [];

for r in range(n_rho_values):
    rho_values += [rmin + r*rstep];

for m in range(n_mu_values):
    mu_values += [mmin + m*mstep];      

# Execute the grid search
best_nll_value = 1e10; # a preposterously bad first NLL

for r in range(n_rho_values):
    for m in range(n_mu_values):
        nll_new = pt_nll([rho_values[r], mu_values[m]], finiteGainVals, finiteLossVals, finiteSafeVals, finiteChoices);
        if nll_new < best_nll_value:
            best_nll_value = nll_new;
            bestR = r + 1; # "+1" corrects for diff. in python vs. R indexing
            bestM = m + 1; # "+1" corrects for diff. in python vs. R indexing

print('The best R index is', bestR, 'while the best M index is', bestM, ', with an NLL of', best_nll_value);

# getting dynamic choice set file
fname = []

#fname.append("../edi/ediTasks/day1_rdm_wmc/ediRDM/ediRDMdynamic/bespoke_choiceset_rhoInd%03i_muInd%03i.csv" % (bestR, bestM))
bespokeFilename = os.path.join(ediRDMdir, "ediRDMdynamic", "bespoke_choiceset_rhoInd%03i_muInd%03i.csv" % (bestR, bestM))
fname.append(bespokeFilename)
dynamicChoiceSetFilename = fname[0] # dyanmic choice set file to be used for participant

# saving out parameter data - "bestRho" & "bestMu"
empty_data_appending()
ediData[data_appending_index()][27:29] = [bestR, bestM]

# prepping for dynamic choice set instructions
fittingStartTxt.draw()
win.flip()
fitInstructionsStart = timer.getTime()
endTask()
core.wait(choiceWin) # added this waiting time - original would end grid search once it would run through everything
fitInstructionsEnd = timer.getTime()
empty_data_appending()
ediData[data_appending_index()][17:19] = [fitInstructionsStart, fitInstructionsEnd]

#
##
### DYNAMIC CHOICE SET ###

# dynamic choice set instructions
dynaStartTxt.draw()
win.flip()
dynaInstructionsStart = timer.getTime()
endTask()
response = event.waitKeys(keyList = ['v', 'n'], timeStamped = timer)
dynaInstructionsEnd = response[0][1]
empty_data_appending()
ediData[data_appending_index()][17:19] = [dynaInstructionsStart, dynaInstructionsEnd]

# load task stimuli file 
dynamicDF = pd.read_csv(dynamicChoiceSetFilename) # create before I officially joined the lab - I never noticed before - I wonder why the practice is an excel while the static is a csv

# randomize trials 
dynamicRandTrial = dynamicDF.sample(frac = 1).reset_index(drop = True) # use pandas to take all the rows and randomize them

# static choice set task
for d in range(dynamicSet):

    # Need to call in the practice file for the gain, loss, and safe text 
    gain = dynamicRandTrial.riskyoption1[d]
    loss = dynamicRandTrial.riskyoption2[d]
    safe = dynamicRandTrial.safeoption[d]

    # Adjusting Trial Start - Python starts at 0: This makes trials start at 1
    trial = s + 1 

    # Dynamic Choice Set Specific Data
    choiceP = dynamicRandTrial.choiceP[d]
    difficulty = dynamicRandTrial.easy0difficult1[d]

    # Round Choice Option Monetary Values to be Shown
    choice_value_rounding()

    # Randomize Choice Option Locations
    choice_location_randomizing()

    # Start of Trial
    decision_window_starting()

    # Counting Trials if Counting
    trial_counting()

    # End Task if Wanted/Needed
    endTask()

    # Choice Made
    decision_making()
    
    # ISI
    isi_waiting()
    
    # Choice Outcome
    outcome_showing()
    
    # ITI
    iti_waiting()
    
    itiStart = timer.getTime()
    core.wait(itiDynamic[d])
    itiEnd = timer.getTime()
    
    # Saving Data
    empty_data_appending()
    ediData[data_appending_index()][0] = trial 
    ediData[data_appending_index()][2:5] = [gainRounded, lossRounded, safeRounded] 
    ediData[data_appending_index()][5:7] = [choiceP, difficulty]
    ediData[data_appending_index()][7:17] = [choiceMade, choiceKey, outcomeValue, 
                                          loc, riskSplitLoc, gainTxtLoc, lossTxtLoc, safeTxtLoc, hideGainLoc, hideLossLoc]
    ediData[data_appending_index()][19:27] = [choiceStart, choiceEnd, 
                                           isiStart, isiEnd, 
                                           outcomeStart, outcomeEnd,
                                           itiStart, itiEnd]

#
##
### CLOSING INSTRUCTIONS ###

# dynamic choice set instructions
endStartTxt.draw()
win.flip()
endInstructionsStart = timer.getTime()
endTask()
response = event.waitKeys(keyList = ['return'], timeStamped = timer)
endInstructionsEnd = response[0][1]
empty_data_appending()
ediData[data_appending_index()][17:19] = [endInstructionsStart, endInstructionsEnd]

if doET:
    et.sendMessage('cgeRDM Recording Stopped')
    et.sendMessage('post 100 pause')
    pylink.pumpDelay(100)
    et.stopRecording()
    et.closeDataFile()
    time_str = time.strftime("_%Y_%m_%d_%H_%M_%S", time.localtime())
    session_identifier = edf_fname + time_str
    et.receiveDataFile(edf_fname + '.edf', os.path.join('/Users/Display/Desktop/Github/cge/CGE/RawData/' + session_identifier + '.edf'))
    et.close()

if doET:
    subprocess.run(["edf2asc.exe", os.path.join('Users/jvonm/Documents/GitHub/edi/ediTasks/day1_rdm_wmc/ediData/ediRDMpupillometry', session_identifier + '.edf')])



# for data files
date = time.strftime("%Y%m%d-%H%M%S")

win.close()

# saving out data
ediRDMdf = pd.DataFrame(ediData)
ediRDMbehavioralFilename = os.path.join(datadirPathbehavioral, f"edi{subID}_RDM_{date}.csv")
ediRDMdf.to_csv(ediRDMbehavioralFilename, header = False, index = False)






        




























































