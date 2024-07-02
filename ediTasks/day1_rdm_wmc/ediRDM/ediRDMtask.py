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

# psychopy 
from psychopy import visual, core, event, monitors
from psychopy.hardware import keyboard # do I need this?

kb = keyboard.Keyboard() # or this?

# task related
import time
import random
import pandas as pd
import numpy as np
import math
import statistics

#
##
### CHANGING DIRECTORIES ### 

# change directory to ediRDM folder with all the task-related files
print(os.getcwd()) # getting the current directory
homeDir = os.path.expanduser('~') # setting to home directory 
# ediTasksDir = os.path.join("Desktop", "edi", "ediTasks") # getting ediTasks directory
ediTasksDir1 = os.path.join("Documents", "GitHub", "edi", "ediTasks", "day1_rdm_wmc")
ediTasksDir2 = os.path.join("Desktop", "GitHub", "edi", "ediTasks")
os.chdir(os.path.join(homeDir, ediTasksDir1 or ediTasksDir2, "ediRDM")) # joining the pathways together and then changing the directory to it
print(os.getcwd())

ediTasksDir = os.path.join("Documents" or "Desktop", "GitHub", "edi", "ediTasks", "day1_rdm_wmc")
os.chdir(os.path.join(homeDir, ediTasksDir, "ediRDM")) # joining the pathways together and then changing the directory to it
print(os.getcwd())


#dirPath = os.chdir(dirName + os.sep + "ediRDM") # os.sep for mac and windows
#dirPath = os.chdir("Desktop" + os.sep + "edi" + os.sep + "ediTasks" + os.sep + "ediRDM") # test run on Von's laptop, tabletas

# set directory for saving ediRDMtask data
datadirPath = os.path.join(homeDir, ediTasksDir, "ediData")
datadirPathbehavioral = os.path.join(homeDir, datadirPath, "ediRDMbehavioral") 
datadirPathpupillometry = os.path.join(homeDir, datadirPath, "ediRDMpupillometry") 
#datadirPath = dataDirName + os.sep + "ediData"
#datadirPathbehavioral = dataDirName + os.sep + "ediData" + os.sep + "ediRDMbehavioral"
#datadirPathpupillometry = dataDirName + os.sep + "ediData" + os.sep + "ediRDMpupillometry"
#datadirPath = ("C:\\Users\\jvonm\\Desktop\\edi\\ediTasks\\ediData") # folder for all data - test run on Von's laptop, tabletas
#datadirPathbehavioral = ("C:\\Users\\jvonm\\Desktop\\edi\\ediTasks\\ediData\\ediRDMbehavioral") # rdm behavioral data - test run on Von's laptop, tabletas
#datadirPathpupillometry = ("C:\\Users\\jvonm\\Desktop\\edi\\ediTasks\\ediData\\ediRDMpupillometry") # rdm pupillometry data - test run on Von's laptop, tabletas

# load task files (only practice and static choice sets for now)
practice = pd.read_excel("ediRDMpractice.xlsx")
static = pd.read_csv("ediRDMstatic.csv") # create before I officially joined the lab - I never noticed before - I wonder why the practice is an excel while the static is a csv

#
##
### CREATING FUNCTIONS ###

# end the task
def endTask():
    end = event.getKeys(keyList = 'escape') # kb. vs. event.
    
    if 'escape' in end:
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

loc = []
# randomize choice option locations
def choice_location():
    loc = random.choice([1,2]) # Would I be able to call this earlier so it doesn't have to be part of every choice set?

    if loc == 1:
        riskSplitLoc = [-.35,0] # risky option is on the left = v
    else:
        riskSplitLoc = [.35,0] # risky option is on the right = n
    
    if loc == 1:
        gainTxtLoc = [-.35,.1]
        lossTxtLoc = [-.35,-.1]
        safeTxtLoc = [.35,0]
        hideGainLoc = [-.35, -.15]
        hideLossLoc = [-.35, .15]
    else:
        gainTxtLoc = [.35,.1]
        lossTxtLoc = [.35,-.1]
        safeTxtLoc = [-.35,0]
        hideGainLoc = [-.35, .15]
        hideLossLoc = [-.35, -.15]

    # Set the Position of the Decision Window Stimuli
    riskSplit.setPos(riskSplitLoc)
    gainTxt.setPos(gainTxtLoc)
    lossTxt.setPos(lossTxtLoc)
    safeTxt.setPos(safeTxtLoc)

    return(loc, gainTxtLoc, lossTxtLoc, safeTxtLoc, hideGainLoc, hideLossLoc)

# count trials
def counting_trials():
    if isCount == 1:
        countTrialTxt.setText(trialNumber)
        countTrialTxt.draw()

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
hideSize = [.6,.3]

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
    practiceSet = 5
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
    countLoc = [0, -.35]
    countColor = c1
elif isCount == 1:
    countLoc = [0, 7]
    countColor = c2

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
### STIMULI SETUP ###

# ~ Instructions ~

# general instructions
RDMstartTxt = visual.TextStim(
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
vChoice = kb # I don't think I need this - this works better with PsychoPy Handlers e.g., nChoice.keys or nChoice.rt

nTxt = visual.TextStim(
    win, 
    text = "N - Left",
    font = a,
    height = choiceTextH,
    pos = nLoc,
    color = c2);
nChoice = kb # I don't think I need this - this works better with PsychoPy Handlers

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

riskyGainHide = visual.Rect(
    win,
    size = choiceSize, 
    fillColor = c1, 
    lineColor = c1,
)

riskyLossHide = visual.Rect(
    win,
    size = choiceSize, 
    fillColor = c1, 
    lineColor = c1,
)

#
##
### TIMER ###
timer = core.Clock()

#
##
### SETTING EDI DATA STRUCTURE ###
ediData = []
ediData.append(
    [
        "trialNumber", # should incrementally increase by 1
        "checkTrial", # should be "0" for no, not a check trial or "1" for yes, a check trial
        "gainValue",
        "lossValue", # should always be $0 (except if a check trial???)
        "safeValue",
        "choiceMade", # should be "v" or "n"
        "outcomeValue", # if a choice was made, the value should match the value location and key response of choiceMade
        "instructionStart", # first point should be ground 0 for when the task starts # second point should be ground 0 for eye-tracking # last should be for closing instructions
        "instructionEnd",
        "choiceStart", # should be the same time as when the choice values and texts are shown - their Start (choices are shown)
        "choiceEnd", # should be the same time as when the choice values and texts are disappear - their End (choice is made)
        "vCircleStart",
        "vCircleEnd",
        "nCircleStart",
        "nCircleEnd",
        "splitStart",
        "splitEnd",
        "gainStart",
        "gainEnd",
        "lossStart",
        "lossEnd",
        "safeStart",
        "safeEnd",
        "orStart",
        "orEnd",
        "vStart",
        "vEnd",
        "nStart",
        "nEnd",
        "isiStart", # should be just after or exactly at the moment of choiceEND
        "isiEnd", # should be 1 sec
        "outcomeStart", # should be just after or exactly at the moment of isiEND
        "outcomeEnd", # should be 1 sec
        "itiStart", # should be just after or exactly at the moment of outcomeEND
        "itiEnd" # should be either 3 or 3.5 sec
    ]
)

#
##
### GENERAL INSTRUCTIONS START ### 
RDMstartTxt.draw()
win.flip()
genInstructionsStart = timer.getTime()
response = event.waitKeys(keyList = ['return'], timeStamped = timer)
genInstructionsEnd = response[0][1]

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
response = event.waitKeys(keyList = ['v', 'n'], timeStamped = timer)
pracInstructionsEnd = response[0][1]


# practice choice set task
for p in range(practiceSet):

    # Adjusting trial start - Python starts at 0: This makes trials start at 1
    trialNumber = p + 1 

    # Need to call in the practice file for the gain, loss, and safe text
    gain = practice.riskyGain[p]
    loss = practice.riskyLoss[p]
    safe = practice.alternative[p]

    # Round Choice Option Monetary Values
    gainRounded = '$%.2f' % round(gain,2)
    lossRounded = '$%.0f' % round(loss,0)
    safeRounded = '$%.2f' % round(safe,2)

    # Set the Monetary Values to be Shown
    gainTxt.setText(gainRounded)
    lossTxt.setText(lossRounded)
    safeTxt.setText(safeRounded)

    # Randomize Choice Option Locations
    choice_location()

    # Start of Trial
    # ~ draw choice options
    vOpt.draw()
    nOpt.draw()
    riskSplit.draw()
    gainTxt.draw()
    lossTxt.draw()
    safeTxt.draw()
    orTxt.draw()
    vTxt.draw()
    nTxt.draw()
    # ~ show choice options
    win.flip() 
    # ~ get times 
    vOptStart = timer.getTime() 
    nOptStart = timer.getTime()
    riskSplitStart = timer.getTime()
    gainTxtStart = timer.getTime()
    lossTxtStart = timer.getTime()
    safeTxtStart = timer.getTime()
    orTxtStart = timer.getTime()
    vTxtStart = timer.getTime()
    nTxtStart = timer.getTime()
    choiceStart = timer.getTime()
    
    # End Task if Needed
    endTask()

    # Choice Made
    response = event.waitKeys(maxWait = choiceWin, keyList = ['v', 'n'], timeStamped = timer)

    if response is None:
        choiceMade = math.nan
        outcomeValue = math.nan
        choiceEnd = math.nan
    elif response[0][0] == 'v' or response[0][0] == 'n': # ['v'] or ['n'] in response[0][0] # 
        if loc == 1 and response == 'v' or loc == 2 and response == 'n':
            choiceMade = 1 # chose the risky option 
            outcomeValue = random.choice([gainTxt, lossTxt]) # randomly chooses the gain or loss
        elif (loc == 1 and response[0][0] == 'n') or (loc == 2 and response[0][0] == 'v'):
            choiceMade = 0 # chose the safe option
            outcomeValue = safeTxt
        choiceEnd = response[0][1]
        
win.close()








    # Choice Outcome
    if outcomeValue == math.nan:
        noRespTxt.draw()
        win.flip()
        outcomeStart = timer.getTime()
        core.wait(oc) # outcomeEnd = core.wait(oc) # ediData.append
    elif outcomeValue == gainTxt:
        riskyGainHide.setPos = hideGainLoc
    elif outcomeValue == lossTxt:
        riskyLossHide.setPos = hideLossLoc
    elif outcomeValue == safeTxt:
        hideGain = riskyGainHide   

        



    # Choice Outcome
    if not pracChoiceResp.keys:
        outcome = math.nan
        noRespLoc = [0,0]
        ocLoc = [5,5]
        ocGambleLoc = [5,5]
        ocSafeLoc = [5,5]
        hideGamLoc = [5,5]
    elif pracChoiceResp.keys == 'v' and loc == 1:
        outcome = random.choice([riskyGain, riskyLoss])
        if outcome == riskyGain:
            ocLoc = [-.35,.1]
            ocGambleLoc = [-.35,0]
            ocSafeLoc = [5,5]
            noRespLoc = [5,5]
            hideGamLoc = [-.35,-.15] #.125
        elif outcome == riskyLoss:
            ocLoc = [-.35,-.1]
            ocGambleLoc = [-.35,0]
            ocSafeLoc = [5,5]
            hideGamLoc = [-.35,.15]
            noRespLoc = [5,5]
    elif pracChoiceResp.keys == 'v' and loc == 2:
        outcome = alternative
        ocLoc = [-.35,0]
        ocGambleLoc = [5,5]
        ocSafeLoc = ocLoc
        hideGamLoc = ocGambleLoc
        noRespLoc = [5,5]
    elif pracChoiceResp.keys == 'n' and loc ==1:
        outcome = alternative
        ocLoc = [.35,0]
        ocGambleLoc = [5,5]
        ocSafeLoc = ocLoc
        hideGamLoc = ocGambleLoc
        noRespLoc = [5,5]
    elif pracChoiceResp.keys == 'n' and loc ==2:
        outcome = random.choice([riskyGain, riskyLoss])
        if outcome == riskyGain:
            ocLoc = [.35,.1]
            ocGambleLoc = [.35,0]
            ocSafeLoc = [5,5]
            hideGamLoc = [.35,-.15]
            noRespLoc = [5,5]
        elif outcome == riskyLoss:
            ocLoc = [.35,-.1]
            ocGambleLoc = [.35,0]
            ocSafeLoc = [5,5]
            hideGamLoc = [.35,.15]
            noRespLoc = [5,5]
    
    if outcome == riskyLoss:
        pracFeedbackRounded = "$%.0f" % round(outcome,0)
    else:
        pracFeedbackRounded = "$%.2f" % round(outcome,2)

    # ISI
    fittingStartTxt.draw()
    wait.flip(isi)
    
        
    pracNoRespTxt.setPos(noRespLoc)
    pracRiskOC.setPos(ocGambleLoc)
    pracSafeOC.setPos(ocSafeLoc)
    pracOCtxt.setColor(color1, colorSpace='rgb')
    pracOCtxt.setPos(ocLoc)
    pracOCtxt.setText(pracFeedbackRounded)
    pracHideRisk.setPos(hideGamLoc)

    































































statStartTxt.draw()
win.flip()
event.waitKeys(keyList = ['v', 'n'])

### Begin Practice Choice Set ###
for s in range(staticSet):

    # Need to call in the practice file for the gain, loss, and safe text
    riskyoption1 = static.riskyoption1[p]
    riskyoption2 = static.riskyoption2[p]
    safeoption = static.safeoption[p]

    # Round Choice Option Monetary Values
    gainRounded = '$%.2f' % round(riskyoption1,2)
    lossRounded = '$%.0f' % round(riskyoption2,0)
    safeRounded = '$%.2f' % round(safeoption,2)

    gainTxt.setText(gainRounded)
    lossTxt.setText(lossRounded)
    safeTxt.setText(safeRounded)

    # Randomize Choice Option Locations
    loc = random.choice([1,2]) # Would I be able to call this earlier so it doesn't have to be part of every choice set?

    if loc == 1:
        riskSplitLoc = [-.35,0] #targetPos
    else:
        riskSplitLoc = [.35,0] #targetPos
    
    if loc == 1:
        gainTxtLoc = [-.35,.1]
        lossTxtLoc = [-.35,-.1]
        safeTxtLoc = [.35,0]
    else:
        gainTxtLoc = [.35,.1]
        lossTxtLoc = [.35,-.1]
        safeTxtLoc = [-.35,0]

    riskSplit.setPos(riskSplitLoc)
    gainTxt.setPos(gainTxtLoc)
    lossTxt.setPos(lossTxtLoc)
    safeTxt.setPos(safeTxtLoc)

    # Draw Choice Options
    vOpt.draw()
    nOpt.draw()
    riskSplit.draw()
    gainTxt.draw()
    lossTxt.draw()
    safeTxt.draw()
    orTxt.draw()
    vTxt.draw()
    nTxt.draw()
    win.flip()

    response = event.waitKeys(maxWait = choiceWin, keyList = ['v', 'n'])

    win.flip()

# Prepare choice set values to remove any nans
finiteChoices = []
finiteGainVals = []
finiteSafeVals = []
finiteLossVals = []

# just save trial things where participant responded
for t in range(len(choices)):
    if math.isfinite(choices[t]):
        finiteChoices.append(choices[t])
        finiteGainVals.append(riskygain_values[t])
        finiteSafeVals.append(certain_values[t])
        finiteLossVals.append(riskyloss_values[t])


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
        nll_new = negLLprospect_cgt([rho_values[r], mu_values[m]], finiteGainVals, finiteLossVals, finiteSafeVals, finiteChoices);
        if nll_new < best_nll_value:
            best_nll_value = nll_new;
            bestR = r + 1; # "+1" corrects for diff. in python vs. R indexing
            bestM = m + 1; # "+1" corrects for diff. in python vs. R indexing

print('The best R index is', bestR, 'while the best M index is', bestM, ', with an NLL of', best_nll_value);

fname.append("../CGE/bespoke_choicesets/bespoke_choiceset_rhoInd%03i_muInd%03i.csv" % (bestR, bestM))
dynamicChoiceSetFilename = fname[0]

# Set experiment start values for variable component dynamicChoiceSet
dynamicChoiceSet = ''
dynamicChoiceSetContainer = []
# Set experiment start values for variable component bestRho
bestRho = ''
bestRhoContainer = []
# Set experiment start values for variable component bestMu
bestMu = ''
bestMuContainer = []


fittingStartTxt.draw()
win.flip()

dynaStartTxt.draw()
win.flip()
event.waitKeys(keyList = ['v', 'n'])


# stop recording; add 100 msec to catch final events before stopping
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