#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul  6 16:06:44 2022

@author: hayley
"""

"""
operation span task for HRB dissertation project: Risk, context and strategy.

For this study, we are running the shortened version (Foster et al 2014) of both the operation span and symmetry span tasks.
We are doing one block for each. 

The structure of the complex span tasks are very similar: instructions, practice letters/red square only, practice distractors only, the practice both.
 
"""
    
def ospanTask(subID, isReal,dirName, dataDirName):

    #subID = '001' # for testing
    try:
    
        # Import modules we need
        import os, random, time
        import pandas as pd
        import numpy as np
        from psychopy import visual, core, event, monitors
        import statistics
        #import numpy as np
        
        # change directory
        #os.chdir('/Users/shlab/Documents/GitHub/rcs/task/ospan')
        os.chdir(dirName + os.sep + "ospan")
        
        
        #dataDirectoryPath = dirName + 'data/'
        #dataDirectoryPath = '/Users/shlab/Documents/Github/rcs/task/data/'
        dataDirectoryPath = dataDirName + "ospanData" + os.sep

        
        # import files
        # practiceOperations = pd.read_excel('/Users/shlab/Documents/GitHub/rcs/task/ospan/practiceOperations.xlsx')
        # practiceOperations2 = pd.read_excel('/Users/shlab/Documents/GitHub/rcs/task/ospan/practiceOperations2.xlsx')
        # operationSet1 = pd.read_excel('/Users/shlab/Documents/GitHub/rcs/task/ospan/operationSet1.xlsx')
        # operationSet2 = pd.read_excel('/Users/shlab/Documents/GitHub/rcs/task/ospan/operationSet2.xlsx')
        
        print(dirName + os.sep + 'ospan' + os.sep + 'practiceOperations.xlsx')
        practiceOperations = pd.read_excel(dirName + os.sep + 'ospan' + os.sep + 'practiceOperations.xlsx')
        practiceOperations2 = pd.read_excel(dirName + os.sep + 'ospan' + os.sep + 'practiceOperations2.xlsx')
        operationSet1 = pd.read_excel(dirName + os.sep + 'ospan' + os.sep + 'operationSet1.xlsx')
        operationSet2 = pd.read_excel(dirName + os.sep + 'ospan' + os.sep + 'operationSet2.xlsx')
        
        operationSet1.columns = ["weight", "problem", "Sum1", "difficulty"]# fix column names
        operationSet1 = operationSet1[operationSet1['weight'] ==1] # removing operations we wont use
        
        # Screen dimensions and drawing stuff
        #scrnsize= [800,800] #how large the screen will be
        scrnsize=[1280,1024] # CORRECT DIMENSIONS FOR REAL TASK
        #scrnsize = [1024,819.2] # 80% of correct size for mac laptop
        center = [0,100]
        centerR = [scrnsize[0]/4,100]
        centerL = [scrnsize[0]/-4,100]
        radius = scrnsize[0]/5.5
        rectHeight = radius +2 #rectangle used to cover up half the circle when outcome is gain or loss
        rectWidth = radius*2+2
        #textHeight = radius/2.1
        textHeight = 40
        wrap = scrnsize[0]*.9 # text wrapping
        boxLetterSize = 125; # letter box size for recall screen
        
        if isReal == 1:
            nTmathPrac = 15
            nTletterPractice = 4
            nTbothPractice = 3
            nTbothReal = 5
        else: 
            nTmathPrac = 3
            nTletterPractice = 2
            nTbothPractice = 3
            nTbothReal = 5
        
        
        # Set up the window
        win = visual.Window(
            size=scrnsize,
            units="pix",
            fullscr=True,
            color=[-1, -1, -1], #black screen
            screen=1 # on second screen
        )
        
        
        ## INSTRUCTIONS STIMULI
        
        forcedInstrWaitTime = 1.5 # participants can't moved forward during instructions until 1.5s have passed

        # screen count for instructions
        instructCount = visual.TextStim(
            win,
            pos =[scrnsize[1]*.95,-360],
            color=[1,1,1],
            height=textHeight/2,
            wrapWidth=wrap,
            alignText="left"
        ) 
            
        generalInstructionsPg1 = visual.TextStim(
            win,
            text= "In this task you will try to memorize letters you see on the screen while you also solve simple math problems. \n\nIn the next few minutes, you will have some practice to get you familiar with how the experiment works. \n\nWe will begin by practicing the letter part of the experiment. \n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight, 
            wrapWidth=wrap,
            alignText="left"
        )
        
        generalInstructionsPg2 = visual.TextStim(
            win,
            text= "For this practice set, letters will appear on the screen one at a time. \n\nTry to remember each letter in the order presented. \n\nAfter 2-3 letters have been shown, you will see a screen listing 12 possible letters. \n\nYour job is to select each letter in the order presented. \n\nTo do this, use the mouse to select the box for each letter. \n\nThe letters you select will appear at the bottom of the screen. \n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        generalInstructionsPg3 = visual.TextStim(
            win,
            text = "When you have selected all the letters, and they are in the correct order, hit the ENTER box at the bottom right of the screen. \n\nIf you make a mistake, hit the CLEAR box to start over. \n\nIf you forget one of the letters, click the BLANK box to mark the spot for the missing letter.\n\n\nPress'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        generalInstructionsPg4 = visual.TextStim(
            win,
            text = "It is very important to get the letters in the same order as you see them. \n\nIf you forget one, use the BLANK box to mark the position. \n\nPlease ask the experimenter any questions you may have at this time. \n\n\nWhen you're ready, press 'enter' to start the letter practice.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        
        mathInstructionsPg1 = visual.TextStim(
            win,
            text= "Now you will practice doing the math part of the experiment.\n\nA math problem will appear on the screen, like this: \n\n(2 x 1) + 1 = ? \n\nAs soon as you see the math problem, you should compute the correct answer. \n\nIn the above problem, the answer 3 is correct. \n\nWhen you know the correct answer, you will click the mouse button.\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        mathInstructionsPg2 = visual.TextStim(
            win,
            text= "You will see a number displayed on the next screen, along with a box marked TRUE and a box marked FALSE. \n\nIf the number on the screen is the correct answer to the math problem, click on the TRUE box with the mouse. \n\nIf the number is not the correct answer, click on the FALSE box. \n\nFor example, if you see the problem (2 x 2) + 1 = ? and the number on the following screen is 5 click the TRUE box, because the answer is correct. \n\nPress 'enter' to continue",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        mathInstructionsPg3 = visual.TextStim(
            win,
            text= "If you see the problem (2 x 2) + 1 = ? and the number on the next screen is 6 click the FALSE box, because the correct answer is 5, not 6. \n\nAfter you click on one of the boxes, the computer will tell you if you made the right choice. \n\nPress 'enter' to continue",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        mathInstructionsPg4 = visual.TextStim(
            win,
            text= "It is VERY important that you get the math problems correct. \n\nIt is also important that you try and solve the problem as quickly as you can. \n\nPlease ask the experimenter any questions you may have at this time. \n\n\nWhen you're ready, press 'enter' to try some practice problems.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        
        mathInstructionsReDoPg1 = visual.TextStim(
            win, 
            text = "You did not have enough correct math problems. \n\nThere will be one more round of the practice math. \n\nIt is important that you get the math problems correct and solve them as quickly as you can. \n\nPlease ask the experimenter any questions you have now. \n\n\nWhen you're ready, press 'enter' to try some practice problems.",
            pos= center,
            color="white", 
            height = textHeight,
            wrapWidth = wrap,
            alignText="left"
        )
        
        mathInstructionsEndofTask = visual.TextStim(
            win, 
            text = "You did not have enough correct math problems on the second round of the math practice. \n\nThis part of the experiment is complete and you will now continue to the next and final task. \n\nThank you for your time.",
            pos= center,
            color="white", 
            height = textHeight,
            wrapWidth = wrap,
            alignText="left"
        )
        
        
        
        letterMathPractInstructionsPg1= visual.TextStim(
            win,
            text= "Now you will practice doing both parts of the experiment at the same time. \n\nIn the next practice set, you will be given one of the math problems. \n\nOnce you make your decision about the math problem, a letter will appear on the screen.  \n\nTry and remember the letter.\n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        
        letterMathPractInstructionsPg2= visual.TextStim(
            win,
            text= "In the previous section where you only solved math problems, the computer computed your average time to solve the problems.\n\nIf you take longer than your average time, the computer will automatically move you onto the letter part, thus skipping the True or False part and will count that problem as a math error. \n\nTherefore it is VERY important to solve the problems as quickly and as accurately as possible. After the letter goes away, another math problem will appear, and then another letter until the trial is complete.\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        
        letterMathPractInstructionsPg3= visual.TextStim(
            win,
            text= "At the end of each set of letters and math problems, a recall screen will appear.\n\nUse the mouse to select the letters you just saw.\n\nTry your best to get the letters in the correct order.\n\nIt is important to work QUICKLY and ACCURATELY on the math.\n\nMake sure you know the answer to the math problem before clicking to the next screen. \n\nYou will not be told if your answer to the math problem is correct. \n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
                
        letterMathPractInstructionsPg4= visual.TextStim(
            win,
            text= "After the recall screen, you will be given feedback about your performance regarding both the number of letters recalled and the percent correct on the math problems. \n\nPlease ask the experimenter any questions you may have at this time.\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        
        letterMathPractInstructionsPg5= visual.TextStim(
            win,
            text= "During the feedback, you will see a number in red in the top right of the screen. \n\nThis indicates your percent correct for the math problems for the entire experiment. \n\nIt is VERY important for you to keep this at least at 85%. \n\nWe can only use data where the participant was at least 85% accurate on the math. \n\nYou must perform at least at 85% on the math problems WHILE doing your best to recall as many letters as possible. \n\nPlease ask the experimenter any questions you may have at this time. \n\nPress 'enter'' to try some practice problems.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        # letterMathPractInstructionsPg6= visual.TextStim(
        #     win,
        #     text= "During the feedback, you will see a number in red in the top right of the screen. \n\nThis indicates your percent correct for the math problems for the entire experiment. \n\nIt is VERY important for you to keep this at least at 85%. \n\nFor our purposes, we can only use data where the participant was at least 85% accurate on the math.  \n\nPlease ask the experimenter any questions you may have at this time. \n\nClick 'enter'' to try some practice problems.",
        #     pos = center,
        #     color="white",
        #     height = textHeight,
        #     wrapWidth=wrap,
        #     alignText="left"
        # )
        
        realTaskInstructionsPg1= visual.TextStim(
            win,
            text = "That is the end of the practice. \n\nThe real trials will look like the practice trials you just completed. \n\nFirst you will get a math problem to solve, then a letter to remember. When you see the recall screen, select the letters in the order presented. \n\nIf you forget a letter, click the BLANK box to mark where it should go. \n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        realTaskInstructionsPg2 = visual.TextStim(
            win,
            text = "Some sets will have more math problems and letters than others. \n\nIt is important that you do your best on both the math problems and the letter recall parts of this experiment. \n\nRemember on the math you must work as QUICKLY and ACCURATELY as possible. \n\nAlso, remember to keep your math accuracy at 85% or above. The experimenter will now leave the room. \n\n\nWhen you're ready to begin the task, press 'enter'.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        endOfTask = visual.TextStim(
            win,
            text = "This part of the study is complete.\n\n\nPress the white call button to get the experimenter.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        
        ## LETTER STIMULI
        
        
        letterList = ["F", "H", "J", "K", "L", "N", "P", "Q", "R", "S", "T", "Y"]
        
        letterDisplay = visual.TextStim(
            win,
            pos = [0,0],
            color="white",
            height=boxLetterSize
        )
        
        
        blankScreen = visual.TextStim(
            win,
            pos = [0,0],
            color="white",
            height=textHeight,
            text=" "
        )
        
        fixationScreen = visual.TextStim(
            win,
            pos = [0,0],
            color="white",
            height=textHeight,
            text="+"
        )
        
        
        # grid of letters for participants to respond
        # Row 1: F, H, J 
        # Row 2: K, L, N 
        # Row 3: P, Q, R 
        # Row 4: S, T, Y
        
        # define location for each column of the letter grid 
        COL1horiz = scrnsize[0]*-.35
        COL2horiz = scrnsize[0]*-.15
        COL3horiz = scrnsize[0]*.05
        ROW1vert = scrnsize[1]*.27
        ROW2vert = scrnsize[1]*.1
        ROW3vert = scrnsize[1]*-.07
        ROW4vert = scrnsize[1]*-.24
        
        
        
        F_r1c1 = visual.TextStim(
            win, 
            text='F', 
            pos = [COL1horiz,ROW1vert],
            color="black", 
            height=boxLetterSize
        )
        
        Fbox = visual.Rect(
            win, 
            name='F',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=[COL1horiz,ROW1vert], 
            fillColor="white" #white
        )
        
        H_r1c2 = visual.TextStim(
            win, 
            text='H', 
            pos = [COL2horiz,ROW1vert],
            color="black", 
            height=boxLetterSize
        )
        
        Hbox = visual.Rect(
            win, 
            name='H',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=H_r1c2.pos, 
            fillColor=[1,1,1] #white
        )
        
        J_r1c3 = visual.TextStim(
            win, 
            text='J', 
            pos = [COL3horiz,ROW1vert],
            color="black", 
            height=boxLetterSize
        )
        
        Jbox = visual.Rect(
            win, 
            name='J',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=J_r1c3.pos, 
            fillColor=[1,1,1] #white
        )
        
        
        K_r2c1 = visual.TextStim(
            win, 
            text='K', 
            pos = [COL1horiz,ROW2vert],
            color="black", 
            height=boxLetterSize
        )
        
        Kbox = visual.Rect(
            win, 
            name='K',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=K_r2c1.pos, 
            fillColor=[1,1,1] #white
        )
        
        
        L_r2c2 = visual.TextStim(
            win, 
            text='L', 
            pos = [COL2horiz,ROW2vert],
            color="black", 
            height=boxLetterSize
        )
        
        Lbox = visual.Rect(
            win, 
            name = 'L',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=L_r2c2.pos, 
            fillColor=[1,1,1] #white
        )
        
        
        N_r2c3 = visual.TextStim(
            win, 
            text='N', 
            pos = [COL3horiz,ROW2vert],
            color="black", 
            height=boxLetterSize
        )
        
        Nbox = visual.Rect(
            win, 
            name='N',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=N_r2c3.pos, 
            fillColor=[1,1,1] #white
        )
        
        P_r3c1 = visual.TextStim(
            win, 
            text='P', 
            pos = [COL1horiz,ROW3vert],
            color="black", 
            height=boxLetterSize
        )
        
        Pbox = visual.Rect(
            win, 
            name='P',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=P_r3c1.pos, 
            fillColor=[1,1,1] #white
        )
        
        
        Q_r3c2 = visual.TextStim(
            win, 
            text='Q', 
            pos = [COL2horiz,ROW3vert],
            color="black", 
            height=boxLetterSize
        )
        
        Qbox = visual.Rect(
            win, 
            name='Q',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=Q_r3c2.pos, 
            fillColor=[1,1,1] #white
        )
        
        R_r3c3 = visual.TextStim(
            win, 
            text='R', 
            pos = [COL3horiz,ROW3vert],
            color="black", 
            height=boxLetterSize
        )
        
        Rbox = visual.Rect(
            win, 
            name='R',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=R_r3c3.pos, 
            fillColor=[1,1,1] #white
        )
        
        S_r4c1 = visual.TextStim(
            win, 
            text='S', 
            pos = [COL1horiz,ROW4vert],
            color="black", 
            height=boxLetterSize
        )
        
        Sbox = visual.Rect(
            win, 
            name='S',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=S_r4c1.pos, 
            fillColor=[1,1,1] #white
        )
        
        T_r4c2 = visual.TextStim(
            win, 
            text='T', 
            pos = [COL2horiz,ROW4vert],
            color="black", 
            height=boxLetterSize
        )
        
        Tbox = visual.Rect(
            win, 
            name='T',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=T_r4c2.pos, 
            fillColor=[1,1,1] #white
        )
        
        Y_r4c3 = visual.TextStim(
            win, 
            text='Y', 
            pos = [COL3horiz,ROW4vert],
            color="black", 
            height=boxLetterSize
        )
        
        Ybox = visual.Rect(
            win, 
            name='Y',
            width=boxLetterSize, 
            height=boxLetterSize, 
            units='pix', 
            pos=Y_r4c3.pos, 
            fillColor=[1,1,1] #white
        )
        
        blankButton = visual.TextStim(
            win, 
            text='BLANK', 
            pos = [scrnsize[0]*.3,scrnsize[1]*.25],
            color="black", 
            height=70
        )
        
        blankButtonBox = visual.Rect(
            win, 
            name='*',
            width=boxLetterSize*3, 
            height=boxLetterSize*1.5, 
            units='pix', 
            pos=blankButton.pos, 
            fillColor=[1,1,1] #white
        )
        
        
        clearButton = visual.TextStim(
            win, 
            text='CLEAR', 
            pos = [scrnsize[0]*.3,scrnsize[1]*.025],
            color="black", 
            height=70
        )
        
        clearButtonBox = visual.Rect(
            win, 
            width=boxLetterSize*3, 
            height=boxLetterSize*1.5, 
            units='pix', 
            pos=clearButton.pos, 
            fillColor="red" #white
        )
        
        
        enterButton = visual.TextStim(
            win, 
            text='ENTER', 
            pos = [scrnsize[0]*.3,scrnsize[1]*-.2],
            color="black", 
            height=70
        )
        
        enterButtonBox = visual.Rect(
            win, 
            width=boxLetterSize*3, 
            height=boxLetterSize*1.5, 
            units='pix', 
            pos=enterButton.pos, 
            fillColor=[0,.6,0] #white
        )
        
        
        showLetterResponse = visual.TextStim(
            win, 
            pos = [COL2horiz +100, ROW4vert -100],
            color="blue", 
            height=textHeight*1.5,
            wrapWidth=wrap,
            #alignText="left"
        )
        
        
        letterRecallText = visual.TextStim(
            win,
            pos = [0,scrnsize[1]*.4],
            color="white",
            height=textHeight,
            text="Select the letters in the order presented. \nUse the blank button to fill in forgotten letters.",
            wrapWidth=wrap
        )
        
        letterFeedbackText = visual.TextStim(
            win,
            pos = [0,0],
            color="white",
            height = textHeight,
            wrapWidth = wrap
        )
        
        ## OPERATION STIMULI
        mathText = visual.TextStim(
            win, 
            #pos = [0,scrnsize[1]*.2],
            pos = [0,0],
            color="white",
            height = boxLetterSize,
            wrapWidth = wrap
        )
        
        mathClickEnter = visual.TextStim(
            win, 
            pos = [0,scrnsize[1]*-.4],
            color="white",
            height = textHeight,
            text= "When you have solved the math problem, click the mouse to continue.",
            wrapWidth=wrap
        )
        
        mathTrueButton = visual.TextStim(
            win, 
            text='TRUE', 
            pos = [scrnsize[0]*-.3,scrnsize[1]*-.2],
            color="black", 
            height=70
        )
        
        mathTrueBox = visual.Rect(
            win, 
            width=boxLetterSize*3, 
            height=boxLetterSize*1.5, 
            units='pix', 
            pos=mathTrueButton.pos, 
            fillColor="white",
            name = "True"
        )
        
        mathFalseButton= visual.TextStim(
            win, 
            text='FALSE', 
            pos = [scrnsize[0]*.3,scrnsize[1]*-.2],
            color="black", 
            height=70
        )
        mathFalseBox = visual.Rect(
            win, 
            width=boxLetterSize*3, 
            height=boxLetterSize*1.5, 
            units='pix', 
            pos=mathFalseButton.pos, 
            fillColor="white",
            name="False"
        )
        
        mathPracFeedback = visual.TextStim(
            win,
            pos = [0,scrnsize[1]*-.4],
            color="green",
            height = textHeight
        )
        
        mathSuggestedAns = visual.TextStim(
            win,
            #pos = [0,scrnsize[1]*.2],
            pos =[0,0],
            color="white",
            height = boxLetterSize
        )
        
        mathErrorsAfterRecall = visual.TextStim(
            win, 
            pos = [0,scrnsize[1]*-.2], 
            color="white",
            height = textHeight, 
            wrapWidth = wrap
        )
        
        mathTotalPercentCorrect = visual.TextStim(
            win,
            pos = [scrnsize[0]*.4, scrnsize[1]*.4],
            color="red",
            height = 90
            #height = boxLetterSize
        )
        
        # Start letter practice
        
        # INSTRUCTIONS
        
        generalInstructionsPg1.draw()
        instructCount.text=text="screen 1/4"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        generalInstructionsPg1.draw()
        instructCount.text=text="screen 1/4"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        generalInstructionsPg2.draw()
        instructCount.text=text="screen 2/4"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)
        
        generalInstructionsPg2.draw()
        instructCount.text=text="screen 2/4"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        generalInstructionsPg3.draw()
        instructCount.text=text="screen 3/4"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)
        
        generalInstructionsPg3.draw()
        instructCount.text=text="screen 3/4"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        generalInstructionsPg4.draw()
        instructCount.text=text="screen 4/4"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)
        
        generalInstructionsPg4.draw()
        instructCount.text=text="screen 4/4"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        blankScreen.draw()
        win.flip()
        core.wait(1)
        
        ## LETTER PRACTICE
        # 4 trials, set sizes = 2,2,3,3 (order random across participants)
        #nTletterPractice = 4
        setSizeLetterPrac = [2,2,3,3]
        random.shuffle(setSizeLetterPrac)
        #lettersRecall = []
        #lettersShown = []
        
        letterPracticeData = [] # create data structure with column names
        letterPracticeData.append(
            [
                "setSize", 
                "lettersShown",
                "response",
                "responseCorrect",
                "trial"
            ]
        )
        
        #for s in range(len(setSizeLetterPrac)):
        for s in range(nTletterPractice):
            
            tmpLettersShown = random.sample(letterList, setSizeLetterPrac[s]) # select letters to show
            
            for t in range(setSizeLetterPrac[s]):
                
                
                # show the letters
                letterDisplay.text = tmpLettersShown[t]
                letterDisplay.draw()
                win.flip()
                core.wait(1) # 1s letter display
                
                if not t == setSizeLetterPrac[s]-1: # dont show isi after the last letter is shown
                    fixationScreen.draw()
                    win.flip()
                    core.wait(.25) # 250ms isi
                
                # show the recall screen and record responses
             
            blankScreen.draw()
            win.flip()
            core.wait(1) # 1s blank screen before letter grid recall screen
            
            # auto draw is on because we want to draw these on each frame.
            Fbox.autoDraw = True
            Hbox.autoDraw = True
            Jbox.autoDraw = True
            Kbox.autoDraw = True
            Lbox.autoDraw = True
            Nbox.autoDraw = True
            Pbox.autoDraw = True
            Qbox.autoDraw = True
            Rbox.autoDraw = True
            Sbox.autoDraw = True
            Tbox.autoDraw = True
            Ybox.autoDraw = True
            blankButtonBox.autoDraw = True
            clearButtonBox.autoDraw = True
            enterButtonBox.autoDraw=True
            F_r1c1.autoDraw = True
            H_r1c2.autoDraw = True
            J_r1c3.autoDraw = True
            K_r2c1.autoDraw = True
            L_r2c2.autoDraw = True
            N_r2c3.autoDraw = True
            P_r3c1.autoDraw = True
            Q_r3c2.autoDraw = True
            R_r3c3.autoDraw = True
            S_r4c1.autoDraw = True
            T_r4c2.autoDraw = True
            Y_r4c3.autoDraw = True
            blankButton.autoDraw = True
            clearButton.autoDraw = True
            enterButton.autoDraw=True
            letterRecallText.autoDraw=True
        
            win.flip()
        
        
        
            # RECORD THE LETTERS AND SHOW THEM BACK TO PARTICIPANTS
            # set up the mouse, it will be called "myMouse"
            myMouse = event.Mouse(visible = True, win = win) 
            myMouse.setPos(newPos =[0,0]); # set mouse to be in the middle of the screen
        
        
            # initiate the response variable where we will store the participants' responses
            tmpLetterRecall = []; 
        
            # store the possible shapes that participants can click on during the recall period (this doesn't include the enter box)
            boxes =[Fbox, Hbox, Jbox, Kbox, Lbox, Nbox, Pbox, Qbox, Rbox, Sbox, Tbox, Ybox, blankButtonBox, clearButtonBox]
              
            # Because mouse clicks sometimes happen slower than the speed of frames in psychopy, there may be multiple recorded responses during a single
            # mouse click. for example, if a participant clicks on "F", if the mouse click took place over multiple frames (let's say 4), then "F" will be
            # recorded four times, even though the participant clicked it once. Frames in psychopy are around 16.7 ms, whereas the mouseclick make take
            # a little longer than that. To get around this, we do the following:
            minFramesAfterClick = 10 # to prevent re-entering the if loop too early, other wise multiple letters are recorded during a single mouse click
            timeAfterClick = 0 # initiate time after click ot be 0 (will update in the loop below)
        
            myMouse.clickReset() # make sure mouseclick is reset to [0,0,0]
        
            while not myMouse.isPressedIn(enterButtonBox): # to exit this, participants must click on the "enter" button. 
                timeAfterClick += 1
        
                for box in boxes:
                    if myMouse.isPressedIn(box) and timeAfterClick >= minFramesAfterClick: # slows things down so that multiple responses are not recorded for a single click
                        tmpLetterRecall.append(box.name)
                        myMouse.clickReset()
                        timeAfterClick=0
        
                        if box == clearButtonBox: # if clear button is pressed, reset everything
                            tmpLetterRecall=[]
                            Fbox.color="white"
                            Hbox.color="white" 
                            Jbox.color="white"
                            Kbox.color="white" 
                            Lbox.color="white" 
                            Nbox.color="white" 
                            Pbox.color="white" 
                            Qbox.color="white" 
                            Rbox.color="white" 
                            Sbox.color="white"
                            Tbox.color="white" 
                            Ybox.color="white"
        
        
                # change clicked boxes to be green (except theblank button, keep it white)
                for box in boxes:
                    if box.name in tmpLetterRecall and not box.name == blankButtonBox.name:
                        box.color = 'green'
             
                # prep the text that shows participant's responses (letters)
                responseText='' 
                for l in range(len(tmpLetterRecall)):
                    responseText = "%s %s " % (responseText, tmpLetterRecall[l])
        
                # draw the response text
                showLetterResponse.text = responseText    
                showLetterResponse.autoDraw=True
                win.flip()
                    
        
                # reset mouse
                myMouse.clickReset() 
                
            # turn off autodraw
            Fbox.autoDraw = False
            Hbox.autoDraw = False
            Jbox.autoDraw = False
            Kbox.autoDraw = False
            Lbox.autoDraw = False
            Nbox.autoDraw = False
            Pbox.autoDraw = False
            Qbox.autoDraw = False
            Rbox.autoDraw = False
            Sbox.autoDraw = False
            Tbox.autoDraw = False
            Ybox.autoDraw = False
            blankButtonBox.autoDraw = False
            clearButtonBox.autoDraw = False
            enterButtonBox.autoDraw=False
            F_r1c1.autoDraw = False
            H_r1c2.autoDraw = False
            J_r1c3.autoDraw = False
            K_r2c1.autoDraw = False
            L_r2c2.autoDraw = False
            N_r2c3.autoDraw = False
            P_r3c1.autoDraw = False
            Q_r3c2.autoDraw = False
            R_r3c3.autoDraw = False
            S_r4c1.autoDraw = False
            T_r4c2.autoDraw = False
            Y_r4c3.autoDraw = False
            blankButton.autoDraw = False
            clearButton.autoDraw = False
            enterButton.autoDraw=False
            showLetterResponse.autoDraw=False
            letterRecallText.autoDraw=False
        
            
            # reset box colors
            Fbox.color="white"
            Hbox.color="white" 
            Jbox.color="white"
            Kbox.color="white" 
            Lbox.color="white" 
            Nbox.color="white" 
            Pbox.color="white" 
            Qbox.color="white" 
            Rbox.color="white" 
            Sbox.color="white"
            Tbox.color="white" 
            Ybox.color="white"
        
            
            #show feedback    
            correctCount = 0
            if len(tmpLetterRecall) == len(tmpLettersShown): # if participant recalls correct number of letters
                for l in range(setSizeLetterPrac[s]):
                    if tmpLetterRecall[l] == tmpLettersShown[l]:
                        correctCount +=1
                letterFeedbackText.text = text = "You recalled %.0f letters correctly out of %.0f." % (correctCount, setSizeLetterPrac[s])
            elif len(tmpLetterRecall) > len(tmpLettersShown): # if sub recalls more letters than set size
                for l in range(setSizeLetterPrac[s]):
                    if tmpLetterRecall[l] == tmpLettersShown[l]:
                        correctCount +=1
                letterFeedbackText.text = text = "You recalled too many letters." 
            elif len(tmpLetterRecall)==0: # if participant does not recall any letters
                letterFeedbackText.text = text = "You did not recall any letters."
            elif (len(tmpLetterRecall)<len(tmpLettersShown)) and not (len(tmpLetterRecall) ==0):
                #for l in range(setSizeLetterPrac[s]):
                for l in range(len(tmpLetterRecall)):
                    if tmpLetterRecall[l] == tmpLettersShown[l]:
                        correctCount +=1
                letterFeedbackText.text = text = "You did not recall enough letters." 
        
        
            #letterFeedbackText.text = text = "You recalled %.0f letters correctly out of %.0f." % (correctCount, setSizeLetterPrac[s])
            letterFeedbackText.draw()
            win.flip()
            core.wait(1.5) # show feedback for 1.5 seconds
            
            blankScreen.draw()
            win.flip()
            core.wait(1) # blank screen for 1s before moving to next trial
            
            # after each trial, add data for:
            #lettersRecall.append(tmpLetterRecall) # recalled letters
            #lettersShown.append(tmpLettersShown) # letters displayed 
           
            letterPracticeData.append(
                [
                    setSizeLetterPrac[s], 
                    tmpLettersShown,
                    tmpLetterRecall,
                    correctCount,
                    s
                ]
            )
            
        # change practice file into pandas dataframe
        letterPracticeData = pd.DataFrame(letterPracticeData) #convert data into pandas dataframe
        letterPracticeData.columns=["setSize","lettersShown","lettersRecall","correctCount","trial"] # add column names
        letterPracticeData = letterPracticeData.iloc[1: , :] # drop the first row which are the variable names
        
        
        
        # OPERATION PRACTICE
        # Instructions:
        mathInstructionsPg1.draw()
        instructCount.text=text="screen 1/4"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)
        
        mathInstructionsPg1.draw()
        instructCount.text=text="screen 1/4"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        mathInstructionsPg2.draw()
        instructCount.text=text="screen 2/4"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        mathInstructionsPg2.draw()
        instructCount.text=text="screen 2/4"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        mathInstructionsPg3.draw()
        instructCount.text=text="screen 3/4"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        mathInstructionsPg3.draw()
        instructCount.text=text="screen 3/4"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        mathInstructionsPg4.draw()
        instructCount.text=text="screen 4/4"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        mathInstructionsPg4.draw()
        instructCount.text=text="screen 4/4"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        blankScreen.draw()
        win.flip()
        core.wait(1)
        
        
        # start math practice (15 trials, math operations only)
        #nTmathPrac = 15
        
        # set up mouse for true/false responses
        myMouse = event.Mouse(visible = True, win = win) 
        minFramesAfterClick = 10 # to prevent re-entering the if loop too early, other wise multiple letters are recorded during a single mouse click
        timeAfterClick = 0
        mathboxes = [mathTrueBox, mathFalseBox]
        
        
        mathPracticeData = [] # create data structure with column names
        mathPracticeData.append(
            [
                "operation", 
                "response",
                "responseCorrect",
                "solveMathRT",
                "suggestedAnswer",
                "suggestAnswerCorrect",
                "trueFalseRT",
                "trial"
            ]
        )
        
        
        for m in range(nTmathPrac):
        
            # set the text for the problem and suggested answer on this trial
            selectedMathProblem = practiceOperations.problemPractice[m]
            mathSuggestedAns.text = str(practiceOperations.suggestAnsPractice[m])
        
            blankScreen.draw()
            win.flip()
            core.wait(.5) # blank screen for 500ms prior to each math operation
            
        
            # Show the math problem
            #selectedMathProblem = text = "%s %s %s = ?" % (selectedOps1.problem[m], selectedOps2.Sign[m], selectedOps2.Op2[m])
            
            mathText.text = selectedMathProblem
            mathText.draw()
            mathClickEnter.draw()
            buttons = [0]*len(event.mouseButtons) #initializes it to a list of 0s with the length equal to the number of active buttons.
            myMouse.setPos(newPos =[0,mathFalseBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
        
            
            win.flip() # show math problem
            myMouse.clickReset() # make sure mouseclick is reset to [0,0,0], restarts the clock
            
            while not any(buttons):
                (buttons,rtTimes) = myMouse.getPressed(getTime=True)
           
            #tmpMathRT.append(rtTimes[0])
            tmpMathRT = rtTimes[0]
            
            #Draw the isi
            fixationScreen.draw() 
            win.flip()
            core.wait(.2) # 200ms isi
            
        
            # Show the suggested answer on screen along with "true" and "false" buttons
            mathSuggestedAns.draw()
            mathTrueBox.draw()
            mathTrueButton.draw()
            mathFalseBox.draw()
            mathFalseButton.draw() 
            
            myMouse.setPos(newPos =[0,mathFalseBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
            win.flip()
        
            # collect response, record RT and check whether participant was correct.
            myMouse.clickReset() # make sure mouseclick is reset to [0,0,0]
        
            mouseResponse = 0;
            
            while mouseResponse == 0:        
                timeAfterClick += 1
        
                for box in mathboxes:
                    if myMouse.isPressedIn(box) and timeAfterClick >= minFramesAfterClick: # slows things down so that multiple responses are not recorded for a single click
                        buttons, times = myMouse.getPressed(getTime=True)
                        tmpMathResp = box.name
                        tmpMathRTtrueFalse = times[0]
                        #tmpMathResp.append(box.name) # was true or false clicked
                        #tmpMathRTtrueFalse.append(times[0]) # store RT
                        
                        # once pressed, change box color to grey, redraw everything
                        box.color = "grey"
                        mathSuggestedAns.draw()
                        mathTrueBox.draw()
                        mathTrueButton.draw()
                        mathFalseBox.draw()
                        mathFalseButton.draw() 
                        
                        
                        # Show “correct” or “incorrect” on the true/false screen for 500ms
                        if tmpMathResp == str(practiceOperations.correctRespPractice[m]):
                            respCorrect = 1
                            mathPracFeedback.text = "Correct"
                            mathPracFeedback.color = "green"
                        else:
                            respCorrect = 0
                            mathPracFeedback.text = "Incorrect"
                            mathPracFeedback.color = "red"
                        
                        mathPracFeedback.draw()
                        win.flip()
                        core.wait(.5)
                        
                        box.color = "white" # reset box color to white
                        myMouse.clickReset()
                        timeAfterClick=0
                        mouseResponse =1 # change to 1 to end while loop
        
            mathPracticeData.append(
                [
                    selectedMathProblem, 
                    tmpMathResp,
                    respCorrect,
                    tmpMathRT,
                    practiceOperations.suggestAnsPractice[m],
                    practiceOperations.correctRespPractice[m],
                    tmpMathRTtrueFalse,
                    m
                ]
            )
            
            
        # Reformat data to pandas dataframe
        mathPracticeData = pd.DataFrame(mathPracticeData)
        mathPracticeData.columns = ["operation","response","responseCorrect", "solveMathRT","suggestedAnswer", "suggestAnswerCorrect","trueFalseRT","trial"]
        mathPracticeData = mathPracticeData.iloc[1: , :] # drop the first row which are the variable names
        
        # check for correct math trials
        correctMathDF = mathPracticeData.loc[mathPracticeData["responseCorrect"]==1]
        
        if len(correctMathDF) >2:
            # calculate the cut off time for following sections of the task: average RT + 2.5* standard deviation RT
            avgRT = statistics.mean(correctMathDF["solveMathRT"])
            stdRT = statistics.stdev(correctMathDF["solveMathRT"])
            
            maxMathDisplay = avgRT + (2.5*stdRT) # calculate the max display for the math problems in the future sets
            if maxMathDisplay <1.5:
                maxMathDisplay = 1.5
            mathPracticeData["maxMathDisp"] = maxMathDisplay # save to the dataframe
        
        elif len(correctMathDF) <=2:
            # show screen that says they did not have any correct math trials andthat thye have a nother chance to do the practice.
        
            mathInstructionsReDoPg1.draw()
            win.flip()
            event.waitKeys(keyList=['return'], timeStamped= False)
        
            # set up mouse for true/false responses
            myMouse = event.Mouse(visible = True, win = win) 
            minFramesAfterClick = 10 # to prevent re-entering the if loop too early, other wise multiple letters are recorded during a single mouse click
            timeAfterClick = 0
            mathboxes = [mathTrueBox, mathFalseBox]
        
        
            mathPracticeData2 = [] # create data structure with column names
            mathPracticeData2.append(
                [
                    "operation", 
                    "response",
                    "responseCorrect",
                    "solveMathRT",
                    "suggestedAnswer",
                    "suggestAnswerCorrect",
                    "trueFalseRT",
                    "trial"
                ]
            )
        
        
            for m in range(nTmathPrac):
            
                # set the text for the problem and suggested answer on this trial
                selectedMathProblem = practiceOperations2.problemPractice[m]
                mathSuggestedAns.text = str(practiceOperations2.suggestAnsPractice[m])
            
                blankScreen.draw()
                win.flip()
                core.wait(.5) # blank screen for 500ms prior to each math operation
                
            
                # Show the math problem
                #selectedMathProblem = text = "%s %s %s = ?" % (selectedOps1.problem[m], selectedOps2.Sign[m], selectedOps2.Op2[m])
                
                mathText.text = selectedMathProblem
                mathText.draw()
                mathClickEnter.draw()
                buttons = [0]*len(event.mouseButtons) #initializes it to a list of 0s with the length equal to the number of active buttons.
                myMouse.setPos(newPos =[0,mathFalseBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
            
                
                win.flip() # show suggested answer
                myMouse.clickReset() # make sure mouseclick is reset to [0,0,0], restarts the clock
                
                while not any(buttons):
                    (buttons,rtTimes) = myMouse.getPressed(getTime=True)
               
                #tmpMathRT.append(rtTimes[0])
                tmpMathRT = rtTimes[0]
                
                #Draw the isi
                fixationScreen.draw() 
                win.flip()
                core.wait(.2) # 200ms isi
                
            
                # Show the suggested answer on screen along with "true" and "false" buttons
                mathSuggestedAns.draw()
                mathTrueBox.draw()
                mathTrueButton.draw()
                mathFalseBox.draw()
                mathFalseButton.draw() 
                
                myMouse.setPos(newPos =[0,mathFalseBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
                win.flip()
            
                # collect response, record RT and check whether participant was correct.
                myMouse.clickReset() # make sure mouseclick is reset to [0,0,0]
            
                mouseResponse = 0;
                
                while mouseResponse == 0:        
                    timeAfterClick += 1
            
                    for box in mathboxes:
                        if myMouse.isPressedIn(box) and timeAfterClick >= minFramesAfterClick: # slows things down so that multiple responses are not recorded for a single click
                            buttons, times = myMouse.getPressed(getTime=True)
                            tmpMathResp = box.name
                            tmpMathRTtrueFalse = times[0]
                            #tmpMathResp.append(box.name) # was true or false clicked
                            #tmpMathRTtrueFalse.append(times[0]) # store RT
                            
                            # once pressed, change box color to grey, redraw everything
                            box.color = "grey"
                            mathSuggestedAns.draw()
                            mathTrueBox.draw()
                            mathTrueButton.draw()
                            mathFalseBox.draw()
                            mathFalseButton.draw() 
                            
                            
                            # Show “correct” or “incorrect” on the true/false screen for 500ms
                            if tmpMathResp == str(practiceOperations2.correctRespPractice[m]):
                                respCorrect = 1
                                mathPracFeedback.text = "Correct"
                                mathPracFeedback.color = "green"
                            else:
                                respCorrect = 0
                                mathPracFeedback.text = "Incorrect"
                                mathPracFeedback.color = "red"
                            
                            mathPracFeedback.draw()
                            win.flip()
                            core.wait(.5)
                            
                            box.color = "white" # reset box color to white
                            myMouse.clickReset()
                            timeAfterClick=0
                            mouseResponse =1 # change to 1 to end while loop
            
                mathPracticeData2.append(
                    [
                        selectedMathProblem, 
                        tmpMathResp,
                        respCorrect,
                        tmpMathRT,
                        practiceOperations.suggestAnsPractice[m],
                        practiceOperations.correctRespPractice[m],
                        tmpMathRTtrueFalse,
                        m
                    ]
                )
                
                
            # Reformat data to pandas dataframe
            mathPracticeData2 = pd.DataFrame(mathPracticeData2)
            mathPracticeData2.columns = ["operation","response","responseCorrect", "solveMathRT","suggestedAnswer", "suggestAnswerCorrect","trueFalseRT","trial"]
            mathPracticeData2 = mathPracticeData2.iloc[1: , :] # drop the first row which are the variable names
        
            # check for correct math trials
            correctMathDF = mathPracticeData2.loc[mathPracticeData2["responseCorrect"]==1]
            
            
            if len(correctMathDF) >2:
                
                #save round 2 in mathPracticeData
                mathPracticeData = mathPracticeData2
                
                # calculate the cut off time for following sections of the task: average RT + 2.5* standard deviation RT
                avgRT = statistics.mean(correctMathDF["solveMathRT"])
                stdRT = statistics.stdev(correctMathDF["solveMathRT"])
                
                maxMathDisplay = avgRT + (2.5*stdRT) # calculate the max display for the math problems in the future sets
                if maxMathDisplay <1.5:
                    maxMathDisplay = 1.5
                mathPracticeData["maxMathDisp"] = maxMathDisplay # save to the dataframe
                
            elif len(correctMathDF) <=2:
                
                #save the second math practice of the experiment
                datetime = time.strftime("%Y%m%d-%H%M%S"); # save date and time
                filenameMathPrac2 = dataDirectoryPath + "cgeOSPANmathPractice2_" + "sub" + subID + "_" + datetime + ".csv"; # make filename
                mathPracticeData2.to_csv(filenameMathPrac2)
                
                #display screen that says they still didnt get any math correct and the experiment is done.
                mathInstructionsEndofTask.draw()
                win.flip()
                event.waitKeys(keyList=['return'], timeStamped=False)
                win.close() # close screen
        
        
        # Start letter + math practice
        
        # INSTRUCTIONS
        
        letterMathPractInstructionsPg1.draw()
        instructCount.text=text="screen 1/5"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        letterMathPractInstructionsPg1.draw()
        instructCount.text=text="screen 1/5"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        letterMathPractInstructionsPg2.draw()
        instructCount.text=text="screen 2/5"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)
        
        letterMathPractInstructionsPg2.draw()
        instructCount.text=text="screen 2/5"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        
        letterMathPractInstructionsPg3.draw()
        instructCount.text=text="screen 3/5"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        letterMathPractInstructionsPg3.draw()
        instructCount.text=text="screen 3/5"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        letterMathPractInstructionsPg4.draw()
        instructCount.text=text="screen 4/5"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        letterMathPractInstructionsPg4.draw()
        instructCount.text=text="screen 4/5"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        letterMathPractInstructionsPg5.draw()
        instructCount.text=text="screen 5/5"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        letterMathPractInstructionsPg5.draw()
        instructCount.text=text="screen 5/5"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        blankScreen.draw()
        win.flip()
        core.wait(1)
        
        
        
        #Start Letter + Math practice
        
        #maxMathDisplay = 4# for testing
        
        #MATH SET UP
        #nTbothPractice = 3 # three trials for the letter-math practice
        setSize = 2 # all trials are 2 letter-math pairs
        
        # Select math problems
        selectedOps1 = operationSet1.sample(n=nTbothPractice*setSize,axis = "rows")
        selectedOps2 = operationSet2.sample(n=nTbothPractice*setSize, axis="rows", replace="True")
        selectedOps1.index=range(nTbothPractice*setSize)
        selectedOps2.index=range(nTbothPractice*setSize)
        operationsLettersDF = pd.concat([selectedOps1, selectedOps2], axis=1) # combine dataframes into one
        correctAnswerMath =[]
        suggestedAnswerMath=[]
        totalSumTmp = []
        
        # Make sure that the sum of operation 1 (e.g. (2/2)) and operation 2 (e.g. -5) are greater than zero, if not
        # add three to the second operation until the sum is greater than 0
        for o in range(nTbothPractice*setSize):
            totalSum = operationsLettersDF["Sum1"][o] + operationsLettersDF["Sum2"][o]
            
            while totalSum <=0:
                operationsLettersDF["Sum2"][o] = operationsLettersDF["Sum2"][o] + 3
                totalSum = operationsLettersDF["Sum1"][o] + operationsLettersDF["Sum2"][o]
            
            operationsLettersDF["Op2"][o] = abs(operationsLettersDF["Sum2"][o]) # update the operation 2 if sum2 changed
            
            # if making the sum of the two operations changes the second operation +, then we need to change the sign for that operation
            if operationsLettersDF["Sum2"][o] > 0:
                operationsLettersDF["Sign"][o] = "+"
                
            # determine whether suggested answer is correct (0=incorrect, 1=correct)
            correctAnswerMath.append(random.randint(0,1)) 
            
            # determine the suggested answer that will be shown
            if correctAnswerMath[o]==1:
                suggestedAnswerMath.append(totalSum)
            elif correctAnswerMath[o] ==0:
                randNum = random.randint(-25,25) # pick a random number
                while totalSum + randNum <0 or totalSum + randNum == totalSum:
                    randNum = randNum + 2
                    #print(randNum)
                
                suggestedAnswerMath.append(totalSum + randNum)
            totalSumTmp.append(totalSum)
        
        operationsLettersDF["totalSum"] = totalSumTmp # save the true sum to the big df
        operationsLettersDF["showCorrectAns"]= correctAnswerMath # save correctAnswerMath variable to selectedOps2
        operationsLettersDF["suggestedAnswerMath"]= suggestedAnswerMath # save suggestedAnswerMath variable to selectedOps2
        operationsLettersDF["setSize"] = setSize # set sizes are the same for practice, all =2
        operationsLettersDF["trialPerSet"] = [0,1]*nTbothPractice # operation number in each set
        #operationsLettersDF["setNumber"] = [0,0,1,1,2,2]
        
        
        # populate our data frame with some useful information
        setNum=[] # to store which the current set (0-4 since there are 5 sets)
        for s in range(nTbothPractice):
            for t in range(setSize):
                setNum.append(s)
        
        # add those variables to our dataframe
        operationsLettersDF["setNumber"] = setNum
        
        
        
        
        
        # LETTER SET UP
        bothPracticeLetters = []
        lettersShownShortFormat = [] # each row has all letters shown
        # select the letters we will show on each trial
        for t in range(nTbothPractice):
            tmpLetters = random.sample(letterList, k = setSize)# randomly select two letters, using sample instead of choices does without replacement
            # we don't want to have repeat letters in a single trial^
            lettersShownShortFormat.append(tmpLetters) # save them in short format
            for s in range(len(tmpLetters)):
                bothPracticeLetters.append(tmpLetters[s]) # also save letters in long format
        
        operationsLettersDF["lettersShown"] = bothPracticeLetters # add letters that are shown following each math operation
        
        
        
        # Set up mouse
        myMouse = event.Mouse(visible = True, win = win) 
        minFramesAfterClick = 10 # to prevent re-entering the if loop too early, other wise multiple letters are recorded during a single mouse click
        timeAfterClick = 0
        mathboxes = [mathTrueBox, mathFalseBox]
        
        # set up data
        bothPracticeData = [] # create data structure with column names
        bothPracticeData.append(
            [
                "operation1", 
                "sum1",
                "operation2",
                "sign",
                "sum2",
                "totalSum",
                "showCorrectAns",
                "suggestedAnswer",
                "mathResponse",
                "mathResponseCorrect",
                "solveMathRT",
                "trueFalseRT",
                "setSize",
                "setNumber",
                "trialPerSet",
                "lettersShown",
                "lettersRecall",
                "correctCount"
                
            ]
        )
        
        
        # Start the practice
        
        for t in range(nTbothPractice): # for each trial
            
            for s in range(setSize): # and set size within each trial
                
                #pull out row we need for each trial/set
                tmpRow = operationsLettersDF.loc[(operationsLettersDF.setNumber ==t) & (operationsLettersDF.trialPerSet==s)]
        
            # set the text for the math problem and suggested answer on this trial
                selectedMathProblem = "%s %s %s = ?" % (tmpRow.problem.iat[0], tmpRow.Sign.iat[0], tmpRow.Op2.iat[0])
                mathSuggestedAns.text = str(tmpRow.suggestedAnswerMath.iat[0])
            
            # set the text for the letter
                letterDisplay.text = tmpRow.lettersShown.iat[0]
            
                fixationScreen.draw()
                win.flip()
                core.wait(.25) # blank screen for 250ms prior to each math operation
                
            #draw the math problem
                mathText.text = selectedMathProblem
                mathText.draw()
                mathClickEnter.draw()
                buttons = [0]*len(event.mouseButtons) #initializes it to a list of 0s with the length equal to the number of active buttons.
                myMouse.setPos(newPos =[0,mathFalseBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
            
                
                win.flip() # show the math problem
                myMouse.clickReset() # make sure mouseclick is reset to [0,0,0], restarts the clock
                mathMaxClock = core.Clock() # start the clock
            
            # wait for math response to move onto math T/F
                while not any(buttons) and mathMaxClock.getTime() <= maxMathDisplay:
                    (buttons,rtTimes) = myMouse.getPressed(getTime=True)
               
                #if participant does not respond in time on math, move to letter part
                if not any(buttons): 
                    tmpMathRT = float("nan") # record RT as nan
                    tmpMathResp = float("nan")
                    respCorrect = 0
                    tmpMathRTtrueFalse = float("nan")
                    
                    # record data
                    bothPracticeData.append(
                        [
                            tmpRow.problem.iat[0], 
                            tmpRow.Sum1.iat[0],
                            tmpRow.Op2.iat[0],
                            tmpRow.Sign.iat[0],
                            tmpRow.Sum2.iat[0],
                            tmpRow.totalSum.iat[0],
                            tmpRow.showCorrectAns.iat[0],
                            tmpRow.suggestedAnswerMath.iat[0],
                            tmpMathResp,
                            respCorrect,
                            tmpMathRT,
                            tmpMathRTtrueFalse,
                            tmpRow.setSize.iat[0],
                            t,
                            tmpRow.trialPerSet.iat[0],
                            tmpRow.lettersShown.iat[0],
                            #'l', # tmp place holder for letters recalled
                            #0 # tmp place holder for correct count
                            
                        ]
                    )
                    
                else: # if participant does respond in time, move onto True/False
                
                
                    tmpMathRT = rtTimes[0]
                
                    #Draw the isi
                    fixationScreen.draw() 
                    win.flip()
                    core.wait(.2) # 200ms isi
                
            
                    # Show the suggested answer on screen along with "true" and "false" buttons
                    mathSuggestedAns.draw()
                    mathTrueBox.draw()
                    mathTrueButton.draw()
                    mathFalseBox.draw()
                    mathFalseButton.draw() 
                
                    myMouse.setPos(newPos =[0,mathFalseBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
                    win.flip()
            
                    # collect response, record RT and check whether participant was correct.
                    myMouse.clickReset() # make sure mouseclick is reset to [0,0,0]
            
                    mouseResponse = 0;
                
                    while mouseResponse == 0:        
                        timeAfterClick += 1
                
                        for box in mathboxes:
                            if myMouse.isPressedIn(box) and timeAfterClick >= minFramesAfterClick: # slows things down so that multiple responses are not recorded for a single click
                                buttons, times = myMouse.getPressed(getTime=True)
                                tmpMathResp = box.name
                                tmpMathRTtrueFalse = times[0]
            
                                
                                # once pressed, change box color to grey, redraw everything
                                box.color = "grey"
                                mathSuggestedAns.draw()
                                mathTrueBox.draw()
                                mathTrueButton.draw()
                                mathFalseBox.draw()
                                mathFalseButton.draw() 
                                
                                
                                # Is response correct or incorrect?
                                if (tmpRow.showCorrectAns.iat[0] ==1) and (tmpMathResp == 'True'):
                                    respCorrect = 1
                                elif (tmpRow.showCorrectAns.iat[0] ==0) and (tmpMathResp == 'True'):
                                    respCorrect = 0
                                elif (tmpRow.showCorrectAns.iat[0] ==1) and (tmpMathResp == 'False'):
                                    respCorrect = 0
                                elif (tmpRow.showCorrectAns.iat[0] ==0) and (tmpMathResp == 'False'):
                                    respCorrect = 1
                                                    
                                
                                box.color = "white" # reset box color to white
                                myMouse.clickReset() # reset mouse
                                timeAfterClick=0 # reset timeAfterClick
                                mouseResponse =1 # change to 1 to end while loop
                
                                bothPracticeData.append(
                                    [
                                        tmpRow.problem.iat[0], 
                                        tmpRow.Sum1.iat[0],
                                        tmpRow.Op2.iat[0],
                                        tmpRow.Sign.iat[0],
                                        tmpRow.Sum2.iat[0],
                                        tmpRow.totalSum.iat[0],
                                        tmpRow.showCorrectAns.iat[0],
                                        tmpRow.suggestedAnswerMath.iat[0],
                                        tmpMathResp,
                                        respCorrect,
                                        tmpMathRT,
                                        tmpMathRTtrueFalse,
                                        tmpRow.setSize.iat[0],
                                        t,
                                        tmpRow.trialPerSet.iat[0],
                                        tmpRow.lettersShown.iat[0]
                                        #'l', # tmp place holder for letters recalled
                                        #0 # tmp place holder for correct count
                                        
                                    ]
                                )
                blankScreen.draw()
                win.flip() #once T/F button is pressed, flip screen
                core.wait(.2) # ISI before letter is shown
                
                letterDisplay.draw() # draw letter
                win.flip() # show letter on screen
                core.wait(1) # show letter for 1s
            
            fixationScreen.draw()
            win.flip()
            core.wait(.5) # .5s ITI before recall screen
            
            #LETTER RECALL SCREEN
            
            # auto draw is on because we want to draw these on each frame.
            Fbox.autoDraw = True
            Hbox.autoDraw = True
            Jbox.autoDraw = True
            Kbox.autoDraw = True
            Lbox.autoDraw = True
            Nbox.autoDraw = True
            Pbox.autoDraw = True
            Qbox.autoDraw = True
            Rbox.autoDraw = True
            Sbox.autoDraw = True
            Tbox.autoDraw = True
            Ybox.autoDraw = True
            blankButtonBox.autoDraw = True
            clearButtonBox.autoDraw = True
            enterButtonBox.autoDraw=True
            F_r1c1.autoDraw = True
            H_r1c2.autoDraw = True
            J_r1c3.autoDraw = True
            K_r2c1.autoDraw = True
            L_r2c2.autoDraw = True
            N_r2c3.autoDraw = True
            P_r3c1.autoDraw = True
            Q_r3c2.autoDraw = True
            R_r3c3.autoDraw = True
            S_r4c1.autoDraw = True
            T_r4c2.autoDraw = True
            Y_r4c3.autoDraw = True
            blankButton.autoDraw = True
            clearButton.autoDraw = True
            enterButton.autoDraw=True
            letterRecallText.autoDraw=True
        
            win.flip()
        
        
            #RECORD THE LETTERS AND SHOW THEM BACK TO PARTICIPANTS
            # reset mouse
            myMouse.setPos(newPos =[0,0]); # set mouse to be in the middle of the screen
        
        
            #initiate the response variable where we will store the participants' responses
            tmpLetterRecall = []; 
        
            #store the possible shapes that participants can click on during the recall period (this doesn't include the enter box)
            boxes =[Fbox, Hbox, Jbox, Kbox, Lbox, Nbox, Pbox, Qbox, Rbox, Sbox, Tbox, Ybox, blankButtonBox, clearButtonBox]
                  
            # Because mouse clicks sometimes happen slower than the speed of frames in psychopy, there may be multiple recorded responses during a single
            # mouse click. for example, if a participant clicks on "F", if the mouse click took place over multiple frames (let's say 4), then "F" will be
            # recorded four times, even though the participant clicked it once. Frames in psychopy are around 16.7 ms, whereas the mouseclick make take
            # a little longer than that. To get around this, we do the following:
            minFramesAfterClick = 10 # to prevent re-entering the if loop too early, other wise multiple letters are recorded during a single mouse click
            timeAfterClick = 0 # initiate time after click ot be 0 (will update in the loop below)
        
            myMouse.clickReset() # make sure mouseclick is reset to [0,0,0]
        
            while not myMouse.isPressedIn(enterButtonBox): # to exit this, participants must click on the "enter" button. 
                timeAfterClick += 1
        
                for box in boxes:
                    if myMouse.isPressedIn(box) and timeAfterClick >= minFramesAfterClick: # slows things down so that multiple responses are not recorded for a single click
                        tmpLetterRecall.append(box.name)
                        myMouse.clickReset()
                        timeAfterClick=0
        
                        if box == clearButtonBox: # if clear button is pressed, reset everything
                            tmpLetterRecall=[]
                            Fbox.color="white"
                            Hbox.color="white" 
                            Jbox.color="white"
                            Kbox.color="white" 
                            Lbox.color="white" 
                            Nbox.color="white" 
                            Pbox.color="white" 
                            Qbox.color="white" 
                            Rbox.color="white" 
                            Sbox.color="white"
                            Tbox.color="white" 
                            Ybox.color="white"
        
        
                # change clicked boxes to be green (except theblank button, keep it white)
                for box in boxes:
                    if box.name in tmpLetterRecall and not box.name == blankButtonBox.name:
                        box.color = 'green'
             
                # prep the text that shows participant's responses (letters)
                responseText='' 
                for l in range(len(tmpLetterRecall)):
                    responseText = "%s %s " % (responseText, tmpLetterRecall[l])
        
                # draw the response text
                showLetterResponse.text = responseText    
                showLetterResponse.autoDraw=True
                win.flip()
                    
        
                # reset mouse
                myMouse.clickReset() 
                
            # turn off autodraw
            Fbox.autoDraw = False
            Hbox.autoDraw = False
            Jbox.autoDraw = False
            Kbox.autoDraw = False
            Lbox.autoDraw = False
            Nbox.autoDraw = False
            Pbox.autoDraw = False
            Qbox.autoDraw = False
            Rbox.autoDraw = False
            Sbox.autoDraw = False
            Tbox.autoDraw = False
            Ybox.autoDraw = False
            blankButtonBox.autoDraw = False
            clearButtonBox.autoDraw = False
            enterButtonBox.autoDraw=False
            F_r1c1.autoDraw = False
            H_r1c2.autoDraw = False
            J_r1c3.autoDraw = False
            K_r2c1.autoDraw = False
            L_r2c2.autoDraw = False
            N_r2c3.autoDraw = False
            P_r3c1.autoDraw = False
            Q_r3c2.autoDraw = False
            R_r3c3.autoDraw = False
            S_r4c1.autoDraw = False
            T_r4c2.autoDraw = False
            Y_r4c3.autoDraw = False
            blankButton.autoDraw = False
            clearButton.autoDraw = False
            enterButton.autoDraw=False
            showLetterResponse.autoDraw=False
            letterRecallText.autoDraw=False
        
            
            # reset box colors
            Fbox.color="white"
            Hbox.color="white" 
            Jbox.color="white"
            Kbox.color="white" 
            Lbox.color="white" 
            Nbox.color="white" 
            Pbox.color="white" 
            Qbox.color="white" 
            Rbox.color="white" 
            Sbox.color="white"
            Tbox.color="white" 
            Ybox.color="white"
        
                
            #show feedback    
            correctCountLetters = 0
        
            if len(tmpLetterRecall) == len(lettersShownShortFormat[t]): # if participant recalls correct number of letters
                for l in range(setSize):
                    if tmpLetterRecall[l] == lettersShownShortFormat[t][l]:
                        correctCountLetters +=1
                letterFeedbackText.text = text = "You recalled %.0f letters correctly out of %.0f." % (correctCountLetters, setSize)
            elif len(tmpLetterRecall) > len(lettersShownShortFormat[t]): # if sub recalls more letters than set size
                for l in range(setSize):
                    if tmpLetterRecall[l] == lettersShownShortFormat[t][l]:
                        correctCountLetters +=1
                letterFeedbackText.text = text = "You recalled too many letters." 
            elif len(tmpLetterRecall)==0: # if participant does not recall any letters
                letterFeedbackText.text = text = "You did not recall any letters."
            elif (len(tmpLetterRecall)<len(lettersShownShortFormat[t])) and not (len(tmpLetterRecall) ==0):
                #for l in range(setSize):
                for l in range(len(tmpLetterRecall)):
                    if tmpLetterRecall[l] == lettersShownShortFormat[t][l]:
                        correctCountLetters +=1
                letterFeedbackText.text = text = "You did not recall enough letters."
        
            # save letters recalled and whether it was correct after each trial (this happens at a different timescale as when we save the data above)
            
            
            bothPracticeData[t*2 + setSize].append(tmpLetterRecall)
            bothPracticeData[t*2 + setSize].append(correctCountLetters)
        
        
            #FEEDBACK AT THE END OF EACH SET (SO AFTER THE LETTER RECAL)
            # feedback here shows both the letter recall, math correct, and overall percent correct in math across sets
        
            # calculate how well participant is doing with math over all
            tmpDF = pd.DataFrame(bothPracticeData) # temporarily make practice data a dataframe for easier handling (this gets overwritten after each set)
            tmpDF.columns = tmpDF.iloc[0]
            tmpDF = tmpDF.iloc[1: , :]
            
            mathPercentCorrect = round(tmpDF.mathResponseCorrect.mean()*100) # number to show in red on feedback screen after letter recall
        
            # calculate number of math errors within a set
            
            mathErrorsDuringSet = sum(tmpDF.mathResponseCorrect[tmpDF.setNumber==t]==0)# number of errors made during the set
        
            bothPracticeData[t*2 + setSize].append(mathPercentCorrect) # save percentmath correct
            bothPracticeData[t*2 + setSize].append(mathErrorsDuringSet) # save math errors during set
            #bothPracticeData["maxMathDisplay"] = maxMathDisplay
        
        
        
            if mathErrorsDuringSet <3:
                  mathErrorsAfterRecall.text = text = "You made %.0f math error(s) for this set of trials." %(mathErrorsDuringSet)
            elif mathErrorsDuringSet >=3:
                  mathErrorsAfterRecall.text = text = "You have made a total of 3 or more errors during this set. Please do your best on the math."
            
            mathTotalPercentCorrect.text = text = "%.0f%%" %(mathPercentCorrect)
            
            mathErrorsAfterRecall.draw()
            mathTotalPercentCorrect.draw()
            letterFeedbackText.draw()
            
            win.flip()
            core.wait(2) # show feedback
            
            fixationScreen.draw()
            win.flip()
            core.wait(1)
                
        
    
    ## REAL OSPAN TASK NOW 
    #try:
    
        # INSTRUCTIONS
        
        realTaskInstructionsPg1.draw()
        instructCount.text=text="screen 1/2"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)
        
        realTaskInstructionsPg1.draw()
        instructCount.text=text="screen 1/2"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        realTaskInstructionsPg2.draw()
        instructCount.text=text="screen 2/2"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        realTaskInstructionsPg2.draw()
        instructCount.text=text="screen 2/2"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        #MATH SET UP
        #nTbothReal = 5 # 5 trials for the letter-math practice
        setSize = ([3,4,5,6,7]) # number of letter-math pairs in each set
        random.shuffle(setSize) # randomize the order or set size
        
        # Select math problems
        selectedOps1 = operationSet1.sample(n=sum(setSize),axis = "rows")
        selectedOps2 = operationSet2.sample(n=sum(setSize), axis="rows", replace="True")
        selectedOps1.index=range(sum(setSize))
        selectedOps2.index=range(sum(setSize))
        
        operationsLettersDF = pd.concat([selectedOps1, selectedOps2], axis=1) # combine dataframes into one
        correctAnswerMath =[]
        suggestedAnswerMath=[]
        totalSumTmp = []
        
        
        # Make sure that the sum of operation 1 (e.g. (2/2)) and operation 2 (e.g. -5) are greater than zero, if not
        # add three to the second operation until the sum is greater than 0
        for o in range(sum(setSize)):
            totalSum = operationsLettersDF["Sum1"][o] + operationsLettersDF["Sum2"][o]
            
            while totalSum <=0:
                operationsLettersDF["Sum2"][o] = operationsLettersDF["Sum2"][o] + 3
                totalSum = operationsLettersDF["Sum1"][o] + operationsLettersDF["Sum2"][o]
            
            operationsLettersDF["Op2"][o] = abs(operationsLettersDF["Sum2"][o]) # update the operation 2 if sum2 changed
            
            # if making the sum of the two operations changes the second operation +, then we need to change the sign for that operation
            if operationsLettersDF["Sum2"][o] > 0:
                operationsLettersDF["Sign"][o] = "+"
                
            # determine whether suggested answer is correct (0=incorrect, 1=correct)
            correctAnswerMath.append(random.randint(0,1)) 
            
            # determine the suggested answer that will be shown
            if correctAnswerMath[o]==1:
                suggestedAnswerMath.append(totalSum)
            elif correctAnswerMath[o] ==0:
                randNum = random.randint(-25,25) # pick a random number
                while totalSum + randNum <0 or totalSum + randNum == totalSum:
                    randNum = randNum + 2
                    #print(randNum)
                
                suggestedAnswerMath.append(totalSum + randNum)
            totalSumTmp.append(totalSum)
        
        operationsLettersDF["totalSum"] = totalSumTmp # save the true sum to the big df
        operationsLettersDF["showCorrectAns"]= correctAnswerMath # save correctAnswerMath variable to selectedOps2
        operationsLettersDF["suggestedAnswerMath"]= suggestedAnswerMath # save suggestedAnswerMath variable to selectedOps2
        
        
        # populate our data frame with some useful information
        setSizeLong = [] #to store the set size of each trial (options: 3-7)
        triPerSet = [] # to store the trial number within a set (0 to max of 6)
        setNum=[] # to store which the current set (0-4 since there are 5 sets)
        for set in setSize:
            for t in range(set):
                setSizeLong.append(set)
                triPerSet.append(t)
                setNum.append(setSize.index(set))
        
        # add those variables to our dataframe
        operationsLettersDF["setSize"] = setSizeLong 
        operationsLettersDF["trialPerSet"] = triPerSet 
        operationsLettersDF["setNumber"] = setNum
        
        
        # LETTER SET UP
        bothRealLetters = []
        lettersShownShortFormat = [] # each row has all letters shown
        # select the letters we will show on each trial
        for t in range(nTbothReal):
            tmpLetters = random.sample(letterList, k = setSize[t])# randomly select two letters, using sample instead of choices does without replacement
            # we don't want to have repeat letters in a single trial^
            lettersShownShortFormat.append(tmpLetters) # save them in short format
            for s in range(len(tmpLetters)):
                bothRealLetters.append(tmpLetters[s]) # also save letters in long format
        
        operationsLettersDF["lettersShown"] = bothRealLetters # add letters that are shown following each math operation
        
        # Set up mouse
        myMouse = event.Mouse(visible = True, win = win) 
        minFramesAfterClick = 10 # to prevent re-entering the if loop too early, other wise multiple letters are recorded during a single mouse click
        timeAfterClick = 0
        mathboxes = [mathTrueBox, mathFalseBox]
        
        # set up data
        bothRealData = [] # create data structure with column names
        bothRealData.append(
            [
                "operation1", 
                "sum1",
                "operation2",
                "sign",
                "sum2",
                "totalSum",
                "showCorrectAns",
                "suggestedAnswer",
                "mathResponse",
                "mathResponseCorrect",
                "solveMathRT",
                "trueFalseRT",
                "setSize",
                "setNumber",
                "trialPerSet",
                "lettersShown",
                "lettersRecall",
                "correctCount"
                
            ]
        )
        
        for t in range(nTbothReal): # for each trial
            
            for s in range(setSize[t]): # and set size within each trial
                
                #pull out row we need for each trial/set
                tmpRow = operationsLettersDF.loc[(operationsLettersDF.setNumber ==t) & (operationsLettersDF.trialPerSet==s)]
        
            # set the text for the math problem and suggested answer on this trial
                selectedMathProblem = "%s %s %s = ?" % (tmpRow.problem.iat[0], tmpRow.Sign.iat[0], tmpRow.Op2.iat[0])
                mathSuggestedAns.text = str(tmpRow.suggestedAnswerMath.iat[0])
            
            # set the text for the letter
                letterDisplay.text = tmpRow.lettersShown.iat[0]
            
                fixationScreen.draw()
                win.flip()
                core.wait(.25) # blank screen for 250ms prior to each math operation
                
            #draw the math problem
                mathText.text = selectedMathProblem
                mathText.draw()
                mathClickEnter.draw()
                buttons = [0]*len(event.mouseButtons) #initializes it to a list of 0s with the length equal to the number of active buttons.
                myMouse.setPos(newPos =[0,mathFalseBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
            
                
                win.flip() # show the math problem
                myMouse.clickReset() # make sure mouseclick is reset to [0,0,0], restarts the clock
                mathMaxClock = core.Clock() # start the clock
            
            # wait for math response to move onto math T/F
                while not any(buttons) and mathMaxClock.getTime() <= maxMathDisplay:
                    (buttons,rtTimes) = myMouse.getPressed(getTime=True)
               
                #if participant does not respond in time on math, move to letter part
                if not any(buttons): 
                    tmpMathRT = float("nan") # record RT as nan
                    tmpMathResp = float("nan")
                    respCorrect = 0
                    tmpMathRTtrueFalse = float("nan")
                    
                    # record data
                    bothRealData.append(
                        [
                            tmpRow.problem.iat[0], 
                            tmpRow.Sum1.iat[0],
                            tmpRow.Op2.iat[0],
                            tmpRow.Sign.iat[0],
                            tmpRow.Sum2.iat[0],
                            tmpRow.totalSum.iat[0],
                            tmpRow.showCorrectAns.iat[0],
                            tmpRow.suggestedAnswerMath.iat[0],
                            tmpMathResp,
                            respCorrect,
                            tmpMathRT,
                            tmpMathRTtrueFalse,
                            tmpRow.setSize.iat[0],
                            t,
                            tmpRow.trialPerSet.iat[0],
                            tmpRow.lettersShown.iat[0],
                            #'l', # tmp place holder for letters recalled
                            #0 # tmp place holder for correct count
                            
                        ]
                    )
                    
                else: # if participant does respond in time, move onto True/False
                
                
                    tmpMathRT = rtTimes[0]
                
                    #Draw the isi
                    fixationScreen.draw() 
                    win.flip()
                    core.wait(.2) # 200ms isi
                
            
                    # Show the suggested answer on screen along with "true" and "false" buttons
                    mathSuggestedAns.draw()
                    mathTrueBox.draw()
                    mathTrueButton.draw()
                    mathFalseBox.draw()
                    mathFalseButton.draw() 
                
                    myMouse.setPos(newPos =[0,mathFalseBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
                    win.flip()
            
                    # collect response, record RT and check whether participant was correct.
                    myMouse.clickReset() # make sure mouseclick is reset to [0,0,0]
            
                    mouseResponse = 0;
                
                    while mouseResponse == 0:        
                        timeAfterClick += 1
                
                        for box in mathboxes:
                            if myMouse.isPressedIn(box) and timeAfterClick >= minFramesAfterClick: # slows things down so that multiple responses are not recorded for a single click
                                buttons, times = myMouse.getPressed(getTime=True)
                                tmpMathResp = box.name
                                tmpMathRTtrueFalse = times[0]
            
                                
                                # once pressed, change box color to grey, redraw everything
                                box.color = "grey"
                                mathSuggestedAns.draw()
                                mathTrueBox.draw()
                                mathTrueButton.draw()
                                mathFalseBox.draw()
                                mathFalseButton.draw() 
                                
                                
                                # Is response correct or incorrect?
                                if (tmpRow.showCorrectAns.iat[0] ==1) and (tmpMathResp == 'True'):
                                    respCorrect = 1
                                elif (tmpRow.showCorrectAns.iat[0] ==0) and (tmpMathResp == 'True'):
                                   respCorrect = 0
                                elif (tmpRow.showCorrectAns.iat[0] ==1) and (tmpMathResp == 'False'):
                                    respCorrect = 0
                                elif (tmpRow.showCorrectAns.iat[0] ==0) and (tmpMathResp == 'False'):
                                    respCorrect = 1
                                                    
                                
                                box.color = "white" # reset box color to white
                                myMouse.clickReset() # reset mouse
                                timeAfterClick=0 # reset timeAfterClick
                                mouseResponse =1 # change to 1 to end while loop
                
                                bothRealData.append(
                                    [
                                        tmpRow.problem.iat[0], 
                                        tmpRow.Sum1.iat[0],
                                        tmpRow.Op2.iat[0],
                                        tmpRow.Sign.iat[0],
                                        tmpRow.Sum2.iat[0],
                                        tmpRow.totalSum.iat[0],
                                        tmpRow.showCorrectAns.iat[0],
                                        tmpRow.suggestedAnswerMath.iat[0],
                                        tmpMathResp,
                                        respCorrect,
                                        tmpMathRT,
                                        tmpMathRTtrueFalse,
                                        tmpRow.setSize.iat[0],
                                        t,
                                        tmpRow.trialPerSet.iat[0],
                                        tmpRow.lettersShown.iat[0]
                                        #'l', # tmp place holder for letters recalled
                                        #0 # tmp place holder for correct count
                                        
                                    ]
                                )
                blankScreen.draw()
                win.flip() #once T/F button is pressed, flip screen
                core.wait(.2) # ISI before letter is shown
                
                letterDisplay.draw() # draw letter
                win.flip() # show letter on screen
                core.wait(1) # show letter for 1s
            
            fixationScreen.draw()
            win.flip()
            core.wait(.5) # .5s ITI before recall screen
            
            #LETTER RECALL SCREEN
            
            # auto draw is on because we want to draw these on each frame.
            Fbox.autoDraw = True
            Hbox.autoDraw = True
            Jbox.autoDraw = True
            Kbox.autoDraw = True
            Lbox.autoDraw = True
            Nbox.autoDraw = True
            Pbox.autoDraw = True
            Qbox.autoDraw = True
            Rbox.autoDraw = True
            Sbox.autoDraw = True
            Tbox.autoDraw = True
            Ybox.autoDraw = True
            blankButtonBox.autoDraw = True
            clearButtonBox.autoDraw = True
            enterButtonBox.autoDraw=True
            F_r1c1.autoDraw = True
            H_r1c2.autoDraw = True
            J_r1c3.autoDraw = True
            K_r2c1.autoDraw = True
            L_r2c2.autoDraw = True
            N_r2c3.autoDraw = True
            P_r3c1.autoDraw = True
            Q_r3c2.autoDraw = True
            R_r3c3.autoDraw = True
            S_r4c1.autoDraw = True
            T_r4c2.autoDraw = True
            Y_r4c3.autoDraw = True
            blankButton.autoDraw = True
            clearButton.autoDraw = True
            enterButton.autoDraw=True
            letterRecallText.autoDraw=True
        
            win.flip()
        
        
            #RECORD THE LETTERS AND SHOW THEM BACK TO PARTICIPANTS
            # reset mouse
            myMouse.setPos(newPos =[0,0]); # set mouse to be in the middle of the screen
        
        
            #initiate the response variable where we will store the participants' responses
            tmpLetterRecall = []; 
        
            #store the possible shapes that participants can click on during the recall period (this doesn't include the enter box)
            boxes =[Fbox, Hbox, Jbox, Kbox, Lbox, Nbox, Pbox, Qbox, Rbox, Sbox, Tbox, Ybox, blankButtonBox, clearButtonBox]
                  
            # Because mouse clicks sometimes happen slower than the speed of frames in psychopy, there may be multiple recorded responses during a single
            # mouse click. for example, if a participant clicks on "F", if the mouse click took place over multiple frames (let's say 4), then "F" will be
            # recorded four times, even though the participant clicked it once. Frames in psychopy are around 16.7 ms, whereas the mouseclick make take
            # a little longer than that. To get around this, we do the following:
            minFramesAfterClick = 10 # to prevent re-entering the if loop too early, other wise multiple letters are recorded during a single mouse click
            timeAfterClick = 0 # initiate time after click ot be 0 (will update in the loop below)
        
            myMouse.clickReset() # make sure mouseclick is reset to [0,0,0]
        
            while not myMouse.isPressedIn(enterButtonBox): # to exit this, participants must click on the "enter" button. 
                timeAfterClick += 1
        
                for box in boxes:
                    if myMouse.isPressedIn(box) and timeAfterClick >= minFramesAfterClick: # slows things down so that multiple responses are not recorded for a single click
                        tmpLetterRecall.append(box.name)
                        myMouse.clickReset()
                        timeAfterClick=0
        
                        if box == clearButtonBox: # if clear button is pressed, reset everything
                            tmpLetterRecall=[]
                            Fbox.color="white"
                            Hbox.color="white" 
                            Jbox.color="white"
                            Kbox.color="white" 
                            Lbox.color="white" 
                            Nbox.color="white" 
                            Pbox.color="white" 
                            Qbox.color="white" 
                            Rbox.color="white" 
                            Sbox.color="white"
                            Tbox.color="white" 
                            Ybox.color="white"
        
        
                # change clicked boxes to be green (except theblank button, keep it white)
                for box in boxes:
                    if box.name in tmpLetterRecall and not box.name == blankButtonBox.name:
                        box.color = 'green'
             
                # prep the text that shows participant's responses (letters)
                responseText='' 
                for l in range(len(tmpLetterRecall)):
                    responseText = "%s %s " % (responseText, tmpLetterRecall[l])
        
                # draw the response text
                showLetterResponse.text = responseText    
                showLetterResponse.autoDraw=True
                win.flip()
                    
        
                # reset mouse
                myMouse.clickReset() 
                
            # turn off autodraw
            Fbox.autoDraw = False
            Hbox.autoDraw = False
            Jbox.autoDraw = False
            Kbox.autoDraw = False
            Lbox.autoDraw = False
            Nbox.autoDraw = False
            Pbox.autoDraw = False
            Qbox.autoDraw = False
            Rbox.autoDraw = False
            Sbox.autoDraw = False
            Tbox.autoDraw = False
            Ybox.autoDraw = False
            blankButtonBox.autoDraw = False
            clearButtonBox.autoDraw = False
            enterButtonBox.autoDraw=False
            F_r1c1.autoDraw = False
            H_r1c2.autoDraw = False
            J_r1c3.autoDraw = False
            K_r2c1.autoDraw = False
            L_r2c2.autoDraw = False
            N_r2c3.autoDraw = False
            P_r3c1.autoDraw = False
            Q_r3c2.autoDraw = False
            R_r3c3.autoDraw = False
            S_r4c1.autoDraw = False
            T_r4c2.autoDraw = False
            Y_r4c3.autoDraw = False
            blankButton.autoDraw = False
            clearButton.autoDraw = False
            enterButton.autoDraw=False
            showLetterResponse.autoDraw=False
            letterRecallText.autoDraw=False
        
            
            # reset box colors
            Fbox.color="white"
            Hbox.color="white" 
            Jbox.color="white"
            Kbox.color="white" 
            Lbox.color="white" 
            Nbox.color="white" 
            Pbox.color="white" 
            Qbox.color="white" 
            Rbox.color="white" 
            Sbox.color="white"
            Tbox.color="white" 
            Ybox.color="white"
        
                
            #show feedback    
            correctCountLetters = 0
        
            if len(tmpLetterRecall) == len(lettersShownShortFormat[t]): # if participant recalls correct number of letters
                for l in range(setSize[t]):
                    if tmpLetterRecall[l] == lettersShownShortFormat[t][l]:
                        correctCountLetters +=1
                letterFeedbackText.text = text = "You recalled %.0f letters correctly out of %.0f." % (correctCountLetters, setSize[t])
            elif len(tmpLetterRecall) > len(lettersShownShortFormat[t]): # if sub recalls more letters than set size
                for l in range(setSize[t]):
                    if tmpLetterRecall[l] == lettersShownShortFormat[t][l]:
                        correctCountLetters +=1
                letterFeedbackText.text = text = "You recalled too many letters." 
            elif len(tmpLetterRecall)==0: # if participant does not recall any letters
                letterFeedbackText.text = text = "You did not recall any letters."
            elif (len(tmpLetterRecall)<len(lettersShownShortFormat[t])) and not (len(tmpLetterRecall) ==0):
                #for l in range(setSize):
                for l in range(len(tmpLetterRecall)):
                    if tmpLetterRecall[l] == lettersShownShortFormat[t][l]:
                        correctCountLetters +=1
                letterFeedbackText.text = text = "You did not recall enough letters."
        
            # save letters recalled and whether it was correct after each trial (this happens at a different timescale as when we save the data above)
            
            
            #bothRealData[t*2 + setSize[t]].append(tmpLetterRecall)
            #bothRealData[t*2 + setSize[t]].append(correctCountLetters)
            
            bothRealData[sum(setSize[0:t+1])].append(tmpLetterRecall)
            bothRealData[sum(setSize[0:t+1])].append(correctCountLetters)
            
        
            #FEEDBACK AT THE END OF EACH SET (SO AFTER THE LETTER RECALL)
            # feedback here shows both the letter recall, math correct, and overall percent correct in math across sets
        
            # calculate how well participant is doing with math over all
            tmpDF = pd.DataFrame(bothRealData) # temporarily make practice data a dataframe for easier handling (this gets overwritten after each set)
            tmpDF.columns = tmpDF.iloc[0]
            tmpDF = tmpDF.iloc[1: , :]
            
            mathPercentCorrect = round(tmpDF.mathResponseCorrect.mean()*100) # number to show in red on feedback screen after letter recall
            
            
            # calculate number of math errors within a set
            
            mathErrorsDuringSet = sum(tmpDF.mathResponseCorrect[tmpDF.setNumber==t]==0)# number of errors made during the set
            
            
            bothRealData[sum(setSize[0:t+1])].append(mathPercentCorrect) # save percentmath correct
            bothRealData[sum(setSize[0:t+1])].append(mathErrorsDuringSet) # save math errors
            #bothRealData["maxMathDisplay"] = maxMathDisplay
            
            if mathErrorsDuringSet <3:
                  mathErrorsAfterRecall.text = text = "You made %.0f math error(s) for this set of trials." %(mathErrorsDuringSet)
            elif mathErrorsDuringSet >=3:
                  mathErrorsAfterRecall.text = text = "You have made a total of 3 or more errors during this set. Please do your best on the math."
            
        
            
            mathTotalPercentCorrect.text = text = "%.0f%%" %(mathPercentCorrect)
            
            mathErrorsAfterRecall.draw()
            mathTotalPercentCorrect.draw()
            letterFeedbackText.draw()
            
            win.flip()
            core.wait(2) # show feedback
            
            fixationScreen.draw()
            win.flip()
            core.wait(1)
                
        endOfTask.draw()
        win.flip()
        event.waitKeys(keyList = ['space'], timeStamped = False) # waiting for key press or until max time allowed
    
    finally:
        win.close()
    
    
        #---- AT THE END OR IF THINGS BREAK - SAVE THE DATA WE HAVE ----#
        
        # if data exists, reformat to pd dataframe if it was not already above, then save it
        datetime = time.strftime("%Y%m%d-%H%M%S"); # save date and time
        
        
        if 'mathPracticeData' in locals(): 
            if not isinstance(mathPracticeData, pd.DataFrame):
                mathPracticeData = pd.DataFrame(mathPracticeData)
                mathPracticeData.columns = ["operation","response","responseCorrect", "solveMathRT","suggestedAnswer", "suggestAnswerCorrect","trueFalseRT","trial"]
                mathPracticeData = mathPracticeData.iloc[1: , :] # drop the first row which are the variable names
            filenameMathPrac = dataDirectoryPath + "cgeOSPANmathPractice_" + "sub" + subID + "_" + datetime + ".csv"; # make filename
            mathPracticeData.to_csv(filenameMathPrac)
                
        if 'letterPracticeData' in locals(): 
            if not isinstance(letterPracticeData, pd.DataFrame):
                letterPracticeData = pd.DataFrame(letterPracticeData) #convert data into pandas dataframe
                letterPracticeData.columns=["setSize","lettersShown","lettersRecall","correctCount","trial"] # add column names
                letterPracticeData = letterPracticeData.iloc[1: , :] # drop the first row which are the variable namesPracticeData.iloc[1: , :] # drop the first row which are the variable names
            filenameLetterPrac = dataDirectoryPath + "cgeOSPANletterPractice_" + "sub" + subID + "_" + datetime + ".csv"; # make filename
            letterPracticeData.to_csv(filenameLetterPrac)   
            
        if 'bothPracticeData' in locals(): 
            if not isinstance(bothPracticeData, pd.DataFrame):
                bothPracticeData = pd.DataFrame(bothPracticeData) #convert data into pandas dataframe
                bothPracticeData.columns=["operation1","sum1","operation2","sign","sum2","totalSum","showCorrectAns","suggestedAnswer","mathResponse","mathResponseCorrect", "solveMathRT", "trueFalseRT", "setSize","setNumber","trialPerSet", "lettersShown", "lettersRecall", "correctCount","percentCorrectMath", "totalMathErrorsInSet"] # add column names
                bothPracticeData = bothPracticeData.iloc[1: , :] # drop the first row which are the variable bothPracticeData.iloc[1: , :] # drop the first row which are the variable names
            filenameBothPrac =dataDirectoryPath + "cgeOSPANbothPractice_" + "sub" + subID + "_" + datetime + ".csv"; # make filename
            bothPracticeData.to_csv(filenameBothPrac)
            
        if 'bothRealData' in locals(): 
            if not isinstance(bothRealData, pd.DataFrame):
                bothRealData = pd.DataFrame(bothRealData) #convert data into pandas dataframe
                bothRealData.columns=["operation1","sum1","operation2","sign","sum2","totalSum","showCorrectAns","suggestedAnswer","mathResponse","mathResponseCorrect", "solveMathRT", "trueFalseRT", "setSize","setNumber","trialPerSet", "lettersShown", "lettersRecall", "correctCount", "percentCorrectMath", "totalMathErrorsInSet"]# add column names
                bothRealData = bothRealData.iloc[1: , :] # drop the first row which are the variable bothPracticeData.iloc[1: , :] # drop the first row which are the variable names
            filenameBothReal =dataDirectoryPath + "cgeOSPANbothReal_" + "sub" + subID + "_" + datetime + ".csv"; # make filename
            bothRealData.to_csv(filenameBothReal)
            
            
            
            
        
