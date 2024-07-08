#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep 21 09:18:44 2022

@author: shlab
"""

"""
symmetry span task for HRB dissertation project: Risk, context and strategy.

For this study, we are running the shortened version (Foster et al 2014) of both the operation span and symmetry span tasks.
We are doing one block for each. 

The structure of the complex span tasks are very similar: instructions, practice letters/red square only, practice distractors only, the practice both.
 
"""

def symSpanTask(subID, isReal, dirName, dataDirName):
        
    #subID = '001' # for testing
    
    try: 
        # Import modules we need
        import os, random, time
        import pandas as pd
        import numpy as np
        from psychopy import visual, core, event, monitors
        import statistics
        import glob
        import re
        
        
        #change directory
        #os.chdir('/Users/shlab/Documents/GitHub/rcs/task/symspan')
        os.chdir(dirName + os.sep + 'symspan')
        
        # set up data directory path
        #dataDirectoryPath = '/Users/shlab/Documents/Github/rcs/task/data/'
        #dataDirectoryPath = dirName + 'data/'
        dataDirectoryPath = dataDirName + "symspanData" + os.sep
        
        
        #load stimuli
        # realSymmImageNames = glob.glob('/Users/shlab/Documents/GitHub/rcs/task/symspan/symBitmaps/symm*.bmp')
        # exampleSymmImageNames = glob.glob('/Users/shlab/Documents/Github/rcs/task/symspan/exampleSymBitmaps/*.bmp')
        # practiceSymmImageNames = glob.glob('/Users/shlab/Documents/Github/rcs/task/symspan/practiceSymBitmaps/*.bmp')
        
        realSymmImageNames = glob.glob(dirName + os.sep + 'symspan/symBitmaps/symm*.bmp')
        exampleSymmImageNames = glob.glob(dirName + os.sep + 'symspan/exampleSymBitmaps/*.bmp')
        practiceSymmImageNames = glob.glob(dirName + os.sep + 'symspan/practiceSymBitmaps/*.bmp')
        
    
        # add the image number and correct response to our data variables that store the filename of the practice images
        practiceImageNumber = []
        for filename in practiceSymmImageNames:
            n = re.search('(?<=symm)\w+', filename)
            practiceImageNumber.append(int(n[0]))
        
        
        practiceSymmImageNames = pd.DataFrame(practiceSymmImageNames)
        practiceSymmImageNames.columns = ["imageFilePath"]
        practiceSymmImageNames["imageNumber"] = practiceImageNumber
        practiceSymmImageNames["symmetrical"] = np.nan
        practiceSymmImageNames.symmetrical[practiceSymmImageNames.imageNumber>=8] = 0  # image numbers greater than 7 are not symmetrical
        practiceSymmImageNames.symmetrical[practiceSymmImageNames.imageNumber<8] = 1 # image numbers 1-7 are symmetrical
        
        
        # add the image number and correct response to our data variables that store the filename of the real images
        realImageNumber = []
        for filename in realSymmImageNames:
            n = re.search('(?<=symm)\w+', filename)
            realImageNumber.append(int(n[0]))
        
        
        realSymmImageNames = pd.DataFrame(realSymmImageNames)
        realSymmImageNames.columns = ["imageFilePath"]
        realSymmImageNames["imageNumber"] = realImageNumber
        realSymmImageNames["symmetrical"] = np.nan
        realSymmImageNames.symmetrical[realSymmImageNames.imageNumber>=25] = 0  # image numbers greater than 24 are not symmetrical
        realSymmImageNames.symmetrical[realSymmImageNames.imageNumber<25] = 1 # image numbers 1-24 are symmetrical
        
                                  
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
        boxLetterSize = 90; # letter box size for recall screen
        matrixBoxSize = 130
        
        
        exampleSymImagePosition = [0, scrnsize[1]*.18] # position of the example symmetry images for instructions
        exampleSymTextPosition = [0,scrnsize[1]*-.25]
        
        
                
        if isReal == 1:
            nTsymprac = 15
            nTsquarePractice = 4
            nTbothPrac = 3
            nTbothReal = 4
        else: 
            nTsymprac = 3
            nTsquarePractice = 2
            nTbothPrac = 3
            nTbothReal = 4
        
        
        
        # Set up the window
        win = visual.Window(
            size=scrnsize,
            units="pix",
            fullscr=True,
            color=[-1, -1, -1], #black screen
            screen=1 # on second screen
        )
        
        
        # set up stimuli
        
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
            text= "In this task you will try to memorize the position of colored squares you see on the screen while you also make judgments about other pictures. \n\nIn the next few minutes, you will have some practice to get you familiar with how the experiment works. \n\nWe will begin by practicing the 'square' part of the experiment. \n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight, 
            wrapWidth=wrap,
            alignText="left"
        )
        
        generalInstructionsPg2 = visual.TextStim(
            win,
            text = "For this practice set, squares will appear on the screen one at a time. \n\nTry to remember where each square was, in the order it was presented in. \n\nAfter 2 - 5 squares have been shown, you will see a grid of the 16 possible places the squares could have been. \n\nYour job is to select each square in the order presented. \n\nTo do this, use the mouse to select the appropriate boxes. \n\nThe squares you select will turn red.\n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        generalInstructionsPg3 = visual.TextStim(
            win,
            text = "When you have selected all the squares, and they are in the correct order, hit the ENTER box at the bottom right of the screen. \n\nIf you make a mistake, hit the CLEAR box to start over. \n\nIf you forget one of the squares, click the BLANK box to mark the spot for the missing square. \n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        generalInstructionsPg4 = visual.TextStim(
            win,
            text = "It is very important to get the squares in the same order as you see them. \n\nIf you forget one, use the BLANK box to mark the position. \n\nPlease ask the experimenter any questions you may have at this time. \n\n\nWhen you are ready, press 'enter' to start the square practice.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        # symmetry instructions
        symmetryInstructionsPg1 = visual.TextStim(
            win,
            text = "Now you will practice doing the symmetry part of the experiment. \n\nA picture will appear on the screen, and you will have to decide if it is symmetrical. \n\nA picture is symmetrical if you can fold it in half vertically and the picture on the left lines up with the picture on the right. \n\nOn the next screen you will see a picture that IS SYMMETRICAL. \n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        symmetryInstructionsPg2_symmEx1 = visual.TextStim(
            win,
            text = "Notice that this picture is symmetrical about the red line. \n\nIn the real pictures the red line will not be present. \n\nPress 'enter' to continue.",
            pos = exampleSymTextPosition,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        exampleSymmImage1 = visual.ImageStim(
            win, 
            image = exampleSymmImageNames[0],
            size = [scrnsize[0]*.5,scrnsize[1]*.5],
            pos = exampleSymImagePosition
        )
        
        symmetryInstructionsPg3_symmEx2 = visual.TextStim(
            win,
            text = "Here, the picture is NOT symmetrical. \n\nIf you folded this across the red line, the boxes would NOT line up. \n\nPress 'enter' to continue.",
            pos = exampleSymTextPosition,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        exampleSymmImage2 = visual.ImageStim(
            win, 
            image = exampleSymmImageNames[1],
            size = [scrnsize[0]*.5,scrnsize[1]*.5],
            pos = exampleSymImagePosition
        )
        
        
        symmetryInstructionsPg4_symmEx3 = visual.TextStim(
            win,
            text = "This is another example of a picture that IS symmetrical. \n\nIf you folded it vertically, the two sides would line up. \n\nPress 'enter' to continue.",
            pos = exampleSymTextPosition,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        exampleSymmImage3 = visual.ImageStim(
            win, 
            image = exampleSymmImageNames[2],
            size = [scrnsize[0]*.5,scrnsize[1]*.5],
            pos = exampleSymImagePosition
        )
        
        symmetryInstructionsPg5_symmEx4 = visual.TextStim(
            win,
            text = "Here is another example of a picture that is NOT symmetrical. If folded, the two sides would not line up. \n\nIf you have any questions about how symmetry works, please ask the experimenter now.\n\nPress 'enter' to continue.",
            pos = exampleSymTextPosition,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        exampleSymmImage4 = visual.ImageStim(
            win, 
            image = exampleSymmImageNames[3],
            size = [scrnsize[0]*.5,scrnsize[1]*.5],
            pos = exampleSymImagePosition
        )
        
        symmetryInstructionsPg6 = visual.TextStim(
            win,
            text = "\nOnce you have decided if the picture is symmetrical, click the mouse. \n\nOn the next screen a YES and NO box will appear.\n\nIf the picture you saw was symmetrical, click the YES box.\n\nIf it was not symmetrical, click the NO box.\n\nAfter you click one of the boxes, the computer will tell you if you made the right choice.\n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        symmetryInstructionsPg7 = visual.TextStim(
            win,
            text = "It is VERY important that you get the pictures correct. \n\nPlease ask the experimenter any questions you may have at this time. \n\n\nWhen you are ready, press 'enter' to try some practice problems.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        
        symmetryInstructionsReDoPg1 = visual.TextStim(
            win, 
            text = "You did not have enough correct symmetry problems. \n\nThere will be one more round of the symmetry practice. \n\nIt is important that you get the symmetry problems correct and solve them as quickly as you can. \n\nPlease ask the experimenter any questions you have now. \n\n\nWhen you're ready, press 'enter' to try some practice problems.",
            pos= center,
            color="white", 
            height = textHeight,
            wrapWidth = wrap,
            alignText="left"
        )
        
        symmetryInstructionsEndofTask = visual.TextStim(
            win, 
            text = "You did not have enough correct symmetry problems on the second round of the symmetry practice. \n\nThe experiment will now end. \n\nThank you for your time.",
            pos= center,
            color="white", 
            height = textHeight,
            wrapWidth = wrap,
            alignText="left"
        )
        
        # symmetry - red square practice instructions
        
        bothInstructionsPg1 = visual.TextStim(
            win,
            text = "Now you will practice doing both parts of the experiment at the same time. \n\nIn the next practice set, you will be given one of the symmetry problems. \n\nOnce you make your decision about the picture, a square will appear on the screen. \n\nTry and remember the position of the square. \n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
    
        bothInstructionsPg2 = visual.TextStim(
            win,
            text = "In the previous section where you only decided about the picture symmetry, the computer computed your average time to solve the problems. \n\nIf you take longer than your average time, the computer will automatically move you onto the square part, thus skipping the YES or NO part and will count that problem as an error. \n\nTherefore it is VERY important to solve the symmetry problems as quickly and as accurately as possible. \n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        bothInstructionsPg3 = visual.TextStim(
            win,
            text = "After the square goes away, another symmetry picture will appear, and then another square. \n\nAt the end of each set of pictures and squares, a recall screen will appear. \n\nUse the mouse to select the squares you just saw. \n\nTry your best to get the squares in the correct order. \n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        bothInstructionsPg4 = visual.TextStim(
            win,
            text = "It is important to work QUICKLY and ACCURATELY. \n\nYou will not be told if your answer to the symmetry picture is correct. \n\nBut, after the square recall screen, you will be given feedback about your performance regarding both the number of squares recalled and the percent correct on the symmetry problems. \n\nPlease ask the experimenter any questions you may have at this time.\n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        
        bothInstructionsPg5 = visual.TextStim(
            win,
            text = "During the feedback, you will see a number in red in the top right of the screen. \n\nThis indicates your percent correct for the symmetry pictures for the entire experiment. \n\nIt is VERY important for you to keep this at least at 85%. \n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        bothInstructionsPg6 = visual.TextStim(
            win,
            text = "For our purposes, we can only use your data if you are at least 85% accurate on the symmetry pictures. \n\nYou must perform at least at 85% on the symmetry problems WHILE doing your best to recall as many squares as possible. \n\nPlease ask the experimenter any questions you may have at this time. \n\n\nPress 'enter' to try some practice problems.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        realInstructionsPg1 = visual.TextStim(
            win,
            text = "That is the end of the practice. \n\nThe real trials will look just like the practice trials you just completed. \n\nSome sets will have more problems than others. \n\nIt is important that you do your best on both the symmetry and the square parts of this experiment. \n\nAlso, remember to keep your symmetry accuracy at 85% or above. \n\n\nPress 'enter' to continue.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        realInstructionsPg2 = visual.TextStim(
            win,
            text = "The experimenter will now leave the room. \n\n\nWhen you're ready to begin the task, press 'enter'.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        endOfTask = visual.TextStim(
            win,
            text = "This task is complete. Thank you! \n\n\nPress the white call button to get the experimenter.",
            pos = center,
            color="white",
            height = textHeight,
            wrapWidth=wrap,
            alignText="left"
        )
        
        
        # red square stimuli (with matrix for response)
        
        COL1horiz_center = scrnsize[0]*-.18
        COL2horiz_center = scrnsize[0]*-.06
        COL3horiz_center = scrnsize[0]*.06
        COL4horiz_center = scrnsize[0]*.18
        
        # define location for each column of the recall matrix  
        
        # originally, we had recall screen being slightly off center but we want to show the red squares in the center
        #COL1horiz = scrnsize[0]*-.34
        #COL2horiz = scrnsize[0]*-.22
        #COL3horiz = scrnsize[0]*-.1
        #COL4horiz = scrnsize[0]*.02
        
        COL1horiz = COL1horiz_center
        COL2horiz = COL2horiz_center
        COL3horiz = COL3horiz_center
        COL4horiz = COL4horiz_center
        
        ROW1vert = scrnsize[1]*.225
        ROW2vert = scrnsize[1]*.075
        ROW3vert = scrnsize[1]*-.075
        ROW4vert = scrnsize[1]*-.225
        

        
        
 
        
        # make vector of locations for each of the ssquares in the matrix recall
        matrixRecallPositions = ([COL1horiz,ROW1vert],[COL2horiz,ROW1vert],[COL3horiz,ROW1vert],[COL4horiz,ROW1vert],
                           [COL1horiz,ROW2vert],[COL2horiz,ROW2vert],[COL3horiz,ROW2vert],[COL4horiz,ROW2vert],
                           [COL1horiz,ROW3vert],[COL2horiz,ROW3vert],[COL3horiz,ROW3vert],[COL4horiz,ROW3vert],
                           [COL1horiz,ROW4vert],[COL2horiz,ROW4vert],[COL3horiz,ROW4vert],[COL4horiz,ROW4vert]) 
        
        
        # make vector locations for each of the squares for the matrix centered on the screen
        matrixCenterPositions = ([COL1horiz_center,ROW1vert],[COL2horiz_center,ROW1vert],[COL3horiz_center,ROW1vert],[COL4horiz_center,ROW1vert],
                           [COL1horiz_center,ROW2vert],[COL2horiz_center,ROW2vert],[COL3horiz_center,ROW2vert],[COL4horiz_center,ROW2vert],
                           [COL1horiz_center,ROW3vert],[COL2horiz_center,ROW3vert],[COL3horiz_center,ROW3vert],[COL4horiz_center,ROW3vert],
                           [COL1horiz_center,ROW4vert],[COL2horiz_center,ROW4vert],[COL3horiz_center,ROW4vert],[COL4horiz_center,ROW4vert]) 
        
        

        
        recallBox_r1c1 = visual.Rect(
            win, 
            name='0',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[0], 
            fillColor="white" #white
        )
        
        
        recallBox_r1c2 = visual.Rect(
            win, 
            name='1',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[1], 
            fillColor=[1,1,1] #white
        )
        
        recallBox_r1c3 = visual.Rect(
            win, 
            name='2',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[2], 
            fillColor=[1,1,1] #white
        )
        
        recallBox_r1c4 = visual.Rect(
            win, 
            name='3',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[3], 
            fillColor=[1,1,1] #white
        )
        
        
        recallBox_r2c1 = visual.Rect(
            win, 
            name='4',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[4], 
            fillColor="white" #white
        )
        
        recallBox_r2c2 = visual.Rect(
            win, 
            name='5',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[5], 
            fillColor="white" #white
        )
        
        recallBox_r2c3 = visual.Rect(
            win, 
            name='6',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[6], 
            fillColor="white" #white
        )
        
        recallBox_r2c4 = visual.Rect(
            win, 
            name='7',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[7],  
            fillColor="white" #white
        )
        
        recallBox_r3c1 = visual.Rect(
            win, 
            name='8',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[8], 
            fillColor="white" #white
        )
        
        recallBox_r3c2 = visual.Rect(
            win, 
            name='9',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[9], 
            fillColor="white" #white
        )
        
        recallBox_r3c3 = visual.Rect(
            win, 
            name='10',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[10], 
            fillColor="white" #white
        )
        
        recallBox_r3c4 = visual.Rect(
            win, 
            name='11',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[11], 
            fillColor="white" #white
        )
        
        recallBox_r4c1 = visual.Rect(
            win, 
            name='12',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[12], 
            fillColor="white" #white
        )
        
        recallBox_r4c2 = visual.Rect(
            win, 
            name='13',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[13], 
            fillColor="white" #white
        )
        
        recallBox_r4c3 = visual.Rect(
            win, 
            name='14',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[14], 
            fillColor="white" #white
        )
        
        recallBox_r4c4 = visual.Rect(
            win, 
            name='15',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixRecallPositions[15], 
            fillColor="white" #white
        )
        
        # will set location and text dynamically during recall
        recallBoxNumber1 = visual.TextStim(
            win, 
            name='0',
            text ='1',
            pos = recallBox_r1c1.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber2 = visual.TextStim(
            win, 
            name='1',
            text ='2',
            pos = recallBox_r1c2.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber3 = visual.TextStim(
            win, 
            name='2',
            text ='3',
            pos = recallBox_r1c3.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber4 = visual.TextStim(
            win, 
            name='3',
            text ='4',
            pos = recallBox_r1c4.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber5 = visual.TextStim(
            win, 
            name='4',
            text ='5',
            pos = recallBox_r2c1.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber6 = visual.TextStim(
            win, 
            name='5',
            text ='6',
            pos = recallBox_r2c2.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber7 = visual.TextStim(
            win, 
            name='6',
            text ='7',
            pos = recallBox_r2c3.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber8 = visual.TextStim(
            win, 
            name='7',
            text ='8',
            pos = recallBox_r2c4.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber9 = visual.TextStim(
            win, 
            name='8',
            text ='9',
            pos = recallBox_r3c1.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber10 = visual.TextStim(
            win, 
            name='9',
            text ='10',
            pos = recallBox_r3c2.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber11 = visual.TextStim(
            win, 
            name='10',
            text ='11',
            pos = recallBox_r3c3.pos,
            color="white", 
            height=textHeight
        )
        
        
        recallBoxNumber12 = visual.TextStim(
            win, 
            name='11',
            text ='12',
            pos = recallBox_r3c4.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber13 = visual.TextStim(
            win, 
            name='12',
            text ='13',
            pos = recallBox_r4c1.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber14 = visual.TextStim(
            win, 
            name='13',
            text ='14',
            pos = recallBox_r4c2.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber15 = visual.TextStim(
            win, 
            name='14',
            text ='15',
            pos = recallBox_r4c3.pos,
            color="white", 
            height=textHeight
        )
        
        recallBoxNumber16 = visual.TextStim(
            win, 
            name='15',
            text ='16',
            pos = recallBox_r4c4.pos,
            color="white", 
            height=textHeight
        )
        
        
        box_r1c1 = visual.Rect(
            win, 
            name='0',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[0], 
            fillColor="white" #white
        )
        
        
        box_r1c2 = visual.Rect(
            win, 
            name='1',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[1], 
            fillColor=[1,1,1] #white
        )
        
        box_r1c3 = visual.Rect(
            win, 
            name='2',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[2], 
            fillColor=[1,1,1] #white
        )
        
        box_r1c4 = visual.Rect(
            win, 
            name='3',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[3], 
            fillColor=[1,1,1] #white
        )
        
        
        box_r2c1 = visual.Rect(
            win, 
            name='4',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[4], 
            fillColor="white" #white
        )
        
        box_r2c2 = visual.Rect(
            win, 
            name='5',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[5], 
            fillColor="white" #white
        )
        
        box_r2c3 = visual.Rect(
            win, 
            name='6',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[6], 
            fillColor="white" #white
        )
        
        box_r2c4 = visual.Rect(
            win, 
            name='7',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[7],  
            fillColor="white" #white
        )
        
        box_r3c1 = visual.Rect(
            win, 
            name='8',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[8], 
            fillColor="white" #white
        )
        
        box_r3c2 = visual.Rect(
            win, 
            name='9',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[9], 
            fillColor="white" #white
        )
        
        box_r3c3 = visual.Rect(
            win, 
            name='10',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[10], 
            fillColor="white" #white
        )
        
        box_r3c4 = visual.Rect(
            win, 
            name='11',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[11], 
            fillColor="white" #white
        )
        
        box_r4c1 = visual.Rect(
            win, 
            name='12',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[12], 
            fillColor="white" #white
        )
        
        box_r4c2 = visual.Rect(
            win, 
            name='13',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[13], 
            fillColor="white" #white
        )
        
        box_r4c3 = visual.Rect(
            win, 
            name='14',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[14], 
            fillColor="white" #white
        )
        
        box_r4c4 = visual.Rect(
            win, 
            name='15',
            width=matrixBoxSize, 
            height=matrixBoxSize, 
            units='pix', 
            pos=matrixCenterPositions[15], 
            fillColor="white" #white
        )
        
        
        redBox = visual.Rect(
            win,
            name="redbox",
            width = matrixBoxSize,
            height=matrixBoxSize,
            units='pix',
            fillColor="red"
        ) # position of this square gets updated on each trial
        
        blankButton = visual.TextStim(
            win, 
            text='BLANK', 
            pos = [scrnsize[0]*.38,scrnsize[1]*.22],
            color="black", 
            height=50
        )
        
        blankButtonBox = visual.Rect(
            win, 
            name='*',
            width=boxLetterSize*2.25, 
            height=boxLetterSize*1.5, 
            units='pix', 
            pos=blankButton.pos, 
            fillColor=[1,1,1] #white
        )
        
        
        clearButton = visual.TextStim(
            win, 
            text='CLEAR', 
            pos = [scrnsize[0]*.38,scrnsize[1]*0],
            color="black", 
            height=50
        )
        
        clearButtonBox = visual.Rect(
            win, 
            width=boxLetterSize*2.25, 
            height=boxLetterSize*1.5, 
            units='pix', 
            pos=clearButton.pos, 
            fillColor="red" #white
        )
        
        
        enterButton = visual.TextStim(
            win, 
            text='ENTER', 
            pos = [scrnsize[0]*.38,scrnsize[1]*-.22],
            color="black", 
            height=50
        )
        
        enterButtonBox = visual.Rect(
            win, 
            width=boxLetterSize*2.25, 
            height=boxLetterSize*1.5, 
            units='pix', 
            pos=enterButton.pos, 
            fillColor=[0,.6,0] #white
        )
        
        squareRecallText = visual.TextStim(
            win,
            pos = [0,scrnsize[1]*.4],
            color="white",
            height=textHeight,
            text="Select the squares in the order presented. \nUse the blank button to fill in forgotten squares.",
            wrapWidth=wrap
        )
        
        
        showSquareResponse = visual.TextStim(
            win, 
            pos = [COL2horiz+57, ROW4vert-125],
            color="blue", 
            height=textHeight*1.5,
            wrapWidth=wrap,
            #alignText="left"
        )
        
        squareFeedbackText = visual.TextStim(
            win,
            pos = [0,0],
            color="white",
            height = textHeight,
            wrapWidth = wrap
        )
        
        ## SYMMETRY STIMULI
        symImage = visual.ImageStim(
            win, 
            size =[scrnsize[0]*.7,scrnsize[1]*.7],
            pos = [0,0]
        )
        
        symClickMouse = visual.TextStim(
            win, 
            pos = [0,scrnsize[1]*-.4],
            color="white",
            height = textHeight,
            text= "\nWhen you have solved the symmetry problem, click the mouse to continue.",
            wrapWidth=wrap
        )
        
        symYesButton = visual.TextStim(
            win, 
            text='YES', 
            pos = [scrnsize[0]*-.3,scrnsize[1]*-.2],
            color="black", 
            height=70
        )
        
        symYesBox = visual.Rect(
            win, 
            width=boxLetterSize*3, 
            height=boxLetterSize*1.5, 
            units='pix', 
            pos=symYesButton.pos, 
            fillColor="white",
            name = "Yes"
        )
        
        symNoButton= visual.TextStim(
            win, 
            text='NO', 
            pos = [scrnsize[0]*.3,scrnsize[1]*-.2],
            color="black", 
            height=70
        )
        symNoBox = visual.Rect(
            win, 
            width=boxLetterSize*3, 
            height=boxLetterSize*1.5, 
            units='pix', 
            pos=symNoButton.pos, 
            fillColor="white",
            name="No"
        )
        
        symPracFeedback = visual.TextStim(
            win,
            pos = [0,scrnsize[1]*-.4],
            color="green",
            height = textHeight
        )
        
        symPracIsThisSymmetrical = visual.TextStim(
            win,
            text = "Is this image symmetrical?",
            pos =[0,0],
            color="white",
            height = 70, 
            wrapWidth=wrap
        )
        
        symErrorsAfterRecall = visual.TextStim(
            win, 
            pos = [0,scrnsize[1]*-.2], 
            color="white",
            height = textHeight, 
            wrapWidth = wrap
        )
        
        symTotalPercentCorrect = visual.TextStim(
            win,
            pos = [scrnsize[0]*.4, scrnsize[1]*.4],
            color="red",
            height = boxLetterSize
        )
        
        # general stimlui
        
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
        
        
        # Do red square practice noly
        # 4 trials with set sizes {2,2,3,3}, uses images to show red square matrices
        
        # INSTRUCTIONS
        
        # first draw for forced viewing time
        generalInstructionsPg1.draw()
        instructCount.text=text="screen 1/4"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)
        
        # then draw for response time
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
        
        # Determine the location of the red square for each trial/setsize
        # 4 trials, set sizes = 2,2,3,3 (order random across participants)
        nTsquarePractice = 4
        setSizeSquarePrac = [2,2,3,3]
        random.shuffle(setSizeSquarePrac)
        
        squarePracticeData = [] # create data structure with column names
        squarePracticeData.append(
            [
                "setSize", 
                "squareLocation",
                "squareNumber",
                "response",
                "responseCorrect",
                "trial"
            ]
        )
        
        #for s in range(len(setSizeSquarePrac)):
        for s in range(nTsquarePractice):    
            tmpSquareLocShown = random.sample(matrixCenterPositions, setSizeSquarePrac[s]) # select locations in matrix to show as red
            
            tmpSquareNumberShown =[]
            
            for t in range(setSizeSquarePrac[s]):
                
                tmpSquareNumberShown.append(matrixCenterPositions.index(tmpSquareLocShown[t])) # save the square number that is shown on each trial
                
                # Show the matrix with red squares
                redBox.pos = tmpSquareLocShown[t] # set redBox position
        
                box_r1c1.draw()
                box_r1c2.draw()
                box_r1c3.draw()
                box_r1c4.draw()
                box_r2c1.draw()
                box_r2c2.draw()
                box_r2c3.draw()
                box_r2c4.draw()
                box_r3c1.draw()
                box_r3c2.draw()
                box_r3c3.draw()
                box_r3c4.draw()
                box_r4c1.draw()
                box_r4c2.draw()
                box_r4c3.draw()
                box_r4c4.draw()
                redBox.draw()
        
                
                
                win.flip()
                core.wait(.650) # 1s letter display
                
                #if not t == setSizeSquarePrac[s]-1: # dont show isi after the last square is shown
                fixationScreen.draw()
                win.flip()
                core.wait(.5) # 500ms isi
                
            # show the recall screen and record responses
             
            blankScreen.draw()
            win.flip()
            core.wait(1) # 1s blank screen before letter grid recall screen
            
        
            recallBox_r1c1.autoDraw=True
            recallBox_r1c2.autoDraw=True
            recallBox_r1c3.autoDraw=True
            recallBox_r1c4.autoDraw=True
            recallBox_r2c1.autoDraw=True
            recallBox_r2c2.autoDraw=True
            recallBox_r2c3.autoDraw=True
            recallBox_r2c4.autoDraw=True
            recallBox_r3c1.autoDraw=True
            recallBox_r3c2.autoDraw=True
            recallBox_r3c3.autoDraw=True
            recallBox_r3c4.autoDraw=True
            recallBox_r4c1.autoDraw=True
            recallBox_r4c2.autoDraw=True
            recallBox_r4c3.autoDraw=True
            recallBox_r4c4.autoDraw=True

            recallBoxNumber1.autoDraw =True
            recallBoxNumber2.autoDraw =True
            recallBoxNumber3.autoDraw =True
            recallBoxNumber4.autoDraw =True
            recallBoxNumber5.autoDraw =True
            recallBoxNumber6.autoDraw =True
            recallBoxNumber7.autoDraw =True
            recallBoxNumber8.autoDraw =True
            recallBoxNumber9.autoDraw =True
            recallBoxNumber10.autoDraw =True
            recallBoxNumber11.autoDraw =True
            recallBoxNumber12.autoDraw =True
            recallBoxNumber13.autoDraw =True
            recallBoxNumber14.autoDraw =True
            recallBoxNumber15.autoDraw =True
            recallBoxNumber16.autoDraw =True
    
            blankButtonBox.autoDraw=True
            blankButton.autoDraw=True
            clearButtonBox.autoDraw=True
            clearButton.autoDraw=True
            enterButtonBox.autoDraw=True
            enterButton.autoDraw=True
            squareRecallText.autoDraw=True
        
        
            win.flip()
            
            # RECORD THE response locations AND change clicked boxes to red
            # set up the mouse, it will be called "myMouse"
            myMouse = event.Mouse(visible = True, win = win) 
            myMouse.setPos(newPos =[0,0]); # set mouse to be in the middle of the screen
        
        
            # initiate the response variable where we will store the participants' responses
            tmpSquareRecall = []; 
        
            # store the possible shapes that participants can click on during the recall period (this doesn't include the enter box)
            boxes =[recallBox_r1c1, recallBox_r1c2, recallBox_r1c3, recallBox_r1c4, 
                    recallBox_r2c1, recallBox_r2c2, recallBox_r2c3, recallBox_r2c4, 
                    recallBox_r3c1, recallBox_r3c2, recallBox_r3c3, recallBox_r3c4,
                    recallBox_r4c1, recallBox_r4c2, recallBox_r4c3, recallBox_r4c4,
                    blankButtonBox, clearButtonBox]
             
            recallBoxes = [recallBoxNumber1, recallBoxNumber2, recallBoxNumber3, recallBoxNumber4, 
                    recallBoxNumber5, recallBoxNumber6, recallBoxNumber7, recallBoxNumber8, 
                    recallBoxNumber9, recallBoxNumber10, recallBoxNumber11, recallBoxNumber12,
                    recallBoxNumber13, recallBoxNumber14, recallBoxNumber15, recallBoxNumber16]

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
                        tmpSquareRecall.append(box.name)
                        myMouse.clickReset()
                        timeAfterClick=0
        
                        if box == clearButtonBox: # if clear button is pressed, reset everything
                            tmpSquareRecall=[]
                            recallBox_r1c1.color="white"
                            recallBox_r1c2.color="white" 
                            recallBox_r1c3.color="white"
                            recallBox_r1c4.color="white" 
                            recallBox_r2c1.color="white" 
                            recallBox_r2c2.color="white" 
                            recallBox_r2c3.color="white" 
                            recallBox_r2c4.color="white" 
                            recallBox_r3c1.color="white" 
                            recallBox_r3c2.color="white"
                            recallBox_r3c3.color="white" 
                            recallBox_r3c4.color="white"
                            recallBox_r4c1.color="white" 
                            recallBox_r4c2.color="white"
                            recallBox_r4c3.color="white" 
                            recallBox_r4c4.color="white"
                            recallBoxNumber1.color ="white"
                            recallBoxNumber2.color ="white"
                            recallBoxNumber3.color ="white"
                            recallBoxNumber4.color ="white"
                            recallBoxNumber5.color ="white"
                            recallBoxNumber6.color ="white"
                            recallBoxNumber7.color ="white"
                            recallBoxNumber8.color ="white"
                            recallBoxNumber9.color ="white"
                            recallBoxNumber10.color ="white"
                            recallBoxNumber11.color ="white"
                            recallBoxNumber12.color ="white"
                            recallBoxNumber13.color ="white"
                            recallBoxNumber14.color ="white"
                            recallBoxNumber15.color ="white"
                            recallBoxNumber16.color ="white"

                            
                #print(tmpSquareRecall)
                # change clicked boxes to be green (except theblank button, keep it white)
                for box in boxes:
                    if box.name in tmpSquareRecall and not box.name == blankButtonBox.name:
                        #print(box.name)
                        box.color = 'red'

                # when a participant clicks on a square, add the number of the square in the sequence
                    for r in range(len(tmpSquareRecall)):
                        if box.name in tmpSquareRecall[r] and not box.name == blankButtonBox.name:
                            #if recallBoxes[b].name ==box.name:
                            recallBoxes[int(tmpSquareRecall[r])].text = r+1
                            recallBoxes[int(tmpSquareRecall[r])].color = "black"
                            
                        
                        
            
                # prep the text that shows participant's responses (letters)
                #responseText='' 
                # for l in range(len(tmpSquareRecall)):
                #     responseText = "%s %s " % (responseText, tmpSquareRecall[l])
                    
                #responseText = "%s squares recalled" % (len(tmpSquareRecall))
        
                # draw the response text
                #showSquareResponse.text = responseText    
                #showSquareResponse.autoDraw=True
                win.flip()
                    
        
                # reset mouse
                myMouse.clickReset() 
                
            # turn off autodraw
            recallBox_r1c1.autoDraw=False
            recallBox_r1c2.autoDraw=False
            recallBox_r1c3.autoDraw=False
            recallBox_r1c4.autoDraw=False
            recallBox_r2c1.autoDraw=False
            recallBox_r2c2.autoDraw=False
            recallBox_r2c3.autoDraw=False
            recallBox_r2c4.autoDraw=False
            recallBox_r3c1.autoDraw=False
            recallBox_r3c2.autoDraw=False
            recallBox_r3c3.autoDraw=False
            recallBox_r3c4.autoDraw=False
            recallBox_r4c1.autoDraw=False
            recallBox_r4c2.autoDraw=False
            recallBox_r4c3.autoDraw=False
            recallBox_r4c4.autoDraw=False
        
            recallBoxNumber1.autoDraw =False
            recallBoxNumber2.autoDraw =False
            recallBoxNumber3.autoDraw =False
            recallBoxNumber4.autoDraw =False
            recallBoxNumber5.autoDraw =False
            recallBoxNumber6.autoDraw =False
            recallBoxNumber7.autoDraw =False
            recallBoxNumber8.autoDraw =False
            recallBoxNumber9.autoDraw =False
            recallBoxNumber10.autoDraw =False
            recallBoxNumber11.autoDraw =False
            recallBoxNumber12.autoDraw =False
            recallBoxNumber13.autoDraw =False
            recallBoxNumber14.autoDraw =False
            recallBoxNumber15.autoDraw =False
            recallBoxNumber16.autoDraw =False
        
            blankButtonBox.autoDraw=False
            blankButton.autoDraw=False
            clearButtonBox.autoDraw=False
            clearButton.autoDraw=False
            enterButtonBox.autoDraw=False
            enterButton.autoDraw=False
            squareRecallText.autoDraw=False
            showSquareResponse.autoDraw=False
        
            
            #reset box colors to white
            recallBox_r1c1.color="white"
            recallBox_r1c2.color="white" 
            recallBox_r1c3.color="white"
            recallBox_r1c4.color="white" 
            recallBox_r2c1.color="white" 
            recallBox_r2c2.color="white" 
            recallBox_r2c3.color="white" 
            recallBox_r2c4.color="white" 
            recallBox_r3c1.color="white" 
            recallBox_r3c2.color="white"
            recallBox_r3c3.color="white" 
            recallBox_r3c4.color="white"
            recallBox_r4c1.color="white" 
            recallBox_r4c2.color="white"
            recallBox_r4c3.color="white" 
            recallBox_r4c4.color="white"
            recallBoxNumber1.color ="white"
            recallBoxNumber2.color ="white"
            recallBoxNumber3.color ="white"
            recallBoxNumber4.color ="white"
            recallBoxNumber5.color ="white"
            recallBoxNumber6.color ="white"
            recallBoxNumber7.color ="white"
            recallBoxNumber8.color ="white"
            recallBoxNumber9.color ="white"
            recallBoxNumber10.color ="white"
            recallBoxNumber11.color ="white"
            recallBoxNumber12.color ="white"
            recallBoxNumber13.color ="white"
            recallBoxNumber14.color ="white"
            recallBoxNumber15.color ="white"
            recallBoxNumber16.color ="white"

            #provide feedback
            correctCount = 0
            if len(tmpSquareRecall) == len(tmpSquareNumberShown): # if participant recalls correct number of letters
                for l in range(setSizeSquarePrac[s]):
                    if tmpSquareRecall[l] == str(tmpSquareNumberShown[l]):
                        correctCount +=1
                squareFeedbackText.text = text = "You recalled %.0f squares correctly out of %.0f." % (correctCount, setSizeSquarePrac[s])
            elif len(tmpSquareRecall) > len(tmpSquareNumberShown): # if sub recalls more letters than set size
                for l in range(setSizeSquarePrac[s]):
                    if tmpSquareRecall[l] == str(tmpSquareNumberShown[l]):
                        correctCount +=1
                squareFeedbackText.text = text = "You recalled too many squares." 
            elif len(tmpSquareRecall)==0: # if participant does not recall any letters
                squareFeedbackText.text = text = "You did not recall any squares."
            elif (len(tmpSquareRecall)<len(tmpSquareNumberShown)) and not (len(tmpSquareRecall) ==0):
                #for l in range(setSizeLetterPrac[s]):
                for l in range(len(tmpSquareRecall)):
                    if tmpSquareRecall[l] == str(tmpSquareNumberShown[l]):
                        correctCount +=1
                squareFeedbackText.text = text = "You did not recall enough squares." 
        
        
            #letterFeedbackText.text = text = "You recalled %.0f letters correctly out of %.0f." % (correctCount, setSizeLetterPrac[s])
            squareFeedbackText.draw()
            win.flip()
            core.wait(1.5) # show feedback for 1.5 seconds
            
            blankScreen.draw()
            win.flip()
            core.wait(1) # blank screen for 1s before moving to next trial
            
            # after each trial, add data for:
            #lettersRecall.append(tmpLetterRecall) # recalled letters
            #lettersShown.append(tmpLettersShown) # letters displayed 
           
            squarePracticeData.append(
                [
                    setSizeSquarePrac[s], 
                    tmpSquareLocShown,
                    tmpSquareNumberShown,
                    tmpSquareRecall,
                    correctCount,
                    s
                ]
            )
            
        
        # change practice file into pandas dataframe
        squarePracticeData = pd.DataFrame(squarePracticeData) #convert data into pandas dataframe
        squarePracticeData.columns=["setSize","redSquarePos", "redSquareNumber","squareRecall","correctCount","trial"] # add column names
        squarePracticeData = squarePracticeData.iloc[1: , :] # drop the first row which are the variable names
            
            
        # Do symmetry ratings only
            # 15 trials where participants report whether the images are symmetrical (using true or false, like the ospan)
            
         # INSTRUCTIONS 
        
        symmetryInstructionsPg1.draw()
        instructCount.text=text="screen 1/7"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        symmetryInstructionsPg1.draw()
        instructCount.text=text="screen 1/7"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        symmetryInstructionsPg2_symmEx1.draw()
        exampleSymmImage1.draw()
        instructCount.text=text="screen 2/7"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        symmetryInstructionsPg2_symmEx1.draw()
        exampleSymmImage1.draw()
        instructCount.text=text="screen 2/7"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        symmetryInstructionsPg3_symmEx2.draw()
        exampleSymmImage2.draw()
        instructCount.text=text="screen 3/7"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        symmetryInstructionsPg3_symmEx2.draw()
        exampleSymmImage2.draw()
        instructCount.text=text="screen 3/7"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        symmetryInstructionsPg4_symmEx3.draw()
        exampleSymmImage3.draw()
        instructCount.text=text="screen 4/7"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        symmetryInstructionsPg4_symmEx3.draw()
        exampleSymmImage3.draw()
        instructCount.text=text="screen 4/7"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        symmetryInstructionsPg5_symmEx4.draw()
        exampleSymmImage4.draw()
        instructCount.text=text="screen 5/7"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        symmetryInstructionsPg5_symmEx4.draw()
        exampleSymmImage4.draw()
        instructCount.text=text="screen 5/7"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        symmetryInstructionsPg6.draw()
        instructCount.text=text="screen 6/7"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        symmetryInstructionsPg6.draw()
        instructCount.text=text="screen 6/7"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        symmetryInstructionsPg7.draw()
        instructCount.text=text="screen 7/7"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        symmetryInstructionsPg7.draw()
        instructCount.text=text="screen 7/7"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        
        blankScreen.draw()
        win.flip()
        core.wait(1)   
           
        # START SYMMETRY PRACTICE
        #nTsymprac = 15
        
        # set up mouse for true/false responses
        myMouse = event.Mouse(visible = True, win = win) 
        minFramesAfterClick = 10 # to prevent re-entering the if loop too early, other wise multiple letters are recorded during a single mouse click
        timeAfterClick = 0
        symSpanBoxes = [symYesBox, symNoBox]
        
        # shuffle the order of the practice images, reset rows to be 0-14
        practiceSymmImageNames = practiceSymmImageNames.sample(frac=1).reset_index(drop=True)
        
        symPracticeData = [] # create data structure with column names
        symPracticeData.append(
            [
                "imageName", 
                "symmetrical",
                "imageNumber",
                "response",
                "responseBinom",
                "responseCorrect",
                "solveSymRT",
                "yesNoRT",
                "trial"
            ]
        )
        
        for m in range(nTsymprac):
        
            # selectt he image to show on this trial
            selectedSymmImage = practiceSymmImageNames.imageFilePath[m]
        
            blankScreen.draw()
            win.flip()
            core.wait(.5) # blank screen for 500ms prior to each math operation
            
        
            # Show the image
            
            symImage.image = selectedSymmImage
            symImage.draw()
            symClickMouse.draw()
            
            
            buttons = [0]*len(event.mouseButtons) #initializes it to a list of 0s with the length equal to the number of active buttons.
            myMouse.setPos(newPos =[0,symNoBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
        
            
            win.flip() # show image
            
            
            myMouse.clickReset() # make sure mouseclick is reset to [0,0,0], restarts the clock
            
            while not any(buttons):
                (buttons,rtTimes) = myMouse.getPressed(getTime=True)
           
            tmpSymRT = rtTimes[0]
            
            #Draw the isi
            fixationScreen.draw() 
            win.flip()
            core.wait(.2) # 200ms isi
            
        
            # Show the prompt 'Is this symmetrical?" with "yes" and "no" buttons
            symPracIsThisSymmetrical.draw()
            symYesBox.draw()
            symYesButton.draw()
            symNoBox.draw()
            symNoButton.draw() 
            
            myMouse.setPos(newPos =[0,symNoBox.pos[1]]); # set mouse to be in the middle of the yes/no buttons
            win.flip()
        
            # collect response, record RT and check whether participant was correct.
            myMouse.clickReset() # make sure mouseclick is reset to [0,0,0]
        
            mouseResponse = 0;
            
            while mouseResponse == 0:        
                timeAfterClick += 1
        
                for box in symSpanBoxes:
                    if myMouse.isPressedIn(box) and timeAfterClick >= minFramesAfterClick: # slows things down so that multiple responses are not recorded for a single click
                        buttons, times = myMouse.getPressed(getTime=True)
                        tmpSymResp = box.name
                        if tmpSymResp == 'No':
                            tmpSymRespBinom = 0
                        elif tmpSymResp == 'Yes':
                            tmpSymRespBinom = 1
                        tmpSymRTyesNo = times[0]
        
                        
                        # once pressed, change box color to grey, redraw everything
                        box.color = "grey"
                        symPracIsThisSymmetrical.draw()
                        symYesBox.draw()
                        symYesButton.draw()
                        symNoBox.draw()
                        symNoButton.draw() 
                        
                        
                        # Show “correct” or “incorrect” on the true/false screen for 500ms
                        if tmpSymRespBinom == practiceSymmImageNames.symmetrical[m]:
                            respCorrect = 1
                            symPracFeedback.text = "Correct"
                            symPracFeedback.color = "green"
                        else:
                            respCorrect = 0
                            symPracFeedback.text = "Incorrect"
                            symPracFeedback.color = "red"
                        
                        symPracFeedback.draw()
                        win.flip()
                        core.wait(.5)
                        
                        box.color = "white" # reset box color to white
                        myMouse.clickReset()
                        timeAfterClick=0
                        mouseResponse =1 # change to 1 to end while loop
        
            symPracticeData.append(
                [
                    selectedSymmImage, 
                    practiceSymmImageNames.imageNumber[m],
                    practiceSymmImageNames.symmetrical[m],
                    tmpSymResp,
                    tmpSymRespBinom,
                    respCorrect,
                    tmpSymRT,
                    tmpSymRTyesNo,
                    m
                ]
            )
            
        # Reformat data to pandas dataframe
        symPracticeData = pd.DataFrame(symPracticeData)
        symPracticeData.columns = ["imageName","imageNumber","symmetrical","response","responseBinom", "responseCorrect","solveSymRT", "yesNoRT","trial"]
        symPracticeData = symPracticeData.iloc[1: , :] # drop the first row which are the variable names
        
        # check for correct symmetrical trials
        correctSymDF = symPracticeData.loc[symPracticeData["responseCorrect"]==1]
        
        if len(correctSymDF) >2:
            # calculate the cut off time for following sections of the task: average RT + 2.5* standard deviation RT
            avgRT = statistics.mean(correctSymDF["solveSymRT"])
            stdRT = statistics.stdev(correctSymDF["solveSymRT"])
            
            maxSymDisplay = avgRT + (2.5*stdRT) # calculate the max display for the math problems in the future sets
            if maxSymDisplay <1:
                maxSymDisplay = 1
            symPracticeData["maxSymDisplay"] = maxSymDisplay # save to the dataframe
        
        elif len(correctSymDF) <=2:
            # show screen that says they did not have any correct symmetry trials andthat thye have nother chance to do the practice.
        
            symmetryInstructionsReDoPg1.draw()
            win.flip()
            event.waitKeys(keyList=['return'], timeStamped= False)
            
            # set up mouse for true/false responses
            myMouse = event.Mouse(visible = True, win = win) 
            minFramesAfterClick = 10 # to prevent re-entering the if loop too early, other wise multiple letters are recorded during a single mouse click
            timeAfterClick = 0
            symSpanBoxes = [symYesBox, symNoBox]
        
            # shuffle the order of the practice images, reset rows to be 0-14
            practiceSymmImageNames = practiceSymmImageNames.sample(frac=1).reset_index(drop=True)
        
            symPracticeData = [] # create data structure with column names
            symPracticeData.append(
                [
                    "imageName", 
                    "symmetrical",
                    "imageNumber",
                    "response",
                    "responseBinom",
                    "responseCorrect",
                    "solveSymRT",
                    "yesNoRT",
                    "trial"
                ]
            )
        
            for m in range(nTsymprac):
        
                # selectt he image to show on this trial
                selectedSymmImage = practiceSymmImageNames.imageFilePath[m]
        
                blankScreen.draw()
                win.flip()
                core.wait(.5) # blank screen for 500ms prior to each math operation
                
        
                # Show the image
                
                symImage.image = selectedSymmImage
                symImage.draw()
                symClickMouse.draw()
                
                
                buttons = [0]*len(event.mouseButtons) #initializes it to a list of 0s with the length equal to the number of active buttons.
                myMouse.setPos(newPos =[0,symNoBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
        
                
                win.flip() # show image
                
                
                myMouse.clickReset() # make sure mouseclick is reset to [0,0,0], restarts the clock
                
                while not any(buttons):
                    (buttons,rtTimes) = myMouse.getPressed(getTime=True)
               
                tmpSymRT = rtTimes[0]
                
                #Draw the isi
                fixationScreen.draw() 
                win.flip()
                core.wait(.2) # 200ms isi
                
        
                # Show the prompt 'Is this symmetrical?" with "yes" and "no" buttons
                symPracIsThisSymmetrical.draw()
                symYesBox.draw()
                symYesButton.draw()
                symNoBox.draw()
                symNoButton.draw() 
                
                myMouse.setPos(newPos =[0,symNoBox.pos[1]]); # set mouse to be in the middle of the yes/no buttons
                win.flip()
        
                # collect response, record RT and check whether participant was correct.
                myMouse.clickReset() # make sure mouseclick is reset to [0,0,0]
        
                mouseResponse = 0;
                
                while mouseResponse == 0:        
                    timeAfterClick += 1
        
                    for box in symSpanBoxes:
                        if myMouse.isPressedIn(box) and timeAfterClick >= minFramesAfterClick: # slows things down so that multiple responses are not recorded for a single click
                            buttons, times = myMouse.getPressed(getTime=True)
                            tmpSymResp = box.name
                            if tmpSymResp == 'No':
                                tmpSymRespBinom = 0
                            elif tmpSymResp == 'Yes':
                                tmpSymRespBinom = 1
                            tmpSymRTyesNo = times[0]
        
                            
                            # once pressed, change box color to grey, redraw everything
                            box.color = "grey"
                            symPracIsThisSymmetrical.draw()
                            symYesBox.draw()
                            symYesButton.draw()
                            symNoBox.draw()
                            symNoButton.draw() 
                            
                            
                            # Show “correct” or “incorrect” on the true/false screen for 500ms
                            if tmpSymRespBinom == practiceSymmImageNames.symmetrical[m]:
                                respCorrect = 1
                                symPracFeedback.text = "Correct"
                                symPracFeedback.color = "green"
                            else:
                                respCorrect = 0
                                symPracFeedback.text = "Incorrect"
                                symPracFeedback.color = "red"
                            
                            symPracFeedback.draw()
                            win.flip()
                            core.wait(.5)
                            
                            box.color = "white" # reset box color to white
                            myMouse.clickReset()
                            timeAfterClick=0
                            mouseResponse =1 # change to 1 to end while loop
        
                symPracticeData.append(
                    [
                        selectedSymmImage, 
                        practiceSymmImageNames.symmetrical[m],
                        practiceSymmImageNames.imageNumber[m],
                        tmpSymResp,
                        tmpSymRespBinom,
                        respCorrect,
                        tmpSymRT,
                        tmpSymRTyesNo,
                        m
                    ]
                )
        
            # Reformat data to pandas dataframe
            symPracticeData = pd.DataFrame(symPracticeData)
            symPracticeData.columns = ["imageName","imageNumber","symmetrical","response","responseBinom", "responseCorrect","solveSymRT", "yesNoRT","trial"]
            symPracticeData = symPracticeData.iloc[1: , :] # drop the first row which are the variable names
        
            # check for correct symmetrical trials
            correctSymDF = symPracticeData.loc[symPracticeData["responseCorrect"]==1]
        
            if len(correctSymDF) >2:
                # calculate the cut off time for following sections of the task: average RT + 2.5* standard deviation RT
                avgRT = statistics.mean(correctSymDF["solveSymRT"])
                stdRT = statistics.stdev(correctSymDF["solveSymRT"])
                
                maxSymDisplay = avgRT + (2.5*stdRT) # calculate the max display for the math problems in the future sets
                if maxSymDisplay <1:
                    maxSymDisplay = 1
                symPracticeData["maxSymDisplay"] = maxSymDisplay # save to the dataframe
            
            elif len(correctSymDF) <=2:
                
                #display screen that says they still didnt get any math correct and the experiment is done.
                symmetryInstructionsEndofTask.draw()
                win.flip()
                event.waitKeys(keyList=['return'], timeStamped=False)
                win.close() # close screen
        
        
        
        
        # Red square - symmetry practice
            # 3 trials with set size = 2
        
         # INSTRUCTIONS 
        bothInstructionsPg1.draw()
        instructCount.text=text="screen 1/5"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        bothInstructionsPg1.draw()
        instructCount.text=text="screen 1/5"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        bothInstructionsPg2.draw()
        instructCount.text=text="screen 2/5"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        bothInstructionsPg2.draw()
        instructCount.text=text="screen 2/5"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        bothInstructionsPg3.draw()
        instructCount.text=text="screen 3/5"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        bothInstructionsPg3.draw()
        instructCount.text=text="screen 3/5"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        bothInstructionsPg4.draw()
        instructCount.text=text="screen 4/5"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        bothInstructionsPg4.draw()
        instructCount.text=text="screen 4/5"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        bothInstructionsPg5.draw()
        instructCount.text=text="screen 5/5"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        bothInstructionsPg5.draw()
        instructCount.text=text="screen 5/5"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        
        # set up symmetry stimuli
        #nTbothPrac = 3 # 3 trials for the box-sym practice
        setSize = ([2,2,2]) # number of box-sym pairs in each set
        
        bothPracDF = realSymmImageNames.sample(n=sum(setSize), axis="rows")
        
        # populate our data frame with some useful information
        setSizeLong = [] #to store the set size of each trial (options:2)
        triPerSet = [] # to store the trial number within a set (0 to 2)
        setNum=[] # to store which the current set (0-2 since there eare 3 sets)
        for set in setSize:
            for t in range(set):
                setSizeLong.append(set)
                triPerSet.append(t)
        
        
        # add those variables to our dataframe
        bothPracDF["setSize"] = setSizeLong 
        bothPracDF["trialPerSet"] = triPerSet 
        bothPracDF["setNumber"] = [0,0,1,1,2,2]
        
        
        # set up red square stimuli
        bothRealRedsquares = []
        redSquaresShortFormat = [] # each row has all squares shown (position)
        # select the location of red square we will show on each trial
        
        for t in range(nTbothPrac):
            tmpSquare = random.sample(matrixCenterPositions, k=2)# randomly select location of red squares, using sample instead of choices does without replacement
            # we don't want to have repeat squares in a single trial^
            redSquaresShortFormat.append(tmpSquare) # save them in short format
            
        
            for s in range(len(tmpSquare)):
                bothRealRedsquares.append(tmpSquare[s]) # also save red square locations in long format
                
                matrixCenterPositions.index(tmpSquare[s])
        
        bothPracDF["redSquarePosition"] = bothRealRedsquares # add red square locations that are shown following each symmetry operation
        
        
        # set up mouse
        myMouse = event.Mouse(visible = True, win = win) 
        minFramesAfterClick = 10 # to prevent re-entering the if loop too early, other wise multiple letters are recorded during a single mouse click
        timeAfterClick = 0
        symSpanBoxes = [symYesBox, symNoBox]
        
        
        # set up data
        bothPracticeData = [] # create data structure with column names
        bothPracticeData.append(
            [
                "imageName", 
                "imageNumber",
                "symmetrical",
                "symResponse",
                "symResponseBinom",
                "symResponseCorrect",
                "solveSymRT",
                "symYesNoRT",
                "setSize",
                "setNumber",
                "trialPerSet",
                "redSquarePos",
                "redSquareNumber",
                "squareRecall",
                "squareCorrectCount"
            ]
        )
        
        for t in range(nTbothPrac): # for each trial
            tmpSquareNumberShown = [] # reseet the red square number variable before the start of each set
            
            for s in range(setSize[t]): # and set size within each trial
            
            # pull out the row of information on each trial
                tmpRow = bothPracDF.loc[(bothPracDF.setNumber ==t) & (bothPracDF.trialPerSet==s)]
                tmpSquareNumberShown.append(matrixCenterPositions.index(tmpRow.redSquarePosition.iat[0])) # save the square number that is shown on each trial
        
            # set up symmetry image and red square location
                # select the image to show on this trial
                selectedSymmImage = tmpRow.imageFilePath.iat[0]
                redBox.pos = tmpRow.redSquarePosition.iat[0] # set redBox position
        
        
                blankScreen.draw()
                win.flip()
                core.wait(.5) # blank screen for 500ms prior to each math operation
                
        
                # Show the symmetry image
                
                symImage.image = selectedSymmImage
                symImage.draw()
                #symClickMouse.draw()
                
                
                buttons = [0]*len(event.mouseButtons) #initializes it to a list of 0s with the length equal to the number of active buttons.
                myMouse.setPos(newPos =[0,symNoBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
        
                
                win.flip() # show image
                
                myMouse.clickReset() # make sure mouseclick is reset to [0,0,0], restarts the clock
                mathSymClock = core.Clock() # start the clock
            
            # wait for math response to move onto math T/F
                while not any(buttons) and mathSymClock.getTime() <= maxSymDisplay:
                    (buttons,rtTimes) = myMouse.getPressed(getTime=True)
            
                #if participant does not respond in time on math, move to letter part
                if not any(buttons): 
                    tmpSymRT = float("nan") # record RT as nan
                    tmpSymResp = float("nan")
                    tmpSymRespBinom=float("nan")
                    respCorrect = 0
                    tmpSymRTyesNo = float("nan")
                    
                    # record data
                    bothPracticeData.append(
                        [
                            tmpRow.imageFilePath.iat[0], 
                            tmpRow.imageNumber.iat[0],
                            tmpRow.symmetrical.iat[0],
                            tmpSymResp,
                            tmpSymRespBinom,
                            respCorrect,
                            tmpSymRT,
                            tmpSymRTyesNo,
                            tmpRow.setSize.iat[0],
                            t,
                            tmpRow.trialPerSet.iat[0],
                            tmpRow.redSquarePosition.iat[0],
                            tmpSquareNumberShown
                            #'l', # tmp place holder for letters recalled
                            #0 # tmp place holder for correct count
                            
                        ]
                    )
                    
                else: # if participant does respond in time, move onto Yes/No
        
        
            
                    tmpSymRT = rtTimes[0]
                    
                    #Draw the isi
                    fixationScreen.draw() 
                    win.flip()
                    core.wait(.2) # 200ms isi
                    
            
                    # Show the prompt 'Is this symmetrical?" with "yes" and "no" buttons
                    symPracIsThisSymmetrical.draw()
                    symYesBox.draw()
                    symYesButton.draw()
                    symNoBox.draw()
                    symNoButton.draw() 
                    
                    myMouse.setPos(newPos =[0,symNoBox.pos[1]]); # set mouse to be in the middle of the yes/no buttons
                    win.flip()
        
                    # collect response, record RT and check whether participant was correct.
                    myMouse.clickReset() # make sure mouseclick is reset to [0,0,0]
            
                    mouseResponse = 0;
                    
                    while mouseResponse == 0:        
                        timeAfterClick += 1
            
                        for box in symSpanBoxes:
                            if myMouse.isPressedIn(box) and timeAfterClick >= minFramesAfterClick: # slows things down so that multiple responses are not recorded for a single click
                                buttons, times = myMouse.getPressed(getTime=True)
                                tmpSymResp = box.name
                                if tmpSymResp == 'No':
                                    tmpSymRespBinom = 0
                                elif tmpSymResp == 'Yes':
                                    tmpSymRespBinom = 1
                                tmpSymRTyesNo = times[0]
            
                                
                                # once pressed, change box color to grey, redraw everything
                                box.color = "grey"
                                symPracIsThisSymmetrical.draw()
                                symYesBox.draw()
                                symYesButton.draw()
                                symNoBox.draw()
                                symNoButton.draw() 
                                
                                
                                # Show “correct” or “incorrect” on the yes/no screen for 500ms
                                if tmpSymRespBinom == tmpRow.symmetrical.iat[0]:
                                    respCorrect = 1
                                    symPracFeedback.text = "Correct"
                                    symPracFeedback.color = "green"
                                else:
                                    respCorrect = 0
                                    symPracFeedback.text = "Incorrect"
                                    symPracFeedback.color = "red"
                                
                                symPracFeedback.draw()
                                win.flip()
                                core.wait(.5)
                                
                                box.color = "white" # reset box color to white
                                myMouse.clickReset()
                                timeAfterClick=0
                                mouseResponse =1 # change to 1 to end while loop
                                
                                # record data
                                bothPracticeData.append(
                                    [
                                        tmpRow.imageFilePath.iat[0], 
                                        tmpRow.imageNumber.iat[0],
                                        tmpRow.symmetrical.iat[0],
                                        tmpSymResp,
                                        tmpSymRespBinom,
                                        respCorrect,
                                        tmpSymRT,
                                        tmpSymRTyesNo,
                                        tmpRow.setSize.iat[0],
                                        t,
                                        tmpRow.trialPerSet.iat[0],
                                        tmpRow.redSquarePosition.iat[0],
                                        tmpSquareNumberShown
                                        #'l', # tmp place holder for letters recalled
                                        #0 # tmp place holder for correct count
                                        
                                    ]
                                )
                            
                blankScreen.draw()
                win.flip() #once Y/N button is pressed, flip screen
                core.wait(.2) # ISI before letter is shown
                
                # draw matrix with red square
                box_r1c1.draw()
                box_r1c2.draw()
                box_r1c3.draw()
                box_r1c4.draw()
                box_r2c1.draw()
                box_r2c2.draw()
                box_r2c3.draw()
                box_r2c4.draw()
                box_r3c1.draw()
                box_r3c2.draw()
                box_r3c3.draw()
                box_r3c4.draw()
                box_r4c1.draw()
                box_r4c2.draw()
                box_r4c3.draw()
                box_r4c4.draw()
                redBox.draw()
                win.flip() # show matrix with red square on screen
                core.wait(.650) #650ms red square display
                   
            
            fixationScreen.draw()
            win.flip()
            core.wait(.5) # .5s ITI before recall screen
        
            #SQUARE RECALL SCREEN
            recallBox_r1c1.autoDraw=True
            recallBox_r1c2.autoDraw=True
            recallBox_r1c3.autoDraw=True
            recallBox_r1c4.autoDraw=True
            recallBox_r2c1.autoDraw=True
            recallBox_r2c2.autoDraw=True
            recallBox_r2c3.autoDraw=True
            recallBox_r2c4.autoDraw=True
            recallBox_r3c1.autoDraw=True
            recallBox_r3c2.autoDraw=True
            recallBox_r3c3.autoDraw=True
            recallBox_r3c4.autoDraw=True
            recallBox_r4c1.autoDraw=True
            recallBox_r4c2.autoDraw=True
            recallBox_r4c3.autoDraw=True
            recallBox_r4c4.autoDraw=True
            
            
            recallBoxNumber1.autoDraw =True
            recallBoxNumber2.autoDraw =True
            recallBoxNumber3.autoDraw =True
            recallBoxNumber4.autoDraw =True
            recallBoxNumber5.autoDraw =True
            recallBoxNumber6.autoDraw =True
            recallBoxNumber7.autoDraw =True
            recallBoxNumber8.autoDraw =True
            recallBoxNumber9.autoDraw =True
            recallBoxNumber10.autoDraw =True
            recallBoxNumber11.autoDraw =True
            recallBoxNumber12.autoDraw =True
            recallBoxNumber13.autoDraw =True
            recallBoxNumber14.autoDraw =True
            recallBoxNumber15.autoDraw =True
            recallBoxNumber16.autoDraw =True
        
            blankButtonBox.autoDraw=True
            blankButton.autoDraw=True
            clearButtonBox.autoDraw=True
            clearButton.autoDraw=True
            enterButtonBox.autoDraw=True
            enterButton.autoDraw=True
            squareRecallText.autoDraw=True
        
        
            win.flip()
            
            # RECORD THE response locations AND change clicked boxes to red
            # set up the mouse, it will be called "myMouse"
            myMouse = event.Mouse(visible = True, win = win) 
            myMouse.setPos(newPos =[0,0]); # set mouse to be in the middle of the screen
        
        
            # initiate the response variable where we will store the participants' responses
            tmpSquareRecall = []; 
        
            # store the possible shapes that participants can click on during the recall period (this doesn't include the enter box)
            boxes =[recallBox_r1c1, recallBox_r1c2, recallBox_r1c3, recallBox_r1c4, 
                    recallBox_r2c1, recallBox_r2c2, recallBox_r2c3, recallBox_r2c4, 
                    recallBox_r3c1, recallBox_r3c2, recallBox_r3c3, recallBox_r3c4,
                    recallBox_r4c1, recallBox_r4c2, recallBox_r4c3, recallBox_r4c4,
                    blankButtonBox, clearButtonBox]
            
                         
            recallBoxes = [recallBoxNumber1, recallBoxNumber2, recallBoxNumber3, recallBoxNumber4, 
                    recallBoxNumber5, recallBoxNumber6, recallBoxNumber7, recallBoxNumber8, 
                    recallBoxNumber9, recallBoxNumber10, recallBoxNumber11, recallBoxNumber12,
                    recallBoxNumber13, recallBoxNumber14, recallBoxNumber15, recallBoxNumber16]

              
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
                        tmpSquareRecall.append(box.name)
                        myMouse.clickReset()
                        timeAfterClick=0
        
                        if box == clearButtonBox: # if clear button is pressed, reset everything
                            tmpSquareRecall=[]
                            recallBox_r1c1.color="white"
                            recallBox_r1c2.color="white" 
                            recallBox_r1c3.color="white"
                            recallBox_r1c4.color="white" 
                            recallBox_r2c1.color="white" 
                            recallBox_r2c2.color="white" 
                            recallBox_r2c3.color="white" 
                            recallBox_r2c4.color="white" 
                            recallBox_r3c1.color="white" 
                            recallBox_r3c2.color="white"
                            recallBox_r3c3.color="white" 
                            recallBox_r3c4.color="white"
                            recallBox_r4c1.color="white" 
                            recallBox_r4c2.color="white"
                            recallBox_r4c3.color="white" 
                            recallBox_r4c4.color="white"
                            recallBoxNumber1.color ="white"
                            recallBoxNumber2.color ="white"
                            recallBoxNumber3.color ="white"
                            recallBoxNumber4.color ="white"
                            recallBoxNumber5.color ="white"
                            recallBoxNumber6.color ="white"
                            recallBoxNumber7.color ="white"
                            recallBoxNumber8.color ="white"
                            recallBoxNumber9.color ="white"
                            recallBoxNumber10.color ="white"
                            recallBoxNumber11.color ="white"
                            recallBoxNumber12.color ="white"
                            recallBoxNumber13.color ="white"
                            recallBoxNumber14.color ="white"
                            recallBoxNumber15.color ="white"
                            recallBoxNumber16.color ="white"

        
                # change clicked boxes to be green (except theblank button, keep it white)
                for box in boxes:
                    if box.name in tmpSquareRecall and not box.name == blankButtonBox.name:
                        box.color = 'red'
            
                # when a participant clicks on a square, add the number of the square in the sequence
                    for r in range(len(tmpSquareRecall)):
                        if box.name in tmpSquareRecall[r] and not box.name == blankButtonBox.name:
                            #if recallBoxes[b].name ==box.name:
                            recallBoxes[int(tmpSquareRecall[r])].text = r+1
                            recallBoxes[int(tmpSquareRecall[r])].color = "black"
                            
                # prep the text that shows participant's responses (letters)
                #responseText='' 
                # for l in range(len(tmpSquareRecall)):
                #     responseText = "%s %s " % (responseText, tmpSquareRecall[l])
                    
                #responseText = "%s squares recalled" % (len(tmpSquareRecall))
        
                # draw the response text
                #showSquareResponse.text = responseText    
                #showSquareResponse.autoDraw=True
                win.flip()
                    
        
                # reset mouse
                myMouse.clickReset() 
                
            # turn off autodraw
            recallBox_r1c1.autoDraw=False
            recallBox_r1c2.autoDraw=False
            recallBox_r1c3.autoDraw=False
            recallBox_r1c4.autoDraw=False
            recallBox_r2c1.autoDraw=False
            recallBox_r2c2.autoDraw=False
            recallBox_r2c3.autoDraw=False
            recallBox_r2c4.autoDraw=False
            recallBox_r3c1.autoDraw=False
            recallBox_r3c2.autoDraw=False
            recallBox_r3c3.autoDraw=False
            recallBox_r3c4.autoDraw=False
            recallBox_r4c1.autoDraw=False
            recallBox_r4c2.autoDraw=False
            recallBox_r4c3.autoDraw=False
            recallBox_r4c4.autoDraw=False
            
            recallBoxNumber1.autoDraw =False
            recallBoxNumber2.autoDraw =False
            recallBoxNumber3.autoDraw =False
            recallBoxNumber4.autoDraw =False
            recallBoxNumber5.autoDraw =False
            recallBoxNumber6.autoDraw =False
            recallBoxNumber7.autoDraw =False
            recallBoxNumber8.autoDraw =False
            recallBoxNumber9.autoDraw =False
            recallBoxNumber10.autoDraw =False
            recallBoxNumber11.autoDraw =False
            recallBoxNumber12.autoDraw =False
            recallBoxNumber13.autoDraw =False
            recallBoxNumber14.autoDraw =False
            recallBoxNumber15.autoDraw =False
            recallBoxNumber16.autoDraw =False
        
        
            blankButtonBox.autoDraw=False
            blankButton.autoDraw=False
            clearButtonBox.autoDraw=False
            clearButton.autoDraw=False
            enterButtonBox.autoDraw=False
            enterButton.autoDraw=False
            squareRecallText.autoDraw=False
            showSquareResponse.autoDraw=False
        
            
            #reset box colors to white
            recallBox_r1c1.color="white"
            recallBox_r1c2.color="white" 
            recallBox_r1c3.color="white"
            recallBox_r1c4.color="white" 
            recallBox_r2c1.color="white" 
            recallBox_r2c2.color="white" 
            recallBox_r2c3.color="white" 
            recallBox_r2c4.color="white" 
            recallBox_r3c1.color="white" 
            recallBox_r3c2.color="white"
            recallBox_r3c3.color="white" 
            recallBox_r3c4.color="white"
            recallBox_r4c1.color="white" 
            recallBox_r4c2.color="white"
            recallBox_r4c3.color="white" 
            recallBox_r4c4.color="white"
            recallBoxNumber1.color ="white"
            recallBoxNumber2.color ="white"
            recallBoxNumber3.color ="white"
            recallBoxNumber4.color ="white"
            recallBoxNumber5.color ="white"
            recallBoxNumber6.color ="white"
            recallBoxNumber7.color ="white"
            recallBoxNumber8.color ="white"
            recallBoxNumber9.color ="white"
            recallBoxNumber10.color ="white"
            recallBoxNumber11.color ="white"
            recallBoxNumber12.color ="white"
            recallBoxNumber13.color ="white"
            recallBoxNumber14.color ="white"
            recallBoxNumber15.color ="white"
            recallBoxNumber16.color ="white"
            
            
            #provide feedback
            correctCountSquares = 0
        
            if len(tmpSquareRecall) == len(tmpSquareNumberShown): # if participant recalls correct number of letters
                for l in range(setSize[t]):
                    if tmpSquareRecall[l] == str(tmpSquareNumberShown[l]):
                        correctCountSquares +=1
                squareFeedbackText.text = text = "You recalled %.0f squares correctly out of %.0f." % (correctCountSquares, setSize[t])
            elif len(tmpSquareRecall) > len(tmpSquareNumberShown): # if sub recalls more squares than set size
                for l in range(setSize[t]):
                    if tmpSquareRecall[l] == str(tmpSquareNumberShown[l]):
                        correctCountSquares +=1
                squareFeedbackText.text = text = "You recalled too many squares." 
            elif len(tmpSquareRecall)==0: # if participant does not recall any letters
                squareFeedbackText.text = text = "You did not recall any squares."
            elif (len(tmpSquareRecall)<len(tmpSquareNumberShown)) and not (len(tmpSquareRecall) ==0):
                #for l in range(setSize):
                for l in range(len(tmpSquareRecall)):
                    if tmpSquareRecall[l] == str(tmpSquareNumberShown[l]):
                        correctCountSquares +=1
                squareFeedbackText.text = text = "You did not recall enough squares."
        
            # save squares recalled and whether it was correct after each trial (this happens at a different timescale as when we save the data above)
            
        
            
            bothPracticeData[sum(setSize[0:t+1])].append(tmpSquareRecall)
            bothPracticeData[sum(setSize[0:t+1])].append(correctCountSquares)
        
            #FEEDBACK AT THE END OF EACH SET (SO AFTER THE SQUARE RECALL)
            # feedback here shows both the letter recall, math correct, and overall percent correct in math across sets
        
            # calculate how well participant is doing with math over all
            tmpDF = pd.DataFrame(bothPracticeData) # temporarily make practice data a dataframe for easier handling (this gets overwritten after each set)
            tmpDF.columns = tmpDF.iloc[0]
            tmpDF = tmpDF.iloc[1: , :]
            
            symPercentCorrect = round(tmpDF.symResponseCorrect.mean()*100) # number to show in red on feedback screen after square recall
            
            
            # calculate number of math errors within a set
            
            symErrorsDuringSet = sum(tmpDF.symResponseCorrect[tmpDF.setNumber==t]==0)# number of errors made during the set
            
            
            bothPracticeData[sum(setSize[0:t+1])].append(symPercentCorrect) # save percent sym correct
            bothPracticeData[sum(setSize[0:t+1])].append(symErrorsDuringSet) # save sym errors
            #bothPracticeData["maxSymDisplay"] = maxSymDisplay
        
            
            if symErrorsDuringSet <3:
                  symErrorsAfterRecall.text = text = "You made %.0f symmetry error(s) for this set of trials." %(symErrorsDuringSet)
            elif symErrorsDuringSet >=3:
                  symErrorsAfterRecall.text = text = "You have made a total of 3 or more symmetry errors during this set. Please do your best on the symmetry."
            
        
            
            symTotalPercentCorrect.text = text = "%.0f%%" %(symPercentCorrect)
            
            symErrorsAfterRecall.draw()
            symTotalPercentCorrect.draw()
            squareFeedbackText.draw()
            
            win.flip()
            core.wait(2) # show feedback
            
            blankScreen.draw() 
            win.flip()
            core.wait(1) # blank after feedback
        
    
    #try:
        # Red square - symmetry real
            # 4 trials with set sizes  2- 5
        realInstructionsPg1.draw()
        instructCount.text=text="screen 1/2"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        realInstructionsPg1.draw()
        instructCount.text=text="screen 1/2"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        realInstructionsPg2.draw()
        instructCount.text=text="screen 2/2"
        instructCount.draw()
        win.flip()
        core.wait(forcedInstrWaitTime)

        realInstructionsPg2.draw()
        instructCount.text=text="screen 2/2"
        instructCount.draw()
        win.flip()
        event.waitKeys(keyList = ['return'], timeStamped = False) # waiting for key press or until max time allowed
        
        # set up symmetry stimuli
        #nTbothReal = 4 # 4 trials for the box-sym practice
        setSize = ([2,3,4,5]) # number of box-sym pairs in each set (2-5)
        random.shuffle(setSize) #shuffle
        
        bothRealDF = realSymmImageNames.sample(n=sum(setSize), axis="rows")
        
        # populate our data frame with some useful information
        setSizeLong = [] #to store the set size of each trial (options:2-5)
        triPerSet = [] # to store the trial number within a set (0 to 3)
        setNum=[] # to store which the current set (0-3 since there are 4 sets)
        for set in setSize:
            for t in range(set):
                setSizeLong.append(set)
                triPerSet.append(t)
                setNum.append(setSize.index(set))
        
        
        # add those variables to our dataframe
        bothRealDF["setSize"] = setSizeLong 
        bothRealDF["trialPerSet"] = triPerSet 
        bothRealDF["setNumber"] = setNum
        
        # set up red square stimuli
        bothRealRedsquares = []
        redSquaresShortFormat = [] # each row has all squares shown (position)
        # select the location of red square we will show on each trial
        
        for t in range(nTbothReal):
            tmpSquare = random.sample(matrixCenterPositions, k=setSize[t])# randomly select location of red squares, using sample instead of choices does without replacement
            # we don't want to have repeat squares in a single trial^
            redSquaresShortFormat.append(tmpSquare) # save them in short format
            
        
            for s in range(len(tmpSquare)):
                bothRealRedsquares.append(tmpSquare[s]) # also save red square locations in long format
                
                matrixCenterPositions.index(tmpSquare[s])
        
        bothRealDF["redSquarePosition"] = bothRealRedsquares # add red square locations that are shown following each symmetry operation
        
        
        # set up mouse
        myMouse = event.Mouse(visible = True, win = win) 
        minFramesAfterClick = 10 # to prevent re-entering the if loop too early, other wise multiple letters are recorded during a single mouse click
        timeAfterClick = 0
        symSpanBoxes = [symYesBox, symNoBox]
        
        
        # set up data
        bothRealData = [] # create data structure with column names
        bothRealData.append(
            [
                "imageName", 
                "imageNumber",
                "symmetrical",
                "symResponse",
                "symResponseBinom",
                "symResponseCorrect",
                "solveSymRT",
                "symYesNoRT",
                "setSize",
                "setNumber",
                "trialPerSet",
                "redSquarePos",
                "redSquareNumber",
                "squareRecall",
                "squareCorrectCount"
            ]
        )
        
        for t in range(nTbothReal): # for each trial
            tmpSquareNumberShown = [] # reseet the red square number variable before the start of each set
            
            for s in range(setSize[t]): # and set size within each trial
            
            # pull out the row of information on each trial
                tmpRow = bothRealDF.loc[(bothRealDF.setNumber ==t) & (bothRealDF.trialPerSet==s)]
                tmpSquareNumberShown.append(matrixCenterPositions.index(tmpRow.redSquarePosition.iat[0])) # save the square number that is shown on each trial
        
            # set up symmetry image and red square location
                # select the image to show on this trial
                selectedSymmImage = tmpRow.imageFilePath.iat[0]
                redBox.pos = tmpRow.redSquarePosition.iat[0] # set redBox position
        
        
                blankScreen.draw()
                win.flip()
                core.wait(.5) # blank screen for 500ms prior to each math operation
                
        
                # Show the symmetry image
                
                symImage.image = selectedSymmImage
                symImage.draw()
                #symClickMouse.draw()
                
                
                buttons = [0]*len(event.mouseButtons) #initializes it to a list of 0s with the length equal to the number of active buttons.
                myMouse.setPos(newPos =[0,symNoBox.pos[1]]); # set mouse to be in the middle of the true/false buttons
        
                
                win.flip() # show image
                
                myMouse.clickReset() # make sure mouseclick is reset to [0,0,0], restarts the clock
                mathSymClock = core.Clock() # start the clock
            
            # wait for math response to move onto math T/F
                while not any(buttons) and mathSymClock.getTime() <= maxSymDisplay:
                    (buttons,rtTimes) = myMouse.getPressed(getTime=True)
            
                #if participant does not respond in time on math, move to letter part
                if not any(buttons): 
                    tmpSymRT = float("nan") # record RT as nan
                    tmpSymResp = float("nan")
                    tmpSymRespBinom=float("nan")
                    respCorrect = 0
                    tmpSymRTyesNo = float("nan")
                    
                    # record data
                    bothRealData.append(
                        [
                            tmpRow.imageFilePath.iat[0], 
                            tmpRow.imageNumber.iat[0],
                            tmpRow.symmetrical.iat[0],
                            tmpSymResp,
                            tmpSymRespBinom,
                            respCorrect,
                            tmpSymRT,
                            tmpSymRTyesNo,
                            tmpRow.setSize.iat[0],
                            t,
                            tmpRow.trialPerSet.iat[0],
                            tmpRow.redSquarePosition.iat[0],
                            tmpSquareNumberShown
                            #'l', # tmp place holder for letters recalled
                            #0 # tmp place holder for correct count
                            
                        ]
                    )
                    
                else: # if participant does respond in time, move onto Yes/No
            
                    tmpSymRT = rtTimes[0]
                    
                    #Draw the isi
                    fixationScreen.draw() 
                    win.flip()
                    core.wait(.2) # 200ms isi
                    
            
                    # Show the prompt 'Is this symmetrical?" with "yes" and "no" buttons
                    symPracIsThisSymmetrical.draw()
                    symYesBox.draw()
                    symYesButton.draw()
                    symNoBox.draw()
                    symNoButton.draw() 
                    
                    myMouse.setPos(newPos =[0,symNoBox.pos[1]]); # set mouse to be in the middle of the yes/no buttons
                    win.flip()
        
                    # collect response, record RT and check whether participant was correct.
                    myMouse.clickReset() # make sure mouseclick is reset to [0,0,0]
            
                    mouseResponse = 0;
                    
                    while mouseResponse == 0:        
                        timeAfterClick += 1
            
                        for box in symSpanBoxes:
                            if myMouse.isPressedIn(box) and timeAfterClick >= minFramesAfterClick: # slows things down so that multiple responses are not recorded for a single click
                                buttons, times = myMouse.getPressed(getTime=True)
                                tmpSymResp = box.name
                                if tmpSymResp == 'No':
                                    tmpSymRespBinom = 0
                                elif tmpSymResp == 'Yes':
                                    tmpSymRespBinom = 1
                                tmpSymRTyesNo = times[0]
            
                                
                                # once pressed, change box color to grey, redraw everything
                                box.color = "grey"
                                symPracIsThisSymmetrical.draw()
                                symYesBox.draw()
                                symYesButton.draw()
                                symNoBox.draw()
                                symNoButton.draw() 
                                
                                
                                # Show “correct” or “incorrect” on the yes/no screen for 500ms
                                if tmpSymRespBinom == tmpRow.symmetrical.iat[0]:
                                    respCorrect = 1
                                    #symPracFeedback.text = "Correct"
                                else:
                                    respCorrect = 0
                                    #symPracFeedback.text = "Incorrect"
                                
                               # symPracFeedback.draw()
                                #win.flip()
                                #core.wait(.5)
                                
                                box.color = "white" # reset box color to white
                                myMouse.clickReset()
                                timeAfterClick=0
                                mouseResponse =1 # change to 1 to end while loop
                                
                                # record data
                                bothRealData.append(
                                    [
                                        tmpRow.imageFilePath.iat[0], 
                                        tmpRow.imageNumber.iat[0],
                                        tmpRow.symmetrical.iat[0],
                                        tmpSymResp,
                                        tmpSymRespBinom,
                                        respCorrect,
                                        tmpSymRT,
                                        tmpSymRTyesNo,
                                        tmpRow.setSize.iat[0],
                                        t,
                                        tmpRow.trialPerSet.iat[0],
                                        tmpRow.redSquarePosition.iat[0],
                                        tmpSquareNumberShown
                                        #'l', # tmp place holder for letters recalled
                                        #0 # tmp place holder for correct count
                                        
                                    ]
                                )
                            
                blankScreen.draw()
                win.flip() #once Y/N button is pressed, flip screen
                core.wait(.2) # ISI before letter is shown
                
                # draw matrix with red square
                box_r1c1.draw()
                box_r1c2.draw()
                box_r1c3.draw()
                box_r1c4.draw()
                box_r2c1.draw()
                box_r2c2.draw()
                box_r2c3.draw()
                box_r2c4.draw()
                box_r3c1.draw()
                box_r3c2.draw()
                box_r3c3.draw()
                box_r3c4.draw()
                box_r4c1.draw()
                box_r4c2.draw()
                box_r4c3.draw()
                box_r4c4.draw()
                redBox.draw()
                win.flip() # show matrix with red square on screen
                core.wait(.650) #650ms red square display
                   
            
            fixationScreen.draw()
            win.flip()
            core.wait(.5) # .5s ITI before recall screen
        
            #SQUARE RECALL SCREEN
            recallBox_r1c1.autoDraw=True
            recallBox_r1c2.autoDraw=True
            recallBox_r1c3.autoDraw=True
            recallBox_r1c4.autoDraw=True
            recallBox_r2c1.autoDraw=True
            recallBox_r2c2.autoDraw=True
            recallBox_r2c3.autoDraw=True
            recallBox_r2c4.autoDraw=True
            recallBox_r3c1.autoDraw=True
            recallBox_r3c2.autoDraw=True
            recallBox_r3c3.autoDraw=True
            recallBox_r3c4.autoDraw=True
            recallBox_r4c1.autoDraw=True
            recallBox_r4c2.autoDraw=True
            recallBox_r4c3.autoDraw=True
            recallBox_r4c4.autoDraw=True
            
            
            recallBoxNumber1.autoDraw =True
            recallBoxNumber2.autoDraw =True
            recallBoxNumber3.autoDraw =True
            recallBoxNumber4.autoDraw =True
            recallBoxNumber5.autoDraw =True
            recallBoxNumber6.autoDraw =True
            recallBoxNumber7.autoDraw =True
            recallBoxNumber8.autoDraw =True
            recallBoxNumber9.autoDraw =True
            recallBoxNumber10.autoDraw =True
            recallBoxNumber11.autoDraw =True
            recallBoxNumber12.autoDraw =True
            recallBoxNumber13.autoDraw =True
            recallBoxNumber14.autoDraw =True
            recallBoxNumber15.autoDraw =True
            recallBoxNumber16.autoDraw =True
        
            blankButtonBox.autoDraw=True
            blankButton.autoDraw=True
            clearButtonBox.autoDraw=True
            clearButton.autoDraw=True
            enterButtonBox.autoDraw=True
            enterButton.autoDraw=True
            squareRecallText.autoDraw=True
        
        
            win.flip()
            
            # RECORD THE response locations AND change clicked boxes to red
            # set up the mouse, it will be called "myMouse"
            myMouse = event.Mouse(visible = True, win = win) 
            myMouse.setPos(newPos =[0,0]); # set mouse to be in the middle of the screen
        
        
            # initiate the response variable where we will store the participants' responses
            tmpSquareRecall = []; 
        
            # store the possible shapes that participants can click on during the recall period (this doesn't include the enter box)
            boxes =[recallBox_r1c1, recallBox_r1c2, recallBox_r1c3, recallBox_r1c4, 
                    recallBox_r2c1, recallBox_r2c2, recallBox_r2c3, recallBox_r2c4, 
                    recallBox_r3c1, recallBox_r3c2, recallBox_r3c3, recallBox_r3c4,
                    recallBox_r4c1, recallBox_r4c2, recallBox_r4c3, recallBox_r4c4,
                    blankButtonBox, clearButtonBox]
            
                         
            recallBoxes = [recallBoxNumber1, recallBoxNumber2, recallBoxNumber3, recallBoxNumber4, 
                    recallBoxNumber5, recallBoxNumber6, recallBoxNumber7, recallBoxNumber8, 
                    recallBoxNumber9, recallBoxNumber10, recallBoxNumber11, recallBoxNumber12,
                    recallBoxNumber13, recallBoxNumber14, recallBoxNumber15, recallBoxNumber16]

              
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
                        tmpSquareRecall.append(box.name)
                        myMouse.clickReset()
                        timeAfterClick=0
        
                        if box == clearButtonBox: # if clear button is pressed, reset everything
                            tmpSquareRecall=[]
                            recallBox_r1c1.color="white"
                            recallBox_r1c2.color="white" 
                            recallBox_r1c3.color="white"
                            recallBox_r1c4.color="white" 
                            recallBox_r2c1.color="white" 
                            recallBox_r2c2.color="white" 
                            recallBox_r2c3.color="white" 
                            recallBox_r2c4.color="white" 
                            recallBox_r3c1.color="white" 
                            recallBox_r3c2.color="white"
                            recallBox_r3c3.color="white" 
                            recallBox_r3c4.color="white"
                            recallBox_r4c1.color="white" 
                            recallBox_r4c2.color="white"
                            recallBox_r4c3.color="white" 
                            recallBox_r4c4.color="white"
                            recallBoxNumber1.color ="white"
                            recallBoxNumber2.color ="white"
                            recallBoxNumber3.color ="white"
                            recallBoxNumber4.color ="white"
                            recallBoxNumber5.color ="white"
                            recallBoxNumber6.color ="white"
                            recallBoxNumber7.color ="white"
                            recallBoxNumber8.color ="white"
                            recallBoxNumber9.color ="white"
                            recallBoxNumber10.color ="white"
                            recallBoxNumber11.color ="white"
                            recallBoxNumber12.color ="white"
                            recallBoxNumber13.color ="white"
                            recallBoxNumber14.color ="white"
                            recallBoxNumber15.color ="white"
                            recallBoxNumber16.color ="white"

        
                # change clicked boxes to be green (except theblank button, keep it white)
                for box in boxes:
                    if box.name in tmpSquareRecall and not box.name == blankButtonBox.name:
                        box.color = 'red'
            
            
                # when a participant clicks on a square, add the number of the square in the sequence
                    for r in range(len(tmpSquareRecall)):
                        if box.name in tmpSquareRecall[r] and not box.name == blankButtonBox.name:
                            #if recallBoxes[b].name ==box.name:
                            recallBoxes[int(tmpSquareRecall[r])].text = r+1
                            recallBoxes[int(tmpSquareRecall[r])].color = "black"
                            
                # prep the text that shows participant's responses (letters)
                #responseText='' 
                # for l in range(len(tmpSquareRecall)):
                #     responseText = "%s %s " % (responseText, tmpSquareRecall[l])
                    
                #responseText = "%s squares recalled" % (len(tmpSquareRecall))
        
                # draw the response text
                #showSquareResponse.text = responseText    
                #showSquareResponse.autoDraw=True
                win.flip()
                    
        
                # reset mouse
                myMouse.clickReset() 
                
            # turn off autodraw
            recallBox_r1c1.autoDraw=False
            recallBox_r1c2.autoDraw=False
            recallBox_r1c3.autoDraw=False
            recallBox_r1c4.autoDraw=False
            recallBox_r2c1.autoDraw=False
            recallBox_r2c2.autoDraw=False
            recallBox_r2c3.autoDraw=False
            recallBox_r2c4.autoDraw=False
            recallBox_r3c1.autoDraw=False
            recallBox_r3c2.autoDraw=False
            recallBox_r3c3.autoDraw=False
            recallBox_r3c4.autoDraw=False
            recallBox_r4c1.autoDraw=False
            recallBox_r4c2.autoDraw=False
            recallBox_r4c3.autoDraw=False
            recallBox_r4c4.autoDraw=False
            
            recallBoxNumber1.autoDraw =False
            recallBoxNumber2.autoDraw =False
            recallBoxNumber3.autoDraw =False
            recallBoxNumber4.autoDraw =False
            recallBoxNumber5.autoDraw =False
            recallBoxNumber6.autoDraw =False
            recallBoxNumber7.autoDraw =False
            recallBoxNumber8.autoDraw =False
            recallBoxNumber9.autoDraw =False
            recallBoxNumber10.autoDraw =False
            recallBoxNumber11.autoDraw =False
            recallBoxNumber12.autoDraw =False
            recallBoxNumber13.autoDraw =False
            recallBoxNumber14.autoDraw =False
            recallBoxNumber15.autoDraw =False
            recallBoxNumber16.autoDraw =False
        
        
            blankButtonBox.autoDraw=False
            blankButton.autoDraw=False
            clearButtonBox.autoDraw=False
            clearButton.autoDraw=False
            enterButtonBox.autoDraw=False
            enterButton.autoDraw=False
            squareRecallText.autoDraw=False
            showSquareResponse.autoDraw=False
        
            
            #reset box colors to white
            recallBox_r1c1.color="white"
            recallBox_r1c2.color="white" 
            recallBox_r1c3.color="white"
            recallBox_r1c4.color="white" 
            recallBox_r2c1.color="white" 
            recallBox_r2c2.color="white" 
            recallBox_r2c3.color="white" 
            recallBox_r2c4.color="white" 
            recallBox_r3c1.color="white" 
            recallBox_r3c2.color="white"
            recallBox_r3c3.color="white" 
            recallBox_r3c4.color="white"
            recallBox_r4c1.color="white" 
            recallBox_r4c2.color="white"
            recallBox_r4c3.color="white" 
            recallBox_r4c4.color="white"
            recallBoxNumber1.color ="white"
            recallBoxNumber2.color ="white"
            recallBoxNumber3.color ="white"
            recallBoxNumber4.color ="white"
            recallBoxNumber5.color ="white"
            recallBoxNumber6.color ="white"
            recallBoxNumber7.color ="white"
            recallBoxNumber8.color ="white"
            recallBoxNumber9.color ="white"
            recallBoxNumber10.color ="white"
            recallBoxNumber11.color ="white"
            recallBoxNumber12.color ="white"
            recallBoxNumber13.color ="white"
            recallBoxNumber14.color ="white"
            recallBoxNumber15.color ="white"
            recallBoxNumber16.color ="white"

            
            #provide feedback
            correctCountSquares = 0
        
            if len(tmpSquareRecall) == len(tmpSquareNumberShown): # if participant recalls correct number of letters
                for l in range(setSize[t]):
                    if tmpSquareRecall[l] == str(tmpSquareNumberShown[l]):
                        correctCountSquares +=1
                squareFeedbackText.text = text = "You recalled %.0f squares correctly out of %.0f." % (correctCountSquares, setSize[t])
            elif len(tmpSquareRecall) > len(tmpSquareNumberShown): # if sub recalls more squares than set size
                for l in range(setSize[t]):
                    if tmpSquareRecall[l] == str(tmpSquareNumberShown[l]):
                        correctCountSquares +=1
                squareFeedbackText.text = text = "You recalled too many squares." 
            elif len(tmpSquareRecall)==0: # if participant does not recall any letters
                squareFeedbackText.text = text = "You did not recall any squares."
            elif (len(tmpSquareRecall)<len(tmpSquareNumberShown)) and not (len(tmpSquareRecall) ==0):
                #for l in range(setSize):
                for l in range(len(tmpSquareRecall)):
                    if tmpSquareRecall[l] == str(tmpSquareNumberShown[l]):
                        correctCountSquares +=1
                squareFeedbackText.text = text = "You did not recall enough squares."
        
            # save squares recalled and whether it was correct after each trial (this happens at a different timescale as when we save the data above)
            
        
            
            bothRealData[sum(setSize[0:t+1])].append(tmpSquareRecall)
            bothRealData[sum(setSize[0:t+1])].append(correctCountSquares)
        
            #FEEDBACK AT THE END OF EACH SET (SO AFTER THE SQUARE RECALL)
            # feedback here shows both the letter recall, math correct, and overall percent correct in math across sets
        
            # calculate how well participant is doing with math over all
            tmpDF = pd.DataFrame(bothRealData) # temporarily make practice data a dataframe for easier handling (this gets overwritten after each set)
            tmpDF.columns = tmpDF.iloc[0]
            tmpDF = tmpDF.iloc[1: , :]
            
            symPercentCorrect = round(tmpDF.symResponseCorrect.mean()*100) # number to show in red on feedback screen after square recall
            
            
            # calculate number of math errors within a set
            
            symErrorsDuringSet = sum(tmpDF.symResponseCorrect[tmpDF.setNumber==t]==0)# number of errors made during the set
            
            
            bothRealData[sum(setSize[0:t+1])].append(symPercentCorrect) # save percent sym correct
            bothRealData[sum(setSize[0:t+1])].append(symErrorsDuringSet) # save sym errors
            #bothRealData["maxSymDisplay"] = maxSymDisplay
            
            if symErrorsDuringSet <3:
                  symErrorsAfterRecall.text = text = "You made %.0f symmetry error(s) for this set of trials." %(symErrorsDuringSet)
            elif symErrorsDuringSet >=3:
                  symErrorsAfterRecall.text = text = "You have made a total of 3 or more symmetry errors during this set. Please do your best on the symmetry."
            
        
            
            symTotalPercentCorrect.text = text = "%.0f%%" %(symPercentCorrect)
            
            symErrorsAfterRecall.draw()
            symTotalPercentCorrect.draw()
            squareFeedbackText.draw()
            
            win.flip()
            core.wait(2) # show feedback
            
            blankScreen.draw() 
            win.flip()
            core.wait(1) # blank after feedback
           
            
        # end of task message to get experimenter
        endOfTask.draw()
        win.flip()
        event.waitKeys(keyList = ['space'], timeStamped = False) # waiting for key press or until max time allowed
          
    finally: # this should save the data even if something in "try" fails
        win.close()   
    
        #---- AT THE END OR IF THINGS BREAK - SAVE THE DATA WE HAVE ----#
        
        # If data exists, reformat as pd dataframe, save it
        
        datetime = time.strftime("%Y%m%d-%H%M%S"); # save date and time
        
        if 'squarePracticeData' in locals():
            if not isinstance(squarePracticeData, pd.DataFrame):
                squarePracticeData = pd.DataFrame(squarePracticeData)
                squarePracticeData.columns=["setSize","redSquarePos", "redSquareNumber","squareRecall","correctCount","trial"]    
                squarePracticeData = squarePracticeData.iloc[1: , :] # drop the first row which are the variable names
            filenameSquarePrac = dataDirectoryPath + "cgeSYMSPANsquarePractice_" + "sub" + subID + "_" + datetime + ".csv"; # make filename
            squarePracticeData.to_csv(filenameSquarePrac)
    
        if 'symPracticeData' in locals():
            if not isinstance(symPracticeData, pd.DataFrame):
                symPracticeData = pd.DataFrame(symPracticeData) #convert data into pandas dataframe
                symPracticeData.columns=["imageName","symmetrical","response", "responseCorrect","solveSymRT", "yesNoRT","trial"]# add column names
                symPracticeData = symPracticeData.iloc[1: , :] # drop the first row which are the variable namesPracticeData.iloc[1: , :] # drop the first row which are the variable names
            filenameSymPrac = dataDirectoryPath + "cgeSYMSPANsymmetryPractice_" + "sub" + subID + "_" + datetime + ".csv"; # make filename
            symPracticeData.to_csv(filenameSymPrac)
                
        if 'bothPracticeData' in locals():
            if not isinstance(bothPracticeData, pd.DataFrame):
                bothPracticeData = pd.DataFrame(bothPracticeData) #convert data into pandas dataframe
                bothPracticeData.columns=["imageName","imageNumber","symmetrical","symResponse","symResponseBinom","symResponseCorrect","solveSymRT","symYesNoRT","setSize","setNumber", "trialPerSet", "redSquarePos", "redSquareNumber","squareRecall","squareCorrectCount", "percentCorrectSym", "totalSymErrorsInSet"] # add column names
                bothPracticeData = bothPracticeData.iloc[1: , :] # drop the first row which are the variable bothPracticeData.iloc[1: , :] # drop the first row which are the variable names
            filenameBothPrac =dataDirectoryPath + "cgeSYMSPANbothPractice_" + "sub" + subID + "_" + datetime + ".csv"; # make filename
            bothPracticeData.to_csv(filenameBothPrac)
        
        if 'bothRealData' in locals():
            if not isinstance(bothRealData, pd.DataFrame):
                bothRealData = pd.DataFrame(bothRealData) #convert data into pandas dataframe
                bothRealData.columns=["imageName","imageNumber","symmetrical","symResponse","symResponseBinom","symResponseCorrect","solveSymRT","symYesNoRT","setSize","setNumber", "trialPerSet", "redSquarePos", "redSquareNumber","squareRecall","squareCorrectCount", "percentCorrectSym", "totalSymErrorsInSet"] # add column names
                bothRealData = bothRealData.iloc[1: , :] # drop the first row which are the variable bothPracticeData.iloc[1: , :] # drop the first row which are the variable names
            filenameBothReal =dataDirectoryPath + "cgeSYMSPANbothReal_" + "sub" + subID + "_" + datetime + ".csv"; # make filename
            bothRealData.to_csv(filenameBothReal)
            
            
            
