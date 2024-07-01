#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May 26 12:20:06 2022

@author: hayley # Modified by Von
"""

"""
Primary script for HRB's dissertation project RCS
This script does some set up for the experiment and calls all required scripts to run the risky decision-making and cognitive control tasks
"""

    

def cge(subID, isReal, compNum, taskSet, doET): # define the function and specify the argument(s)

    # subID
            # Must be three digits (e.g., 001, 093, 458, etc.)
    # isReal
            # 0 - for testing
            # 1 - for real
    # compNum:
            # 1 - VM Laptop
            # 2 - Chicharron
            # 3 - tofu 
    # taskSet:
            # 1 - do all
            # 2 - do ospan and symspan only
            # 1 - do symspan only
    # doET:
            # 0 - No eye-tracking
            # 1 - Do eye-tracking
    
    # let us know things are starting...
    print('starting study for participant', subID)    

    # IMPORT MODULES
    import os
    import pandas as pd
    import sys
    #from psychopy import core

    # SET WORKING DIRECTORY #
    if compNum ==1:
        dirName = ("C:\\Users\\jvonm\\Documents\\GitHub\\cge\\CGE")
        dataDirName = ("C:\\Users\\jvonm\\Documents\\GitHub\\cge\\CGE\\data")
    elif compNum ==2:
        dirName = ("/Users/shlab/Documents/Github/cge/CGE/")
        dataDirName = ("/Users/shlab/Documents/Github/cge/CGE/data")
    elif compNum ==3:
        dirName = ("/Users/Display/Desktop/Github/cge/CGE/")
        dataDirName = ("/Users/Display/Desktop/Github/cge/CGE/data")
    
    os.chdir(dirName)

    # IMPORT TASK SCRIPTS #
    # cgeRDM
    #from cgeRDMdraftET_test import cgeRDM 
    import cgeRDM_noWrap
    # OSpan
    from ospan.ospanTaskModule import ospanTask
    # SymSpan
    from symspan.symSpanTaskModule import symSpanTask
    
    # SETTING TASK VARIABLES & PRESENTATION ORDER
    if taskSet ==1:
        
        # risky decision-making task (input arguments determined above) 
        cgeRDM_noWrap
        #cgeRDM(subID, isReal, doET)
        
        # ospan instructions + instructions quiz + practice + task
        ospanTask(subID, isReal,dirName, dataDirName)
        
        # symspan instructions + instructions quiz + practice + task
        symSpanTask(subID, isReal,dirName, dataDirName)
        
    elif taskSet==2:
        
        ospanTask(subID, isReal,dirName, dataDirName)

        symSpanTask(subID, isReal,dirName, dataDirName)
        
    elif taskSet==3:
        
        symSpanTask(subID, isReal,dirName, dataDirName)

    win.close()
    #core.quit()
    sys.exit()
    
    # simple analysis script (checks for missing trials, runs simple glm, scores span tasks, notes whether we keep the data and then adjusts the condition file)


    
    
    
    
    
    
