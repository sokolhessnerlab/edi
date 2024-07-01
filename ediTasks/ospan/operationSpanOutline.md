## Operation Span Task (shortened version, Foster et al (2014), Memory and Cognition)

### Task summary:
In this task, participants solve a series of mathematical problems which are considered the distractors in this task. Following each operation, participants briefly view a letter which is the to-be-remembered item. On each trial, the number of sets, or the number of operation-letter pairs range from 3-7. At the end of each trial, participants report the order of the letters they saw. The goal of the task is to accurately recall the order of the letters in each trial. 

### Set sizes:
Each participant completes 5 trials with set sizes ranging from 3-7 (set and trial is interchangable here). The order of the set sizes is random across all participants (e.g. one participant will do set sizes 3,7,4,5,6 and another may do 4,3,5,7,6).

### Stimuli:
The math problems and letters are available from the Englelab and are copied in the spreadsheets ([here](./correctAnswer.xlsx), [here](operationSet1.xlsx), and [here](operationSet2.xlsx)) in this directory. The set of letters are the same across practice and real task. The math problems are different between the practice and real tasks.

### Practice:
Participants have three types of practices: letters only, math operations only, then both.

### Variables to record:
1.	Reaction time on math problems (time between when the operation is shown and when participant clicks to move on to answer). 
2.	Reaction time to solve math problems: Should also record (just to be safe) the time between when the answer screen is shown (“6”, True or False?) and the participants’ response. I don’t think this is used but it is better to have that saved just in case.
3.	Math problem response (true/false)
4.	Math accuracy (correct/incorrect)
5.	Over all percentage correct on math problems. This is displayed and updated throughout the task.
6.	Letter responses (in order)
7.	Response time for letter responses (also not sure whether these are used, but good to have).
8.	Whether participant was correct on their letter responses
9.	Set sizes
10.	Which math problems were shown on each trial
11.	Which letters were shown on each trial


### Task outline
#### 1.	Instructions (3 screens, participant clicks through when ready, each sentence should be a new paragraph)
_Screen 1:_
  ” In this experiment you will try to memorize letters you see on the screen while you also solve simple math problems. In the next few minutes, you will have some practice to get you familiar with how the experiment works. We will begin by practicing the letter part of the experiment. Click the mouse to continue.”

_Screen 2:_
” For this practice set, letters will appear on the screen one at a time. Try to remember each letter in the order presented. After 2-3 letters have been shown, you will see a screen listing 12 possible letters with a check box beside each one. Your job is to select each letter in the order presented.  To do this, use the mouse to select the box beside each letter.  The letters you select will appear at the bottom of the screen. Click the mouse button to continue.”

_Screen 3:_
“When you have selected all the letters, and they are in the correct order, hit the ENTER box at the bottom right of the screen. If you make a mistake, hit the CLEAR box to start over. If you forget one of the letters, click the BLANK box to mark the spot for the missing letter. Remember, it is very important to get the letters in the same order as you see them.  If you forget one, use the BLANK box to mark the position. Please ask the experimenter any questions you may have at this time. When you're ready, click the mouse button to start the letter practice.”


#### 2.	Blank screen (1000ms)

#### 3.	Letter practice
-  Four trials with possible set sizes {2, 2, 3, 3} randomly ordered across participants
- Letters are predefined from Engle Lab: F, P, Q, J, H, K, T, S, N, R, Y, L
- Timing for practice (on each trial, the following happens):
  - Show letter for 1000ms (randomly selected, not duplicating)
  - ISI 250ms
  - Repeat until set size/trial is complete
  - ITI 1000ms
  - Recall grid (4 rows x 3 columns) where participant inputs the letter order. This screen has an “enter”, “clear” and “blank” button. The blank button is to mark an empty space in the series of letters, for example if the letters were FPR but participant forgot the 3rd, it would be FP_.) 
    -	Row 1: F, H, J. Row 2: K, L, N. Row 3: P, Q, R. Row 4: S, T, Y
    - “Select the letters in the order presented. Use the blank button to fill in forgotten letters.”
  - Score is calculated. Behind the scenes code will compute the scores after each recall (more detail below).
  - Feedback is shown on the screen for 1500ms and says “You recalled ___ letters correctly out of ___"
  -	ITI for 1000ms following feedback, before the next trial starts.


 
#### 4.	Math practice INSTRUCTIONS 
  - 3 screens, participants click through at their own pace

_Screen 1:_
Now you will practice doing the math part of the experiment. A math problem will appear on the screen, like this: (2 x 1) + 1 = ? As soon as you see the math problem, you should compute the correct answer.  In the above problem, the answer 3 is correct. When you know the correct answer, you will click the mouse button. Click the mouse to continue.”

_Screen 2:_
“You will see a number displayed on the next screen, along with a box marked TRUE and a box marked FALSE. If the number on the screen is the correct answer to the math problem, click on the TRUE box with the mouse. If the number is not the correct answer, click on the FALSE box. For example, if you see the problem (2 x 2) + 1 = ? and the number on the following screen is 5 click the TRUE box, because the answer is correct. If you see the problem (2 x 2) + 1 =  ? and the number on the next screen is 6 click the FALSE box, because the correct answer is 5, not 6. After you click on one of the boxes, the computer will tell you if you made the right choice. Click the mouse to continue.”

_Screen 3:_
“It is VERY important that you get the math problems correct.  It is also important that you try and solve the problem as quickly as you can. Please ask the experimenter any questions you may have at this time. When you're ready, click the mouse to try some practice problems.”


#### 5.	Blank screen (1000ms)

#### 6.	Math practice
-	15 trials to calculate the average RT on CORRECT trials that is used to determine the math problem display duration in the practice both and real sets.
-	Math problems for the practice are to be done in the same order and pulled from:[practiceOperations](./practiceOperations.xlsx)
-	Timing for practice (on each trial, the following happens):
    -	Blank screen 500ms prior to showing each math problem
    -	Problem shown on the screen with text “When you have solved the math problem, click the mouse to continue”. it is HERE that we want the reaction time (i.e. once participant indicates they have solved the problem)
    -	ISI 200ms
    -	Suggested answer shown on screen with “True” and “False” button. No time limit on this screen. Start the clock to record the RT (not sure we will use this but record anyways)
    -	Once participant responds, record RT and check whether participant was correct.
    -	Show “correct” or “incorrect” on the true/false screen for 500ms
-	Calculating average RT is determined by the average reaction time on CORRECT trials in the math practice trials. The average is then used to calculate the maximum amount of time that the math problems are displayed in the following practice and real trials: average RT + 2.5* standard deviation RT. If this duration ends up being less than 1500ms, the math viewing duration is set to 1500ms (i.e. the math problem must be displayed for at least 1500ms). 
- DOUBLE CHECK THIS CODE IN EPRIME – DO THEY ADD 1000MS TO AVERAGE RT?


#### 7.	Practice both math and letter INSTRUCTIONS

_Screen 1:_
“Now you will practice doing both parts of the experiment at the same time. In the next practice set, you will be given one of the math problems.  Once you make your decision about the math problem, a letter will appear on the screen.  Try and remember the letter. In the previous section where you only solved math problems, the computer computed your average time to solve the problems.  If you take longer than your average time, the computer will automatically move you onto the letter part, thus skipping the True or False part and will count that problem as a math error. Therefore it is VERY important to solve the problems as quickly and as accurately as possible. Click the mouse to continue.”

_Screen 2:_
“After the letter goes away, another math problem will appear, and then another letter. At the end of each set of letters and math problems, a recall screen will appear.  Use the mouse to select the letters you just saw.  Try your best to get the letters in the correct order. It is important to work QUICKLY and ACCURATELY on the math.  Make sure you know the answer to the math problem before clicking to the next screen. You will not be told if your answer to the math problem is correct. After the recall screen, you will be given feedback about your performance regarding both the number of letters recalled and the percent correct on the math problems. Please ask the experimenter any questions you may have at this time. Click the mouse to continue.”

_Screen 3:_
“During the feedback, you will see a number in red in the top right of the screen. This indicates your percent correct for the math problems for the entire experiment. It is VERY important for you to keep this at least at 85%. For our purposes, we can only use data where the participant was at least 85% accurate on the math. Therefore, in order for you to be asked to come back for future experiments, you must perform at least at 85% on the math problems WHILE doing your best to recall as many letters as possible. Please ask the experimenter any questions you may have at this time. Click the mouse to try some practice problems.”

#### 8.	Practice both math and letters
  - Three practice trials, each with set size = 2 (i.e. two letter-math operation pairs).
  - The letter stimuli are same as above
  -	Math operations are in the spreadsheets ([correctAnswer](./correctAnswer.xlsx), [operationSet1.xlsx,](operationSet1.xlsx), and [operationSet2.xlsx](operationSet2.xlsx))
  -	EPRIME code for selecting the math operations is down below in “behind the scenes code”. Could determine all of this before each trial begins.
  -	Timing for practice (on each trial, the following happens):
    -	Math problem is shown with the text “Click the mouse to continue”. Math problem is displayed until participant clicks or until the max duration determined by their average RT. Record RT. 
    -	If participant responds prior to the math duration cut off, immediately show the answer screen with the “True” and “False” buttons. If participant did not respond before math duration, skip this screen and go straight to showing the letter and mark the math as an error. This screen is shown until participant selects “True” or “False”. Record RT and response. 
      -	Continuously calculating/updating the red number that is shown in the upper right corner that shows percentage correct for math judgements across all trials within the block.
    - Suggested amount and “True” and “False” are shown. Record RT and response.
    -	No feedback is given about math answer during this practice.
    -	ISI 200ms
    -	Letter shown for 1000ms
    -	ISI 250ms before next math problem
    -	Repeat until set/trial is done.
    -	When trial is complete, 500ms ITI
    -	Letter grid for recall with “enter”, “clear” and “blank” buttons
    -	Compute scores
    -	Show feedback (2000ms): “You recalled __ letters correctly out of _. You made __ math error(s) for this set of trials”. 
      -	If participant has 3 or more errors, show feedback: "You made a total of 3 or more math errors during this set. Please do your best on the math."
      -	Also on the screen in the top right corner is percentage of how well participant is doing on the math only(percentage of correctly math over the block, not just one set). This is just shown on this final feedback screen following each trial.
    -	May need to be keeping track of math total time?
    -	ITI 1000ms


#### 9.	Start real task INSTRUCTIONS
_Screen 1:_
” That is the end of the practice. The real trials will look like the practice trials you just completed. First you will get a math problem to solve, then a letter to remember.  When you see the recall screen, select the letters in the order presented.   If you forget a letter, click the BLANK box to mark where it should go.Some sets will have more math problems and letters than others. It is important that you do your best on both the math problems and the letter recall parts of this experiment. Remember on the math you must work as QUICKLY and ACCURATELY as possible. Also, remember to keep your math accuracy at 85% or above. Please click the mouse to begin the experiment.”

#### 10.	Start real task (just one block for shortened version)
  -	Set sizes: {3, 4, 5, 6, 7}, order randomly determined for each participant
  -	each math is something like (1 / 1) + 9=?
  -	The procedure and stimuli for the real task is identical to the practice both above.
  -	Math operations are in the spreadsheets ([correctAnswer](./correctAnswer.xlsx), [operationSet1.xlsx,](operationSet1.xlsx), and [operationSet2.xlsx](operationSet2.xlsx))
  -	EPRIME code for selecting the math operations is down below in “behind the scenes code”. Could determine all of this before each trial begins.
  -	Timing for real task (on each trial, the following happens):
    -	Math problem is shown with the text “Click the mouse to continue”. Math problem is displayed until participant clicks or until the max duration determined by their average RT. Record RT. 
    -	If participant responds prior to the math duration cut off, immediately show the answer screen with the “True” and “False” buttons. If participant did not respond before math duration, skip this screen and go straight to showing the letter and mark the math as an error. This screen is shown until participant selects “True” or “False”. Record RT and response. 
      -	Continuously calculating/updating the red number that is shown in the upper right corner that shows percentage correct for math judgements across all trials within the block.
    - Suggested amount and “True” and “False” are shown. Record RT and response.
    -	No feedback is given about math answer.
    -	ISI 200ms
    - Letter shown for 1000ms
    -	ISI 250ms before next math problem
    -	Repeat until set/trial is done.
    -	When trial is complete, 500ms ITI
    -	Letter grid for recall with “enter”, “clear” and “blank” buttons
    -	Compute scores
    -	Show feedback (2000ms): “You recalled __ letters correctly out of _. You made __ math error(s) for this set of trials”
      -	If participant has 3 or more errors, show feedback: "You made a total of 3 or more math errors during this set. Please do your best on the math."
      -	Also on the screen in the top right corner is percentage of how well participant is doing on the math only(percentage of correctly math over the block, not just one set). This is just shown on this final feedback screen following each trial.
    -	May need to be keeping track of math total time?
    -	Blank screen after feedback from 1000ms before starting next trial (ITI)
    
#### 11.	When task is over, display “Task is complete, please get experimenter”
  - For online study, this will say something like “Task complete, being redirected now”.


## Scoring details
  1. OSPAN Absolute Score:  The sum of the correctly recalled elements from only the items in which all the elements are recalled in correct serial order (formally OSPAN Score)
  2. OSPAN Partial Load Score:  The sum of correctly recalled elements from all items, regardless if all elements in a trial were recalled correctly (formally OSPAN Total).
  3. OSPAN Partial Unit Score: The proportions of elements within an item that were recalled correctly summed.

- For more information, refer to these papers:
  - Conway, A. R. A., Kane, M. J., Bunting, M. F., Hambrick, D. Z., Wilhelm, O., & Engle, R. W. (2005).  Working memory span tasks: A methodological review and user's guide.  Psychonomic Bulletin and Review, 12, 769 - 786.
  - Unsworth, N., Heitz, R.P., Schrock, J.C., & Engle, R.W. (2005).  An automated version of the operation span task.  Behavior Research Methods, 37, 498 - 505.

## “Behind the scenes code”:
This code is from eprime and is included just as a guide if necessary for coding in python/psychopy.


### Computing scores from letter responses (in letter practice):
          'Compare each value of inputArray and correctArray to determine how many letters were
          'recalled correctly

          numbcorrect = 0
          count = 0

          endcount = cnt

          'score arrays
          Do
            If inputArray(count) = correctArray(count) Then
              numbcorrect = numbcorrect + 1
              count = count + 1
            Else 
              count = count + 1
            End If
          Loop Until count = cnt

          'calculate total number correct
          '	c.setattrib "SpanTotal", numbcorrect

          'Calculate "spanscore."  Credit equal to the setsize is given only if the entire set
          'is recalled correctly in serial order, as presented.  The trial is scored as a "0"
          'otherwise.  "Trialspan" is recorded in the list object and written to the data file.

          'if numbcorrect = cnt then
          '	c.setattrib "SpanScore", numbcorrect
          '	spnscr = spnscr + numbcorrect
          'else
          '	c.setattrib "SpanScore", 0
          'end if

          'keep a total count within the summation object
          'spansum.AddObservation numbcorrect


### Calculating math duration following math only practice:
          MathDuration = practiceRT.Mean + 2.5 * practiceRT.StdDevS

          If MathDuration < 1500 Then
            MathDuration = 1500
          End If


          'MsgBox "Mean correct RT: " & (MathDuration) 
          'MsgBox practiceRT.mean
          'MsgBox practiceRT.StdDevS

          'MsgBox "total correct: " & (practiceRT.N)
          'set the math time for the subject based on their performance on the practice math problems.
          'adds 1000ms to their average RT.  This can be messed with in the setRT inline object.

### Stuff gets calculated to be shown on the screen:
            a = c.getattrib("sum")
            b = c.getattrib ("sum2")
            rand = c.getattrib ("randnum")
            correct = c.getattrib ("percrand")
            ab = a + b

            'checks that the sum is greater than 0, if it isn't
            'it keeps adding 3 until it is.
              'While correct = 1 and ab < 0  
                While ab < 0  
                b = b + 3
                ab = a + b
                c.SetAttrib "stim2", Abs(b)
              Wend



            If b > 0 Then
                c.SetAttrib "symbol", "+"
                End If


            'if the correct attribute is 2, we want to present an incorrect answer
            'this adds a random number, either positive or negative, and checks that it is greater

              While correct = 2 And ab + rand < 0 Or ab + rand = ab
                rand = rand + 2
              Wend
            'this determines which mouse click or keyboard response the person should make
            'in order to specify that the problem is correct or incorrect
            If  correct = 1 Then
              c.SetAttrib "total", ab
              'c.SetAttrib "CorrectAnswer",1
              'c.SetAttrib "CorrectAnswer","TRUE"
            '	TrialList.setattrib 1,  "CorrectAnswer","TRUE"
            ElseIf correct = 2 Then
              c.SetAttrib "total", ab + rand
              'c.SetAttrib "CorrectAnswer",2
              'c.SetAttrib "CorrectAnswer","FALSE"
            '	TrialList.setattrib 1,  "CorrectAnswer","FALSE"

            End If

            'MsgBox "correct answser " & TrialList.getattrib (1, "CorrectAnswer")

### Compute scores after practice both:
            'Compare each value of inputArray and correctArray to determine how many letters were
            'recalled correctly

            numbcorrect = 0
            count = 0

            endcount = cnt

            'score arrays
            Do
              If inputArray(count) = correctArray(count) Then
                numbcorrect = numbcorrect + 1
                count = count + 1
              Else 
                count = count + 1
              End If
            Loop Until count = cnt

            'calculate total number correct
              c.setattrib "SpanTotal", numbcorrect

            'calculate total number correct
              c.setattrib "SpanTotalBlock1", numbcorrect

            'Calculate "spanscore."  Credit equal to the setsize is given only if the entire set
            'is recalled correctly in serial order, as presented.  The trial is scored as a "0"
            'otherwise.  "Trialspan" is recorded in the list object and written to the data file.

            If numbcorrect = cnt Then
                c.setattrib "SpanScore", numbcorrect
                spnscr = spnscr + numbcorrect
                spnunit = spnunit + 1
                spnunitCount = spnunitCount + 1
            Else
                spnunitCount = spnunitCount + 1
                c.setattrib "SpanScore", 0
            End If

            'keep a total count within the summation object
            spansum.AddObservation numbcorrect

            'keep a total count within the summation object
            spansum1.AddObservation numbcorrect

            'Calculate partial credit unit scoring'
            spanprop.AddObservation numbcorrect/c.GetAttrib("setsz")

            'keep a total count within the summation object'
            spanprop1.AddObservation numbcorrect/c.GetAttrib("setsz")

### Code for feedback during practice both:
            'a few resets for the previous stuff.
            x = 0
            cnvs.TextBackColor = CColor ("white")


            'for feedback
            Dim mathtex As String
            Dim lettext As String
            Dim percent As Integer

            mathtex = " math error(s) for this set of trials"
            lettext = " letters correctly out of "


            'the feedback for the subject on recall.
              forFeedback.text = "You recalled " & numbcorrect & lettext & cnt

            'warn subject for every 3 0r more math errors.
            If totalerrors - e >= 3  Then
              Feedback2.text = "You have made a total of 3 or more math errors during this trial.  Please do your best on the math."
            Else
              Feedback2.text = "You made " & OpErrors.total & mathtex
            End If
              e = totalerrors



            'MsgBox "The array size is: " & (UBound(inputArray) - UBound(inputArray) + 1) 


            percent = MathItemCorrect/(MathItemCorrect + totalerrors) * 100
            percentage.text = percent & " %"

### Getting the data file set up?:
            Dim AverageMathTime As Double
            Dim AverageMathTimeCorrect As Double
            Dim totalacc As Double
            AverageMathTime = MathTotalTime / (MathItemCorrect + totalerrors)
            AverageMathTimeCorrect = MathTotalTime / MathItemCorrect
            totalacc = MathItemCorrect/(MathItemCorrect + totalerrors)

            c.setattrib "MathErrorTotal", totalerrors
            c.setattrib "SpeedErrorTotal", speederrors
            c.setattrib "AccErrorTotal", accerrors
            c.setattrib "OspanPartialScore", spansum.total
            c.setattrib "OspanPartialScoreBlock1", spansum1.total
            c.setattrib "OspanPartialScoreBlock2", spansum2.total
            c.setattrib "OspanPartialScoreBlock3", spansum3.total
            c.setattrib "OspanAbsoluteScore", spnscr
            c.SetAttrib "AvgMathTimeCorrect", AverageMathTimeCorrect
            c.SetAttrib "AvgMathTime", AverageMathTime
            c.SetAttrib "MathCorrect", MathItemCorrect
            c.SetAttrib "MathACC", totalacc
            c.SetAttrib "OspanPartialUnitScore", spanprop.mean
            c.SetAttrib "OspanPartialUnitScoreBlock1", spanprop1.mean
            c.SetAttrib "OspanPartialUnitScoreBlock2", spanprop2.mean
            c.SetAttrib "OspanPartialUnitScoreBlock3", spanprop3.mean
            c.SetAttrib "OspanAbsoluteUnitScore", spnunit/spnunitCount

            'Prints Operation Span scores to a tab-delimited text file.
            'If you don't want this additional text file created, just comment out all of the following commands.

            'If you don't want the final screen to show the scores to the subject, just move the textdisplay object 'scores' to the Unreferenced E-Objects

            'Dim filename as string

            'Dim TheDate as string
            'TheDate = Date$()
            'Dim TheTime as string
            'TheTime= Time$()

            'filename = "S" & c.GetAttrib("subject")  & "_OperationSpan" & ".txt"


            'Open filename For Append As #1

            'Write #1, "Open this file in excel with the text import wizard, select comma & tab delimited"
            'Write #1, "Subject: " & c.GetAttrib("subject")  
            'Write #1, "Date: " & TheDate & " Time: " & TheTime
            'Write #1, "OSPAN Absolute Score","OSPAN Total Correct","Operation Errors","Speed Errors","Accuracy Errors"
            'Write #1, c.GetAttrib("OspanScore"), c.GetAttrib("OspanTotal"), c.GetAttrib("MathErrorTotal"), c.GetAttrib("SpeedErrorTotal"), c.GetAttrib("AccErrorTotal")
            'Close #1



            'If FileExists(CurDir$ & "\\OSpan_Scores.txt") Then
            '    Open "OSpan_Scores.txt" For Append As #1
            '        Print #1, c.GetAttrib("Subject") & chr(9) & spansum.Total & chr(9) & spanprop.Mean & chr(9) & _
            '        spnscr & chr(9) & spnunit/spnunitCount & chr(9) & totalacc & chr(9) & AverageMathTime
            '    Close
            'Else
            '    Open "OSpan_Scores.txt" For Append As #1
            '        Print #1, "Subject" & chr(9) & "OSpanPartialScore" & chr(9) & "OSpanPartialUnitScore" & chr(9) & _
            '        "OSpanAbsoluteScore" & chr(9) & "OSpanAbsoluteUnitScore" & chr(9) & "MathACC" & chr(9) & "MathTimeMean"
            '    Close
            '    Open "OSpan_Scores.txt" For Append As #1
            '        Print #1, c.GetAttrib("Subject") & chr(9) & spansum.Total & chr(9) & spanprop.Mean & chr(9) & _
            '        spnscr & chr(9) & spnunit/spnunitCount & chr(9) & totalacc & chr(9) & AverageMathTime
            '    Close
            'End If

### Computing score:
          'Compare each value of inputArray and correctArray to determine how many letters were
          'recalled correctly

          numbcorrect = 0
          count = 0

          endcount = cnt

          'score arrays
          Do
            If inputArray(count) = correctArray(count) Then
              numbcorrect = numbcorrect + 1
              count = count + 1
            Else 
              count = count + 1
            End If
          Loop Until count = cnt

          'calculate total number correct
            c.setattrib "SpanTotal", numbcorrect

          'calculate total number correct
            c.setattrib "SpanTotalBlock1", numbcorrect

          'Calculate "spanscore."  Credit equal to the setsize is given only if the entire set
          'is recalled correctly in serial order, as presented.  The trial is scored as a "0"
          'otherwise.  "Trialspan" is recorded in the list object and written to the data file.

          If numbcorrect = cnt Then
              c.setattrib "SpanScore", numbcorrect
              spnscr = spnscr + numbcorrect
              spnunit = spnunit + 1
              spnunitCount = spnunitCount + 1
          Else
              spnunitCount = spnunitCount + 1
              c.setattrib "SpanScore", 0
          End If

          'keep a total count within the summation object
          spansum.AddObservation numbcorrect

          'keep a total count within the summation object
          spansum1.AddObservation numbcorrect

          'Calculate partial credit unit scoring'
          spanprop.AddObservation numbcorrect/c.GetAttrib("setsz")

          'keep a total count within the summation object'
          spanprop1.AddObservation numbcorrect/c.GetAttrib("setsz")



