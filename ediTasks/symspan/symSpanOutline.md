## Symmetry Span Task (shortened version, Foster et al (2014), Memory and Cognition)

### Task summary:
In this task, participants view images that are either symmetrical or nonsymmetrical across a red vertical line and rate whether the images are symmetrical. The symmetry ratings are considered distractors. Then participants briefly view a 4x4 matrix of squares, one of which is red. The location of the red square is the “to-be-remembered” item. On each trial, the number of sets, or the number of symmetry-red square pairs range from 2-5. At the end of each trial, participants report the location and order of each red square in a trial. The goal of the task is to accurately recall the location of the red squares. 

### Set sizes:
Each participant completes 4 trials with set sizes ranging from 2-5. The order of the set sizes across the trials is random across all participants (e.g. one participant will do set sizes 3,2,4,5 and another may do 4,2,3,5).

### Stimuli:
The images for both the symmetry and red-square judgements are available from the Englelab. There are a set of images used for the instructions, a set for the practice and a set for the real task (some of these overlap and will be explained in more detail later on).

### Practice:
Participants have three types of practices: red square matrix only, symmetry ratings only, then both.

### Variables to record:
1.	Reaction time on symmetry judgements (time between when the image is shown and when participant clicks to move on to rating). 
2.	Reaction time to make symmetry judgements: Should also record (just to be safe) the time between when the “Is this symmetrical” screen is shown and the participants’ response. I don’t think this is used but it is better to have that saved just in case.
3.	Symmetry judgment (true/false)
4.	Symmetry judgement accuracy (correct/incorrect)
5.	Over all percentage correct on symmetry judgements. This is displayed and updated throughout the task.
6.	Red square matrix responses (in order)
7.	Response time for red square matrices (also not sure whether these are used, but good to have).
8.	Whether participant was correct on their red square responses
9.	Set sizes
10.	Which symmetry images were shown on each trial
11.	Which red square matrices were shown on each trial


### Task outline
#### 1.	Instructions (3 screens, participant clicks through when ready, each sentence should be a new paragraph)
_Screen 1:_
  "In this experiment you will try to memorize the position of colored squares you see on the screen while you also make judgments about other pictures.
  In the next few minutes, you will have some practice to get you familiar with how the experiment works. We will begin by practicing the "square" part of    the experiment. Click the mouse to continue."
  
_Screen 2:_
"For this practice set, squares will appear on the screen one at a time. Try to remember where each square was, in the order it was presented in. After 2 - 5 squares have been shown, you will see a grid of the 16 possible places the squares could have been. Your job is to select each square in the order presented.  To do this, use the mouse to select the appropriate boxes. The squares you select will turn red. Click the mouse button to continue."

_Screen 3:_
"When you have selected all the squares, and they are in the correct order, hit the ENTER box at the bottom right of the screen. If you make a mistake, hit the CLEAR box to start over. If you forget one of the squares, click the BLANK box to mark the spot for the missing square.
Remember, it is very important to get the squares in the same order as you see them.  If you forget one, use the BLANK box to mark the position.
Please ask the experimenter any questions you may have at this time. When you are ready, click the mouse button to start the square practice."


#### 2.	Blank screen (1000ms)

#### 3.	Red matrices Practice
- Four trials with possible set sizes {2, 2, 3, 3} randomly ordered across participants
- Matrix images are predefined from Engle Lab 
- Timing for practice (on each trial, the following happens):
  - Show matrix (randomly selected from all matrix images) for 650ms
  - Interstimuli interval (ISI) 500ms
  - Repeat until set size is complete (i.e. 2-3 matrices have been displayed)
  - ISI 1000ms (this means after the last matrix in the set has been shown, there is a 1500ms ISI)
  - Recall: blank matrix where participant selected order of red boxes in the matrix. Also on the screen is a “blank” button that can be used to mark a forgotten location of a square in the sequence. For example if the participants saw two red square matrices but just remembers the location of the second one, they can use the blank key for their first response and then mark the location of the second box. Participants have an unlimited time to respond here. 
    - The text on the recall screen says “Select the squares in the order presented. Use the Blank button to fill in forgotten squares.”
  - Behind the scenes code will compute the scores after each recall (more detail below).
  - Feedback screen is shown for 1500ms and says “You recalled ___ squares correctly out of ___”
  - Intertrial interval for 1000ms before the next trial begins.
 
#### 4.	Symmetry judgements practice INSTRUCTIONS 
  - 7 screens, participants click through at their own pace

_Screen 1: _
"Now you will practice doing the symmetry part of the experiment.A picture will appear on the screen, and you will have to decide if it is symmetrical.  A picture is symmetrical if you can fold it in half vertically and the picture on the left lines up with the picture on the right.  On the next screen you will see a picture that IS SYMMETRICAL. Click the mouse to continue."

_Screen 2: (display symmexample1)_
" Notice that this picture is symmetrical about the red line. In the real pictures the red line will not be present. Click the mouse to continue."

_Screen 3: (display nonsymmexample2)_
"Here, the picture is NOT symmetrical. If you folded this across the red line, the boxes would NOT line up. Click the mouse to continue."

_Screen 4: (display symmexample1)_
"This is another example of a picture that IS symmetrical. If you folded it vertically, the two sides would line up. Click the mouse to continue."

_Screen 5: (display nonsymmexample2)_
"Here is another example of a picture that is NOT symmetrical. Notice that if folded, the two sides would not line up. If you have any questions about how symmetry works, please ask them now. Click the mouse to continue."

_Screen 6:_
"Once you have decided if the picture is symmetrical, click the mouse. On the next screen a YES and NO box will appear. If the picture you saw was symmetrical, click the YES box.  If it was not symmetrical, click the NO box. After you click on one of the boxes, the computer will tell you if  you made the right choice. Click the mouse to continue."

_Screen 7:_
"It is VERY important that you get the pictures correct. Please ask the experimenter any questions you may have at this time. When you are ready, click the mouse to try some practice problems."

#### 5.	Blank screen (1000ms)

#### 6.	Practice symmetry judgements 
- 15 trials where participants report whether an image is symmetrical after viewing each image.
- Set up: there are 15 practice images (e.g. pracsymm1.bmp) and images 1-7 are symmetrical and 8-15 nonsymmetrical. Foster et al had a variable “corrResponse” set to True for images 1-7 and False for images 8-15 and used this variable to check against participants responses.
- Timing for practice (on each trial, the following happens):
  - Symmetry stimuli shown (randomly selected), participant clicks when ready to move on to rating. The reaction time should be stored here. No text on the screen when this image is shown.
  - 200ms ISI
  - Rating screen: “Is this symmetric?” with “True” and “False” buttons.
  - Once participant responds, check if response is correct and provide feedback on the same screen for 500ms. If correct, display “correct” and if incorrect, display “incorrect” on the screen.
  - Collecting RTs: in this task, Foster et al are only collecting RTs if the participant was correct. To be cautious, we should collect all RTs.  
  - ITI 500ms
- Calculating average reaction time for the next practice set and the real task.
  - The average RT is determined by the average reaction time on CORRECT trials in the symmetry judgement practice trials. The average is then used to calculate the maximum amount of time the symmetry stimulus is displayed: average RT + 2.5 * standard deviation RT. If the symmetry stimulus ends up being less than 1000ms, then the symmetry duration is set to 1000 (i.e. the symmetry image must be displayed for at least 1000ms).

#### 7.	Practice both red squares and symmetry judgments INSTRUCTIONS

_Screen 1:_
"Now you will practice doing both parts of the experiment at the same time. In the next practice set, you will be given one of the symmetry problems.  Once you make your decision about the picture, a square will appear on the screen. Try and remember the position of the square. In the previous section where you only decided about the picture symmetry, the computer computed your average time to solve the problems. If you take longer than your average time, the computer will automatically move you onto the square part, thus skipping the YES or NO part and will count that problem as an error. Therefore it is VERY important to solve the problems as quickly and as accurately as possible. Click the mouse to continue."

_Screen 2:_
"After the square goes away, another symmetry picture will appear, and then another square. At the end of each set of pictures and squares, a recall screen will appear.  Use the mouse to select the squares you just saw.  Try your best to get the squares in the correct order. It is important to work QUICKLY and ACCURATELY. You will not be told if your answer to the symmetry picture is correct. But, after the square recall screen, you will be given feedback about your performance regarding both the number of squares recalled and the percent correct on the symmetry problems. Please ask the experimenter any questions you may have at this time. Click the mouse to continue."

_Screen 3:_
"During the feedback, you will see a number in red in the top right of the screen. This indicates your percent correct for the symmetry pictures for the entire experiment. It is VERY important for you to keep this at least at 85%. For our purposes, we can only use your data if you are at least 85% accurate on the symmetry pictures. Therefore, in order for you to be asked to come back for future experiments, you must perform at least at 85% on the symmetry problems WHILE doing your best to recall as many squares as possible. Please ask the experimenter any questions you may have at this time. Click the mouse to try some practice problems."

#### 8.	Practice both red squares and symmetry judgments 
  - Three practice trials, each with set size = 2 (i.e. two red square-symmetry judgement pairs).
  - The red square stimuli are the same as the practice red square stimuli 
  - Symmetry stimuli are different with 48 different images (e.g. symm1.bmp) where images 1-24 are symmetrical and 25-48 are nonsymmetrical. As mentioned above, Foster et al have a variable (“corrResponse”) that is True for symmetrical images and False for nonsymmetrical images and this is used to check against participants’ ratings.
  - Stimuli are randomly selected each trial.
  - Timing for practice (on each trial, the following happens):
    - Symmetry image is shown first and displayed until participant clicks to continue or for the symmetry duration based on their practice trials. 
    - If participant responds prior to the symmetry duration cut off, immediately show the response screen “Is this symmetrical?” with the “True” and “False” buttons. If participant did not respond before stimulus duration, skip this screen and go straight to the red box (symmetry judgement is counted as an error). This screen is shown until participant selects “True” or “False”. Record RT and response. 
    - Continuously calculating/updating the red number that is shown in the upper right corner that shows percentage correct for symmetry judgements across all trials.
    - Display feedback for 500ms (“correct” or “incorrect”) on the “is this symmetrical screen”.
    - 200ms blank screen?
    - Show matrix with red square for 650ms
    - 250 ms blank screen
    - Repeat for each set
    - ITI for 500ms (only after the set/trial is over)
    - Show recall grid with “clear”, “enter” and “blank” buttons. There is no time limit here. Text says “Select the squares in order. Use the Blank button to fill in forgotten squares.”
    - Compute the scores and provide feedback after each trial for 2000ms:
      - “You made __ symmetry error(s) for this set of trials
      - "You recalled __ square correctly out of  __”
    - If participant has more than 3 errors on the trial: "You have made a total of 3 or more symmetry errors during this trial.  Please do your best on the symmetry part."
    - Blank screen after feedback for 1000ms


#### 9.	Start real task INSTRUCTIONS
_Screen 1:_
"That is the end of the practice. The real trials will look just like the practice trials you just completed. Some sets will have more problems than others. It is important that you do your best on both the symmetry and the square parts of this experiment. Also, remember to keep your symmetry accuracy at 85% or above. Please click the mouse to begin the experiment."

#### 10.	Start real task (just one block for shortened version)

#### 11.	Compute scores after red square matrix recall on each trial:
  - Very similar to practice both, but do not show symmetry judgment feedback
  - 4 trials, set size 2-5 (i.e. two red square-symmetry judgement pairs).
  - The red square and symmetry stimuli are the same as the practice red square stimuli 
  - Stimuli are randomly selected each trial.
  - Timing for real task (on each trial, the following happens):
    - Symmetry image is shown first and displayed until participant clicks to continue or for the symmetry duration based on their practice trials. 
    - If participant responds prior to the symmetry duration cut off, immediately show the response screen “Is this symmetrical?” with the “True” and “False” buttons. If participant did not respond before stimulus duration, skip this screen and go straight to the red box (symmetry judgement is counted as an error). This screen is shown until participant selects “True” or “False”. Record RT and response. 
  - Continuously calculating/updating the red number that is shown in the upper right corner that shows percentage correct for symmetry judgements across all trials.
  - 250ms blank screen?
  - Show matrix with red square for 650ms
  - 250 ms blank screen
  - Repeat for each set
  - ITI for 500ms (only after the set/trial is over)
  - Show recall grid with “clear”, “enter” and “blank” buttons. There is no time limit here. Text says “Select the squares in order. Use the Blank button to fill in forgotten squares.”
  - Compute the scores and provide feedback after each trial for 2000ms:
      - “You made __ symmetry error(s) for this set of trials
      - You recalled __ square correctly out of  __”
      - If participant has more than 3 errors on the trial: "You have made a total of 3 or more symmetry errors during this trial.  Please do your best on the symmetry part."
  - Blank screen after feedback for 1000ms before starting next trial.


#### 12.	When task is over, display “Task is complete, please get experimenter”
  - For online study, this will say something like “Task complete, being redirected now”.


## Scoring details
  1. SSPAN Absolute Score:  The sum of the correctly recalled elements from only the items in which all the elements are recalled in correct serial order (formally SSPAN Score)
  2. SSPAN Partial Load Score:  The sum of correctly recalled elements from all items, regardless if all elements in a trial were recalled correctly (formally SSPAN Total).
  3. SSPAN Partial Unit Score: The proportions of elements within an item that were recalled correctly summed.

- For more information, refer to these papers:
  - Conway, A. R. A., Kane, M. J., Bunting, M. F., Hambrick, D. Z., Wilhelm, O., & Engle, R. W. (2005).  Working memory span tasks: A methodological review and user's guide.  Psychonomic Bulletin and Review, 12, 769 - 786.
  - Unsworth, N., Heitz, R.P., Schrock, J.C., & Engle, R.W. (2005).  An automated version of the operation span task.  Behavior Research Methods, 37, 498 - 505.

## “Behind the scenes code”:
This code is from eprime and is included just as a guide if necessary for coding in python/psychopy.

### Computing the scores after each recall:
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

          Mouse.ShowCursor False


### Computing symmetry duration:
          SymmetryDuration = practiceRT.Mean + 2.5 * practiceRT.StdDevS

          If SymmetryDuration < 1000 Then
            SymmetryDuration = 1000
          End If


          'MsgBox "Mean correct RT: " & (MathDuration) 
          'MsgBox practiceRT.mean
          'MsgBox practiceRT.StdDevS

          'MsgBox "total correct: " & (practiceRT.N)

          Mouse.ShowCursor False

### Checking response once participant selects true or false (practice both):
          'Designate "theState" as the Default Slide State, which is the 
          'current, ActiveState on the Slide object "math"
          Dim theState As SlideState
          Set theState = CheckResponse.States ("Default")

          Dim ptMouse As Point
          Dim strHit As String

          'Find coordinates of mouse click
          Mouse.GetCursorPos ptMouse.x, ptMouse.y

          'Determine string name of SlideImage or SlideText object at 
          'mouse click coordinates. Assign that value to strHit
          strHit = theState.HitTest(ptMouse.x, ptMouse.y)
          '
          'used to debug the thing
          'MsgBox "string " & strHit

          'check if they clicked the correct box

          'clicked on nothing, loops them back through
          If strHit = "" Then
            GoTo waitforclick
          'clicked on the symmetry problem, loops them back through
          ElseIf strHit = "SYMPROBLEM" Then
            GoTo waitforclick
          'their answer matches the correct answer, gives them a correct and goes on
          ElseIf strHit = c.GetAttrib ("CorrectAnswer") Then
          '	mathFeedback.text = "Correct"
            CheckResponse.ACC = 1	
          '	mathFeedback.run
          'clicked the wrong box, gives them an incorrect and continues
          'elseif strHit <> c.GetAttrib ("CorrectAnswer") Then
          '	mathFeedback.text = "Incorrect"
          '	OPERATION.ACC = 0	
          '	mathFeedback.run
          End If

          'if showproblem.rt = "" then operrors.addobservation 1
          'if showproblem.rt = "" then
          '	totalerrors = totalerrors + 1
          'end if
          'if math problem incorrect, record the rt to the summation object
          'if c.getattrib("PracticeMode") = "no" then
            If CheckResponse.acc = 1 Then
              SymmCrt.addobservation 1
              SymmetryTotalCorrect = 1 + SymmetryTotalCorrect
              SymmetryTotalTime = showsymm.RT + SymmetryTotalTime
              'msgbox "correct"
            End If


            If CheckResponse.acc = 0 Then
            'msgbox operation.acc
              SymmErrors.addObservation 1
            'msgbox operrors.total

              totalerrors = totalErrors + 1
              accerrors = accerrors + 1
            Else
              SymmErrors.addObservation 0
            End If
          'end if


          'If math.ACC = "1" Then
          'practiceRT.AddObservation c.GetAttrib("math.RT")
          'End If

          'msgbox totalerrors
          'msgbox operrors.total


### Compute scores after red square matrix recall on each trial:
          'Compare each value of inputArray and correctArray to determine how many squares were
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
          Else
            c.setattrib "SpanScore", 0
          End If

          'keep a total count within the summation object
          spansum.AddObservation numbcorrect

          'keep a total count within the summation object
          spansum1.AddObservation numbcorrect

### Compute scores after red square matrix recall on each trial (real task – should be same as practice both):
          'Compare each value of inputArray and correctArray to determine how many squares were
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
          Else
            c.setattrib "SpanScore", 0
          End If

          'keep a total count within the summation object
          spansum.AddObservation numbcorrect

          'keep a total count within the summation object
          spansum1.AddObservation numbcorrect
