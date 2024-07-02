
function [output] = ediTask01_count(subjID,doHB,isreal,doprac)

% function [output] = ediTask01_count(subjID,doHB,isreal,doprac)
%
% Runs the HD and HC tasks for EDI.
% subjID = The three-digit TEXT string (e.g. ['000'])
% doHB = 0 if you don't want to do the heartbeat task, [1] if you do.
% isreal = 0 if this isn't "real" (makes it short), [1] if it is real
% doprac = [1] if you want to do the practice, 0 if you do not
%
% Works on 64-bit Windows machines
%   see http://psychtoolbox.org/FaqTTLTrigger
%   see http://apps.usd.edu/coglab/psyc770/IO64.html
%
% PSH 7/2/24

if nargin < 4
    doprac = 1; % if [1], do practice; if 0, don't do practice.
    if nargin < 3
        isreal = 1; % assume not real (i.e. it's a test) (experience blocks will be shorter)
        if nargin < 2
            doHB = 1; % assume heartbeat (otherwise, sound just plays)
            if nargin < 1
                subjID = '000'; % the default subject ID
            end
        end
    end
end

if length(subjID) > 3
    error('Subject ID too long')
elseif length(subjID) < 3
    error('Subject ID too short')
end

try
    homepath = 'C:\Users\sokolhessnerlab\Desktop\ediTask\output';
    taskpath = 'C:\Users\sokolhessnerlab\Desktop\ediTask\task';
    %homepath = '~/Documents/PSH/Libraries/MATLAB_lib/IMA/';
    cd(homepath);
catch
    homepath = pwd;
    cd(homepath);
end

try
    % ---------- BEGIN PREP ---------- %
    %Initialize rand to a different state each time:
    if verLessThan('matlab','7.12')
        rand('twister',sum(100*clock))
        fprintf('Set rng using oldest method\n');
    elseif verLessThan('matlab','7.14')
        RandStream.setDefaultStream ...
            (RandStream('mt19937ar','seed',sum(100*clock))); % 'shuffle' makes the seed based on the current time
        fprintf('Set rng using intermediate method\n')
    else
        rng('shuffle'); % 'shuffle' makes the seed based on the current time
        fprintf('Set rng using newest method\n')
    end
    
    if isreal
        HideCursor;
    end
    
    % Basic screen & keyboard prep
    KbName('UnifyKeyNames');
    [wind, rect] = Screen('OpenWindow', max(Screen('Screens')));
    Screen('BlendFunction', wind, GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); % Turn on aliasing to allow line to be smooth
    blk = BlackIndex(wind);
    wht = WhiteIndex(wind);
    gry = GrayIndex(wind);
    Screen('FillRect', wind, gry); % set background
    Priority(MaxPriority(wind));
    txt_size = 30;
    Screen('TextFont',wind,'Courier New');
    Screen('TextSize',wind,txt_size);
    DrawFormattedText(wind,'Setting up...','center','center',blk,40);
    Screen('Flip',wind);
    w = rect(3); h = rect(4); % get the screen dimensions
    
    % Keys
    leftKey = {'1' '1!' 'q' 'v'};
    rightKey  = {'2' '2@' 'p' 'b'};
    esc_key_code = KbName('ESCAPE');
    trig_key_code = KbName('Return');
    lKey = KbName(leftKey);
    rKey = KbName(rightKey);
    
    
    % ---------- STUDY SETUP ---------- %
    
    if isreal
        nT = 160; % number of trials (MUST BE EVEN B/C OF BLOCKS)
        nB = 4; % number of blocks
    else
        nT = 20;
        nB = 2;
    end
    
    nHBs = 10; % number of heartbeats/trial
    
    scaleInc = 0.01; % increments on the scale for moving around, in proportion of scale length
    scaleW = 0.6; % width of the scale
    scaleNTicks = 6;
    
    % Timing
    tAtt = 1;
    del = 0.5;
    respT = 2;
    rateT = 3;
    itis = sortrows([rand(nT,1) [repmat(0.75,nT/2,1); repmat(1.25,nT/2,1)]]);
    itis = itis(:,2);
    
    while 1
        tmp = sortrows([rand(nT,1) [zeros(nT/2,1); ones(nT/2,1)]]);
        tmp = tmp(:,2);
        if max(diff(find(diff(tmp)~=0))) < 7 % no more than 7 identical trials in a row
            syncState = tmp; % 1 = in-sync; 0 = out-of-sync
            break
        end
    end
    
    % Option location
    txtoffset = 50; % guess at this
    optLx = round(w*1/3) - txtoffset;
    optRx = round(w*2/3) - txtoffset;
    opty = h*3/4;
    
    % Zero the parallel port
    if doHB
        adOut = hex2dec('D010');
        adIn = adOut + 1;
        config_io;
        outp(adOut,0);
    end
    
    % Prepare the beep sound (CHECK ON WINDOWS)
    %[s,Fs] = wavread('beep.wav');
    [s,Fs] = audioread('beep.wav');
    s = s';
    nrchannels = size(s,1); % number of rows = number of channels
    reqlatencyclass = 1; % pushing latency (CONSIDER 3)
    
    InitializePsychSound(1); % Ask it to push for low latency
    
    bp = PsychPortAudio('Open', [], [], reqlatencyclass, Fs, nrchannels);
    PsychPortAudio('FillBuffer', bp, s);
    PsychPortAudio('Start', bp); % Do the sound once to prevent memory delays; Hereafter, expect a latency of 3-5ms
    
    
    % ---------- PREP OUTPUT OBJECT ---------- %
    output.params.nT = nT;
    output.params.nB = nB;
    output.params.timing.tAtt = tAtt;
    output.params.timing.del = del;
    output.params.timing.respT = respT;
    output.params.timing.rateT = rateT;
    output.params.timing.itis = itis;
    output.params.syncState = syncState;
    output.params.doHB = doHB;
    output.params.isreal = isreal;
    
    output.subjID = subjID;
    output.rt = nan(nT,1);
    output.choice = nan(nT,1); % 0 = not in sync; 1 = in sync
    output.rating = nan(nT,1);
    output.tstart = nan(nT,1); % when a trial starts
    output.toptions = nan(nT,1); % when choice is presented to subjects
    output.tratings = nan(nT,1);
    
    %save(imread('imaTask_params_IMA%s_%.4f.mat',subjID,now),'output')
    save(sprintf('ediTask_params_EDI%s_%.4f.mat',subjID,now),'output')
    
    WaitSecs(1);
    
    % -------------- RELAX -------------- %
    DrawFormattedText(wind,'We''ll begin the practice in a few minutes.\n\nFor now, just relax.','center','center',blk,40);
    Screen('Flip', wind);
    while 1
        [keyIsDown,secs,keyCode] = KbCheck;
        if (keyIsDown && size(find(keyCode), 2) == 1);
            if keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif any(keyCode(trig_key_code))
                Screen('Flip', wind);
                break;
            end
        end
    end
    WaitSecs(1);
    
    
    % ------------ PRACTICE ------------ %
    
    if doprac
        
        DrawFormattedText(wind,'PRACTICE\n\nAs we discussed in the instructions, this will consist of 6 trials. Trials 1, 3, and 5 will be IN SYNC, and trials 2, 4, and 6 will be DELAYED. This will be indicated on the screen throughout each practice trial.\n\n\nPress any key to continue.','center','center',blk,40);
        Screen('Flip',wind);
        WaitSecs(2);
        while 1
            [keyIsDown,secs,keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                else
                    break;
                end
            end
        end
        
        DrawFormattedText(wind,'Even though we''re telling you which trial is which, everything else will be like it is in the real task - you''ll be asked whether tones were in sync or delayed, and then asked to rate your confidence in that judgment. Take this opportunity to practice responding in time!\n\nPress any key to continue.','center','center',blk,40);
        Screen('Flip',wind);
        WaitSecs(2);
        while 1
            [keyIsDown,secs,keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                else
                    break;
                end
            end
        end
        
        DrawFormattedText(wind,sprintf('Don''t forget: you have %g seconds to make your choice, and %g seconds to rate your confidence. Use the "v" and "b" keys to choose, and then to slide the red confidence cursor.\n\nPress any key to continue.',respT,rateT),'center','center',blk,40);
        Screen('Flip',wind);
        WaitSecs(2);
        while 1
            [keyIsDown,secs,keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                else
                    break;
                end
            end
        end
        
        DrawFormattedText(wind,'If you have any questions, please ask your experimenter now!\n\nOtherwise, you''re ready to begin the practice. Remember: take a moment to try to find your heartbeat now, BEFORE beginning!\n\n When you''ve got it, press "v" or "b" to begin.','center','center',blk,40);
        Screen('Flip',wind);
        WaitSecs(1);
        while 1
            [keyIsDown,secs,keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                elseif any(keyCode([lKey rKey]));
                    break;
                end
            end
        end
        
        DrawFormattedText(wind,'Beginning the practice in 3 seconds...','center','center',blk,40);
        Screen('Flip',wind); startT = GetSecs;
        while (GetSecs - startT) < 3
            [keyIsDown,secs,keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                end
            end
        end
        
        nTp = 6;
        syncStateP = [1 0 1 0 1 0]; % 1 = in-sync; 0 = out-of-sync
        syncStateTxt = {'DELAYED','IN SYNC'};
        
        for t = 1:nTp
            tmpT = GetSecs;
            DrawFormattedText(wind,sprintf('[This trial is %s]',syncStateTxt{syncStateP(t)+1}),'center',h*.2,blk,40);
            Screen('Flip',wind);
            while (GetSecs - tmpT) < 1
                [keyIsDown, secs, keyCode] = KbCheck;
                if (keyIsDown && size(find(keyCode), 2) == 1);
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
            end
            %%%% Trials %%%%
            startT = GetSecs;
            DrawFormattedText(wind,'Attend...','center','center',blk,40);
            DrawFormattedText(wind,sprintf('[This trial is %s]',syncStateTxt{syncStateP(t)+1}),'center',h*.2,blk,40);
            Screen('Flip',wind);
            while (GetSecs - startT) < tAtt
                [keyIsDown, secs, keyCode] = KbCheck;
                if (keyIsDown && size(find(keyCode), 2) == 1);
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
            end
            
            % HEARTBEAT & TONES
            soundT = .2 + (1-syncStateP(t))*.3; % if Sync == 1, 200ms; if Sync == 0, 500ms;
            count = 0;
            if doHB
                outp(adOut,0); % zero the parallel port
                while count < nHBs % do this nHB times
                    if inp(adIn) < 60 % if HB trigger
                        PsychPortAudio('Start',bp,[],GetSecs + soundT); % schedule the new sound
                        outp(adOut,1); % send marking pulse
                        count = count + 1; % increment the count
                        while inp(adIn) < 60 % wait until the trigger resets (only finishes when inp pulls a value > 60)
                            % loop
                        end
                        outp(adOut,0); % turn off marking pulse
                    end
                end
            else
                while count < nHBs % if we're not doing heartbeats, play some fast beeps
                    PsychPortAudio('Start',bp);
                    WaitSecs(soundT);
                    count = count + 1;
                end
            end
            
            % DELAY
            startT = GetSecs;
            while (GetSecs - startT) < del
                [keyIsDown, secs, keyCode] = KbCheck;
                if (keyIsDown && size(find(keyCode), 2) == 1);
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
            end
            
            % RESPONSE
            respsucc = NaN;
            lr = round(rand);
            if lr
                DrawFormattedText(wind,'In Sync',optLx,opty);
                DrawFormattedText(wind,'Delayed',optRx,opty);
            else
                DrawFormattedText(wind,'Delayed',optLx,opty);
                DrawFormattedText(wind,'In Sync',optRx,opty);
            end
            DrawFormattedText(wind,sprintf('[This trial is %s]',syncStateTxt{syncStateP(t)+1}),'center',h*.2,blk,40);
            Screen('Flip',wind);
            while (GetSecs - startT) < (del + respT)
                [keyIsDown, secs, keyCode] = KbCheck;
                if (keyIsDown && size(find(keyCode), 2) == 1);
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    elseif any(keyCode([lKey rKey]));
                        respsucc = 1;
                        DrawFormattedText(wind,sprintf('[This trial is %s]',syncStateTxt{syncStateP(t)+1}),'center',h*.2,blk,40);
                        Screen('Flip',wind);
                        break;
                    end
                end
            end
            
            while (GetSecs - startT) < (del + respT + del/4);
                [keyIsDown, secs, keyCode] = KbCheck;
                if (keyIsDown && size(find(keyCode), 2) == 1);
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
            end
            
            % RATING
            if isnan(respsucc)
                DrawFormattedText(wind,'You did not respond in time.\n\nPlease respond faster!','center','center',[255 0 0],40);
                DrawFormattedText(wind,sprintf('[This trial is %s]',syncStateTxt{syncStateP(t)+1}),'center',h*.2,blk,40);
                Screen('Flip',wind);
                while (GetSecs - startT) < (del + respT + del/4 + rateT)
                    [keyIsDown, secs, keyCode] = KbCheck;
                    if (keyIsDown && size(find(keyCode), 2) == 1);
                        if keyCode(esc_key_code)
                            error('Experiment aborted by user!');
                        end
                    end
                end
            else
                currXpos = .5 + randn/25; % jittered starting position in center
                
                while (GetSecs - startT) < (del + respT + del/4 + rateT)
                    DrawScale(wind,h,w,scaleW,scaleNTicks,txtoffset,currXpos);
                    DrawFormattedText(wind,sprintf('[This trial is %s]',syncStateTxt{syncStateP(t)+1}),'center',h*.2,blk,40);
                    Screen('Flip',wind);
                    WaitSecs(0.001);
                    
                    [keyIsDown, secs, keyCode] = KbCheck;
                    if (keyIsDown && size(find(keyCode), 2) == 1);
                        if keyCode(esc_key_code)
                            error('Experiment aborted by user!');
                        elseif any(keyCode(lKey));
                            currXpos = currXpos - scaleInc;
                        elseif any(keyCode(rKey));
                            currXpos = currXpos + scaleInc;
                        end
                        
                        if currXpos < 0 % boundary the x position
                            currXpos = 0;
                        elseif currXpos > 1
                            currXpos = 1;
                        end
                    end
                end
                DrawScale(wind,h,w,scaleW,scaleNTicks,txtoffset,currXpos,wht);
                DrawFormattedText(wind,sprintf('[This trial is %s]',syncStateTxt{syncStateP(t)+1}),'center',h*.2,blk,40);
                Screen('Flip',wind);
                while (GetSecs - startT) < (del + respT + del/4 + rateT + del)
                    [keyIsDown, secs, keyCode] = KbCheck;
                    if (keyIsDown && size(find(keyCode), 2) == 1);
                        if keyCode(esc_key_code)
                            error('Experiment aborted by user!');
                        end
                    end
                end
            end
            
            % ITI
            Screen('Flip',wind);
            while (GetSecs - startT) < (del + respT + del/4 + rateT + del + itis(t))
                [keyIsDown, secs, keyCode] = KbCheck;
                if (keyIsDown && size(find(keyCode), 2) == 1);
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
            end
        end
        
        DrawFormattedText(wind,'You''ve finished the practice!','center','center',blk,40);
        Screen('Flip', wind);
        tmpT = GetSecs;
        while (GetSecs-tmpT) < 2
            [keyIsDown, secs, keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                end
            end
        end
        
    end
    
    DrawFormattedText(wind,'Do you have any questions before we begin?\n\n\nWaiting for Experimenter to start the task.','center','center',blk,40);
    Screen('Flip', wind);
    while 1
        [keyIsDown, secs, keyCode] = KbCheck;
        if (keyIsDown && size(find(keyCode), 2) == 1);
            if keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif any(keyCode(trig_key_code))
                break;
            end
        end
    end
    
    DrawFormattedText(wind,'Beginning the task in 5 seconds...','center','center',blk,40);
    Screen('Flip', wind);
    startT = GetSecs;
    while (GetSecs - startT) < 4
        [keyIsDown, secs, keyCode] = KbCheck;
        if (keyIsDown && size(find(keyCode), 2) == 1);
            if keyCode(esc_key_code)
                error('Experiment aborted by user!');
            end
        end
    end
    Screen('Flip', wind);
    WaitSecs(1);
    
    
    % ---------- RUN THE EXPERIMENT ---------- %
    b = 1;
    for t = 1:nT
        
        %%%% Interblock Break %%%%
        if t > ((nT/nB)*b)
            DrawFormattedText(wind,'Block finished.\n\nThe next block will begin in 30 seconds. If you''d like to continue before 30 seconds are up, you can do so at any time by pressing any key.','center','center',blk,40);
            Screen('Flip',wind); startT = GetSecs;
            WaitSecs(1);
            b = b + 1;
            while (GetSecs - startT) < 27
                [keyIsDown, secs, keyCode] = KbCheck;
                if (keyIsDown && size(find(keyCode), 2) == 1);
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    else
                        break;
                    end
                end
            end
            
            DrawFormattedText(wind,'Beginning the next block in 3 seconds...','center','center',blk,40);
            Screen('Flip',wind); startT = GetSecs;
            while (GetSecs - startT) < 3
                [keyIsDown, secs, keyCode] = KbCheck;
                if (keyIsDown && size(find(keyCode), 2) == 1);
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
            end
        end
        
        %%%% Trials %%%%
        startT = GetSecs;
        output.tstart(t) = startT;
        DrawFormattedText(wind,'Attend...','center','center',blk,40);
        Screen('Flip',wind);
        while (GetSecs - startT) < tAtt
            [keyIsDown, secs, keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                end
            end
        end
        
        % HEARTBEAT & TONES
        soundT = .2 + (1-syncState(t))*.3; % if Sync == 1, 200ms; if Sync == 0, 500ms;
        count = 0;
        if doHB
            outp(adOut,0); % zero the parallel port
            while count < nHBs % do this nHB times
                if inp(adIn) < 60 % if HB trigger
                    PsychPortAudio('Start',bp,[],GetSecs + soundT); % schedule the new sound
                    outp(adOut,1); % send marking pulse
                    count = count + 1; % increment the count
                    while inp(adIn) < 60 % wait until the trigger resets (only finishes when inp pulls a value > 60)
                        % loop
                    end
                    outp(adOut,0); % turn off marking pulse
                end
            end
        else
            while count < nHBs % if we're not doing heartbeats, play some fast beeps
                PsychPortAudio('Start',bp);
                WaitSecs(soundT);
                count = count + 1;
            end
        end
        
        % DELAY
        startT = GetSecs;
        while (GetSecs - startT) < del
            [keyIsDown, secs, keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                end
            end
        end
        
        % RESPONSE
        output.toptions(t) = GetSecs;
        lr = round(rand);
        if lr
            DrawFormattedText(wind,'In Sync',optLx,opty);
            DrawFormattedText(wind,'Delayed',optRx,opty);
        else
            DrawFormattedText(wind,'Delayed',optLx,opty);
            DrawFormattedText(wind,'In Sync',optRx,opty);
        end
        Screen('Flip',wind);
        while (GetSecs - startT) < (del + respT)
            [keyIsDown, secs, keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                elseif any(keyCode([lKey rKey]));
                    output.rt(t) = GetSecs-output.toptions(t);
                    if ismember({KbName(keyCode)}, leftKey)
                        if lr
                            output.choice(t) = 1; % In Sync
                        else
                            output.choice(t) = 0; % Out of Sync
                        end
                    elseif ismember({KbName(keyCode)}, rightKey)
                        if lr
                            output.choice(t) = 0; % Out of Sync
                        else
                            output.choice(t) = 1; % In Sync
                        end
                    end
                    Screen('Flip',wind);
                    break;
                end
            end
        end
        
        while (GetSecs - startT) < (del + respT + del/4)
            [keyIsDown, secs, keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                end
            end
        end
        
        % RATING
        if isnan(output.choice(t))
            DrawFormattedText(wind,'You did not respond in time.\n\nPlease respond faster!','center','center',[255 0 0],40);
            Screen('Flip',wind);
            while (GetSecs - startT) < (del + respT + del/4 + rateT)
                [keyIsDown, secs, keyCode] = KbCheck;
                if (keyIsDown && size(find(keyCode), 2) == 1);
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
            end
        else
            output.tratings(t) = GetSecs;
            currXpos = .5 + randn/25; % jittered starting position in center
            
            while (GetSecs - startT) < (del + respT + del/4 + rateT)
                DrawScale(wind,h,w,scaleW,scaleNTicks,txtoffset,currXpos);
                Screen('Flip',wind);
                WaitSecs(0.001);
                
                [keyIsDown, secs, keyCode] = KbCheck;
                if (keyIsDown && size(find(keyCode), 2) == 1);
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    elseif any(keyCode(lKey));
                        currXpos = currXpos - scaleInc;
                    elseif any(keyCode(rKey));
                        currXpos = currXpos + scaleInc;
                    end
                    
                    if currXpos < 0 % boundary the x position
                        currXpos = 0;
                    elseif currXpos > 1
                        currXpos = 1;
                    end
                end
            end
            output.rating(t) = currXpos;
            DrawScale(wind,h,w,scaleW,scaleNTicks,txtoffset,currXpos,wht);
            Screen('Flip',wind);
            while (GetSecs - startT) < (del + respT + del/4 + rateT + del)
                [keyIsDown, secs, keyCode] = KbCheck;
                if (keyIsDown && size(find(keyCode), 2) == 1);
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
            end
        end
        
        % ITI
        Screen('Flip',wind);
        while (GetSecs - startT) < (del + respT + del/4 + rateT + del + itis(t))
            [keyIsDown, secs, keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                end
            end
        end
    end
    
    
    
    
    
    
    %%%% Picking the random outcome %%%%
    
    DrawFormattedText(wind, 'Tone Task complete!', 'center', 'center', blk, 40);
    Screen('Flip',wind);
    WaitSecs(2);
    
%     pickt = randi(nT);
%     
%     if output.choice(pickt) == syncState(pickt) % if they were right
%         otc = 1;
%         txt1 = 'were correct';
%         txt2 = '+$5';
%         txt3 = '+$10';
%     elseif output.choice(pickt) == 1-syncState(pickt) % if they were wrong
%         otc = 0;
%         txt1 = 'were incorrect';
%         txt2 = '$0';
%         txt3 = '$5';
%     elseif isnan(output.choice(pickt)) % if they did not respond in time
%         otc = NaN;
%         txt1 = 'did not respond in time';
%         txt2 = '-$5';
%         txt3 = '$0';
%     end
%     
%     output.pickt = pickt;
%     output.otc = otc;
%     
%     DrawFormattedText(wind,sprintf('Randomly selected trial: #%d.\n\nOn this trial, you %s. As a result, we will adjust the $5 we''ve already given you by %s, for a total bonus of %s.\n\n\nNext, whenever you''re ready, we have the heartbeat counting task!',pickt,txt1,txt2,txt3),'center','center',blk,40);
%     Screen('Flip',wind);
    DrawFormattedText(wind,'Take a moment now to relax.\n\nNext, whenever you''re ready, we have the heartbeat counting task!','center','center',blk,40);
    Screen('Flip',wind);
    while 1
        [keyIsDown, ~, keyCode] = KbCheck;
        if (keyIsDown && size(find(keyCode), 2) == 1);
            if keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif any(keyCode(trig_key_code))
                Screen('Flip', wind);
                break;
            end
        end
    end
    
    % ---------- COUNTING TASK ---------- %
    
    WaitSecs(2);
    
    ctrials=[25
        30
        35
        40
        45
        50];
    ncT = length(ctrials);
    torder = randperm(ncT); % random ordering
    nHBs = nan(length(ctrials),2); % # of HBs in col 1, # of seconds in col 2
    
    DrawFormattedText(wind,'We''re now ready to begin the Counting Task.\n\nRemember, you just need to start counting your heartbeat when the screen says "COUNT". Sometimes you''ll count for a short amount of time, others for a longer amount of time. Just keep going until the screen says "Time''s up!". When time is up, you''ll tell your experimenter how many heartbeats you counted.\n\nPlease don''t feel for your heartbeat directly - try to sense it instead.','center','center',blk,40);
    DrawFormattedText(wind,'Waiting for experimenter to start the task','center',.9*h,blk,40);
    Screen('Flip',wind);
    WaitSecs(1);
    
    while 1
        [keyIsDown, ~, keyCode] = KbCheck;
        if (keyIsDown && size(find(keyCode), 2) == 1);
            if keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif any(keyCode(trig_key_code))
                Screen('Flip', wind);
                break;
            end
        end
    end
    
    for t = 1:ncT
        DrawFormattedText(wind,'Beginning the next trial in 3 seconds...','center','center',blk,40);
        Screen('Flip',wind); startT = GetSecs;
        while (GetSecs - startT) < 3
            [keyIsDown, secs, keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                end
            end
        end
        
        nc = 0;
        
        DrawFormattedText(wind,'COUNT','center','center',blk,50);
        Screen('Flip',wind);
        startT = GetSecs;
        
        if doHB
            outp(adOut,0); % zero the parallel port
            while ctrials(torder(t)) > (GetSecs-startT)
                if inp(adIn) < 60 % if HB trigger
                    nc = nc + 1; % increment the count
                    while inp(adIn) < 60 % wait until the trigger resets (only finishes when inp pulls a value > 60)
                        % loop
                    end
                end
            end
        end
        
        nHBs(t,1) = nc; nHBs(t,2) = ctrials(torder(t));
        
        DrawFormattedText(wind,'Time''s up!\n\nPlease tell your experimenter how many heartbeats you counted.','center','center',blk,40);
        Screen('Flip',wind);
        
        while 1
            [keyIsDown, ~, keyCode] = KbCheck;
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if keyCode(esc_key_code)
                    error('Experiment aborted by user!');
                elseif any(keyCode(trig_key_code))
                    Screen('Flip', wind);
                    break;
                end
            end
        end
        
        WaitSecs(1);

    end

    DrawFormattedText(wind,'You''re done with both heartbeat tasks!\n\n\nNow we have a few short questionnaires, and then you''re done!','center','center',blk,40);
    Screen('Flip',wind);
    
    output.count.ctrials = ctrials;
    output.count.torder = torder;
    output.count.nHBs = nHBs;
    
    %----------- CLEAN UP ---------------%
    
    save(sprintf('ediTask_EDI%s_%.4f.mat',subjID,now),'output')
    
    PsychPortAudio('Close');
    
    WaitSecs(3);
    
    while 1
        [keyIsDown,~,keyCode] = KbCheck;
        if (keyIsDown && size(find(keyCode), 2) == 1);
            if any(keyCode(trig_key_code))
                Screen('Flip', wind);
                break;
            end
        end
    end
    
    if isreal
        ShowCursor
    end
    
    cd(taskpath); % move back to the directory with the task
    
    sca
    Priority(0);
    
catch ME
    PsychPortAudio('Close');
    try
        save(sprintf('ediTask_EDI%s_%.4f.mat',subjID,now),'output')
    end
    ShowCursor;
    sca
    Priority(0);
    rethrow(ME);
end

