% Script to analyze EDI Heartbeat Detection (HBD) data
%
% 

base_github_path = '/Users/sokolhessner/Documents/gitrepos/edi/';
base_data_path = '/Volumes/shlab/Projects/EDI/data/';
raw_data_path = [base_data_path 'raw/'];

fnames = dir([raw_data_path '/*/ediTask_EDI*.mat']);

nS = length(fnames);
subjIDs = cell(0,1);
for s = 1:nS
    subjIDs{s,1} = fnames(s).name(9:14);
end

HT = nan(nS,1);
MI = nan(nS,1);
CR = nan(nS,1);
FA = nan(nS,1);

pcorrLC = nan(nS,1);
pcorrHC = nan(nS,1);
ratingcorr = nan(nS,2); % r and p
meanjudg = nan(nS,1);
meanconf = nan(nS,1);
sdconf = nan(nS,1);
confWrong = nan(nS,1);
confRight = nan(nS,1);
missedTs = nan(nS,1);

% Meta-d' things
M_dprime = nan(nS,1);
M_ratio = nan(nS,1);
M_diff = nan(nS,1);
nQ = 4; % desired number of end responses
bin_counts = nan(nS,nQ,2);

nR_S1 = cell(nS,1);
nR_S2 = cell(nS,1);

rtmtx = nan(nS,160);

pcorrHalf = nan(nS,2); % p(correct) by half
bTime = nan(nS,2); % beta and p-value for linear effect of time on accuracy

manually_entered_data = readtable([raw_data_path 'EDI_Post_Study_Questionnaire_Quant.xlsx']);

countdata = nan(6,3,nS); % trials by details (time, nHB, reported count) by subj
% repcounts = xlsread('ILD_CountEstimates.xlsx')';
% repcounts = repcounts(2:end,:);
countacc = nan(6,nS); % for the 6 trials
countaccM = nan(nS,1); % Mean count accuracy using standard formula
counterr = nan(6,nS); % error in counts for each of 6 trials
counterrM = nan(nS,1); % Mean error

for s = 1:nS
    fprintf('%s\n',subjIDs{s});
%     fn = dir(sprintf('ildTask_%s*.mat',subjIDs{s}));
    load([fnames(s).folder filesep fnames(s).name])
%     fprintf('  %s\n',output.subjID);
    
    % Get variables for the wrapper function
    ind = ~isnan(output.choice); % Don't include missed trials
    syncState = output.params.syncState(ind); % In or out of sync
    choice = output.choice(ind); % response
    rating = output.rating(ind); % rating
    
    % The below calculated assuming "in-sync" is signal.
    HT(s) = sum(choice(syncState==1)==1); % hit (say in-sync when in-sync)
    MI(s) = sum(choice(syncState==1)==0); % miss (say delayed when in-sync)
    CR(s) = sum(choice(syncState==0)==0); % correct rejection (say delayed when delayed)
    FA(s) = sum(choice(syncState==0)==1); % false alarm (say in-sync when delayed)
    
    rttmp = output.rt;
    rtmtx(s,1:length(rttmp)) = rttmp; % save out the RT data
    
    choicecorr = double(choice==syncState);
    
    pcorrHalf(s,1) = mean(choicecorr(1:round(length(choicecorr)/2)));
    pcorrHalf(s,2) = mean(choicecorr(round(length(choicecorr)/2):end));
    
    t = 1:output.params.nT; t = t';
    [btmp,stats] = robustfit(t(ind),choicecorr);
    bTime(s,1) = btmp(2); % average beta is not sig. different from zero (p = 0.5)
    bTime(s,2) = stats.p(2); % turns out 6/80 people have an individually significant effect of time
    
    missedTs(s) = sum(isnan(output.choice));
    
    [ratingcorr(s,1),ratingcorr(s,2)] = corr(rating,choicecorr);
    pcorrLC(s) = mean(choicecorr(rating <= median(rating)));
    pcorrHC(s) = mean(choicecorr(rating > median(rating)));
    
    meanjudg(s) = mean(choice);
    meanconf(s) = mean(rating);
    sdconf(s) = std(rating);
    
    confWrong(s) = mean(rating(choicecorr==0));
    confRight(s) = mean(rating(choicecorr==1));
    
    % Quantize the continuous ratings
    q = quantile(rating,nQ-1); 
    qrating = ones(size(rating));
    for i = 1:length(q)
        qrating(rating >= q(i)) = i+1; % replace continuous ratings w/ 1-4 ratings
    end
    for b = 1:nQ
        bin_counts(s,b,1) = sum(qrating(choicecorr==1)==b); % correct bins
        bin_counts(s,b,2) = sum(qrating(choicecorr==0)==b); % incorrect bins
    end
    
    [nR_S1{s},nR_S2{s}] = trials2counts(syncState,choice,qrating,nQ);
    
%     try
%         out = type2_SDT_MLE(syncState,choice,qrating,nQ,[],1); % assume equal variance
%         
%         M_ratio(s) = out.M_ratio; % meta-d'/d' (1 is optimal; expect ? 1)
%         M_diff(s) = out.M_diff; % meta-d' - d'
%         M_dprime(s) = out.meta_da; % meta-dprime itself
%     end
    
    if isfield(output,'count')
        try
            countdata(:,1,s) = output.count.nHBs(:,2); % put in the trial lengths
            countdata(:,2,s) = output.count.nHBs(:,1); % put in the actual HBs
%             countdata(:,3,s) = repcounts(repcounts(:,1)==str2num(subjIDs{s}(4:6)),2:end)'; % put in their reported counts
            countdata(:,3,s) = table2array(manually_entered_data(s,{'HBTrial1', 'HBTrial2', 'HBTrial3', 'HBTrial4', 'HBTrial5', 'HBTrial6'})); % put in their reported counts
            countacc(:,s) = 1-abs(countdata(:,2,s)-countdata(:,3,s))./((countdata(:,2,s)+countdata(:,3,s))/2);
            countaccM(s) = mean(countacc(:,s));
            counterr(:,s) = countdata(:,2,s) - countdata(:,3,s); % Observed - Reported
            counterrM(s) = mean(counterr(:,s));
        end
    end
end


HTr = HT./(HT + MI);
MIr = MI./(HT + MI);
CRr = CR./(CR + FA);
FAr = FA./(CR + FA);
dprime = norminv(HTr) - norminv(FAr);
dprime(isinf(dprime)) = norminv(HTr(isinf(dprime))) - norminv(1/length(output.rt)); % pad any no-FA people with 1/2N standard correction

pcorr = (HT + CR)./(HT + MI + CR + FA);
pcorrInSync = HT./(HT + MI);
pcorrDelayed = CR./(CR + FA);

btrChance = 1-binocdf(HT+CR,HT+MI+CR+FA,.5); % the probability of this score given a null hypothesis of chance performance
thresh = 0.05;
fprintf('\n%d participants significantly better than chance at p = %.2f (%d not)\n',sum(btrChance < thresh),thresh,nS-sum(btrChance < thresh))

ap = aprime(HTr,FAr); % A-Prime

apnorm = 2*asin(sqrt(ap)); % Normalized A-Prime

apcorr = ap; % Corrected A-Prime
for s = 1:length(apcorr)
    if apcorr(s) < 0.5
        apcorr(s) = aprime(FAr(s),HTr(s));
    end
end

cutoff = .25; % dprime cutoff; ends up removing the bottom 1/3rd of subjects
keepind = dprime >= cutoff;


figN = 0;

figN = figN + 1;
figure(figN); clf;
plot(FAr,HTr,'rx'); hold on;
plot([0 1],[0 1],'k')
axis square
xlim([0 1])
ylim([0 1])
xlabel('False Alarm Rate')
ylabel('Hit Rate')
title('ROC curve')

figN = figN + 1;
figure(figN); clf;
plot(pcorrHalf(:,1),pcorrHalf(:,2),'ko'); hold on;
plot([0 1],[0 1],'k')
plot([.5 .5],[0 1],'k--')
plot([0 1],[.5 .5],'k--')
xlim([0 1])
ylim([0 1])
axis square
xlabel('first half')
ylabel('second half')
[~,p] = ttest(pcorrHalf(:,1),pcorrHalf(:,2));
title(sprintf('p(correct); 1st vs. 2nd half p = %.2f',p))

figN = figN + 1;
figure(figN);clf;
plot(dprime,ones(size(dprime)),'go','markersize',10); hold on;
plot([0 0],[0 2],'k-')
plot([cutoff cutoff],[0 2],'r--');
ylim([0 2])
set(gca,'ytick',[])
xlabel('D-Prime')

figN = figN + 1;
figure(figN); clf;
ecdf(dprime);
xlabel('d-prime')
ylabel('probability of observing this value or lower')
hold on;
yl = ylim;
plot([cutoff cutoff],yl,'r--');

% export_fig '~/Desktop/tmp.eps' % NOT CURRENTLY INSTALLED

figN = figN + 1; 
figure(figN); clf
subplot(1,2,1)
plot(confWrong,confRight,'bo'); hold on;
plot([0 1],[0 1],'k')
axis([0 1 0 1])
axis square
xlabel('Confidence when Wrong')
ylabel('Confidence when Right')
title('Model-free: Confidence when Right/Wrong')

subplot(1,2,2)
plot(pcorrLC,pcorrHC,'bo'); hold on;
plot([0 1],[0 1],'k')
axis([0 1 0 1])
axis square
xlabel('p(Correct|Low Confidence)')
ylabel('p(Correct|High Confidence)')
title('Model-free: Performance when Low/High Confident')


figN = figN + 1; 
figure(figN); clf
plot(dprime,M_dprime,'ro'); hold on
lb = floor(min([min(dprime) min(M_dprime)]));
ub = ceil(max([max(dprime) max(M_dprime)]));
plot([lb ub],[lb ub],'k')
plot([lb ub],[0 0],'k--')
plot([0 0],[lb ub],'k--')
xlabel('D-PRIME [performance]')
ylabel('META D-PRIME [metacog]')
axis square
title('Single-Subj MLE: Meta-D'' and D''')

figN = figN + 1;
figure(figN); clf
subplot(1,5,1:4)
boxplot(countacc,'labels',subjIDs,'labelorientation','inline')
ylabel('Counting Accuracy across trials')
title('Counting Accuracy by Subject')
subplot(1,5,5)
boxplot(countaccM); hold on;
plotSpread(countaccM,'distributionMarkers','o','distributionColors','k')
ylabel('Mean Counting Accuracy Score')
title('Mean Counting Accuracy')

figN = figN + 1;
figure(figN); clf
subplot(1,6,1:4)
boxplot(counterr,'labels',subjIDs,'labelorientation','inline'); hold on;
plot([0 nS],[0 0],'k')
ylabel('Actual - Reported HBs for each trial')
title('Signed Error in Counting Task by Subject')
subplot(1,6,5)
boxplot(counterrM); hold on;
plot([0 2],[0 0],'k')
plotSpread(counterrM,'distributionMarkers','o','distributionColors','k')
ylabel('Mean(Actual - Reported HBs)')
title('Mean Signed Error')
subplot(1,6,6)
boxplot(abs(counterrM)); hold on;
% plot([0 2],[0 0],'k')
plotSpread(abs(counterrM),'distributionMarkers','o','distributionColors','k')
ylabel('|Mean(Actual - Reported HBs)|')
title('Mean Absolute Error')

figN = figN + 1;
figure(figN); clf
plot(countaccM,abs(counterrM),'mo')
xlabel('Counting Accuracy Score')
ylabel('Mean Absolute Error (|Actual - Reported HBs|)')
[r,p] = corr(abs(counterrM),countaccM,'rows','pairwise','type','spearman');
% The scores are highly correlated, e.g. r = -0.94 or so, with tiny p's,
% e.g. p = 5e-28, even if non-parametric
title(sprintf('Spearman''s Rho r = %.2f, p = %.2f',r,p))

figN = figN + 1;
figure(figN); clf
subplot(1,2,1)
plot(abs(counterrM),squeeze(mean(countdata(:,2,:))),'ko','markerfacecolor','g')
xlabel('Mean Absolute Error')
ylabel('Mean number of heartbeats')
[r,p] = corr(abs(counterrM),squeeze(mean(countdata(:,2,:))),'rows','pairwise','type','spearman');
title(sprintf('Spearman''s Rho r = %.2f, p = %.2f',r,p))
% ... but the mean absolute error is also correlated with the number of
% heartbeats (higher heartbeats, more error). 
subplot(1,2,2)
plot(countaccM,squeeze(mean(countdata(:,2,:))),'ko','markerfacecolor','r')
xlabel('Mean Accuracy Score')
ylabel('Mean number of heartbeats')
[r,p] = corr(countaccM,squeeze(mean(countdata(:,2,:))),'rows','pairwise','type','spearman');
title(sprintf('Spearman''s Rho r = %.2f, p = %.2f',r,p))
% ... while the accuracy score is weakly correlated with the # of HBs, p =
% 0.02



% Ran a few quick correlations; count accuracy is correlated with
% interoceptive performance! Not SUPER strong, p = 0.03 or so, r = 0.29,
% but it shows up with abs(counterrM) and countaccM. 


cd ../HMeta-d-master;

%%% Set some parameters
mcmc_params.doparallel = 0;
mcmc_params.nchains = 4;

%%% defaults
mcmc_params.response_conditional = 0;   % response-conditional meta-d?
mcmc_params.estimate_dprime = 0;    % also estimate dprime in same model?
% mcmc_params.nchains = 3; % How Many Chains?
mcmc_params.nburnin = 1000; % How Many Burn-in Samples?
mcmc_params.nsamples = 10000;  %How Many Recorded Samples?
mcmc_params.nthin = 1; % How Often is a Sample Recorded?
% mcmc_params.doparallel = 0; % Parallel Option
mcmc_params.dic = 1;

%%% Run it
% % pp = parpool(mcmc_params.nchains);
fit_all = fit_meta_d_mcmc_group(nR_S1,nR_S2,mcmc_params);
fit_good = fit_meta_d_mcmc_group(nR_S1(keepind),nR_S2(keepind),mcmc_params);
% % delete(pp)

save('edi_analysis_01-JAGSfitobj.mat','fit_all','fit_good');
save('edi_analysis_02-JAGSfitobj.mat','fit_all','fit_good');
% 
% load('ild_analysis_01-JAGSfitobj.mat');
% load('ild_analysis_02-JAGSfitobj.mat');

% fit_all and fit_good have similar M-ratio output.


participant_demos = readtable('ILDData.xlsx','Sheet','forMatlab');

[r,p] = corr(dprime,participant_demos.BMI) % no relationship (pearson or spearman)
[r,p] = corr(dprime,participant_demos.age) % no relationship (pearson or spearman)

[h,p,ci,stats] = ttest2(dprime(participant_demos.genderf1m0 == 1), ...
    dprime(participant_demos.genderf1m0 == 0)) 
% trend: p = 0.057; Wilcoxon rank-sum p = 0.08




cd(datapath);

output = table(cell2mat(subjIDs),pcorr,btrChance,pcorrHalf,meanconf,...
    dprime,M_dprime,M_ratio,fit_all.meta_d',fit_all.Mratio',...
    countaccM,counterrM,abs(counterrM),...
    'VariableNames',{'SubjectID','pcorrect','pbetterthanchance','pcorrectbyhalf',...
    'MeanConfidence','dprime','metadprimeMLE','MRatioMLE','metadprimeBayesian','MRatioBayesian',...
    'CountAccuracy','CountError','AbsCountError'});

writetable(output,'ILD_taskperformance_output.csv')




regoutput = [table(TATLdata(:,1)) output(tatlind,:) table(ovacc,fofacc,pleacc,keepSCL,keepPPG,...
    TL_FOF_PPG_diff,TL_P_PPG_diff,TL_FOF_SCL_diff,TL_P_SCL_diff)];

writetable(regoutput,'ILD_forregression_output.csv')





% Predicting accuracy in lie detection with mix of physio during lie
% detection and interoception