% Script to analyze ILD data
%
% PSH 1/25/18

basepath = '/Users/sokolhessner/Documents/Dropbox/Libraries/MATLAB_lib/ILD';
datapath = [basepath '/data-2018_07_11/'];
cd(datapath);

fnames = dir('ildTask_ILD*.mat');

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

countdata = nan(6,3,nS); % trials by details (time, nHB, reported count) by subj
repcounts = xlsread('ILD_CountEstimates.xlsx')';
repcounts = repcounts(2:end,:);
countacc = nan(6,nS); % for the 6 trials
countaccM = nan(nS,1); % Mean count accuracy using standard formula
counterr = nan(6,nS); % error in counts for each of 6 trials
counterrM = nan(nS,1); % Mean error

for s = 1:nS
    fprintf('%s\n',subjIDs{s});
    fn = dir(sprintf('ildTask_%s*.mat',subjIDs{s}));
    load(fn.name)
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
            countdata(:,3,s) = repcounts(repcounts(:,1)==str2num(subjIDs{s}(4:6)),2:end)'; % put in their reported counts
            if s == 11
                countdata(:,:,s) = NaN; % ILD014 had something wrong with their EKG - recorded 0, 0, 0, 3, 3, and 5 heartbeats during their intervals. Data is useless.
            end
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

% %%% Run it
% % % pp = parpool(mcmc_params.nchains);
% fit_all = fit_meta_d_mcmc_group(nR_S1,nR_S2,mcmc_params);
% fit_good = fit_meta_d_mcmc_group(nR_S1(keepind),nR_S2(keepind),mcmc_params);
% % % delete(pp)

% save('ild_analysis_01-JAGSfitobj.mat','fit_all','fit_good');
% save('ild_analysis_02-JAGSfitobj.mat','fit_all','fit_good');
% 
% load('ild_analysis_01-JAGSfitobj.mat');
load('ild_analysis_02-JAGSfitobj.mat');

% fit_all and fit_good have similar M-ratio output.


participant_demos = readtable('ILDData.xlsx','Sheet','forMatlab');

[r,p] = corr(dprime,participant_demos.BMI) % no relationship (pearson or spearman)
[r,p] = corr(dprime,participant_demos.age) % no relationship (pearson or spearman)

[h,p,ci,stats] = ttest2(dprime(participant_demos.genderf1m0 == 1), ...
    dprime(participant_demos.genderf1m0 == 0)) 
% trend: p = 0.057; Wilcoxon rank-sum p = 0.08

cd('../')
TATLdata = xlsread('ILDDataforPeter08092018.xlsx','Data');
%{
1 = ID
BEHAVIOR
    22 = truth fof percent acc
    23 = lie fof percent acc
    24 = truth p percent acc
    25 = lie p percent acc
PHYSIO (all v. baseline)
    13 = truth fof PPG
    14 = lie fof PPG
    15 = truth fof SCL
    16 = lie fof SCL
    17 = truth p PPG
    18 = lie p PPG
    19 = truth p SCL
    20 = lie p SCL
PHYSIO TRIAL-LEVEL
    27-90
EMOTION RATINGS
    92-98 - Truth, Ple
    99-105 - Lie, Ple
    106-112 - Truth-Lie Ple
        Happy
        Sad
        Fear
        Anger
        Disgust
        Contempt
        NoEmo
%}

% Remove one TATL participant who is not in ILD
TATLdata = TATLdata(TATLdata(:,1)~=109,:);

keepSCL = logical(TATLdata(:,7)==0);
keepPPG = logical(TATLdata(:,8)==0);

tmp = importdata('ILDTATLReconciliationSheet.csv');
idKey = sortrows([(1:nS)' sortrows(tmp.data,1)],3); % col1 = ILD, col2 = TATL
tatlind = idKey(isfinite(idKey(:,3)),1); % This index puts ILD data into TATL space.

nST = length(tatlind);

figN = figN + 1;
figure(figN); clf
subplot(1,4,1:2)
boxplot(TATLdata(:,22:25),'labels',{'Truth FOF','Lie FOF','Truth P','Lie P'})
ylabel('Percent Accuracy')
subplot(1,4,3)
boxplot([mean(TATLdata(:,22:23),2) mean(TATLdata(:,24:25),2)],'labels',{'Overall FOF Accuracy','Overall P accuracy'})
subplot(1,4,4)
boxplot(mean(TATLdata(:,22:25),2),'labels','Overall Accuracy'); hold on;
plot(ones(nST,1)+.1*randn(nST,1),mean(TATLdata(:,22:25),2),'ko')

mean(mean(TATLdata(:,22:25),2)); % mean accuracy 53.05%

ovacc = mean(TATLdata(:,22:25),2); % 53.1%
fofacc = mean(TATLdata(:,22:23),2); % 51.8%
pleacc = mean(TATLdata(:,24:25),2); % 54.3%
[~,p] = ttest(fofacc,pleacc) % no significant difference between FOF & P accuracy (p = 0.19)
[r,p] = corr(fofacc,pleacc) % NOT correlated (Pearson's r = 0.12, p = 0.34; Spearman's Rho = 0.20, p = 0.09)

[~,p] = ttest(fofacc,50) % not significantly better than chance as a group (p = 0.16)
[~,p] = ttest(pleacc,50) % significantly better than chance p = 0.01
[~,p] = ttest(ovacc,50) % significantly better than chance p = 0.006


nTfof = 20;
nTple = 12;
btrChancefof = 1-binocdf(fofacc/100*nTfof,nTfof,.5); % the probability of this score given a null hypothesis of chance performance
btrChanceple = 1-binocdf(pleacc/100*nTple,nTple,.5); % the probability of this score given a null hypothesis of chance performance
thresh = 0.05;
sum(btrChancefof < thresh) % 3
sum(btrChanceple < thresh) % 7

sum(btrChancefof < thresh)/nST % 4% of participants do better than chance; that's... chance.
sum(btrChanceple < thresh)/nST % 10% of participants do better than chance; there's some performance here
% can say that 3 people perform individually significantly better than
% chance on FOF task; 7 on Pleader task (out of 71)

figN = figN + 1;
figure(figN); clf
plot(fofacc+randn(nST,1)*.4,pleacc+randn(nST,1)*.4,'ro')
xlabel('Friend or Foe Accuracy')
ylabel('Pleader Accuracy')
axis square
xlim([0 100])
ylim([0 100])
hold on;
plot([0 100],[0 100],'k')
plot([50 50],[0 100],'k')
plot([0 100],[50 50],'k')



% So all signs are that FOF performance is at chance (at the group level;
% at the individual level). There are signs that pleader performance may be
% better than chance.
%
% FOCUS ON PLEADER ACCURACY

% Calculate truth bias (percentage of time they said "truth")
biasfof = mean([TATLdata(:,22) (100-TATLdata(:,23))],2);
biasple = mean([TATLdata(:,24) (100-TATLdata(:,25))],2);
[~,p] = ttest(biasfof,biasple) % not significantly different from one another (p = 0.32)
[r,p] = corr(biasfof,biasple) % significantly correlated (Pearson's r = 0.32, p = 0.007; Spearman's Rho = 0.32, p = 0.007
biasov = mean([biasfof biasple],2);
[~,p] = ttest(biasov,50) % significant bias 

figN = figN + 1;
figure(figN); clf
plot(biasfof+randn(nST,1)*.5,biasple+randn(nST,1)*.5,'ko')
xlabel('FOF Truth Bias')
ylabel('PLE Truth Bias')
axis square
xlim([0 100])
ylim([0 100])
hold on;
plot([0 100],[0 100],'k')
plot([50 50],[0 100],'k')
plot([0 100],[50 50],'k')

% So bias is correlated across domains while accuracy is not.
% and there is a "truth bias" (mean p(say truth) = 0.53, p = 0.01)
%
% POSSIBLE "TRUTH BIAS" TO WORK WITH?



% PPG
figN = figN + 1;
figure(figN); clf
subplot(1,2,1)
boxplot(TATLdata(keepPPG,[13,14,17,18]),'labels',{'Truth FOF','Lie FOF','Truth P','Lie P'})
ylabel('PPG')
subplot(1,2,2)
boxplot(TATLdata(keepSCL,[15,16,19,20]),'labels',{'Truth FOF','Lie FOF','Truth P','Lie P'})
ylabel('SCL')

figN = figN + 1;
figure(figN); clf
subplot(1,4,1)
boxplot(TATLdata(keepPPG,13)-TATLdata(keepPPG,14),'labels',{'Truth - Lie FOF'})
ylabel('PPG')
subplot(1,4,2)
boxplot(TATLdata(keepPPG,17)-TATLdata(keepPPG,18),'labels',{'Truth - Lie P'})
ylabel('PPG')
subplot(1,4,3)
boxplot(TATLdata(keepSCL,15) - TATLdata(keepSCL,16),'labels',{'Truth - Lie FOF'})
ylabel('SCL')
subplot(1,4,4)
boxplot(TATLdata(keepSCL,19) - TATLdata(keepSCL,20),'labels',{'Truth - Lie P'})
ylabel('SCL')

T_FOF_PPG = TATLdata(:,13);
L_FOF_PPG = TATLdata(:,14);
T_P_PPG = TATLdata(:,17);
L_P_PPG = TATLdata(:,18);
T_FOF_SCL = TATLdata(:,15);
L_FOF_SCL = TATLdata(:,16);
T_P_SCL = TATLdata(:,19);
L_P_SCL = TATLdata(:,20);

TL_FOF_PPG_diff = T_FOF_PPG - L_FOF_PPG;
TL_P_PPG_diff = T_P_PPG - L_P_PPG;
TL_FOF_SCL_diff = T_FOF_SCL - L_FOF_SCL;
TL_P_SCL_diff = T_P_SCL - L_P_SCL;

FOF_PPG_reactivity = mean([T_FOF_PPG L_FOF_PPG],2);
P_PPG_reactivity = mean([T_P_PPG L_P_PPG],2);
FOF_SCL_reactivity = mean([T_FOF_SCL L_FOF_SCL],2);
P_SCL_reactivity = mean([T_P_SCL L_P_SCL],2);
[~,p] = ttest(FOF_PPG_reactivity(keepPPG),0) % all very significant (all p's < 0.000009)
[~,p] = ttest(P_PPG_reactivity(keepPPG),0)
[~,p] = ttest(FOF_SCL_reactivity(keepSCL),0)
[~,p] = ttest(P_SCL_reactivity(keepSCL),0)


% FOF testing
[~,p] = ttest(TL_FOF_PPG_diff(keepPPG)) % FOF test T vs. L NS  0.35
[p] = signrank(TL_FOF_PPG_diff(keepPPG)) % FOF test T vs. L NS 0.23
[~,p] = ttest(TL_FOF_SCL_diff(keepSCL)) % FOF test T vs. L SIG  0.005
[p] = signrank(TL_FOF_SCL_diff(keepSCL)) % FOF test T vs. L SIG 0.002

% PLE testing
[~,p] = ttest(TL_P_PPG_diff(keepPPG)) % P test T vs. L SIG   0.000005
[p] = signrank(TL_P_PPG_diff(keepPPG)) % P test T vs. L SIG  0.000009
[~,p] = ttest(TL_P_SCL_diff(keepSCL)) % P test T vs. L NS     0.97
[p] = signrank(TL_P_SCL_diff(keepSCL)) % P test T vs. L SIG   0.01

% For the FOF task, SCL differentiates between Truth and Lie (but PPG does
% not). Weak effect.
% Lower SCL to TRUTH vs. LIE (70% of participants, mean Truth  - Lie = 
% -1.8). 
% Numerically higher PPG to TRUTH vs. LIE (58% of participants, mean =
% 0.0013)

% For the PLE task, PPG differentiates between Truth and Lie (but SCL does
% not). STRONG effect.
% Higher PPG to TRUTH vs. LIE (73% of participants, mean = 0.009)
% Numerically lower SCL to TRUTH vs. LIE (67% of paricipants, mean = -0.07)


tlevel_groundtruth = xlsread('ILD_TATL_triallevel_groundtruth.xlsx');
tlevel_tl = tlevel_groundtruth(:,1)';
tlevel_fofple = tlevel_groundtruth(:,2)';
tlevel_ppgscl = tlevel_groundtruth(:,3)';

tlevel_data = TATLdata(:,27:end);

nParams = 4 * 2; % parameters per participant X pleader/FOF

niter = 200;
params = nan(nST,nParams,2); % subjects X parameters X estimates/errors
nll = nan(nST,2); % subjects X pleader/FOF
output = cell(nST,2); % output struct
pctpred = nan(nST,2); % subjects X pleader/FOF


for s = 1:nST
    if keepPPG(s) & keepSCL(s)
        tic
        % FOF
        data = [tlevel_data(s,(tlevel_fofple==1)&(tlevel_ppgscl==1))'...
            tlevel_data(s,(tlevel_fofple==1)&(tlevel_ppgscl==2))'... 
            tlevel_tl((tlevel_fofple==1)&(tlevel_ppgscl==1))'];
        
        [params(s,1:(nParams/2),1),...
            params(s,1:(nParams/2),2),...
            nll(s,1),output{s,1}] = estimparamsH_ILD_m1(data,niter);
        
        %PLE
        data = [tlevel_data(s,(tlevel_fofple==2)&(tlevel_ppgscl==1))'...
            tlevel_data(s,(tlevel_fofple==2)&(tlevel_ppgscl==2))'... 
            tlevel_tl((tlevel_fofple==2)&(tlevel_ppgscl==1))'];
        
        [params(s,(nParams/2+1):nParams,1),...
            params(s,(nParams/2+1):nParams,2),...
            nll(s,2),output{s,2}] = estimparamsH_ILD_m1(data,niter);
        toc
    end
end

maxacc = nan(nST,2);
for s = 1:nST
    if keepPPG(s) & keepSCL(s)
        % FOF
        data = [tlevel_data(s,(tlevel_fofple==1)&(tlevel_ppgscl==1))'...
            tlevel_data(s,(tlevel_fofple==1)&(tlevel_ppgscl==2))'... 
            tlevel_tl((tlevel_fofple==1)&(tlevel_ppgscl==1))'];
        [~,choiceP] = nll_ppgscl_truthvslie(params(s,1:4,1),data);
        maxacc(s,1) = sum(data(:,3) == (choiceP > 0.5))/length(choiceP);

        % PLE
        data = [tlevel_data(s,(tlevel_fofple==2)&(tlevel_ppgscl==1))'...
            tlevel_data(s,(tlevel_fofple==2)&(tlevel_ppgscl==2))'... 
            tlevel_tl((tlevel_fofple==2)&(tlevel_ppgscl==1))'];
        [~,choiceP] = nll_ppgscl_truthvslie(params(s,5:8,1),data);
        maxacc(s,2) = sum(data(:,3) == (choiceP > 0.5))/length(choiceP);

    end
end

% PSH SEE: https://www.mathworks.com/help/stats/perfcurve.html
% and https://www.mathworks.com/matlabcentral/answers/396532-how-to-calculate-auc-of-roc-curve-from-these-data

% indiv_fof_ppg_pval = nan(nST,1); % for two-sample t-tests of truth vs. lie
% indiv_ple_ppg_pval = nan(nST,1);
% indiv_fof_scl_pval = nan(nST,1);
% indiv_ple_scl_pval = nan(nST,1);
% 
% indiv_fof_ppg_optimal = nan(nST,1); % for the optimal this person could do with a hard cutoff in this task using this physio measure
% indiv_ple_ppg_optimal = nan(nST,1);
% indiv_fof_scl_optimal = nan(nST,1);
% indiv_ple_scl_optimal = nan(nST,1);
% 
% figN = figN + 1;
% for s = 1:nST
%     tic; 
%     figure(figN); clf
%     if keepPPG(s)==1
%         [~,indiv_fof_ppg_pval(s)] = ttest2(tlevel_data(s,(tlevel_tl==1)&(tlevel_fofple==1)&(tlevel_ppgscl==1)),...
%                                            tlevel_data(s,(tlevel_tl==0)&(tlevel_fofple==1)&(tlevel_ppgscl==1)));
%         [~,indiv_ple_ppg_pval(s)] = ttest2(tlevel_data(s,(tlevel_tl==1)&(tlevel_fofple==2)&(tlevel_ppgscl==1)),...
%                                            tlevel_data(s,(tlevel_tl==0)&(tlevel_fofple==2)&(tlevel_ppgscl==1)));
%         
%         % Do FOF, PPG
%         tmpind = (tlevel_fofple==1)&(tlevel_ppgscl==1); % which game & physio variable
%         possinc = diff(sort(tlevel_data(s,tmpind)));
%         possinc = possinc(possinc > 0);
%         inc = min(0.0001,min(possinc)/10);
%         tmpcrit = min(tlevel_data(s,tmpind))-inc:inc:max(tlevel_data(s,tmpind))+inc;
%         tmpperf = nan(size(tmpcrit));
%         for t = 1:length(tmpcrit)
%             tmpperf(t) = sum(tlevel_data(s,(tlevel_tl==1)&tmpind) > tmpcrit(t)) + ... % NOTE: higher PPG to truth than lie
%                          sum(tlevel_data(s,(tlevel_tl==0)&tmpind) < tmpcrit(t));
%             tmpperf(t) = tmpperf(t)/sum(tmpind);
%         end
%         indiv_fof_ppg_optimal(s) = max(tmpperf);
%         
%         subplot(4,1,1)
%         plot(tmpcrit,tmpperf,'b'); hold on % the criterion values against the performance
%         ytmp = mean(tmpperf); 
%         plot(tlevel_data(s,(tlevel_tl==1)&tmpind),ytmp,'go')
%         plot(tlevel_data(s,(tlevel_tl==0)&tmpind),ytmp,'ro')
%         title(sprintf('Subject %d for FOF & PPG',TATLdata(s,1)));
%         ylabel('performance')
%         xlabel('Criterion')
% 
%         % Do PLE, PPG
%         tmpind = (tlevel_fofple==2)&(tlevel_ppgscl==1);
%         possinc = diff(sort(tlevel_data(s,tmpind)));
%         possinc = possinc(possinc > 0);
%         inc = min(0.0001,min(possinc)/10);
%         tmpcrit = min(tlevel_data(s,tmpind))-inc:inc:max(tlevel_data(s,tmpind))+inc;
%         tmpperf = nan(size(tmpcrit));
%         for t = 1:length(tmpcrit)
%             tmpperf(t) = sum(tlevel_data(s,(tlevel_tl==1)&tmpind) > tmpcrit(t)) + ...
%                          sum(tlevel_data(s,(tlevel_tl==0)&tmpind) < tmpcrit(t));
%             tmpperf(t) = tmpperf(t)/sum(tmpind);
%         end
%         indiv_ple_ppg_optimal(s) = max(tmpperf);
% 
%         subplot(4,1,2)
%         plot(tmpcrit,tmpperf,'b'); hold on % the criterion values against the performance
%         ytmp = mean(tmpperf); 
%         plot(tlevel_data(s,(tlevel_tl==1)&tmpind),ytmp,'go')
%         plot(tlevel_data(s,(tlevel_tl==0)&tmpind),ytmp,'ro')
%         title(sprintf('Subject %d for PLE & PPG',TATLdata(s,1)));
%         ylabel('performance')
%         xlabel('Criterion')
%     end
%     if keepSCL(s)==1
%         [~,indiv_fof_scl_pval(s)] = ttest2(tlevel_data(s,(tlevel_tl==1)&(tlevel_fofple==1)&(tlevel_ppgscl==2)),...
%                                            tlevel_data(s,(tlevel_tl==0)&(tlevel_fofple==1)&(tlevel_ppgscl==2)));
%         [~,indiv_ple_scl_pval(s)] = ttest2(tlevel_data(s,(tlevel_tl==1)&(tlevel_fofple==2)&(tlevel_ppgscl==2)),...
%                                            tlevel_data(s,(tlevel_tl==0)&(tlevel_fofple==2)&(tlevel_ppgscl==2)));
%         
%         % Do FOF, SCL
%         tmpind = (tlevel_fofple==1)&(tlevel_ppgscl==2);
%         possinc = diff(sort(tlevel_data(s,tmpind)));
%         possinc = possinc(possinc > 0);
%         inc = min(0.0001,min(possinc)/10);
%         tmpcrit = min(tlevel_data(s,tmpind))-inc:inc:max(tlevel_data(s,tmpind))+inc;
%         tmpperf = nan(size(tmpcrit));
%         for t = 1:length(tmpcrit)
%             tmpperf(t) = sum(tlevel_data(s,(tlevel_tl==1)&tmpind) < tmpcrit(t)) + ... % NOTE: Lower SCL to truth than lie
%                          sum(tlevel_data(s,(tlevel_tl==0)&tmpind) > tmpcrit(t));
%             tmpperf(t) = tmpperf(t)/sum(tmpind);
%         end
%         indiv_fof_scl_optimal(s) = max(tmpperf);
%         
%         subplot(4,1,3)
%         plot(tmpcrit,tmpperf,'b'); hold on % the criterion values against the performance
%         ytmp = mean(tmpperf); 
%         plot(tlevel_data(s,(tlevel_tl==1)&tmpind),ytmp,'go')
%         plot(tlevel_data(s,(tlevel_tl==0)&tmpind),ytmp,'ro')
%         title(sprintf('Subject %d for FOF & SCL',TATLdata(s,1)));
%         ylabel('performance')
%         xlabel('Criterion')
%         
%         % Do PLE, SCL
%         tmpind = (tlevel_fofple==2)&(tlevel_ppgscl==2);
%         possinc = diff(sort(tlevel_data(s,tmpind)));
%         possinc = possinc(possinc > 0);
%         inc = min(0.0001,min(possinc)/10);
%         tmpcrit = min(tlevel_data(s,tmpind))-inc:inc:max(tlevel_data(s,tmpind))+inc;
%         tmpperf = nan(size(tmpcrit));
%         for t = 1:length(tmpcrit)
%             tmpperf(t) = sum(tlevel_data(s,(tlevel_tl==1)&tmpind) < tmpcrit(t)) + ...
%                          sum(tlevel_data(s,(tlevel_tl==0)&tmpind) > tmpcrit(t));
%             tmpperf(t) = tmpperf(t)/sum(tmpind);
%         end
%         indiv_ple_scl_optimal(s) = max(tmpperf);
%         
%         subplot(4,1,4)
%         plot(tmpcrit,tmpperf,'b'); hold on % the criterion values against the performance
%         ytmp = mean(tmpperf); 
%         plot(tlevel_data(s,(tlevel_tl==1)&tmpind),ytmp,'go')
%         plot(tlevel_data(s,(tlevel_tl==0)&tmpind),ytmp,'ro')
%         title(sprintf('Subject %d for PLE & SCL',TATLdata(s,1)));
%         ylabel('performance')
%         xlabel('Criterion')
% 
%     end
%     x = toc;
%     fprintf('Done with Subject %d of %d in %.1f seconds.\n',s,nST,x);
%     saveas(gcf,sprintf('./optphysiographs/%d_optphysio.png',s));
% end
% 
% tmpoutput = table(TATLdata(:,1),indiv_fof_ppg_pval, indiv_ple_ppg_pval, ...
%     indiv_fof_scl_pval, indiv_ple_scl_pval, ...
%     indiv_fof_ppg_optimal, indiv_ple_ppg_optimal, ...
%     indiv_fof_scl_optimal, indiv_ple_scl_optimal, ...
%     'VariableNames',{'TATLID',...
%     'indiv_fof_ppg_pval','indiv_ple_ppg_pval',...
%     'indiv_fof_scl_pval','indiv_ple_scl_pval',...
%     'indiv_fof_ppg_optimal','indiv_ple_ppg_optimal',...
%     'indiv_fof_scl_optimal','indiv_ple_scl_optimal'});
% 
% writetable(tmpoutput,'ILD_physio_ttests_optimal.csv')

% T = readtable('ILD_physio_ttests_optimal.csv'); % for some reason this broke?!
T = importfileILDdata('ILD_physio_ttests_optimal.csv');
indiv_fof_ppg_pval = T.indiv_fof_ppg_pval;
indiv_ple_ppg_pval = T.indiv_ple_ppg_pval;
indiv_fof_scl_pval = T.indiv_fof_scl_pval;
indiv_ple_scl_pval = T.indiv_ple_scl_pval;
indiv_fof_ppg_optimal = T.indiv_fof_ppg_optimal;
indiv_ple_ppg_optimal = T.indiv_ple_ppg_optimal;
indiv_fof_scl_optimal = T.indiv_fof_scl_optimal;
indiv_ple_scl_optimal = T.indiv_ple_scl_optimal;


% How many people have individually significant diffs btwn TRUTH and LIE
% in the FOF or PLE tasks using either PPG or SCL? (chance = 5%)
sum(indiv_fof_ppg_pval < .05) % 5 / 62  8.1%
sum(indiv_fof_scl_pval < .05) % 4 / 63  6.4%
sum(indiv_ple_ppg_pval < .05) % 2 / 62  3.2%
sum(indiv_ple_scl_pval < .05) % 4 / 63  6.4%
% No evidence on the individual participant level that PPG or SCL reliably
% discriminates truth from lie in the FOF or PLE tasks, but # of trials is
% low for two-sample t-tests (6 vs 6 and 10 vs 10).

% What is average optimal performance, given only physio measures & optimal
% choice of criterion? (and group-level truth vs. lie pattern)
mean(indiv_fof_ppg_optimal(keepPPG)) % 63%
mean(indiv_fof_scl_optimal(keepSCL)) % 65%

mean(indiv_ple_ppg_optimal(keepPPG)) % 68%
mean(indiv_ple_scl_optimal(keepSCL)) % 66%
% Highly correlated with the T vs. L difference scores
figN = figN + 1;
figure(figN); clf
plot(indiv_ple_ppg_optimal(keepPPG),TL_P_PPG_diff(keepPPG),'ro')
xlabel('Optimal possible performance | Physio')
ylabel('Mean Truth - Lie Physio diffs')
title('PPG Responses in the Pleader Task')

figN = figN + 1;
figure(figN); clf
boxplot(indiv_ple_ppg_optimal(keepPPG)-pleacc(keepPPG)/100)
ylabel('(Optimal|Physio) - Actual')
hold on
plot(ones(sum(keepPPG),1)+randn(sum(keepPPG),1)*.05,indiv_ple_ppg_optimal(keepPPG)-pleacc(keepPPG)/100,'ko')

[r,p] = corr(indiv_ple_ppg_optimal(keepPPG),pleacc(keepPPG),'type','spearman') % very non-significant
% no relationship of optimal possible accuracy with actual accuracy
figN = figN + 1;
figure(figN); clf
plot(pleacc(keepPPG)/100+randn(sum(keepPPG),1)*.01,indiv_ple_ppg_optimal(keepPPG)+randn(sum(keepPPG),1)*.01,'m*')
xlabel('Actual Accuracy')
ylabel('Optimal Accuracy|Physio')

pleppg_error = indiv_ple_ppg_optimal-pleacc/100; pleppg_error(keepPPG==0) = nan;
fofppg_error = indiv_fof_ppg_optimal-fofacc/100; fofppg_error(keepPPG==0) = nan;
plescl_error = indiv_ple_scl_optimal-pleacc/100; plescl_error(keepSCL==0) = nan;
fofscl_error = indiv_fof_scl_optimal-fofacc/100; fofscl_error(keepSCL==0) = nan;


% Publication Figure Drafting

figN = figN + 1;
figure(figN); clf;
plot(pleppg_error,TL_P_PPG_diff,'o','MarkerEdgeColor',[0 0 1],'MarkerSize',10)
hold on; l1 = lsline;
l1.Color = 'k';
l1.LineWidth = 2;
axis square


figN = figN + 1;
figure(figN); clf;

subplot(1,3,1)
plot(dprime(tatlind),TL_P_PPG_diff,'o','MarkerEdgeColor',[1 0 0],'MarkerSize',10)
axis square
hold on; l1 = lsline;
l1.Color = 'r';
l1.LineWidth = 3;
xlabel('Interoception D-prime')
ylabel('Vasoconstriction Truth - Lie')

centers = (1:12)/12;
subplot(1,3,2)
hist(indiv_ple_ppg_optimal(keepPPG),centers');
hold on % ADDS ACTUAL ACCURACY DISTRIBUTION
hist(pleacc(keepPPG)/100,centers');
h = findobj(gca,'Type','patch');
h(1).FaceColor = [0.5 0.5 0.5];
h(1).FaceAlpha = 0.5;
h(2).FaceColor = [1 0 0];
h(2).FaceAlpha = 0.5;
% hold on;
% line([mean(pleacc(keepPPG))/100 mean(pleacc(keepPPG))/100],[0 100],'color','k','linewidth',3)
xlabel('Lie Detection Accuracy')
ylabel('Count')
xlim([0 1])
ylim([0 26])
axis square

subplot(1,3,3)
plot(dprime(tatlind),pleacc/100,'o','MarkerEdgeColor','k','MarkerSize',10)
axis square
xlabel('Interoception D-prime')
ylabel('Achieved Lie Detection Accuracy')
hold on; l1 = lsline;
l1.Color = 'k';
l1.LineWidth = 3;


[r,p] = corr(dprime(tatlind),TL_P_PPG_diff,'rows','complete')
[r,p] = corr(dprime(tatlind),TL_P_PPG_diff,'rows','complete','type','spearman')


[r,p] = corr(dprime(tatlind),pleacc/100)
[r,p] = corr(dprime(tatlind),pleacc/100,'type','spearman')


[h,p,stats] = swtest(dprime)
[h,p,stats] = swtest(dprime(tatlind))

[h,p,stats] = swtest(TL_P_PPG_diff)

[h,p,stats] = swtest(pleacc)



%%%% THE BELOW ISN'T QUITE TRUE; JUST ASKS WHO HAS THE OVERALL PATTERN OF SCL L > T).
% SO...
% ... in the FOF task, SCL can achieve 70% accuracy. 
% ... in the PLE task, PPG can achieve 73% accuracy.
% ... and the pattern of truth vs. lie is the same in FOF & PLE (for each
% of PPG and SCL, though their pattern is opposite from one another).

% Are physio differences to truth vs. lie correlated with accuracy in that
% domain?

% FOF
[r,p] = corr(TL_FOF_PPG_diff(keepPPG),fofacc(keepPPG)) % no relationship (p = 0.40 for pearson, p = 0.59 for spearman)
[r,p] = corr(TL_FOF_PPG_diff(keepPPG),fofacc(keepPPG),'type','spearman')

figN = figN + 1;
figure(figN); clf;
subplot(1,2,1)
plot(TL_FOF_PPG_diff(keepPPG),fofacc(keepPPG),'bo')
xlabel('T vs. L physio diff')
ylabel('Accuracy')
title('FOF, PPG')

[r,p] = corr(TL_FOF_SCL_diff(keepSCL),fofacc(keepSCL)) % no relationship (p = 0.38 for pearson, p = 0.38 for spearman)
[r,p] = corr(TL_FOF_SCL_diff(keepSCL),fofacc(keepSCL),'type','spearman')

subplot(1,2,2)
plot(TL_FOF_SCL_diff(keepSCL),fofacc(keepSCL),'bo')
xlabel('T vs. L physio diff')
ylabel('Accuracy')
title('FOF, SCL')

% PLE
[r,p] = corr(TL_P_PPG_diff(keepPPG),pleacc(keepPPG)) % no relationship (p = 0.83 for pearson, p = 0.66 for spearman)
[r,p] = corr(TL_P_PPG_diff(keepPPG),pleacc(keepPPG),'type','spearman')

figN = figN + 1;
figure(figN); clf;
subplot(1,2,1)
plot(TL_P_PPG_diff(keepPPG),pleacc(keepPPG),'bo')
xlabel('T vs. L physio diff')
ylabel('Accuracy')
title('PLE, PPG')

[r,p] = corr(TL_P_SCL_diff(keepSCL),pleacc(keepSCL)) % no relationship (p = 0.27 for pearson, p = 0.78 for spearman)
[r,p] = corr(TL_P_SCL_diff(keepSCL),pleacc(keepSCL),'type','spearman')

subplot(1,2,2)
plot(TL_P_SCL_diff(keepSCL),pleacc(keepSCL),'bo')
xlabel('T vs. L physio diff')
ylabel('Accuracy')
title('PLE, SCL')

%   SPOILER: NO RELATIONSHIPS BETWEEN PHYSIO T-L DIFFS AND PERCENT ACCURACY


figN = figN + 1;
figure(figN); clf
plot(dprime(tatlind),ovacc,'mo') % no correlation (spearman or pearson)
xlabel('dprime')
ylabel('Overall accuracy')

figN = figN + 1;
figure(figN); clf
plot(countaccM(tatlind),ovacc,'bo') % no correlation (spearman or pearson)
xlabel('Count Accuracy')
ylabel('Overall accuracy')


figN = figN + 1;
figure(figN); clf
plot(fit_all.Mratio(tatlind),ovacc,'bo') % no correlation (spearman or pearson)
xlabel('M-Ratio')
ylabel('Overall accuracy')
[r,p] = corr(fit_all.Mratio(tatlind)',ovacc) 
[r,p] = corr(fit_all.Mratio(tatlind)',ovacc,'type','spearman')




%{
EMOTION RATINGS FROM PLEADERS ONLY
    92-98 - Truth, Ple
    99-105 - Lie, Ple
    106-112 - Truth-Lie Ple
        Happy
        Sad
        Fear
        Anger
        Disgust
        Contempt
        NoEmo
%}

T_ple_emorating = TATLdata(:,92:98);
L_ple_emorating = TATLdata(:,99:105);
TL_ple_emorating = TATLdata(:,106:112);

for e = 1:7
    [~,p] = ttest(T_ple_emorating(:,e),L_ple_emorating(:,e));
    p = signrank(T_ple_emorating(:,e),L_ple_emorating(:,e));
end
%{
Happy       0.0011 (signrank 0.0007) (14 have T>L; 56 have T=L; 1 has L>T)
Sad         0.0105 (signrank 0.0135) (39 have T>L; 10 have T=L; 22 have L>T)
Fear        0.21   (signrank 0.25)
Anger       0.37   (signrank 0.24)
Disgust     0.02   (signrank 0.0071) (21 have T>L; 12 have T=L; 38 have L>T)
Contempt    0.96   (signrank 0.88)
NoEmo       0.98   (signrank 0.57)
%}
% Differences in HAPPY, SAD, and DISGUST
% Happy & Sad higher with T than L videos
% Disgust higher with L than T videos
%
% Pretty non-continuous. 
[r,p] = corr(TL_ple_emorating(:,1),TL_ple_emorating(:,2),'type','spearman') % p = 0.16
[r,p] = corr(TL_ple_emorating(:,2),TL_ple_emorating(:,5),'type','spearman') % p = 0.74
[r,p] = corr(TL_ple_emorating(:,1),TL_ple_emorating(:,5),'type','spearman') % p = 0.20
% None correlated with each other

[r,p] = corr(pleacc,TL_ple_emorating(:,1),'type','spearman') % n.s. (p = 0.56)
[r,p] = corr(pleacc,TL_ple_emorating(:,2),'type','spearman') % r = 0.37, p = 0.002
[r,p] = corr(pleacc,TL_ple_emorating(:,5),'type','spearman') % r = -0.49, p = 0.00001
% But two of them correlate with actual performance!!!!!

compositediff = TL_ple_emorating(:,1) + TL_ple_emorating(:,2) - TL_ple_emorating(:,5);
% p = 0.0005 vs. zero for composite diff
% More continuous variable to use?

% Actual Accuracy for Pleaders is highly correlated with emotion ratings
[r,p] = corr(pleacc,compositediff); % highly correlated, r = 0.62, p = 6.47e-9
[r,p] = corr(pleacc,compositediff,'type','spearman'); % highly correlated, r = 0.59, p = 6.1e-8

% No relationship between ratings & PPG T/L differences
[r,p] = corr(compositediff(keepPPG),TL_P_PPG_diff(keepPPG)) % no relationship (p = 0.78)
[r,p] = corr(compositediff(keepPPG),TL_P_PPG_diff(keepPPG),'type','spearman') % no relationship (p = 0.86)

% A relationship between ratings & SCL T/L differences
[r,p] = corr(compositediff(keepSCL),TL_P_SCL_diff(keepSCL)) % r = -0.26, p = 0.04
[r,p] = corr(compositediff(keepSCL),TL_P_SCL_diff(keepSCL),'type','spearman') % r = -0.33, p = 0.008



% So, emotion ratings from pleaders predict actual accuracy for pleaders.
% And emotion ratings correlate with skin conductance level (but SCL does
% not itself predict accuracy (p = 0.27)).


% Check mean emo ratings w/ interoception - higher values? 

% Focus on interoception --> Physio responses (optimality?) not accuracy
% per se



% No relationship to actual lie detection performance.
% Additional possibilities:
%   1) interoception/metacog correl w/ physio diffs
%   2) interoception/metacog correl w/ optimal accuracy (or error)
%   3) something more complex w/ intero/metacog interacting w/ physio to
%   predict accuracy?


% Main ILD measures:
% dprime(tatlind) or pcorr(tatlind) (NOTE: Not normal; log on pcorr does best, p = 0.03 lilliefors)
% countaccM(tatlind)
% fit_all.Mratio(tatlind)

% Main TATL measures:
% ovacc, pleacc, fofacc, BUT levels are low for all
% compositediff (combining the three uncorrelated emotion ratings that show a difference)
% TL_FOF_PPG_diff (e.g.; physio differences)
% indiv_fof_ppg_optimal (e.g.; optimal accuracy based on physio diffs)
% fofppg_error (e.g.; error btwn optimal and actual accuracy

mainILD = [dprime(tatlind) pcorr(tatlind) countaccM(tatlind) fit_all.Mratio(tatlind)'];
mainTATL = [ovacc pleacc fofacc...
    compositediff...
    TL_FOF_PPG_diff TL_P_PPG_diff TL_FOF_SCL_diff TL_P_SCL_diff ...
    indiv_fof_ppg_optimal indiv_ple_ppg_optimal indiv_fof_scl_optimal indiv_ple_scl_optimal ...
    fofppg_error pleppg_error fofscl_error plescl_error];

for a = 1:size(mainILD,2)
    disp(a)
    for b = 1:size(mainTATL,2)
        [r,p] = corr(mainILD(:,a),mainTATL(:,b),'rows','complete'); % 'type','spearman',
        if p < 0.1
            fprintf('a=%d b=%d p=%.3f\n',a,b,p)
        end
    end
end

% Using SPEARMAN non-parametric correlations...
% trend relationships between COUNT ACCURACY and...
%   ovacc, fofacc, TL_FOF_SCL_diff, indiv_fof_ppg_optimal, fofppg_error
% trend relationships between M-RATIO and...
%   indiv_ple_scl_optimal, plescl_error
% No relationships to dprime or percent correct. 

% Using PEARSON correlations... 
% dprime & TL_P_PPG_diff p = 0.01
% pcorrect & TL_P_PPG_diff p = 0.02
% countaccM & ...
%   indiv_fof_ppg_optimal, fofppg_error
% M-ratio & ...
%   indiv_ple_scl_optimal, plescl_error

% Is it possible there are relationships between ILD and TATL stuff *only
% for the people with good performance?*

indbtrChance = btrChance(tatlind) < 0.05; % the people who performed interoception better than chance
    % (perfectly equivalent to a median split on pcorr or dprime in full ILD space N = 80)
indcountaccMmed = countaccM(tatlind) > nanmedian(countaccM(tatlind));
indMratiomed = fit_all.Mratio(tatlind)' > median(fit_all.Mratio(tatlind));

for b = 1:size(mainTATL,2)
    [h,p] = ttest2(mainTATL(indbtrChance==1,b),mainTATL(indbtrChance==0,b));
    if p < 0.1
        fprintf('Tone task performance\n');
        fprintf('b=%d p=%.3f\n',b,p);
    end
    [h,p] = ttest2(mainTATL(indcountaccMmed==1,b),mainTATL(indcountaccMmed==0,b));
    if p < 0.1
        fprintf('Counting performance\n');
        fprintf('b=%d p=%.3f\n',b,p);
    end
    [h,p] = ttest2(mainTATL(indMratiomed==1,b),mainTATL(indMratiomed==0,b));
    if p < 0.1
        fprintf('Metacognitive performance\n');
        fprintf('b=%d p=%.3f\n',b,p);
    end
end
% Using median split on Metacognitive perf to do a 2-sample t-test ON:
%   compositediff, p = 0.04 (smaller compositediff for good M-Ratio people)
%   plescl_error, p = 0.07 (bigger error for good M-ratio people)

% Main TATL measures:
% ovacc, pleacc, fofacc, BUT levels are low for all
% compositediff (combining the three uncorrelated emotion ratings that show a difference)
% TL_FOF_PPG_diff (e.g.; physio differences)
% indiv_fof_ppg_optimal (e.g.; optimal accuracy based on physio diffs)
% fofppg_error (e.g.; error btwn optimal and actual accuracy

% Overall accuracy and the composite difference
[r,p] = corr(pleacc(indbtrChance),compositediff(indbtrChance)) % r = 0.63, p = 0.0007
[r,p] = corr(pleacc(indcountaccMmed),compositediff(indcountaccMmed)) % r = 0.45, p = 0.008
[r,p] = corr(pleacc(indMratiomed),compositediff(indMratiomed)) % r = 0.50, p = 0.002
% comparable to the overall finding, r = 0.62, p = 6.5e-9


% PLE TASK
[r,p] = corr(pleacc(indbtrChance&keepPPG),TL_P_PPG_diff(indbtrChance&keepPPG)) % n.s.
[r,p] = corr(pleacc(indcountaccMmed&keepPPG),TL_P_PPG_diff(indcountaccMmed&keepPPG)) % n.s.
[r,p] = corr(pleacc(indMratiomed&keepPPG),TL_P_PPG_diff(indMratiomed&keepPPG)) % n.s.
% No relationship between accuracy & PPG differences for the 'good'
% participants

[r,p] = corr(pleacc(indbtrChance&keepSCL),TL_P_SCL_diff(indbtrChance&keepSCL)) % n.s.
[r,p] = corr(pleacc(indcountaccMmed&keepSCL),TL_P_SCL_diff(indcountaccMmed&keepSCL)) % n.s.
[r,p] = corr(pleacc(indMratiomed&keepSCL),TL_P_SCL_diff(indMratiomed&keepSCL)) % n.s.
% No relationship between accuracy & SCL differences for the 'good'
% participants

[r,p] = corr(pleacc(indbtrChance&keepPPG),indiv_ple_ppg_optimal(indbtrChance&keepPPG)) % n.s.
[r,p] = corr(pleacc(indcountaccMmed&keepPPG),indiv_ple_ppg_optimal(indcountaccMmed&keepPPG)) % p = 0.095
[r,p] = corr(pleacc(indMratiomed&keepPPG),indiv_ple_ppg_optimal(indMratiomed&keepPPG)) % n.s.
% No relationship between accuracy & optimal possible accuracy based on PPG
% differences for the 'good' participants

[r,p] = corr(pleacc(indbtrChance&keepSCL),indiv_ple_scl_optimal(indbtrChance&keepSCL)) % n.s.
[r,p] = corr(pleacc(indcountaccMmed&keepSCL),indiv_ple_scl_optimal(indcountaccMmed&keepSCL)) % n.s.
[r,p] = corr(pleacc(indMratiomed&keepSCL),indiv_ple_scl_optimal(indMratiomed&keepSCL)) % n.s.
% No relationship between accuracy & optimal possible accuracy based on SCL
% differences for the 'good' participants




% FOF TASK
[r,p] = corr(fofacc(indbtrChance&keepPPG),TL_FOF_PPG_diff(indbtrChance&keepPPG)) % n.s.
[r,p] = corr(fofacc(indcountaccMmed&keepPPG),TL_FOF_PPG_diff(indcountaccMmed&keepPPG)) % n.s.
[r,p] = corr(fofacc(indMratiomed&keepPPG),TL_FOF_PPG_diff(indMratiomed&keepPPG)) % n.s.
% No relationship between accuracy & PPG differences for the 'good'
% participants

[r,p] = corr(fofacc(indbtrChance&keepSCL),TL_FOF_SCL_diff(indbtrChance&keepSCL)) % n.s.
[r,p] = corr(fofacc(indcountaccMmed&keepSCL),TL_FOF_SCL_diff(indcountaccMmed&keepSCL)) % n.s.
[r,p] = corr(fofacc(indMratiomed&keepSCL),TL_FOF_SCL_diff(indMratiomed&keepSCL)) % n.s.
% No relationship between accuracy & SCL differences for the 'good'
% participants

[r,p] = corr(fofacc(indbtrChance&keepPPG),indiv_fof_ppg_optimal(indbtrChance&keepPPG)) % n.s.
[r,p] = corr(fofacc(indcountaccMmed&keepPPG),indiv_fof_ppg_optimal(indcountaccMmed&keepPPG)) % n.s.
[r,p] = corr(fofacc(indMratiomed&keepPPG),indiv_fof_ppg_optimal(indMratiomed&keepPPG)) % n.s.
% No relationship between accuracy & optimal possible accuracy based on PPG
% differences for the 'good' participants

[r,p] = corr(fofacc(indbtrChance&keepSCL),indiv_fof_scl_optimal(indbtrChance&keepSCL)) % n.s.
[r,p] = corr(fofacc(indcountaccMmed&keepSCL),indiv_fof_scl_optimal(indcountaccMmed&keepSCL)) % n.s.
[r,p] = corr(fofacc(indMratiomed&keepSCL),indiv_fof_scl_optimal(indMratiomed&keepSCL)) % n.s.
% No relationship between accuracy & optimal possible accuracy based on SCL
% differences for the 'good' participants



% No relationship between ratings & PPG T/L differences
[r,p] = corr(compositediff(indbtrChance&keepPPG),TL_P_PPG_diff(indbtrChance&keepPPG)) % no relationship 
[r,p] = corr(compositediff(indcountaccMmed&keepPPG),TL_P_PPG_diff(indcountaccMmed&keepPPG)) % no relationship 
[r,p] = corr(compositediff(indMratiomed&keepPPG),TL_P_PPG_diff(indMratiomed&keepPPG)) % no relationship 

% No relationship between ratings & SCL T/L differences
[r,p] = corr(compositediff(indbtrChance&keepSCL),TL_P_SCL_diff(indbtrChance&keepSCL)) % n.s.
[r,p] = corr(compositediff(indcountaccMmed&keepSCL),TL_P_SCL_diff(indcountaccMmed&keepSCL)) % n.s.
[r,p] = corr(compositediff(indMratiomed&keepSCL),TL_P_SCL_diff(indMratiomed&keepSCL)) % n.s.




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