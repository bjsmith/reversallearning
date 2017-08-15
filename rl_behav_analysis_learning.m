load('/Users/benjaminsmith/Google Drive/joint-modeling/reversal-learning/behavioral-analysis/RL_behav/rlp_sub103_run1_09-Mar_11-37.mat')

%MID=1; %
%Mtrial=2; %
% Mcrres=3; % Correct Response
% Mcond=4; % 1: reversal; 2: control
% Mres=5; % left or right key. counterbalance across subjects;
% MRT=6; % reaction time;
% Mscore=7; % 1: correct; -1: wrong; 0: no response
% Monset=8; % designed trial onset time
% MAonset=9; % actually trial onset time
% Misrev=10; % is this trial a reversal one?
% Mrunid=11; % is this trial a reversal one?
% Mblockid=12; % is this trial a reversal one?

%trials
unique(RL(:,2))
length(unique(RL(:,2)))
%22 trials for this subject. Are 'trials' what we want?
length(RL(:,2))
%out of a total of 218 records - which is average of 10 to 11
%trials per record for this subject.
%that's about right.

