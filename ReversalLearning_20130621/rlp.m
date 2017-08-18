% function ReversalLearning_punish(subjectID,run)
% TR =2; task duration= 600s; % 300TR
% response key counterbalance across subject;
% for subject with odd subjectID: left=L, right=R;
% for subject with even subjectID: left=R; right=L;
% By FX 30th Nov. 2011


function reversallearning_punish(subjectID,run)
if nargin~=2
    eval(sprintf('error(''please run me as: %s(subjectID,run)'');',mfilename));
end
    % Do dummy calls to GetSecs, WaitSecs, KbCheck to make sure
    % they are loaded and ready when we need them - without delays
    % in the wrong moment:
    KbCheck;
    WaitSecs(0.1);
    GetSecs;
    
AssertOpenGL
% clear all
% subjectID=1;
% run=2;
warning('off')
mkdir('.','results');

% keys={'f','j'};
keys={'2' '3'};
TR=2;
NScan=361;
stim_dur=1;
feedback_dur=0.7;
fixation='+';
% ISIP=0;
% ISI=1;

% set image display dimensions (abstract images are 128x128; can change dispaly to 256x256 for bigger but low res image)
stim_image_size=[256 256];

% type of feedback to give
%fb_test='info';
fb_test='money';
fb_format='points'; % or money
total_money=0;
total_shock=0;
% define fb_type, isi and  stim_fb_interval if not defined in "infilename" stim presentation properties file.
% assume monetary/points feedback
% note that fb_type only needs to be defined if "fb_type" is set to
% "mixed" (mixed info and money/points feedback) so this is a little
% redundant here.
fb_type=1; % 1 is money, 0 is info
% stim_fb_interval=0

%%%%%%%%%%%%%%%%%%%%%%%%
% define feedback type and outcome strings presented to subject%
%%%%%%%%%%%%%%%%%%%%%%%%
        
if strcmp(fb_format,'money'),
    gain=.50;
    loss=0;
    outcome_text={'RAIN' 'SUNSHINE' ; ...
        [sprintf('-$%0.2f',loss)] [sprintf('+$%0.2f',gain)]};    
elseif strcmp(fb_format,'points'),
    gain=1;
    loss=0;
    outcome_text={'RAIN' 'SUNSHINE' ; ...
        [sprintf('%d points',loss)] [sprintf('%d points',gain)]};
end

% rand('state',sum(100*clock));  % seed the random number generator

load('rl_seq');
% load('stim_rl');
% jitterdata=jitter(jitter(:,2)>0 & jitter(:,4)==run,:);
% jittertmp=jitter(jitter(:,2)==0 & jitter(:,4)==run,:);
% % for fMRI task there will be 2s before and after each session.
% lasttrial_dur=jittertmp(end,3)+2;
% TTN=size(jitterdata,1);
eval(sprintf('TTN=size(run%d,1);',run));

% genseq: to generate sequence for this exp. genseq(subjd,exptype);
% in reward exp, exptype=1; in punish exp, exptype=2;
% seqoutfile=sprintf('stimlist/%s_sub%02d',mfilename,subjectID);
% if run==1
%     myseq=genseq(subjectID,2,run);
%     if mod(subjectID,2)==0
%         myseq(:,1)=myseq(:,1)+24;
%         myseq_run1=myseq(1:size(myseq,1)/2,:);
%         myseq_run2=myseq(size(myseq,1)/2+1:end,:);
% %         myseq_run2(:,1)=myseq_run2(:,1)+24;
%     else
%         myseq_run1=myseq(1:size(myseq,1)/2,:);
%         myseq_run2=myseq(size(myseq,1)/2+1:end,:);
% %         myseq_run1(:,1)=myseq(:,1)+24;
%     end
%     eval(sprintf('save %s myseq_run1 myseq_run2',seqoutfile));
% else
%     eval(sprintf('load %s',seqoutfile));
% end
%%%% generate sequence
MID=1; %
Mtrial=2; %
Mcrres=3; % Correct Response
Mcond=4; % 1: reversal; 2: control
Mres=5; % left or right key. counterbalance across subjects;
MRT=6; % reaction time;
Mscore=7; % 1: correct; -1: wrong; 0: no response
Monset=8; % designed trial onset time
MAonset=9; % actually trial onset time
Misrev=10; % is this trial a reversal one?
Mrunid=11; % is this trial a reversal one?
Mblockid=12; % is this trial a reversal one?
RL(:,MID)=[1:TTN]';
eval(sprintf('RL(:,Mtrial)=run%d(:,1)+50;',run));
eval(sprintf('RL(:,Mcond)=run%d(:,2);',run));
eval(sprintf('RL(:,Misrev)=run%d(:,3);',run));
eval(sprintf('RL(:,Mcrres)=run%d(:,4);',run));
eval(sprintf('RL(:,Monset)=run%d(:,5);',run));
RL(:,Mrunid)=ones(size(RL,1),1)*run;
eval(sprintf('RL(:,Mblockid)=run%d(:,6);',run));

% RL(:,Mcrres)=myseq(:,4);
% RL(:,Mcond)=myseq(:,2);
% RL(:,Misrev)=myseq(:,3);
% for fMRI task there will be 2s before and after each session.
% RL(:,Monset)=jitterdata(:,1)+2;
%%%%%%%%%%%%%%%%%%preparing the Screen
% size and position based on Screen resolution
sca;
s=max(Screen('Screens'));
black=BlackIndex(s); white=WhiteIndex(s); gray=GrayIndex(s); bkGround=black; % mean luminance
[w, rect]=Screen('OpenWindow', s, bkGround,[], 32);
HideCursor;
Priority(MaxPriority(w));
textSize=round(48/1024*rect(3)/2)*2;
theFont='Times New Roman';
% Screen(w,'FillRect',bkGround);
Screen(w,'TextFont',theFont);
xcenter=rect(3)/2;
ycenter=rect(4)/2;
% Screen(w,'TextSize',textSize);

% find the dimensions for the image stimuli relative to the Screen's center
image_dims=[xcenter-(stim_image_size(1)/2) ycenter-(stim_image_size(2)/2) xcenter+(stim_image_size(1)/2) ycenter+(stim_image_size(2)/2)];

% stims=unique(stim_nums(stim_nums>0)); % don't exchange the 0s in the stim list (baseline trials)
% 
maxstimid=max(RL(:,Mtrial));
% nstims=length(stims);

% load in images for weather stims (scr cell for task and base_scr for baseline_
imageData=cell(1,maxstimid);



for x=1:maxstimid,
    fname=sprintf('images/abs%d.jpg',x);
	imageData(x)={imread(fname,'jpg')};
end;


%%% create feedback graphical frames (around stimulus)
graphic = ones(stim_image_size(1)+20,stim_image_size(1)+20,3)*255;
red_frame=graphic;
red_frame(:,:,2)=0;
red_frame(:,:,3)=0;
blue_frame=graphic;
blue_frame(:,:,1)=0;
blue_frame(:,:,2)=0;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print instructions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Screen('TextSize',w,28);

inst={{['RLP Task--' 'Run(' num2str(run) ')']}...
      {''} ...
      {''}...
      {'Correct answer for each image'}...
      {'may change from time to time.'}...
      {' '}...
      {''} ...
      {'Your goal is to AVOID SHOCK.'}...
      {''} ...
      {'Please make your response FAST'}...
      {''} ...
      {'Please keep your head very STILL in the scanner'};
};

for x=1:size(inst,2),
    Screen('DrawText',w,inst{x}{:},150,100+x*28,white);
end;

Screen('Flip',w);
    
c=clock;
d=date;
outfile=sprintf('results/%s_sub%02d_run%d_%s_%02.0f-%02.0f',mfilename,subjectID,run,d(1:6),c(4),c(5));
Screen(w,'TextSize',textSize*2);

WaitTrigger;
startSecs=GetSecs;
Screen('Flip', w);
try
    for trial=1:TTN
        
        WaitTill(startSecs + RL(trial,Monset));
        
%        Screen(w,'FillRect', black);
%        Screen('Flip', w);
        Screen(w,'PutImage',imageData{RL(trial,Mtrial)},image_dims);
        Screen('Flip', w);
        
        RL(trial,MAonset)=GetSecs-startSecs;
        [key,timeSec]=WaitTill(startSecs+RL(trial,MAonset)+stim_dur,keys,1);
%         Screen('Flip', w);
        if ~isempty(key)
            RL(trial,Mres)=find(cell2mat(keys)==key);
            RL(trial,MRT)=timeSec-(startSecs+RL(trial,MAonset));
            if mod(subjectID,2)==1
                RL(trial,Mscore)=1-abs(RL(trial,Mcrres)-find(cell2mat(keys)==key));
            else
                RL(trial,Mscore)=abs(RL(trial,Mcrres)-find(cell2mat(keys)==key));
            end
            if RL(trial,Mscore)==0
                RL(trial,Mscore)=-1;
                shockonce;
                total_shock=total_shock+1;
            end
%         else
%             % To capture slow response
%             if trial~=TTN
%                 [key,timeSec]=WaitTill(startSecs+RL(trial+1,Monset),keys,1);
%             else
%                 [key,timeSec]=WaitTill(startSecs+RL(trial,MAonset)+stim_dur+lasttrial_dur,keys,1);
%             end
%             if ~isempty(key)
%                 RL(trial,Mres)=find(cell2mat(keys)==key);
%                 RL(trial,MRT)=timeSec-startSecs-RL(trial,MAonset);
%                 if mod(subjectID,2)==1
%                     RL(trial,Mscore)=1-abs(RL(trial,Mcrres)-find(cell2mat(keys)==key));
%                 else
%                     RL(trial,Mscore)=abs(RL(trial,Mcrres)-find(cell2mat(keys)==key));
%                 end
%                 if RL(trial,Mscore)==0, then RL(trial,Mscore)=-1, end;
%             end
        else
            shockonce;
            total_shock=total_shock+1;
        end
% % %                 feedback
            if strcmp(fb_test,'mixed'), % i.e. showing both monetary and info feedback
                fb=fb_type(trial_index);
            elseif strcmp(fb_test,'info'),
                fb=0;	% use info feedback only
            elseif strcmp(fb_test,'money'),
                fb=1;	% use monetary feedback only
            end
            fontsize=36;
            Screen('TextSize',w,fontsize);
            if RL(trial,Mscore)==0
                outcome_string='No response';
                bounds=Screen('TextBounds',w,outcome_string);
                TextWidth=bounds(3);
                Screen('DrawText',w, outcome_string,xcenter-(TextWidth/2), ycenter-180, white);
            else
                fdbk_frame=[];
                if RL(trial,Mscore)==1
                    % "correct" -> gain
                    outcome_string=outcome_text{fb+1,2};
                    total_money=total_money+gain;
                    fdbk_frame=blue_frame;
                else
                    % "incorrect" -> loss
                    outcome_string=outcome_text{fb+1,1};
                    total_money=total_money-loss;
                    fdbk_frame=red_frame;
                end
                tex = Screen('MakeTexture',w,fdbk_frame);
                Screen('DrawTexture',w,tex);
            end
            % display outcome/feedback string
            Screen(w,'PutImage',imageData{RL(trial,Mtrial)},image_dims);
            % display instruction above stim
%             bounds=Screen('TextBounds',w,outcome_string);
%             TextWidth=bounds(3);
%             Screen('DrawText',w, outcome_string,xcenter-(TextWidth/2), ycenter-200, white);
            % display total money/points accumulated so far
%             if strcmp(fb_format,'points'),
%                 totalmoney_string=sprintf('total: %d',total_money);
%             elseif strcmp(fb_format,'money'),
%                 totalmoney_string=sprintf('total: $%0.2f',total_money);
%             end
%             bounds=Screen('TextBounds',w,totalmoney_string);
%             TextWidth=bounds(3);
%             Screen('DrawText',w, totalmoney_string,xcenter-(TextWidth/2), ycenter+170, white);
            Screen('Flip', w);
            WaitSecs(feedback_dur);
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % END FEEDBACK PRESENTATION
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%             Screen('FillRect', w, black);
            Screen('Flip', w);
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % put up fixation during ISI
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            fontsize=36;
            Screen('TextSize',w,fontsize);
            bounds=Screen('TextBounds',w,fixation);
            TextWidth=bounds(3);
            TextHeight=bounds(4);
            Screen(w,'DrawText',fixation,xcenter-(TextWidth/2),ycenter-(TextHeight/2), white);
            Screen('Flip', w);
%             WaitSecs(isi(trial_index));
        %%% save results
    end % end for trial
        eval(sprintf('save %s RL',outfile));
        WaitTill(startSecs + NScan*TR);
%         WaitSecs(lasttrial_dur);
        sca;
        Priority(0);
%         disp([totalmoney_string ' points']);
        disp(['total shocks: ' num2str(total_shock)]);
%         sprintf('Accuracy:  %d%%',round(mean(RL(:,Mscore))*100))
catch
    outfile=sprintf('results/tmp_%s_sub%02d_run%d_%s_%02.0f-%02.0f',mfilename,subjectID,run,d(1:6),c(4),c(5));
    eval(sprintf('save %s RL',outfile));
    sca;
    Priority(0);
%     disp([totalmoney_string ' points']);
    disp(['total shocks: ' num2str(total_shock)]);
    lasterr;
end