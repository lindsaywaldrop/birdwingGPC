% Surrogate estimate for phylogenetic analysis
% 
% To reproduce the results for the phylogenetic analysis (estimates of performance based 
% on the surrogate functions), simply change the surrogateFilename (line 19) to one of these options:
%   panelwing_Vzmin_surrogate_Fxn       ::  estimates minimum sinking speed Vz,min
%   panelwing_clcd_surrogate_Fxn        ::  estimates maximum lift-to-drag ratio CL/CD
%   panelwing_CLatVzmin_surrogate_Fxn   ::  estimates lift coefficient at minimum sinking speed
%   panelwing_CLatmaxCLCD_surrogate_Fxn ::  estimates lift coefficient at max CL/CD (not used in paper)
% Then run the script!

clear all

% File definitions
morphologyDataFilename='birds_for_surrogates_v4';
startRowData=1;
endRowData=163;
startColData=2;
endColData=4;
surrogateFilename='panelwing_Vzmin_surrogate_Fxn``';

% Loads file with bird morphology parameters. 
disp('Loading morphology data...')
disp(' ')
birdDots=csvread([morphologyDataFilename,'.csv'],startRowData,startColData,...
	[startRowData startColData endRowData endColData]);

AR = birdDots(:,1); % Wing chordwise camber
AR(30)=12;
Camber = birdDots(:,2);		% Aspect ratio
Re = birdDots(:,3);		% Reynolds number
disp('Done!')
disp(' ')

% Loads surrogate to be estimated
disp('Loading surrogate function...')
disp(' ')
load([surrogateFilename,'.mat']);
disp('Done!')
disp(' ')


% Use surrogate function to estimate output performance values.
disp('Estimating values based on surrogates...')
disp(' ')
[newDots]=Fxn(AR,Camber,Re);

disp('Done!')
disp(' ')

% Save results
disp('Saving results...')
disp(' ')

Tresults=table(newDots,'VariableNames',{'OutEstimate'});
writetable(Tresults,['results_',surrogateFilename,'.csv'],'WriteVariableNames',true);
disp('Done!')
disp(' ')




