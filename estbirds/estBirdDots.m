% Surrogate estimate for phylogenetic analysis
clear all

% File definitions
morphologyDataFilename='birds_for_surrogates_v3';
startRowData=1;
endRowData=141;
startColData=2;
endColData=4;
surrogateFilename='panelwing_CLatVzmin_surrogate_Fxn';

% Loads file with bird morphology parameters. 
disp('Loading morphology data...')
disp(' ')
birdDots=csvread([morphologyDataFilename,'.csv'],startRowData,startColData,...
	[startRowData startColData endRowData endColData]);

AR = birdDots(:,1); % Wing chordwise camber
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




