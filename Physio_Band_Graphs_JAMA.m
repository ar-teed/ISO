%Create HR, RVV, RVT and Dial graphs
%Make sure you're in the directory where you have this file saved (Stuff
%for Sahib's Lab/Stuff for RO1
%load('ALL_ISO_3_Physio_Dial_fix.mat')


%%
% I want the smaller set of AN/HC/MA that is better matched. So remove any
% of the subjects not using.
%HC_remove = [2,3,4,6,11,12,15,17,21,25,26,30,36,41,43,44,45,46,47,48,49,50];
%GAD_remove = [4,10,22,24];
%HC_remove_other = [5,6,7,8,19,20,21,22,27,28,35,36,49,50];
%GAD_remove_other = [7,8,19,20,43,44,47,48];

%HC_subs(HC_remove) = [];
%HC_Saline_Dial(:,HC_remove_other) = [];
%HC_pFive_Dial(:,HC_remove_other) = [];
%HC_Two_Dial(:,HC_remove_other) = [];
%HC_Saline_HR(:,HC_remove_other) = [];
%HC_pFive_HR(:,HC_remove_other) = [];
%HC_Two_HR(:,HC_remove_other) = [];


%GAD_subs(GAD_remove) = [];
%GAD_Saline_Dial(:,GAD_remove_other) = [];
%GAD_pFive_Dial(:,GAD_remove_other) = [];
%GAD_Two_Dial(:,GAD_remove_other) = [];
%GAD_Saline_HR(:,GAD_remove_other) = [];
%GAD_pFive_HR(:,GAD_remove_other) = [];
%GAD_Two_HR(:,GAD_remove_other) = [];

% Load the struct with all fields prepped
load('ALL_ISO_3_Physio_Dial_fix.mat')

times = 1:240;

nhc = length(HC_subs);
%nax = length(AN_subs)
nma = length(GAD_subs);

fs = 22 
lw = 2

%% Making graphs for HR, dial, and RVV modalities for each dose showing lines for each participant group
clf
HC_HR = mean(HC_Saline_HR,2);
GAD_HR = mean(GAD_Saline_HR,2);
%AN_HR = mean(AN_Saline_HR,2);

HC_HR_smooth = smooth(HC_HR);
GAD_HR_smooth = smooth(GAD_HR);
%AN_HR_smooth = smooth(AN_HR);

make_it_tight = true;
figure
subplot = @(m,n,p) subtightplot (m, n, p, [0.6 0.04], [0.11 0.038], [0.055 0.011]);
%subplot = @(m,n,p) subtightplot (m, n, p, [0.1 0.05], [0.02 0.013], [0.038 0.005]);
subplot(1,3,1)
shadedErrorBar(times,HC_HR,(std(transpose(HC_Saline_HR))/sqrt(nhc)),'lineprops',{'color', [0.4,0.4,1], 'LineWidth', 2})
hold on
box on
axis([0,240,60,110])
shadedErrorBar(times,GAD_HR,(std(transpose(GAD_Saline_HR))/sqrt(nma)),'lineprops',{'color', [0.2,0.6,0], 'LineWidth', 2})
%plot(times,GAD_HR,'r','LineWidth',2)
set(gca,'FontSize',fs)
title('Saline')
set(gca,'XTick',[0 45 60 80 120 180 240]);
xtickangle(45)
xTlab = get(gca,'XTickLabel');
set(gca,'FontSize',fs)
set(gca,'XTickLabel',xTlab,'FontName','Arial','fontsize',24)
set(gca,'XGrid','On')
T=xlabel('Time (s)')
set(T, 'FontSize', 28) 
ylabel('Heart Rate (bpm)', 'FontSize', 28)
%set(D, 'FontSize', 28) 
%legend({'HC', 'GAD'}, 'Location', 'NorthEast')

HC_HR = mean(HC_pFive_HR,2);
GAD_HR = mean(GAD_pFive_HR,2);
%AN_HR = mean(AN_pFive_HR,2);

HC_HR_smooth = smooth(HC_HR);
GAD_HR_smooth = smooth(GAD_HR);
%AN_HR_smooth = smooth(AN_HR);

subplot(1,3,2)
shadedErrorBar(times,HC_HR,(std(transpose(HC_pFive_HR))/sqrt(nhc)),'lineprops',{'color', [0.4,0.4,1], 'LineWidth', 2})
hold on
box on
axis([0,240,60,110])
shadedErrorBar(times,GAD_HR,(std(transpose(GAD_pFive_HR))/sqrt(nma)),'lineprops',{'color', [0.2,0.6,0], 'LineWidth', 2})
%plot(times,GAD_HR,'r','LineWidth',2)
set(gca,'FontSize',fs)
title('0.5 \mug')
set(gca,'XTick',[0 45 60 80 120 180 240]);
xtickangle(45)
xTlab = get(gca,'XTickLabel');
set(gca,'FontSize',fs)
set(gca,'XTickLabel',xTlab,'FontName','Arial','fontsize',24)
set(gca,'XGrid','On')
T=xlabel('Time (s)')
set(T, 'FontSize', 28) 
%ylabel('HR (bpm) - 0.5 \mug')
%set(D, 'FontSize', 28) 
legend({'HC', 'GAD'}, 'Location', 'North')


HC_HR = mean(HC_Two_HR,2);
GAD_HR = mean(GAD_Two_HR,2);
%AN_HR = mean(AN_Two_HR,2);

HC_HR_smooth = smooth(HC_HR);
GAD_HR_smooth = smooth(GAD_HR);
%AN_HR_smooth = smooth(AN_HR);

subplot(1,3,3)
shadedErrorBar(times,HC_HR,(std(transpose(HC_Two_HR))/sqrt(nhc)),'lineprops',{'color', [0.4,0.4,1], 'LineWidth', 2})
hold on
box on
axis([0,240,60,110])
shadedErrorBar(times,GAD_HR,(std(transpose(GAD_Two_HR))/sqrt(nma)),'lineprops',{'color', [0.2,0.6,0], 'LineWidth', 2})
%plot(times,GAD_HR,'r','LineWidth',2)
set(gca,'FontSize',fs)
title('2 \mug')
set(gca,'XTick',[0 45 60 80 120 180 240]);
xtickangle(45)
xTlab = get(gca,'XTickLabel');
set(gca,'FontSize',fs)
set(gca,'XTickLabel',xTlab,'FontName','Arial','fontsize',24)
set(gca,'XGrid','On')
T=xlabel('Time (s)')
set(T, 'FontSize', 28) 
%ylabel('HR (bpm) - 2 \mug')
%set(D, 'FontSize', 28)
%legend({'HC', 'GAD'}, 'Location', 'NorthEast')

hold off

print('HC_GAD_HR_unsmoothed_angle','-dpng','-r500');

%% Now all the dials one one for each dose

clf
HC_Dial = mean(HC_Saline_Dial,2);
GAD_Dial = mean(GAD_Saline_Dial,2);
%AN_Dial = mean(AN_Saline_Dial,2);

HC_Dial_smooth = smooth(HC_Dial);
GAD_Dial_smooth = smooth(GAD_Dial);
%AN_Dial_smooth = smooth(AN_Dial);

make_it_tight = true;
figure
subplot = @(m,n,p) subtightplot (m, n, p, [0.6 0.04], [0.11 0.038], [0.045 0.011]);
%subplot = @(m,n,p) subtightplot (m, n, p, [0.1 0.05], [0.02 0.013], [0.038 0.005]);
subplot(1,3,1)
shadedErrorBar(times,HC_Dial,(std(transpose(HC_Saline_Dial))/sqrt(nhc)),'lineprops',{'color', [0.4,0.4,1], 'LineWidth', 2})
hold on
box on
axis([0,240,0,70])
shadedErrorBar(times,GAD_Dial,(std(transpose(AN_Saline_Dial))/sqrt(nma)),'lineprops',{'color', [0.2,0.6,0], 'LineWidth', 2})
%plot(times,GAD_Dial,'r','LineWidth',2)
set(gca,'FontSize',fs)
title('Saline')
set(gca,'XTick',[0 45 60 80 120 180 240]);
xtickangle(45)
xTlab = get(gca,'XTickLabel');
set(gca,'FontSize',fs)
set(gca,'XTickLabel',xTlab,'FontName','Arial','fontsize',24)
set(gca,'XGrid','On')
T=xlabel('Time (s)')
set(T, 'FontSize', 28) 
ylabel('Dial Rating', 'FontSize', 28)
%set(D, 'FontSize', 28) 
%legend({'HC', 'GAD'}, 'Location', 'NorthEast')



HC_Dial = mean(HC_pFive_Dial,2);
GAD_Dial = mean(GAD_pFive_Dial,2);
%AN_Dial = mean(AN_pFive_Dial,2);

HC_Dial_smooth = smooth(HC_Dial);
GAD_Dial_smooth = smooth(GAD_Dial);
%AN_Dial_smooth = smooth(AN_Dial);

subplot(1,3,2)
shadedErrorBar(times,HC_Dial,(std(transpose(HC_pFive_Dial))/sqrt(nhc)),'lineprops',{'color', [0.4,0.4,1], 'LineWidth', 2})
hold on
box on
axis([0,240,0,70])
shadedErrorBar(times,GAD_Dial,(std(transpose(AN_pFive_Dial))/sqrt(nma)),'lineprops',{'color', [0.2,0.6,0], 'LineWidth', 2})
%plot(times,GAD_Dial,'r','LineWidth',2)
set(gca,'FontSize',fs)
title('0.5 \mug')
set(gca,'XTick',[0 45 60 80 120 180 240]);
xtickangle(45)
xTlab = get(gca,'XTickLabel');
set(gca,'FontSize',fs)
set(gca,'XTickLabel',xTlab,'FontName','Arial','fontsize',24)
set(gca,'XGrid','On')
T=xlabel('Time (s)')
set(T, 'FontSize', 28) 
%ylabel('Dial - 0.5 \mug')
%set(D, 'FontSize', 28) 
legend({'HC', 'GAD'}, 'Location', 'North')



HC_Dial = mean(HC_Two_Dial,2);
GAD_Dial = mean(GAD_Two_Dial,2);
%AN_Dial = mean(AN_Two_Dial,2);

HC_Dial_smooth = smooth(HC_Dial);
GAD_Dial_smooth = smooth(GAD_Dial);
%AN_Dial_smooth = smooth(AN_Dial);

subplot(1,3,3)
shadedErrorBar(times,HC_Dial,(std(transpose(HC_Two_Dial))/sqrt(nhc)),'lineprops',{'color', [0.4,0.4,1], 'LineWidth', 2})
hold on
box on
axis([0,240,0,70])
shadedErrorBar(times,GAD_Dial,(std(transpose(AN_Two_Dial))/sqrt(nma)),'lineprops',{'color', [0.2,0.6,0], 'LineWidth', 2})
%plot(times,GAD_Dial,'r','LineWidth',2)
set(gca,'FontSize',fs)
title('2 \mug')
set(gca,'XTick',[0 45 60 80 120 180 240]);
xtickangle(45)
xTlab = get(gca,'XTickLabel');
set(gca,'FontSize',fs);
set(gca,'XTickLabel',xTlab,'FontName','Arial','fontsize',24)
set(gca,'XGrid','On')
T=xlabel('Time (s)')
set(T, 'FontSize', 28) 
%label('Dial - 2 \mug')
%set(D, 'FontSize', 28) legend({'HC', 'GAD'}, 'Location', 'NorthEast')

hold off

print('HC_GAD_Dial_unsmoothed_angle','-dpng','-r500');


%% Lets make a version with all the RVV on one and all the dials one one for each dose
clf
HC_RVV = mean(HC_Saline_RVV,2);
GAD_RVV = mean(GAD_Saline_RVV,2);
%AN_RVV = mean(AN_Saline_RVV,2);

HC_RVV_smooth = smooth(HC_RVV);
GAD_RVV_smooth = smooth(GAD_RVV);
%AN_RVV_smooth = smooth(AN_RVV);

make_it_tight = true;
figure
subplot = @(m,n,p) subtightplot (m, n, p, [0.6 0.06], [0.1 0.01], [0.05 0.011]);
%subplot = @(m,n,p) subtightplot (m, n, p, [0.1 0.05], [0.02 0.013], [0.038 0.005]);
subplot(1,3,1)
shadedErrorBar(times,HC_RVV,(std(transpose(HC_Saline_RVV))/sqrt(nhc)),'lineprops',{'color', [0.4,0.4,1], 'LineWidth', 2})
hold on
axis([0,240,0,30])
shadedErrorBar(times,GAD_RVV,(std(transpose(AN_Saline_RVV))/sqrt(nax)),'lineprops',{'color', [0.2,0.6,0], 'LineWidth', 2})
%plot(times,GAD_RVV,'r','LineWidth',2)
set(gca,'FontSize',fs)
title('Saline')
set(gca,'XTick',[0 45 60 80 120 180 240]);
xtickangle(45)
xTlab = get(gca,'XTickLabel');
set(gca,'FontSize',fs)
set(gca,'XTickLabel',xTlab,'FontName','Arial','fontsize',22)
set(gca,'XGrid','On')
T=xlabel('Time (s)')
set(T, 'FontSize', 28) 
ylabel('Resp Volume Variability')
%set(D, 'FontSize', 28) legend({'HC', 'GAD'}, 'Location', 'NorthEast')
%legend({'HC', 'GAD'}, 'Location', 'NorthEast')


HC_RVV = mean(HC_pFive_RVV,2);
GAD_RVV = mean(GAD_pFive_RVV,2);
%AN_RVV = mean(AN_pFive_RVV,2);

HC_RVV_smooth = smooth(HC_RVV);
GAD_RVV_smooth = smooth(GAD_RVV);
%AN_RVV_smooth = smooth(AN_RVV);

subplot(1,3,2)
shadedErrorBar(times,HC_RVV,(std(transpose(HC_pFive_RVV))/sqrt(nhc)),'lineprops',{'color', [0.4,0.4,1], 'LineWidth', 2})
hold on
axis([0,240,0,30])
shadedErrorBar(times,GAD_RVV,(std(transpose(AN_pFive_RVV))/sqrt(nax)),'lineprops',{'color', [0.2,0.6,0], 'LineWidth', 2})
%plot(times,GAD_RVV,'r','LineWidth',2)
set(gca,'FontSize',fs)
title('0.5 \mug')
set(gca,'XTick',[0 45 60 80 120 180 240]);
xtickangle(45)
xTlab = get(gca,'XTickLabel');
set(gca,'FontSize',fs)
set(gca,'XTickLabel',xTlab,'FontName','Arial','fontsize',22)
set(gca,'XGrid','On')
T=xlabel('Time (s)')
set(T, 'FontSize', 28) 
%ylabel('RVV - 0.5 \mug')
%set(D, 'FontSize', 28) legend({'HC', 'GAD'}, 'Location', 'NorthEast')
legend({'HC', 'GAD'}, 'Location', 'North')


HC_RVV = mean(HC_Two_RVV,2);
GAD_RVV = mean(GAD_Two_RVV,2);
%AN_RVV = mean(AN_Two_RVV,2);

HC_RVV_smooth = smooth(HC_RVV);
GAD_RVV_smooth = smooth(GAD_RVV);
%AN_RVV_smooth = smooth(AN_RVV);

subplot(1,3,3)
shadedErrorBar(times,HC_RVV,(std(transpose(HC_Two_RVV))/sqrt(nhc)),'lineprops',{'color', [0.4,0.4,1], 'LineWidth', 2})
hold on
axis([0,240,0,30])
shadedErrorBar(times,GAD_RVV,(std(transpose(AN_Two_RVV))/sqrt(nax)),'lineprops',{'color', [0.2,0.6,0], 'LineWidth', 2})
%plot(times,GAD_RVV,'r','LineWidth',2)
set(gca,'FontSize',fs)
title('2 \mug')
set(gca,'XTick',[0 45 60 80 120 180 240]);
xtickangle(45)
xTlab = get(gca,'XTickLabel');
set(gca,'FontSize',fs)
set(gca,'XTickLabel',xTlab,'FontName','Arial','fontsize',22)
set(gca,'XGrid','On')
T=xlabel('Time (s)')
set(T, 'FontSize', 28) 
%ylabel('RVV - 2 \mug')
%set(D, 'FontSize', 28) legend({'HC', 'GAD'}, 'Location', 'NorthEast')
%legend({'HC', 'GAD'}, 'Location', 'NorthEast')

hold off
print('HC_GAD_RVV_unsmoothed_angle','-dpng','-r500');

