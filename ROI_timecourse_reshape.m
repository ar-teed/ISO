clust_HC = readtable('Clust_HC.csv','Format','%s%s%s%s%s%f%f%f%f%f')
clust_GAD = readtable('Clust_GAD.csv','Format','%s%s%s%s%s%f%f%f%f%f')

ClustTimeCourse.HC_ID = unique(clust_HC(:,1))
ClustTimeCourse.GAD_ID = unique(clust_GAD(:,1))

temp_GAD_Saline = (clust_GAD.Dose=="Placebo")
temp_GAD_Point5 = (clust_GAD.Dose=="Point5")
temp_GAD_Two = (clust_GAD.Dose=="Two")
temp_HC_Saline = (clust_HC.Dose=="Placebo")
temp_HC_Point5 = (clust_HC.Dose=="Point5")
temp_HC_Two = (clust_HC.Dose=="Two")


for v = 1:2;
q=v+6;
ClustTimeCourse.GAD_Saline_raw(:,v) = table2array(clust_GAD(temp_GAD_Saline,q));
ClustTimeCourse.HC_Saline_raw(:,v) = table2array(clust_HC(temp_HC_Saline,q));
ClustTimeCourse.GAD_Point5_raw(:,v) = table2array(clust_GAD(temp_GAD_Point5,q));
ClustTimeCourse.HC_Point5_raw(:,v) = table2array(clust_HC(temp_HC_Point5,q));
ClustTimeCourse.GAD_Two_raw(:,v) = table2array(clust_GAD(temp_GAD_Two,q));
ClustTimeCourse.HC_Two_raw(:,v) = table2array(clust_HC(temp_HC_Two,q));
end

temp_GAD_Saline_raw = ClustTimeCourse.GAD_Saline_raw
temp_HC_Saline_raw = ClustTimeCourse.HC_Saline_raw
temp_GAD_Point5_raw = ClustTimeCourse.GAD_Point5_raw
temp_HC_Point5_raw = ClustTimeCourse.HC_Point5_raw
temp_GAD_Two_raw = ClustTimeCourse.GAD_Two_raw
temp_HC_Two_raw = ClustTimeCourse.HC_Two_raw

%ClustTimeCourse.HC_Point5_raw = removevars(ClustTimeCourse.HC_Point5_raw, 'TR'); <No longer needed
ClustTimeCourse.ROInames = {'Self'; 'Valence'; 'Autonomic'}

ClustTimeCourse.TR = clust_HC(1:116,6)

ClustTimeCourse.GAD_Saline_sort = reshape(temp_GAD_Saline_raw,116,[])
ClustTimeCourse.HC_Saline_sort = reshape(temp_HC_Saline_raw,116,[])
ClustTimeCourse.GAD_Point5_sort = reshape(temp_GAD_Point5_raw,116,[])
ClustTimeCourse.HC_Point5_sort = reshape(temp_HC_Point5_raw,116,[])
ClustTimeCourse.GAD_Two_sort = reshape(temp_GAD_Two_raw,116,[])
ClustTimeCourse.HC_Two_sort = reshape(temp_HC_Two_raw,116,[])

ClustTimeCourse.GAD_Saline(1:116,1:3) = zeros()
ClustTimeCourse.HC_Saline(1:116,1:3) = zeros()
ClustTimeCourse.GAD_Point5(1:116,1:3) = zeros()
ClustTimeCourse.HC_Point5(1:116,1:3) = zeros()
ClustTimeCourse.GAD_Two(1:116,1:3) = zeros()
ClustTimeCourse.HC_Two(1:116,1:3) = zeros()

    for t = 1:116;

        ClustTimeCourse.GAD_Saline(t,1) = mean(ClustTimeCourse.GAD_Saline_sort(t,1:29));
        ClustTimeCourse.GAD_Saline(t,2) = mean(ClustTimeCourse.GAD_Saline_sort(t,30:58));
        ClustTimeCourse.GAD_Saline(t,3) = mean(ClustTimeCourse.GAD_Saline_sort(t,59:87));
        ClustTimeCourse.HC_Saline(t,1) = mean(ClustTimeCourse.HC_Saline_sort(t,1:29));
        ClustTimeCourse.HC_Saline(t,2) = mean(ClustTimeCourse.HC_Saline_sort(t,30:58));
        ClustTimeCourse.HC_Saline(t,3) = mean(ClustTimeCourse.HC_Saline_sort(t,59:87));
        ClustTimeCourse.GAD_Point5(t,1) = mean(ClustTimeCourse.GAD_Point5_sort(t,1:29));
        ClustTimeCourse.GAD_Point5(t,2) = mean(ClustTimeCourse.GAD_Point5_sort(t,30:58));
        ClustTimeCourse.GAD_Point5(t,3) = mean(ClustTimeCourse.GAD_Point5_sort(t,59:87));
		ClustTimeCourse.HC_Point5(t,1) = mean(ClustTimeCourse.HC_Point5_sort(t,1:29));
        ClustTimeCourse.HC_Point5(t,2) = mean(ClustTimeCourse.HC_Point5_sort(t,30:58));
        ClustTimeCourse.HC_Point5(t,3) = mean(ClustTimeCourse.HC_Point5_sort(t,59:87));
        ClustTimeCourse.GAD_Two(t,1) = mean(ClustTimeCourse.GAD_Two_sort(t,1:29));
        ClustTimeCourse.GAD_Two(t,2) = mean(ClustTimeCourse.GAD_Two_sort(t,30:58));
        ClustTimeCourse.GAD_Two(t,3) = mean(ClustTimeCourse.GAD_Two_sort(t,59:87));
        ClustTimeCourse.HC_Two(t,1) = mean(ClustTimeCourse.HC_Two_sort(t,1:29));
        ClustTimeCourse.HC_Two(t,2) = mean(ClustTimeCourse.HC_Two_sort(t,30:58));
        ClustTimeCourse.HC_Two(t,3) = mean(ClustTimeCourse.HC_Two_sort(t,59:87));
    end

save('vmpfcROIcourse.mat','ClustTimeCourse')







long_HC = readtable('vmPFC_ROI_graph_HC.csv','Format','%s%s%s%s%s%f%f%f%f')
long_GAD = readtable('vmPFC_ROI_graph_GAD.csv','Format','%s%s%s%s%s%f%f%f%f')

ROItimeCourse.HC_ID = unique(long_HC(:,1))
ROItimeCourse.GAD_ID = unique(long_GAD(:,1))

temp_GAD_Saline = (long_GAD.Dose=="Placebo")
temp_GAD_Point5 = (long_GAD.Dose=="Point5")
temp_GAD_Two = (long_GAD.Dose=="Two")
temp_HC_Saline = (long_HC.Dose=="Placebo")
temp_HC_Point5 = (long_HC.Dose=="Point5")
temp_HC_Two = (long_HC.Dose=="Two")


for v = 1:3;
q=v+6;
ROItimeCourse.GAD_Saline_raw(:,v) = table2array(long_GAD(temp_GAD_Saline,q));
ROItimeCourse.HC_Saline_raw(:,v) = table2array(long_HC(temp_HC_Saline,q));
ROItimeCourse.GAD_Point5_raw(:,v) = table2array(long_GAD(temp_GAD_Point5,q));
ROItimeCourse.HC_Point5_raw(:,v) = table2array(long_HC(temp_HC_Point5,q));
ROItimeCourse.GAD_Two_raw(:,v) = table2array(long_GAD(temp_GAD_Two,q));
ROItimeCourse.HC_Two_raw(:,v) = table2array(long_HC(temp_HC_Two,q));
end

temp_GAD_Saline_raw = ROItimeCourse.GAD_Saline_raw
temp_HC_Saline_raw = ROItimeCourse.HC_Saline_raw
temp_GAD_Point5_raw = ROItimeCourse.GAD_Point5_raw
temp_HC_Point5_raw = ROItimeCourse.HC_Point5_raw
temp_GAD_Two_raw = ROItimeCourse.GAD_Two_raw
temp_HC_Two_raw = ROItimeCourse.HC_Two_raw

%ROItimeCourse.HC_Point5_raw = removevars(ROItimeCourse.HC_Point5_raw, 'TR'); <No longer needed
ROItimeCourse.ROInames = {'Self'; 'Valence'; 'Autonomic'}

ROItimeCourse.TR = long_HC(1:116,6)

ROItimeCourse.GAD_Saline_sort = reshape(temp_GAD_Saline_raw,116,[])
ROItimeCourse.HC_Saline_sort = reshape(temp_HC_Saline_raw,116,[])
ROItimeCourse.GAD_Point5_sort = reshape(temp_GAD_Point5_raw,116,[])
ROItimeCourse.HC_Point5_sort = reshape(temp_HC_Point5_raw,116,[])
ROItimeCourse.GAD_Two_sort = reshape(temp_GAD_Two_raw,116,[])
ROItimeCourse.HC_Two_sort = reshape(temp_HC_Two_raw,116,[])

ROItimeCourse.GAD_Saline(1:116,1:3) = zeros()
ROItimeCourse.HC_Saline(1:116,1:3) = zeros()
ROItimeCourse.GAD_Point5(1:116,1:3) = zeros()
ROItimeCourse.HC_Point5(1:116,1:3) = zeros()
ROItimeCourse.GAD_Two(1:116,1:3) = zeros()
ROItimeCourse.HC_Two(1:116,1:3) = zeros()

    for t = 1:116;

        ROItimeCourse.GAD_Saline(t,1) = mean(ROItimeCourse.GAD_Saline_sort(t,1:29));
        ROItimeCourse.GAD_Saline(t,2) = mean(ROItimeCourse.GAD_Saline_sort(t,30:58));
        ROItimeCourse.GAD_Saline(t,3) = mean(ROItimeCourse.GAD_Saline_sort(t,59:87));
        ROItimeCourse.HC_Saline(t,1) = mean(ROItimeCourse.HC_Saline_sort(t,1:29));
        ROItimeCourse.HC_Saline(t,2) = mean(ROItimeCourse.HC_Saline_sort(t,30:58));
        ROItimeCourse.HC_Saline(t,3) = mean(ROItimeCourse.HC_Saline_sort(t,59:87));
        ROItimeCourse.GAD_Point5(t,1) = mean(ROItimeCourse.GAD_Point5_sort(t,1:29));
        ROItimeCourse.GAD_Point5(t,2) = mean(ROItimeCourse.GAD_Point5_sort(t,30:58));
        ROItimeCourse.GAD_Point5(t,3) = mean(ROItimeCourse.GAD_Point5_sort(t,59:87));
		ROItimeCourse.HC_Point5(t,1) = mean(ROItimeCourse.HC_Point5_sort(t,1:29));
        ROItimeCourse.HC_Point5(t,2) = mean(ROItimeCourse.HC_Point5_sort(t,30:58));
        ROItimeCourse.HC_Point5(t,3) = mean(ROItimeCourse.HC_Point5_sort(t,59:87));
        ROItimeCourse.GAD_Two(t,1) = mean(ROItimeCourse.GAD_Two_sort(t,1:29));
        ROItimeCourse.GAD_Two(t,2) = mean(ROItimeCourse.GAD_Two_sort(t,30:58));
        ROItimeCourse.GAD_Two(t,3) = mean(ROItimeCourse.GAD_Two_sort(t,59:87));
        ROItimeCourse.HC_Two(t,1) = mean(ROItimeCourse.HC_Two_sort(t,1:29));
        ROItimeCourse.HC_Two(t,2) = mean(ROItimeCourse.HC_Two_sort(t,30:58));
        ROItimeCourse.HC_Two(t,3) = mean(ROItimeCourse.HC_Two_sort(t,59:87));
    end

save('vmpfcROIcourse.mat','ROItimeCourse')





vmPFC = readtable('vmPFC_HR_dial_fpc_projection.csv','Format','%s%s%s%s%s%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f%f')
tempGAD = (vmPFC.Group=="GAD")
tempHC = (vmPFC.Group=="HC")

temp_GAD_pFive = (vmPFC.Group=="GAD" & vmPFC.Dose=="Point5")
temp_GAD_Sal = (vmPFC.Group=="GAD" & vmPFC.Dose=="Placebo")
temp_GAD_Two = (vmPFC.Group=="GAD" & vmPFC.Dose=="Two")
temp_HC_pFive = (vmPFC.Group=="HC" & vmPFC.Dose=="Point5")
temp_HC_Sal = (vmPFC.Group=="HC" & vmPFC.Dose=="Placebo")
temp_HC_Two = (vmPFC.Group=="HC" & vmPFC.Dose=="Two")

FPC.ID = unique(vmPFC(:,1))
%FPC.GAD_ID = unique(tempGAD(:,1))
FPC.TR = 5:120

for v = 1:4;
q=v+22;
FPC.GAD_pFive_raw(:,v) = table2array(vmPFC(temp_GAD_pFive,q));
FPC.HC_pFive_raw(:,v) = table2array(vmPFC(temp_HC_pFive,q));
FPC.GAD_Sal_raw(:,v) = table2array(vmPFC(temp_GAD_Sal,q));
FPC.HC_Sal_raw(:,v) = table2array(vmPFC(temp_HC_Sal,q));
FPC.GAD_Two_raw(:,v) = table2array(vmPFC(temp_GAD_Two,q));
FPC.HC_Two_raw(:,v) = table2array(vmPFC(temp_HC_Two,q));
end
FPC.Vars = {'vmPFC'; 'vmPFC1_project'; 'vmPFC2_project'; 'vmPFC3_project'}

temp_GAD_pFive_raw = FPC.GAD_pFive_raw
temp_HC_pFive_raw = FPC.HC_pFive_raw
temp_GAD_Sal_raw = FPC.GAD_Sal_raw
temp_HC_Sal_raw = FPC.HC_Sal_raw
temp_GAD_Two_raw = FPC.GAD_Two_raw
temp_HC_Two_raw = FPC.HC_Two_raw

FPC.GAD_pFive_sort = reshape(temp_GAD_pFive_raw,116,[])
FPC.HC_pFive_sort = reshape(temp_HC_pFive_raw,116,[])
FPC.GAD_Sal_sort = reshape(temp_GAD_Sal_raw,116,[])
FPC.HC_Sal_sort = reshape(temp_HC_Sal_raw,116,[])
FPC.GAD_Two_sort = reshape(temp_GAD_Two_raw,116,[])
FPC.HC_Two_sort = reshape(temp_HC_Two_raw,116,[])

FPC.GAD_pFive(1:116,1:4) = zeros()
FPC.HC_pFive(1:116,1:4) = zeros()
FPC.GAD_Sal(1:116,1:4) = zeros()
FPC.HC_Sal(1:116,1:4) = zeros()
FPC.GAD_Two(1:116,1:4) = zeros()
FPC.HC_Two(1:116,1:4) = zeros()

    for t = 1:116;

        FPC.GAD_Sal(t,1) = mean(FPC.GAD_Sal_sort(t,1:29));
        FPC.GAD_Sal(t,2) = mean(FPC.GAD_Sal_sort(t,30:58));
        FPC.GAD_Sal(t,3) = mean(FPC.GAD_Sal_sort(t,59:87));
        FPC.GAD_Sal(t,4) = mean(FPC.GAD_Sal_sort(t,88:116));
        FPC.HC_Sal(t,1) = mean(FPC.HC_Sal_sort(t,1:29));
        FPC.HC_Sal(t,2) = mean(FPC.HC_Sal_sort(t,30:58));
        FPC.HC_Sal(t,3) = mean(FPC.HC_Sal_sort(t,59:87));
        FPC.HC_Sal(t,4) = mean(FPC.HC_Sal_sort(t,88:116));
        FPC.GAD_pFive(t,1) = mean(FPC.GAD_pFive_sort(t,1:29));
        FPC.GAD_pFive(t,2) = mean(FPC.GAD_pFive_sort(t,30:58));
        FPC.GAD_pFive(t,3) = mean(FPC.GAD_pFive_sort(t,59:87));
        FPC.GAD_pFive(t,4) = mean(FPC.GAD_pFive_sort(t,88:116));
        FPC.HC_pFive(t,1) = mean(FPC.HC_pFive_sort(t,1:29));
        FPC.HC_pFive(t,2) = mean(FPC.HC_pFive_sort(t,30:58));
        FPC.HC_pFive(t,3) = mean(FPC.HC_pFive_sort(t,59:87));
        FPC.HC_pFive(t,4) = mean(FPC.HC_pFive_sort(t,88:116));
        FPC.GAD_Two(t,1) = mean(FPC.GAD_Two_sort(t,1:29));
        FPC.GAD_Two(t,2) = mean(FPC.GAD_Two_sort(t,30:58));
        FPC.GAD_Two(t,3) = mean(FPC.GAD_Two_sort(t,59:87));
        FPC.GAD_Two(t,4) = mean(FPC.GAD_Two_sort(t,88:116));
		FPC.HC_Two(t,1) = mean(FPC.HC_Two_sort(t,1:29));
        FPC.HC_Two(t,2) = mean(FPC.HC_Two_sort(t,30:58));
        FPC.HC_Two(t,3) = mean(FPC.HC_Two_sort(t,59:87));
        FPC.HC_Two(t,4) = mean(FPC.HC_Two_sort(t,88:116));
    end

save('FPC.mat','FPC')



long_HC = readtable('HC_ISO_10.csv','Format','%s%s%s%s%f%f%f%f%f%f%f%f%f%f%f')
long_GAD = readtable('GAD_ISO_10.csv','Format','%s%s%s%s%f%f%f%f%f%f%f%f%f%f%f')

ROItimeCourse.HC_ID = unique(long_HC(:,1))
ROItimeCourse.GAD_ID = unique(long_GAD(:,1))

temp_GAD_pFive = (long_GAD.Dose=="pFive")
temp_GAD_Sal = (long_GAD.Dose=="Sal")
temp_GAD_Two = (long_GAD.Dose=="Two")
temp_HC_pFive = (long_HC.Dose=="pFive")
temp_HC_Sal = (long_HC.Dose=="Sal")
temp_HC_Two = (long_HC.Dose=="Two")

for v = 1:10;
q=v+5;
ROItimeCourse.GAD_pFive_raw(:,v) = table2array(long_GAD(temp_GAD_pFive,q));
ROItimeCourse.HC_pFive_raw(:,v) = table2array(long_HC(temp_HC_pFive,q));
ROItimeCourse.GAD_Sal_raw(:,v) = table2array(long_GAD(temp_GAD_Sal,q));
ROItimeCourse.HC_Sal_raw(:,v) = table2array(long_HC(temp_HC_Sal,q));
ROItimeCourse.GAD_Two_raw(:,v) = table2array(long_GAD(temp_GAD_Two,q));
ROItimeCourse.HC_Two_raw(:,v) = table2array(long_HC(temp_HC_Two,q));
end

ROItimeCourse.GAD_pFive_raw(:,v) = table2array(long_GAD(temp_GAD_pFive,q));
ROItimeCourse.HC_pFive_raw(:,v) = table2array(long_HC(temp_HC_pFive,q));
ROItimeCourse.GAD_Sal_raw(:,v) = table2array(long_GAD(temp_GAD_Sal,q));
ROItimeCourse.HC_Sal_raw(:,v) = table2array(long_HC(temp_HC_Sal,q));
ROItimeCourse.GAD_Two_raw(:,v) = table2array(long_GAD(temp_GAD_Two,q));
ROItimeCourse.HC_Two_raw(:,v) = table2array(long_HC(temp_HC_Two,q));

temp_GAD_pFive_raw = ROItimeCourse.GAD_pFive_raw
temp_HC_pFive_raw = ROItimeCourse.HC_pFive_raw
temp_GAD_Sal_raw = ROItimeCourse.GAD_Sal_raw
temp_HC_Sal_raw = ROItimeCourse.HC_Sal_raw
temp_GAD_Two_raw = ROItimeCourse.GAD_Two_raw
temp_HC_Two_raw = ROItimeCourse.HC_Two_raw

%ROItimeCourse.HC_pFive_raw = removevars(ROItimeCourse.HC_pFive_raw, 'TR'); <No longer needed
ROItimeCourse.ROInames = {'vmPFC_ACC'; 'dmPFC'; 'PCC_Precuneus'; 'L_Anterior_insula'; 'R_Anterior_insula'; 'L_Posterior_insula'; 'R_Posterior_insula'; 'dACC'; 'Amygdala'; 'Caudate'}

ROItimeCourse.TR = ROItimeCourse.HC_Sal_raw(1:116,1)

ROItimeCourse.GAD_pFive_sort = reshape(temp_GAD_pFive_raw,116,[])
ROItimeCourse.HC_pFive_sort = reshape(temp_HC_pFive_raw,116,[])
ROItimeCourse.GAD_Sal_sort = reshape(temp_GAD_Sal_raw,116,[])
ROItimeCourse.HC_Sal_sort = reshape(temp_HC_Sal_raw,116,[])
ROItimeCourse.GAD_Two_sort = reshape(temp_GAD_Two_raw,116,[])
ROItimeCourse.HC_Two_sort = reshape(temp_HC_Two_raw,116,[])

ROItimeCourse.GAD_pFive(1:116,1:10) = zeros()
ROItimeCourse.HC_pFive(1:116,1:10) = zeros()
ROItimeCourse.GAD_Sal(1:116,1:10) = zeros()
ROItimeCourse.HC_Sal(1:116,1:10) = zeros()
ROItimeCourse.GAD_Two(1:116,1:10) = zeros()
ROItimeCourse.HC_Two(1:116,1:10) = zeros()

    for t = 1:116;

        ROItimeCourse.GAD_Sal(t,1) = mean(ROItimeCourse.GAD_Sal_sort(t,1:29));
        ROItimeCourse.GAD_Sal(t,2) = mean(ROItimeCourse.GAD_Sal_sort(t,30:58));
        ROItimeCourse.GAD_Sal(t,3) = mean(ROItimeCourse.GAD_Sal_sort(t,59:87));
        ROItimeCourse.GAD_Sal(t,4) = mean(ROItimeCourse.GAD_Sal_sort(t,88:116));
        ROItimeCourse.GAD_Sal(t,5) = mean(ROItimeCourse.GAD_Sal_sort(t,117:145));
        ROItimeCourse.GAD_Sal(t,6) = mean(ROItimeCourse.GAD_Sal_sort(t,146:174));
        ROItimeCourse.GAD_Sal(t,7) = mean(ROItimeCourse.GAD_Sal_sort(t,175:203));
        ROItimeCourse.GAD_Sal(t,8) = mean(ROItimeCourse.GAD_Sal_sort(t,204:232));
        ROItimeCourse.GAD_Sal(t,9) = mean(ROItimeCourse.GAD_Sal_sort(t,233:261));
        ROItimeCourse.GAD_Sal(t,10) = mean(ROItimeCourse.GAD_Sal_sort(t,272:290));
        ROItimeCourse.HC_Sal(t,1) = mean(ROItimeCourse.HC_Sal_sort(t,1:29));
        ROItimeCourse.HC_Sal(t,2) = mean(ROItimeCourse.HC_Sal_sort(t,30:58));
        ROItimeCourse.HC_Sal(t,3) = mean(ROItimeCourse.HC_Sal_sort(t,59:87));
        ROItimeCourse.HC_Sal(t,4) = mean(ROItimeCourse.HC_Sal_sort(t,88:116));
        ROItimeCourse.HC_Sal(t,5) = mean(ROItimeCourse.HC_Sal_sort(t,117:145));
        ROItimeCourse.HC_Sal(t,6) = mean(ROItimeCourse.HC_Sal_sort(t,146:174));
        ROItimeCourse.HC_Sal(t,7) = mean(ROItimeCourse.HC_Sal_sort(t,175:203));
        ROItimeCourse.HC_Sal(t,8) = mean(ROItimeCourse.HC_Sal_sort(t,204:232));
        ROItimeCourse.HC_Sal(t,9) = mean(ROItimeCourse.HC_Sal_sort(t,233:261));
        ROItimeCourse.HC_Sal(t,10) = mean(ROItimeCourse.HC_Sal_sort(t,272:290));
        ROItimeCourse.GAD_pFive(t,1) = mean(ROItimeCourse.GAD_pFive_sort(t,1:29));
        ROItimeCourse.GAD_pFive(t,2) = mean(ROItimeCourse.GAD_pFive_sort(t,30:58));
        ROItimeCourse.GAD_pFive(t,3) = mean(ROItimeCourse.GAD_pFive_sort(t,59:87));
        ROItimeCourse.GAD_pFive(t,4) = mean(ROItimeCourse.GAD_pFive_sort(t,88:116));
        ROItimeCourse.GAD_pFive(t,5) = mean(ROItimeCourse.GAD_pFive_sort(t,117:145));
        ROItimeCourse.GAD_pFive(t,6) = mean(ROItimeCourse.GAD_pFive_sort(t,146:174));
        ROItimeCourse.GAD_pFive(t,7) = mean(ROItimeCourse.GAD_pFive_sort(t,175:203));
        ROItimeCourse.GAD_pFive(t,8) = mean(ROItimeCourse.GAD_pFive_sort(t,204:232));
        ROItimeCourse.GAD_pFive(t,9) = mean(ROItimeCourse.GAD_pFive_sort(t,233:261));
        ROItimeCourse.GAD_pFive(t,10) = mean(ROItimeCourse.GAD_pFive_sort(t,272:290));
        ROItimeCourse.HC_pFive(t,1) = mean(ROItimeCourse.HC_pFive_sort(t,1:29));
        ROItimeCourse.HC_pFive(t,2) = mean(ROItimeCourse.HC_pFive_sort(t,30:58));
        ROItimeCourse.HC_pFive(t,3) = mean(ROItimeCourse.HC_pFive_sort(t,59:87));
        ROItimeCourse.HC_pFive(t,4) = mean(ROItimeCourse.HC_pFive_sort(t,88:116));
        ROItimeCourse.HC_pFive(t,5) = mean(ROItimeCourse.HC_pFive_sort(t,117:145));
        ROItimeCourse.HC_pFive(t,6) = mean(ROItimeCourse.HC_pFive_sort(t,146:174));
        ROItimeCourse.HC_pFive(t,7) = mean(ROItimeCourse.HC_pFive_sort(t,175:203));
        ROItimeCourse.HC_pFive(t,8) = mean(ROItimeCourse.HC_pFive_sort(t,204:232));
        ROItimeCourse.HC_pFive(t,9) = mean(ROItimeCourse.HC_pFive_sort(t,233:261));
        ROItimeCourse.HC_pFive(t,10) = mean(ROItimeCourse.HC_pFive_sort(t,272:290));
        ROItimeCourse.GAD_Two(t,1) = mean(ROItimeCourse.GAD_Two_sort(t,1:29));
        ROItimeCourse.GAD_Two(t,2) = mean(ROItimeCourse.GAD_Two_sort(t,30:58));
        ROItimeCourse.GAD_Two(t,3) = mean(ROItimeCourse.GAD_Two_sort(t,59:87));
        ROItimeCourse.GAD_Two(t,4) = mean(ROItimeCourse.GAD_Two_sort(t,88:116));
        ROItimeCourse.GAD_Two(t,5) = mean(ROItimeCourse.GAD_Two_sort(t,117:145));
        ROItimeCourse.GAD_Two(t,6) = mean(ROItimeCourse.GAD_Two_sort(t,146:174));
        ROItimeCourse.GAD_Two(t,7) = mean(ROItimeCourse.GAD_Two_sort(t,175:203));
        ROItimeCourse.GAD_Two(t,8) = mean(ROItimeCourse.GAD_Two_sort(t,204:232));
        ROItimeCourse.GAD_Two(t,9) = mean(ROItimeCourse.GAD_Two_sort(t,233:261));
        ROItimeCourse.GAD_Two(t,10) = mean(ROItimeCourse.GAD_Two_sort(t,272:290));
        ROItimeCourse.HC_Two(t,1) = mean(ROItimeCourse.HC_Two_sort(t,1:29));
        ROItimeCourse.HC_Two(t,2) = mean(ROItimeCourse.HC_Two_sort(t,30:58));
        ROItimeCourse.HC_Two(t,3) = mean(ROItimeCourse.HC_Two_sort(t,59:87));
        ROItimeCourse.HC_Two(t,4) = mean(ROItimeCourse.HC_Two_sort(t,88:116));
        ROItimeCourse.HC_Two(t,5) = mean(ROItimeCourse.HC_Two_sort(t,117:145));
        ROItimeCourse.HC_Two(t,6) = mean(ROItimeCourse.HC_Two_sort(t,146:174));
        ROItimeCourse.HC_Two(t,7) = mean(ROItimeCourse.HC_Two_sort(t,175:203));
        ROItimeCourse.HC_Two(t,8) = mean(ROItimeCourse.HC_Two_sort(t,204:232));
        ROItimeCourse.HC_Two(t,9) = mean(ROItimeCourse.HC_Two_sort(t,233:261));
        ROItimeCourse.HC_Two(t,10) = mean(ROItimeCourse.HC_Two_sort(t,272:290));
    end

save('ROItimeCourse.mat','ROItimeCourse')




%Scrap past this point

temp_pFive = (long_GAD.Dose=="pFive")

for t = 1:116

length(long_GAD(:))
first = (long_GAD.Dose=="pFive" & long_GAD.TR==t)

for r = 1:15
    G_pFive(:,r) = (long_GAD.Dose=="pFive")


pFive = pFive{:,:}
for r = 1:10
    GAD_pFive = 
end
end
end
temp_HC_pFive = (long_HC.Dose=="pFive")




for v = 1:11;
q=v+4;
ROItimeCourse.GAD_pFive_raw(:,v) = table2array(long_GAD(temp_GAD_pFive,q));
ROItimeCourse.HC_pFive_raw(:,v) = table2array(long_HC(temp_HC_pFive,q));
end
%ROItimeCourse.HC_pFive_raw = removevars(ROItimeCourse.HC_pFive_raw, 'TR');

ROItimeCourse.GAD_pFive_raw = ROItimeCourse.GAD_pFive_raw{:,:}
ROItimeCourse.HC_pFive_raw = ROItimeCourse.HC_pFive_raw{:,:}
ROItimeCourse.GAD_Sal_raw = ROItimeCourse.GAD_Sal_raw{:,:}
ROItimeCourse.HC_Sal_raw = ROItimeCourse.HC_Sal_raw{:,:}
ROItimeCourse.GAD_Two_raw = ROItimeCourse.GAD_Two_raw{:,:}
ROItimeCourse.HC_Two_raw = ROItimeCourse.HC_Two_raw{:,:}



ROItimeCourse.GAD_pFive(1:116,1:11) = zeros()
ROItimeCourse.HC_pFive(1:116,1:11) = zeros()
ROItimeCourse.GAD_pFive(1:116,1) = 5:120
ROItimeCourse.HC_pFive(1:116,1) = 5:120

    for t = 1:116;
        ROItimeCourse.GAD_pFive(t,1) = mean(ROItimeCourse.GAD_pFive_sort(t,1:29));
        ROItimeCourse.GAD_pFive(t,2) = mean(ROItimeCourse.GAD_pFive_sort(t,30:58));
        ROItimeCourse.GAD_pFive(t,3) = mean(ROItimeCourse.GAD_pFive_sort(t,59:87));
        ROItimeCourse.GAD_pFive(t,4) = mean(ROItimeCourse.GAD_pFive_sort(t,88:116));
        ROItimeCourse.GAD_pFive(t,5) = mean(ROItimeCourse.GAD_pFive_sort(t,117:145));
        ROItimeCourse.GAD_pFive(t,6) = mean(ROItimeCourse.GAD_pFive_sort(t,146:174));
        ROItimeCourse.GAD_pFive(t,7) = mean(ROItimeCourse.GAD_pFive_sort(t,175:203));
        ROItimeCourse.GAD_pFive(t,8) = mean(ROItimeCourse.GAD_pFive_sort(t,204:232));
        ROItimeCourse.GAD_pFive(t,9) = mean(ROItimeCourse.GAD_pFive_sort(t,233:261));
        ROItimeCourse.GAD_pFive(t,10) = mean(ROItimeCourse.GAD_pFive_sort(t,272:290));
        ROItimeCourse.HC_pFive(t,1) = mean(ROItimeCourse.HC_pFive_sort(t,1:29));
        ROItimeCourse.HC_pFive(t,2) = mean(ROItimeCourse.HC_pFive_sort(t,30:58));
        ROItimeCourse.HC_pFive(t,3) = mean(ROItimeCourse.HC_pFive_sort(t,59:87));
        ROItimeCourse.HC_pFive(t,4) = mean(ROItimeCourse.HC_pFive_sort(t,88:116));
        ROItimeCourse.HC_pFive(t,5) = mean(ROItimeCourse.HC_pFive_sort(t,117:145));
        ROItimeCourse.HC_pFive(t,6) = mean(ROItimeCourse.HC_pFive_sort(t,146:174));
        ROItimeCourse.HC_pFive(t,7) = mean(ROItimeCourse.HC_pFive_sort(t,175:203));
        ROItimeCourse.HC_pFive(t,8) = mean(ROItimeCourse.HC_pFive_sort(t,204:232));
        ROItimeCourse.HC_pFive(t,9) = mean(ROItimeCourse.HC_pFive_sort(t,233:261));
        ROItimeCourse.HC_pFive(t,10) = mean(ROItimeCourse.HC_pFive_sort(t,272:290));
    end

ROItimeCourse.HC_pFive_raw = removevars(ROItimeCourse.HC_pFive_raw, 'TR');

ROItimeCourse.GAD_pFive(1:116,1) = 5:120
ROItimeCourse.GAD_pFive_raw = ROItimeCourse.GAD_pFive_raw{:,:}

ROItimeCourse.GAD_pFive_sort = reshape(ROItimeCourse.GAD_pFive_raw,116,[])

for r = 1:length(ROItimeCourse.GAD_pFive_raw(1,:))
    c = r+1
    ROItimeCourse.GAD_pFive_sort = reshape(ROItimeCourse.GAD_pFive_raw,116,[])
    ROItimeCourse(1).GAD_pFive(c) = mean(ROItimeCourse.GAD_pFive_sort(r,1:29))

vmPFC(5) is pFive_sort(1,1:29)

ROItimeCourse.GAD_pFive(1:116,1:11) = zeros()
ROItimeCourse.GAD_pFive(1:116,1) = 5:120

    for t = 116;
        ROItimeCourse.GAD_pFive(t,2) = mean(ROItimeCourse.GAD_pFive_sort(t,1:29));
        ROItimeCourse.GAD_pFive(t,3) = mean(ROItimeCourse.GAD_pFive_sort(t,30:58));
        ROItimeCourse.GAD_pFive(t,4) = mean(ROItimeCourse.GAD_pFive_sort(t,59:87));
        ROItimeCourse.GAD_pFive(t,5) = mean(ROItimeCourse.GAD_pFive_sort(t,88:116));
        ROItimeCourse.GAD_pFive(t,6) = mean(ROItimeCourse.GAD_pFive_sort(t,117:145));
        ROItimeCourse.GAD_pFive(t,7) = mean(ROItimeCourse.GAD_pFive_sort(t,146:174));
        ROItimeCourse.GAD_pFive(t,8) = mean(ROItimeCourse.GAD_pFive_sort(t,175:203));
        ROItimeCourse.GAD_pFive(t,9) = mean(ROItimeCourse.GAD_pFive_sort(t,204:232));
        ROItimeCourse.GAD_pFive(t,10) = mean(ROItimeCourse.GAD_pFive_sort(t,233:261));
        ROItimeCourse.GAD_pFive(t,11) = mean(ROItimeCourse.GAD_pFive_sort(t,272:290));
    end
ROItimeCourse.GAD_pFive{1:29}(1:116) = zeros(29,116)

for r = 1:length(ROItimeCourse.GAD_pFive_raw(1,:))
    c = r+1
    for t = 116
        ROItimeCourse(t).GAD_pFive(c) = mean(ROItimeCourse.GAD_pFive_sort(t,1:29))