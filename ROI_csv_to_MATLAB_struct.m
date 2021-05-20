%%% Extract csv values, here from ROIs, and put them in a structure
%%% By Adam Teed 5/28/20

long_HC = readtable('HC_ISO_10.csv','Format','%s%s%s%s%f%f%f%f%f%f%f%f%f%f%f')
long_GAD = readtable('GAD_ISO_10.csv','Format','%s%s%s%s%f%f%f%f%f%f%f%f%f%f%f')

temp_GAD_pFive = (long_GAD.Dose=="pFive")
temp_GAD_Sal = (long_GAD.Dose=="Sal")
temp_GAD_Two = (long_GAD.Dose=="Two")
temp_HC_pFive = (long_HC.Dose=="pFive")
temp_HC_Sal = (long_HC.Dose=="Sal")
temp_HC_Two = (long_HC.Dose=="Two")

%Begin creating the struct and only work with fields going forward
ROItimeCourse.HC_ID = unique(long_HC(:,1))
ROItimeCourse.GAD_ID = unique(long_GAD(:,1))

for v = 1:11;
q=v+4;
%Create long form array fields in the struct from the subset tables
ROItimeCourse.GAD_pFive_raw(:,v) = table2array(long_GAD(temp_GAD_pFive,q));
ROItimeCourse.HC_pFive_raw(:,v) = table2array(long_HC(temp_HC_pFive,q));
ROItimeCourse.GAD_Sal_raw(:,v) = table2array(long_GAD(temp_GAD_Sal,q));
ROItimeCourse.HC_Sal_raw(:,v) = table2array(long_HC(temp_HC_Sal,q));
ROItimeCourse.GAD_Two_raw(:,v) = table2array(long_GAD(temp_GAD_Two,q));
ROItimeCourse.HC_Two_raw(:,v) = table2array(long_HC(temp_HC_Two,q));
end
%I removed the 1st, 'TR,' column from the _raw fields after deciding I wanted that in a separate field);

%Chop each raw field up so that rows are the timeseries and each 29 columns are the persons values
%for an ROI that you will later average, while these _sort fields will be needed for std calculation
ROItimeCourse.GAD_pFive_sort = reshape(ROItimeCourse.GAD_pFive_raw,116,[])
ROItimeCourse.HC_pFive_sort = reshape(ROItimeCourse.HC_pFive_raw,116,[])
ROItimeCourse.GAD_Sal_sort = reshape(ROItimeCourse.GAD_Sal_raw,116,[])
ROItimeCourse.HC_Sal_sort = reshape(ROItimeCourse.HC_Sal_raw,116,[])
ROItimeCourse.GAD_Two_sort = reshape(ROItimeCourse.GAD_Two_raw,116,[])
ROItimeCourse.HC_Two_sort = reshape(ROItimeCourse.HC_Two_raw,116,[])

%Specify the size of your mean fields
ROItimeCourse.GAD_pFive(1:116,1:10) = zeros()
ROItimeCourse.HC_pFive(1:116,1:10) = zeros()
ROItimeCourse.GAD_Sal(1:116,1:10) = zeros()
ROItimeCourse.HC_Sal(1:116,1:10) = zeros()
ROItimeCourse.GAD_Two(1:116,1:10) = zeros()
ROItimeCourse.HC_Two(1:116,1:10) = zeros()

    %Get means for each ROI
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



temp_pFive = (long_GAD.Dose=="pFive")

for t = 1:116

length(long_GAD(:))
first = (long_GAD.Dose=="pFive" & long_GAD.TR==t)

for r = 1:15
    G_pFive(:,r) = (long_GAD.Dose=="pFive")


pFive = pFive{:,:}
for r = 1:10
    GAD_pFive = 

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