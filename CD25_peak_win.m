% Get means for rolling 5 second windows along time-course to approximate
% time-course peak w/o too much sensitivity to signal spikes
% Inefficient code could use more loops

%First from resting state images for GADs then healthy controls
load('Rest_HR_29.mat')
%Read_Resting_HR_Data AX787 4
%AX787 = ans
%save("Rest_HR_29.mat")

for r = 1:length(MA_Rest_HR(1,:))
w1=0; w5=4;
for q = 1:(length(MA_Rest_HR)-4)
MA_R_HR_win(q,r)=mean(MA_Rest_HR(w1+q:w5+q,r));
end
MA_Rest_Win(r,1)=max(MA_R_HR_win(80:115,r));
end
for r = 1:length(HC_Rest_HR(1,:))
w1=0; w5=4;
for q = 1:(length(HC_Rest_HR)-4)
HC_R_HR_win(q,r)=mean(HC_Rest_HR(w1+q:w5+q,r));
end
HC_Rest_Win(r,1)=max(HC_R_HR_win(80:115,r));
end

%Next from infusion task by isoproterenol dose
clear all
load("29_HR_RVV_RVT_Dial_matrices_SOBP.mat")
d=0
Doses = ["Saline", "pFive", "Two"]
for s = 1:length(MA_subs)
d=d+1
MA_max(d).Subject = MA_subs(s)
d=d+1
MA_max(d).Subject = MA_subs(s)
d=d+1
MA_max(d).Subject = MA_subs(s)
end

d=0
for r = 1:length(MA_Saline_HR(1,:))
w1=0; w5=4;
for q = 1:(length(MA_Saline_HR)-4)
MA_S_HR_win(q,r)=mean(MA_Saline_HR(w1+q:w5+q,r));
MA_pF_HR_win(q,r)=mean(MA_pFive_HR(w1+q:w5+q,r));
MA_T_HR_win(q,r)=mean(MA_Two_HR(w1+q:w5+q,r));
end
MA_max_run(1,r)=max(MA_S_HR_win(80:115,r));
MA_max_run(2,r)=max(MA_pF_HR_win(80:115,r));
MA_max_run(3,r)=max(MA_T_HR_win(80:115,r));
end

for c = 1:length(MA_Saline_HR(1,:))
v=(c-1+c)
d=d+1
MA_max(d).Dose = Doses(1);
MA_max(d).Run1 = MA_max_run(1,v);
MA_max(d).Run2 = MA_max_run(1,v+1);
d=d+1
MA_max(d).Dose = Doses(2);
MA_max(d).Run1 = MA_max_run(2,v);
MA_max(d).Run2 = MA_max_run(2,v+1);
d=d+1
MA_max(d).Dose = Doses(3)
MA_max(d).Run1 = MA_max_run(3,v);
MA_max(d).Run2 = MA_max_run(3,v+1);
end
MA_max(88).Subject = []; MA_max(88).Dose = []; MA_max(88).Run1 = []; MA_max(88).Run2 = [];
MA_max(88) = [];
d=0
Doses = ["Saline", "pFive", "Two"]
for s = 1:length(HC_subs)
d=d+1
HC_max(d).Subject = HC_subs(s)
d=d+1
HC_max(d).Subject = HC_subs(s)
d=d+1
HC_max(d).Subject = HC_subs(s)
end
d=0
for r = 1:length(HC_Saline_HR(1,:))
w1=0; w5=4;
for q = 1:(length(HC_Saline_HR)-4)
HC_S_HR_win(q,r)=mean(HC_Saline_HR(w1+q:w5+q,r));
HC_pF_HR_win(q,r)=mean(HC_pFive_HR(w1+q:w5+q,r));
HC_T_HR_win(q,r)=mean(HC_Two_HR(w1+q:w5+q,r));
end
HC_max_run(1,r)=max(HC_S_HR_win(80:115,r));
HC_max_run(2,r)=max(HC_pF_HR_win(80:115,r));
HC_max_run(3,r)=max(HC_T_HR_win(80:115,r));
end
for c = 1:length(HC_Saline_HR(1,:))
v=(c-1+c)
d=d+1
HC_max(d).Dose = Doses(1);
HC_max(d).Run1 = HC_max_run(1,v);
HC_max(d).Run2 = HC_max_run(1,v+1);
d=d+1
HC_max(d).Dose = Doses(2);
HC_max(d).Run1 = HC_max_run(2,v);
HC_max(d).Run2 = HC_max_run(2,v+1);
d=d+1
HC_max(d).Dose = Doses(3)
HC_max(d).Run1 = HC_max_run(3,v);
HC_max(d).Run2 = HC_max_run(3,v+1);
end
HC_max(88) = [];
save("HC_Max.mat",'HC_max')
save("MA_Max.mat",'MA_max')
