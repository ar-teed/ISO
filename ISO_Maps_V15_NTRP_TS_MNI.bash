#!/bin/bash

#  ISO_testing_ContrastMaps.bash
#
#
#  Created by Maria Puhl on 4/5/18.
#  Modified by Adam Teed on 11/11/2019.
###  To correctly skip scans with too much motion, output and echo quality control metrics and statements
#
#  Modified by Adam Teed on 8/25/20.
###  To keep time-series images labelled by dose and convert to MNI space
#

#This generates z-scores for the participants by dose and epoch, all using the same baseline time period
function EndMNI_Contrast_Maps_15 {
cd ../
subj=$1


#First rename everything
origdir=original_subject_data_MP/ISO_Scans/${subj}/scan*
###Infodir looks to be unused by the script, which is good because the script that downloads and names the actual epi scans doesn't always line up with the record here
infodir=$origdir/scan_record_files
epidir=$origdir/EPI_files
procdir=processed_data_2/ISO_Scans_V15_NTRP/${subj}

# verify that the results directory does not yet exist and if it does, quit the script
if [ -d "$procdir/Mean_Files" ]; then
	echo "Subject file for $subj already exists."
	return
fi


files=($epidir/epi_*ISO*.BRIK)
nfiles=${#files[@]}

#One of the first things you need to do is go through the ss_review script and pull out the necessary information like TSNR, etc.
#Note that this version uses interpolation for signal outliers, so no volumes were actually censored to print counts of 
cd $procdir
tsnrline=($(grep "TSNR average" out.ss_review.${subj}.txt))
tsnr=${tsnrline[3]}
outlimitline=($(grep "num TRs above out limit" out.ss_review.${subj}.txt))
outlimit=${outlimitline[6]}
motionlimitline=($(grep "num TRs above mot limit" out.ss_review.${subj}.txt))
motionlimit=${motionlimitline[6]}
avcenmotline=($(grep "average censored motion" out.ss_review.${subj}.txt))
avcenmot=${avcenmotline[4]}
maxcendisline=($(grep "max censored displacement" out.ss_review.${subj}.txt))
maxcendis=${maxcendisline[4]}
###trperrunline records the number of censored TRs in each run and could be referred to for dropping a run with too much motion
trperrunline=($(grep "num TRs per run (censored)" out.ss_review.${subj}.txt))
trperrun=${trperrunline[@]5:5}
declare -a newinfo=($subj $tsnr $outlimit $motionlimit $avcenmot $maxcendis ${trperrun[@]})
cd ../
echo ${newinfo[@]} >> MotionSummary.txt

cd ../../

####################################
###Skip this whole loop if debugging
#First do the renaming.
for i in $(seq 1 $nfiles)
	do
	#Need to get ISO_R${i} -
	currentfile=($epidir/epi_*_ISO_R${i}*.BRIK)
	#This is probably not the most effective way to do this, but I need the number scan it is, and type.
	hold1=${currentfile#*epi_}
	ScanNumber=${hold1%+orig*}
	
	cd $procdir
	#Now that I have the information I need, pull out the appropriate piece of the errts file, and rename it. The subbrick selection won't take variables, so I'm going to have to split it up with if statments.
	if [ $i -eq 1 ]; then
		3dTcat -prefix epi_${ScanNumber}_errts -tr 2 errts.${subj}_REML+orig'[0..115]'
	fi
	
	if [ $i -eq 2 ]; then
		3dTcat -prefix epi_${ScanNumber}_errts -tr 2 errts.${subj}_REML+orig'[116..231]'
	fi
	
	if [ $i -eq 3 ]; then
		3dTcat -prefix epi_${ScanNumber}_errts -tr 2 errts.${subj}_REML+orig'[232..347]'
	fi

	if [ $i -eq 4 ]; then
		3dTcat -prefix epi_${ScanNumber}_errts -tr 2 errts.${subj}_REML+orig'[348..463]'
	fi

	if [ $i -eq 5 ]; then
		3dTcat -prefix epi_${ScanNumber}_errts -tr 2 errts.${subj}_REML+orig'[464..579]'
	fi

	if [ $i -eq 6 ]; then
		3dTcat -prefix epi_${ScanNumber}_errts -tr 2 errts.${subj}_REML+orig'[580..695]'
	fi

	cd ../../../
done
####################################

#Now need to do the reorganizing.
#First need to go to procdir and get the outlier list.
#We DO HAVE an outlier file for these so no need for a placeholder
#declare -a outlierlist=( $(for i in {1..696}; do echo 1; done) )
cd $procdir

readarray outlierlist < censor_${subj}_combined_2.1D

mkdir -p ReviewFiles
mkdir -p ExtraFiles

mv X.* ReviewFiles/
#mv mat.* ReviewFiles/
#mv out* ReviewFiles/
#mv corr_brain* ReviewFiles/
mv errts* ExtraFiles/
mv all_runs* ExtraFiles/
#mv mask* ExtraFiles/
mv stats* ExtraFiles/
#mv full_mask* ExtraFiles/
#mv vr_* ExtraFiles/

#rm mprage*
mv *.1D ExtraFiles/
#mv final_epi* ExtraFiles/
mv fitts.* ExtraFiles/
mv *.err ReviewFiles/
#mv @* ReviewFiles/

cd ../../../
###############
#Now do all of the contrast map stuff
mkdir -p $procdir/Mean_Files
contdir=$procdir/Mean_Files

#These are the types of scans we have:
scantype=(0 .5 2)
scantypename=(Sal pFive Two)

#Loop through each type of scan and create the contrast maps.
for i in $(seq 1 3)
do
	nscan=()
	scanarray=()
	firstone=()
	secondone=()
	curind=$(($i-1))
	curtype=${scantype[$curind]}
	#Figure out which scans of theirs have this type, by calling the "DetermineScanType" function
	scanarray=($(DetermineScanType $subj $curtype))
	
	#Now check the length of scanarray
	nscan=${#scanarray[@]}
	if [ "$nscan" -eq 0 ]; then
		echo "There are no scans of type $curtype for subject $subj".
		continue
	fi

	#Need to check the outlier files for these scans, and determine if we are going to skip them.
	skip=()
	val=0
	cd $procdir
	for k in $(seq 0 $(($nscan-1)))
	do
		tempscan=${scanarray[$k]}
		if [ $tempscan -eq 1 ]; then
			tempvector=${outlierlist[@]:0:116}
		elif [ $tempscan -eq 2 ]; then
			tempvector=${outlierlist[@]:116:116}
		elif [ $tempscan -eq 3 ]; then
			tempvector=${outlierlist[@]:232:116}
		elif [ $tempscan -eq 4 ]; then
			tempvector=${outlierlist[@]:348:116}
		elif [ $tempscan -eq 5 ]; then
			tempvector=${outlierlist[@]:464:116}
		elif [ $tempscan -eq 6 ]; then
			tempvector=${outlierlist[@]:580:116}
		fi
		#Split it into an array
		IFS=' ' read -r -a temparray <<< "$tempvector"
		#Define 
		goodTR=$(awk 'BEGIN {t=0; for (i in ARGV) t+=ARGV[i]; print t}' "${temparray[@]}" )
		ngood=${goodTR/.*}
		#Calculate # of outlying image volumes and then mark those above drop threshold
		nbad=$((116-$ngood))
		if [ "$nbad" -gt 23 ]; then
			echo "Motion outlier count for run$tempscan is $nbad, which is > than 20% cutoff"
			drop=$tempscan
			skip[$val]=$(($k+1))
			val=$(($val+1))
		fi
	done

	cd ../../../

	nskip=${#skip[@]}
	#Make sure we aren't going to skip one of these scans! If we are, then we need to remove it.
	if [ "$nskip" -ne 0 ]; then
	    echo "$tempscan $nskip $curtype"
	    scanarraynew=( ${scanarray[@]} )
	    for k in $(seq 0 $(($nskip-1)))
	    do
	        for j in $(seq 0 $(($nscan-1)))
	        do
	            #Likely the msot elegant way to drop for motion would be to get "${skip[$k]}" -eq "${scanarray[$j]}" to work.
                #if [ "${skip[$k]}" -eq "${scanarray[$j]}" ];
	            if [ "$drop" == "${scanarray[$j]}" ];
	                then
	                unset scanarraynew[$j]
	            fi
	        done
	    done
	    scanarray=( "${scanarraynew[@]}" )
	    nscan=${#scanarray[@]}
	fi
	
	echo "For dose $curtype, nscan = $nscan."
	echo "ISO runs ${scanarray[@]}"
	
	if [ "$nscan" -eq 0 ]; then
		echo "There are no scans of type $curtype for subject $subj, after having removed the scans with too much motion."
		continue
	fi
	
	if [ "$nskip" -eq 1 ]; then
		echo "One scan of type $curtype was skipped for subject $subj due to too much motion."
	fi

	cd $procdir
	###This will kick in if a run is missing or if one is to be skipped for motion
	if [ "$nscan" -eq 1 ]; then
		firstone=($(find epi_*_R${scanarray[0]}_errts+orig.HEAD))
		echo "Only scan $firstone kept for type $curtype due to a missing 2nd scan or after dropping the 2nd scan due to motion."
		
		#Copy the original errts timeseries labeled by dosage
		3dcopy -verb $firstone epi_${subj}_${scantypename[$curind]}_TS

		#Now pull out the volumes for each period and make a mean image
		3dTstat -prefix BaseAverage1_Delete -mean $firstone'[0..17]'
		3dTstat -prefix PrepAverage1_Delete -mean $firstone'[18..24]'
		3dTstat -prefix AntAverage1_Delete -mean $firstone'[25..34]'
		3dTstat -prefix PeakAverage1_Delete -mean $firstone'[35..54]'
		3dTstat -prefix RecAverage1_Delete -mean $firstone'[55..115]'
		3dTstat -prefix Rec1Average1_Delete -mean $firstone'[55..84]'
		3dTstat -prefix Rec2Average1_Delete -mean $firstone'[85..115]'

		#Now get % signal change from baseline.
		3dcalc -prefix PrepMBase_Delete_Final -a PrepAverage1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		3dcalc -prefix AntMBase_Delete_Final -a AntAverage1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		3dcalc -prefix PeakMBase_Delete_Final -a PeakAverage1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		3dcalc -prefix RecMBase_Delete_Final -a RecAverage1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		3dcalc -prefix Rec1MBase_Delete_Final -a Rec1Average1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		3dcalc -prefix Rec2MBase_Delete_Final -a Rec2Average1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		
		# New mean files have subbricks not corresponding to periods but to contrasts of periods minus baseline as follows: 0 = Prep - Baseline, 1 = Anticipatory - Baseline, 2 = Peak - Baseline, 3 = Both Recovery - Baseline, 4 = Recovery1 - Baseline, 5 = Recovery2 - Baseline
		3dbucket -prefix epi_${subj}_${scantypename[$curind]}_Means -fbuc PrepMBase_Delete_Final+orig AntMBase_Delete_Final+orig PeakMBase_Delete_Final+orig RecMBase_Delete_Final+orig Rec1MBase_Delete_Final+orig Rec2MBase_Delete_Final+orig

		rm *_Delete*

		mv epi_${subj}_${scantypename[$curind]}_TS* Mean_Files
		mv epi_${subj}_${scantypename[$curind]}_Means* Mean_Files
	fi

	###This will trigger if both runs are present and clean
	if [ "$nscan" -eq 2 ]; then
		firstone=($(find epi_*_R${scanarray[0]}_errts+orig.HEAD))
		secondone=($(find epi_*_R${scanarray[1]}_errts+orig.HEAD))
		echo "Scans $firstone and $secondone were both kept for type $curtype."

		#Copy the original errts timeseries labeled by dosage
		3dcopy -verb $firstone epi_${scantypename[$curind]}_TS_temp1
		3dcopy -verb $secondone epi_${scantypename[$curind]}_TS_temp2
		3dMean -prefix epi_${subj}_${scantypename[$curind]}_TS epi_${scantypename[$curind]}_TS_temp1+orig epi_${scantypename[$curind]}_TS_temp2+orig

		#Now pull out the volumes for each period and make a mean image
		3dTstat -prefix BaseAverage1_Delete -mean $firstone'[0..17]'
		3dTstat -prefix PrepAverage1_Delete -mean $firstone'[18..24]'
		3dTstat -prefix AntAverage1_Delete -mean $firstone'[25..34]'
		3dTstat -prefix PeakAverage1_Delete -mean $firstone'[35..54]'
		3dTstat -prefix RecAverage1_Delete -mean $firstone'[55..115]'
		3dTstat -prefix Rec1Average1_Delete -mean $firstone'[55..84]'
		3dTstat -prefix Rec2Average1_Delete -mean $firstone'[85..115]'
		3dTstat -prefix BaseAverage2_Delete -mean $secondone'[0..17]'
		3dTstat -prefix PrepAverage2_Delete -mean $secondone'[18..24]'
		3dTstat -prefix AntAverage2_Delete -mean $secondone'[25..34]'
		3dTstat -prefix PeakAverage2_Delete -mean $secondone'[35..54]'
		3dTstat -prefix RecAverage2_Delete -mean $secondone'[55..115]'
		3dTstat -prefix Rec1Average2_Delete -mean $secondone'[55..84]'
		3dTstat -prefix Rec2Average2_Delete -mean $secondone'[85..115]'
		#Now get % signal change from baseline.
		3dcalc -prefix PrepMBase_Delete1 -a PrepAverage1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		3dcalc -prefix AntMBase_Delete1 -a AntAverage1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		3dcalc -prefix PeakMBase_Delete1 -a PeakAverage1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		3dcalc -prefix RecMBase_Delete1 -a RecAverage1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		3dcalc -prefix Rec1MBase_Delete1 -a Rec1Average1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		3dcalc -prefix Rec2MBase_Delete1 -a Rec2Average1_Delete+orig -b BaseAverage1_Delete+orig -expr '(a-b)'
		3dcalc -prefix PrepMBase_Delete2 -a PrepAverage2_Delete+orig -b BaseAverage2_Delete+orig -expr '(a-b)'
		3dcalc -prefix AntMBase_Delete2 -a AntAverage2_Delete+orig -b BaseAverage2_Delete+orig -expr '(a-b)'
		3dcalc -prefix PeakMBase_Delete2 -a PeakAverage2_Delete+orig -b BaseAverage2_Delete+orig -expr '(a-b)'
		3dcalc -prefix RecMBase_Delete2 -a RecAverage2_Delete+orig -b BaseAverage2_Delete+orig -expr '(a-b)'
		3dcalc -prefix Rec1MBase_Delete2 -a Rec1Average2_Delete+orig -b BaseAverage2_Delete+orig -expr '(a-b)'
		3dcalc -prefix Rec2MBase_Delete2 -a Rec2Average2_Delete+orig -b BaseAverage2_Delete+orig -expr '(a-b)'

		#Now average across the two scans.
		3dMean -prefix PrepMBase_Delete_Final PrepMBase_Delete1+orig PrepMBase_Delete2+orig
		3dMean -prefix AntMBase_Delete_Final AntMBase_Delete1+orig AntMBase_Delete2+orig
		3dMean -prefix PeakMBase_Delete_Final PeakMBase_Delete1+orig PeakMBase_Delete2+orig
		3dMean -prefix RecMBase_Delete_Final RecMBase_Delete1+orig RecMBase_Delete2+orig
		3dMean -prefix Rec1MBase_Delete_Final Rec1MBase_Delete1+orig Rec1MBase_Delete2+orig
		3dMean -prefix Rec2MBase_Delete_Final Rec2MBase_Delete1+orig Rec2MBase_Delete2+orig

		# New mean files have subbricks not corresponding to periods but to contrasts of periods minus baseline as follows: 0 = Prep - Baseline, 1 = Anticipatory - Baseline, 2 = Peak - Baseline, 3 = Both Recovery - Baseline, 4 = Recovery1 - Baseline, 5 = Recovery2 - Baseline
		3dbucket -prefix epi_${subj}_${scantypename[$curind]}_Means -fbuc PrepMBase_Delete_Final+orig AntMBase_Delete_Final+orig PeakMBase_Delete_Final+orig RecMBase_Delete_Final+orig Rec1MBase_Delete_Final+orig Rec2MBase_Delete_Final+orig

		rm *_Delete*

		mv epi_${subj}_${scantypename[$curind]}_TS* Mean_Files
		mv epi_${subj}_${scantypename[$curind]}_Means* Mean_Files
	fi

cd ../../../
done

cd Scripts

EndMNI_To_MNI $subj;

}
####################################################################
function EndMNI_To_MNI {

cd ../
subj=$1

#Go to their processed directory
procdir=processed_data_2/ISO_Scans_V15_NTRP/${subj}

cd $procdir
#transform their anatomical to tlrc space
orignsmprage=anat_final.*+orig.HEAD
#orignsmprage=mprage_*+orig.HEAD
@auto_tlrc -base TT_avg152T1+tlrc -input ${orignsmprage} -no_ss

#move everything to that folder
mv *+tlrc* Mean_Files/
#mv *+tlrc* Prep_Max_Mean_Files/

#swtich to the mean_files folder
cd Mean_Files/
#cd Prep_Max_Mean_Files/

#now convert your epi files

mnimprage=anat_final.*+tlrc.HEAD
#mnimprage=mprage_*+tlrc.HEAD

@auto_tlrc -apar ${mnimprage} -input epi_${subj}_Sal_Means+orig -dxyz 2
@auto_tlrc -apar ${mnimprage} -input epi_${subj}_pFive_Means+orig -dxyz 2
@auto_tlrc -apar ${mnimprage} -input epi_${subj}_Two_Means+orig -dxyz 2
@auto_tlrc -apar ${mnimprage} -input epi_${subj}_Sal_TS+orig -dxyz 2
@auto_tlrc -apar ${mnimprage} -input epi_${subj}_pFive_TS+orig -dxyz 2
@auto_tlrc -apar ${mnimprage} -input epi_${subj}_Two_TS+orig -dxyz 2

#for i in $(seq 1 6)
#do
#@auto_tlrc -apar ${mnimprage} -input epi_${subj}_r0${i}_PrepMeans+orig -dxyz 2
#done

cd ../../../../Scripts
}


#############################################################################################
#This function will take a subject name, and scan type "0" (saline) ".5", or "2", and determine which of their scans match this type.
function DetermineScanType {

	subj=$1
	type=$2

	###Checks the raw epi files for naming purposes because the run names in ISOonly.txt does not always line up with the dose order
	origdir=original_subject_data_MP/ISO_Scans/${subj}/scan*
	###Infodir looks to be unused by the script, which is good because the script that downloads and names the actual epi scans doesn't always line up with the record here
	infodir=$origdir/scan_record_files
	epidir=$origdir/EPI_files

	len=${#subj}
	if [ $len -eq 13 ]; then
		subj=${subj::${#subj}-8}
	fi

	#This will pull out the dose information needed from the ISOonly.txt file
	fileline=($(grep "$subj" ISOonly.txt))
	iso_dose_order=${fileline[5]}
	#Split the dose order into an array
	IFS=',' read -r -a dosearray <<< "$iso_dose_order"
	
	ndoses=${#dosearray[@]}
	whichscans=()
	for i in $(seq 1 $ndoses)
	do
		curval=$(($i-1))
		curdose=${dosearray[$curval]}
		if [ "${curdose}" == $type ]; then
			whichscans+=("$i")
		fi
	done

echo ${whichscans[@]};
}

