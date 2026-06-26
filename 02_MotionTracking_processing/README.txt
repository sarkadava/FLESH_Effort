Note that raw data are not available in this repository. They will be published in an upcoming work. 
If you are interested in viewing the data, contact kadava[et]leibniz[dot]de

Due to space management, we also do not provide the individual json files resulting from OpenPose. You can however find here the triangulated files (folder /pose-3d). 
We also do not share the participants' height and weight that are necessary for scaling the anatomical model and for inverse dynamics. Scaled models and resulting ID files are,
however, available in folder /kinematics.

SETUP

1) in folder \openpose follow the STEP1 and STEP2 urls to download necessary files

* STEP1: place bin folder which would be in the openpose folder
* STEP2: place the 'pose_iter_XXXXXX.caffemodel' in the models/pose/body_135/ folder.

(Now you are ready to run scripts 01_Video_preparation and 02_Track_OpenPose Always make sure you are in the folder of the script you are running)

2) Create virtual environment FLESH_MTRACK (We do not use FLESH_TSPROCESS here because of compatibility/dependency issues)

cd FLESH_ContinuousBodilyEffort
conda env create -f mt-environment.yml
conda activate FLESH_MTRACK

3) Add Conda Environment to Jupyter Notebook
python -m ipykernel install --user --name=FLESH_MTRACK  --display-name "Python (FLESH_MTRACK)"

(Now you are ready to run script 03_Track_pose2sim_OpenSim. Always make sure you are in the folder of the script you are running and that you selected kernel FLESH_MTRACK)

TROUBLESHOOTING

For motion tracking related trouble-shooting that is not addressed here, see respective documentations:

- OpenPose: https://github.com/CMU-Perceptual-Computing-Lab/openpose
- pose2sim: https://github.com/perfanalytics/pose2sim
- OpenSim: https://opensimconfluence.atlassian.net/wiki/spaces/OpenSim/overview