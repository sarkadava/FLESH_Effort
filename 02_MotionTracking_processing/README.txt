SETUP

If you continue from 01_XDF_processing, you can skip steps 1) and 2)

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

conda create --name FLESH_TSPROCESS

conda activate FLESH_TSPROCESS

2) install necessary packages

cd '\xxx\02_MotionTracking_processing'

pip install requirements_general.txt

3) in folder \openpose follow the STEP1 and STEP2 urls to download necessary files

* STEP1: place bin folder which would be in the openpose folder
* STEP2: place the 'pose_iter_XXXXXX.caffemodel' in the models/pose/body_135/ folder.

(Now you are ready to run scripts 01_Video_preparation and 02_Track_OpenPose)

4) create virtual environment FLESH_pose2sim

conda create --name FLESH_pose2sim

conda activate FLESH_pose2sim

5) install necessary packages

cd '\xxx\02_MotionTracking_processing'

pip install requirements_pose2sim.txt

(Now you are ready to run script 03_Track_pose2sim.ipynb)

4) create virtual environment FLESH_opensim

conda create --name FLESH_opensim

conda activate FLESH_opensim

5) install necessary packages

cd '\xxx\02_MotionTracking_processing'

pip install requirements_opensim.txt

(Now you are ready to run script 04_Track_InverseKinDyn.ipynb)



TROUBLESHOOTING

For motion tracking related trouble-shooting that is not addressed here, see respective documentations:

- OpenPose: https://github.com/CMU-Perceptual-Computing-Lab/openpose
- pose2sim: https://github.com/perfanalytics/pose2sim
- OpenSim: https://opensimconfluence.atlassian.net/wiki/spaces/OpenSim/overview