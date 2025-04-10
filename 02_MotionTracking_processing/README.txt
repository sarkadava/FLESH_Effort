SETUP

If you continue from 01_XDF_processing, you can skip steps 1-3

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

conda create --name FLESH_TSPROCESS python=3.12.2

conda activate FLESH_TSPROCESS

2) - Add Conda Environment to Jupyter Notebook
pip install ipykernel
python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"

3) install necessary packages

pip install -r requirements_general.txt

4) in folder \openpose follow the STEP1 and STEP2 urls to download necessary files

* STEP1: place bin folder which would be in the openpose folder
* STEP2: place the 'pose_iter_XXXXXX.caffemodel' in the models/pose/body_135/ folder.

(Now you are ready to run scripts 01_Video_preparation and 02_Track_OpenPose)

5) create virtual environment FLESH_pose2sim

conda create --name FLESH_pose2sim python=3.10 -y

conda activate FLESH_pose2sim

6) Add Conda Environment to Jupyter Notebook

pip install ipykernel
python -m ipykernel install --user --name=FLESH_pose2sim --display-name "Python (FLESH_pose2sim)"

7) install necessary packages

cd '\xxx\02_MotionTracking_processing'

pip install -r requirements_pose2sim.txt

(Now you are ready to run script 03_Track_pose2sim.ipynb)

8) create virtual environment FLESH_opensim

conda create --name FLESH_opensim python=3.8.19

conda activate FLESH_opensim

9) Add Conda Environment to Jupyter Notebook

pip install ipykernel
python -m ipykernel install --user --name=FLESH_opensim --display-name "Python (FLESH_opensim)"

10) install necessary packages

cd '\xxx\02_MotionTracking_processing'

pip install -r requirements_opensim.txt

(Now you are ready to run script 04_Track_InverseKinDyn.ipynb)

TROUBLESHOOTING

For motion tracking related trouble-shooting that is not addressed here, see respective documentations:

- OpenPose: https://github.com/CMU-Perceptual-Computing-Lab/openpose
- pose2sim: https://github.com/perfanalytics/pose2sim
- OpenSim: https://opensimconfluence.atlassian.net/wiki/spaces/OpenSim/overview