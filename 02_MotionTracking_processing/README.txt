SETUP



In this pipeline, we perform motion tracking in three steps

1) 2d coordinates with openpose

for that we need to have cutted videos into 3 individual cameras, placed into 2d-raw folders (Session-Participant-Trial)

2) 3d coordinates with pose2sim, filtered and augmented

for that we need 2d coodinates (in pose folder)
and Config.toml file (in Pose2Sim folder) that is distributed to each session/participant/trial

3) inverse kinematics and dynamics with opensim

for that we use the 3d (augmented) markers

3.1 first we scale the pose2sim 25b model according to our participant's mass and height, based on a tpose

all settings are in Scaling_Setup file

3.2 then we used the scaled model and perform inverse kinematics to each trial

all settings are in IK_Setup fike

3.3 last, we perform ID on each trial's IK file

all settings are in ID_Setup file