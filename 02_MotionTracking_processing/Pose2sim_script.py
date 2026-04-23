#| code-fold: true
#| code-summary: Code to prepare the environment

from Pose2Sim import Pose2Sim
import os
import glob
import pandas as pd
from trc import TRCData
import pandas as pd
import shutil
import cv2
import numpy as np
import toml
import traceback
import logging

def log_step_error(step_name, session_id, folder, error_log_path):
    # Create a human-readable message
    msg = f'{step_name} failed for session {session_id} in folder {folder}'
    print(msg)
    logging.error(msg)

    # Write message + full traceback into a file
    with open(error_log_path, 'a') as f:
        f.write(msg + '\n')
        traceback.print_exc(file=f)  # full stack trace

curfolder = os.getcwd()
#error_log = []

# Prepare special log txt
error_log = curfolder + '/pose2sim_error_log.txt'
if not os.path.exists(error_log):
    with open(error_log, 'w') as f:
        f.write('Pose2Sim processing error log\n')
        f.write('============================\n\n')

# Here is our config.file
#pose2simprjfolder = curfolder + '\Pose2Sim\FLESH_setup\\'
pose2simprjfolder = os.path.join(curfolder, 'Pose2Sim', 'FLESH_setup')

# Here we store the data
inputfolder = os.path.join(curfolder, 'projectdata')
folderstotrack = glob.glob(os.path.join(curfolder, 'projectdata', '*'))
#print(folderstotrack)

#target = ['_6_2']  # only these sessions will be processed

# Filter only folders that contain target sessions
#folderstotrack = [x for x in folderstotrack if any(t in x for t in target)]
          

# Here we store mass information (weight, height) about participants
META = pd.read_csv(os.path.join(curfolder, '..', '00_raw', 'all_demodata.csv')) #CHANGED

# Initiate empty list
pcnfolders = []

# Get all the folders per session, per participant
for i in folderstotrack:
    pcnfolders = glob.glob(os.path.join(i, '*'))

# Get rid of all pontetially confusing files/folders
pcnfolders = [x for x in pcnfolders if 'Config' not in x]
pcnfolders = [x for x in pcnfolders if 'opensim' not in x]
pcnfolders = [x for x in pcnfolders if 'xml' not in x]
pcnfolders = [x for x in pcnfolders if 'ResultsInverseDynamics' not in x]
pcnfolders = [x for x in pcnfolders if 'ResultsInverseKinematics' not in x]
pcnfolders = [x for x in pcnfolders if 'sto' not in x]
pcnfolders = [x for x in pcnfolders if 'txt' not in x]
pcnfolders = [x for x in pcnfolders if 'calibration' not in x]
print(pcnfolders[0:10])

todo = ['18_2', '35_2', '36_2', '49_2']
#| eval: false

# Set framerate
framerate = 60

# How many x-th frame do we extract from the calibration video? 
framepick = 10

# Copy a folder in pose2simprjfolder and its contents to folders
source1 = pose2simprjfolder+'/Config.toml'
source2 = pose2simprjfolder+'/opensim/'

for i in folderstotrack:

    # print(i)
    # break

    # if '62_' not in i:
    #     continue

    os.chdir(i)
    print('working on folder: ' + i)



    sessionID = i.split(os.sep)[-1].split('_')[1]
    session_part = i.split(os.sep)[-1].split('_')[2]

    ID = sessionID + '_' + session_part
    if ID not in todo:
        print(f'Skipping session {sessionID} as it is not in the todo list.')
        continue
    

    

    # check all the folders in i
    # pcn1folders = glob.glob(i + '/P0/*')
    # pcn2folders = glob.glob(i + '/P1/*')
    # pcnfolders_in_session = pcn1folders + pcn2folders

    # # Get rid of all pontetially confusing files/folders
    # pcnfolders_in_session = [x for x in pcnfolders_in_session if 'Config' not in x]
    # pcnfolders_in_session = [x for x in pcnfolders_in_session if 'opensim' not in x]
    # pcnfolders_in_session = [x for x in pcnfolders_in_session if 'xml' not in x]
    # pcnfolders_in_session = [x for x in pcnfolders_in_session if 'ResultsInverseDynamics' not in x]
    # pcnfolders_in_session = [x for x in pcnfolders_in_session if 'ResultsInverseKinematics' not in x]
    # pcnfolders_in_session = [x for x in pcnfolders_in_session if 'sto' not in x]
    # pcnfolders_in_session = [x for x in pcnfolders_in_session if 'txt' not in x]
    #print(pcnfolders_in_session[0:10])

    # check if all folders have folder pose-3d with two .trc files
    # all_folders_ready = True
    # for folder in pcnfolders_in_session:
    #     pose3d_folder = os.path.join(folder, 'pose-3d')
    #     if not os.path.exists(pose3d_folder):
    #         all_folders_ready = False
    #         print(f'Missing pose-3d folder in {folder}')
            
    #     trc_files = glob.glob(os.path.join(pose3d_folder, '*.trc'))
    #     if len(trc_files) < 2:
    #         all_folders_ready = False
    #         print(f'Not enough .trc files in {pose3d_folder}')
            
    # if all_folders_ready:
    #     print(f'All folders ready in session {sessionID}, skipping to next session.')
        #continue

    # First we need to prepare Config.file to all levels of folders (plus opensim to P0 and P1)

    # Copy to session folder
    # shutil.copy(source1, i + '/')

    # input_toml = load_toml(i+'/Config.toml')

    # # Update the p0 info
    # mass_p0 = META.loc[(META['pcn_ID'] == sessionID + '_0'), 'weight'].values[0] # this is new
    # height_p0 = META.loc[(META['pcn_ID'] == sessionID + '_0'), 'height'].values[0]
    # updated_toml_p0 = update_participant_info(input_toml, height_p0, mass_p0)

    # # Update p1 info
    # mass_p1 = META.loc[(META['pcn_ID'] == sessionID + '_1'), 'weight'].values[0] # this is new
    # height_p1 = META.loc[(META['pcn_ID'] == sessionID + '_1'), 'height'].values[0]
    # updated_toml_p1 = update_participant_info(input_toml, height_p1, mass_p1)
    
    # # Save the updated TOML data
    # save_toml(updated_toml_p0, i+'/P0/Config.toml')
    # save_toml(updated_toml_p1, i+'/P1/Config.toml')

    # p0_source = i+'/P0/Config.toml'
    # p1_source = i+'/P1/Config.toml'

    # # Copy necessary files 
    # for j in pcnfolders:
    #     if 'P0' in j:
    #         shutil.copy(p0_source, j + '/')
    #         print('source = ' + source1 + ' to destination: ' + j+'/')

    #     if 'P1' in j:
    #         shutil.copy(p1_source, j + '/')
    #         print('source = ' + source1 + ' to destination: ' + j+'/')

    # if not os.path.exists(i+'/P0/opensim/'):
    #     shutil.copytree(source2, i+'/P0/opensim/')
    #     print('source = ' + source2 + ' to destination: ' + i+'/P0/opensim/')

    # if not os.path.exists(i+'/P1/opensim/'):
    #     shutil.copytree(source2, i+'/P1/opensim/')
    #     print('source = ' + source2 + ' to destination: ' + i+'/P1/opensim/')

    # Now we calibrate
    print('Step: Calibration')

    # Calibrate only if there is no toml file in the calibration folder
    if not os.path.exists(i+'/calibration/Object_points.trc'): ## HERE NEW
        print('Extrensic Calibration file not found')
        
        # Now we prepare images from calibration videos
        calib_folders = glob.glob(i+'/calibration/*/*')

        #if selected_frame.png is not, select one
        if not os.path.exists(os.path.join(i, 'calibration', 'extrinsics', 'cam1', 'selected_frame.png')):
            input_videos = [
                glob.glob(os.path.join(i, 'calibration', 'extrinsics', 'cam1', '*cam1.avi'))[0],
                glob.glob(os.path.join(i, 'calibration', 'extrinsics', 'cam2', '*cam2.avi'))[0],
                glob.glob(os.path.join(i, 'calibration', 'extrinsics', 'cam3', '*cam3.avi'))[0]
            ]

            output_dirs = [
                os.path.join(i, 'calibration', 'extrinsics', 'cam1'),
                os.path.join(i, 'calibration', 'extrinsics', 'cam2'),
                os.path.join(i, 'calibration', 'extrinsics', 'cam3')
            ]
            
            # Call the function to select and save frames
            select_and_save_frame_every_x(input_videos, output_dirs, x_interval=framepick)



        # for c in calib_folders:
        #     print(c)
        #     split = c.split(os.path.sep)
        #     camIndex = split[-1]
        #     # Extrinsic calibration
        #     if 'extrinsics' in c:
        #         # find the avi file in the folder
        #         input_video = glob.glob(os.path.join(c, sessionID + '_*_' + camIndex + '.avi'))[0]
        #         #input_video = c+'/'+ sessionID +'_*_'+camIndex+'.avi'  #NEW
        #         print(input_video)
        #     # Intrinsic
        #     # else:
        #     #     input_video = c+'/'+ sessionID +'_checker_intrinsics_'+camIndex+'.avi'

        #     output_dir = c + '/'
            
            # print('We are now saving frames extracted from calibration videos')
            # saveFrame_fromVideo(framepick, output_dir, input_video)
    
        print('Calibration file does not exist, calibrating...')
        Pose2Sim.calibration() 
        

        # Get the last element of the i
        split = i.split(os.path.sep)
        parts = split[-1].split('_')
        # Get the sessionID
        session_id = parts[1]
        session_part = parts[-1]

        # If session_part is 1, we copy trc and calib file to the session that has some id, but part 2
        if session_part == '1':
            # Copy the calibration file to the session with the same id, but part 2
            copy_to_part = '2'
            # Get the new folder name
            new_folder = 'Session_'+session_id+'_'+copy_to_part
            # Get the new folder path
            new_folder_path = inputfolder + new_folder
            # In new_folder_path, create folder calibration if it doesn't exist
            if not os.path.exists(new_folder_path+'\\calibration\\'):
                os.makedirs(new_folder_path+'\\calibration\\')
            
            # Get the calibration file path
            #calib_file = i + '/calibration/Calib_board.toml'
            # Get the trc file path
            trc_file = i + '/calibration/Object_points.trc'
            
            # Copy the files to the new folder
            #shutil.copy(calib_file, new_folder_path + '/calibration/') ##changed- that is already there
            shutil.copy(trc_file, new_folder_path + '/calibration/')
        
        # Part 2 does not need to be calibrated so we can just proceed
        else:
            continue

    # If calibration file exists, then we can skip calibration
    else:
        print('Calibration file found, no need to calibrate')
    
    # try:
    #     print('Step: triangulation')
    #     Pose2Sim.triangulation()
    # except Exception as e:
    #     log_step_error('Triangulation', sessionID, i, error_log)
    #     continue

    # try:
    #     print('Step: filtering')
    #     Pose2Sim.filtering()
    # except Exception as e:
    #     log_step_error('Filtering', sessionID, i, error_log)
    #     continue

    #Inverse kinematics
    try:
        print('Step: kinematics')
        Pose2Sim.kinematics()
    except Exception as e:
        log_step_error('Kinematics', sessionID, i, error_log)
        continue

    #Marker augmentation (note that this works only with model 25)
    # print('Step: marker augmentation')
    # Pose2Sim.markerAugmentation()



