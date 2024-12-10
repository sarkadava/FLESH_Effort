import cv2
import os

# setup folder

curfolder = os.getcwd()
print(curfolder)
video = curfolder + '/calib_lab.avi'

def cut_video(input_video, output_video, start_frame, end_frame):
    # Open the input video
    cap = cv2.VideoCapture(input_video)
    
    # Get the frames per second (fps), width, and height of the video
    fps = cap.get(cv2.CAP_PROP_FPS)
    width = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH))
    height = int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
    
    # Define the codec and create VideoWriter object
    fourcc = cv2.VideoWriter_fourcc(*'XVID')
    out = cv2.VideoWriter(output_video, fourcc, fps, (width, height))
    
    # Set the frame position to the start_frame
    cap.set(cv2.CAP_PROP_POS_FRAMES, start_frame)
    
    # Read until the end_frame
    while cap.isOpened() and cap.get(cv2.CAP_PROP_POS_FRAMES) <= end_frame:
        ret, frame = cap.read()
        if ret:
            # Write the frame to the output video
            out.write(frame)
        else:
            break
    
    # Release everything when done
    cap.release()
    out.release()
    cv2.destroyAllWindows()

output_video = 'calib_cutted.avi'
start_frame = 1126  # Change this to the starting frame you desire
end_frame = 3748    # Change this to the ending frame you desire
cut_video(video, output_video, start_frame, end_frame)
