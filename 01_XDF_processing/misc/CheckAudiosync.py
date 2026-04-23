from __future__ import print_function
import wave
import numpy as np
import matplotlib.pyplot as plt
import os
import numpy as np
import wave
import glob
import random


# Set folders
curfolder = os.getcwd()
experiment_to_process = curfolder + '\\..\\00_RAWDATA\\'

audio_48 = experiment_to_process
audio_16 = curfolder + '\\data\\Data_processed\\CsvDataTS_raw\\Audio\\'
audio_16_files = glob.glob(audio_16 + '*denoised.wav')

audio_48_files = glob.glob(audio_48 + '**/*denoised_aligned.wav', recursive=True)

errorlist = []

segment_duration_sec = 600  # 10 minutes


def extract_segment(wav_path, segment_frames, start_frame):
    with wave.open(wav_path, 'rb') as wf:
        framerate = wf.getframerate()
        n_channels = wf.getnchannels()
        total_frames = wf.getnframes()

        if start_frame + segment_frames > total_frames:
            segment_frames = total_frames - start_frame  # adjust if file is too short

        wf.setpos(start_frame)
        frames = wf.readframes(segment_frames)

        audio_data = np.frombuffer(frames, dtype=np.int16)
        if n_channels == 2:
            audio_data = audio_data[::2]  # Use only left channel
        return audio_data

def plot_two_segments(data1, data2, label1="Audio 1 (16 kHz)", label2="Audio 2 (48 kHz)"):
    plt.figure(figsize=(20, 8))
    plt.subplot(2, 1, 1)
    plt.plot(data1)
    plt.title(label1)
    plt.xlabel("Sample")
    plt.ylabel("Amplitude")

    plt.subplot(2, 1, 2)
    plt.plot(data2)
    plt.title(label2)
    plt.xlabel("Sample")
    plt.ylabel("Amplitude")

    plt.tight_layout()
    plt.show()

# Example file lists (replace with actual paths)
# audio_48_files = [...]  # List of full paths to 48kHz files
# audio_16_files = [...]  # List of full paths to 16kHz files

for file48 in audio_48_files:
    print('Working on: ' + file48)

    try:
        filename = os.path.basename(file48).split('.')[0]
        parts = filename.split('_')
        sessionID = parts[0] + '_' + parts[1]
    except Exception as e:
        print(f"Filename parsing error: {file48}")
        errorlist.append(file48)
        continue

    try:
        file16 = [x for x in audio_16_files if sessionID in x][0]
        print('Corresponding file: ' + file16)

        # Get frame rates and total frames
        with wave.open(file48, 'rb') as f48:
            framerate_48 = f48.getframerate()
            total_frames_48 = f48.getnframes()

        segment_frames_48 = segment_duration_sec * framerate_48

        if segment_frames_48 >= total_frames_48:
            print("File is shorter than 10 minutes — using start frame 0")
            start_frame_48 = 0
        else:
            start_frame_48 = random.randint(0, total_frames_48 - segment_frames_48)

        # Align 16kHz start time to match the same segment in seconds
        with wave.open(file16, 'rb') as f16:
            framerate_16 = f16.getframerate()

        segment_frames_16 = segment_duration_sec * framerate_16
        start_frame_16 = int((start_frame_48 / framerate_48) * framerate_16)

        # Extract aligned segments
        audio_data1 = extract_segment(file16, segment_frames_16, start_frame_16)
        audio_data2 = extract_segment(file48, segment_frames_48, start_frame_48)

        # Plot both in one figure
        plot_two_segments(audio_data1, audio_data2)

    except Exception as e:
        print('Error in file: ' + file48)
        print(f'Details: {e}')
        errorlist.append(file48)
        continue