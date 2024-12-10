from scipy.signal import savgol_filter
import pandas as pd
import os
import glob

curfolder = os.getcwd()
print(curfolder)

projectdata = os.path.join(curfolder, 'projectdata')

sessionstotrack = glob.glob(os.path.join(projectdata, 'Session*'))

print(sessionstotrack)

# get trc files 
MTtotrack = glob.glob(sessionstotrack[0] + '*/P*/*', recursive=True)


# get rid of all the folders that are not the ones we want to track, like .sto files
MTtotrack = [x for x in MTtotrack if 'sto' not in x]
MTtotrack = [x for x in MTtotrack if 'txt' not in x]
MTtotrack = [x for x in MTtotrack if 'xml' not in x]
MTtotrack = [x for x in MTtotrack if 'opensim' not in x]
MTtotrack = [x for x in MTtotrack if 'Results' not in x]
MTtotrack = [x for x in MTtotrack if 'toml' not in x]

print(MTtotrack)

# get trc file from first MTto track
trcfiles = glob.glob(MTtotrack[0] + '/*/*.trc')
print(trcfiles)

sample = trcfiles[0]
print(sample)

# get mot files
MTtotrack2 = glob.glob(sessionstotrack[0] + '*/P*/ResultsInverseKinematics', recursive=True)
print(MTtotrack2)
motfiles = glob.glob(MTtotrack2[0] + '/*.mot')
motfiles

motsample = motfiles[0]

# get sto files
MTtotrack3 = glob.glob(sessionstotrack[0] + '*/P*/ResultsInverseDynamics', recursive=True)
print(MTtotrack3)
stofiles = glob.glob(MTtotrack3[0] + '/*.sto')
stofiles

stosample = stofiles[0]

def smooth_trc_mot(file_path, end='trc', window_length=15, polyorder=3):
    # read the file
    if end == 'trc':
        df = pd.read_csv(file_path, sep='\t', skiprows=4)

        # get all the columns except the first two
        columns = df.columns[2:]
    elif end == 'mot':
        df = pd.read_csv(file_path, sep='\t', skiprows=10)
        # get all columns except time
        columns = df.columns[1:]

    elif end == 'sto':
        df = pd.read_csv(file_path, sep='\t', skiprows=6)
        # get all columns except time
        columns = df.columns[1:]

    print(df)
    
    # smooth them
    for col in columns:
        df[col] = savgol_filter(df[col], 15, 3)
    # save it back
    df.to_csv(file_path, sep='\t', index=False)

    return df

sample_smoothed = smooth_trc_mot(sample)
sample_smoothed

sampledf = pd.read_csv(sample, sep='\t', skiprows=4)

sampledf
# plot first column of sample and sample_smoothed
import matplotlib.pyplot as plt

plt.plot(sample_smoothed['X1'], label='smoothed')
plt.plot(sampledf['X1'], label='original')
plt.legend()
plt.show()

motdf = pd.read_csv(motsample, sep='\t', skiprows=10)
mot_smoothed = smooth_trc_mot(motsample, end='mot')

# plot first column of mot and mot_smoothed
plt.plot(mot_smoothed['pelvis_tilt'], label='smoothed')
plt.plot(motdf['pelvis_tilt'], label='original')
plt.legend()
plt.show()

stodf = pd.read_csv(stosample, sep='\t', skiprows=6)
sto_smoothed = smooth_trc_mot(stosample, end='sto')

# plot first column of sto and sto_smoothed
plt.plot(sto_smoothed['pelvis_tilt_moment'], label='smoothed')
plt.plot(stodf['pelvis_tilt_moment'], label='original')
plt.legend()
plt.show()