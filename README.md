# FLESH_ContinuousBodilyEffort


TODO:
- have one META.txt with pcnID, sex, weight and height in RAWDATA
- req + installation for movement annnotation
- req + installation for final merge
- req + installation for feature extraction
- req + installation for xgboost
- req + installation for modelling

- check if sound annotation based on envelope would suffice
- check training vs classifying dataset for different number of columns
- check if three main vars are correlated


NOTES FOR 03

Notes 01

01 - You dont say what the aggregated kinematic measures actually are
01 - are you taking euclidean sum from speed of connected joints?
01 - just curious, "get sampling rate 1/np.mean(np.diff(mot_df['time']))" -> so maybe we can discuss whether super variable frame rates will be problematic here (otherwise we pre-regularize perhaps)
01 - lets discuss some smoothing at some point, why smooth more or less, would love to hear your reasoning
01 - not needed, but nice to know: so scipy.signal.savgol_filter is also used sometimes to take better estimates of derivatives, could be an option instead of differencing and then smoothing https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.savgol_filter.html (you just set deriv = 1 or 2 (1st, 2nd derivative).
Notes 02

missed intro's, but looks fine to me
Notes 03

do we really need soundgen? same for praat. I dont think we should change it, but it would be great to hear your thinking on this
not necessary, but to consider: https://www.freecodecamp.org/news/how-to-run-r-programs-directly-in-jupyter-notebook-locally/
notes 04

lacks intros, and explanations of what exactly were checking for
Envelope peaks as a reference point for formants -> cool!
peaks, _ = find_peaks(env_trial['envelope'], height=np.mean(env_trial['envelope'])) -> we need to be careful here. maybe we can discuss (would you not set this the same for all paricipants?)
Notes general At the top we could write a method like section that overviews the whole procedure (which then can serve as input for a more abbreviated method in the paper). This way people can connect code with conceptual level steps. For some scripts I lose the concepts with the script (because no data, no method, and no intro to the code chunk).