form Formant optimization
    comment Directory of sound files:
    text soundfiles_dir C:\Users\Sarka Kadava\Documents\Github\FLESH_ContinuousBodilyEffort\TS_processing\Audio\
    sentence soundfile_extension .wav
    comment Give the folder where to save extracted data frames:
    sentence Folder C:\Users\Sarka Kadava\Documents\Github\FLESH_ContinuousBodilyEffort\TS_processing\TS_formants\
endform

soundfiles_list = Create Strings as file list... soundfiles 'soundfiles_dir$'*'soundfile_extension$'
soundfiles_list_id = selected("Strings", -1)
number_of_sound_files = Get number of strings

# Formant extraction parameters
ceil_lo = 3500
ceil_hi = 6000
timestep = 0.005

for i to number_of_sound_files
    # Get the current sound file
    soundfile_with_ext$ = Get string... i
    
    # Separate the base filename (without extension) from the extension
    soundfile$ = soundfile_with_ext$ - soundfile_extension$

    # Read the sound file
    Read from file... 'soundfiles_dir$''soundfile_with_ext$'

    # create baseline formant object
    To Formant (burg)... timestep 5 ceil_lo 0.025 50
    Rename... 'soundfile$'_baseline

    # create baseline formant track matrices
    for j from 1 to 5
        select Formant 'soundfile$'_baseline
        To Matrix... j
        Rename... f'j'
    endfor

    # iterate through F5 ceilings (ceil_lo Hz - ceil_hi Hz) in steps of 50 Hz
    steps = (ceil_hi - ceil_lo)/50

    for k from 1 to steps
        # get current F5 ceiling (Hz)
        step = k * 50
        ceiling = ceil_lo + step

        # create formant tracks with the current ceiling
        select Sound 'soundfile$'
        To Formant (burg)... timestep 5 ceiling 0.025 50

        # add formant measures to their respective matrices
        for j from 1 to 5
            select Formant 'soundfile$'
            To Matrix... j
            Rename... f'j'_new

            plus Matrix f'j'
            Merge (append rows)

            select Matrix f'j'
            Remove

            select Matrix f'j'_f'j'_new
            Rename... f'j'
        endfor

        # clean up
        select Formant 'soundfile$'
        for j from 1 to 5
            plus Matrix f'j'_new
        endfor
        Remove
    endfor

    # Create the table to save formant data
    select Matrix f1
    points = Get number of columns
    Create Table with column names... 'soundfile$'_formants points time f1 f2 f3 f4 f5

    # iterate through each time step
    for l from 1 to points
        # estimate the point of measurement stability for each formant at the time step
        for j from 1 to 5
            select Matrix f'j'
            f'j'# = Get all values in column... l

            # calculate the first difference (velocity) of the formant values
            diff# = zero#(points - 1)		
            for m from 1 to points - 1
                diff#[m] = abs(f'j'#[m + 1] - f'j'#[m])
            endfor

            # find the maximum absolute difference
            maxidx = imax(diff#)

            # trim from the maximum index
            ftrim# = zero#(1 + points - maxidx)
            n = 1
            for m from maxidx to points
                ftrim#[n] = f'j'#[m]
                n = n + 1
            endfor

            # calculate the first difference of the trimmed data
            diff# = zero#(points - maxidx)
            for m from 1 to points - maxidx - 1
                diff#[m] = abs(ftrim#[m + 1] - ftrim#[m])
            endfor
            
            # find the minimum absolute difference of the trimmed data 
            minidx = imax(0 - diff#)
            
            # get the formant value at the stability point
            f'j' = ftrim#[minidx]
        endfor 

        select Formant 'soundfile$'_baseline
        time = Get time from frame number... l

        # add time step and median formant values to table
        select Table 'soundfile$'_formants
        Set numeric value... l "time" time
        for j from 1 to 5
            Set numeric value... l "f'j'" f'j'
        endfor
    endfor

    # Save the table to the specified directory
    select Table 'soundfile$'_formants
    Save as tab-separated file... 'Folder$''soundfile$'_formants.txt

    # Clean up
    select Formant 'soundfile$'_baseline
    for j from 1 to 5
        plus Matrix f'j'
    endfor
    Remove
endfor
