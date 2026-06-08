'''
created: 31 May 2023
last update: 4 October 2023
@author: sarkadava
'''


'''
This is first part of experiment.
It asks participants to perform a concept, and the second participant to guess the meaning of the performance. There is no feedback.

Number of practice stimuli per person per condition: 2
Number of stimuli per person per condition: 10

'''


### PACKAGES ###
from psychopy import visual, core, event
import pandas as pd
import random
import csv
from pylsl import StreamInfo, StreamOutlet  
from rusocsci import buttonbox
import os
import json

# define working directory
curfolder = os.getcwd()

## BUTTON BOX
# initiate button box
bb = buttonbox.Buttonbox()

# participant id
participant_id_input = input("Enter participant ID: ")
team_name = input("Enter team name: ")

# json file for prompts.
experiment_data_path = curfolder+"\\json\\NL_exp2_data.json"
with open(experiment_data_path, 'r', encoding='utf-8') as json_file:
    experiment_data = json.load(json_file)

# json file for instructions
instructions_path = curfolder+"\\json\\NL_exp2_instructions.json"
with open(instructions_path, 'r', encoding='utf-8') as json_file:
    instructions = json.load(json_file)

### FUNCTIONS ###

# Define a function to create TextStim objects
def create_text_stim(win, prompt_key):
    properties = experiment_data.get(prompt_key, {})
    text = properties.get("text", "")
    height = properties.get("height", 20)
    color = properties.get("color", "black")
    pos = properties.get("pos", None)
    wrapWidth = properties.get("wrapWidth", 1200)


    text_stim = visual.TextStim(win, text=text, height=height, color=color,  wrapWidth=wrapWidth)
    
    if pos is not None:
        text_stim.pos = pos

    return text_stim

# Define a function to create instructions
def create_instr(win, condition, prompt_key):
    properties = instructions.get(condition, {}).get(prompt_key, {})
    text = properties.get("text", "")
    height = properties.get("height", 40)
    color = properties.get("color", "black")
    pos = properties.get("pos", [0,0])
    wrapWidth = properties.get("wrapWidth", 1700)

    instructions_text = visual.TextStim(win, text=text, height=height, color=color, wrapWidth=wrapWidth)
    
    if pos is not None:
        instructions_text.pos = pos

    return instructions_text

# Function to create visual stimuli

def create_visual(win, prompt_key):
    properties = experiment_data.get(prompt_key, {})
    image = properties.get("image", None)
    pos = properties.get("pos", [0,0])
    opacity = properties.get("opacity", 1)
    size = properties.get("size", [200,200])
    ori = properties.get("ori", 0)

    visual_stim = visual.ImageStim(win, image=image, size=size, opacity=opacity, pos=pos, ori=ori)
    
    if pos is not None:
        visual_stim.pos = pos

    return visual_stim

# Function to move to the next stimulus
def next_stimulus():
    # Code to move to the next stimulus/slide
    print("Moving to the next stimulus/slide...")

# Function to exit the experiment
def exit_experiment():
    # Code to exit the experiment
    print("End of experiment")  
    outlet.push_sample([markernames[8]])
    print("Experiment ended prematurely...")
    core.quit()

# Function to handle key presses
def key_press(event):
    if event.name == 'space':
        next_stimulus()
    elif event.name == 'esc':
        exit_experiment()

# Function to shuffle and distribute the concepts to the participants
# do this outside, generate more, save them and check for distribution
def create_stimuli(concept_list):
    # shuffle the concept list
    random.shuffle(concept_list)

    # separate it to two lists, each one for one participant (cycle)
    list_1 = concept_list[:len(concept_list)//2]
    random.shuffle(list_1)
    list_2 = concept_list[len(concept_list)//2:]
    random.shuffle(list_2)

    # separate each of these list to three modalities
    list_1_ges = list_1[:len(list_1)//3]
    list_1_mult = list_1[len(list_1)//3:2*len(list_1)//3]
    list_1_voc = list_1[2*len(list_1)//3:]

    list_2_ges = list_2[:len(list_2)//3]
    list_2_mult = list_2[len(list_2)//3:2*len(list_2)//3]
    list_2_voc = list_2[2*len(list_2)//3:]
    
    return list_1_ges, list_1_mult, list_1_voc, list_2_ges, list_2_mult, list_2_voc

## get input function
def get_input(text_input, question, prompt, confirm, which_win):
    text_input = ''
    while True:
        keys = event.getKeys()
        if 'escape' in keys:
            exit_experiment()
        elif 'return' in keys:
            break
        elif 'space' in keys:
            text_input += ' '  # Add a blank space
        elif 'backspace' in keys:
            text_input = text_input[:-1]  # Delete the last character
        elif keys:
            text_input += keys[0]
        else:
            # add nothing
            pass
        prompt.text = f'{question}\n\n {text_input}'
        prompt.draw()
        confirm.draw()
        which_win.flip()

    return text_input

# Define a function to display text on two windows and additional stimuli
def display_on_windows(win0, text0, win1, text1, additional_stimuli_win0=[], additional_stimuli_win1=[]):
    # Draw on the first window
    for stim in additional_stimuli_win0:
        stim.draw(win0)
    text0.draw(win0)


    # Draw on the second window
    for stim in additional_stimuli_win1:
        stim.draw(win1)
    text1.draw(win1)

    # clear events because of the buttonbox, maybe not needed
    event.clearEvents()
    bb.clearEvents()

    # Flip both windows
    win0.flip()
    win1.flip()


# Function to handle incorrect answers
def handle_incorrect_response(response, concept):
    global correction
    global correct_answer
    global game_point
# If it is incorrect, involve administrator
    # wait for the key
    # event.waitKeys()
    key = []
    while key == []: 
        key = bb.getButtons(buttonList= ['A', 'E'])
        core.wait(0.01)
    if 'A' in key:
        feedback_text.text = f"Goed zo! Het antwoord {response} is correct."
        feedback_text.size = 60
        feedback_text.color = 'green'
        user_answer_text.text = f'Goed gedaan! Je partner raadde {response} correct en nu kunnen jullie door naar het volgende woord.'
        user_answer_text.size = 70
        user_answer_text.color = 'green'
        correct_answer = True  # Set the flag to indicate a correct answer
        if correction == 0:
            game_point = 2
        elif correction == 1:
            game_point = 1
        else:
            game_point = 0.5
    elif 'E' in key and correction >= 2:
        feedback_text.text = f"Je antwoord {response} was helaas niet juist, in plaats daarvan probeerde je teamgenoot {concept} uit te drukken. Dit was hierbij je laatste kans, maar geef niet op! Probeer het opnieuw bij het volgende woord."
        feedback_text.size = 60
        feedback_text.color = 'red'
        user_answer_text.text = f"Je probeerde {concept} uit te drukken, maar je parner raadde {response}.\n\nDit was hierbij je laatste kans, maar geef niet op! Probeer het opnieuw bij het volgende woord."
        user_answer_text.color = 'red'
        user_answer_text.size = 70
        game_point = 0
                
    elif 'E' in key and correction < 2: 
        feedback_text.text = f"Je antwoord {response} was helaas niet juist.\n\nGelukkig heb je nog een kans. Kijk/luister opnieuw goed naar de performance van je teamgenoot."
        feedback_text.color = 'red'
        feedback_text.size = 60
        user_answer_text.text = f"Je bedoelde {concept} maar je teamgenoot raadde in plaats daarvan {response}.\n\nGelukkig heb je nog een kans. Maak je klaar!"
        user_answer_text.color = 'red'
        user_answer_text.size = 70

    else:
        print('Error in evaluating correct answer.')

    # Display the user's answer on win0
    user_answer_text.draw()
    feedback_text.draw()   
    win1.flip()
    win0.flip()

    # third screen
    user_answer_text.draw(win2)
    feedback_text.draw(win3)
    win2.flip()
    win3.flip()

    return correction
    return correct_answer

# Function to handle responses
def handle_response(response, concept):
    global correction
    global correct_answer
    global game_point
    if response == concept:
        feedback_text.text = f"Goed zo! Het antwoord {response} is correct."
        feedback_text.color = 'green'
        feedback_text.size = 60
        user_answer_text.text = f'Goed gedaan! Je partner raadde {response} correct en nu kunnen jullie door naar het volgende woord.'
        user_answer_text.color = 'green'
        user_answer_text.size = 70
        correct_answer = True  # Set the flag to indicate a correct answer
        if correction == 0:
            game_point = 2
        elif correction == 1:
            game_point = 1
        else:
            game_point = 0.5
        # Display the user's answer on win0
        user_answer_text.draw(win1)
        feedback_text.draw(win0)
        win1.flip()
        win0.flip()

        # third screen
        user_answer_text.draw(win2)
        feedback_text.draw(win3)
        win2.flip()
        win3.flip()

            # If it is incorrect, involve administrator
    elif response != concept:
        admin_text.text = f"The answer was {response}. The correct answer is {concept}. Decide whether it is correct or incorrect."
        admin_text.draw(win4)
        win4.flip()       
        handle_incorrect_response(response, concept)
        win4.flip(clearBuffer=True)
    return correct_answer
    return correction


# function to save results
# Save the trial results to a CSV file after each trial
def save_trial_results(results):
    with open(results_filename, 'a', newline='') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=['ID', 'exp_start', 'block_start', 'practice','cycle', 'display_start', 'trial_start', 'trial_end', 'RT','word', 'modality', 'correction','answer', 'points'])

        if csvfile.tell() == 0:
            writer.writeheader()

        writer.writerow(results)

# save team points
def save_points(game_points_file):
    with open(leaderboard_filename, 'a', newline='') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=['team_name', 'points_part1', 'points_part2','points_sum'])

        if csvfile.tell() == 0:
            writer.writeheader()

        writer.writerow(game_points_file)

### LSL STREAM ###

info = StreamInfo('MyMarkerStream', 'Markers', 1, 0, 'string', 'myuidw43536')
outlet = StreamOutlet(info)

print("now sending markers...")
markernames = ['Experiment_start', 'New block starts', 'Trials start', 'Participant change', 'Trial_start', 'Trial_end', 'Next word', 'New correction', 'Experiment_end', 'Practice_starts', 'Practice trial starts', 'Practice trial ends']


### SYSTEM SETTINGS ###

# initiate clocks
globalClock = core.Clock()

# Set up a window to display the images in
win0 = visual.Window(       # guesser screen
    size=(1200, 600),        # make full screen
    units="pix",
    fullscr=True,
    screen=3,
    color="white"
)

win1 = visual.Window(       # performer screen
    size=(1200, 600),        # make fullscreen
    units="pix",
    fullscr=True,
    screen=1,               # when debugging 0, when running 1
    color="white"
)

win2 = visual.Window(       # experimenter screen - copy of win0
    size=[900, 500],          
    pos=(30,50),          # specify position
    units="pix",
    fullscr=False,
    screen=2,               # when debugging 0, when running 2
    color="white"
)
win3 = visual.Window(       # experimenter screen - copy of win 1
    size=[900, 500],         
    pos=(950,50),          # specify position
    units="pix",
    fullscr=False,
    screen=2,               # when debugging 0, when running 2
    color="white"
)
win4 = visual.Window(       # experimenter screen - new window
    size=[800, 400],            
    pos=(450, 630),         # specify position
    units="pix",
    fullscr=False,
    screen=2,               # when debugging 0, when running 2
    color="white"
)

### WORDS ### 

# Load in lists
# go to path wordlists/trial+participant ID/experiment_1 and load in the lists, use curfolder
ges1 = pd.read_csv(curfolder + "\\wordlists\\" + str(participant_id_input) + "\\experiment2\\ges_1.csv")
ges1 = ges1.iloc[:,1]
print(ges1)
ges1 = ges1.tolist()

mult1 = pd.read_csv(curfolder + "\\wordlists\\" +str(participant_id_input) + "\\experiment2\\mult_1.csv")
mult1 = mult1.iloc[:,1]
mult1 = mult1.tolist()

voc1 = pd.read_csv(curfolder + "\\wordlists\\" + str(participant_id_input) + "\\experiment2\\voc_1.csv")
voc1 = voc1.iloc[:,1]
voc1 = voc1.tolist()

ges2 = pd.read_csv(curfolder + "\\wordlists\\" + str(participant_id_input) + "\\experiment2\\ges_2.csv")
ges2 = ges2.iloc[:,1]
ges2 = ges2.tolist()

mult2 = pd.read_csv(curfolder + "\\wordlists\\" + str(participant_id_input) + "\\experiment2\\mult_2.csv")
mult2 = mult2.iloc[:,1]
mult2 = mult2.tolist()

voc2 = pd.read_csv(curfolder + "\\wordlists\\" + str(participant_id_input) + "\\experiment2\\voc_2.csv")
voc2 = voc2.iloc[:,1]
voc2 = voc2.tolist()

# same for practice round
# load in words
practice_concepts = pd.read_csv("./words/concepts_practice2.csv")
practice_concepts_list = practice_concepts['word'].tolist()

# create stimuli for both participants and all conditions
pr_ges1, pr_mult1, pr_voc1, pr_ges2, pr_mult2, pr_voc2 = create_stimuli(practice_concepts_list)

# check if it works
# print(ges1)

### STIMULI AND PROMPTS ###

# text stimulus
text_stimuli = create_text_stim(win1, "text_stimuli")
# watch prompt
watch_stimuli = create_text_stim(win0, "watch_stimuli")
# get ready prompt
get_ready = create_text_stim(win0, "get_ready")
# marker prompt - start
marker_start = create_text_stim(win1, "marker_start")
# marker prompt - end
marker_end = create_text_stim(win1, "marker_end")
# progress bar
progress_bar = create_text_stim(win0, "progress_bar")
# Create a text stimulus for displaying the user's answer on win0
user_answer_text = create_text_stim(win1, "user_answer_text")
feedback_text = create_text_stim(win0, "feedback_text")
# text for moving to the next trial, not needed because we have buttonbox
# reminder for locking hands 
signal_text = create_text_stim(win1, "signal_text")
signal_guesser = create_text_stim(win0, "signal_guesser")
marker_demo = create_text_stim(win1, "marker_demo")
# enter to proceed
proceed_enter = create_text_stim(win0, "proceed_enter")
# prompt to write ID
prompt_participant_id = create_text_stim(win0, "prompt_participant_id")
# welcome screen
welcome_text_g = create_text_stim(win0, "welcome_text_g")
welcome_text_p = create_text_stim(win1, "welcome_text_p")
# practice text
practice_text = create_text_stim(win0, "practice_text")
# prompt for answer
prompt_answer = create_text_stim(win0, "prompt_answer")
# new round
new_round = create_text_stim(win0, "new_round")
# end of block
block_end = create_text_stim(win0, "block_end")
# start of experiment
start_text = create_text_stim(win0, "start_text")
# end of experiment
end = create_text_stim(win0, "end")
# image
arrow = create_visual(win1, "arrow")
# tropy image
trophy = create_visual(win1, "trophy")
# admin
admin_text = visual.TextStim(win4, text="", height=30, color='black',  wrapWidth=800)
# start
start = create_text_stim(win0, "start")

### CONDITIONS ###
# make a condition list
conditions = ["gebaren", "combinatie", "geluiden"]
# shuffle the conditions
random.shuffle(conditions)

### INITIATE THE WINDOWS ###

### PREPARE FILE TO BE SAVED ### 
# Specify the path and filename for saving the results
results_filename = './data/' + str(participant_id_input) + '_results_part2.csv'
leaderboard_filename = './data/' + 'teams_leaderboard.csv'


# start screen
display_on_windows(win0, start, win1, start, [], [])

# wait for the key
# event.waitKeys()
key = []
while key == []: 
    key = bb.getButtons(buttonList='A')
    core.wait(0.01)

# Start the experiment
print("experiment starts") 
# get the time of the start
start_time = globalClock.getTime()
#send marker 
outlet.push_sample([markernames[0]])

#welcome team
welcome_team = visual.TextStim(win1, 
                                 text=f"Welkom terug {team_name}!", 
                                 height=80, 
                                 color="black",
                                 wrapWidth=1000)
display_on_windows(win0, welcome_team, win1, welcome_team, [], [])

# wait for the key
# event.waitKeys()
key = []
while key == []: 
    key = bb.getButtons(buttonList='A')
    core.wait(0.01)

# welcome text
display_on_windows(win0, welcome_text_g, win1, welcome_text_p, [], [])

# Show also on third screen
welcome_text_g.draw(win2)
welcome_text_p.draw(win3)
win2.flip()
win3.flip()

# wait for the key
# event.waitKeys()
key = []
while key == []: 
    key = bb.getButtons(buttonList='A')
    core.wait(0.01)

# send marker
print("practice starts")  
outlet.push_sample([markernames[9]])

# 3 blocks
for block_num, block in enumerate (conditions, start=1):

    # Display block instructions
    block_text = visual.TextStim(win1, 
                                 text=f"Dit is blok {block_num}: {block}", 
                                 height=80, 
                                 color="black",
                                 wrapWidth=1000)
    if block == "gebaren":
        image_stimulus = create_visual(win0, "gebaren")
    elif block == "combinatie":
        image_stimulus = create_visual(win0, "combinatie")
    elif block == "geluiden":
        image_stimulus = create_visual(win0, "geluiden")
    else:
        print("error in generating picture")

    # display
    display_on_windows(win0, block_text, win1, block_text, [image_stimulus], [image_stimulus])

    # display on third screen
    block_text.draw(win2)
    block_text.draw(win3)
    win2.flip()
    win3.flip()


    # send marker
    print("Practice: new block starts")  
    outlet.push_sample([markernames[1]])

    # get the time of the start
    block_time = globalClock.getTime()

    # wait for the key
    # event.waitKeys()
    key = []
    while key == []: 
        key = bb.getButtons(buttonList='A')
        core.wait(0.01)

    # 2 cycles for each participant
    cycle  = 0  # 1 cycle for 1 participant
    while cycle < 2:
        # Present each concept as a stimulus, up to 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  stimuli
        index = 0  # Initialize index
        presented_concepts = 1  # Initialize counter for concepts
        correction = 0 # Initialize counter for corrections

        #if the cycle has number 1, use gesture_, vocal_ and multimodal_ lists from participant 1, if 2, use lists from participant 2
        if cycle == 0:
            if block == "gebaren":
                practice_list = pr_ges1
                concept_list = ges1
                instructions_g = create_instr(win0, "gebaren", "instructions_g1")
                instructions_p = create_instr(win0, "gebaren", "instructions_p1")
            elif block == "combinatie":
                practice_list = pr_mult1
                concept_list = mult1
                instructions_g = create_instr(win0, "combinatie", "instructions_g1")
                print(instructions_g)
                instructions_p = create_instr(win0, "combinatie", "instructions_p1")
            elif block == "geluiden":
                practice_list = pr_voc1
                concept_list = voc1
                instructions_g = create_instr(win0, "geluiden", "instructions_g1")
                instructions_p = create_instr(win0, "geluiden", "instructions_p1")
            else:
                print("error in generating block instructions and stimuli")
        elif cycle == 1:
            if block == "gebaren":
                practice_list = pr_ges2
                concept_list = ges2
                instructions_g = create_instr(win0, "gebaren", "instructions_g2")
                instructions_p = create_instr(win0, "gebaren", "instructions_p2")
            elif block == "combinatie":
                practice_list = pr_mult2
                concept_list = mult2
                instructions_g = create_instr(win0, "combinatie", "instructions_g2")
                instructions_p = create_instr(win0, "combinatie", "instructions_p2")
            elif block == "geluiden":
                practice_list = pr_voc2
                concept_list = voc2
                instructions_g = create_instr(win0, "geluiden", "instructions_g2")
                instructions_p = create_instr(win0, "geluiden", "instructions_p2")
            else:
                print("error in generating block instructions and stimuli")
        else:
            print("error in generating block instructions and stimuli")


        # update picture 
        image_stimulus.size = (300,300)
        image_stimulus.pos = [0, 0]
        image_stimulus.opacity = 0.2

        # display
        display_on_windows(win0, instructions_g, win1, instructions_p, [image_stimulus], [image_stimulus])

        # display on third screen
        instructions_g.draw(win2)
        instructions_p.draw(win3)
        win2.flip()
        win3.flip()

        # wait for the key
        # event.waitKeys()
        key = []
        while key == []: 
            key = bb.getButtons(buttonList='A')
            core.wait(0.01)

        ### practice ###
        display_on_windows(win0, practice_text, win1, practice_text, [], [])

        # display on third screen
        practice_text.draw(win2)
        practice_text.draw(win3)
        win2.flip()
        win3.flip()

        # wait for the key
        # event.waitKeys()
        key = []
        while key == []: 
            key = bb.getButtons(buttonList='A')
            core.wait(0.01)

        # how to signal
        display_on_windows(win0, signal_guesser, win1, signal_text, [], [arrow, marker_demo])

        # display on third screen
        signal_guesser.draw(win2)
        signal_text.draw(win3)
        win2.flip()
        win3.flip()

        # wait for the key
        # event.waitKeys()
        key = []
        while key == []: 
            key = bb.getButtons(buttonList='A')
            core.wait(0.01)

        # send the marker
        print("practice: trials start")  
        outlet.push_sample([markernames[2]])
        
        while index < len(practice_list) and presented_concepts <= 2: 
            concept = practice_list[index]
            practice = 'practice'
            # Display the concept
            text_stimuli.text = concept
            # resize picture
            image_stimulus.size = (150,150)
            image_stimulus.pos = [-250,400]
            image_stimulus.opacity = 0.75
            progress_bar.text = f"Progress: {presented_concepts}/2"
            display_on_windows(win0, get_ready, win1, text_stimuli, [progress_bar], [progress_bar, marker_start, image_stimulus])

            #display on third screen
            get_ready.draw(win2)
            text_stimuli.draw(win3)
            win2.flip()
            win3.flip()

            # save time of word display
            word_display_time = globalClock.getTime() 
            
            # wait for the key
            # event.waitKeys()
            key = []
            while key == []: 
                key = bb.getButtons(buttonList='A')
                core.wait(0.01)

            
            # save time of performance start
            performance_start_time = globalClock.getTime()

            # send marker to 'start' recording
            print("practice: trial started")  
            outlet.push_sample([markernames[10]])

            # Display the concept with marker_recording
            display_on_windows(win0, watch_stimuli, win1, text_stimuli, [progress_bar], [progress_bar, marker_end, image_stimulus])

            # display on third screen
            watch_stimuli.draw(win2)
            text_stimuli.draw(win3)
            win2.flip()
            win3.flip()

            # wait for the key
            # event.waitKeys()
            key = []
            while key == []: 
                key = bb.getButtons(buttonList='A')
                core.wait(0.01)

            # save time of performance end
            performance_end_time = globalClock.getTime() 
            # send marker
            print("practice: trial ended")  
            outlet.push_sample([markernames[11]])


            # Display the concept without marker_recording and get input from guesser 
            response = visual.TextBox2(win0, size=(0.8, 0.2), pos=(0, -0.2), font = 'Arial', text='', color='black')
            display_on_windows(win0, prompt_answer, win1, text_stimuli, [progress_bar], [progress_bar, image_stimulus])

            # display on third screen
            prompt_answer.draw(win2)
            text_stimuli.draw(win3)
            win2.flip()
            win3.flip()
            
            correct_answer = False
            response = get_input(response, 'Wat denk je dat je teamgenoot probeerde uit te drukken?', prompt_answer, proceed_enter, win0)
            
            # save time of response
            response_time = globalClock.getTime() 

            # Check if the answer matches the concept
            handle_response(response, concept)

            # set points to 0 because this is only practice
            game_point = 0

            # save trial results
            trial_result = {'ID': participant_id_input, 'exp_start': start_time, 'block_start': block_time, 'practice': practice, 'cycle': cycle, 'display_start': word_display_time, 'trial_start': performance_start_time, 'trial_end': performance_end_time, 'RT': response_time, 'word': concept, 'modality': block, 'correction': correction, 'answer': response, 'points': game_point}
            save_trial_results(trial_result)

            # Wait for a key press to proceed to the next word or retry'
            # event.waitKeys()
            key = []
            while key == []: 
                key = bb.getButtons(buttonList='A')
                core.wait(0.01)
            if 'A' in key and correct_answer:
                presented_concepts += 1  # Increment the counter for repeated concepts
                index += 1
                correction = 0
                print("Practice: next word")  
                outlet.push_sample([markernames[6]])
                continue
            elif 'A' in key and not correct_answer:
                if correction < 2:
                    correction += 1
                    print("Practice: new correction")  
                    outlet.push_sample([markernames[7]])
                    continue 
                else:
                    presented_concepts += 1
                    index += 1
                    correction = 0
                    print("Practice: next word")  
                    outlet.push_sample([markernames[6]])
                    continue

            else:
                print('Error in evaluating what to do next.')

            # Clear win0 before presenting the next word
            win1.flip(clearBuffer=True)
            win0.flip(clearBuffer=True)    

        index = 0 # reinitialize index
        correction = 0 # reinitialize correction
        presented_concepts = 1 # reinitialize presented concepts
        display_on_windows(win0, start_text, win1, start_text, [], []) 

        # third screen
        start_text.draw(win2)
        start_text.draw(win3) 
        win2.flip()
        win3.flip()

        # wait for the key
        # event.waitKeys()
        key = []
        while key == []: 
            key = bb.getButtons(buttonList='A')
            core.wait(0.01)
  
        while index < len(concept_list) and presented_concepts <= 7: #set ptresented concepts to less when debugging
            concept = concept_list[index]
            practice = 'none'
            game_point = 0      # Initialize game points
            # Display the concept
            text_stimuli.text = concept
            # resize it
            image_stimulus.size = (100,100)
            image_stimulus.pos = [-200,400]
            progress_bar.text = f"Progress: {presented_concepts}/7"
            display_on_windows(win0, get_ready, win1, text_stimuli, [progress_bar], [progress_bar, marker_start, image_stimulus])

            # third screen
            text_stimuli.draw(win2)
            get_ready.draw(win3) 
            win2.flip()
            win3.flip()

            # save time of word display
            word_display_time = globalClock.getTime() 
            
            # wait for the key
            # event.waitKeys()
            key = []
            while key == []: 
                key = bb.getButtons(buttonList='A')
                core.wait(0.01)
        
            # save time of performance start
            performance_start_time = globalClock.getTime()

            # send marker to 'start' recording
            print("trial started")  
            outlet.push_sample([markernames[4]])

            # Display the concept with marker_recording
            display_on_windows(win0, watch_stimuli, win1, text_stimuli, [progress_bar], [progress_bar, marker_end, image_stimulus])

            # third screen
            text_stimuli.draw(win2)
            watch_stimuli.draw(win3) 
            win2.flip()
            win3.flip()

            # wait for the key
            # event.waitKeys()
            key = []
            while key == []: 
                key = bb.getButtons(buttonList='A')
                core.wait(0.01)

            # save time of performance end
            performance_end_time = globalClock.getTime() 
            # send marker
            print("trial ended")  
            outlet.push_sample([markernames[5]])

            # Display the concept without marker_recording and get input from guesser 
            response = visual.TextBox2(win0, size=(0.8, 0.2), pos=(0, -0.2), font = 'Arial', text='', color='black')
            display_on_windows(win0, prompt_answer, win1, text_stimuli, [progress_bar], [progress_bar, image_stimulus])

            # third screen
            text_stimuli.draw(win2)
            prompt_answer.draw(win3) 
            win2.flip()
            win3.flip()

            
            correct_answer = False
            response = get_input(response, 'Wat denk je dat je teamgenoot probeerde uit te drukken?', prompt_answer, proceed_enter, win0)
            
            # save time of response
            response_time = globalClock.getTime() 

            # initiate game points
            game_point = 0
            # Check if the answer matches the concept
            handle_response(response, concept)

            # save trial results
            trial_result = {'ID': participant_id_input, 'exp_start': start_time, 'block_start': block_time, 'practice': practice, 'cycle': cycle, 'display_start': word_display_time, 'trial_start': performance_start_time, 'trial_end': performance_end_time, 'RT': response_time, 'word': concept, 'modality': block, 'correction': correction, 'answer': response, 'points': game_point}
            save_trial_results(trial_result)

            # Wait for a key press to proceed to the next word or retry
            # event.waitKeys()
            key = []
            while key == []: 
                key = bb.getButtons(buttonList='A')
                core.wait(0.01)
            if 'A' in key and correct_answer:
                presented_concepts += 1  # Increment the counter for repeated concepts
                index += 1
                correction = 0
                print("Next word")  
                outlet.push_sample([markernames[6]])
                continue
            elif 'A' in key and not correct_answer:
                if correction < 2:
                    correction += 1
                    print("New correction")  
                    outlet.push_sample([markernames[7]])
                    continue 
                else:
                    presented_concepts += 1
                    index += 1
                    correction = 0
                    print("Next word")  
                    outlet.push_sample([markernames[6]])
                    continue
            else:
                print('Error in evaluating what to do next.')
           
            # Clear win0 before presenting the next word
            win1.flip(clearBuffer=True)
            win0.flip(clearBuffer=True)

        cycle += 1  
        if cycle == 1 and block_num <= 3:
            # send markers
            print("change of participants")  
            outlet.push_sample([markernames[3]])
            # display
            display_on_windows(win0, new_round, win1, new_round, [], [])

            # Third screen FLAGGED
            new_round.draw(win2)
            new_round.draw(win3)
            win3.flip()
            win2.flip()

            # wait for the key
            # event.waitKeys()
            key = []
            while key == []: 
                key = bb.getButtons(buttonList='A')
                core.wait(0.01)
            
        elif cycle == 2 and block_num < 3:
            # display end of block
            display_on_windows(win0, block_end, win1, block_end, [], [])

            # third screen
            block_end.draw(win2)
            block_end.draw(win3) 
            win2.flip()
            win3.flip()

            # wait for the key
            # event.waitKeys()
            key = []
            while key == []: 
                key = bb.getButtons(buttonList='A')
                core.wait(0.01)
            continue
        else:
            # window with end
            path_to_results_part1 = curfolder + "/data/" + participant_id_input + "_results_part1.csv"
            path_to_results_part2 = curfolder + "/data/" + participant_id_input + "_results_part2.csv" 

            df1 = pd.read_csv(path_to_results_part1, sep=',', encoding='utf-8')
            points_part1 = float(df1["points"].sum())
            df2 = pd.read_csv(path_to_results_part2, sep=',', encoding='utf-8')
            points_part2 = float(df2["points"].sum())

            points_total = points_part1+points_part2

            point_results = {'team_name': team_name, 'points_part1': points_part1, 'points_part2': points_part2,'points_sum':points_total}
            save_points(point_results)

            path_to_leaderboard = curfolder + "/data/" + "teams_leaderboard.csv"

            leaderboard_final = pd.read_csv(path_to_leaderboard, sep=',', encoding='utf-8')
            # sort it
            leaderboard_sorted = leaderboard_final.sort_values(by='points_sum', ascending=False)
            # create textStim
            leaderboard_text = visual.TextStim(win0,
                text='',
                pos=(0,0),
                height=45,
                color='black')
            #create a leaderboard
            leaderboard_str = 'Leaderboard:\n\n'
            for index, row in leaderboard_sorted.iterrows():
                leaderboard_str += f"{row['team_name']}: {row['points_sum']} points\n"


            end_points = visual.TextStim(win1,
                text=f"Dit is het einde van het experiment. Jullie team {team_name} heeft {points_total} punten gescoord, gefeliciteerd!",
                height=60,
                color="black",
                wrapWidth=1200)

            # Third screen 
            end_points.draw(win2)
            end_points.draw(win3)
            win2.flip()
            win3.flip()

            display_on_windows(win0, end_points, win1, end_points, [], [])


            # wait for the key
            # event.waitKeys()
            key = []
            while key == []: 
                key = bb.getButtons(buttonList='A')
                core.wait(0.01)

            # display leaderboard

            leaderboard_text.text = leaderboard_str
            display_on_windows(win0, leaderboard_text, win1, leaderboard_text, [trophy],[trophy])


            # Third screen 
            leaderboard_text.draw(win2)
            leaderboard_text.draw(win3)
            win2.flip()
            win3.flip()

            # wait for the key
            # event.waitKeys()
            key = []
            while key == []: 
                key = bb.getButtons(buttonList='A')
                core.wait(0.01)

# send markers
print("end of experiment")  
outlet.push_sample([markernames[8]])

# close the windows
win0.close()
win1.close()
win2.close()
win3.close()
win4.close()

core.quit()
