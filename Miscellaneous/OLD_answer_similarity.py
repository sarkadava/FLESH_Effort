import os
import glob
import pandas as pd
from transformers import BertTokenizer, BertModel
from transformers import RobertaTokenizer, RobertaForSequenceClassification
import torch
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np
import openpyxl

curfolder = os.getcwd()
answerfolder = curfolder + '/TS_processing/answer_data/'
answerfiles = glob.glob(answerfolder + '*.csv')

# load in one file
df = pd.read_csv(answerfiles[0])
df2 = pd.read_csv(answerfiles[1])

# load in concept list.xlsx
df_concepts = pd.read_excel(answerfolder + '/conceptlist_info.xlsx')
df_concepts

# merge df and df 2
df_all = pd.concat([df, df2], ignore_index=True)
df_all
# keep only columns word and answer
df = df_all[['word', 'answer']]
# in df_concepts, keep only English and Dutch
df_concepts = df_concepts[['English', 'Dutch']]
# rename Dutch to word
df_concepts = df_concepts.rename(columns={'Dutch': 'word'})
# merge df and df_concepts on word
df = pd.merge(df, df_concepts, on='word', how='left')
df

# show rows where English is NaN
df[df['English'].isnull()]

# add translations manually for each
df.loc[df['word'] == 'bloem', 'English'] = 'flower'
df.loc[df['word'] == 'dansen', 'English'] = 'to dance'
df.loc[df['word'] == 'auto', 'English'] = 'car'
df.loc[df['word'] == 'olifant', 'English'] = 'elephant'
df.loc[df['word'] == 'comfortabel', 'English'] = 'comfortable'
df.loc[df['word'] == 'bal', 'English'] = 'ball'
df.loc[df['word'] == 'haasten', 'English'] = 'to hurry'
df.loc[df['word'] == 'gek', 'English'] = 'crazy'
df.loc[df['word'] == 'snijden', 'English'] = 'to cut'
df.loc[df['word'] == 'koken', 'English'] = 'to cook'
df.loc[df['word'] == 'juichen', 'English'] = 'to cheer'
df.loc[df['word'] == 'zingen', 'English'] = 'to sing'
df.loc[df['word'] == 'glimlach', 'English'] = 'smile'
df.loc[df['word'] == 'klok', 'English'] = 'clock'
df.loc[df['word'] == 'fiets', 'English'] = 'bicycle'
df.loc[df['word'] == 'vliegtuig', 'English'] = 'airplane'
df.loc[df['word'] == 'geheim', 'English'] = 'secret'
df.loc[df['word'] == 'telefoon', 'English'] = 'telephone'
df.loc[df['word'] == 'zwaaien', 'English'] = 'to wave'
df.loc[df['word'] == 'sneeuw', 'English'] = 'snow'

df

# show me nan in English
df[df['English'].isnull()]


########### DUTCH

# merge
meanings_nl = list(df['word'])
answers_nl = list(df['answer'])

# BERT

tokenizer = BertTokenizer.from_pretrained('GroNLP/bert-base-dutch-cased')
model = BertModel.from_pretrained('GroNLP/bert-base-dutch-cased')

# tokenizer = BertTokenizer.from_pretrained('bert-base-multilingual-cased')
# model = BertModel.from_pretrained('bert-base-multilingual-cased')

# tokenizer = RobertaTokenizer.from_pretrained('pdelobelle/robbert-v2-dutch-base') # https://github.com/iPieter/RobBERT
# model = RobertaForSequenceClassification.from_pretrained('pdelobelle/robbert-v2-dutch-base')

# other models here: https://huggingface.co/transformers/v3.3.1/pretrained_models.html

# Function to get BERT embeddings
def get_word_embedding(word):
    inputs = tokenizer(word, return_tensors='pt')
    with torch.no_grad():
        outputs = model(**inputs)
    # Extract the embeddings of the [CLS] token, which is the aggregate representation
    embeddings = outputs.last_hidden_state[:, 0, :].squeeze()
    return embeddings

# Calculate embeddings
guessed_embeddings_nl = [get_word_embedding(word) for word in meanings_nl]
answer_embeddings_nl = [get_word_embedding(word) for word in answers_nl]

# Calculate cosine similarities
similarities_nl = [cosine_similarity([guessed_emb], [answer_emb])[0][0] 
                for guessed_emb, answer_emb in zip(guessed_embeddings_nl, answer_embeddings_nl)]

# Print similarities
for guessed_word, answer, similarity in zip(meanings_nl, answers_nl, similarities_nl):
    print(f"Similarity between '{guessed_word}' and '{answer}': {similarity:.4f}")

# calculate euclidian similarity
def euclidean_similarity(a, b):
    return np.linalg.norm(a - b)

# Calculate euclidean similarities
euclidean_similarities_nl = [euclidean_similarity(guessed_emb, answer_emb) for guessed_emb, answer_emb in zip(guessed_embeddings_nl, answer_embeddings_nl)]

# Print similarities
for guessed_word, answer, similarity in zip(meanings_nl, answers_nl, euclidean_similarities_nl):
    print(f"Euclidean similarity between '{guessed_word}' and '{answer}': {similarity:.4f}")


#### ENGLISH
meanings_en = list(df['English'])
len(meanings_en)

answers_nl = list(df['answer'])
answers_nl
len(answers_nl)

answers_en = ['party', 'to cheer', 'tasty', 'to shoot', 'to breathe', 'zombie', 'bee', 'sea', 'dirty', 'tasty', 'car', 'to eat', 'to eat', 'to blow', 'hose', 'hose', 'to annoy', 'to make noise', 'to make noise', 'to run away', 'elephant', 'to cry', 'cold', 'outfit', 'silence', 'to ski', 'wrong', 'to play basketball', 'to search', 'disturbed', 'to run', 'to lick', 'to lift', 'to lightning', 'to think', 'to jump', 'to fall', 'to write', 'to dance', 'shoulder height', 'horn', 'dirty', 'boring', 'to drink', 'strong', 'elderly', 'to mix', 'fish', 'fish', 'dirty', 'wrong', 'smart', 'to box', 'to box', 'dog', 'to catch', 'to cheer', 'to sing', 'pregnant', 'hair', 'to shower', 'pain', 'burnt', 'hot', 'I', 'to chew', 'bird', 'airplane', 'to fly', 'to think', 'to choose', 'to doubt', 'graffiti', 'fireworks', 'bomb', 'to smile', 'to laugh', 'smile', 'clock', 'to wonder', 'height', 'big', 'height', 'space', 'to misjudge', 'to wait', 'satisfied', 'happy', 'fish', 'to smell', 'wind', 'pain', 'to burn', 'hot', 'to cycle', 'to fly', 'airplane', 'bird', 'to crawl', 'to drink', 'waterfall', 'water', 'fire', 'top', 'good', 'to hear', 'to point', 'distance', 'there', 'to whisper', 'quiet', 'to be silent', 'phone', 'to blow', 'to distribute', 'to give', 'cat', 'to laugh', 'tasty', 'to eat', 'yummy', 'to sleep', 'mountain', 'dirty', 'to vomit', 'to be disgusted', 'to greet', 'hello', 'goodbye', 'to smell', 'nose', 'scent', 'to fly', 'fireworks', 'to blow', 'to cut', 'pain', 'hot', 'to slurp', 'to throw', 'to fall', 'to fall', 'whistle', 'heartbeat', 'mouse', 'to hit', 'to catch', 'to grab', 'to throw', 'to fall', 'to shoot', 'circus', 'trunk', 'to fall', 'to fight', 'pain', 'to push open', 'to growl', 'to cut', 'to eat', 'knife', 'to slurp', 'to drink', 'drink', 'to eat', 'delicious', 'tasty', 'to cough', 'sick', 'to cry', 'to cry']
len(answers_en)

tokenizer_en = BertTokenizer.from_pretrained('bert-large-uncased-whole-word-masking')
model_en = BertModel.from_pretrained('bert-large-uncased-whole-word-masking')

# Calculate embeddings
guessed_embeddings_en = [get_word_embedding(word) for word in meanings_en]
answer_embeddings_en = [get_word_embedding(word) for word in answers_en]

# Calculate cosine similarities
similarities_en = [cosine_similarity([guessed_emb], [answer_emb])[0][0] for guessed_emb, answer_emb in zip(guessed_embeddings_en, answer_embeddings_en)]

# Print similarities
for guessed_word, answer, similarity in zip(meanings_en, answers_en, similarities_en):
    print(f"Similarity between '{guessed_word}' and '{answer}': {similarity:.4f}")

# calculate euclidian similarity
def euclidean_similarity(a, b):
    return np.linalg.norm(a - b)

# Calculate euclidean similarities
euclidean_similarities_en = [euclidean_similarity(guessed_emb, answer_emb) for guessed_emb, answer_emb in zip(guessed_embeddings_en, answer_embeddings_en)]

# Print similarities
for guessed_word, answer, similarity in zip(meanings_en, answers_en, euclidean_similarities_en):
    print(f"Euclidean similarity between '{guessed_word}' and '{answer}': {similarity:.4f}")
