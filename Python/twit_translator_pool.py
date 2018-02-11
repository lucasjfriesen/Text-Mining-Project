#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
from tqdm import tqdm
from googletrans import Translator
import pandas as pd
import numpy as np
import json
import codecs
from multiprocessing.dummy import Pool as ThreadPool
import glob, os

# Break the meag-file into chunks of 100 tweets
def decompose():
    with open(r"/Users/lucasfriesen/Google Drive/UBC/Projects/IELTS Project/Data/merged_file_b.json") as file:
        df = json.load(file)

    for i in range(0, len(df), 1000):
        chunk = df[i:i+1000]
        with open(r"/Users/lucasfriesen/Google Drive/UBC/Projects/IELTS Project/Data/" + "chunk_" + str(i) + ".json",
                  "w") as filename:
            json.dump(chunk, filename)


def translate(self):
    print("Processing: " + self)
    with open(self) as file:
        df = json.load(file)
    filename = self[5:]
    textlist = np.array(df)
    translator = Translator()

    for i in textlist:
        try:
            translation = translator.translate(i['text'])
        except:
            i.update({'trans': 'NA'})
            i.update({'lang': 'NA'})
            continue
        i.update({'trans': translation.text})
        i.update({'lang': translation.src})
    print("Finished: "+ filename)
    pd.Series(textlist).to_json(filename, orient='values')


def clean_it(df):
    # stop_chars = [",,", "、", "'", '"', "シ"]
    for i in range(len(df)):
        encoded_str = df[i]['text'].encode('unicode-escape')
        decoded_str = codecs.decode(encoded_str, errors='backslashreplace')
        df[i].update({'text': decoded_str})
    return df


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process some foreign text.")
    parser.add_argument('--input_file', default=r"/Users/lucasfriesen/Google Drive/UBC/Projects/IELTS Project/Data/merged_file_b.json"
                                                , help="A JSON file for translating")
    parser.add_argument('--output_file', default="translated_data")
    parser.add_argument('--clean', default=False, help="Clean the data of foreign characters? Default = False")
    parser.add_argument('--decompose', default=False, help="Do you want to break the data file into chunks to aid in processing?")
    args = parser.parse_args()

    input_file = args.input_file
    output_file = args.output_file
    clean = args.clean

    os.chdir(r"/Users/lucasfriesen/Google Drive/UBC/Projects/IELTS Project/Data/chunks")
    file_paths = []
    for file in glob.glob("*.json"):
        file_paths.append(file)

    pool = ThreadPool(12)
    pool.map(translate, file_paths)
    pool.close()
    pool.join()


