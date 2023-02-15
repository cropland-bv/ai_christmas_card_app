from transformers import TextGenerationPipeline, GPT2LMHeadModel, AutoTokenizer
from pathlib import Path
import os 
#BASE_DIR =  Path(__file__).resolve(strict=True).parent
MODEL = GPT2LMHeadModel.from_pretrained(os.getcwd()+"\\app\model", max_length=250, min_length = 150, num_beams=5, no_repeat_ngram_size=2, early_stopping=True, temperature = 1.5)
TOKENIZER = AutoTokenizer.from_pretrained('gpt2-medium')
PIPE =   TextGenerationPipeline(model=MODEL, tokenizer=TOKENIZER, return_all_scores=True, skip_special_tokens=True)

def generate_nieuwjaarsbrief(input_text):
    output= PIPE(input_text)[0]['generated_text']
    return output
