from fastapi import FastAPI
from pydantic import BaseModel
from app.model.model import generate_nieuwjaarsbrief

app = FastAPI()

class TextIn(BaseModel):
    text: str

class GenerationOut(BaseModel):
    output: str

@app.post("/predict", response_model=GenerationOut)
def generate(payload: TextIn):
    output = generate_nieuwjaarsbrief(payload.text)
    return {"output": output}
