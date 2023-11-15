from openai import OpenAI
client = OpenAI(api_key="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")

def get_embedding(text_to_embed):
	# Embed a line of text
	response = openai.Embedding.create(
    	model= "text-embedding-ada-002",
    	input=[text_to_embed]
	)
	# Extract the AI output embedding as a list of floats
	embedding = response["data"][0]["embedding"]
  return embedding

# get your data
# df = 
df["embedding"] = df["body"].astype(str).apply(get_embedding)

