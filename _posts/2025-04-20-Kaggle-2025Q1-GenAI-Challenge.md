---
title: Kaggle 2025Q1 GenAI Challenge
---

# Poor Ben's Almanack

For Kaggle's 2025Q1 GenAI Challenge capstone project, I created Poor Ben's Almanack, a chatbot that replies as Benjamin Franklin.
This chatbot uses Google's Gemini API, the gemini-2.0-flash model, the text-embedding-004 embedding model, and ChromaDB.

## Retrieval Augmented Generation

In RAG (Retrieval Augmented Generation), the LLM's knowledge is augmented with additional information retrieved from an external source. A common way to implement
RAG is to embed the user query as a vector of numbers and run a similarity search on a vector database for similar documents. The results are augmented to the prompt
that is sent to the LLM.

In this project, *The Autobiography of Benjamin Franklin* is used as a source for RAG.

First, it is downloaded via a wget command from Project Gutenberg. Then it is is divided into fixed-length chunks with overlapping between
chunks to retain context:

{% highlight python linenos %}
def chunk_text(filename:str, encoding:str='utf-8', chunk_size:int=4096, chunk_overlap:int=400) -> list[str]:
    """ Reads a file and divides it into a list of chunks with size chunk_size that overlap by chunk_overlap bytes.
    """
    chunks = []
    with open('pg148.txt', 'r', encoding="utf-8") as fh:
        text = fh.read()

        start = 0
        chunk_size = 4096
        chunk_overlap = 400
        while start < len(text):
            chunk = text[start:start + chunk_size]
            chunks.append(chunk)
            start += (chunk_size - chunk_overlap)
    return chunks
    
chunks = chunk_text('pg148.txt')
{% endhighlight %}

A custom embedding function is used to embed the chunks as they are loaded into ChromaDB.

{% highlight python linenos %}
class GeminiEmbeddingFunction(EmbeddingFunction):
    # Specify whether to generate embeddings for documents, or queries
    document_mode = True
    
    def __init__(self, client:genai.Client, model:str):
        self.client = client
        self.model = model

    @retry.Retry(predicate=is_retriable)
    def __call__(self, input: Documents) -> Embeddings:
        if self.document_mode:
            embedding_task = "retrieval_document"
        else:
            embedding_task = "retrieval_query"

        response = self.client.models.embed_content(
            model=self.model,
            contents=input,
            config=types.EmbedContentConfig(
                task_type=embedding_task,
            ),
        )
        return [e.values for e in response.embeddings]
{% endhighlight %}

The ChromaDB collection is created with this embedding function:

{% highlight python linenos %}
# Create an instance of GeminiEmbeddingFunction with te text-embedding-004 model.
embed_fn = GeminiEmbeddingFunction(client, "models/text-embedding-004")
# ...
db = chroma_client.get_or_create_collection(name=db_name, embedding_function=embed_fn)
{% endhighlight %}

The document chunks are loaded in batches to not exceed ChromaDB's API limits:

{% highlight python linenos %}
    # Load up to batch_size documents at a time.
    while ndocs > batch_size:
        db.add(documents=docs[start:start+batch_size], ids=[str(i) for i in range(start, start+batch_size)])
        start += batch_size
        ndocs -= batch_size
    # Load the leftovers when ndocs is less than batch_size.
    db.add(documents=docs[start:start+ndocs], ids=[str(i) for i in range(start, start+ndocs)])    
{% endhighlight %}

The embedding function is applied to them as they are added.

## Retrieval

When a user asks a question, it is sent as a query to ChromaDB, which applies the same embedding function to the query and then returns the most similar passages from the database collection. The embedding function includes a boolean flag that is switched to query mode at this point:

{% highlight python %}
embed_fn.document_mode = False
{% endhighlight %}

The most similar documents are returned as a list of strings:

{% highlight python linenos %}
result = db.query(query_texts=[query], n_results=num_passages)
[all_passages] = result['documents']
return all_passages
{% endhighlight %}

``num_passages`` is set to a default of five in the notebook for this project, but could be experimented with.

## Prompt Engineering

The prompt sent to Gemini starts with instructions to reply as Benjamin Franklin using both existing background information and the passages returned in the previous step. The user's question is appended to the prompt, as well as each passage:

{% highlight python linenos %}
prompt = f"""You are a helpful, witty and friendly bot version of Benjamin Franklin. 
Use the reference passages and any background information you may have to answer the question
in the character of Benjamin Franklin.

QUESTION: {query_oneline}
"""

for passage in all_passages:
    passage_oneline = passage.replace("\n", " ")
    prompt += f"PASSAGE: {passage_oneline}\n"
{% endhighlight %}

## Further Work

This project could serve as the back end for an agent that enables users to chat with a bot version of Benjamin Franklin, and it has the potential to be extended to other historical figures or even fictional characters. 