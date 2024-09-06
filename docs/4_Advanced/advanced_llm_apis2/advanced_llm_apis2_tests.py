import hashlib
from hashlib import sha256

def hash(data):
    h=hashlib.new("SHA256")
    h.update(data.encode())
    return h.hexdigest()


class Tests():
    
    def test1(answer):
        if str(hash(answer)) == "6b23c0d5f35d1b11f9b683f0b0a617355deb11277d91ae091d399c655b87940d":
            return print("Correct!")
        else:
            return print("Incorrect, see above as to how we were able to get only the output response from the Ollama API. Make sure your answer is capitalized!")
        
    def test2(answer):
        if str(hash(answer)) == "df7e70e5021544f4834bbee64a9e3789febc4be81470df629cad6ddb03320a5c":
            return print("Correct!")
        if str(hash(answer)) == "559aead08264d5795d3909718cdd05abd49572e84fe55590eef31a88a08fdffd":
            return print("Incorrect, LLMs are not necessarily alogrithms.")
        else: 
            return print("Incorrect, these are applications of LLMs.")
        