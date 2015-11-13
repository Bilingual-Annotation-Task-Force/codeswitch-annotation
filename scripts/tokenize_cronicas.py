f = open("Cronicas_split_for_token - Sheet1.csv")
words = []
for line in f:
  tokens = line.split(",")
  words.append(tokens[1])

output = open("cronicas_words", "w")
for w in words:
  output.write(w + " ")

