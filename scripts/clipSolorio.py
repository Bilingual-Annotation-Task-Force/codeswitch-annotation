f = open("Solorio_7k")
outputGS = open("SolorioGoldStandard.txt", "wb")
outputText = open("SolorioText", "wb")
for line in f.readlines():
  line = line.split(",")
  word = line[0]
  tag = line[len(line) - 1]
  if (tag == "Other"):
    tag = "Punct"
  elif (tag == "Span"):
    tag = "Spn"
  outputGS.write(word + "\t" + tag)
  outputText.write(word + " ")
f.close()
outputGS.close()
outputText.close()
