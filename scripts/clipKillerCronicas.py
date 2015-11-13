import re
f = open("Killer_Cronicas_GoldSt-v3-annotated.txt")
clippedGS = open("KillerCronicasGoldStandard.txt", "wb")
clippedText = open("KillerCronicas_clipped.txt", "wb")
for line in f.readlines():
  l = line.split("\t")
  if (int(l[0]) >= 10000 and int(l[0]) <= 20000):
    clippedGS.write(l[1] + "\t" + l[2] + "\n")
    clippedText.write(l[1] + " ")

clippedGS.close()
clippedText.close()

