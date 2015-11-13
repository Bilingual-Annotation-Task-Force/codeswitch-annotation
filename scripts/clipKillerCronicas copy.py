import re
f = open("Killer_Cronicas_GoldSt-v3-annotated.txt")
clippedGS = open("KillerCronicasGoldStandard", "wb")
clippedText = open("KillerCronicas_clipped", "wb")
for line in f.readlines():
  l = line.split("\t")
  print l[2]
  if (int(l[0]) >= 10174 and int(l[0]) <= 20289):
    clippedGS.write(l[1] + "\t" + l[2])
    clippedText.write(l[1] + " ")

clippedGS.close()
clippedText.close()

