#!/usr/bin/python

# Open files
fmain = open('docker_code.hs', 'r')
f1    = open('Graphic.hs', 'r')
f2    = open('ListTree.hs', 'r')
f3    = open('Music.hs', 'r')

# Filter
def cutLanguageExtsAndImports(f):
    ret = ""
    for line in f:
        if   line.find("{-# LANGUAGE ") == 0: continue
        elif line.find("import ")       == 0: continue
        elif line.find("module ")       == 0: continue
        ret = ret + line
    return ret
        
# Print file
subFiles = [f1,f2,f3]
subFilesFiltered = map(cutLanguageExtsAndImports, subFiles)
subFilesText = ''.join(subFilesFiltered)
print fmain.read()
print subFilesText
