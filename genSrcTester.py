import fileinput
import sys
from subprocess import call
import re
import os
import tempfile

#pattern to find project's name
nameRe = re.compile(".*name:\s*\"(.+)\".*")
uriRe = re.compile(".*uri:\s*\"(.+)#.*\".*")
printDeps1 = re.compile(".*\"set\s*scalacOptions.*\"")
printDeps2 = re.compile(".*\"set\s*libraryDependencies.*\"")
printDeps3 = re.compile(".*\"set\s*publishArtifact.*\"")
printDeps4 = re.compile(".*\"set\s*addCompilerPlugin.*\"")

#individual settings

dbuildPath = '/home/vova/scala-projects/GSoC/script2/dbuild-0.6.4/'
dbuildGenSourcePath = os.getcwd()

dbuildExec = dbuildPath + 'bin/dbuild'
dbuildProjects = dbuildGenSourcePath + '/target-0.6.4/project-builds/'
printPlugin = 'printPlugin'

filename = sys.argv[-1]

print("dbuildExec: " + dbuildExec)
print("filename: " + filename)
#uncomment this
call([dbuildExec, filename])

names = []
uriNames = []
prKeys = {}
prFiles = {}

text=open(sys.argv[1], "r").read().split('\n')
for line in text:
    match = re.match(nameRe, line)
    if match:
        prName = match.group(1)
        if prName != printPlugin:
    	    names.append(prName.strip())
files = os.popen('ls -tr ' + dbuildProjects).read().split('\n')

for prName in names:
    for fileName in reversed(files):
        if prName.lower() in fileName.lower():
            prKeys[prName] = fileName
            break
print("prKeys: ")
print(prKeys)

for prName in names:
   if not (printPlugin.lower() in prName.lower()):
       #(cd myPath/ && exec sbt "run arg1")
       command = "(cd " + dbuildProjects + prKeys.get(prName, prName) + " && sbt compile)"
       print(command)
       os.system(command)
