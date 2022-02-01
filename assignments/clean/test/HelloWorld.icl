module HelloWorld

import StdEnv

// This module contains two basic "Hello world" programs: one for Clean (without iTasks) and one for iTasks.
// Uncomment the section you need and build the project with `cpm HelloWorld.prj`.
// Run the compiled program with `./HelloWorld.exe`.

// Clean version
/*
Start = helloWorldClean

helloWorldClean :: String
helloWorldClean = "Hello, world!"
*/

// iTasks version
import iTasks

Start world = helloWorldITasks world

helloWorldITasks :: *World -> *World
helloWorldITasks world = doTasks helloWorld world
where
	helloWorld :: Task String
	helloWorld = viewInformation [] "Hello, world!"
