package com.marty777.adventofcode2022

import java.nio.file.{Paths, Files}

def usage = {
	println("""
	Usage: 
	sbt "run [DAY] [INPUT FILE]"
	
	DAY			The advent day to run (1-25)
	INPUT FILE		The path to the input text file
""");
	sys.exit(0);
}

def logo: String = {
"""
   ___     __              __    ____  ___  _____        __   
  / _ |___/ /  _____ ___  / /_  / __ \/ _/ / ___/__  ___/ /__ 
 / __ / _  / |/ / -_) _ \/ __/ / /_/ / _/ / /__/ _ \/ _  / -_)
/_/ |_\_,_/|___/\__/_//_/\__/  \____/_/   \___/\___/\_,_/\__/ 
                                                              
############### Advent of Code 2022 - Scala 3 ###############
"""
}

@main def AOC2022(day: Int, path: String) =
	println(logo)

	println(s"Starting Advent of Code 2022 day $day with input file $path")
	if(!Files.exists(Paths.get(path))) {
		println(s"The input file $path could not be found");
		usage;
	}
	if(day < 1 || day > 25) {
		println(s"The advent day entered ($day) must be 1-25")
		usage
	}
	
	val puzzle = day match {
		case 1 => Day01
		case 2 => Day02
		case 3 => Day03
		case 4 => Day04
		case 5 => Day05
		case 6 => Day06
		case 7 => Day07
		case 8 => Day08
		case 9 => Day09
		case _ => {
			println(s"Puzzle day $day has not been implemented yet")
			usage
		}
	}
	val startTime = System.nanoTime
	val (result1, result2) = puzzle.run(path)
	val duration = (System.nanoTime - startTime) / 1e6d // milliseconds
	println(s"Part 1: $result1")
	println(s"Part 2: $result2")
	println(s"Completed in $duration ms")