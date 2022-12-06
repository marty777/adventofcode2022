package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day05Suite extends AnyFunSuite {

	// line breaks cause problems if this file isn't unix-lf formatted
	val sampleInput:Seq[String] = """
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2""".split("\n").toSeq

	test("Day05 Part 1") { 
		assert(Day05.part1(sampleInput.takeRight(sampleInput.size - 1)) == "CMZ")
	}
	
	test("Day05 Part 2") {
		assert(Day05.part2(sampleInput.takeRight(sampleInput.size - 1)) == "MCD")
	}
}