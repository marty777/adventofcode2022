package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day18Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	2,2,2
	1,2,2
	3,2,2
	2,1,2
	2,3,2
	2,2,1
	2,2,3
	2,2,4
	2,2,6
	1,2,5
	3,2,5
	2,1,5
	2,3,5
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day18 Part 1") { 
		assert(Day18.part1(sampleInput.flatMap(Day18.parseCubes)) == 64)
	}
	
	test("Day18 Part 2") {
		assert(Day18.part2(sampleInput.flatMap(Day18.parseCubes)) == 58)
	}
}