package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day14Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	498,4 -> 498,6 -> 496,6
	503,4 -> 502,4 -> 502,9 -> 494,9
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day14 Part 1") { 
		assert(Day14.part1(Day14.parseLines(sampleInput)) == 24)
	}
	
	test("Day14 Part 2") {
		assert(Day14.part2(Day14.parseLines(sampleInput)) == 93)
	}
}