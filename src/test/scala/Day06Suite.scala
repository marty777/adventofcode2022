package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day06Suite extends AnyFunSuite {

	// line break consistant with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
    zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw
	""".trim.split(fileNewline).toSeq

	test("Day06 Part 1") {
		assert(Day06.part1(sampleInput) == 11)
	}
	
	test("Day05 Part 2") {
		assert(Day06.part2(sampleInput) == 26)
	}
}