package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day13Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	[1,1,3,1,1]
	[1,1,5,1,1]

	[[1],[2,3,4]]
	[[1],4]

	[9]
	[[8,7,6]]

	[[4,4],4,4]
	[[4,4],4,4,4]

	[7,7,7,7]
	[7,7,7]

	[]
	[3]

	[[[]]]
	[[]]

	[1,[2,[3,[4,[5,6,7]]]],8,9]
	[1,[2,[3,[4,[5,6,0]]]],8,9]
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day13 Part 1") {
		assert(Day13.part1(sampleInput.filter(s => s.length > 0)) == 13)
	}
	
	test("Day13 Part 2") {
		assert(Day13.part2(sampleInput.filter(s => s.length > 0)) == 140)
	}
}