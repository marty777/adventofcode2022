package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day02Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	A Y
	B X
	C Z
	""".trim.split(fileNewline).toSeq.map(_.trim)

	test("Day02 Part 1") {
		assert(Day02.part1(sampleInput.flatMap(Day02.parseGuideEntry1)) == 15)
	}
	
	test("Day02 Part 2") {
		assert(Day02.part2(sampleInput.flatMap(Day02.parseGuideEntry2)) == 12)
	}
}