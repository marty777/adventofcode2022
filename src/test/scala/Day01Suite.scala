package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day01Suite extends AnyFunSuite {
	
	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	1000
	2000
	3000

	4000

	5000
	6000

	7000
	8000
	9000

	10000
	""".trim.split(fileNewline).toSeq.map(_.trim)

	test("Day01 Part 1") {
		assert(Day01.part1(Day01.parseElves(sampleInput)) == 24000)
	}
	
	test("Day01 Part 2") {
		assert(Day01.part2(Day01.parseElves(sampleInput)) == 45000)
	}
}