package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day08uite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	30373
	25512
	65332
	33549
	35390
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day08 Part 1") {
		assert(Day08.part1(Day08.parseLines(sampleInput)) == 21)
	}
	
	test("Day08 Part 2") {
		assert(Day08.part2(Day08.parseLines(sampleInput)) == 8)
	}
}