package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day12Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	Sabqponm
	abcryxxl
	accszExk
	acctuvwj
	abdefghi
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day12 Part 1") {
		assert(Day12.part1(sampleInput) == 31)
	}
	
	test("Day12 Part 2") {
		assert(Day12.part2(sampleInput) == 29)
	}
}