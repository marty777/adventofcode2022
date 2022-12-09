package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day09Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
    R 5
	U 8
	L 8
	D 3
	R 17
	D 10
	L 25
	U 20
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day09 Part 1") {
		assert(Day09.part1(sampleInput.flatMap(Day09.parseLine)) == 88)
	}
	
	test("Day09 Part 2") {
		assert(Day09.part2(sampleInput.flatMap(Day09.parseLine)) == 36)
	}
}