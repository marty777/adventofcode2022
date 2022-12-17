package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day17Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day17 Part 1") { 
		assert(Day17.part1(sampleInput) == 3068)
	}
	
	test("Day17 Part 2") {
		val expectedResult:Long = 1514285714288
		assert(Day17.part2(sampleInput) == expectedResult)
	}
}