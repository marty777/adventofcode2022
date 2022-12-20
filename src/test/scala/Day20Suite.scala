package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day20Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	1
	2
	-3
	3
	-2
	0
	4
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day20 Part 1") { 
		assert(Day20.part1(sampleInput.map(_.toLong)) == 3)
	}
	
	test("Day20 Part 2") {
		assert(Day20.part2(sampleInput.map(_.toLong)) == 1623178306)
	}
}