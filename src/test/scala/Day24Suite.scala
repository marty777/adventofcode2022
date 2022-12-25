package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day24Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	#.######
	#>>.<^<#
	#.<..<<#
	#>v.><>#
	#<^v^^>#
	######.#
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day24 Part 1") { 
		assert(Day24.part1(sampleInput) == 18)
	}
	
	test("Day24 Part 2") {
		assert(Day24.part2(sampleInput) == 54)
	}
}