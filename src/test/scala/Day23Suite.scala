package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day23Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	....#..
	..###.#
	#...#.#
	.#...##
	#.###..
	##.#.##
	.#..#..
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day23 Part 1") { 
		assert(Day23.part1(sampleInput) == 110)
	}
	
	test("Day23 Part 2") {
		assert(Day23.part2(sampleInput) == 20)
	}
}