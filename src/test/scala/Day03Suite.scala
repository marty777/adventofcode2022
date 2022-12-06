package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day03Suite extends AnyFunSuite {

	// line break consistant with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	vJrwpWtwJgWrhcsFMMfFFhFp
	jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
	PmmdzqPrVvPwwTWBwg
	wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
	ttgJtRGJQctTZtZT
	CrZsJsPPZsGzwwsLwLmpwMDw
	""".trim.split(fileNewline).toSeq.map(_.trim)

	test("Day03 Part 1") {
		assert(Day03.part1(sampleInput) == 157)
	}
	
	test("Day03 Part 2") {
		assert(Day03.part2(sampleInput) == 70)
	}
}