package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day04Suite extends AnyFunSuite {

	val sampleInput:Seq[String] = """
	2-4,6-8
	2-3,4-5
	5-7,7-9
	2-8,3-7
	6-6,4-6
	2-6,4-8
	""".trim.split("\n").toSeq.map(_.trim)

	test("Day04 Part 1") {
		assert(Day04.part1(sampleInput.flatMap(Day04.parseRangePair)) == 2)
	}
	
	test("Day04 Part 2") {
		assert(Day04.part2(sampleInput.flatMap(Day04.parseRangePair)) == 4)
	}
}