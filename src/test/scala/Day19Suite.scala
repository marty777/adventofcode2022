package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day19Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
	Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day19 Part 1") {
		assert(Day19.part1(sampleInput.flatMap(Day19.parseBlueprint)) == 33)
	}
	
	test("Day19 Part 2") {
		assert(Day19.part2(sampleInput.flatMap(Day19.parseBlueprint)) == 3472)
	}
}