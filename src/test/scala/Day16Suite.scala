package com.marty777.adventofcode2022
import org.scalatest.funsuite.AnyFunSuite

class Day16Suite extends AnyFunSuite {

	// line break consistent with the line formatting of this file
	val fileNewline = """
"""
	val sampleInput:Seq[String] = """
	Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
	Valve BB has flow rate=13; tunnels lead to valves CC, AA
	Valve CC has flow rate=2; tunnels lead to valves DD, BB
	Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
	Valve EE has flow rate=3; tunnels lead to valves FF, DD
	Valve FF has flow rate=0; tunnels lead to valves EE, GG
	Valve GG has flow rate=0; tunnels lead to valves FF, HH
	Valve HH has flow rate=22; tunnel leads to valve GG
	Valve II has flow rate=0; tunnels lead to valves AA, JJ
	Valve JJ has flow rate=21; tunnel leads to valve II
	""".trim.split(fileNewline).map(_.trim).toSeq

	test("Day16 Part 1") { 
		assert(Day16.part1(sampleInput.flatMap(Day16.parseValve)) == 1651)
	}
	
	test("Day16 Part 2") {
		assert(Day16.part2(sampleInput.flatMap(Day16.parseValve)) == 1707)
	}
}